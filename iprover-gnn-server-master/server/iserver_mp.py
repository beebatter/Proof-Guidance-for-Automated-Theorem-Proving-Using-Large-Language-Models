#!/usr/bin/env python3
"""
A stateful server for iProver.
"""

import os
#os.environ["OMP_NUM_THREADS"] = "1"
#os.environ["OPENBLAS_NUM_THREADS"] = "1"
#os.environ["MKL_NUM_THREADS"] = "1"
#os.environ["VECLIB_MAXIMUM_THREADS"] = "1"
#os.environ["NUMEXPR_NUM_THREADS"] = "1"

import sys
import socket
from multiprocessing import Process, Queue, Manager
import argparse
import contextlib
import tempfile
import random
#import signal

import pickle
import uuid

import time

import traceback

import orjson as json

#import math

#import parsing
import fcoplib as cop

# Contains numpy, hence OPENBLASS threads... !!!
from graph_data import GraphData

# Score return for conjectures
CONJECTURE_DEFAULT_SCORE = 1.0

# It is probably not necessary, 3.7 should work as well...
# 3.11 for KeyboardInterrupt
MIN_PYTHON_VERSION = (3, 11)

if sys.version_info < MIN_PYTHON_VERSION:
    print(f'You should use at least Python {".".join([str(x) for x in MIN_PYTHON_VERSION])}')
    sys.exit(1)
    
def create_cnf_file(tmpfile, clauses, contains_conj):
    for cl in clauses:
        tmpfile.write(preprocess_clause(cl).encode() + '\n'.encode())

    if not contains_conj:
        tmpfile.write('cnf(empty_conj,negated_conjecture,(~(a=a))).\n'.encode())

    tmpfile.flush()

def preprocess_clause(s):
    """
    DIRTY HACK!!!

    Remove last part of cnf...
    
    cnf(c_83,plain,
    ( ~ state(X0,X1,X2,X3,X4,h(X5,X6),X7,X8,X9,X10,e1(s(X5),X6),e2(s(X5),s(X6)))
    | state(X0,X1,X2,X3,X4,h(s(X5),X6),X7,X8,X9,X10,e1(X5,X6),e2(X5,s(X6))) ),
    file('problem.p', h_down) ).

    """

    s_split = s.split('file(')
    if len(s_split) < 2:
        s_split = s.split('inference(')
    if len(s_split) < 2:
        s_split = s.split('theory(')

        
    assert len(s_split) == 2, [s_split]

    s_end = s_split[0].rstrip()

    assert s_end.endswith(',')

    s_out = s_end[:-1] + ').'

    return s_out


def chunks(in_list, max_size):
    for i in range(0, len(in_list), max_size):
        yield in_list[i:i + max_size]


def readuntil(conn, previous_msg):
    symbol = b'\0'
    rest_data = [b'']

    if len(previous_msg) > 1:
        return previous_msg[0], previous_msg[1:]
    else:
        data = previous_msg
    
    while True:
        chunk = conn.recv(4096)

        if not chunk:
            break
        elif symbol in chunk:
            chunk_split = chunk.split(symbol)

            assert len(chunk_split) > 1

            data.append(chunk_split[0])
            rest_data = chunk_split[1:]
            break
        else:
            data.append(chunk)

    return b''.join(data), rest_data

class State:
    def __init__(self,
                 state_proc_id=None,
                 conn=None,
                 eval_queue=None,
                 eval_results_queue=None,
                 args=None,
                 logdebug=False,
                 loginfo=False,
                 create_train_mode=False):
        self.clauses = dict()
        self.conjectures = set()
        self.given_clauses = set()
        self.given_clauses_hist = list()
        self.server_queries_ended = False
        self.wait_for_msg = True
        self.args = args
        self.logdebug = logdebug
        self.loginfo = loginfo

        self.eval_data = list()

        self.create_train_mode = create_train_mode

        if not self.create_train_mode:
            assert state_proc_id is not None
            self.state_proc_id = state_proc_id

            assert conn is not None
            self.conn = conn

            assert eval_queue is not None
            self.eval_queue = eval_queue

            assert eval_results_queue is not None
            self.eval_results_queue = eval_results_queue
        else:
            self.train_examples = []
            self.train_queries = []
            self.clauses_basic_id = dict()
            


    def send_end_queries(self):
        # End queries
        out_msg = json.dumps({'tag':'server_queries_end'})
        out_msg += '\n'.encode()
        self.conn.sendall(out_msg)

    def register_clauses(self, json_msg):
        for cl in json_msg['clauses']:
            cl_id = cl['clause_id']
            cl_clause = cl['clause']
            cl_isconj = cl['clause_features']['conj_dist'] == 0

            assert cl_id not in self.clauses
            self.clauses[cl_id] = cl_clause

            if self.create_train_mode:
                basic_cl_id = cl['clause_features']['basic_clause_id']                
                self.clauses_basic_id[cl_id] = basic_cl_id

            if cl_isconj:
                self.conjectures.add(cl_id)

    def scores_req(self, json_msg):
        self.server_queries_ended = True

        if self.args.zero_scores:
            self.send_end_queries()

            out_msg = json.dumps({'tag':'scores_res',
                                  'scores': len(json_msg['clause_ids'])*[0.0]})
            out_msg += '\n'.encode()
            self.conn.sendall(out_msg)
        else:
            if not self.create_train_mode:
                self.send_end_queries()

            added_ids = set(json_msg['clause_ids'])

            assert added_ids.issubset(self.clauses.keys())

            ids_to_eval = [cl_id for cl_id in json_msg['clause_ids']]
            ids_to_eval_conj_pos = [cl_id in self.conjectures for cl_id in json_msg['clause_ids']]

            context_ids = list()
            context_ids_conj_pos = list()

            # Add all conjectures not in msg
            if len(self.conjectures) > self.args.context_conjectures:
                context_conjectures = random.sample(list(self.conjectures), self.args.context_conjectures)
            else:
                context_conjectures = list(self.conjectures)


            for conj_id in context_conjectures:
                if conj_id not in added_ids:
                    context_ids.append(conj_id)
                    context_ids_conj_pos.append(True)
                    added_ids.add(conj_id)

            # Add other context

            # Add random sample of given clauses if limit reached!
            if len(self.given_clauses) > self.args.context_given_clauses:
                if args.first_context:
                    # Some given clauses can be move from active to passive!
                    context_given_clauses = [cl_id for cl_id, _ in self.given_clauses_hist if cl_id in self.given_clauses][:self.args.context_given_clauses]
                elif args.last_context:
                    context_given_clauses = [cl_id for cl_id, _ in self.given_clauses_hist if cl_id in self.given_clauses][-self.args.context_given_clauses:]
                else:
                    context_given_clauses = random.sample(list(self.given_clauses), self.args.context_given_clauses)
            else:
                context_given_clauses = list(self.given_clauses)

            for cl_id in context_given_clauses:
                if cl_id not in added_ids:
                    context_ids.append(cl_id)
                    # If conjectures added before, this should be always False (but sampled!)
                    context_ids_conj_pos.append(cl_id in self.conjectures)
                    added_ids.add(cl_id)                    

            # To know what returns
            original_len = len(json_msg['clause_ids'])

            # Use better chunks, because the last one can be small!!!
            ids_to_eval_chunks = list(chunks(ids_to_eval, self.args.query_max_size))

            eval_results = []

            if self.create_train_mode:
                for i, chunk in enumerate(ids_to_eval_chunks):
                    clauses_to_eval = chunk + context_ids

                    self.train_queries.append(clauses_to_eval)
            else:
                with contextlib.ExitStack() as stack:
                    for i, chunk in enumerate(ids_to_eval_chunks):
                        tmpfile = stack.enter_context(tempfile.NamedTemporaryFile(dir='/dev/shm/'))

                        clauses_to_eval = chunk + context_ids

                        chunk_with_conj = len(set(clauses_to_eval) & self.conjectures) > 0

                        create_cnf_file(tmpfile,
                                        [self.clauses[cl_id] for cl_id in clauses_to_eval],
                                        chunk_with_conj
                                        )

                        eval_queue.put((self.state_proc_id, i, tmpfile.name))

                    while len(eval_results) < len(ids_to_eval_chunks):
                        eval_result = eval_results_queue[self.state_proc_id].get()
                        eval_results.append(eval_result)

                assert len(eval_results) == len(ids_to_eval_chunks)

                # x[0] is i
                sorted_eval_results = sorted(eval_results, key=lambda x:x[0])

                reversed_results = list(reversed([float(x) for y in sorted_eval_results for x in y[2]]))

                out_scores = []

                for is_conj in ids_to_eval_conj_pos:
                    if is_conj:
                        out_scores.append(CONJECTURE_DEFAULT_SCORE)
                    else:
                        out_scores.append(reversed_results.pop())

                assert len(out_scores) == original_len

                out_msg = json.dumps({'tag': 'scores_res',
                                      'scores':out_scores})
                out_msg += '\n'.encode()

                self.conn.sendall(out_msg)

                if self.args.evaldata_dir:
                    eval_record = (clauses_to_eval, original_len, ids_to_eval_conj_pos, [float(x) for y in sorted_eval_results for x in y[2]])
                    self.eval_data.append(eval_record)

                if self.logdebug:
                    logprint(f'Out msg len: {len(msg)}')
                    logprint(f'Out msg: {out_msg}')

    def given_clause(self, json_msg):
        assert len(json_msg['clause_ids']) == 1

        clause_id = json_msg['clause_ids'][0]
        component_id = json_msg['component_id']

        self.given_clauses.add(clause_id)
        self.given_clauses_hist.append((clause_id, component_id))

    def moved_from_active_to_passive(self, json_msg):
        # Remove from given clauses?
        clause_ids = json_msg['clause_ids']
        component_id = json_msg['component_id']

        for clause_id in clause_ids:
            self.given_clauses_hist.append((-clause_id, component_id))

    def server_queries_start(self):
        if not self.create_train_mode:
            if not self.server_queries_ended:
                self.send_end_queries()
            self.server_queries_ended = False

    def current_given_clauses(self):
        given_clauses = set()

        # Take all given clauses from all components
        for cl_id, _ in self.given_clauses_hist:
            if cl_id < 0:
                # Remove clauses when moved_from_active_to_passive
                given_clauses.remove(abs(cl_id))
            else:
                given_clauses.add(cl_id)

        assert given_clauses.issubset(self.given_clauses)

        return given_clauses
        

    def proof_out(self, json_msg):
        if self.create_train_mode:
            self.train_given_clauses = self.current_given_clauses()

            proof_clauses = set(json_msg['clause_ids'])

            self.proof_clauses_basic_ids = {self.clauses_basic_id[cl_id] for cl_id in proof_clauses}

            # Create examples
            covered_basic_ids = set()

            for cl_id in sorted(self.train_given_clauses):
                basic_cl_id = self.clauses_basic_id[cl_id]

                # Add only clauses that are not covered_basic_ids
                if basic_cl_id not in covered_basic_ids:
                    if basic_cl_id in self.proof_clauses_basic_ids:
                        self.train_examples.append((self.clauses[cl_id], True))
                        covered_basic_ids.add(basic_cl_id)
                    else:
                        self.train_examples.append((self.clauses[cl_id], False))
                        covered_basic_ids.add(basic_cl_id)

            #assert examples, [proof_clauses, given_clauses]
        
    def proc_json_msg(self, msg):
        json_msg = json.loads(msg.decode())

        tag = json_msg['tag']

        if tag == 'register_clauses':
            self.register_clauses(json_msg)
        elif tag == 'scores_req':
            self.scores_req(json_msg)
        elif tag == 'given_clause':
            self.given_clause(json_msg)
        elif tag == 'passive_clauses':
            pass
        elif tag == 'szs_result_out':
            pass
        elif tag == 'moved_from_active_to_passive':
            self.moved_from_active_to_passive(json_msg)
        elif tag == 'proof_out':
            self.proof_out(json_msg)

        elif tag == 'simplified_clauses':
            pass
        elif tag == 'server_queries_start':
            if not self.create_train_mode:
                self.server_queries_start()
        elif tag == 'scores_res':
            if not self.create_train_mode:
                raise
        else:
            #peername = writer.get_extra_info('peername')
            logprint(f'Wrong type of JSON query {json_msg["tag"]}') # {peername}')
            logprint(json_msg)


        self.wait_for_msg = True
    
        
def state_client(state_proc_id,
                 conn,
                 eval_queue,
                 eval_results_queue,
                 args,
                 logdebug,
                 loginfo,
                 ):
    try:

        state = State(state_proc_id=state_proc_id,
                      conn=conn,
                      eval_queue=eval_queue,
                      eval_results_queue=eval_results_queue,
                      args=args,
                      logdebug=logdebug,
                      loginfo=loginfo,
                      create_train_mode=False)

        if logdebug:
            logprint('New remote connected.')

        #while wait_for_msg:

        previous_msg = [b'']

        #with conn.makefile() as connfile:
        if True:
            while True:
                wait_for_msg = False

                try:

                    msg, rest_msg = readuntil(conn, previous_msg)
                    previous_msg = rest_msg

                    if not msg.strip():
                        if logdebug:
                            logprint('No msg.')
                        break
                except Exception as e:
                    logprint(f'Error reading msg {e}')
                    break

  
                if logdebug:
                    logprint(f'Msg JSON: {msg}')


                try:
                    state.proc_json_msg(msg)
                except Exception as e:
                    if loginfo:
                        logprint(f'JSON load error {e}')
                    if logdebug:
                        logprint(f'Msg JSON: {msg}')
                    break

    except Exception as e:
        logprint(f'Error state client {state_proc_id} {e}')
        conn.close()

    try:
        if args.evaldata_dir:
            with open(os.path.join(args.evaldata_dir, uuid.uuid4().hex), 'wb') as f:
                data_pickle = {'clauses': state.clauses,
                               'conjectures': state.conjectures,
                               'given_clauses': state.given_clauses,
                               'given_clauses_hist': state.given_clauses_hist,
                               'eval_data': state.eval_data,
                               }
                
                pickle.dump(data_pickle, f, pickle.HIGHEST_PROTOCOL)
            
    except:
        logprint(f'Error writing evals.')


def state_worker(i,
                 conn_queue,
                 eval_queue,
                 eval_results_queue,
                 args,
                 logdebug,
                 loginfo,
                 ):
    #import signal
    #signal.signal(signal.SIGINT, signal.SIG_IGN)
    if loginfo:
        logprint(f'State worker {i} initialized.')
    
    while True:
        try:
            conn = conn_queue.get()

            state_client(i,
                         conn,
                         eval_queue,
                         eval_results_queue,
                         args,
                         logdebug,
                         loginfo,
                         )
            
        except KeyboardInterrupt:
            if loginfo:
                logprint(f'Key interrupt')
            break
        except Exception as e:
            logprint(f'Error (worker {i}): {e}')
            #sys.exit(2)
            raise

def load_cnf_file(fname):
    d = cop.load_ipremsel(fname)

    if d:        
        data, (lens, labels, symbols) = d
    else:
        logprint('Error cop.load_ipremsel')
        raise

    return GraphData(data), (lens, labels)
    

            
def eval_files(msgs, network):
    try:
        # msg is (pool_proc_id, i, tmpfile.name)
        messages = [load_cnf_file(msg[2]) for msg in msgs]
    except Exception as e:
        logprint('Error pack data', e)
        traceback.print_exc()

        # CHANGE!!!
        return len(msgs) * ['ERROR\n'.encode()]

    try:
        pred = network.predict(messages)

    except Exception as e:
        logprint('Error network predict', e)

        for i, msg in enumerate(msgs):
            logprint(f'File {i} {msg[2]}')
            with open(msg[2], 'r') as f:
                for l in f:
                    logprint(l)
        
        traceback.print_exc()

        # CHANGE!!!
        return len(msgs) * ['ERROR\n'.encode()]
    
    assert len(pred) == len(msgs), [len(pred), len(msgs)]

    # join as tuples
    out_msgs = [m + (p,) for m, p in zip(msgs, pred)]

    return out_msgs

def gpu_worker(gpu_proc_id,
               eval_queue,
               eval_results_queue,
               args,
               logdebug,
               loginfo,
               ):
    #import signal
    #signal.signal(signal.SIGINT, signal.SIG_IGN)

    import torch
    ### Remove or keep???
    torch.set_num_threads(1)
    torch.set_num_interop_threads(1)

    #print(torch.__config__.parallel_info(), flush=True)

    #import torch.multiprocessing as mp

    from piegnn_torch_server import PIEGNN    
   
    network = PIEGNN(tuple(args.start_dims),
                     tuple(args.next_dims),
                     hidden_dim=16,
                     layers=args.layers,
                     residual=True,
                     normalization="layer",
                     pool_proc_id=gpu_proc_id,
                     )
    network.load_state_dict(torch.load(args.model, map_location=network.device))


    if loginfo:
        logprint(f'GPU worker {gpu_proc_id} initialized.')

    #counter = 0

    while True:
        #counter += 1 
        try:
            first_msg = eval_queue.get()

            all_msgs = [first_msg]

            for _ in range(min(eval_queue.qsize(), max(args.batch - 1, 0))):
                try:
                    all_msgs.append(eval_queue.get_nowait())
                except Exception as e:
                    if logdebug:
                        logprint(f'Eval queue empty {gpu_proc_id} {e}.')
                    break

            if loginfo:
                logprint(f'Worker: {gpu_proc_id} Batch size: {len(all_msgs)}')

            if args.zero_gpu_scores:
                msg_len = args.query_max_size + args.context_conjectures + args.context_given_clauses

                if args.zero_gpu_scores > 1:
                    for _, _, fname in all_msgs:
                        #_ = cop.load_ipremsel(fname)
                        _ = load_cnf_file(fname)

                results = [msg + (msg_len * ['0.0'],) for msg in all_msgs]
            else:
                with torch.inference_mode():
                    results = eval_files(all_msgs, network)

            for worker_id, query_pos, query_fname, result in results:
                eval_results_queue[worker_id].put((query_pos, query_fname, result))


        except KeyboardInterrupt:
            if loginfo:
                logprint(f'Key interrupt')
            break
        except Exception as e:
            logprint(f'Error (worker {i}): {e}')
            #sys.exit(2)

def logprint(*args, **kwargs):
    print(*args, **kwargs, flush=True)

# if args.verbose:
#     #logging.basicConfig(level=logging.DEBUG)
#     import logging
    
#     #logging.getLogger("asyncio").setLevel(logging.DEBUG)
#     logdebug = True
#     loginfo = True
# elif args.quiet:
#     #logging.basicConfig(level=logging.WARNING)
#     logdebug = False
#     loginfo = False  
# else:
#     #logging.basicConfig(level=logging.INFO)
#     logdebug = False
#     loginfo = True
    
# if logdebug:  
#     logprint(args)


if __name__ == "__main__":
    os.environ["OMP_NUM_THREADS"] = "1"
    os.environ["OPENBLAS_NUM_THREADS"] = "1"
    os.environ["MKL_NUM_THREADS"] = "1"
    os.environ["VECLIB_MAXIMUM_THREADS"] = "1"
    os.environ["NUMEXPR_NUM_THREADS"] = "1"
    
    parser = argparse.ArgumentParser(description='iProver server using a PyTorch model.',
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser_group = parser.add_mutually_exclusive_group()
    parser_group.add_argument("-v", "--verbose", action="store_true")
    parser_group.add_argument("-q", "--quiet", action="store_true")

    parser_group2 = parser.add_mutually_exclusive_group()
    parser_group2.add_argument("--first_context",
                               help='use the first given clauses as contexts',
                               action="store_true")
    parser_group2.add_argument("--last_context",
                               help='use the last given clauses as contexts',
                               action="store_true")
    
    parser.add_argument('--ip',
                    default='127.0.0.1',
                    type=str,
                    help='server IP')

    parser.add_argument('--port',
                    default=12300,
                    type=int,
                    help='server port')

    parser.add_argument('--state_workers',
                     default=48,
                     type=int,
                     help='the number of state workers')

    parser.add_argument('--gpu_workers',
                     default=32,
                     type=int,
                     help='the number of GPU workers')


    parser.add_argument('--stream_limit',
                     default=256 * 1024 * 1024,
                     type=int,
                     help='stream reader limit in bytes')

    #parser.add_argument('--wait_for',
    #                     default=1e-2,
    #                     type=float,
    #                     help='a worker waits for this amount of seconds if batch is not full')

    #parser.add_argument('--gpus',
    #                default=[],
    #                type=int,
    #                nargs='*',
    #                help='ids of gpus to use via CUDA_VISIBLE_DEVICES')

    parser.add_argument('--model',
                        type=str,
                        default='models/test.pt',
                        help='model filename')

    parser.add_argument('--start_dims',
                        type=int,
                        nargs='+',
                        default=[16, 16, 16],
                        help='start_dims')

    parser.add_argument('--next_dims',
                        type=int,
                        nargs='+',
                        default=[16, 16, 16],
                        help='next_dims')


    parser.add_argument('--layers',
                        default=10,
                        type=int,
                        help='the number of layers in the model')

    parser.add_argument('--batch',
                         default=1,
                         type=int,
                         help='batch size')

    parser.add_argument('--timeout',
                         default=60,
                         type=int,
                         help='message timeout')


    #parser.add_argument("-t",
    #                    "--traindata",
    #                    action="store_true",
    #                    help="we want to store train data")

    parser.add_argument("-z",
                        "--zero_scores",
                        action="store_true",
                        help="returns 0 scores without eval")

    parser.add_argument("-zgpu",
                        "--zero_gpu_scores",
                        default=0,
                        type=int,
                        help="0 means nothing, 1 adds return 0 scores by a GPU worker, 2 adds load cnf files and build GraphData")

    #parser.add_argument('--traindata_dir',
    #                    type=str,
    #                    default='proofs/',
    #                    help='where are the recorded proofs stored')

    parser.add_argument('--evaldata_dir',
                        type=str,
                        default='',
                        help='where are evals recorded')
    
    
    parser.add_argument('--query_max_size',
                     default=2048,
                     type=int,
                     help='the maximal size of a query for GNN (+contexts)')


    parser.add_argument('--context_conjectures',
                     default=512,
                     type=int,
                     help='the maximal number of other conjectures used for context')


    parser.add_argument('--context_given_clauses',
                     default=512,
                     type=int,
                     help='the maximal number of clauses used for context')

    parser.add_argument('--stop_after',
                     default=0,
                     type=int,
                     help='it stops the main loop after given number of seconds')

    

    args = parser.parse_args()

    if args.verbose:
        #logging.basicConfig(level=logging.DEBUG)
        import logging

        #logging.getLogger("asyncio").setLevel(logging.DEBUG)
        logdebug = True
        loginfo = True
    elif args.quiet:
        #logging.basicConfig(level=logging.WARNING)
        logdebug = False
        loginfo = False  
    else:
        #logging.basicConfig(level=logging.INFO)
        logdebug = False
        loginfo = True

    if logdebug:  
        logprint(args)

    
    #with Manager() as manager:
    conn_queue = Queue()
    eval_queue = Queue()

    eval_results_queue = dict()

    for i in range(args.state_workers):
        eval_results_queue[i] = Queue()

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((args.ip, args.port))
        # How many we wait for
        s.listen(1024)

        processes = []

        for i in range(args.gpu_workers):
            p = Process(target=gpu_worker,
                        args=(i,
                              eval_queue,
                              eval_results_queue,
                              args,
                              logdebug,
                              loginfo,
                              ))
            #p.daemon = True
            p.start()
            processes.append(p)

        for i in range(args.state_workers):
            p = Process(target=state_worker,
                        args=(i,
                              conn_queue,
                              eval_queue,
                              eval_results_queue,
                              args,
                              logdebug,
                              loginfo,                                  
                              ))
            p.start()
            processes.append(p)

        #try:
        start_time = time.time()
        while True:
            conn, addr = s.accept()
            if logdebug:
                logprint('Connected by', addr)
            conn_queue.put(conn)

            if args.stop_after:
                if time.time() - start_time > args.stop_after:
                    logprint(f'Stop after {args.stop_after}')
                    break
                
                # conn.close but after used by state worker!!!
        #except KeyboardInterrupt:
        #    loginfo("KeyboardInterrupt")

        #print('Sleep', flush=True)
        #time.sleep(10) 
        #print('After sleep', flush=True)       
        for p in processes:
            logprint('Kill')
            p.kill()

        s.shutdown(socket.SHUT_RDWR)
        s.close()

        logprint('Socket closed.')
