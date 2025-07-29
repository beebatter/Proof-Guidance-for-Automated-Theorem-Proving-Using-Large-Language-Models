#!/usr/bin/env python3
"""
Extract training data from iProver logs.
"""

import os

os.environ["OMP_NUM_THREADS"] = "1"
os.environ["OPENBLAS_NUM_THREADS"] = "1"
os.environ["MKL_NUM_THREADS"] = "1"
os.environ["VECLIB_MAXIMUM_THREADS"] = "1"
os.environ["NUMEXPR_NUM_THREADS"] = "1"

import sys
sys.path.append('../server/')

from iserver_mp2 import State, preprocess_clause

import argparse
import os
import lzma
import orjson as json
import logging
from collections import Counter

parser = argparse.ArgumentParser(description='Extract training data from iProver logs.',
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser_group = parser.add_mutually_exclusive_group()
parser_group.add_argument("-v", "--verbose", action="store_true")
parser_group.add_argument("-q", "--quiet", action="store_true")

parser.add_argument('input_file',
                type=str,
                help='input file')

parser.add_argument('output_dir',
                type=str,
                help='output directory')

parser.add_argument('--input_problems',
                default='all_problems_noint',
                type=str,
                help='input problems for conjectures')

parser.add_argument("-z",
                    "--zero_scores",
                    action="store_true",
                    help="returns 0 scores without eval")

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



args = parser.parse_args()

if args.verbose:
    logging.basicConfig(level=logging.DEBUG)
elif args.quiet:
    logging.basicConfig(level=logging.WARNING)
else:
    logging.basicConfig(level=logging.INFO)
  
logging.debug(args)


def cut_clause(s):
    return preprocess_clause(s)

def example_line(long_cnf, example_type):
    cnf = cut_clause(long_cnf)

    if example_type is True:
        cnf_type = 'axiom_useful'
    elif example_type is False:
        cnf_type = 'axiom_redundant'

    # Consider these examples as redundant
    elif example_type is None:
        #cnf_type = 'axiom_unclear'
        cnf_type = 'axiom_redundant'
    else:
        raise

    cnf_split = cnf.split(',')
    cnf_split[1] = cnf_type

    return ','.join(cnf_split).rstrip()+'\n'

with lzma.open(args.input_file) as fin:
    json_input = b''
    previous_sockets = b''
    read_json = False

    state = State(create_train_mode=True, args=args)
    
    for l in fin:
        if l.startswith(b'simplify_new:smt:'):
            pass        
        elif l.startswith(b'Sockets:trace:'):
            logging.debug(l)
            if previous_sockets:
                if json_input:
                    read_json = True
                else:
                    assert b'reading' or b'server_queries_end' in previous_sockets, [previous_sockets]

            previous_sockets = l

        elif l.startswith(b'%') or l.startswith(b'---') or l.startswith(b'warning'):
            if json_input:
                read_json = True
            else:
                assert b'reading' or b'server_queries_end' in previous_sockets, [previous_sockets]

            previous_sockets = b''

        elif previous_sockets:
            json_input += l

        else:
            pass

        if read_json:

            state.proc_json_msg(json_input)
            
            json_input = b''
            read_json = False

problem_name = os.path.basename(args.input_file).split('.')[0]

with open(os.path.join(args.input_problems, problem_name), 'r') as fconjecture:
    for l in fconjecture:
        if 'conjecture' == l.split(',')[1].strip():
            conjecture = l.rstrip() + '\n'
            break
        
output_fname = os.path.join(args.output_dir, problem_name)

assert not os.path.isfile(output_fname)

if state.train_examples:
    # We have a positive example
    assert {cl_id for cl_id, cl_id_type in state.train_examples if cl_id_type}, [args.input_file]
    
    with open(output_fname, 'w') as fout:
        fout.write(conjecture)

        for cnf, cnf_type in state.train_examples:
            fout.write(example_line(cnf, cnf_type))

def cl_id_type(state, cl_id):
    basic_cl_id = state.clauses_basic_id[cl_id]    
    if basic_cl_id in state.proof_clauses_basic_ids:
        return True
    elif cl_id in state.train_given_clauses:
        return False
    else:
        return None

for i, query in enumerate(state.train_queries):
    query_types = [cl_id_type(state, cl_id) for cl_id in query]
    logging.debug(Counter(query_types))

    query_output_fname = output_fname + '.' + str(i)

    assert not os.path.isfile(query_output_fname)    

    with open(query_output_fname, 'w') as fout:
        fout.write(conjecture)

        for cl_id, cnf_type in zip(query, query_types):
            fout.write(example_line(state.clauses[cl_id], cnf_type))

    
    
