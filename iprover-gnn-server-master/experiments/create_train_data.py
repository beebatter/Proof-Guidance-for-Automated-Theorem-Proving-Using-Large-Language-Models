#!/usr/bin/env python3
"""
Extract training data from iProver logs.
"""

import argparse
import os
import lzma
import orjson as json
import logging

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


args = parser.parse_args()

if args.verbose:
    logging.basicConfig(level=logging.DEBUG)
elif args.quiet:
    logging.basicConfig(level=logging.WARNING)
else:
    logging.basicConfig(level=logging.INFO)
  
logging.debug(args)


clauses = dict()

# Syntactically the same clauses have the same
clauses_basic_id = dict()

conjectures = set()

given_clauses_hist = list()

examples = list()

def cut_clause(s):
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

def example_line(long_cnf, example_type):
    cnf = cut_clause(long_cnf)

    if example_type:
        cnf_type = 'axiom_useful'
    else:
        cnf_type = 'axiom_redundant'

    cnf_split = cnf.split(',')
    cnf_split[1] = cnf_type

    return ','.join(cnf_split).rstrip()+'\n'

with lzma.open(args.input_file) as fin:
    json_input = b''
    previous_sockets = b''
    read_json = False
    
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
            try:
                json_data = json.loads(json_input.decode())
            except Exception as e:
                logging.error(e)
                logging.error(json_input)
                raise

            tag = json_data['tag']
            logging.debug(f'Tag: {tag}')

            if tag == 'register_clauses':
                for cl in json_data['clauses']:
                    cl_id = cl['clause_id']
                    cl_clause = cl['clause']
                    cl_isconj = cl['clause_features']['conj_dist'] == 0
                    basic_cl_id = cl['clause_features']['basic_clause_id']
                    
                    assert cl_id not in clauses

                    clauses[cl_id] = cl_clause
                    clauses_basic_id[cl_id] = basic_cl_id
                    

                    if cl_isconj:
                        conjectures.add(cl_id)
            elif tag == 'given_clause':
                assert len(json_data['clause_ids']) == 1

                clause_id = json_data['clause_ids'][0]
                component_id = json_data['component_id']

                given_clauses_hist.append((clause_id, component_id))
                
            elif tag == 'moved_from_active_to_passive':
                # Remove from given clauses???
                clause_ids = json_data['clause_ids']
                component_id = json_data['component_id']

                for clause_id in clause_ids:
                    given_clauses_hist.append((-clause_id, component_id))
            elif tag == 'proof_out':
                given_clauses = set()

                # Take all given clauses from all components, correct?
                for cl_id, _ in given_clauses_hist:
                    if cl_id < 0:
                        # Remove clauses when moved_from_active_to_passive
                        given_clauses.remove(abs(cl_id))
                    else:
                        given_clauses.add(cl_id)



                proof_clauses = set(json_data['clause_ids'])

                proof_clauses_basic_ids = {clauses_basic_id[cl_id] for cl_id in proof_clauses}

                # Create examples
                covered_basic_ids = set()
                                   
                for cl_id in sorted(given_clauses):
                    basic_cl_id = clauses_basic_id[cl_id]
                                    
                    # Add only clauses that are not covered_basic_ids
                    if basic_cl_id not in covered_basic_ids:
                        if basic_cl_id in proof_clauses_basic_ids:
                            examples.append((clauses[cl_id], True))
                            covered_basic_ids.add(basic_cl_id)
                        else:
                            examples.append((clauses[cl_id], False))
                            covered_basic_ids.add(basic_cl_id)

                #assert examples, [proof_clauses, given_clauses]
                #print(examples, [proof_clauses, given_clauses])
                                    
            elif tag == 'scores_req':
                pass
            elif tag == 'scores_res':
                pass
            elif tag == 'passive_clauses':
                pass
            elif tag == 'szs_result_out':
                pass
            elif tag == 'simplified_clauses':
                pass
            elif tag == 'server_queries_start':
                pass
            else:
                logging.error(f'Wrong type of JSON query {json_data["tag"]}')
                logging.error(json_data)
            
            

            json_input = b''
            read_json = False

if examples:
    # We have a positive example
    assert {cl_id for cl_id, cl_id_type in examples if cl_id_type}, [args.input_file]
    
    problem_name = os.path.basename(args.input_file).split('.')[0]

    with open(os.path.join(args.input_problems, problem_name), 'r') as fconjecture:
        for l in fconjecture:
            if 'conjecture' == l.split(',')[1].strip():
                conjecture = l.rstrip() + '\n'

    output_fname = os.path.join(args.output_dir, problem_name)

    assert not os.path.isfile(output_fname)

    with open(output_fname, 'w') as fout:
        fout.write(conjecture)

        for cnf, cnf_type in examples:
            fout.write(example_line(cnf, cnf_type))
