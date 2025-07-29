#!/bin/python3 -u

import sys
import os
import re
import subprocess
import collections
from timeit import default_timer as time_now
from iprover_process_module import *

# The heuristic specification files
HEUR_SPEC_SAT = "heur_conf_sat_0"

#HEUR_SPEC_LTB = "heur_conf_ltb_0"
#HEUR_SPEC_LTB_INITIAL = "heur_conf_ltb_initial"
#HEUR_SPEC_LTB = "heur_conf_ltb_v2"
#HEUR_SPEC_LTB_INITIAL = "heur_conf_ltb_initial_v2"
HEUR_SPEC_LTB = "heur_conf_ltb_v4"
HEUR_SPEC_LTB_INITIAL = "heur_conf_ltb_initial_v3"

#HEUR_SPEC_FEW = "heur_conf_few_0"
HEUR_SPEC_FEW = "heur_conf_few_1" # With learnt+sched heuristics
HEUR_SPEC_FEW_INITIAL = "heur_conf_few_initial" # UNUSED!


# The minimum amount of time we run each problem for in phase 2
MIN_ATTEMPT_TIME = 32

# Mode for suppressing unnecessary output during the competition
#SUPPRESS_OUTPUT = True

#KK
SUPPRESS_OUTPUT = True

# Outputs help message when LTB mode is incorrectly called
def print_ltb_input_arg_error():
    print("")
    print("Usage: python3 {0} LTB LTB.batch Output_Dir".format(sys.argv[0]))
    print("LTB.batch is a batch specification file see CASC-J6 description.")
    print("")
    sys.exit(1)


# Assigns and checks the LTB input arguments
def check_ltb_input_args():

    print_help = False

    # Check the number of arguments
    if not len(sys.argv) == 5:
        print("ERR: Incorrect number of arguments")
        print_ltb_input_arg_error()

    # Check if batchfile exists
    if os.path.isfile(sys.argv[2]):
        batch_file = sys.argv[2]
    else:
        print("ERR: Batchfile does not exist")
        print_help = True

    # Check if output directory exists:
    if os.path.isdir(sys.argv[3]):
        output_dir = sys.argv[3]
    else:
        print("ERR: Output direcotry does not exist")
        print_help = True

    batch_path = sys.argv[4]
    if batch_path[-1] != "/":
        batch_path += "/"


    if print_help:
        print_ltb_input_arg_error()

    return batch_file, output_dir, batch_path


# Reads a batchfile and reports/returns the timelimit and the batch problems
def read_batch_file(batch_file):

    # This function implements a few shortcuts as we know that in CASC-27
    # - Problems are unordered
    # - output.required Proof
    # - limit.time.problem.wc 0  # No problem timelimit


    # Flag for helping to check what line we are on such that
    # problems will be read correctly
    read_batch = False

    # List for holding the batches
    batch = []
#KK
    #batch_dir=os.path.dirname(os.path.realpath(batch_file))

    # Read batch file
    with open(batch_file, 'r') as bf:

        for line in bf:
            # Obtain time limit
            if re.match("limit.time.overall.wc", line):
                time_limit = int(line.split()[1])
            # Set starting to read batch
            if line == "% SZS start BatchProblems\n":
                read_batch = True
                # List for holding the read problem processes
                problems = []
            # Set stopping to read batch
            elif line == "% SZS end BatchProblems\n":
                read_batch = False
                batch += [(time_limit, problems)]
            elif read_batch:
                # Get path and problem name
                try:
                    problem = line.split()
                    problems += [ProbProcLTB(problem[1], problem[0])]
#KK
#                    problems += [ProbProcLTB(problem[1], batch_dir+'/'+problem[0])]
                except IndexError:
                    pass
    return batch


# Output some brief output statistics to stdout
def compute_end_ltb_stats(ltb_results):

    no_solved = 0
    tot_solved_time = 0

    for res in ltb_results:
        if res.solved:
            no_solved += 1
            tot_solved_time += res.solved_time

    print("")
    print("")
    print("# # # # # # # # # # # # # # # # # # #")
    print("")
    print("Number of problems: {0}".format(len(ltb_results)))
    print("Number of solved problems: {0}".format(no_solved))
    print("AVG CPU time per solved problem: {0:.2f}".format(tot_solved_time / no_solved))
    print("")


# Start running the prover on the problems in the batchfile
def run_prover_on_batch(start_time, global_time_limit, problems, output_dir, batch_path):

    # Holds the CPU time used in this problem iteration
    global_cpu = 0


    ## # # # # # # # # # # # # # # # # # #
    ## Run the Initial low-timelimit loop!

    for num, problem in enumerate(problems):

        # No need to re-solve already solved problems (This should never be triggered in the initial)
        if problem.solved:
            continue

        if not SUPPRESS_OUTPUT:
            print("")
            print("")
            print("#################################################")
            print("")
            print("Initial Run")
            print("")


        # Run the prover on the problem
        # Quick fix, set the timeout to X>10  seconds, while it can mac only take ~10
        prover_res = setup_and_start_prover(problem, 12, HEUR_SPEC_LTB_INITIAL, ltb_mode=True, initial_run=True, output_dir=output_dir, batch_path=batch_path)

        # Store problem prover output
        problems[num] = prover_res

        # Update the global cpu within this batch session
        global_cpu += prover_res.tot_cpu

        if not SUPPRESS_OUTPUT:
            print("")
            print("% % % % % % % % % %")
            print("Global WC:  {0:.2f}".format(time_now() - start_time))
            print("Global CPU: {0:.2f}".format(global_cpu))

    if not SUPPRESS_OUTPUT:
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        print()
        print("Number of problems solved by initial run: {0}".format(len([p for p in problems if  p.solved])))
        print("Global WC:  {0:.2f}".format(time_now() - start_time))
        print()
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

    ###
    ### Run the Full Timelimit loop!

    # Get the number of problems remaining
    no_prob_remaining = len([p for p in problems if not p.solved])

    for num, problem in enumerate(problems):

        # No need to re-solve already solved problems!
        if problem.solved:
            continue

        if not SUPPRESS_OUTPUT:
            print("")
            print("")
            print("#################################################")
            print("")
            print("Long Run")
            print("")

        # Compute the time available for this problem, based on the number
        # of problems left and the wallclock time left.
        time_remaining = start_time + global_time_limit - time_now()
        prob_time_avail = min(max(time_remaining / no_prob_remaining, MIN_ATTEMPT_TIME), time_remaining)

        # Run the prover on the problem
        prover_res = setup_and_start_prover(problem, prob_time_avail, HEUR_SPEC_LTB, ltb_mode=True, rolling_run=True, output_dir=output_dir, batch_path=batch_path)

        # Store problem prover output
        problems[num] = prover_res

        # Decrement the number of problems remaining in this session
        no_prob_remaining -= 1

        # Update the global cpu within this batch session
        global_cpu += prover_res.tot_cpu


        if not SUPPRESS_OUTPUT:
            print("")
            print("% % % % % % % % % %")
            print("Global WC:  {0:.2f}".format(time_now() - start_time))
            print("Global CPU: {0:.2f}".format(global_cpu))


    return problems


# Start running the prover on the problems in the batchfile
def run_few_schedule(problem, global_time_limit):



    # Holds the CPU time used in this problem iteration
    global_cpu = 0

    # Start the time
    start_time = time_now()

    """
    ## # # # # # # # # # # # # # # # # # #
    ## Run the Initial low-timelimit loop!

    if not SUPPRESS_OUTPUT:
        print("")
        print("")
        print("#################################################")
        print("")
        print("Initial Run")
        print("")


    # Run the prover on the problem
    # Quick fix, set the timeout to X>10  seconds, while it can mac only take ~10
    prover_res = setup_and_start_prover(problem, 12, HEUR_SPEC_FEW_INITIAL, initial_run=True)

    if prover_res.solved:
        sys.exit(0)
    """

    ###
    ### Run the Full Timelimit loop!

    # Get the number of problems remaining

    if not SUPPRESS_OUTPUT:
        print("")
        print("")
        print("#################################################")
        print("")
        print("Long Run")
        print("")

    # Compute the time available for this problem, based on the number
    # of problems left and the wallclock time left.
    prob_time_avail = start_time + global_time_limit - time_now()

    # Run the prover on the problem
    prover_res = setup_and_start_prover(problem, prob_time_avail, HEUR_SPEC_FEW, rolling_run=True)

    if not SUPPRESS_OUTPUT:
        print("")
        print("% % % % % % % % % %")
        print("Global WC:  {0:.2f}".format(time_now() - start_time))


    return problem



def ltb_mode():

    # Read and validate the provided arguments
    batch_file, output_dir, batch_path = check_ltb_input_args()

    # Read the batchfile and get the overall timelimit and the problems
    batches = read_batch_file(batch_file)

    if not SUPPRESS_OUTPUT:
        print("Number of batches: {0}".format(len(batches)))

    # Run over each batch
    for time_limit, problems in batches:

        if not SUPPRESS_OUTPUT:
            print("  " * 15)
            print("&&" * 15)
            print("Start batch")
            print("Timelimit: {0}".format(time_limit))
            print("Number of problems: {0}".format(len(problems)))
            print("&&" * 15)


        # Start the time
        start_time = time_now()

        # Perform the prover solving on a per batch basis
        problems = run_prover_on_batch(start_time, time_limit, problems, output_dir, batch_path)

        if not SUPPRESS_OUTPUT:
            # Report reults
            compute_end_ltb_stats(problems)


def check_sat_input_args():

    print_help = False

    # Check the number of arguments
    if not len(sys.argv) == 4:
        print("ERR: Incorrect number of arguments")
        print_sat_input_arg_error()

    # Check problem exists
    if os.path.isfile(sys.argv[2]):
        problem = sys.argv[2]
    else:
        print("Err: Problem file does not exist")
        print_help = True

    # Get the timeout
    try:
        time_limit = float(sys.argv[3])
    except ValueError as ex:
        print("Err: {0} cannot be converted to a float: {1}".format(sys.argv[3], ex))
        print_help = True

    if print_help:
        print_sat_input_arg_error()

    return problem, time_limit


def print_sat_input_arg_error():
    print("")
    print("Usage: python3 {0} SAT problem.p timelimit".format(sys.argv[0]))
    print("")
    sys.exit(1)


# Function to read a string in a heuristic configuration file
def read_heuristic_string(heuristic_path):
    with open(heuristic_path) as heuristic:
        heur_config = heuristic.readline().strip("\n")
    return heur_config


# Function to get the heuristic paths and store the heuristic
# name together with the heuristic configuration.
def get_heuristic_path(heur_spec_file):

    # Store file_name- heur string as list of tuples
    heur_mod = []

    # Open specification file for reading the heuristics to run
    with open("heuristics/heur_conf/"+ str(heur_spec_file), "r") as spec_file:
        # For each heuristic in the specification file
        for heur in spec_file:

            # Check that it is not an empty string
            heur = str(heur).strip("\n")
            if not heur.isspace():

                # Read the heuristic configuration
                heur_config = read_heuristic_string("heuristics/heur_mod/" + heur)

                # Store the heuristic name and the heuristic configuration
                heur_mod += [(heur, heur_config)]

    return heur_mod


# Start running a given problem with prover processes
def setup_and_start_prover(prob_proc, time_limit, heur_spec_file, ltb_mode=False, initial_run=False, rolling_run=False, output_dir=None, batch_path=None):

    # Read the and get the heuristic modifications
    heur_spec = get_heuristic_path(heur_spec_file)

    # Start Problem
    if ltb_mode:
        print("% SZS status Started for {0}+1.p".format(prob_proc.name))
        print("% SZS status Started for {0}+2.p".format(prob_proc.name))

    # Get number of processor cores available
    no_cores = get_no_cores()

    # Start running prover processes on the problem
    res = run_prover(prob_proc, heur_spec, time_limit, no_cores, ltb_mode=ltb_mode, initial_run=initial_run, rolling_run=rolling_run, batch_path=batch_path)


    # If the problem was solved, output the proof
    if prob_proc.solved and ltb_mode:
        output_proof(prob_proc, output_dir)
    elif prob_proc.solved:
#        print(prob_proc.proof.decode("utf-8"))
# KK
        print(prob_proc.proof)

    # End Problem
    if ltb_mode:
        print("% SZS status Ended for {0}+1.p".format(prob_proc.name))
        print("% SZS status Ended for {0}+2.p".format(prob_proc.name))

    return res


# Output the proof to the given directory
def output_proof(prob_proc, output_dir):
    # Ensure backslash on the directory
    if output_dir[-1] != "/":
        output_dir += "/"

    with open(output_dir + prob_proc.name + '.s', 'w') as f:
        f.write(prob_proc.proof)


# Creates a ProblemProcess for the sat mode
def get_sat_prob_proc(problem_path):
    return ProbProc(extract_problem_name_from_path(problem_path), problem_path)


# Extracts a typical problem name from a path
def extract_problem_name_from_path(problem_path):
    return problem_path.split("/")[-1].split(".")[0]


# Runs the sat mode of the prover
def sat_mode():

    # Check and validate input arguments
    problem_path, time_limit = check_sat_input_args()

    # Construct a problem process
    prob_proc = get_sat_prob_proc(problem_path)

    # Run the prover
    setup_and_start_prover(prob_proc, time_limit, HEUR_SPEC_SAT)


# Runs the FEW mode of the prover
def few_mode():

    # Check and validate input arguments
    problem_path, time_limit = check_sat_input_args()

    # Construct a problem process
    prob_proc = get_sat_prob_proc(problem_path)

    # Run the prover
    run_few_schedule(prob_proc, time_limit)




# Prints out an error message when a correct mode is not selected.
def mode_select_error(help_str):
    print("*******************")
    print("#  #   ERROR   #  #")
    print(help_str)
    print("help: this.py [sat|ltb|few]")
    print("*******************")
    sys.exit(1)


# Run Program

#KK
#print("__name__",__name__)

if __name__ == "__main__":

    # Check if at least one argument is provided
    if len(sys.argv) == 0:
        print("% No mode provided")
        mode_select_error("No mode provided")

    # Check if we have entered SAT or LTB mode
    if sys.argv[1] == "sat" or sys.argv[1] == "SAT":
        print("% sat_mode")
        sat_mode()
    elif sys.argv[1] == "ltb" or sys.argv[1] == "LTB":
        print("% ltb_mode")
        ltb_mode()
    elif sys.argv[1] == "few" or sys.argv[1] == "FEW":
        print("% few_mode")
        few_mode()
    else:
        print("% mode_select_error")
        mode_select_error("Invalid mode provided: {0}".format(sys.argv[1]))

