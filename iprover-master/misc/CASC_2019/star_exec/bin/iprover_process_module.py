import subprocess
from platform import system
from timeit import default_timer as time_now
from time import sleep
import os
import re
import signal
import time
import sys

# The prover executable
#PROVER = './iproveropt_new'
PROVER = './iproveropt'
#PROVER = 'iproveropt_2019_7_29'
#PROVER = 'iproveropt_2019_7_24'

# The max time for running a problem
MAX_PROBLEM_TIME = 300
# The min time for running a problem
MIN_PROBLEM_TIME = 60

# The time for running the initial problems,
# set to 10s such that we only have very har problems left.
INITIAL_RUNNING_TIME = 10

# When in "rolling mode" we keep on quickly terminating and switching between heuristics
# The timelimit is for how long we run the rolling heuristic for, while the
# heuristic_start is at which heuristic we start rolling.
# If this value is X, we only start rolling heuristics after starting up the X initial heuristics.
ROLLING_HEURISTIC_START = 6
ROLLING_HEURISTIC_TIMELIMIT = 24

# Mode for suppressing unnecessary output during the competition
#SUPPRESS_OUTPUT = True
#KK
SUPPRESS_OUTPUT = True


# Mode for printing out the process output of the heuristics.
# The overall output (above) needs to be false (non-suppressed)
# to print out the output.
PROCESS_OUTPUT = False


## Class Problem Process
# This class is used to represent a problem and basic stats about if/how it is solved.
class ProbProc():

    def __init__(self, problem_name, problem_path):
        self.name = problem_name
        self.path = problem_path

        self.tot_cpu = 0
        self.tot_wc = 0

        self.solved = False
        self.szs = None
        self.solved_heur = None
        self.solved_time = -1

        self.proof = ""

    # Return the problem path
    def get_path(self):
        return self.path


# Special instance for LTB problems as CASC27 have problem versions
class ProbProcLTB(ProbProc):

    # Give the problem path with the '*' (CASC27)
    def __init__(self, problem_name, problem_path):
        super().__init__(problem_name, problem_path)
        # Use a flag as there are only 2 available path
        self.path_switch = False


    # Return the problem path version for LTB problems
    def get_path(self):
        # In CASC27 we allow _1 and _2 extensions for our format
        if self.path_switch:
            path = self.path.replace('*', '+1')
        else:
            path = self.path.replace('*', '+2')

        # Inverse the switch flag
        self.path_switch = not self.path_switch
        return path


# Get the number of cores available
def get_no_cores():
    #return 4 #CASC'17 starexec
    return 8 # StarExec Miami


# Method for starting the prover on a problem with the given heuristic
# The subprocess starts in to background and send the output to the process pipe
def start_prover_process(prob_proc, heur_string, time_limit, batch_path=None):

    # Insert time limit into the heuristic, in case it is incorporated with the clausifier
    heur_string = heur_string.format(time_limit)

    problem_version_name = prob_proc.get_path()

    if batch_path is not None:
        problem_version_name = batch_path + problem_version_name


    # The execution string
    # (2 second grace for TreeLimitedRun)
    cmd = 'ulimit -s 200000; ./TreeLimitedRun -q1 0 {4:.2f} {0} {3} --time_out_real {1:.2f} {2}'.format(PROVER, time_limit, problem_version_name, heur_string, time_limit+2)

    cmd = 'ulimit -s 200000; ./TreeLimitedRun -q1 0 {4:.2f} {0} {3} --out_options none --stats_out none --time_out_real {1:.2f} {2}'.format(PROVER, time_limit, problem_version_name, heur_string, time_limit+2)

    tmp_file = "tmp/{0}".format(time.time())
    cmd += '  >> ' + str(tmp_file)

    # Start the proving process
    #proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, preexec_fn=os.setsid)
    ## Do not PIPE output - send to file
    proc = subprocess.Popen(cmd, stderr=subprocess.PIPE, shell=True, preexec_fn=os.setsid)

    # Return the prover and when the process was started
    #return proc, time_now(), problem_version_name
    return proc, time_now(), problem_version_name, tmp_file


# Function for returning whether a terminated prover run was successfull or not
def check_prover_success(szs_status, ltb_mode=False):

    if re.match('(Theorem|Unsatisfiable)', szs_status):
        return True
    else:
        return False

    # TODO just run it as this due to some issues with FEW
    """
    # In ltb mode unsat and theorem are the only answers as we can ge sat
    # by saturating the reduced axiom set
    if ltb_mode:
        if re.match('(Theorem|Unsatisfiable)', szs_status):
            return True
        else:
            return False
    # Standard zsz ontology success check
    else:
        if re.match('(Theorem|Unsatisfiable|Satisfiable|CounterSatisfiable)', szs_status):
            return True
        else:
            return False
    """


# Function for checking which processes in a process dictionary that is still running
# and terminated that process group (parent and children). the process dictionary
# is further updated.
def kill_process(proc_dict):

    # Variable to store the used CPU time of the killed processes
    cpu_time = 0

    # Loop through every process in the list
    proc_list = list(proc_dict.items())
    for heur, proc_tuple in proc_list:

        # Access the process
        proc = proc_tuple[0]

        # If process is still active
        if proc.poll() is None:
            try:
                # Get process group id and kill the group
                pgrp = os.getpgid(proc.pid)
                os.killpg(pgrp, signal.SIGTERM)

                # Get running time of the killed process
                proc_time = time_now() - proc_tuple[1]
                cpu_time += proc_time

                # Process has been terminated, remove it from from the process dict
                del proc_dict[heur]

            except ProcessLookupError:
                # Process terminated before we could terminate it. The cpu time and proc_dict update
                # therefore does not occur. The terminated process should be handled by the the
                # terminated process handler function
                pass

    # Return CPU time and the updated process overview
    return cpu_time, proc_dict


# Get the running time and szs status of the output of a terminated prover run.
# The expected prover output is a bytestring.
def get_output_status(prover_out, err):

    try:
        # Get the iProver computed running time
        #running_time = re.search(b'total_time: \s+ \d+\.\d*', prover_out).group(0)
        #running_time = re.search('total_time:\s+ \d+\.\d*', prover_out).group(0)
        #running_time = float(running_time.split()[-1])
        running_time = 0 # Hack

        # Get the SZS status
        #szs_status = re.search(b'% SZS status .*', prover_out).group(0)
        szs_status = re.search('% SZS status.*', prover_out).group(0)
        #szs_status = szs_status.split()[3].decode("utf-8")
        szs_status = szs_status.split()[3]

    except AttributeError:
        # Somehow, it has happened that the running time cannot be found in the output string
        # Try to signal this by setting runnning time to 0
        running_time = 0
        szs_status = "SZSErrUnknown"

#KK
#    print(szs_status) #TESTY

    return running_time, szs_status


# Get the total CPU time of prover processes that has terminated but not been handled.
# The function assumes that no running processes are in the proc_dict as this would
# cause .communicate to wait until termination.
def get_terminated_prover_times(proc_dict):

    # The total CPU time spent on the terminated prover runs
    cpu_time = 0
    proc_list = list(proc_dict.items())
    for heuristic, proc_tuple in proc_list:

        # Get prover output
        proc = proc_tuple[0]
#        prover_out, err = proc.communicate()

#KK
        prover_out, err = proc.communicate(timeout=300)
        # Get and sum CPU time
        running_time, _ = get_output_status(prover_out, err)

        # Remove the process from the process overview
        # as it has temrinated and been handled.
        del proc_dict[heuristic]

    return cpu_time, proc_dict


# Update a problem process which has successfully terminated
def update_problem_solving_stats(prob_proc, running_time, szs_status, heur_name, prover_output):

    prob_proc.solved = True
    prob_proc.szs = szs_status
    prob_proc.proof = prover_output

    prob_proc.solved_heur = heur_name
    prob_proc.solved_time = running_time

    return prob_proc


# Kills running processes and updates the CPU times for cleaning up
def handle_and_kill_processes(prob_proc, proc_dict):

    # Kill all the running processes and update the CPU used time
    process_time, proc_dict = kill_process(proc_dict)
    prob_proc.tot_cpu += process_time

    # TODO HACK : we remove this after introducig output files
    # Some processes may have terminated before starting the killing processes
    # Hence we read their output and update the CPU time
    #terminated_time, proc_dict = get_terminated_prover_times(proc_dict)
    #prob_proc.tot_cpu += terminated_time

    return prob_proc, proc_dict


# This function looks for terminate dprocesses, and if they exist handles them.
# If a successfull prover run is found, the function terminates all other processes.
# Simultaneously it handles the overall CPU time spent on the processes
def handleTerminatedProcesses(prob_proc, proc_dict, provers_running, ltb_mode):

    # Loop through all the processes currently running
    proc_list = list(proc_dict.items())
    for heur_index, proc_tuple in proc_list:

        # If process has finished
        if proc_tuple[0].poll() is not None:



            if not SUPPRESS_OUTPUT and PROCESS_OUTPUT:
                print("Finished: {0}".format(proc_tuple[2]))

            # Delete process from the running process overview
            del proc_dict[heur_index]

            # Decrement the number of processes running
            provers_running -= 1

            try:
                # Aquire the prover output
                # Add a timeout, as the clausifier occassionally doesn't terminates
                # and insteads hangs
#                prover_out, err = proc_tuple[0].communicate(timeout=5)
#KK
                # Replace by reading from file
                prover_out, err = proc_tuple[0].communicate(timeout=300)
#KK
#                print(err)

                tmp_file = proc_tuple[-1]
#KK
#                print(tmp_file)

                with open(tmp_file, 'r') as f:
                    prover_out = f.read()
                    err = ""
#KK
                #print(prover_out) #TESTY

                running_time, szs_status = get_output_status(prover_out, err)


                # Update the CPU running time
                prob_proc.tot_cpu += running_time

                # If the prover run was successfull
                if check_prover_success(szs_status, ltb_mode):

#                    print("SUCCESS")

                    # TODO is this actually used? And is it still necessary?
                    # Set Proving stats
                    prob_proc = update_problem_solving_stats(prob_proc, running_time, szs_status, proc_tuple[2], prover_out)

                    # Return prover has terminated with success
                    return provers_running, True, proc_dict, proc_tuple[3] ## TODO just add this on the end for now
            except subprocess.TimeoutExpired:
                # This exception is thrown from when .communicate has timed out.
                # That results in a unsuccessfull prover run, which we make sure to kill
                try:
                    pgrp = os.getpgid(proc_tuple[0].pid)
                    os.killpg(pgrp, signal.SIGTERM)
                except ProcessLookupError:
                    # The process terminated before we could terminate it
                    pass
            except Exception as err:
                print("Unknown Error")
                print(err)


    # All provers processes are running
    return provers_running, False, proc_dict, "" ## TODO empty string because of the problem versions and quick-fix


# Function for reporting the result of an ended prover run
def report_result(prob_proc, ltb_mode):

    if not ltb_mode:
        # Report all stats on screen
        print("")
        print("Problem: {0}".format(prob_proc.name))
        print("Tot WC: {0:.2f}".format(prob_proc.tot_wc))
        print("Tot CPU: {0:.2f}".format(prob_proc.tot_cpu))

        if prob_proc.solved:
            print("% SZS status {0} for {1}".format(prob_proc.szs, prob_proc.name))
            print("Solved by: {0}".format(prob_proc.solved_heur))
            print("Solved time: {0:.2f}".format(prob_proc.solved_time))
        else:
            print("% SZS status GaveUp for {0}".format(prob_proc.name))

    else:
        # Report main stats and return the problem procedure object to call
        print("")
        print("Problem: {0}".format(prob_proc.name))
        print("Tot WC: {0:.2f}".format(prob_proc.tot_wc))

        if prob_proc.solved:
            print("% SZS status {0} for {1}".format(prob_proc.szs, prob_proc.name))
            print("Solved by: {0}".format(prob_proc.solved_heur))
        else:
            print("% SZS status GaveUp for {0}".format(prob_proc.name))


# Start running the prover over the cores with the different configurations given as heuristics.
# A problem driver script should interact with this function as it controls the overall processes.
def run_prover(prob_proc, heuristics_spec, time_limit, no_cores, ltb_mode=False, initial_run=False, rolling_run=False, batch_path=None):

     ## Make tmp dir if not exists
    if not os.path.exists('tmp'):
            os.makedirs('tmp')

    # The current heuristic index
    heuristic_index = 0

    # The number of provers/processes currently running
    provers_running = 0

    # The start time for local wall clock
    start_time = time_now()

    # Dictionary for storing process information
    proc_dict = {}

    # Represents a successfull prover run
    prover_success = False

    # Start running prover processes for each core available.
    # We continue to start new prover processes while:
    #   - There are more heuristics left
    #   - The timeout for the problem is not met
    #   - A process has not terminated with success
    while heuristic_index < len(heuristics_spec) and time_now() - start_time < time_limit and not prover_success:

        # Compute the current time available (problem timeout)
        time_avail = (start_time + time_limit) - time_now()

        # Check if this is the first/initial run
        # In the ltb division the initial run is a low-timelimit run
        if initial_run:
            prob_time_avail = INITIAL_RUNNING_TIME
        elif rolling_run and heuristic_index >= ROLLING_HEURISTIC_START:
            prob_time_avail = ROLLING_HEURISTIC_TIMELIMIT
        else:
            # Compute the problem running time according to the upper and lower bound of our times
            #prob_time_avail = max(min(time_avail, MAX_PROBLEM_TIME), MIN_PROBLEM_TIME)

            # We do not trunacate the times in this run
            prob_time_avail = max(time_avail, MIN_PROBLEM_TIME)


        if not SUPPRESS_OUTPUT and PROCESS_OUTPUT:
            print("{0:.1f}  :  {1}  *  {2:.0f}".format((time_now() - start_time), heuristics_spec[heuristic_index][0], prob_time_avail))

        # Start running heuritic problem pair with the time available
        #proc, proc_time, problem_version_name = start_prover_process(prob_proc, heuristics_spec[heuristic_index][1], prob_time_avail)
        proc, proc_time, problem_version_name, tmp_file = start_prover_process(prob_proc, heuristics_spec[heuristic_index][1], prob_time_avail, batch_path)

        # Store the process heuristic pair
        # Key: heur_index || process, proc_time, heur_name
        #proc_dict[str(heuristic_index)] = (proc, proc_time, heuristics_spec[heuristic_index][0], problem_version_name)
        proc_dict[str(heuristic_index)] = (proc, proc_time, heuristics_spec[heuristic_index][0], problem_version_name, tmp_file)


        # Increment to the next heuristic
        heuristic_index += 1

        # Increment the number of provers running
        provers_running += 1

        # If we are utilising all the cores, have no successfull proofs and have not met the timelimit
        # we sleep until the above is not satisfied (most likely a prover terminates)
        while (provers_running >= no_cores or heuristic_index == len(heuristics_spec)) and not prover_success and time_now() - start_time < time_limit:

            # Set program to sleep
            sleep(0.3)

            # Check if some processes have terminated
            provers_running, prover_success, proc_dict, problem_version_name = handleTerminatedProcesses(prob_proc, proc_dict, provers_running, ltb_mode)


    # For some reason we have stopped running processes, and need to clean up
    prob_proc, proc_dict = handle_and_kill_processes(prob_proc, proc_dict)

    # Update local WC
    prob_proc.tot_wc = time_now() - start_time

    if not SUPPRESS_OUTPUT:
        # Report the end result
        report_result(prob_proc, ltb_mode)
    else:
        if prob_proc.solved and ltb_mode:
            #print("% SZS status {0} for {1}".format(prob_proc.szs, prob_proc.name))
            print("% SZS status {0} for {1}".format(prob_proc.szs, problem_version_name))
        elif not prob_proc.solved and ltb_mode and not initial_run:
            print("% SZS status GaveUp for {0}+1.p".format(prob_proc.name))
            print("% SZS status GaveUp for {0}+2.p".format(prob_proc.name))
        elif not prob_proc.solved and not ltb_mode:
            print("% SZS status GaveUp")


    return prob_proc

