from casc_ltb_iprover import HEUR_SPEC_LTB, HEUR_SPEC_LTB_INITIAL, read_heuristic_string, HEUR_SPEC_FEW, HEUR_SPEC_FEW_INITIAL
from iprover_process_module import start_prover_process, ProbProc, get_output_status, check_prover_success
import os
import glob


########################################################
########### Heuristics exists check


##### Check that the specification files exists
conf_prefix = "heuristics/heur_conf/"

if not os.path.isfile(conf_prefix + HEUR_SPEC_LTB):
    print("ERR heur_conf: {0} does not exists!".format(HEUR_SPEC_LTB))

if not os.path.isfile(conf_prefix + HEUR_SPEC_LTB_INITIAL):
    print("ERR heur_conf: {0} does not exists!".format(HEUR_SPEC_LTB_INITIAL))

if not os.path.isfile(conf_prefix + HEUR_SPEC_FEW):
    print("ERR heur_conf: {0} does not exists!".format(HEUR_SPEC_FEW))

if not os.path.isfile(conf_prefix + HEUR_SPEC_FEW_INITIAL):
    print("ERR heur_conf: {0} does not exists!".format(HEUR_SPEC_FEW_INITIAL))


######################################################
###### Check that every heuristic in the conf files exists
###### and that it does not contain options to suppress proof output!

 # Open specification file for reading the heuristics to run
for postfix in [HEUR_SPEC_LTB, HEUR_SPEC_LTB_INITIAL, HEUR_SPEC_FEW_INITIAL, HEUR_SPEC_FEW]:
    with open("heuristics/heur_conf/"+ str(postfix), "r") as spec_file:
        # For each heuristic in the specification file
        for heur in spec_file:
            heur = str(heur).strip("\n")
            if not os.path.isfile("heuristics/heur_mod/" + heur):
                print("ERR heur_mod {0}: {1}  does not exists!".format(postfix, heur))



######################################################
## Check if heuristic string suppresses proof output
# by containin res_out_proof/inst_out_proof (false)
path = "heuristics/heur_mod/"
files = [f for f in glob.glob(path + "*", recursive=False)]

# For every file
for f in files:

    # Get the heuristic string
    heuristic_string = read_heuristic_string(f)

    if "--res_out_proof" in heuristic_string:
        print("ERR heur_mod {0}:  Suppresses resolution proof!".format(f))
    if "--inst_out_proof" in heuristic_string:
        print("ERR heur_mod {0}:  Suppresses instantiaton proof!".format(f))



# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #



######################################################
###### Check that every heuristic in the path works!

# Find every file
path = "heuristics/heur_mod/"
files = [f for f in glob.glob(path + "*", recursive=False)]


# Create Problems

TIMEOUT = 4

prob = "HL400143+1.p"
prob_procs = ProbProc("", prob)

# For every file
for f in files:

    # Get the heuristic string
    heuristic_string = read_heuristic_string(f)

    # Start problem process
    proc, time_now, problem_version_name, tmp_file = start_prover_process(prob_procs, heuristic_string, TIMEOUT)

    # Wait for problem to finish, get error
    _, err = proc.communicate(timeout=15)

    # Get file output
    with open(tmp_file) as o:
        output = o.read()

    # This could be stupid, but good for manual checking?
    running_time, szs = get_output_status(output, err)
    success = check_prover_success(szs, ltb_mode=True)

    # Check for error, somehow, this seems to work..
    #if err != b'' or not success:
    if err != b'' and not success:
        # An error has occured
        print("")
        print("##" * 15)
        print("##" * 15)
        print("ERR: Heuristic configuration: {0}".format(f))

        print(output)
        print(err)

