#!/bin/bash

# iprover_run_sat.sh 300 problem_name

# set -x 

TIMELIMIT=$1
Problem=$2

SCRIPTPATH=$0

grace=10
 
TIMELIMIT=$(($TIMELIMIT+$grace))

Dir=`dirname "$0"`

SYS=$(uname -s)

if [ "$SYS" = "Darwin" ]; 
then     
    ULIMIT=60000
    TreeLimitedRun=""
#TreeLimitedRun does not work under Mac
else
    ULIMIT=200000
    TreeLimitedRun="$Dir/TreeLimitedRun -q1 $TIMELIMIT $TIMELIMIT"
fi


#echo $Dir

VClausifier="$Dir/VClausifier/vclausify_rel"
VOPT=" --mode clausify "

EClausifier="$Dir/E_Prover/eprover"
EOPT=" --tstp-format --free-numbers --free-objects --split-method=1  --silent --proof-object --cnf "

OPTS=" --schedule sat --sat_fm_uc_incr true --bc_imp_inh [] --reset_solvers true --inst_sel_renew solver --inst_prop_sim_given true --inst_prop_sim_new false --inst_lit_activity_flag false "



#(ulimit -s $ULIMIT; $TreeLimitedRun "$Dir/iprover_sat_single.sh" "$TIMELIMIT" "$Dir/iproveropt" "--schedule none --out_options none --sat_mode true --superposition_flag false --sat_finite_models true --sat_fm_uc_incr true --bc_imp_inh "[]" --inst_prop_sim_given false --inst_prop_sim_new false --reset_solvers false --inst_lit_activity_flag false --clausifier $EClausifier " 1 " --out_options none --clausifier $EClausifier " 1/4 "--schedule none --out_options none --sat_mode true --sat_finite_models true --sat_fm_uc_incr true --bc_imp_inh "[]" --inst_prop_sim_given false --inst_prop_sim_new false --reset_solvers false --inst_lit_activity_flag false --clausifier $VClausifier" 1/10 "$Problem")

OTPS_SAT_26357=$(cat $Dir/options_exp/opts_2018_06_26357_sat.txt)

ulimit -s $ULIMIT; $TreeLimitedRun "$Dir/iprover_sat_single.sh" "$TIMELIMIT" "$Dir/iproveropt" "$OTPS_SAT_26357 " 1 " --out_options none --clausifier $EClausifier " 1/4 "--schedule none --out_options none --sat_mode true --superposition_flag false --sat_finite_models true --sat_fm_uc_incr true --bc_imp_inh "[]" --inst_prop_sim_given false --inst_prop_sim_new false --reset_solvers false --inst_lit_activity_flag false --clausifier $VClausifier" 1/10 "--superposition_flag false --schedule none --instantiation_flag false --res_time_limit 400 --res_lit_sel neg_max --res_lit_sel_side num_symb --splitting_mode input --res_passive_queues [[+age;+num_var;-num_symb];[+age;-num_symb;-num_var]] --res_prop_simpl_given false " 1/15 "$Problem"
