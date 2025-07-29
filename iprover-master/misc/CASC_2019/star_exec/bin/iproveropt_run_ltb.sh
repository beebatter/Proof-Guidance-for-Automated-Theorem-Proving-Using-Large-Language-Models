#!/bin/bash

# iproveropt_run_ltb.sh batch_file out_dir
# CASC 2016 change: should be run from the dir contaning Problems/
# TPTP env should contain path to top dir of Axioms

Problem=$1
OUTDIR=$2

SCRIPTPATH=$0

ULIMIT=200000

export TPTP=`dirname "$Problem"`

Dir=`dirname "$0"`

#echo $Dir


VClausifier="$Dir/VClausifier/vclausify_rel"
VOPT=" --mode clausify "

EClausifier="$Dir/E_Prover/eprover"
EOPT=" --tstp-format --free-numbers --free-objects --split-method=1  --silent --cnf "


#(ulimit -s $ULIMIT;  "$Dir/TreeLimitedRun" -q1 "$TIMELIMIT" "$TIMELIMIT"  "$Dir/iproveropt" --out_options none --time_out_real $TIMELIMIT --clausifier "$VClausifier" $Problem)

#(ulimit -s $ULIMIT; "$Dir/LTB/iprover_sine.sh" "$Dir/iproveropt" "--bc_imp_inh [conj_cone] --conj_cone_tolerance 10 --inst_passive_queues "[[-conj_dist;+conj_symb;-num_var];[+age;-num_symb];[+bc_imp_inh;-num_var]]" --inst_passive_queues_freq "[20;25;20]" --res_passive_queues "[[-conj_dist;+conj_symb;-num_symb];[+age;-num_symb];[+bc_imp_inh;-num_var]]" --res_passive_queues_freq "[20;25;20]" " "$Dir/VClausifier/vclausify_rel" 0 "$Problem")

#------- some prover parameters can be overriden by iprover_sine_single.sh

(ulimit -s $ULIMIT; "$Dir/LTB/iprover_sine.sh" "$Dir/iproveropt" "--bc_imp_inh [conj_cone] --conj_cone_tolerance 10 --inst_passive_queues [[-conj_dist;+conj_symb;-num_var];[+age;-num_symb];[+bc_imp_inh;-num_var]] --inst_passive_queues_freq [20;25;20] --res_passive_queues [[-conj_dist;+conj_symb;-num_symb];[+age;-num_symb];[+bc_imp_inh;-num_var]] --res_passive_queues_freq [20;25;20]" "$Dir/VClausifier/vclausify_rel" 0 "$OUTDIR" "$Problem")


