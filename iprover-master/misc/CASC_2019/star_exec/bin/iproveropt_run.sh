#!/bin/bash

# iprover_run.sh 300 problem_name



TIMELIMIT=$1
Problem=$2
SCRIPTPATH=$0

ULIMIT=200000

Dir=`dirname "$0"`

#echo $Dir


VClausifier="$Dir/VClausifier/vclausify_rel"
VOPT=" --mode clausify "

EClausifier="$Dir/E_Prover/eprover"
EOPT=" --tstp-format --free-numbers --free-objects --split-method=1  --silent --cnf "


(ulimit -s $ULIMIT;  "$Dir/TreeLimitedRun" -q1 "$TIMELIMIT" "$TIMELIMIT"  "$Dir/iproveropt" --schedule superposition --out_options none --time_out_real $TIMELIMIT --clausifier "$VClausifier" $Problem)

