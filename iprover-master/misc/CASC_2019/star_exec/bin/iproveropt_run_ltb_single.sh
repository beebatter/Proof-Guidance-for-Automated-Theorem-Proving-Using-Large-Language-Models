#!/bin/bash
# time ./iproveropt_run_ltb_single.sh LTB.SMO 5 300 ~/TPTP-v6.1.0/Problems/CSR/CSR086+3.p 
# 5 is the number of core; 300 is timelimit



CAT=$1
CORES=$2
TIMELIM=$3
Problem=$4

#set -x
#OUTDIR=$2

SCRIPTPATH=$0

ULIMIT=200000

if [[ $? -ne 0 || $# -ne 4 || $1 = "-h" ]]
then
    echo ""
    echo "-------- iProver --------"
    echo ""
    echo "Usage: iproveropt_run_ltb_single.sh ltb_cat num_processes time_limit problem.p"
    echo ""
    echo "Example: ./iproveropt_run_ltb_single.sh LTB.SMO 5 300 ~/TPTP-v6.1.0/Problems/CSR/CSR086+3.p"
    echo ""
    echo "ltb_cat -- is one of LTB categories: LTB.SMO; LTB.MZR; LTB.HOL; LTB.ISA"
    echo ""
    echo "num_processes -- is the number of simultenuous processes; for a large number of processes memory can be a limiting factor" 
    echo ""
    echo "timelimit -- is wallclock limit in seconds"
    echo ""
    exit 1
fi

#export TPTP=`dirname "$Problem"`

Dir=`dirname "$0"`

LTBDIR=${Dir}/LTB

SYS=$(uname -s)

if [ "$SYS" = "Darwin" ]; 
then     
    ULIMIT=60000
    TreeLimitedRun=""
#TreeLimitedRun does not work under Mac
else
    ULIMIT=200000
    TreeLimitedRun="$Dir/TreeLimitedRun -q1 0 $TIMELIM"
fi

PROVER="$Dir/iproveropt"

TMPOUT=`mktemp /tmp/iprover_ltb.XXXX`

# some prover parameters can be overriden by iprover_sine_single.sh

PROVERPARAM="--bc_imp_inh [conj_cone] --conj_cone_tolerance 10 --inst_passive_queues [[-conj_dist;+conj_symb;-num_var];[+age;-num_symb];[+bc_imp_inh;-num_var]] --inst_passive_queues_freq [20;25;20] --res_passive_queues [[-conj_dist;+conj_symb;-num_symb];[+age;-num_symb];[+bc_imp_inh;-num_var]] --res_passive_queues_freq [20;25;20]"

CLAUSIFIER="$Dir/VClausifier/vclausify_rel"


$TreeLimitedRun $LTBDIR/iprover_sine_single.sh $CAT $TIMELIM $CORES "$PROVER" "$PROVERPARAM" "$CLAUSIFIER" $Problem $TMPOUT

cat "$TMPOUT"
rm -f  "$TMPOUT"

