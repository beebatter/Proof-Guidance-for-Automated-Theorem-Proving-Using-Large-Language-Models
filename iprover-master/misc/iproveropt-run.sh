#!/bin/bash

# iprover_run.sh time_limit problem_name
# iprover_run.sh 300 problem_name


if [[ $? -ne 0 || $# -ne 2 || $1 = "-h" ]]
then
    echo ""
    echo "-------- iProver --------"
    echo ""
    echo "Usage: iproveropt_run.sh time_limit problem.p"
    echo ""
    echo "where timelimit is overall time limit in seconds"
    echo ""
    exit 1
fi

TIMELIMIT=$1
Problem=$2
SCRIPTPATH=$0

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


(ulimit -s $ULIMIT; $TreeLimitedRun "$Dir/iproveropt" --out_options none --time_out_real $TIMELIMIT $Problem)

