#!/bin/bash

#Part of iProver 

echo ""
echo "%---------------- iProver v2.0 (CASC-25 2016) ----------------%"
echo ""
echo "%----------------          SAT mode           ----------------%"
echo ""



#debug
#set -x

args=("$@")
NARGS=$#

#time limit
TLIMIT=$1

#number of cpu cores
#CPU_CORES=$3

PROVER=$2
LAST_ARG="${args[$(($NARGS -1))]}"

INP=$LAST_ARG

echo "Input problem: $LAST_ARG"
echo ""
echo "Solving is in progress......."

# next arguments are interleaving PROVERPARAM/FRAC of time for this param
I=2
LAST_PROVER_ARG=$(($NARGS - 2))



J=0
while [ "$I" -le "$LAST_PROVER_ARG" ]; 
do
    SLICE[$J]="${args[$I]}"
    let I=I+1
    FRAC[$J]="${args[$I]}"
    let I=I+1 
    let J=J+1
done



SYS=$(uname -s)

if [ "$SYS" = "Darwin" ]; 
then     
    CPU_CORES=`sysctl -n hw.physicalcpu`
else
    CPU_CORES=`grep processor /proc/cpuinfo | wc -l` 
fi


#!!!!
#the following two functions need to be adapted for a prover, the rest should be prover-independent
#(as long as the prover passes the slice strings as options to the Vampire clausifier:)
#!!!!

#MEMLIM=$((($(free|awk '/^Mem:/{print $2}')*2)/$CPU_CORES))

MEMLIM=""

#echo "Memory Limit $MEMLIM"

function runProver {
# <sine strategy> <time> <out file> <background run (1 means true, other false)>
# if the process is run on background, its PID must be assigned to the global 
# variable PROVER_PID

#        echo ""
#	echo "Slice \"$1\" for $2 seconds"
#	echo " "> $3
#	echo "Slice \"$1\" for $2 seconds" >> $3
	if [ "$4" == "1" ]; then

	    (ulimit -s 200000; "$PROVER" $1 --time_out_real $2 $INP >> $3 2>/dev/null)&  
      	    PROVER_PID=$!
	else	 
	    (ulimit -s 200000; "$PROVER" $1 --time_out_real $2 $INP >> $3 2>/dev/null)
	fi
}
 
function wasSuccess {
# <out file>
#return 0 status if proof was found

	grep -q '% SZS status \(Theorem\|Unsatisfiable\|Satisfiable\|CounterSatisfiable\)' $1
	return
}

#slices to be attempted
#Slice string must not be equal to "" -- slice with no arguments 
#should be denoted as " ".


function killtree {
    local ppid=$1
    if [ "$SYS" = "Darwin" ]; 
    then
	local CHILDREN=`ps -o pid,ppid | awk -v ppid=$ppid '$2 ~ ppid' | awk '{print $1;}'`
#	local CHILDREN=`pstree -p $ppid | tr "\n" " " |sed "s/[^0-9]/ /g" |sed "s/\s\s*/ /g"`
	#local CHILDREN=`ps -o pid,ppid | grep '^[0-9]' | grep ' '$ppid | cut -f 1 -d ' '`
    else
	local CHILDREN=`ps -o pid --no-heading --ppid $ppid`
    fi
     

    if [ ! -z "$CHILDREN" ];
    then
        for child_pid in ${CHILDREN}; 
        do
            killtree ${child_pid}
        done
    fi
   # kill -TERM ${ppid}
    kill -9 ${child_pid} 2>/dev/null
}

function killChildProcesses {

    killtree "$$"
}

function terminate {
# <SZS status> <process result>
#        echo ""
#	echo "% SZS status $1 for $INP" >> $OUTP
#	echo "% SZS status $1 for $INP"
#	if [ $2 -eq 1 ]; then
#	    echo "% SZS status $1 for $INP" >> $OUTP
#	fi
 #       echo ""
 #       grep "% SZS answers Tuple" $OUTP
 #       echo ""
	killChildProcesses


	rm -rf $TMP
	wait
	exit $1
}

function timeOut {
    echo "% Timout"
    echo "% SZS status GaveUp"
    terminate 1
}

function giveup {
   echo "% SZS status GaveUp"
   terminate 1
}

function interrupted {
        echo "% User interrupted"
	echo "% SZS status User"
	terminate 1
}

function success {
#    echo "success"
    terminate 0
}

function postprocessRun {
# <prover output>
# should be called on output of each run of a prover

	#we want to keep the record of the run of the prover, but we 
	#don't want to report failure while we're still trying
#	grep -v "% SZS status" $1 >> $OUTP
    if wasSuccess $1; then	    
	##KK
#	echo ""
#	echo "success: $1"
#	echo ""
	cat "$1" 
	#	    cat $1 >> $OUTP
	success
	
	#	else
	#	    grep -v "% SZS status" $1 >> $OUTP 
	#	    grep -v "% SZS status" $1 
    fi
    echo "terminated: $1"
}

function addChild {
# <pid> <output file> <slice time>
#    echo "addChild pid: $1 out_file: $2 time: $3 "

	CHILDREN[$CHILD_CNT]=$1
	PROCESS_OUTPUTS[$1]=$2
	PROCESS_STARTS[$1]=$SECONDS
	PROCESS_TIMES[$1]=$3
	CHILD_CNT=$(($CHILD_CNT+1))
#	TAVAIL=$(($TAVAIL-$3))
}

function handleDeadChildProcess {
#if there is a newly terminated child process, run the postprocessRun function for it
#and return 0; otherwise return 1. (we return 0 only if the run was unsuccessful;
#if proof was found, we terminate)
        local I=0
        while [ "$I" -lt "$CHILD_CNT" ]; do
        	local CHILD_PID=${CHILDREN[$I]}
#		if [ '!' -e /proc/$CHILD_PID ]; then
		if ! ps -p $CHILD_PID > /dev/null 
		then
		    local CHILD_OUT=${PROCESS_OUTPUTS[$CHILD_PID]}
		    postprocessRun $CHILD_OUT
		    #we get here only if the prover run was unsuccessful
			
			#local SAVED_TIME=$((${PROCESS_STARTS[$CHILD_PID]}+${PROCESS_TIMES[$CHILD_PID]}-$SECONDS))
		    #TAVAIL=$(($TAVAIL+$SAVED_TIME))
		    
		    CHILD_CNT=$(($CHILD_CNT-1))
		    CHILDREN[$I]=${CHILDREN[$CHILD_CNT]}
		    return 0
		fi
		I=$(($I+1))
        done
        return 1
}


#trap timeOut SIGALRM
#trap timeOut SIGXCPU
#trap interrupted SIGINT

#this variable is every second increased by shell
SECONDS=0

TMP=`mktemp -d /tmp/iprover.XXXXXX`
#rm -f $OUTP

SLICE_CNT=0
while [ "${SLICE[$SLICE_CNT]}" != "" ]; do
	SLICE_CNT=$(($SLICE_CNT+1))
done

# if [ "$CPU_CORES" == 1 ]; then

# #single core implementation
# SLICE_INDEX=0
# while [ "${SLICE[$SLICE_INDEX]}" != "" ]; do
# 	SLC="${SLICE[$SLICE_INDEX]}"
	
# 	TIME_REMAINS=$(( $TLIMIT-$SECONDS ))

# 	if [ $TIME_REMAINS -le 0 ]; then
# 		timeOut
# 	fi
	
# 	SLICES_REMAIN=$(($SLICE_CNT-$SLICE_INDEX))
# 	SLICE_TIME=$(( ($TIME_REMAINS+$SLICES_REMAIN-1)/$SLICES_REMAIN ))
	
	
# 	SLICE_OUT=$TMP"/1.tmp"
# 	runProver "$SLC" $SLICE_TIME $SLICE_OUT
# 	postprocessRun $SLICE_OUT
	
# 	SLICE_INDEX=$(($SLICE_INDEX+1))
# done

# else

#parallel implementation

#CORES_LEFT=$CPU_CORES

#number of child processes
CHILD_CNT=0

#array of child processes
#on indexes from 0 to $CHILD_CNT-1 it contains children's PIDs
CHILDREN[0]=0

#array of children's output files
#indexed by children's PIDs, it contains their output filenames
PROCESS_OUTPUTS[0]=0

#array of children's start times
#indexed by children's PIDs, it contains content of the $SECONDS 
#variable at the time of their start
PROCESS_STARTS[0]=0

#array of children's time limits
#indexed by children's PIDs
PROCESS_TIMES[0]=0


#amount of available CPU time
#TAVAIL=$(($TLIMIT*$CPU_CORES))

SLICE_INDEX=0
while [ $SLICE_INDEX -lt $SLICE_CNT ]; do
	SLC="${SLICE[$SLICE_INDEX]}"
	
	SLICES_REMAIN=$(($SLICE_CNT-$SLICE_INDEX))

#	SLICE_TIME=$(($TAVAIL/$SLICES_REMAIN))

#KK hack we run 0 slice until for the whole time	
	
#	if [ "$SLICE_INDEX" == 0 ]; then
#	    SLICE_TIME=$(($TLIMIT))
#	fi

#KK Slice time is always TILIMIT but if a core is freed then it is occupied by the next slice
#	SLICE_TIME=$(($TLIMIT))

	FRAC_SLICE=${FRAC[$SLICE_INDEX]}
#	echo "FRAC_SLICE $FRAC_SLICE"
	SLICE_TIME=$(($TLIMIT*$FRAC_SLICE))

#	echo "SLICE_TIME: $SLICE_TIME"
	SLICE_OUT=$TMP"/"$SLICE_INDEX".tmp"
	runProver "$SLC" $SLICE_TIME $SLICE_OUT 1
	
	addChild $PROVER_PID $SLICE_OUT $SLICE_TIME

#	CORES_LEFT=$(($CORES_LEFT-1))
	
#	while [ $CORES_LEFT -le 0 ]; do
#	while ! handleDeadChildProcess; do
#	    sleep 0.2
#	done
#		CORES_LEFT=$(($CORES_LEFT+1))
#	done
	
	SLICE_INDEX=$(($SLICE_INDEX+1))
done

while [ $CHILD_CNT -gt 0 ]; do
	while ! handleDeadChildProcess; do
		sleep 0.2
	done
done


#fi

#timeOut

giveup
