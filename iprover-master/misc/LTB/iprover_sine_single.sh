#!/bin/bash
# part of iProver v2.6

#debug
#set -x

#category: LTB.SMO, LTB.MZR, MZR.MZR, LTB.CYC, LTB.HOL 
CAT=$1
#time limit
TLIMIT=$2
#number of cpu cores
CPU_CORES=$3

PROVER=$4
PROVERPARAM=$5
CLAUSIFIER=$6
#problem file
INP=$7
#output file
OUTP=$8

#echo "iprover_single.sh"

#!!!!
#the following two functions need to be adapted for a prover, the rest should be prover-independent
#(as long as the prover passes the slice strings as options to the Vampire clausifier:)
#!!!!

SYS=$(uname -s)

if [ "$SYS" = "Darwin" ]; 
then 
    MEMLIM=$((16777216/$CPU_CORES))
else 
    MEMLIM=$((($(free|awk '/^Mem:/{print $2}')*2)/$CPU_CORES))
fi

#echo "Memory Limit $MEMLIM"

function runProver {
# <sine strategy> <time> <out file> <background run (1 means true, other false)> <extra iProver paramters>
# if the process is run on background, its PID must be assigned to the global 
# variable PROVER_PID
#        echo ""
#	echo "Slice \"$1\" for $2 seconds"
	echo " "> $3
	echo "Slice \"$1\" for $2 seconds" >> $3        
 
# TFF categories need to use "--mode tclausify" and others "--mode clausify
        cat $INP | grep "tff" |  grep -q -v "%"
        if [ $? -eq 0 ]; 
        then
            clausify_mode="tclausify"
        else
            clausify_mode="clausify" 
        fi
	if [ "$4" == "1" ]; then
	#    echo  \(ulimit -v $MEMLIM;"$PROVER" --clausifier "$CLAUSIFIER" --clausifier_options "--mode clausify -t $2 $1" --time_out_real $2 $INP\)
 #       	 iProver_sine/TreeLimitedRun 10000 10000 $MEMLIM "$PROVER" --clausifier "$CLAUSIFIER" --clausifier_options "--mode clausify -t $2 $1" --time_out_real $2 $INP > $3 &
	   # (ulimit -v $MEMLIM -s 300000; "$PROVER" $PROVERPARAM --clausifier "$CLAUSIFIER" --clausifier_options "--mode clausify -t $2 $1" --sat_out_model none  --time_out_real $2 $INP >> $3 2>/dev/null)&  

# limit on stack is set in ltb scripts
            (ulimit -v $MEMLIM; "$PROVER" $PROVERPARAM $5 --clausifier "$CLAUSIFIER" --clausifier_options "--mode $clausify_mode -t $2 $1" --sat_out_model none  --time_out_real $2 $INP >> $3 2>/dev/null)&  
      		PROVER_PID=$!
	else	 
	    (ulimit -v $MEMLIM;"$PROVER" $PROVERPARAM $5 --clausifier "$CLAUSIFIER" --clausifier_options "--mode $clausify_mode -t $2 $1" --sat_out_model none --time_out_real $2 $INP >> $3 2>/dev/null)
	fi
}

function wasSuccess {
# <out file>
#return 0 status if proof was found

	grep -q '% SZS status \(Theorem\|Unsatisfiable\)' $1
	return
}

#slices to be attempted
#Slice string must not be equal to "" -- slice with no arguments 
#should be denoted as " ".

#----------- OLD 

# if [ "$CAT" == "LTB.SMO" ]; then
#     echo "$CAT Slices"
#     SLICE[0]="-ss included -sd 3 -st 3"
#     SLICE[1]="-ss axioms -sd 1 -st 1"
#     SLICE[2]="-ss axioms -sd 0 -st 1.5"
#     SLICE[3]="-ss axioms -sd 0 -st 2"
#     SLICE[4]="-ss axioms -sd 5 -st 1.7"
#     SLICE[5]="-ss axioms -sd 10 -st 1"
#     SLICE[6]="-ss included -sd 10 -st 1.5"
#     SLICE[7]="-ss included -sd 10 -st 2"
#     SLICE[8]=" "
#     SLICE[9]="-ss axioms -sd 20 -st 3"
#     SLICE[10]="-ss axioms -sd 4 -st 4"
#     SLICE[11]="-ss axioms -sd 5 -st 5"
#     SLICE[12]="-ss axioms -sd 5 -st 2 -sgt 10"
#     SLICE[13]="-ss included -sd 6 -st 6"
#     SLICE[14]="-ss included -sd 40  -st 3"
#     SLICE[12]=" "
# else
#     if [ "$CAT" == "LTB.MZR" -o "$CAT" == "MZR.MZR" ]; then
# 	echo "$CAT Slices"

#   SLICE[0]="-ss axioms -sd 2  -st 1";    IPR[0]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false "
#   SLICE[1]="-ss axioms -sd 1  -st 2.5";  IPR[1]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[2]="-ss axioms -sd 2  -st 2.5";  IPR[2]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[3]="-ss axioms -sd 1  -st 1.5";  IPR[3]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[4]="-ss axioms -sd 1  -st 5";    IPR[4]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false "
#   SLICE[5]="-ss axioms -sd 2  -st 5";    IPR[5]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[6]="-ss axioms -sd 1  -st 1.5";  IPR[6]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false "
#   SLICE[7]="-ss axioms -sd 2  -st 2";    IPR[7]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false  "
#   SLICE[8]="-ss axioms -sd 1 -st 1";     IPR[8]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[9]="-ss axioms -sd 1 -st 3";     IPR[9]=""
#   SLICE[10]="-ss axioms -sd 8  -st 1";   IPR[10]=""
#   SLICE[11]="-ss axioms -sd 1  -st 2";   IPR[11]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[12]=" "

# else
#     if [ "$CAT" == "LTB.HLL" -o "$CAT" == "HLL.HLL" ]; then
# 	echo "$CAT Slices"

#   SLICE[0]="-ss axioms -sd 1 -st 3";   IPR[0]=" --abstr_ref_sig true --abstr_ref_until_sat true "
#   SLICE[1]="-ss axioms -sd 1 -st 1";   IPR[1]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false "
#   SLICE[2]="-ss axioms -sd 2 -st 1";   IPR[2]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true " 
#   SLICE[3]="-ss axioms -sd 1 -st 1.5"; IPR[3]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[4]="-ss axioms -sd 1 -st 5";   IPR[4]=" --abstr_ref_sig true --abstr_ref_until_sat true "
#   SLICE[5]="-ss axioms -sd 1 -st 3.5"; IPR[5]=" --abstr_ref_sig true --abstr_ref_until_sat true "
#   SLICE[6]="-ss axioms -sd 2 -st 1";   IPR[6]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[7]="-ss axioms -sd 8 -st 9";   IPR[7]=" --abstr_ref_sig true --abstr_ref_until_sat false  "
#   SLICE[8]="-ss axioms -sd 1 -st 1";   IPR[8]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[9]="-ss axioms -sd 2 -st 4.5"; IPR[9]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[10]=" "


# else
#     if [ "$CAT" == "LTB.HL4" -o "$CAT" == "HL4.HL4" ]; then
# 	echo "$CAT Slices"

#   SLICE[0]="-ss axioms -sd 1  -st 1";   IPR[0]=" --abstr_ref_sig true --abstr_ref_until_sat true "
#   SLICE[1]="-ss axioms -sd 1  -st 5";   IPR[1]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[2]="-ss axioms -sd 2  -st 1";   IPR[2]=" --abstr_ref_sig true --abstr_ref_until_sat true  "
#   SLICE[3]="-ss axioms -sd 1  -st 2";   IPR[3]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[4]="-ss axioms -sd 1  -st 4";   IPR[4]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false "
#   SLICE[5]="-ss axioms -sd 1  -st 1.5"; IPR[5]=" --abstr_ref_sig true --abstr_ref_until_sat false  "
#   SLICE[6]="-ss axioms -sd 1  -st 3";   IPR[6]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[7]="-ss axioms -sd 1  -st 1";   IPR[7]=" --abstr_ref_arg_filter true --abstr_ref_until_sat false "
#   SLICE[8]="-ss axioms -sd 16  -st 1";  IPR[8]=" --abstr_ref_sig true --abstr_ref_until_sat false "
#   SLICE[9]="-ss axioms -sd 2  -st 2";   IPR[9]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true  "
#   SLICE[10]="-ss axioms -sd 2 -st 1";   IPR[10]=" --abstr_ref_arg_filter true --abstr_ref_until_sat true "
#   SLICE[11]=" "

#else
#----------New

 #    if [ "$CAT" == "LTB.ISA" -o  "$CAT" == "LTB.MZR" -o "$CAT" == "ISA.ISA" -o "$CAT" == "LTB.SLH" -o "$CAT" == "LTB.SLD" ]; then
# 	echo "$CAT Slices"

#   SLICE[0]="-ss axioms -sd 1 -st 1";     IPR[0]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_until_sat false "
#   SLICE[1]="-ss axioms -sd 2 -st 1";     IPR[1]=" --abstr_ref [subs;arg_filter]  --abstr_ref_until_sat true "
#   SLICE[2]="-ss axioms -sd 2 -st 2.5";   IPR[2]=" --abstr_ref [] "
#   SLICE[3]="-ss axioms -sd 2 -st 3";     IPR[3]=" --abstr_ref [subs;arg_filter]  "
#   SLICE[4]="-ss axioms -sd 3 -st 1";     IPR[4]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_until_sat true "
#   SLICE[5]="-ss axioms -sd 1 -st 1";     IPR[5]=" --abstr_ref [arg_filter] --abstr_ref_until_sat false "
#   SLICE[6]="-ss axioms -sd 3 -st 1.5";   IPR[6]=" --abstr_ref [sig] --abstr_ref_until_sat true --abstr_ref_prep true "
#   SLICE[7]="-ss axioms -sd 2 -st 2.5";   IPR[7]=" --abstr_ref [subs;sig]  --abstr_ref_until_sat false "
#   SLICE[8]="-ss axioms -sd 6 -st 1.5";   IPR[8]="  --abstr_ref [subs;sig;arg_filter] --abstr_ref_until_sat true  --abstr_ref_prep true "
#   SLICE[9]="-ss axioms -sd 1 -st 6";     IPR[9]=" "
#   SLICE[10]="-ss axioms -sd 2 -st 3";    IPR[10]=" "
#   SLICE[11]="-ss axioms -sd 2 -st 1";    IPR[11]=" "
#   SLICE[12]=" "

# else
# 	if [ "$CAT" == "LTB.CYC" ]; then
# 	    echo "$CAT Slices"
# 	    SLICE[0]="-ss included"
#             SLICE[1]="-ss included -st 5 -sd 2"
# 	    SLICE[2]=" "
# 	    SLICE[3]="-ss included -st 1.2"
# 	    SLICE[4]="-ss included -st 2 -sd 1"
# 	    SLICE[5]="-ss included -st 1.5"
# 	    SLICE[6]="-ss included -st 2"
# 	    SLICE[7]="-ss included -st 5"
# 	    SLICE[8]="-ss included -st 5 -sd 1" 
# 	    SLICE[9]="-ss axioms"
# 	    SLICE[10]="-ss included -sd 1"
# 	    SLICE[11]="-ss included -sd 2"
# 	else
# 	    echo "Category $CAT"
#             echo "Default: All Slices"

SLICE[0]="-ss axioms -sd 1 -st 1.0"; IPR[0]=" --abstr_ref [sig;subs;arg_filter] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk true --abstr_ref_until_sat true "
SLICE[1]="-ss axioms -sd 1 -st 2.0"; IPR[1]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_sig_restrict skc --abstr_ref_af_restrict_to_split_sk false --abstr_ref_until_sat false "
SLICE[2]="-ss axioms -sd 2 -st 1.0"; IPR[2]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_sig_restrict skc --abstr_ref_af_restrict_to_split_sk false --abstr_ref_until_sat false "
SLICE[3]="-ss axioms -sd 1 -st 4.0"; IPR[3]=" --abstr_ref [arg_filter;sig;subs] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk true --abstr_ref_until_sat true "
SLICE[4]="-ss axioms -sd 1 -st 1.0"; IPR[4]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_sig_restrict skc --abstr_ref_af_restrict_to_split_sk true --abstr_ref_until_sat false "
SLICE[5]="-ss axioms -sd 1 -st 1.0"; IPR[5]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk false --abstr_ref_until_sat false "
SLICE[6]="-ss axioms -sd 2 -st 1.0"; IPR[6]=" --abstr_ref [sig] --abstr_ref_sig_restrict skc --abstr_ref_af_restrict_to_split_sk false --abstr_ref_until_sat false "
SLICE[7]="-ss axioms -sd 1 -st 8.0"; IPR[7]=" --abstr_ref [subs;sig;arg_filter] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk false --abstr_ref_until_sat false "
SLICE[8]="-ss axioms -sd 1 -st 1.0"; IPR[8]=" --abstr_ref [arg_filter;subs;sig] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk true --abstr_ref_until_sat false "
SLICE[9]="-ss axioms -sd 2 -st 1.0"; IPR[9]=" --abstr_ref [arg_filter;sig;subs] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk true --abstr_ref_until_sat true "
SLICE[10]="-ss axioms -sd 2 -st 1.0"; IPR[10]=" --abstr_ref [arg_filter] --abstr_ref_sig_restrict funpre --abstr_ref_af_restrict_to_split_sk false --abstr_ref_until_sat false "
SLICE[11]=" "



#	    if [ "$CAT" == "LTB.ISA" -o "$CAT" == "LTB.HOL" ]; then
#		echo "$CAT Slices"
		# SLICE[0]="-ss axioms -sd 1 -st 1"
		# SLICE[1]="-ss axioms -sd 1 -st 2"
		# SLICE[2]="-ss axioms -sd 1 -st 3"
		# SLICE[3]="-ss axioms -sd 1 -st 4"
		# SLICE[4]="-ss axioms -sd 1 -st 5"
		# SLICE[5]="-ss axioms -sd 1 -st 6"
		# SLICE[6]="-ss axioms -sd 2 -st 1"
		# SLICE[7]="-ss axioms -sd 2 -st 2"
		# SLICE[8]="-ss axioms -sd 2 -st 3"
		# SLICE[9]="-ss axioms -sd 2 -st 4"
		# SLICE[10]="-ss axioms -sd 2 -st 5"
		# SLICE[11]="-ss axioms -sd 2 -st 6"
                # SLICE[12]=" "

#		SLICE[12]="-ss axioms -sd 0 -st 4"
#		SLICE[14]="-ss axioms -sd 0 -st 5"
#		SLICE[15]="-ss axioms -sd 0 -st 7"
#            else	
#		echo "Category $CAT"
#		echo "Default: All Slices"
#		SLICE[0]="-ss axioms"
#		SLICE[1]="-ss included -sd 2"
#		SLICE[2]=" "
#		SLICE[3]="-ss included -sd 1 -st 1"
#		SLICE[4]="-ss included -sd 0 -st 1.2"
#		SLICE[5]="-ss axioms -sd 2 -st 1.5"
#		SLICE[6]="-ss axioms -sd 3 -st 2"
#		SLICE[7]="-ss included -sd 4 -st 2.5"
#		SLICE[8]="-ss axioms -sd 5 -st 3"
#		SLICE[9]="-ss included -sd 6 -st 4"
#		SLICE[10]="-ss included -sd 7 -st 5"
#		SLICE[11]="-ss included -sd 0 -st 6" 
 	   
#            fi

#    fi
#    fi
#    fi
#	  fi
#    fi
#fi

# function killChildProcesses {
# 	#first store child list and only then start killing them (as 'ps' is a child as well)
# 	local CHILDREN=`ps -o pid --no-heading --ppid $$`
# 	local CHILD
	
# 	for CHILD in $CHILDREN; do 
# 		kill -9 $CHILD 2>/dev/null
# 	done
# }


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
        echo ""
#	echo "% SZS status $1 for $INP" >> $OUTP
	echo "% SZS status $1 for $INP"
	if [ $2 -eq 1 ]; then
	    echo "% SZS status $1 for $INP" >> $OUTP
	fi
        echo ""
        grep "% SZS answers Tuple" $OUTP
        echo ""
	killChildProcesses

	rm -rf $TMP
	wait
	exit $2
}

function timeOut {
	terminate GaveUp 1
}

function interrupted {
	terminate User 1
}

function success {
	terminate Theorem 0
}

function postprocessRun {
# <prover output>
# should be called on output of each run of a prover

	#we want to keep the record of the run of the prover, but we 
	#don't want to report failure while we're still trying
#	grep -v "% SZS status" $1 >> $OUTP
	if wasSuccess $1; then	    
##KK 
	    cat $1 >> $OUTP
	    success
	else
	    grep -v "% SZS status" $1 >> $OUTP 
	fi
}

function addChild {
# <pid> <output file> <slice time>

	CHILDREN[$CHILD_CNT]=$1
	PROCESS_OUTPUTS[$1]=$2
	PROCESS_STARTS[$1]=$SECONDS
	PROCESS_TIMES[$1]=$3
	CHILD_CNT=$(($CHILD_CNT+1))
	TAVAIL=$(($TAVAIL-$3))
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
			
			local SAVED_TIME=$((${PROCESS_STARTS[$CHILD_PID]}+${PROCESS_TIMES[$CHILD_PID]}-$SECONDS))
			TAVAIL=$(($TAVAIL+$SAVED_TIME))
			
			CHILD_CNT=$(($CHILD_CNT-1))
			CHILDREN[$I]=${CHILDREN[$CHILD_CNT]}
			return 0
		fi
		I=$(($I+1))
        done
        return 1
}


trap timeOut SIGALRM
trap timeOut SIGXCPU
trap interrupted SIGINT

#this variable is every second increased by shell
SECONDS=0

TMP=`mktemp -d /tmp/iprover.XXXX`
rm -f $OUTP

SLICE_CNT=0
while [ "${SLICE[$SLICE_CNT]}" != "" ]; do
	SLICE_CNT=$(($SLICE_CNT+1))
done

if [ "$CPU_CORES" == 1 ]; then

#single core implementation
    SLICE_INDEX=0
    while [ "${SLICE[$SLICE_INDEX]}" != "" ]; do
	SLC="${SLICE[$SLICE_INDEX]}"
	
	TIME_REMAINS=$(( $TLIMIT-$SECONDS ))
        
	if [ $TIME_REMAINS -le 0 ]; then
		timeOut
	fi
	
	SLICES_REMAIN=$(($SLICE_CNT-$SLICE_INDEX))
	SLICE_TIME=$(( ($TIME_REMAINS+$SLICES_REMAIN-1)/$SLICES_REMAIN ))
	
	
	SLICE_OUT=$TMP"/1.tmp"	
	runProver "$SLC" $SLICE_TIME $SLICE_OUT "${IPR[$SLICE_INDEX]}"
	postprocessRun $SLICE_OUT
	
	SLICE_INDEX=$(($SLICE_INDEX+1))
    done
    
else

#parallel implementation

CORES_LEFT=$CPU_CORES

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
TAVAIL=$(($TLIMIT*$CPU_CORES))

SLICE_INDEX=0
while [ $SLICE_INDEX -lt $SLICE_CNT ]; do

	TIME_REMAINS=$(( $TLIMIT-$SECONDS ))        
	if [ $TIME_REMAINS -le 0 ]; then
		timeOut
	fi

	SLC="${SLICE[$SLICE_INDEX]}"
	
	SLICES_REMAIN=$(($SLICE_CNT-$SLICE_INDEX))

#	SLICE_TIME=$(($TAVAIL/$SLICES_REMAIN))

#KK hack we run 0 slice until for the whole time	
	
#	if [ "$SLICE_INDEX" == 0 ]; then
#	    SLICE_TIME=$(($TLIMIT))
#	fi

#KK Slice time is always TILIMIT but if a core is freed then it is occupied by the next slice
	SLICE_TIME=$(($TLIMIT))
	SLICE_OUT=$TMP"/"$SLICE_INDEX".tmp"
	runProver "$SLC" $SLICE_TIME $SLICE_OUT 1 "${IPR[$SLICE_INDEX]}"
	
	addChild $PROVER_PID $SLICE_OUT $SLICE_TIME

	CORES_LEFT=$(($CORES_LEFT-1))
	
	while [ $CORES_LEFT -le 0 ]; do
		while ! handleDeadChildProcess; do
			sleep 0.2
		done
		CORES_LEFT=$(($CORES_LEFT+1))
	done
	
	SLICE_INDEX=$(($SLICE_INDEX+1))
done

while [ $CHILD_CNT -gt 0 ]; do
	while ! handleDeadChildProcess; do
		sleep 0.2
	done
done


fi

timeOut
