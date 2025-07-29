#!/bin/bash

OUTDIR=heur_mod
INDIR=heur_mod_orig

for i in $( ls $INDIR); do
    set -o pipefail
    grep -q "abstr_ref " $INDIR/$i 
    RESULT1=$?
    grep -q -v "abstr_ref '\[\]'" $INDIR/$i
    RESULT2=$?
    
#     echo  "$i: RESULT1: $RESULT1 RESULT2:$RESULT2"

    if [[ $RESULT1 -eq 0 && $RESULT2 -eq 0  ]]; then
        echo  $i
        cat $INDIR/$i abstr_no_res_sup.txt | tr -d '\n' > $OUTDIR/$i
    fi
#    if [ $RESULT -eq 0 ]; then
#        echo  $i
#    else
#       echo "non $RESULT: $i"
#    fi
done

