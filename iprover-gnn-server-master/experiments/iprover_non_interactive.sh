#!/bin/bash
timeout -k $(($1+10)) $(($1+5)) ../iprover/iproveropt \
	--interactive_mode false \
	--inst_learning_loop_flag false \
	--schedule none \
	--preprocessing_flag false \
	--instantiation_flag true \
	--superposition_flag false \
	--resolution_flag false \
	--time_out_real "$1" \
	--inst_unprocessed_bound "$2" \
	"$3" 2> "$4.err" | xz -0 > "$4.out.xz"
