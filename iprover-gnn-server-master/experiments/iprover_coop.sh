#!/bin/bash
timeout -k $(($1+10)) $(($1+5)) ../iprover/iproveropt \
	--interactive_mode true \
	--external_ip_address "127.0.0.1" \
	--external_port "12300" \
	--inst_learning_loop_flag false \
	--schedule none \
	--preprocessing_flag false \
	--instantiation_flag true \
	--superposition_flag false \
	--resolution_flag false \
	--inst_passive_queue_type priority_queues \
	--inst_passive_queues_freq "[27;25;2]" \
	--inst_passive_queues "[[+external_score];[-conj_dist;+conj_symb;-num_var];[+age;-num_symb]]" \
	--time_out_real "$1" \
	--inst_unprocessed_bound "$2" \
	"$3" 2> "$4.err" | xz -0 > "$4.out.xz"
