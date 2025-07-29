(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2016 Konstantin Korovin and The University of Manchester. 
   This file is part of iProver - a theorem prover for first-order logic.

   iProver is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or 
   (at your option) any later version.
   iProver is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
   See the GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with iProver.  If not, see <http://www.gnu.org/licenses/>.         *)
(*----------------------------------------------------------------------[C]-*)


val time_global_flag : bool

type stat_int_entry
type stat_float_entry
type stat_fun_entry 

(** Set stat *)
val assign_int_stat : int  -> stat_int_entry -> unit
(** Add 1 to stat *)
val bump_int_stat   : stat_int_entry ->  unit
(** Add n to stat *)
val incr_int_stat   : int  -> stat_int_entry ->  unit

(** Set stat *)
val assign_float_stat : float -> stat_float_entry -> unit
(** Add n to stat *)
val add_float_stat   : float -> stat_float_entry -> unit

(** Set function that will return the stat value *)
val assign_fun_stat : (unit -> int) -> stat_fun_entry -> unit 


(** Return values *)
val get_val_stat       : stat_int_entry   -> int
val get_float_val_stat : stat_float_entry -> float

(** Can fail if fun is Undef *)    
val get_val_stat_fun   : stat_fun_entry   -> int

(** Returns 0 if fun is Undef *)    
val get_val_stat_fun0   : stat_fun_entry   -> int
    
(* runs function and add running time to stat (not working) *)
(* val run_and_time  :  stat_float_entry -> ('a-> 'b) -> 'a -> 'b *)



(* Time stats *)

(** record timing statistics; usage e.g.:
 let prop_solve ... =    
    Statistics.time Statistics.prop_solver_time @@ fun () -> 
    code block
 *)      
val time : stat_float_entry -> (unit -> 'a) -> 'a 

(** Run at start and end of block. Time difference will be added to stat. 
    Globally toggleable, if these statistics are not wanted. *)
(*
val time_start : stat_float_entry -> unit
val time_end   : stat_float_entry -> unit
*)


(*------- Problem properties---------*)

val clauses       : stat_int_entry
val conjectures   : stat_int_entry
val epr           : stat_int_entry
val horn          : stat_int_entry
val ground        : stat_int_entry
val unary           : stat_int_entry
val binary          : stat_int_entry
val lits          : stat_int_entry
val lits_eq       : stat_int_entry
val fd_pure         : stat_int_entry
val fd_pseudo       : stat_int_entry
val fd_cond         : stat_int_entry
val fd_pseudo_cond : stat_int_entry
val ac_symbols    : stat_fun_entry

(*-------General---------*)

val abstr_ref_over_cycles : stat_int_entry
val abstr_ref_under_cycles : stat_int_entry
val gc_basic_clause_elim : stat_int_entry
val num_of_symbols       : stat_fun_entry 
val num_of_terms         : stat_fun_entry 

val num_of_input_clauses : stat_int_entry 
val num_of_input_neg_conjectures : stat_int_entry 

(*-- timings *)
val parsing_time         : stat_float_entry 
val unif_index_cands_time : stat_float_entry 
val unif_index_add_time  : stat_float_entry 
val orderings_time       : stat_float_entry 
val out_proof_time       : stat_float_entry 
val total_time           : stat_float_entry 

(*-------- Preprocessing ---------*)

val num_of_splits        : stat_int_entry
val num_of_fresh_symb    : stat_int_entry 
val num_of_reused_defs   : stat_int_entry 
val num_eq_ax_congr_red  : stat_int_entry
val num_of_sem_filtered_clauses : stat_int_entry
val num_of_subtypes : stat_int_entry
val monotx_restored_types : stat_int_entry
val sat_num_of_epr_types : stat_int_entry
val sat_num_of_non_cyclic_types : stat_int_entry
val sat_num_of_guarded_non_collapsed_types : stat_int_entry
val pure_diseq_elim : stat_int_entry
val simp_replaced_by : stat_int_entry
val res_preprocessed : stat_int_entry
val sup_preprocessed : stat_int_entry
val prep_upred       : stat_int_entry
val prep_unflattend :  stat_int_entry
val prep_well_definedness : stat_int_entry
val smt_new_axioms  :  stat_int_entry
val pred_elim_cands : stat_int_entry
val pred_elim :  stat_int_entry
val pred_elim_cl :  stat_int_entry
val pred_elim_cycles : stat_int_entry
val merged_defs : stat_int_entry
val merged_defs_ncl : stat_int_entry
val bin_hyper_res : stat_int_entry
val prep_cycles : stat_int_entry

(*-- timings *)
val splitting_time       : stat_float_entry
val sem_filter_time      : stat_float_entry 
val monotx_time          : stat_float_entry  
val subtype_inf_time     : stat_float_entry
val res_prep_time        : stat_float_entry
val sup_prep_time        : stat_float_entry
val pred_elim_time       : stat_float_entry
val bin_hyper_res_time   : stat_float_entry
val prep_time_total      : stat_float_entry

(*----Propositional Solver-----*)

val prop_solver_calls              : stat_fun_entry 
val prop_fast_solver_calls         : stat_fun_entry 
val smt_solver_calls               : stat_int_entry 
val smt_fast_solver_calls          : stat_int_entry 
val prop_num_of_clauses            : stat_int_entry 
val prop_preprocess_simplified     : stat_int_entry 
val prop_fo_subsumed               : stat_int_entry 

(*-- timings *)
val prop_solver_time               : stat_float_entry 
val prop_fast_solver_time          : stat_float_entry 
val prop_unsat_core_time           : stat_float_entry 
val smt_solver_time                : stat_float_entry 
val smt_fast_solver_time           : stat_float_entry 

(*------- QBF ---------*)

val qbf_q_res                      : stat_int_entry
val qbf_num_tautologies            : stat_int_entry
val qbf_prep_cycles                : stat_int_entry

(*----BMC1---------------------*)

val bmc1_current_bound             : stat_int_entry
val bmc1_last_solved_bound         : stat_int_entry
val bmc1_unsat_core_size           : stat_int_entry
val bmc1_unsat_core_parents_size   : stat_int_entry
val bmc1_merge_next_func           : stat_int_entry
val bmc1_unsat_core_clauses_time   : stat_float_entry

(*----Instantiation------------*)

val inst_num_of_clauses            : stat_fun_entry 
val inst_num_in_passive            : stat_fun_entry 
val inst_num_in_active             : stat_int_entry 
val inst_num_in_unprocessed        : stat_int_entry 
val inst_num_of_loops              : stat_int_entry 
val inst_num_of_learning_restarts  : stat_int_entry 
val inst_num_moves_active_passive  : stat_int_entry 
val inst_max_lit_activity          : stat_int_entry 
val inst_lit_activity_moves        : stat_int_entry 
val inst_num_tautologies           : stat_int_entry 
val inst_num_of_duplicates         : stat_int_entry 
val inst_num_prop_implied          : stat_int_entry 
val inst_num_existing_simplified   : stat_int_entry 
val inst_num_eq_res_simplified     : stat_int_entry
val inst_num_child_elim            : stat_int_entry 
val inst_num_of_dismatching_blockings : stat_int_entry 
val inst_num_of_non_proper_insts   : stat_int_entry 
val inst_num_of_duplicates         : stat_int_entry  
val inst_num_from_inst_to_res      : stat_int_entry 

(*-- timings *)
val inst_time_sim_new                  : stat_float_entry
val inst_time_sim_given                : stat_float_entry
val inst_time_dismatching_checking     : stat_float_entry
val inst_time_total                    : stat_float_entry

val clear_inst_stat                : unit -> unit

(*-----Resolution----------*)

val res_num_of_clauses                   : stat_fun_entry 
val res_num_in_passive                   : stat_fun_entry 
val res_num_in_active                    : stat_int_entry 
val res_num_of_loops                     : stat_int_entry 
val res_forward_subset_subsumed          : stat_int_entry  
val res_backward_subset_subsumed         : stat_int_entry 
val res_forward_subsumed                 : stat_int_entry 
val res_backward_subsumed                : stat_int_entry 
val res_forward_subsumption_resolution   : stat_int_entry 
val res_backward_subsumption_resolution  : stat_int_entry 
val res_clause_to_clause_subsumption     : stat_int_entry 
val res_subs_bck_cnt                     : stat_int_entry  
val res_orphan_elimination               : stat_int_entry 
val res_tautology_del                    : stat_int_entry 
val res_num_eq_res_simplified            : stat_int_entry
val res_num_sel_changes                  : stat_int_entry 
val res_moves_from_active_to_pass        : stat_int_entry 

(*-- timings *)
val res_time_sim_new             : stat_float_entry
val res_time_sim_fw_given        : stat_float_entry
val res_time_sim_bw_given        : stat_float_entry
val res_time_total               : stat_float_entry

(* -- Superposition -- *)

val sup_num_of_clauses            : stat_fun_entry
val sup_num_in_active             : stat_fun_entry
val sup_num_in_passive            : stat_fun_entry
val sup_immediate_simplified      : stat_int_entry
val sup_given_eliminated          : stat_int_entry
val sup_num_of_loops              : stat_int_entry
val sup_fw_superposition          : stat_int_entry
val sup_bw_superposition          : stat_int_entry
val sup_eq_factoring              : stat_int_entry
val sup_eq_resolution             : stat_int_entry
val sup_given_eliminated          : stat_int_entry
val sup_immediate_simplified      : stat_int_entry
val comparisons_done              : stat_int_entry
val comparisons_avoided           : stat_int_entry
val comparisons_inc_criteria      : stat_int_entry
val sup_deep_cl_discarded         : stat_int_entry
val sup_num_of_deepenings         : stat_int_entry
val sup_num_of_restarts           : stat_int_entry

(*-- timings *)
val sup_time_generating          : stat_float_entry
val sup_time_sim_fw_full         : stat_float_entry
val sup_time_sim_bw_full         : stat_float_entry
val sup_time_sim_fw_immed        : stat_float_entry
val sup_time_sim_bw_immed        : stat_float_entry
val sup_time_prep_sim_fw_input   : stat_float_entry
val sup_time_prep_sim_bw_input   : stat_float_entry
val sup_time_total               : stat_float_entry

(* -- Simplifications -- *)

val sim_repeated           : stat_int_entry
val sim_fw_subset_subsumed : stat_int_entry
val sim_bw_subset_subsumed : stat_int_entry
val sim_fw_subsumed        : stat_int_entry
val sim_bw_subsumed        : stat_int_entry
val sim_fw_subsumption_res : stat_int_entry
val sim_bw_subsumption_res : stat_int_entry
val sim_fw_unit_subs       : stat_int_entry
val sim_bw_unit_subs       : stat_int_entry
val sim_tautology_del      : stat_int_entry
val sim_eq_tautology_del   : stat_int_entry
val sim_eq_res_simp        : stat_int_entry
val sim_fw_demodulated     : stat_int_entry
val sim_bw_demodulated     : stat_int_entry
val sim_encompassment_demod: stat_int_entry
val sim_demod_cache_hit_pos : stat_int_entry
val sim_demod_cache_hit_neg : stat_int_entry
val sim_demod_cache_miss    : stat_int_entry   
val sim_light_normalised   : stat_int_entry
val sim_ac_normalised      : stat_int_entry
val sim_ac_joinable_taut   : stat_int_entry
val sim_ac_joinable_simp   : stat_int_entry
val sim_fw_ac_demod        : stat_int_entry
val sim_bw_ac_demod        : stat_int_entry
val sim_smt_subsumption    : stat_int_entry
val sim_smt_simplified     : stat_int_entry
val sim_ground_joinable    : stat_int_entry
val sim_bw_ground_joinable : stat_int_entry
val sim_connectedness      : stat_int_entry

(*-- timings *)
val sim_time_fw_subset_subs     : stat_float_entry
val sim_time_bw_subset_subs     : stat_float_entry
val sim_time_fw_subs            : stat_float_entry
val sim_time_bw_subs            : stat_float_entry
val sim_time_fw_subs_res        : stat_float_entry
val sim_time_bw_subs_res        : stat_float_entry
val sim_time_fw_unit_subs       : stat_float_entry
val sim_time_bw_unit_subs       : stat_float_entry
val sim_time_tautology_del      : stat_float_entry
val sim_time_eq_tautology_del   : stat_float_entry
val sim_time_eq_res_simp        : stat_float_entry
val sim_time_fw_demod           : stat_float_entry
val sim_time_bw_demod           : stat_float_entry
val sim_time_light_norm         : stat_float_entry
(* val sim_time_joinable_taut      : stat_float_entry *)
(* val sim_time_joinable_simp      : stat_float_entry *)
val sim_time_joinable           : stat_float_entry
val sim_time_ac_norm            : stat_float_entry
val sim_time_fw_ac_demod        : stat_float_entry
val sim_time_bw_ac_demod        : stat_float_entry
val sim_time_smt_subs           : stat_float_entry
val sim_time_fw_gjoin           : stat_float_entry
val sim_time_fw_connected       : stat_float_entry



(* -- Misc -- *)
val clear_res_stat : unit -> unit
val clear_sup_stat : unit -> unit

val out_stat : unit -> unit
