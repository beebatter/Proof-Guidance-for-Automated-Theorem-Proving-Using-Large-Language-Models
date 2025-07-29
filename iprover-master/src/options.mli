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

open Lib







(*-----------------Option Types---------------------------*)

type out_options_type = Out_All_Opt | Out_Control_Opt | Out_No_Opt

(*-----------*)
type stats_out_type = Stats_out_none | Stats_out_sel | Stats_out_all | Stats_out_nonzero

(*----- proof dot output *)

type proof_reduce_dot_type = 
  | PRD_unit
  | PRD_all_neg
  | PRD_all_pos
  | PRD_ground

(** Output no statistics, statistics after every bound or only after
    the last bound in BMC1 *)
type bmc1_out_stat_type =
    BMC1_Out_Stat_None | BMC1_Out_Stat_Full | BMC1_Out_Stat_Last

(** Axioms for BMC1 *)
type bmc1_axioms_type =
    BMC1_Axioms_Reachable_All | BMC1_Axioms_Reachable_Last

(** Adding unsat core for next bound *)
type bmc1_add_unsat_core_type =
  | BMC1_Add_Unsat_Core_None (** Do not add clauses from unsat core *)
  | BMC1_Add_Unsat_Core_Clauses (** Add clauses in unsat core *)
  | BMC1_Add_Unsat_Core_Leaves (** Add leaf (input) clauses *)
  | BMC1_Add_Unsat_Core_All (** Add all clauses and their parents *)

type bmc1_ucm_cone_mode_type =
  | BMC1_Ucm_Cone_Mode_None
  | BMC1_Ucm_Cone_Mode_AIG
  | BMC1_Ucm_Cone_Mode_Symb
  | BMC1_Ucm_Cone_Mode_UC

(*--------*)
type splitting_mode_type = Split_Input |Split_Full | Split_None

(*--------*)
type prep_sem_filter_type =
    Sem_Filter_None | Sem_Filter_Pos | Sem_Filter_Neg | Sem_Filter_Exhaustive

type sched_opt_time = string * float (* option file with timout *)
type sched_opt_files = sched_opt_time list

type schedule_type =
  |Schedule_none
  |Schedule_default
  |Schedule_sat
  |Schedule_opt_files of sched_opt_files
(*  | Schedule_opt_files of string list *)
  |Schedule_abstr_ref
  |Schedule_abstr_ref_sat
  |Schedule_verification_epr_old
  |Schedule_verification_epr_tables
  |Schedule_verification_epr
  |Schedule_smac_tmp
  |Schedule_superposition
(*  |Schedule_iter_default *)

(*-----Lit Params----------*)

type lit_cmp_type =
  | Lit_Sign    of bool
  | Lit_Ground  of bool
  | Lit_Prop    of bool
  | Lit_Num_of_Var  of bool
  | Lit_Num_of_Symb of bool
  | Lit_Atom_depth of bool
  | Lit_Definition of bool
  | Lit_Id         of bool
  | Lit_has_conj_symb of bool
  | Lit_has_bound_constant of bool
  | Lit_next_state of bool
  | Lit_reachable_state of bool
  | Lit_has_non_prolific_conj_symb of bool
  | Lit_eq                         of bool
  | Lit_clock                      of bool
  | Lit_less                       of bool
  | Lit_range                      of bool

(*----------------------------- *)

type lit_bool_prop_type = 
  | Lit_bp_sign of bool
  | Lit_bp_epr of bool
  | Lit_bp_eq of bool 
  | Lit_bp_ground of bool 
  | Lit_bp_prop of bool 
  | Lit_bp_conj_symb of bool 

type prop_impl_unit_type = lit_bool_prop_type list

(*----Clause Param---------*)
type cl_cmp_type =
  |Cl_Age         of bool
  |Cl_Num_of_Var  of bool
  |Cl_Num_of_Symb of bool
  |Cl_Num_of_Lits of bool
  |Cl_Score       of bool
  |Cl_External_Score of bool
  |Cl_Ground      of bool
  |Cl_Conj_Dist   of bool
  |Cl_Has_Conj_Symb of bool
  |Cl_has_bound_constant of bool
  |Cl_has_next_state of bool
  |Cl_has_reachable_state of bool
  |Cl_Has_Non_Prolific_Conj_Symb of bool
  |Cl_Max_Atom_Input_Occur of bool
  |Cl_Horn         of bool
  |Cl_EPR          of bool
  |Cl_in_unsat_core of bool
  |Cl_Has_Eq_Lit   of bool
  |Cl_min_defined_symb of bool
  |Cl_bc_imp_inh of bool
  |Cl_abstr_ref_concr of bool
  |Cl_rnd of bool
        
val cl_cmp_type_to_str : cl_cmp_type -> string

(*-------------------- smt preprocessing --------------*)
module Flag_SMT_AC : sig
  type t = Full | Fast | Off
  val to_string : t -> string
  val of_string : string -> t
end

(*-------------------- bc_imp_inh: important basic clauses --------------*)

type bc_imp_inh_type = BCI_bmc1_lemma | BCI_conj_cone

type bc_imp_inh_list_type = bc_imp_inh_type list

(*----------- abstr-ref -----------*)
exception Unknown_abstr_ref_type

type abstr_ref_type = 
  |Abstr_ref_identity
  |Abstr_ref_subs 
  |Abstr_ref_sig  
  |Abstr_ref_arg_filter
  |Abstr_ref_gen

val abstr_ref_type_to_str : abstr_ref_type -> string
val str_to_abstr_ref_type : string -> abstr_ref_type

type abstr_ref_list_type = abstr_ref_type list 
val abstr_ref_list_type_to_str : abstr_ref_list_type -> string
val str_to_abstr_ref_list_type : string ->  abstr_ref_list_type    

(*-------------------- Abstr_ref signature abstraction restricted ----------------*)
exception Unknown_abstr_ref_sig_restrict_type

type abstr_ref_sig_restrict_type = Funpre | Skc

val abstr_ref_sig_restrict_type_to_str : abstr_ref_sig_restrict_type -> string
val str_to_abstr_ref_sig_restrict_type : string -> abstr_ref_sig_restrict_type

(*-------------------- Abstr_ref under-approximating abstractions ----------------*)
exception Unknown_abstr_ref_under_type

type abstr_ref_under_type =
  | Abstr_ref_under_identity
  | Abstr_ref_under_cone

val abstr_ref_under_type_to_str : abstr_ref_under_type -> string
val str_to_abstr_ref_under_type : string -> abstr_ref_under_type

type abstr_ref_under_list_type = abstr_ref_under_type list
val abstr_ref_under_list_type_to_str : abstr_ref_under_list_type -> string
val str_to_abstr_ref_under_list_type : string ->  abstr_ref_under_list_type

(*--------------*)
type qbf_dom_inst_type = 
  | QBF_dom_inst_none
  | QBF_dom_inst_single 
  | QBF_dom_inst_chain

(*---Inst Lit Sel----*)

type inst_lit_sel_type    = lit_cmp_type list

type cl_measure_type = 
  |CM_num_lit
  |CM_num_var
  |CM_num_symb
  |CM_cnt
  |CM_none

type passive_queue_type   = PQT_Queue | PQT_Stack | PQT_List | PQT_PriorityQueues | PQT_External_Agent
type pass_queue_type      = cl_cmp_type  list
type pass_queue_list_type = pass_queue_type list
type passive_queue_freqs  = int list

type inst_sel_renew_type = Inst_SR_Solver | Inst_SR_Model



(*---------------------sat_out_model option types-----------------------------------*)

type sat_out_model_type = Model_Small | Model_Pos | Model_Neg | Model_Implied | Model_Debug | Model_Intel |Model_None


(*--------------------Resolution Option Types--------------*)

(*----Subsumption----*)
type res_subs_type = Subs_Full | Subs_Subset | Subs_By_Length of int

(*---Selection Fun----*)
type res_lit_sel_type =
   Res_adaptive 
  | Res_adaptive_neg 
  | Res_adaptive_max  
  | Res_KBO_max 
  | Res_neg_sel_max 
  | Res_neg_sel_min 
  | Res_pos_sel_max 
  | Res_pos_sel_min 
  | Res_neg_sel_nrc

(*-----*)
type res_ord_type =
  | Res_ord_kbo
  | Res_ord_kbo_pred

type res_to_prop_solver_type =
    Res_to_Solver_Active | Res_to_Solver_Passive | Res_to_Solver_None

(*-----*)

module Demod_check : sig
  type t = Full | Fast | Off | FullOld | FastOld
  val to_string : t -> string
  val of_string : string -> t
end


module SupSimplificationSetup : sig
  type index = 
    (* | SubsumptionIndex *)
    (* | SubsetSubsumptionIndex *)
    | FwDemodIndex
    | BwDemodIndex
    | LightNormIndexReduce
    | LightNormIndexNoreduce
    | SMTIncrIndex
    | SMTSetIndex
    | UnitSubsumptionIndex
    | NonunitSubsumptionIndex
    | FwACDemodIndex
    | BwACDemodIndex
    | RevDemodIndex
    | BwGjoinIndex
    (* | HybridSubsumptionIndex *)
 
  type trivRule = 
    (* | EqResolutionSimp
    | TautologyElim
    | EqTautologyElim
    | TrivRules *)
    | PropSubs
    | Unflattening
    | SMTSimplify

  type fwRule =
    (* | FwSubsetSubsumption *)
    | FwSubsumption
    (* | FwSubsumptionNonStrict *)
    | FwSubsumptionRes
    | FwUnitSubsumption
    (* | FwHybridSubsumption *)
    | FwDemod
    | FwDemodLoopTriv
    | FwLightNorm
    | FwDemodLightNormLoopTriv
    | ACJoinability
    | ACNormalisation
    | SMTSubs
    | FwACDemod
    | FwGroundJoinability
    | FwConnectedness

  type bwRule =
    (* | BwSubsetSubsumption *)
    | BwSubsumption
    | BwSubsumptionRes
    | BwUnitSubsumption
    (* | BwHybridSubsumption *)
    | BwDemod
    | BwACDemod

  module CacheSim : sig type t = All | Once | None end



  type spec = {
    (* mutable indices_passive: index list;
    mutable indices_active: index list;
    mutable indices_immed: index list;
    mutable indices_input: index list; *)
    mutable indices_passive: index list;

    mutable full_triv: trivRule list;
    mutable full_fw: fwRule list;
    mutable full_bw: bwRule list;

    mutable immed_triv: trivRule list;
    mutable immed_fw_main: fwRule list;
    mutable immed_fw_immed: fwRule list;
    mutable immed_bw_main: bwRule list;
    mutable immed_bw_immed: bwRule list;

    mutable input_triv: trivRule list;
    mutable input_fw: fwRule list;
    mutable input_bw: bwRule list;

    mutable full_fixpoint: bool;
    mutable main_fixpoint: bool;
    mutable immed_fixpoint: bool;
    mutable input_fixpoint: bool;

    mutable cache_sim: CacheSim.t;
    mutable sup_smt_interval: int;
    mutable sup_bw_gjoin_interval: int;
  }
end


type sup_to_prop_solver_type =
    Sup_to_Solver_Active | Sup_to_Solver_Passive | Sup_to_Solver_None

module Ordering : sig
  module Func : sig
    type t = 
      KBO | KBOalt | LPO | Theory | TheoryN of int
  end

  module Symb : sig
    type t =
      | Invfreq | InvfreqArity | InvfreqInvArity | Arity | ArityRev | ArityRandom | Random
      | Custom of string list
  end

  module Weight : sig
    type t =
      | Default
      | Custom of {wvar: int; wsym: (string * int) list}
  end
end


type sup_score_type = 
  | Sup_score_sim
  | Sup_score_sim_gen
  | Sup_score_sim_d_gen 


(*---------*)

type extra_neg_conj_type = ENC_none | ENC_all_neg | ENC_all_pos | ENC_all_pos_neg


(*---------*)
type comb_type =     
  |Comb_clause_based 
  |Comb_time_based

(*-----All options-----*)

(* Warning: functional options such as inst_lit_sel and inst_pass_queue1 *)
(* declare only types! if the options are changed, *)
(* one needs to change corresponding functions separately *)

type options = {
    mutable out_options           : out_options_type; 
    mutable tptp_safe_out         : bool;

    (*----Input-------*)
    mutable problem_path          : string;
    mutable include_path          : string;
    mutable problem_files         : string list;
    mutable clausifier            : string;
    mutable clausifier_options    : string;
    mutable stdin                 : bool;
    mutable proof_out             : bool;
    mutable proof_dot_file        : string; 
    mutable proof_reduce_dot      : proof_reduce_dot_type list;
    mutable suppress_sat_res      : bool;
    mutable suppress_unsat_res    : bool;
    mutable stats_out             : stats_out_type;
    mutable stats_mem             : bool;
    mutable theory_stats_out      : bool;

    (*----General--------*)
    mutable fof                   : bool;
    mutable time_out_real         : float;
    mutable time_out_prep_mult    : float;
    mutable time_out_virtual      : float;
    mutable rnd_seed              : int;
    mutable schedule              : schedule_type;
    mutable preprocessing_flag    : bool;
    mutable splitting_mode        : splitting_mode_type;
    mutable splitting_grd         : bool;
    mutable splitting_cvd         : bool;
    mutable splitting_cvd_svl     : bool;
    mutable splitting_nvd         : int;
    mutable non_eq_to_eq          : bool;
    mutable prep_gs_sim           : bool;
    mutable prep_unflatten        : bool;
    mutable prep_res_sim          : bool;
    mutable prep_sup_sim_all      : bool;
    mutable prep_sup_sim_sup      : bool;
    mutable prep_upred            : bool;
    mutable prep_well_definedness : bool;
    mutable res_sim_input         : bool;
    mutable clause_weak_htbl      : bool;
    mutable gc_record_bc_elim     : bool;
    mutable symbol_type_check     : bool;
    mutable clausify_out          : bool;
    mutable sig_cnt_out           : bool;
    mutable trig_cnt_out          : bool;
    mutable trig_cnt_out_tolerance : float;
    mutable trig_cnt_out_sk_spl   : bool;
    mutable abstr_cl_out          : bool;

    mutable interactive_mode : bool;
    mutable external_ip_address : string; 
    mutable external_port : int;

    mutable prep_sem_filter       : prep_sem_filter_type;
    mutable prep_sem_filter_out   : bool;
    mutable preprocessed_out      : bool;
    mutable preprocessed_stats    : bool;
    mutable sub_typing            : bool;
    mutable prep_eq_flat_conj     : bool;
    mutable prep_eq_flat_all_gr   : bool;
    mutable eq_ax_congr_red       : bool;
    mutable brand_transform       : bool;
    mutable pure_diseq_elim       : bool;
    mutable min_unsat_core        : bool;
    mutable pred_elim             : bool;
    mutable prop_solver_per_cl    : int;
    mutable subs_bck_mult         : int;
    mutable add_important_lit     : bool;
    mutable soft_assumptions      : bool;
    mutable soft_lemma_size       : int;
    mutable prop_impl_unit_size   : int; 
    mutable prop_impl_unit        : prop_impl_unit_type;
    mutable share_sel_clauses     : bool;
    mutable smt_preprocessing     : bool;
    mutable smt_ac_axioms         : Flag_SMT_AC.t;
    mutable reset_solvers         : bool;
    mutable bc_imp_inh            : bc_imp_inh_list_type;
    mutable conj_cone_tolerance   : float;
    mutable extra_neg_conj        : extra_neg_conj_type;

    mutable abstr_ref : abstr_ref_list_type;
    (*
    mutable abstr_ref_arg_filter  : bool;
    mutable abstr_ref_sig         : bool;
    mutable abstr_ref_subs        : bool;
    *)
    mutable abstr_ref_prep        : bool;
    mutable abstr_ref_until_sat   : bool;
    (* mutable abstr_terms_sig       : bool;
     * mutable abstr_skolem_sig      : bool; *)
    mutable abstr_ref_sig_restrict : abstr_ref_sig_restrict_type;
    mutable abstr_ref_af_restrict_to_split_sk : bool;
    mutable abstr_ref_under : abstr_ref_under_list_type;
    mutable prep_def_merge        : bool;
    mutable prep_def_merge_prop_impl : bool;
    mutable prep_def_merge_mbd    : bool;
    mutable prep_def_merge_tr_red : bool;
    mutable prep_def_merge_tr_cl  : bool;

    (*---Large Theories---------------*)
    mutable large_theory_mode     : bool;
    mutable prolific_symb_bound   : int;
    (*---threshold when the theory is considered to be large---*)
    mutable lt_threshold          : int;

    (*----Sat Mode-----------*)
    mutable sat_mode              : bool;
    mutable sat_fm_restart_options : string;
    mutable sat_gr_def            : bool;
    mutable sat_epr_types         : bool;
    mutable sat_non_cyclic_types  : bool;
    mutable sat_finite_models     : bool;
    mutable sat_fm_lemmas         : bool;
    mutable sat_fm_prep           : bool;
    mutable sat_fm_uc_incr        : bool;   
    mutable sat_out_model         : sat_out_model_type;
    mutable sat_out_clauses       : bool;

    (*---- QBF Mode-----------*)
    mutable qbf_mode      : bool;
    mutable qbf_elim_univ : bool;
    mutable qbf_dom_inst  : qbf_dom_inst_type;
    mutable qbf_dom_pre_inst : bool;
    mutable qbf_sk_in     : bool;
    mutable qbf_pred_elim : bool;
    mutable qbf_split     : int;

    (*----BMC1---------------*)
    mutable bmc1_incremental      : bool;
    mutable bmc1_axioms           : bmc1_axioms_type;
    mutable bmc1_min_bound        : int;
    mutable bmc1_max_bound        : int;
    mutable bmc1_max_bound_default : int;
    mutable bmc1_symbol_reachability : bool;
    mutable bmc1_property_lemmas  : bool;
    mutable bmc1_k_induction      : bool;
    mutable bmc1_non_equiv_states : bool;
    mutable bmc1_deadlock         : bool;
    mutable bmc1_ucm              : bool;
    mutable bmc1_add_unsat_core   : bmc1_add_unsat_core_type;
    mutable bmc1_unsat_core_children : bool;
    mutable bmc1_unsat_core_extrapolate_axioms : bool;
    mutable bmc1_ground_init          : bool;
    mutable bmc1_pre_inst_next_state  : bool;
    mutable bmc1_pre_inst_state       : bool;
    mutable bmc1_pre_inst_reach_state : bool;
    mutable bmc1_out_stat         : bmc1_out_stat_type;
    mutable bmc1_out_unsat_core   : bool;
    mutable bmc1_aig_witness_out  : bool;
    mutable bmc1_verbose          : bool;
    mutable bmc1_dump_clauses_tptp : bool;
    mutable bmc1_dump_unsat_core_tptp : bool;
    mutable bmc1_dump_file        : string option;
    (*----BMC1 UCM --*)
    mutable bmc1_ucm_expand_uc_limit : int;
    mutable bmc1_ucm_n_expand_iterations : int;
    mutable bmc1_ucm_extend_mode : int;
    mutable bmc1_ucm_init_mode : int;
    mutable bmc1_ucm_cone_mode : bmc1_ucm_cone_mode_type;
    mutable bmc1_ucm_reduced_relation_type : int;
    mutable bmc1_ucm_relax_model : int;
    mutable bmc1_ucm_full_tr_after_sat : bool;
    mutable bmc1_ucm_expand_neg_assumptions : bool;
    mutable bmc1_ucm_layered_model : bmc1_ucm_cone_mode_type;
    (* lemmas *)
    mutable bmc1_ucm_max_lemma_size : int;

    (*----AIG----------------*)
    mutable aig_mode               : bool;

    (*----Instantiation------*)
    mutable instantiation_flag                : bool;
    mutable inst_sos_flag                     : bool;
    mutable inst_sos_phase                    : bool;
    mutable inst_sos_sth_lit_sel              : inst_lit_sel_type;
    mutable inst_lit_sel                      : inst_lit_sel_type;
    mutable inst_lit_sel_side                 : cl_measure_type;
    mutable inst_solver_per_active            : int;  
    (* mutable inst_solver_per_clauses           : int; *)
    mutable inst_solver_calls_frac            : float;
    mutable inst_to_smt_solver                 : bool;
    mutable inst_passive_queue_type           : passive_queue_type;
    mutable inst_passive_queues               : pass_queue_list_type;
    mutable inst_passive_queues_freq          : passive_queue_freqs;
    mutable inst_dismatching                  : bool;
    mutable inst_eager_unprocessed_to_passive : bool;
    mutable inst_unprocessed_bound            : int;
    mutable inst_prop_sim_given               : bool;
    mutable inst_prop_sim_new                 : bool;
    mutable inst_subs_given                   : bool;
    mutable inst_subs_new                     : bool;    
    mutable inst_eq_res_simp                  : bool;
    mutable inst_orphan_elimination           : bool;
    mutable inst_learning_loop_flag           : bool;
    mutable inst_learning_start               : int;
    mutable inst_learning_factor              : int;
    mutable inst_start_prop_sim_after_learn   : int;
    mutable inst_sel_renew                    : inst_sel_renew_type;
    mutable inst_lit_activity_flag            : bool;
    mutable inst_restr_to_given               : bool;
    mutable inst_activity_threshold           : int;

    (*----Resolution---------*)
    mutable resolution_flag               : bool;
    mutable res_lit_sel                   : res_lit_sel_type;
    mutable res_lit_sel_side              : cl_measure_type;
    mutable res_ordering                  : res_ord_type;
    mutable res_to_prop_solver            : res_to_prop_solver_type;
    mutable res_prop_simpl_new            : bool;
    mutable res_prop_simpl_given          : bool;
    mutable res_to_smt_solver              : bool;
    
    mutable res_passive_queue_type        : passive_queue_type;
    mutable res_passive_queues            : pass_queue_list_type;
    mutable res_passive_queues_freq       : passive_queue_freqs;

    mutable res_forward_subs              : res_subs_type;
    mutable res_backward_subs             : res_subs_type;
    mutable res_forward_subs_resolution   : bool;
    mutable res_backward_subs_resolution  : bool;
    mutable res_orphan_elimination        : bool;
    mutable res_time_limit                : float;

    (* ---Superposition--- *)
    mutable superposition_flag            : bool;
    mutable sup_passive_queue_type        : passive_queue_type;
    mutable sup_passive_queues            : pass_queue_list_type;
    mutable sup_passive_queues_freq       : passive_queue_freqs;
    mutable demod_completeness_check      : Demod_check.t;
    mutable demod_use_ground              : bool;
    mutable sup_unprocessed_bound         : int;
    mutable sup_to_prop_solver            : sup_to_prop_solver_type;
    mutable sup_prop_simpl_new            : bool;
    mutable sup_prop_simpl_given          : bool;
    mutable sup_fun_splitting             : bool;
    mutable sup_iter_deepening            : int;
    mutable sup_restarts_mult             : int;
    mutable sup_score                     : sup_score_type;
    mutable sup_share_score_frac          : float;
    mutable sup_share_max_num_cl          : int;
    mutable sup_simplification_setup      : SupSimplificationSetup.spec;
    (* mutable sup_smt_interval              : int; *)
    mutable sup_ordering                  : Ordering.Func.t;
    mutable sup_symb_ordering             : Ordering.Symb.t;
    mutable sup_term_weight               : Ordering.Weight.t;

    (*----Combination--------*)
    mutable comb_mode           : comb_type;
    mutable comb_inst_mult      : int;
    mutable comb_res_mult       : int;
    mutable comb_sup_mult       : int;
    mutable comb_sup_deep_mult  : int;

   (*------Debug---------*)
    mutable dbg_backtrace : bool;
    mutable dbg_dump_prop_clauses : bool;
    mutable dbg_dump_prop_clauses_file : string;
    mutable dbg_out_stat : bool;
    mutable dbg_just_parse : bool; 

}

type named_options = {options_name : string; options : options}

(* global options are modified by the command line   *)
(* in proof search modules we pass schedule options; which are modifications of global/default options *)
(* global_options are normally used directly in input/output parts of iProver *)

val global_options : options ref 

val get_named_global_options : unit -> named_options 

val input_options : options
val input_named_options : named_options

(*-------------------------------------------*)
(* maps bc_imp_inh into integer priorities; *)
(* priority decreases with values highest priority is 0, lowest max_int *)
(* priorities are grouped in blocks by shifting by 8 bits;  *)   

(* maps bc_imp_inh into integer priorities *)   
val bc_imp_inh_default_val : int

(* get the summand for the priority block corresponding to bc_imp_inh_type  *)
val get_bc_imp_inh_shift : options -> bc_imp_inh_type -> int

val bc_imp_inh_exists :  options -> bc_imp_inh_type -> bool

(*-------------------------------------------*)
(* if there is no conjectures then we can to remove corresponding comparisons*)

val strip_conj_named_opt : named_options -> named_options


type opt_val_type = string * string

val opt_val_to_string : opt_val_type -> string

val opt_val_list_to_string : opt_val_type list -> string

val opt_val_to_stdout : opt_val_type -> unit

val opt_val_list_to_stdout : opt_val_type list -> unit

val pass_queues_type_to_str : pass_queue_list_type -> string
val passive_queue_freqs_to_str : int list -> string

(*--------Creates a reasonable option to deal with many axioms such as SUMO-----*)
(*-------based on a given option-------------------*)
val named_opt_to_many_axioms_named_opt1 : named_options -> named_options
val named_opt_to_many_axioms_named_opt2 : named_options -> named_options
val named_opt_to_many_axioms_named_opt3 : named_options -> named_options

(* switch off sub_typing, pred_elim, sem_filter*)
val named_opt_to_prep_tff : named_options -> named_options

val named_option_1   : unit -> named_options
val named_option_1_1 : unit -> named_options
val named_option_1_2 : unit -> named_options
val named_option_2   : unit -> named_options
val named_option_2_1 : unit -> named_options
val named_option_3   : unit -> named_options
val named_option_3_1 : unit -> named_options
val named_option_4   : unit -> named_options
val named_option_4_1 : unit -> named_options

val named_option_finite_models : unit -> named_options
val named_opt_sat_mode_off : named_options -> named_options
val named_opt_sat_mode_on  : named_options -> named_options

val named_option_epr_non_horn_non_eq : unit -> named_options
val named_option_epr_non_horn_eq : unit -> named_options

(* val option_epr_non_horn_eq : unit -> options *)

val named_option_epr_horn_non_eq     : unit -> named_options
(* val option_epr_horn : unit -> options *)


val named_option_verification_epr_old : unit -> named_options
val named_option_verification_epr_tables : unit -> named_options
val named_option_verification_epr : unit -> named_options

val named_option_ueq : unit -> named_options 
val named_option_ueq_1 : unit -> named_options 
val named_option_ueq_2 : unit -> named_options 
val named_option_ueq_3 : unit -> named_options 

val named_option_smac_tmp : unit -> named_options

val res_lit_sel_type_to_str : res_lit_sel_type -> string
val get_problem_files : unit -> string list


(* val options_to_str : options -> string *)  (** Deprecated *)
val options_to_stdout : options -> unit

(* inferred options: *)

val prop_solver_is_on : unit -> bool


val superposition_named_options : named_options


(** Validation *)

exception Repeated_index of string
exception Repeated_simplification of string
exception Invalid_simplification of string


(** Removes repetitions in list options: useful for auto gen options
    including signed repetions in lists +/- sign:  eg. [-eq;+ground;+eq;-ground] -> [-eq;+ground]; 
    first occurrences are kept  *)

val remove_repetitions : options -> options

(** Will raise if options are invalid *)
val validate : options -> unit



(** Makes a copy of the options so mutable modifications can be done on the copy *)
val copy_options : options -> options 

val read_opt_file : string -> named_options
(* val read_opt_files : string list -> named_options list *)

(*------------*)



(*---------------------- OLD --------------------------*)

(* Set new current options, preserving overridden option values *)
(* val set_new_current_options : options -> unit *)

(* Option values with defaults and overrides from files or command-line *)

(*
(** An option set from different sources *)
type 'a override =
  (** Option has the default value *)
  | ValueDefault of 'a

	(** Default has been overridden by file argument *)
  | ValueFile of 'a

	(** Default has been overridden by command-line argument *)
  | ValueCmd of 'a

(** Get current value of option *)
val val_of_override : 'a override -> 'a

(** Override a default value, returning a new default value, but keep
    file and command-line values *)
val override_default : 'a -> 'a override -> 'a override

(** Override a default or file value, returning a new file value,
    but keep a command-line value *)
val override_file : 'a -> 'a override -> 'a override

(** Override a default, file or command-line value, returning a new
    command-line value *)
val override_cmd : 'a -> 'a override -> 'a override
*)
