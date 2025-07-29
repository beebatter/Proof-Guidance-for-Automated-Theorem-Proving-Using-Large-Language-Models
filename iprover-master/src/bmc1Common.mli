val dbg_flag : bool
type dbg_gr = D_model_selected | D_model_rejected
val dbg_gr_to_str : dbg_gr -> string
val dbg_groups : dbg_gr list
val module_name : string
val dbg : dbg_gr -> string Lazy.t -> unit
val dbg_env : dbg_gr -> (unit -> unit) -> unit
type k_induction_stage = IndBase | IndStep
val k_induction_stage_flip : k_induction_stage -> k_induction_stage
val stage_to_string : k_induction_stage -> string
type mc_phase = {
  mutable mc_cur_bound : int;
  mutable mc_alg_stage : k_induction_stage;
  mutable mc_deadlock_stay_base : bool;
}
val phase_to_string : mc_phase -> string
val get_initial_phase : unit -> mc_phase
type bmc_handlers = {
  mc_task_name : string;
  mc_update_phase : mc_phase -> unit;
  mc_after_sat : mc_phase -> unit;
  mc_after_unsat : mc_phase -> unit;
  mc_get_next_bound_axioms :
    mc_phase -> Logic_interface.clause list * Logic_interface.term list;
}
val print_clauses : string -> Clause.clause list -> unit
val current_task_name : opts:Options.options -> string
val need_init_predicate : opts:Options.options -> bool
val need_property_from_predicate : opts:Options.options -> bool
val need_property_predicate : opts:Options.options -> bool
val need_equal_to_predicate : opts:Options.options -> bool
val need_equal_from_predicate : opts:Options.options -> bool
val need_deadlock_predicate : opts:Options.options -> bool
val address_type : Symbol.symbol
val state_type : Symbol.symbol
val bitindex_type : Symbol.symbol
val address_diff_symbol_name : string
val address_val_symbol_name : string
val address_symbol_name : string
val base_guard_symbol : string
val full_rel_guard_symbol : string
val bound_symbol_format :
  (int -> string, unit, string, string, string, string) format6
val bitindex_symbol_format :
  (int -> string, unit, string, string, string, string) format6
val state_symbol_format :
  (int -> string, unit, string, string, string, string) format6
val var_n : Var.symbol -> int -> Var.var
val term_xn : Var.symbol -> int -> Logic_interface.term
val term_x0 : Var.symbol -> Logic_interface.term
val term_x1 : Var.symbol -> Logic_interface.term
val term_x2 : Var.symbol -> Logic_interface.term
val create_typed_equation :
  Logic_interface.symbol ->
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val create_constant_term : string -> Symbol.symbol -> Logic_interface.term
val create_skolem_term :
  ('a -> string, unit, string, string, string, string) format6 ->
  Symbol.symbol -> 'a -> Logic_interface.term
val create_atom :
  ?is_special:bool ->
  string ->
  Symbol.symbol list -> Logic_interface.term list -> Logic_interface.term
val create_atom_symb :
  Logic_interface.symbol -> Logic_interface.term list -> Logic_interface.term
val get_arg_types_list : Symbol.symbol -> Symbol.symbol list
val get_arg_var_list_of_types : Var.symbol list -> Logic_interface.term list
val get_arg_var_list : Symbol.symbol -> Logic_interface.term list
val cone_exclude_symbs : Logic_interface.SSet.t ref
val add_cone_ex_symb : Logic_interface.SSet.elt -> unit
val create_state_term : int -> Logic_interface.term
val create_bound_atom : int -> Logic_interface.term
val create_bound_atom_list :
  Logic_interface.term list -> int -> int -> Logic_interface.term list
val create_step_guard : unit -> Logic_interface.term
val create_base_guard : unit -> Logic_interface.term
val create_base_assumption : unit -> Logic_interface.term
val create_step_assumption : unit -> Logic_interface.term
val create_full_r_assumption : unit -> Logic_interface.term
val create_short_r_assumption : unit -> Logic_interface.term
val create_short_r_guard : unit -> Logic_interface.term
val create_full_r_guard : unit -> Logic_interface.term
val create_bitindex_term : int -> Logic_interface.term
val create_bitvector_atom :
  string -> Logic_interface.term -> Logic_interface.term
val next_state_consts_ref : Logic_interface.TSet.t ref
val get_next_state_consts : unit -> Logic_interface.TSet.t
val create_next_atom :
  Logic_interface.term ->
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val rel_index_const : opts:Options.options -> Term.term
val rel_index_var : unit -> Logic_interface.term
val create_auto_path_atom :
  opts:Options.options ->
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val create_init_atom :
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val create_auto_init_atom :
  opts:Options.options -> Logic_interface.term -> Logic_interface.term
val create_property_atom : Logic_interface.term -> Logic_interface.term
val create_equal_state_atom :
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val create_eligible_atom : Logic_interface.term -> Logic_interface.term
val create_deadlock_atom : Logic_interface.term -> Logic_interface.term
val create_address_diff_atom :
  Logic_interface.term ->
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val create_address_val_atom :
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val create_address_atom : Logic_interface.term -> Logic_interface.term
val create_path_atom : int -> Logic_interface.term
val create_path_axiom : opts:Options.options -> int -> Logic_interface.clause
val create_property_axiom : int -> Logic_interface.clause
val create_neg_property_axiom : int -> Logic_interface.clause
val create_init_atom_b : int -> Logic_interface.term
val create_guarded_init_axiom :
  opts:Options.options -> int -> Logic_interface.clause
val create_neg_init_axiom : int -> Logic_interface.clause
val create_non_equal_state_axiom : int -> int -> Logic_interface.clause
val get_model_lits :
  Instantiation_env.inst_pre_model_entry Logic_interface.BCMap.t ->
  use_clause:(Logic_interface.BCMap.key -> bool) ->
  apply_grounding:bool -> Logic_interface.TSet.t
