val dbg_flag : bool
type dbg_gr = D_trace
val dbg_gr_to_str : dbg_gr -> string
val dbg_groups : dbg_gr list
val module_name : string
val dbg : dbg_gr -> string Lazy.t -> unit
val dbg_env : dbg_gr -> (unit -> unit) -> unit
exception Parsing_fails
exception FOF_format
exception TFF_format
exception THF_format
type buffer_name = FileName of string | Clausifier of string | Stdin
val buffer_name_to_string : buffer_name -> string
val buffer_name_param_to_string : buffer_name Lib.param -> string
val position_init_lnum : Lexing.position -> Lexing.position
val buffer_name_ref : buffer_name Lib.param ref
val assign_current_buffer_name : buffer_name -> unit
val init_lexbuf : buffer_name -> Lexing.lexbuf -> unit
type includes = {
  includes_file_name : string;
  include_formula_list : string list;
  include_source_file_name : buffer_name;
}
val create_neg_conjecture :
  Clause.tstp_source -> Clause.literal_list -> Clause.clause
val parsed_clauses : Logic_interface.clause list ref
val neg_conjectures : Logic_interface.clause list ref
val has_ext_theory_names : bool ref
val unsupported_theory_fun_names : string list ref
val includes : includes list ref
val less_map : int Logic_interface.SMap.t ref
val range_map : (int * int) Logic_interface.SMap.t ref
module StrKey :
  sig type t = string val compare : Lib.String.t -> Lib.String.t -> int end
module StrMap :
  sig
    type key = StrKey.t
    type 'a t = 'a Map.Make(StrKey).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type formula_role =
    Axiom
  | Hypothesis
  | Definition
  | Assumption
  | Lemma
  | Theorem
  | Corollary
  | Conjecture
  | Negated_conjecture
  | Plain
  | FType
  | Fi_domain
  | Fi_functors
  | Fi_predicates
  | Unknown
val formula_role_to_string : formula_role -> string
exception Unknown_formula_role
val string_to_formula_role : string -> formula_role
val assign_param_input_symb : Symbol.symbol -> unit
type tff_cl_var_conext = {
  tff_fresh_vars_env : Var.fresh_vars_env;
  mutable tff_name_var_map : Var.t StrMap.t;
}
val tff_cl_vc_int : unit -> tff_cl_var_conext
val tff_cl_vc : tff_cl_var_conext ref
type squashed_types = {
  mutable type_terms_to_symb : Logic_interface.symbol Logic_interface.TMap.t;
  mutable type_symb_to_terms : Logic_interface.term Logic_interface.SMap.t;
}
val sqs_glb : squashed_types ref
val add_sq_type :
  squashed_types -> Logic_interface.TMap.key -> Logic_interface.symbol
val add_sq_type_glb : Logic_interface.TMap.key -> Logic_interface.symbol
val tff_typed_variable_fun : StrMap.key -> Var.symbol -> unit
val tff_get_vt : StrMap.key -> Var.t
val tff_reset_vt : unit -> unit
val clock_map : int list Logic_interface.SMap.t ref
val cardinality_map : int Logic_interface.SMap.t ref
val max_address_width_map : int Logic_interface.SMap.t ref
val state_constant_map : int Logic_interface.SMap.t ref
val address_base_name_map : string Logic_interface.SMap.t ref
val father_of_map : string list Logic_interface.SMap.t ref
val bit_vector_name_map : int Logic_interface.SMap.t ref
val memory_name_map : (int * int) Logic_interface.SMap.t ref
val pos_deadlock_name_set : Symbol.Set.t ref
val neg_deadlock_name_set : Symbol.Set.t ref
type bv_operations =
    BV_add
  | BV_sub
  | BV_bvugt
  | BV_bvuge
  | BV_bvult
  | BV_bvule
  | BV_bveq
  | BV_shl1
  | BV_shr1
val bv_op_to_str : bv_operations -> string
exception Not_BV_OP
val bv_name_to_op : string -> bv_operations
module BV_OP_Key :
  sig
    type t = bv_operations
    val compare : 'a -> 'a -> int
    val equal : 'a -> 'a -> bool
    val hash : 'a -> int
  end
module BV_OP_Htbl :
  sig
    type key = BV_OP_Key.t
    type 'a t = 'a Hashtbl.Make(BV_OP_Key).t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type bv_op_symb_arg_names_map = string list Logic_interface.SMap.t
val bv_op_htbl : string list Logic_interface.SMap.t BV_OP_Htbl.t
val bv_op_add_symb_htbl :
  BV_OP_Htbl.key -> Logic_interface.SMap.key -> string list -> unit
val bv_op_get_symb_map : BV_OP_Htbl.key -> string list Logic_interface.SMap.t
val distinct : Logic_interface.term list list ref
val bot_term : Term.term
val top_term : Term.term
val is_less_symb : Logic_interface.SMap.key -> bool
val is_range_symb : Logic_interface.SMap.key -> bool
val is_clock_symb : Logic_interface.SMap.key -> bool
val is_less_or_range_symb : Logic_interface.SMap.key -> bool
val default_var_type : Symbol.symbol
val max_var_ref : Var.var ref
val var_table_ref : (string, Logic_interface.var) Hashtbl.t ref
type input_problem_type = FOF | CNF | TFF | THF | SMT | AIG | QBF | UNK
val input_problem_type_to_string : input_problem_type -> string
val input_problem_type : input_problem_type option ref
val assign_input_problem_type : input_problem_type -> unit
val init : unit -> unit
val get_clauses_without_extra_axioms : unit -> Logic_interface.clause list
val create_theory_term : Term.symbol -> Term.term list -> TermDB.term
val include_file_fun : string -> string list -> unit
val comment_fun : 'a -> unit
val comment_E_prover_fun : 'a -> unit
val annotation_fun : 'a -> unit
val contains_distinct : bool ref
val analyse_distinct : Term.term list -> unit
val retype_term : Var.symbol -> Term.term -> Term.term
val retype_term_list : Var.symbol list -> Term.term list -> Term.term list
val retype_lit : Term.term -> Term.term
val retype_lits : Term.term list -> Term.term list
val is_extra_conj : Term.literal list -> bool
val is_neg_conj : string -> Term.literal list -> bool
val is_ext_axiom : string -> bool
val cnf_formula_fun : string -> string -> Clause.literal_list -> 'a -> unit
val is_false_lit : Term.literal -> bool
val disjunction_fun : Term.literal list -> Term.literal -> Term.literal list
val equality_fun : Logic_interface.term list -> Logic_interface.term
val inequality_fun : Logic_interface.term list -> TermDB.term
val neg_fun : Term.term -> TermDB.term
val plain_term_fun : string -> Symbol.stype -> Term.term list -> Term.term
val overriding_arities_warning_was_shown_ref : bool ref
val plain_term_fun_typed :
  is_top:bool -> string -> Term.term list -> Term.term
val create_type_ext_def_term :
  is_top:bool -> string -> Term.term list -> TermDB.term
val def_fun_unsupported_warning_shown : bool ref
val defined_term_fun : string -> Term.term list -> TermDB.term
val defined_pred_fun :
  string -> Logic_interface.term list -> Logic_interface.term
val defined_infix_pred_fun :
  string ->
  Logic_interface.term -> Logic_interface.term -> Logic_interface.term
val reg_exp_less : Str.regexp
val reg_exp_range : Str.regexp
val reg_exp_clock : Str.regexp
val system_pred_name_pref_reg_expr : Str.regexp
val adjust_next_args : TermDB.term list -> TermDB.term list
val system_pred_fun : string -> TermDB.term list -> TermDB.term
val system_term_fun : string -> Term.term list -> TermDB.term
val term_variable_fun : Term.var -> Term.term
val variable_fun : StrMap.key -> Var.t
val num_term : Symbol.symbol -> Symbol.sproperty -> string -> Term.term
val term_of_int_number_fun : string -> Term.term
val term_of_rat_number_fun : string * string -> Term.term
val term_of_real_number_fun : string -> Term.term
val ttf_atomic_type_fun : string -> SymbolDB.symbol
val ttf_defined_atomic_type_fun : string -> SymbolDB.symbol
val is_bound_constant_type : Symbol.symbol -> bool
val ttf_add_typed_atom_out_symb_fun :
  string -> Symbol.stype -> SymbolDB.symbol
val ttf_add_typed_atom_fun : string -> Symbol.stype -> unit
type attr_args =
    Attr_IList of int list
  | Attr_SList of string list
  | Attr_Int of int
  | Attr_Str of string
type attr_type =
    ALess of int
  | ARange of int * int
  | AFatherOf of string
  | ASonOf of string
  | AClock of int list
  | ACardinality of int
  | AStateConstant of int
  | AAddressBaseName of string
  | AAddressMaxWidth of int
  | ABitVector of int
  | AMemory of int * int
  | ABV_OP of bv_operations * string list
  | ADeadlockState of int
  | AAig of string
  | AOther of string * attr_args
type attr = Attr of attr_type * attr_args
val attr_fun : string -> attr_args -> attr_type
val find_recognised_main_attr : attr_type list -> attr_type option
val find_recognisd_bv_operation_attr : attr_type list -> attr_type option
val get_all_father_of : attr_type list -> string list
val is_defined_symbol : attr_type list -> bool
val process_deadlock_attribute : Symbol.Set.elt -> attr_type list -> unit
val process_aig_attribute : Important_lit.SSet.elt -> attr_type list -> unit
val ttf_add_typed_atom_atrr_fun :
  string -> Symbol.stype -> attr_type list -> unit
val bv_get_size : Logic_interface.SMap.key -> int
val change_conj_symb_input : unit -> unit
