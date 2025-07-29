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

type var    = Var.var
type symbol = Symbol.symbol
type args 
type fun_info   
type var_info

type prop_key = TableArray.key

(** Association list for counting the number of occurences of vars in a term *)
type var_ass_list = var num_ass_list

type term =
  | Fun of symbol * args * fun_info
  | Var of var * var_info 

and grounding = {
  (** ID of grounding sustitution that was used to obtain grounded_term *)
  mutable grounding_subst_id : int; 

  (** Grounding can change e.g. in finite models or bmc1 *)
  mutable grounded_term : term; (* term obtained after grounding *) 
}

(*
(* when cluase is ground we only store prop_key and when it is non_ground we store current grounded term with the corresponding subst_id *)
and grounding_info =  Grounding_non_gr of grounding_non_gr | Grounding_gr_key of prop_key 
*)

exception Grounding_undef

val assign_grounding : grounding -> term -> unit

(*
type grounding = 
  {
   (* id of grounding sustitution that was used to obtain grounded_term *)
   mutable grounding_subst_id : int; 

(* grounding can change e.g. in finite models or bmc1 *)
   mutable grounded_term : term; (* term obtained after grounding *) 
   (* if the original term was already ground then ground_term reference the term itself *)

   mutable prop_gr_key : prop_key; (* prop_gr_key = prop_gr_key of grounded_term *)

  (* prop. key of term (without grounding) used for simplifiactions *)
(*   mutable prop_key : prop_key param; *)

 }
*)

type atom = term 
type lit = term 
type literal = lit 

type bound_term = term Lib.bind

(* exception Term_fast_key_undef *)

val create_var_term       : var -> term
val create_var_term_info  : var -> var_info -> term

val create_fun_term       : symbol -> term list -> term
val create_fun_term_args  : symbol -> args -> term
val create_fun_term_info  : symbol -> term list -> fun_info -> term

(** From [atom] create [Symbol.symb_neg(atom)]. *)
val create_neg_lit : term -> term

val get_num_of_symb         : term -> int
(** As [get_num_of_symb], but doesn't count negation. E.g. [get_num_of_symb 
    a != f(b)] is 6, [get_num_of_symb_skipneg] is 5. *)
val get_num_of_symb_skipneg : term -> int
val get_num_of_var        : term -> int

(* assume that term is a Var term*)
val get_var               : term -> var


(* association list of (vars, num_of_occ) *)
val get_var_ass_list          : term -> var_ass_list  

(* adds all vars of the term to the set *)
val add_var_set : Var.VSet.t -> term -> Var.VSet.t

(** Return set of all variables in term *)
val get_var_set : term -> Var.VSet.t

(** Return the list of variables occurring in the term *)
val get_vars : term -> var list 

(**  set of vars of t are subset of vars of s *)
val var_subset : term -> term -> bool



val get_var_info : term -> var_info
val get_fun_info : term -> fun_info

(* bool params *)
type fun_term_bool_param 
type var_term_bool_param 

val has_conj_symb       : fun_term_bool_param
val has_bound_constant  : fun_term_bool_param (* used for BMC *)
val fun_in_term_db      : fun_term_bool_param
val var_in_term_db      : var_term_bool_param


val is_next_state_lit   : term -> bool
val is_reachable_state_lit   : term -> bool

(* val orient_eq_lit : (term -> term -> PartialOrd.t) -> term -> term *)
(* val orient_if_eq_lit : (term -> term -> PartialOrd.t) -> term -> term *)
(* val assign_oriented : (term -> term -> PartialOrd.t) -> term -> unit *)
val assign_has_conj_symb      : term -> unit

val has_non_prolific_conj_symb : fun_term_bool_param
val assign_has_non_prolific_conj_symb :  term -> unit

(* val is_oriented_eq_lit : term -> bool *)

(* val get_oriented_lr : term -> bool
val get_oriented_rl : term -> bool
val get_oriented    : term -> int

val set_oriented_lr : term -> bool -> unit
val set_oriented_rl : term -> bool -> unit
val set_oriented    : term -> int -> unit *)

(*------ fun bool params --------*)
val set_fun_bool_param : bool ->  fun_term_bool_param -> term -> unit
val get_fun_bool_param : fun_term_bool_param -> term -> bool 

(* a bit faster *)
val set_fun_info_bool_param : bool ->  fun_term_bool_param -> fun_info -> unit
val get_fun_info_bool_param : fun_term_bool_param -> fun_info -> bool 


(*------ var bool params --------*)
val set_var_bool_param : bool ->  var_term_bool_param -> term -> unit
val get_var_bool_param : var_term_bool_param -> term -> bool 

(* a bit faster *)
val set_var_info_bool_param : bool ->  var_term_bool_param -> var_info -> unit
val get_var_info_bool_param : var_term_bool_param -> var_info -> bool 


(* prop_gr_key for the table prop_var-term *)



(* prop. key of term (without grounding) used for simplifiactions *)

exception Prop_key_undef
val get_prop_key          : term -> prop_key

val assign_prop_key       : prop_key -> term -> unit 



val get_orderings_cache_info : int -> fun_info -> PartialOrd.t
val set_orderings_cache_info : int -> PartialOrd.t -> fun_info -> unit

val get_ac_subterms_info : fun_info -> term list option
val set_ac_subterms_info : term list -> fun_info -> unit

(** Arguments: ordering id and AC operator set *)
val get_is_ac_normalised_info : fun_info -> int -> _ Symbol.Map.t -> bool
val set_is_ac_normalised_info : fun_info -> int -> _ Symbol.Map.t -> unit



(* prop. key of term after grounding *)
(*exception Prop_gr_key_undef*)
val get_prop_gr_key          : term -> prop_key

(*val assign_prop_gr_key       : prop_key -> term -> unit *)
(* *)


(** Complementary literal. Always use [Logic_interface.add_compl_lit] instead. *)
val compl_lit   : literal -> literal
(** Checks if literal is positive *)
val is_pos_lit  : literal -> bool 
(** Checks if literal is negative *)
val is_neg_lit  : literal -> bool
(** As [is_pos_lit] *)
val get_sign_lit : literal -> bool
(** Decompose a literal into sign and atom *)
val split_sign_lit : literal -> bool * term

(* *)
exception Type_check_failed
val type_check : ?allow_sub_types: bool-> term -> unit

(* as type_check but returns bool instead of raising Type_check_failed *)
val is_well_typed_term : ?allow_sub_types: bool ->  term -> bool 

(* checkes compatibility of value types: for now Symbol.symb_type_types and Symbol.symb_bot_type can unify with any other types *)

val compatible_val_types : term -> term -> bool

val is_complementary : term -> term -> bool

(* apply only to literals, returns if a literal is in EPR: all arguments are eiter constants or vars*)
val is_epr_lit  : term -> bool

(* can raise exception Grounding_undef *)
val get_grounding : term -> grounding


val get_grounded_term         : term -> term

(* only atoms get assigned groundings *)
val get_grounding_lit: term -> term

val get_args : term -> args

(** Convenience functions to unpack arguments from a function known to have 
    a certain number of arguments (fails otherwise). *)
val get_1_args : args -> term
val get_2_args : args -> term * term
val get_3_args : args -> term * term * term

(* use get_prop_key ! *)
(* propositional id of grounding of the literal *)
(* can raise Term_ground_undef*)
(* val get_propos_gr_id : term -> int *)

(*folds f on all symbols in the term*)
val fold_sym : ('a -> symbol -> 'a) -> 'a -> term -> 'a 

val iter_sym : (symbol-> unit) -> term -> unit

(* folds f to over all subterms inlcuding the term intself *)
val fold_subterms : ('a -> term -> 'a ) -> 'a -> term -> 'a 

(* f: first arg  is depth and second is sym, f is applied from top to bottom*)
(* depth starts with 1, (0 if symbol does not occur) *)
val iter_sym_depth : (int -> symbol -> unit) -> term -> unit 

(* assign_fast_key is done when building termDB *)
 
val assign_fast_key   : term -> int -> unit

(*
val assign_db_id   : term -> int -> unit
*)

(* only to be used in termDB*)
val assign_num_of_symb : term -> unit
(* first arg is a ground term assigned to the second arg *)
(* val assign_grounding  : term  -> term -> unit *)

(* val assign_var_list   : term -> unit *)
val assign_fun_all        : term -> unit
val assign_var_all        : term -> unit

(*
exception Term_hash_undef
(* assume that for all subterms hash has been assigned before*)
(*val assign_hash           : term -> unit*)

(* assigns hash to term without assumptions and returns this hash *)
val assign_hash_full      : term -> int
val get_hash              : term -> int
*)

val arg_map          : (term -> term) -> args -> args	
val arg_map_list     : (term -> 'a) -> args -> 'a list
val arg_to_list      : args -> term list
val list_to_arg      : term list -> args

val is_empty_args       : args -> bool
(* explicitly maps from left to right, 
   since order can matter when use imperative features *)
val arg_map_left     : (term -> term) -> args -> args 
(** Same, but like [List.mapi] the function also takes the index as an argument *)
val arg_mapi_left     : (int -> term -> term) -> args -> args	


val arg_fold_left    : ('a -> term -> 'a)-> 'a -> args -> 'a
val arg_fold_right   : (term -> 'a -> 'a)-> args -> 'a -> 'a 
val arg_fold_left2   : 
    ('a ->  term -> term -> 'a) -> 'a -> args -> args -> 'a
val arg_for_all2 : (term -> term -> bool) -> args -> args -> bool

val arg_iter  : (term -> unit) -> args -> unit 
val arg_iter2 : (term -> term -> unit) -> args -> args -> unit

(* folds over all subterems of the term including term itself *)
(*val fold_left : ('a -> term -> 'a) -> 'a -> term -> 'a *)
val fold_left_var : ('a -> var -> 'a) -> 'a -> term -> 'a

(* creates a new term by applying f to all its subterms bottom up including term itsef *)
(*val map  : (term -> term) -> term -> term *)
val map_var  : (var -> term) -> term -> term


(* iterates f over term bottom up *)
val iter : (term -> unit) -> term -> unit

(* check whether there extists a subterm (including term itself) satisfying f *)
val exists : (term -> bool) -> term -> bool 


(*  is_subterm s t = true if  s is a subterm of t using == *)
val is_subterm : term -> term -> bool 

val is_constant : term -> bool
val is_const_term : term -> bool
val is_ground   : term -> bool
val is_var      : term -> bool 
val var_in      : var -> term -> bool

(* compare = compare_fast_key and should not be used before 
   fast_key is assigned i.e. termDB is build; 
   before that use compare_key *)  

val compare               : term -> term -> int 

(* compare_key impl. as structural equality used for termDB*)
(* variables and variables as terms Var(v,i) can have different fast keys*)
val compare_key           : term -> term -> int
val compare_key_modulo_var: term -> term -> int
val compare_fast_key      : term -> term -> int

(** Compare two sides of an equation to consistently orient. Not perfect (but 
    globally consistent). *)
val compare_eq_orient : term -> term -> int

val equal_bterm          : term bind -> term bind -> bool
val compare_bterm        : term bind -> term bind -> int

val is_neg_lit            : literal -> bool
val get_atom              : literal -> term
val is_eq_lit             : literal -> bool
val is_eq_atom            : term    -> bool

(* decompose_eq_atom atom then if atom =  "t = s" then returns [t;s], returns [] if atom is not equality *) (* andrepd: actually returns [etype;t;s] *)
val decompose_eq_atom     : term -> term list  (* andrepd: return tuple instead, raise or None if not equality *)
val flip_eq_lit : term -> term

val is_clock_lit : literal -> bool
val is_less_lit : literal -> bool
val is_range_lit : literal -> bool


exception Var_term
val get_top_symb          : term -> symbol
val lit_get_top_symb      : term -> symbol

val get_term_type : term -> symbol

(* compare two terms *)
val cmp_ground   : term -> term -> int 
val cmp_num_symb : term -> term -> int 
val cmp_num_var  : term -> term -> int 
val cmp_sign     : term -> term -> int 
val cmp_definition    : term -> term -> int 
val cmp_top      : term -> term -> int 

val is_definition_lit : term -> bool

val lit_cmp_type_to_fun : Options.lit_cmp_type -> (literal -> literal -> int)
val lit_cmp_type_list_to_lex_fun :  
    Options.lit_cmp_type list -> (literal -> literal -> int) 

(*-----------*) 
val lit_bool_prop_to_bool : Options.lit_bool_prop_type -> literal -> bool
val compose_bool_prop_opt_list : (bool->bool->bool) -> bool -> Options.lit_bool_prop_type list -> literal -> bool


(* replace all occurrences of subterm by byterm in t *)
(* we assume that t, p, and q are in the term db and therefore we are using == *)
(* the resulting term will need to be  added in term db separately *)
(* replace : subterterm -> byterm:term -> term -> term *)
val replace : term -> term -> term -> term


val to_stream           : 'a string_stream -> term -> unit
val to_stream_tptp           : 'a string_stream -> term -> unit
val out                 : term -> unit

val pp_term : Format.formatter -> term -> unit
val pp_term_tptp : Format.formatter -> term -> unit

val term_list_to_stream : 'a string_stream -> (term list) -> unit
val out_term_list       : (term list) -> unit

(* better use to_stream *)
val to_string : term -> string
val term_list_to_string : (term list) -> string


(* applies function to atom i.e. if ~p then ~f(p); if p then f(p)  *)
(* f should not return negative literals *)
(* adding to term_db should be done separately  *)
val apply_to_atom : (term -> term) -> term -> term 

val get_fast_key : term -> int

val hash : term -> int

(*---------*)

val is_skolem_const : term -> bool 
val is_skolem_lit   : term -> bool
val is_addr_const   : term -> bool 


(*---------------------------------*)

(* Map/Set for lits of terms *)
module TMapList : Map.S with type key = term list
module TSetList : Set.S with type elt = term list 


(*---------------------------------*)
module TKey :
  sig
    type t = term
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end


module TMap : Map.S with type key = term
module TSet : Set.S with type elt = term
module THashtbl : Hashtbl.S with type key = term

type term_set = TSet.t
(*---------------------------------*)

(* the same for bound vars *)
module BTKey :
  sig
    type t = bound_term
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

module BTMap : Map.S with type key = bound_term
module BTSet : Set.S with type elt = bound_term
module BTHashtbl : Hashtbl.S with type key = bound_term



val term_list_to_set : term list -> TSet.t

(* terms are assumed in termDB *)
val remove_duplicate_terms : term list -> term list

(* replaces subterms based on map; the result needs to be added to term_db  *)
val replace_map : term TMap.t -> term -> term



(** # Working with positions *)

(** Represents a position of a subterm in a term *)
type position = int list

(** Returns the subterm at a given position *)
val at_position : term -> position -> term

(** Similar in spirit to [List.iteri], but the function takes [position element] as arguments instead of [index element]. *)
val iter_position : (position -> term -> unit) -> term -> unit

(** Similar in spirit to [List.mapi], but the function takes [position element] as arguments instead of [index element]. *)
(* val map_position : (position -> term -> 'b) -> term -> 'b list *)



(** Iterates through subterms, in preorder *)
val iter_preorder  : (term -> unit) -> term -> unit
(** As [iter_preorder], but skips variables *)
val iter_preorder_novar  : (term -> unit) -> term -> unit

(** Iterates through subterms, in postorder *)
val iter_postorder : (term -> unit) -> term -> unit
(** As [iter_postorder], but skips variables *)
val iter_postorder_novar : (term -> unit) -> term -> unit

(** Iterates through _proper_ subterms, in preorder. *)
val iter_subterms_preorder  : (term -> unit) -> term -> unit
(** As [iter_subterms_preorder], but skips variables. *)
val iter_subterms_preorder_novar  : (term -> unit) -> term -> unit

(** Iterates in breadth-first manner through subterms. *)
val iter_bfs : (term -> unit) -> term -> unit
(** Iterates in breadth-first manner through _proprt_ subterms. *)
val iter_subterms_bfs : (term -> unit) -> term -> unit



module Eq : sig
  (* val sign : literal -> bool *)

  (** True if positive or negative equality. *)
  val is_eq : literal -> bool

  (** True if positive equality. *)
  val is_pos_eq : literal -> bool

  (** True if negative equality. *)
  val is_neg_eq : literal -> bool

  (** Given a literal, if it's an equation l=r returns [Some (l,r)]. If 
      it's not an equation (or is negative) returns [None]. *)
  val decompose_atom : literal -> (term*term) option

  (** Given a literal, if it's an equation l=r or l!=r, returns [(sign, 
      Some (l,r))]. If it's not an equality/disequality, returns [(sign, 
      None)]. *)
  val decompose_lit : literal -> (bool * term*term) option

  (** As [decompose_atom], but returns [Some (eq_type,l,r)] *)
  val decompose_atom_type : literal -> (term * term*term) option

  val decompose_lit_type : literal -> (bool * term * term*term) option

  (** Versions which assume the atom/literal is an equality, and raise 
      [Invalid_argument] otherwise. *)
  val decompose_atom_exn      : literal -> (term*term)
  val decompose_lit_exn       : literal -> (bool * term*term)
  val decompose_atom_type_exn : literal -> (term * term*term)
  val decompose_lit_type_exn  : literal -> (bool * term * term*term)

  (** Returns left- or right-hand side of equality.
      @raises Invalid_argument if not an equality literal *)
  val left  : literal -> term
  val right : literal -> term 

  (** Returns equality type. 
      @raises Invalid_argument if not an equality literal *)
  val eq_type : literal -> term 

  (** Returns greatest/leastest side in an oriented literal.
      Precondition: the literal is oriented, one way or another *)
  (* val oriented_big : literal -> term *)
  (* val oriented_small : literal -> term  *)

  (** [regularize_pos 1 l r] = [l,r] 
      [regularize_pos 2 l r] = [r,l] *)
  val regularize_pos : int -> term -> term -> (term*term)

  (** True if literal is of the form P(x1,...) {=,!=} top_term *)
  val is_predicate_eq : literal -> bool
  (** True if literal is of the form P(x1,...) = top_term *)
  val is_pos_predicate_eq : literal -> bool
  (** True if literal is of the form P(x1,...) != top_term *)
  val is_neg_predicate_eq : literal -> bool

  (** True if literal is an equality *but* not of the form P(x1,...) {=,!=} top_term *)
  val is_nonpredicate_eq : literal -> bool
  (** True if literal is an equality *but* not of the form P(x1,...) = top_term *)
  val is_pos_nonpredicate_eq : literal -> bool
  (** True if literal is an equality *but* not of the form P(x1,...) != top_term *)
  val is_neg_nonpredicate_eq : literal -> bool



  (** True if literal is l=r with l≻r *)
  (* val is_oriented : literal -> bool *)

  (* type is_oriented = LeftGreater | RightGreater | NotOriented *)

  (** [LeftGreater] if l≻r, [RightGreater] if r≻l, or [NotOriented] otherwise *)
  (* val is_oriented : literal -> is_oriented *)

  (** [true] if l≻r or r≻l, [false] otherwise *)
  (* val is_any_oriented : literal -> bool *)



  (** Return the result of Given an ordering on terms *)
  (** Cache the result of applying an ordering on the lhs and rhs of an equation *)
  val oriented : (term -> term -> PartialOrd.t) -> int -> (lit -> PartialOrd.t)

  (** As [oriented'], but when the lhs and rhs are equal returns GT instead. Thus 
      this returning GT means lhs >= rhs, this returning LT means lhs < rhs, and 
      this returning INC means lhs ? rhs. This is what you want in practice. *)
  (* val oriented : (term -> term -> PartialOrd.t) -> int -> PartialOrd.t *)

  (* As [match oriented ... with GT | LT -> true | EQ | INC -> false] *)
  val oriented_any : (term -> term -> PartialOrd.t) -> int -> (lit -> bool)
end
