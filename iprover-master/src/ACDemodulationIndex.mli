open Lib
open Logic_interface



type t

val create : 
  ac_symbols:AC.Table.t -> 
  order:ordering -> 
  subs_bck_mult:int -> 
  ?complete:bool -> 
  unit -> t

val clear : t -> unit

(* SMap.t is the set of AC symbols to be indexed *)
val add_equation :
  t -> clause -> unit

val add_bwd_clause :
  t -> clause -> unit

val elim_equation :
  t -> clause -> unit

val elim_bwd_clause :
  t -> clause -> unit

(** Returns Some (lhs, rhs, subst, parent), if it exists, such that: 
      lhs subst ⊂ term, 
      parent is lhs=rhs. *)
val get_fwd : 
  t -> term -> (term * term * subst * clause) option

(** Returns list of (t, subst, clauses), such that: 
      term subst ⊂ t, 
      t occurs in clauses,
    and each clause comes with [None] if at a subterm or smaller side, or 
    [Some r] if at top on maximal side with other side [r]. *)
val get_bwd : 
  t -> term -> (term * subst * (clause * term option) list) list

(** Returns either Some (term', parent) or None. *)
val get_fwd_special : 
  t -> term -> (term * clause) option

val is_special_axiom : clause -> bool