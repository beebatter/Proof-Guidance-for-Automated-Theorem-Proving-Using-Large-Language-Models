(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module is the implementation of the abstract domain, which can be
    'over' or 'under' depending of which approximation we will use. *)

open Logic_interface

type aclw = private
  { swp : term; (* switch predicate *)
    orig_cls : BCSet.t; (* Concrete clauses of an abstract clause *)
    lits : lit list; (* Without the switch pred *)
    abstr_cl : clause; (* Form: literals \/ ~switch_pred *)
  }
(** Abstract clause wrap: type of the abstract clauses in the strong abstract
    domain (over-approximation). *)

type approx = Over | Under
(** Type of the approximation. *)

type t
(** Type of the abstract domain. *)

val empty: approx -> t
(** The empty over/under abstract domain. *)

val ov_add: Options.abstr_ref_type -> clause -> lits -> lits -> t -> t
(** [ov_add x c cl al t] adds the abstraction of the clause [c] by passing its
    concrete literals [cl] and its abstract literals [al]. This function only
    works for an over-approximating abstract domain. *)

val un_add: clause -> t -> t
(** [un_add ac t] adds an abstract clause [ac] to the under-approximating
    abstract domain. *)

val gamma_fun: Term.term -> t -> BCSet.t
(** [gamma_fun id t] Concretises an over-approximating abstract clause by using
    an [id] (switch predicate) to identify the abstract clause. *)

val get_all_clauses: t -> Clause.clause list
(** [get_all_clauses t] returns a list with all the clauses in the domain. This
    includes abstract and concrete clauses. *)

val get_abstr_ids: t -> Term.term list
(** [get_abstr_ids t] returns the over-approximating ids of the abstract clauses
    in the abstract domain. *)

val get_all_symbols_by_abstr_id: Term.term -> t -> SSet.t
(** [get_all_symbols_by_abstr_id id t] returns all the symbols in the abstract
    clause corresponding to the [id]. *)

val find_by_abstr_id: Term.term -> t -> aclw option
(** [find_by_abstr_id id t] returns an over-approximating abstract clause
    corresponding to [id]. *)

val rm_by_abstr_id: Term.term -> t -> t
(** [rm_by_abstr_id id t] removes an over-approximating abstract clause
    identified by [id]. *)

val split_by_ids: TSet.t -> t -> t * t
(** [split_by_ids ids t] make two partitions of the over-approximating abstract
    domain. One of them contains all the clauses with [ids] and the other one
    the remain elements. *)

val union: t -> t -> t
(** [union t1 t2] unify two over-approximating abstract domains. *)

val concrete_domain_from_cset: BCSet.t -> t -> t
(** [concrete_domain_from_cset cset t] creates a concrete domain from a set of
    clauses. *)

val to_string: [`V0 | `V1 | `V2] -> t -> string
(** [to_string v t] returns a string representation of the abstract domain with
    a proper level of verbosity [v]. *)
