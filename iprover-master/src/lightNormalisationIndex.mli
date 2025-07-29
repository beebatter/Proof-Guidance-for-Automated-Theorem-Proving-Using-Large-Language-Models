open Lib
open Logic_interface

type t

(** Create an empty TRS. *)
val create : order:ordering -> eq_types:Symbol.sym_set -> unit -> t

(** Semantically equivalent to assigning [create()]. *)
val clear : t -> unit

(** Prints all the rules in the TRS, for debug purposes. *)
val to_string : t -> string



type completeness = L of term | R of term | LR of term * term | None

(** Get value of term under direct application of the TRS. *)
(* val get : t -> term -> term *)

(** Get normal form of the term under the TRS, recursively normalising 
    subterms. Returns the term and the set of parent clauses. *)
val normalise : t -> no_check:(completeness Lazy.t) -> term -> term * BCSet.t

(** Simply retrieve normal form, without recursing, without any ordering check *)
val normalise_term : t -> term -> term * BCSet.t



(** Add oriented unit equality to the TRS, without performing any other 
    operations. *)
val add_bare : t -> clause -> unit

(** Given an oriented unit equality, forward normalise it, then use it to 
    backward normalise the equalities in the TRS, then add it to the trs. If 
    there is a conflict (adding l=r when there is already a l=r'), choose 
    the one with the smaller right-hand side. 
    Returns tuple of old and new rules. *)
val add_and_update : t -> clause -> unit

(** Remove oriented unit equality from the TRS. *)
val remove : t -> clause -> unit
