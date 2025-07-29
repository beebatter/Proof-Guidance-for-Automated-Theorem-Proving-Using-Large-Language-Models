open Lib
open Logic_interface

type t

val create : unit -> t

val clear : t -> unit

val add_clause : t -> clause -> unit

val remove_clause : t -> clause -> unit

(** Clause is simplified by clauses in index to [Some clause'] (which can be 
    itself if no simplifications apply) or [None] if it is subsumed. *)
val is_subsumed : subs_bck_mult:int -> t -> clause -> clause option

(** Returns clauses in the index which this unit clause can simplify by 
    subsumption or subsumption resolution. Tuple has first the clauses 
    eliminated by subsumption, then the pairs of clauses to clauses 
    simplified by subsumption resolution. *)
val subsumes : subs_bck_mult:int -> t -> clause -> clause list * (clause * clause) list
