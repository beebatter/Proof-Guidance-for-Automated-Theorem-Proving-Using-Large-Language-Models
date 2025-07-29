open Lib
open Logic_interface

type t

val create : unit -> t

val clear : t -> unit

val add_clause : t -> clause -> unit

val remove_clause : t -> clause -> unit

(** Checks if the clause is subsumed by clauses in the index, return [Nothing]
    if there's no simplification, [Subs clause] if it is subsumed by [clause], 
    or [SubsRes (parent, lits)] if via [parent] it is simplified with 
    subsumption resolution to [lits]. Subsumption is strict, i.e. a clause 
    isn't subsumed by itself. *)
type subsumption_result = Nothing | Subs of clause | SubsRes of clause list * lits
val is_subsumed : t -> clause -> subsumption_result

(** Returns clauses in the index which this unit clause can simplify by 
    subsumption or subsumption resolution. Tuple has first the clauses 
    eliminated by subsumption, then the pairs of clauses to lit lists. *)
val subsumes : t -> lit -> clause list * (clause * lits) list
