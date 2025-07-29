open Lib
open Logic_interface

module SMT = SMTSolver



(** A set of consistent ground clauses. *)
type set

(** Initialize a set wrt. a SMT solver, and optionally give some initial clauses. *)
val make_set : ?initial_clauses:clause list -> SMT.state -> set

(** Add (the grounding of) a clause to the set. It will also check inconsistency and then return true if it is unsat. *)
val add : set -> clause -> bool

(** As [add] but will *not* check inconsistency. *)
val add_no_check : set -> clause -> unit

(** Perform global subsumption of a clause wrt. the clauses in the set. *)
val global_smt_subsumption : set -> clause -> clause
