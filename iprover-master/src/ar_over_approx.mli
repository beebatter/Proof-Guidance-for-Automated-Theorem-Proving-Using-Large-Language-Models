(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module implements the over approximation procedure of the
    abstraction-refinement framework. *)

val solve: Options.options -> float Lib.param ->
  Logic_interface.clause list -> unit
(** [solve o tl cl] uses the over approximation procedure to try determining the
    satisfiability of a set of clauses [cl] in a given time limit [tl]. It uses
    the over-approximating abstractions in [o.abstr_ref]. *)

val get_solver: Options.abstr_ref_list_type -> Options.options ->
  float Lib.param -> Clause.clause list -> Ar_atp.answer * float Lib.param
(** [get_solver arl] returns a solver, which apply the list of abstractions
    [arl] in the over-approximating process. The signature of the returned
    solver is: Options.options -> float Lib.param -> Clause.clause list ->
               Ar_atp.answer * float Lib.param *)
