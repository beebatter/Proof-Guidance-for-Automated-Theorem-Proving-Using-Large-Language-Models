(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module implements the under approximation procedure of the
    abstraction-refinement framework. *)

val solve: Options.options -> (float Lib.param) ->
  Logic_interface.clause list -> unit
(** [solve o tl cl] uses the under approximation procedure to try determining the
    satisfiability of a set of clauses [cl] in a given time limit [tl]. It uses
    the under-approximating abstractions in [o.abstr_ref_under]. *)

val get_solver:
  (Options.options -> float Lib.param -> Clause.clause list ->
   Ar_atp.answer * float Lib.param) ->
  Options.abstr_ref_under_list_type -> Options.options -> float Lib.param ->
  Clause.clause list -> Ar_atp.answer * float Lib.param
(** [get_solver atp  arl] returns a solver, which apply the list of abstractions
    [arl] in the under-approximating process and uses the passed ATP [atp] as
    a basic solver. Thus, we can pass an instance of the over-approximation
    procedure as an ATP. The signature of the returned solver is:
      Options.options -> float Lib.param -> Clause.clause list ->
      Ar_atp.answer * float Lib.param *)
