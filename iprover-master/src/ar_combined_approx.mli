(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module implements the combined approximation procedure of the
    abstraction-refinement framework. *)

val solve: Options.options -> (float Lib.param) ->
  Logic_interface.clause list -> unit
(** [solve o tl cl] uses the combined approximation procedure to try determining
    the satisfiability of a set of clauses [cl] in a given time limit [tl]. It
    uses the over-approximating abstractions in [o.abstr_ref] and the
    under-approximating abstractions in [o.abstr_ref_under]. *)
