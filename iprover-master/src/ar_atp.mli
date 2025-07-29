(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module plans to contain different types of solvers that return proper
    answer based on the following types. *)

type exec = private
  | Some of exn
  | None
(** Option type for the threw exceptions *)

type uc = private
  | EmptyCl of Clause.clause
  | Assump of Logic_interface.TSet.t * Clause.clause list
  | NoAssump
(** The type of an UNSAT core. *)

type model = private
  | Inst of Instantiation_env.inst_pre_model
  | Res of Resolution_loop.res_model
  | Sup of Superposition.sup_model
(** The type of the model when the answer is SAT. *)

type answer = private
  | UNSAT of uc
  | SAT of model
  | Unknown of exec
(** The type of possible answers given by the solver. *)

val complete_and_sound: Options.options -> float Lib.param ->
  Clause.clause list -> answer * float Lib.param
(** [comple_and_sound opts tl cls] is a wrap of Proof_search_loop.ps_full_loop,
    which receives the options [opts], a time limit [tl] and a list of clauses
    [cls].It catches some of the known exceptions for SAT and UNSAT answers and
    convert them into a proper answer of the above type. *)

val check_answer: answer * 'a -> 'b
(** Raise a proper exception depending on the given answer, which will be
    handled by iProver. *)
