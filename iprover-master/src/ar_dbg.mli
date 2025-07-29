(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module implements the debug/logger for the abstraction-refinement
    modules. *)

type dbg_gr =
  | D_trace
  | D_SGA
  | D_AFA
  | D_GA
  | D_SBA
  | D_CSA
  | D_over
  | D_under
  | D_atp

val dbg : dbg_gr -> string lazy_t -> unit
val dbg_env : dbg_gr -> (unit -> unit) -> unit
