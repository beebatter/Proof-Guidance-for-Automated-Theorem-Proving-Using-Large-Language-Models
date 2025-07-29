(** Given an SMT solver, make a prop solver *)
module Make (M: (module type of SMTSolver)) : (module type of PropSolver)
(* module X = SMTSolver_Z3 *)
