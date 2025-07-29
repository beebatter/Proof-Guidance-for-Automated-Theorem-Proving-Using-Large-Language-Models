open Lib

let options : SMTSolver.options = {
  interpreted_arithmetic = true;
}

let initial_state = SMTSolver.make_state options

let state = ref initial_state
