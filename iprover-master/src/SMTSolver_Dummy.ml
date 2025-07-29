open Lib
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace


let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
  D_trace;
]
    
let module_name = "SMTSolver_Dummy"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)





type state = unit

type term = unit

type clause = unit

module Ip = Logic_interface



type options = {
  interpreted_arithmetic: bool;
}

let make_state _ = 
  ()



let lit_to_smt _ _ = ()

let clause_to_smt _ _ = ()



let clause_to_smt_ground _ _ = ()



let lit_to_smt_prop _ _ = ()

let clause_to_smt_prop _ _ = ()

let int_to_smt_prop _ _ _ = ()



let lit_to_smt_quant _ _ = ()

let clause_to_smt_quant _ _ = ()



let term_of_smt _ _ = undefined ()

let lit_of_smt _ _ = undefined ()

let clause_of_smt _ _ = undefined ()



let term_to_unit_clause _ _ = ()

let term_list_to_clause _ _ = ()

let tagged_formula _ _ _ = ()



type problem = unit

let make_problem _ = ()





let add _ _ = ()

let add_many _ _ = ()

let push _ = ()

let pop _ = ()

let clear _ = ()



type result = Sat | Unsat | Unknown
(* INVARIANT: Cannot have fields (`of stuff`) *)

let check _ = Unknown

let check_assumptions _ _ = Unknown



type model = unit

let model _ = None



module Uc = struct
  type problem = unit

  type tag = unit

  let tag_equal = (==)

  let make_problem _ = ()

  let next_numbering _ = ()

  let add _ _ = ()

  let add_many _ _ = []

  let check _ = Unknown

  let check_assumptions _ _ = Unknown

  let push _ = ()

  let pop _ = ()

  let clear _ = ()

  let unsat_core _ = []
end



let make_problem_fast _ = ()



let eval_term _ _ = ()

let eval_term_bool _ _ = None

let eval_clause _ _ = ()

let eval_clause_bool _ _ = None



let simplify_term (* _ *) _ = ()

let simplify_clause (* _ *) _ = ()





module Term = struct
  type t = term

  let equal a b = true

  let compare a b = 0

  let hash x = 0
end

module Clause = struct
  type t = clause

  let equal a b = true

  let compare a b = 0

  let hash x = 0
end
