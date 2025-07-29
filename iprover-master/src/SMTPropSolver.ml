open Lib



(*----- debug modifiable part-----*)

let dbg_flag = true

type dbg_gr = 
  | D_trace


let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
   D_trace;
]
    
let module_name = "SMTPropSolver"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)



(* module Make (M: (module type of SMTSolver_Z3)) : (module type of PropSolver) = struct *)
module Make (M: (module type of SMTSolver)) : (module type of PropSolver) = struct

type solver = {
  uid: int;
  state: M.state;
  problem: M.problem;
  mutable clause_unique_id: int;
  mutable model: M.model option;  (* Compute+memoise model lazily *)

  mutable clauses_with_id: int;
}

type solver_uc = solver

(*type var*)

(*
type solver_name  = MiniSat | ZChaff
val current_solver :  solver_name
*)

type lit = M.term

type lit_uc = M.term

type var_id = int

type solver_out = Sat | Unsat
type fast_solve = FSat | FUnsat | FUnknown

type lit_val  = True | False | Any
type lit_sign = Pos  | Neg


let sign_to_bool = function
  | Pos -> true
  | Neg -> false

let bool_to_sign = function
  | true  -> Pos
  | false -> Neg

let smt_result_to_solver_out = function
  | M.Sat -> Sat
  | M.Unsat -> Unsat
  | M.Unknown -> failwith "SMT solver returned unknown on propositional problem"

let smt_result_to_fast_solver_out = function
  | M.Sat -> FSat
  | M.Unsat -> FUnsat
  | M.Unknown -> FUnknown

let solver_out_to_smt_result = function
  | Sat -> M.Sat
  | Unsat -> M.Unsat



exception Create_lit_error

(* if true creates a simplifiaction solver and if false creates an incemental solver *)

(* let () = Random.self_init() *)

let solver_id_cnt = ref 0 

let get_new_id () = 
  let new_id = solver_id_cnt in
  solver_id_cnt := solver_id_cnt + 1


let create_solver _ = 
  let state = M.make_state () in
  let problem = M.make_problem state in
  { state; problem; clause_unique_id=1; model=None; 
    clauses_with_id=0;
    uid = get_new_id ();
  }

let reset_solver {state; problem} =
  M.clear problem

let delete_solver _ = ()

let create_solver_uc = create_solver

let reset_solver_uc = reset_solver

let delete_solver_uc = delete_solver

let num_of_solver_calls      _ = 0
let num_of_fast_solver_calls _ = 0
let num_of_vars              _ = 0
let num_of_clauses           _ = 0

let is_simplification _ = false



let add_var_solver solver n = 
  ()

let create_lit solver sign var_id =
  dbg D_trace @@ lazy (sprintf "create_lit %d     (on %d)" var_id solver.uid);
  M.int_to_smt_prop solver.state (sign_to_bool sign) var_id

(* let create_lit_uc = create_lit *)
let create_lit_uc solver sign var_id = 
  dbg D_trace @@ lazy (sprintf "create_lit_uc %d  (on %d)" var_id solver.uid);
  M.int_to_smt_prop solver.state (sign_to_bool sign) var_id

let lit_sign solver lit = 
  (* failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"] *)
  let lit : Z3.Expr.expr = Obj.magic lit in
  not @@ Z3.Boolean.is_not lit

let lit_var solver lit = 
  (* failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"] *)
  let lit : Z3.Expr.expr = Obj.magic lit in
  dbg D_trace @@ lazy (sprintf "lit_var of %s" (Z3.Expr.to_string lit));
  let lit = 
    if Z3.Boolean.is_not lit then
      List.hd @@ Z3.Expr.get_args lit
    else
      lit
  in
  (* ((lit |> Z3.Expr.get_func_decl |> Z3.FuncDecl.get_id) - (2 lsl 28)) *)
  ((lit |> Z3.Expr.get_func_decl |> Z3.FuncDecl.get_name |> Z3.Symbol.get_int) - (2 lsl 28))
  |> tap (fun x -> dbg D_trace @@ lazy (sprintf "lit_var %d" x))

let lit_sign_uc solver lit = 
  (* failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"] *)
  lit_sign solver lit

let lit_var_uc solver lit = 
  (* failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"] *)
  lit_var solver lit



let add_clause solver lit_list =
  dbg D_trace @@ lazy (sprintf "add_clause  (on %d)" solver.uid);
  M.term_list_to_clause solver.state lit_list
  |> M.add solver.problem

(* val add_clause_with_id : solver_uc -> int option -> lit_uc list -> int option *)
let add_clause_with_id solver id lit_list =
  solver.clauses_with_id <- succ solver.clauses_with_id;
  let id' = 
    match id with
    | Some n -> n
    | None -> 
      let n = solver.clause_unique_id in 
      solver.clause_unique_id <- succ solver.clause_unique_id; 
      n
  in
  M.term_list_to_clause solver.state lit_list
  |> M.tagged_formula solver.state id'
  |> M.add solver.problem;
  Some id'


(* val clauses_with_id : solver_uc -> int *)

(* val set_important_lit : solver -> lit -> unit *)

(* implemented only in C++ version of minisat *)
(* val set_decision_var : solver -> bool -> lit -> unit *)
(* val set_decision_var_uc : solver_uc -> bool -> lit_uc -> unit *)

(* can raise Unsatisfiable_gr_na *)
let solve ?reset state = 
  (* if reset then (
    M
  ); *)
  M.check state.problem
  |> smt_result_to_solver_out

(* can raise Unsatisfiable_gr_na *)
let solve_uc state =
  solve state



(* val get_conflicts : solver_uc -> int list  *)

(* can raise Unsatisfiable_gr_na *)
(* Lits is interpreted as the list of corresponding unit clauses *)
let solve_assumptions ?reset state lits =
  lits
  |> List.map (M.term_to_unit_clause state.state)
  |> M.check_assumptions state.problem 
  |> smt_result_to_solver_out

(* can raise Unsatisfiable_gr_na *)
let solve_assumptions_uc state lits =
  solve_assumptions state lits

(* can raise Unsatisfiable_gr_na *)
(* val solve_assumptions_upto_id_uc : solver_uc -> lit_uc list -> int -> solver_out  *)



(* can raise Unsatisfiable_gr_na *)
let fast_solve state lits = 
  M.check state.problem
  |> smt_result_to_fast_solver_out



let lit_val state lit =
  let model = 
    match state.model with
    | Some x -> x
    | None -> 
      let model = 
        M.model state.problem 
        |> Option.default (failwith "SMTPropSolver.lit_val: no model")
      in
      state.model <- Some model;
      model
  in
  match M.eval_term_bool model lit |> Option.default (failwith "SMTPropSolver.lit_val: not a truth value") with
  | true -> True
  | false -> False



(* can raise Not_found *)
(* val get_next_implied_unit: solver -> lit
val get_next_ass_implied_unit: solver -> lit *)


(*---- prop literal key/map/hash ----*)

module PLKey = M.Term

module PLMap = Map.Make(PLKey)
module PLSet = Set.Make(PLKey)
module PLHashtbl = Hashtbl.Make(PLKey)



let lit_to_string state l =
  (* failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"] *)
  M.Term.hash l |> string_of_int

let lit_list_to_string state l =
  List.X.to_string ~first:"" ~last:"0" ~sep:" " (lit_to_string state) l

let lit_uc_to_string state x = 
  lit_to_string state x

let lit_uc_list_to_string state x = 
  lit_list_to_string state x



(* let solver_out_to_string = PropSolver.solver_out_to_string
let lit_val_to_string = PropSolver.lit_val_to_string
let lit_sign_to_string = PropSolver.lit_sign_to_string *)

let solver_out_to_string = function
  | Sat   -> "Satisfiable"
  | Unsat -> "Unsatisfiable"
  
let lit_val_to_string = function 
  | True  -> "True"
  | False -> "False"
  | Any   -> "Any"

let lit_sign_to_string = function
  | Pos  -> "Positive"
  | Neg  -> "Negative"



let pp_lit state fmt x =
  Format.pp_print_string fmt (lit_to_string state x)

let pp_lit_dimacs state fmt x =
  pp_lit state fmt x

let rec pp_lit_list_dimacs state fmt l = 
  match l with
  | [] -> 
    Format.fprintf fmt "0"
  | hd::tl -> 
    Format.fprintf fmt "%a " (pp_lit_dimacs state) hd;
    pp_lit_list_dimacs state fmt tl







let get_next_ass_implied_unit x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let get_next_implied_unit x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let solve_assumptions_upto_id_uc x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let get_conflicts x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let set_decision_var_uc x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let set_decision_var x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let set_important_lit x =
  failwith ("Unimplemented: "^__LOC__) [@@alert unimpl "Unimplemented"]
let clauses_with_id state =
  state.clauses_with_id





end
