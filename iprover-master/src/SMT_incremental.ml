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
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
  
(*----- debug -----*)



module SMT = SMTSolver

module SMTClauseMap = Map.Make(SMT.Clause)

type set = {
  state: SMT.state;
  problem: SMT.problem;
  (* For global subsumption, we need a map from smt clauses back to iprover clauses *)
  (* mutable mapping: clause SMTClauseMap.t; *)

  strategy_add: SMT.problem -> SMT.result;
  strategy_check: SMT.problem -> SMT.clause list -> SMT.result;
}

let make_set ?initial_clauses state =
  let problem = SMT.make_problem_fast state in
  begin match initial_clauses with
  | None -> ()
  | Some initial_clauses -> 
    initial_clauses
    |> List.map (SMT.clause_to_smt state)
    |> SMT.add_many problem
  end;
  (* let mapping = SMTClauseMap.empty in *)

  let strategy_add = 
    (* SMT.check *)
    (* fun x -> SMT_check_assumptions x [] *)
    SMT.check
  in
  let strategy_check problem lits = 
    (* SMT.check_assumptions problem [lit] *)
    (* let tactic = Z3.Tactic.mk_tactic state "solve-eqs" in *)
    SMT.check_assumptions problem lits
  in

  {state; problem; (* mapping; *) strategy_add; strategy_check}



(* let (<-@) a f = a <- f a *)

let add_no_check set clause =
  let smt_clause = SMT.clause_to_smt set.state clause in
  SMT.add set.problem smt_clause
  (* Add reverse mapping (disabled for now) *)
  (*set.mapping <- set.mapping |> SMTClauseMap.add smt_clause clause;*)

let add set clause =
  add_no_check set clause;
  (* Run and check if inconsistent *)
  match set.strategy_add set.problem with
  | SMT.Unsat -> true
  | SMT.Sat | SMT.Unknown -> false





let global_smt_subsumption set clause =
  (* Invariant is that clause itself is already in set *)
  (* For each literal, check if it is implied *)
  let lits = Clause.get_lits clause in
  let smt_lits = 
    lits |> List.map (fun lit -> 
      (* SMT.term_to_unit_clause set.state @@ SMT.lit_to_smt set.state lit *)
      lit
      |> add_compl_lit
      |> SMT.lit_to_smt set.state
      |> SMT.term_to_unit_clause set.state 
    )
  in

  let any_changed = ref false in
  let rec loop checked rest checked_ip rest_ip =
    match rest with
    | hd::tl ->
      let hd_ip, tl_ip = List.X.hd_tl rest_ip in
      dbg D_trace @@ lazy (sprintf "loop: %s" (Term.to_string hd_ip));
      (* Check implication without this literal *)
      let assumptions = checked @ tl in
      begin match assumptions with 
      | [] -> 
        dbg D_trace @@ lazy (sprintf "no assumptions");
        dassert (fun () -> match set.strategy_check set.problem assumptions with SMT.Sat | SMT.Unknown -> true | SMT.Unsat -> false);
        loop (hd::checked) tl (hd_ip::checked_ip) tl_ip
      | _::_ -> 
        begin match set.strategy_check set.problem assumptions with
        | SMT.Unsat -> 
          dbg D_trace @@ lazy (sprintf "implied");
          any_changed := true; 
          loop     (checked) tl        (checked_ip) tl_ip
        | SMT.Sat | SMT.Unknown -> 
          dbg D_trace @@ lazy (sprintf "not implied");
          loop (hd::checked) tl (hd_ip::checked_ip) tl_ip
        end
      end
    | [] (*| [_]*) -> List.rev checked_ip  (* To recover the initial order *)
  in
  let lits' = loop [] smt_lits [] lits in

  if !any_changed then (
    (* TODO: check something about "raw clause?" *)
    let prop_dep_lvl = 1 in (* What is this? *)
    let tstp_source = Clause.tstp_source_global_subsumption prop_dep_lvl clause in
    let clause' = create_clause tstp_source lits' in
    clause'
  ) else (
    clause
  )



(* let remove set clause =
  let smt_clause = SMT.clause_to_smt set.state clause in
  SMT.Uc. *)
