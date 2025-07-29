(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Lib
type exec =
  | Some of exn
  | None

type uc =
  | EmptyCl of Clause.clause
  | Assump of Logic_interface.TSet.t * Clause.clause list
  | NoAssump

type model =
  | Inst of Instantiation_env.inst_pre_model
  | Res of Resolution_loop.res_model
  | Sup of Superposition.sup_model

type answer =
  | UNSAT of uc
  | SAT of model
  | Unknown of exec

(* Estimates the time left for the current schedule *)
let estimate_time_limit start_time time_limit =
  match time_limit with
  | Lib.Undef -> Lib.Undef
  | Lib.Def time_l ->
    let used_time = (Unix.gettimeofday ()) -. start_time in
    Lib.Def(time_l -. used_time) (* can be negative *)

let check_answer (ans,_) =
  match ans with
  | SAT (Sup model) -> raise (Superposition.Sup_satisfiable model)
  | SAT (Inst model) -> raise (Instantiation_loop.Inst_satisfiable model)
  | SAT (Res model) -> raise (Resolution_loop.Res_satisfiable model)
  | UNSAT EmptyCl cl -> raise(Clause.Empty_clause cl)
  | UNSAT NoAssump ->
      assert (Prop_solver_exchange.solve () == Prop_solver_exchange.PropSolver.Unsat);
      raise Lib.Unsatisfiable_gr_na

  | UNSAT Assump _ -> failwith ("ar_atp.ml:check_answer: " ^
                                "UNSAT Assump; this should not happen")
  | Unknown (Some exc) -> raise_trace exc
  | Unknown None -> failwith ("ar_atp.ml:check_answer: " ^
                              "Unknown None; this should not happen")

let complete_and_sound (opts:Options.options) time_limit clauses =
  let start_time = Unix.gettimeofday () in
  let answer =
    try
      Prop_solver_exchange.clear_soft_assumptions ();
      let clauses' =
        if opts.abstr_ref_prep then
          let prep_state =
            Preprocess.prep_create_state ~prep_opts:opts ~clause_list:clauses
              ~extra_side_atoms:[]
          in
          Preprocess.preprocess_sim ~before_eq_axioms:false prep_state;
          Preprocess.prep_get_clauses prep_state
        else
          clauses
      in
      Ar_dbg.dbg D_atp (lazy(
          "\n\t** all assumptions: " ^ Term.term_list_to_string
            (Term.TSet.elements (Prop_solver_exchange.get_solver_fof_assumptions
                                   ~soft:false ~sim:false))));
      Proof_search_loop.ps_full_loop ~opts ~time_limit
        (Proof_search_loop.clauses_with_eq_axs_to_ps_input_clauses clauses');
      Unknown(None)
    with
    | Lib.Unsatisfiable_gr_na ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP: UNSAT_gr_na"));
      UNSAT NoAssump
    | Lib.Unsatisfiable_gr ->
      assert (Prop_solver_exchange.soft_assumptions_is_empty ());
      let uc = Prop_solver_exchange.get_unsat_core ~soft:false () in
      let uc_assump = UnsatCore.get_assumptions uc in
      let uc_clauses = UnsatCore.get_clauses uc in
      if List.X.is_empty uc_assump
      then begin
        Ar_dbg.dbg D_trace(lazy("\n\t* ATP: UNSAT_gr: UC NoAssump"));
        UNSAT NoAssump
      end
      else begin
        Ar_dbg.dbg D_trace(lazy("\n\t* ATP: UNSAT_gr: UC Assump"));
        UNSAT (Assump (Logic_interface.TSet.of_list uc_assump, uc_clauses))
      end
    | Clause.Empty_clause clause ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP: Empty clause"));
      UNSAT (EmptyCl clause)
    | Instantiation_loop.Inst_satisfiable model ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP: SAT Instantiation"));
      SAT (Inst model)
    | Resolution_loop.Res_satisfiable model ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP: SAT Resolution"));
      SAT (Res model)
    | Superposition.Sup_satisfiable model ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP: SAT Superposition"));
      SAT (Sup model)
    | Proof_search_loop.PS_loop_time_out x ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP exception: loop_time_out"));
      Unknown (Some (Proof_search_loop.PS_loop_time_out x))
    | other ->
      Ar_dbg.dbg D_trace (lazy ("\n\t* ATP exception: " ^ Printexc.to_string other));
      Unknown (Some other)
  in
  let new_time_limit = estimate_time_limit start_time time_limit in
  (answer, new_time_limit)
