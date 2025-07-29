(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Logic_interface

module AD = Ar_abstract_domain
module C = Ar_common

(* Module signature of atomic abstractions *)
module type AM = sig
  type details
  val abstract: Options.options -> BCSet.t -> AD.t * details
  val refine: AD.t -> details -> TSet.t -> BCSet.t -> AD.t * details
  val empty_details: details
end

module type AME = sig
  include AM
  val solver: Options.options -> float Lib.param -> Clause.clause list ->
    Ar_atp.answer * float Lib.param
end

module type PS = sig
  type t
  val abstract: t -> t
  val atp_wrap: (Options.options -> float Lib.param -> Clause.clause list ->
            Ar_atp.answer * float Lib.param) -> t -> t * Ar_atp.answer
  val check_answer: t * Ar_atp.answer -> Ar_atp.answer * float Lib.param
end

(* This functor encapsulates functions for the abstraction-refinement process.
 * Functions in the functor are parametrised by the module passed, which is an
 * atomic abstraction.
 *)
module P (A: AME) : PS with type t = A.details C.state = struct
  type t = A.details C.state

  let postabstr_process dtls adom state =
    Ar_dbg.dbg D_trace (lazy("\n\t* postabs_pc:adom: " ^ AD.to_string `V0 adom));
    AD.get_abstr_ids adom
    |> Prop_solver_exchange.add_solver_assumptions ~only_norm:true;
    C.update_abstract_assets dtls adom state

  let abstract (state:t) =
    let (adom,dtls) = A.abstract state.opts state.orgcls in
    postabstr_process dtls adom state

  let atp_wrap solver (state:t) =
    let (ans,ntimelmt) =
      AD.get_all_clauses state.abstr_dom
      |> solver state.opts state.time_limit
    in
    AD.get_abstr_ids state.abstr_dom
    |> Prop_solver_exchange.remove_solver_assumptions;
    let state' = C.update_time_limit state ntimelmt in
    (state',ans)

  let is_uc_concr (state:t) uc_ids =
    AD.get_abstr_ids state.abstr_dom
    |> TSet.of_list
    |> TSet.inter uc_ids
    |> TSet.is_empty

  let basic_refinement (st:t) uc_ids =
    Ar_dbg.dbg D_trace (lazy("\n\t* Basic refinement"));
    let (adom,dtls) = A.refine st.abstr_dom st.details uc_ids st.orgcls in
    postabstr_process dtls adom st

  let refine_until_sat (st:t) =
    let rec refine_until_sat' (ucst:t) =
      let (nst,ans) = atp_wrap Ar_atp.complete_and_sound ucst in
      match ans with
      | UNSAT EmptyCl cl ->
        Ar_dbg.dbg D_trace (lazy("\n\t* EmptyCl: ref until SAT"));
        raise (Clause.Empty_clause cl)
      | UNSAT NoAssump ->
        Ar_dbg.dbg D_trace (lazy("\n\t* UNSAT NoAssump: ref until SAT"));
        assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
        raise Lib.Unsatisfiable_gr_na
      | UNSAT Assump (ucc_ids,_) when is_uc_concr nst ucc_ids ->
        assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
        Ar_dbg.dbg D_trace (lazy("\n\t* UC is concr in cntx; ref until SAT"));
        let (_,ucdt) = A.refine nst.abstr_dom nst.details ucc_ids nst.orgcls in
        let (ad,dt) = A.refine st.abstr_dom ucdt TSet.empty st.orgcls in
        C.update_time_limit (postabstr_process dt ad st) nst.time_limit
      | UNSAT (Assump (ucc_ids,_)) ->
        Ar_dbg.dbg D_trace (lazy("\n\t* Call refine until SAT again"));
        basic_refinement nst ucc_ids |> refine_until_sat'
      | Unknown (Some (Proof_search_loop.PS_loop_time_out _)) ->
        Ar_dbg.dbg D_trace (lazy("\n\t* Unt SAT:Unkwn Some:time out:retn st"));
        C.update_time_limit st nst.time_limit
      | Unknown (Some exc) ->
        Ar_dbg.dbg D_trace (lazy("\n\t* Unt SAT:Unknown Some: rise exception"));
        raise exc
      | Unknown None -> failwith ("ar_over_approx.ml:refine_until_sat:" ^
                                  " Unknown None; this should not happen")
      | SAT _ ->
        Ar_dbg.dbg D_trace (lazy("\n\t* Refinement until SAT finishes"));
        (* We pass an empty set of UC ids to the refine function to only apply
         * the refined abstraction obtained in the previous steps.
         *)
        let (ad,dt) = A.refine st.abstr_dom nst.details TSet.empty st.orgcls in
        C.update_time_limit (postabstr_process dt ad st) nst.time_limit
    in
    refine_until_sat'

  let refine_prc (st:t) uc_ids concr_uc ccls_uc =
    if not st.opts.abstr_ref_until_sat then begin
      Ar_dbg.dbg D_trace (lazy("\n\t* Normal refinement flow"));
      basic_refinement st uc_ids
    end
    else begin
      Ar_dbg.dbg D_trace (lazy("\n\t* Refine until SAT"));
      (* We create a closure to capture the general state in the refinement
       * until SAT procedure. Thus, we can apply the refinement until SAT only
       * to the spurious UC and when this refinement finishes the refined
       * abstraction is applied to the original clauses in the general state.
       *)
      let closure_until_sat = refine_until_sat st in
      (* We refine the abstraction using the data in 'st' but the refined
       * abstraction is applied to the concrete clauses of the UC (concr_uc).
       * Then, we create a fresh state using only the UC clauses to pass it to
       * the refinement until SAT.
       *)
      let (uc_adom,dtls) = A.refine st.abstr_dom st.details uc_ids concr_uc in
      C.create_state AD.Over ccls_uc A.empty_details st.opts st.time_limit
      |> postabstr_process dtls uc_adom
      |> closure_until_sat
    end

  let concretise_uc uc_ids uc_cls (state:t) =
    Ar_dbg.dbg D_trace (lazy("\n\t* concretising UC"));
    let join id a =
      let concr_cls = AD.gamma_fun id state.abstr_dom in
      BCSet.union concr_cls a
    in
    let open TstpProof in 
    let proof = get_proof (clauses_to_pf uc_cls) in       
    let parents_uc = BCSet.of_list (proof_to_list_cls proof) in
    let concr_uc_ids = TSet.fold join uc_ids BCSet.empty in
    let concr_parents = BCSet.inter parents_uc state.orgcls in
    let concr_uc = BCSet.union concr_uc_ids concr_parents in
    Ar_dbg.dbg D_over (lazy("\n\t concr_uc:\n" ^ Clause.clause_list_to_tptp
                              (BCSet.elements concr_uc)));
    concr_uc

  let rec check_answer ((state:t),ans) =
    Statistics.(incr_int_stat 1 abstr_ref_over_cycles);
    match ans with
    | Ar_atp.SAT _ ->
      Ar_dbg.dbg D_trace (lazy("\n\t* SAT; return answer"));
      (ans, state.time_limit)
    | Unknown None -> failwith ("ar_over_approx.ml:check_answer: " ^
                                "Unknown None; this should not happen")
    | UNSAT EmptyCl cl ->
      Ar_dbg.dbg D_trace (lazy("\n\t* UNSAT Empty clause"));
      raise(Clause.Empty_clause cl)
    | UNSAT NoAssump ->
      Ar_dbg.dbg D_trace (lazy("\n\t* UNSAT NoAssump"));
      assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
      raise Lib.Unsatisfiable_gr_na
    | UNSAT Assump (uc_ids,_) when is_uc_concr state uc_ids ->
      assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
      Ar_dbg.dbg D_trace (lazy("\n\t* UC is concrete in context"));
      (ans, state.time_limit)
    | UNSAT Assump (uc_ids,uc_cls) -> check_spurious_uc state uc_ids uc_cls
    | Unknown _ -> (ans, state.time_limit)

  and check_spurious_uc (st:t) uc_ids uc_cls =
    let cncr_uc = concretise_uc uc_ids uc_cls st in
    let ccls_uc = BCSet.elements cncr_uc in
    Ar_dbg.dbg D_trace (lazy("\n\t* checking whether UC is spurious"));
    (* The time limit to check the UC authenticity is limited to the value in
     * 'shrt_tml'. If the ATP reaches the time limit, we assume that the UC is
     * spurious. Thus, the abstraction will be refined.
     *)
    let shrt_tml = Lib.Def 1. in
    let (ans,tml) = Ar_atp.complete_and_sound st.opts shrt_tml ccls_uc in
    let ntml = C.time_limit_diff_intrvl shrt_tml tml st.time_limit in
    let st' = C.update_time_limit st ntml in
    match ans with
    | UNSAT EmptyCl cl ->
      Ar_dbg.dbg D_trace (lazy("\n\t* EmptyCl: UC is authentic"));
      raise (Clause.Empty_clause cl)
    | UNSAT NoAssump ->
      assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
      Ar_dbg.dbg D_trace (lazy("\n\t* UNSAT NoAssump: UC is authentic"));
      raise Lib.Unsatisfiable_gr_na
    | UNSAT Assump (ucc_ids,_) when is_uc_concr st ucc_ids ->
      assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
      Ar_dbg.dbg D_trace (lazy("\n\t* UC is concrete in context; authentic"));
      (ans, ntml)
    | UNSAT Assump _ -> failwith ("ar_over_approx.ml:check_spurious_uc: " ^
                                  "UNSAT Assump; this should not happen")
    | Unknown (Some (Proof_search_loop.PS_loop_time_out _))
    | SAT _ ->
      Ar_dbg.dbg D_trace (lazy("\n\t* Go to refinement"));
      refine_prc st' uc_ids cncr_uc ccls_uc |> atp_wrap A.solver |> check_answer
    | Unknown None -> failwith ("ar_over_approx.ml:check_spurious_uc: " ^
                                "Unknown None; this should not happen")
    | Unknown _ -> (ans, ntml)
end

(* This function returns a function that encapsulates the abstraction-refinement
 * process depending of the module passed.
 * (module AM) ->
 * (Options.options -> float Lib.param -> clause list -> Ar_atp.answer) ->
 * Options.options -> float Lib.param -> clause list -> Ar_atp.answer
 *)
let ar_process (module Abstr: AM) solver =
  let module OAP = P (struct include Abstr let solver = solver end) in
  fun opts timelmt clauses ->
    C.create_state AD.Over clauses Abstr.empty_details opts timelmt
    |> OAP.abstract
    |> OAP.atp_wrap solver
    |> OAP.check_answer

let make_ar_process abstr solver =
  let ar_module =
    match abstr with
    | Options.Abstr_ref_sig -> (module Ar_signature_grouping: AM)
    | Abstr_ref_arg_filter -> (module Ar_argument_filtering: AM)
    | Abstr_ref_gen -> (module Ar_generalisation: AM)
    | Abstr_ref_subs -> (module Ar_subsumption_based: AM)
    | x -> failwith ("ar_over_approx.ml:make_ar_process: " ^
                     "not supported abstraction: " ^
                     Options.abstr_ref_type_to_str x)
  in
  ar_process ar_module solver

let rec get_solver = function
  | [] -> failwith ("ar_over_approx.ml:get_solver: " ^
                    "abstrs_list cannot be empty; this should not happen")
  | [hd] -> make_ar_process hd Ar_atp.complete_and_sound
  | hd :: tl -> make_ar_process hd (get_solver tl)

let solve (opts:Options.options) timelmt clauses =
  Ar_dbg.dbg D_trace (lazy (Printf.sprintf "ar_over_approx:solve:timelmt: %s" (Lib.param_to_string string_of_float timelmt)));
  Statistics.(assign_int_stat 0 abstr_ref_over_cycles);
  let solver = get_solver opts.abstr_ref in
  ignore(Ar_atp.check_answer (solver opts timelmt clauses))
