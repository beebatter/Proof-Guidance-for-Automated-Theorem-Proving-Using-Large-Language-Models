(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain
module C = Ar_common

let depth_g = ref 0

(* Module signature of atomic abstractions *)
module type AM = sig
  type details
  val abstract: Options.options -> LI.BCSet.t -> AD.t * details
  val refine: AD.t -> details -> AD.t * details
  val empty_details: details
  (* The below function can be removed if an ATP_S is implemented and we check
   * the authenticity of the obtained models.
  *)
  val should_try_sat: unit -> bool
end

module type AME = sig
  include AM
  val solver: Options.options -> float Lib.param -> Clause.clause list ->
    Ar_atp.answer * float Lib.param
  val depth: int
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

  (* We use a short time limit to avoid the ATP stuck for a probably SAT set of
   * clauses. This can change if we use an ATP sound but unnecessary complete
   * (for quick seek of proof) with authenticity checking of the models.
   *)
  let shrt_tml_g = ref (Lib.Def 10.)

  let abstract (state:t) =
    let (adom,dtls) = A.abstract state.opts state.orgcls in
    Ar_dbg.dbg D_trace (lazy("\n\t* abstr:adom: " ^ AD.to_string `V0 adom));
    C.update_abstract_assets dtls adom state

  let refine_prc (state:t) =
    let (adom,dtls) = A.refine state.abstr_dom state.details in
    Ar_dbg.dbg D_trace (lazy("\n\t* refnmt:adom: " ^ AD.to_string `V0 adom));
    C.update_abstract_assets dtls adom state

  let atp_wrap solver (state:t) =
    let (ans,tml) =
      AD.get_all_clauses state.abstr_dom
      |> solver state.opts !shrt_tml_g
    in
    let ntml = C.time_limit_diff_intrvl !shrt_tml_g tml state.time_limit in
    let state' = C.update_time_limit state ntml in
    (state',ans)

  let rec check_answer ((state:t),ans) =
    Ar_dbg.dbg D_under (lazy("\n\t* In Ch.Ans: dpth " ^ string_of_int A.depth));
    Ar_dbg.dbg D_under (lazy (Printf.sprintf "state.time_limit: %s" (Lib.param_to_string string_of_float state.time_limit)));
    Statistics.(incr_int_stat 1 abstr_ref_under_cycles);
    match ans with
    | Ar_atp.UNSAT EmptyCl cl -> raise(Clause.Empty_clause cl)
    | UNSAT NoAssump ->
      Ar_dbg.dbg D_trace (lazy("\n\t* UNSAT NoAssump"));
      assert((Prop_solver_exchange.solve ()) = Prop_solver_exchange.PropSolver.Unsat);
      raise Lib.Unsatisfiable_gr_na
    | UNSAT Assump _ -> failwith ("ar_under_approx.ml:check_answer: " ^
                                  "UNSAT Assump; this should not happen")
    | Unknown (Some (Proof_search_loop.PS_loop_time_out e as tout)) -> begin
        (* This case can be removed if an ATP_S is implemented and we check the
         * authenticity of the obtained models.
         *)
        match state.time_limit with
        | Lib.Def t when t < 0. && A.depth = (!depth_g - 1) ->
          Ar_dbg.dbg D_trace (lazy("\n\t* Raise time out"));
          raise tout
        | Lib.Def t when t < 0. ->
          Ar_dbg.dbg D_trace (lazy("\n\t* Time out; return answer"));
          (ans, state.time_limit)
        | _ -> go_to_refinement state ans
      end
    | SAT _ -> go_to_refinement state ans
    | Unknown None -> failwith ("ar_under_approx.ml:check_answer: " ^
                                "Unknown None; this should not happen")
    | Unknown _ -> (ans, state.time_limit)

  and go_to_refinement (st:t) ans =
    if A.should_try_sat () && A.depth = (!depth_g - 1) then begin
      Ar_dbg.dbg D_trace (lazy("\n\t* Try ATP complete & sound"));
      shrt_tml_g := st.time_limit;
      let cncr_dom = AD.concrete_domain_from_cset st.orgcls st.abstr_dom in
      C.update_abstract_assets st.details cncr_dom st
      |> atp_wrap Ar_atp.complete_and_sound
      |> fun (s,a) -> (a, s.time_limit)
    end else begin
      Ar_dbg.dbg D_trace (lazy("\n\t* Go to refinement"));
      refine_prc st |> atp_wrap A.solver |> check_answer
    end
end

let get_depth () =
  let depth = !depth_g in
  depth_g := !depth_g + 1;
  depth

(* This function returns a function that encapsulates the abstraction-refinement
 * process depending of the module passed.
 * (module AM) ->
 * (Options.options -> float Lib.param -> clause list -> Ar_atp.answer) ->
 * Options.options -> float Lib.param -> clause list -> Ar_atp.answer
 *)
let ar_process (module Abstr: AM) solver =
  let module UAP = P (struct
      include Abstr
      let solver = solver
      let depth = get_depth ()
    end)
  in
  fun opts timelmt clauses ->
    C.create_state AD.Under clauses Abstr.empty_details opts timelmt
    |> UAP.abstract
    |> UAP.atp_wrap solver
    |> UAP.check_answer

let make_ar_process abstr solver =
  let ar_module =
    match abstr with
    | Options.Abstr_ref_under_cone -> (module Ar_cone_symbols: AM)
    | x -> failwith ("ar_under_approx.ml:make_ar_process: " ^
                     "not supported abstraction: " ^
                     Options.abstr_ref_under_type_to_str x)
  in
  ar_process ar_module solver

let rec get_solver atp = function
  | [] -> failwith ("ar_under_approx.ml:get_solver: " ^
                    "abstrs_list cannot be empty; this should not happen")
  | [hd] -> make_ar_process hd atp
  | hd :: tl -> make_ar_process hd (get_solver atp tl)

let solve (opts:Options.options) timelmt clauses =
  Ar_dbg.dbg D_trace (lazy (Printf.sprintf "ar_under_approx:solve:timelmt: %s" (Lib.param_to_string string_of_float timelmt)));
  depth_g := 0;
  Statistics.(assign_int_stat 0 abstr_ref_under_cycles);
  let solver = get_solver Ar_atp.complete_and_sound opts.abstr_ref_under in
  ignore(Ar_atp.check_answer (solver opts timelmt clauses))
