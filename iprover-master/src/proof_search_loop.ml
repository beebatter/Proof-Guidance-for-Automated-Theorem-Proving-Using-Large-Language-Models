(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2021 Konstantin Korovin and The University of Manchester. 
   This file is part of iProver - a theorem prover for first-order logic.

   iProver is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or 
   (at your option) any later version.
   iProver is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
   See the GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with iProver.  If not, see <http://www.gnu.org/licenses/>.         *)
(*----------------------------------------------------------------------[C]-*)


open Lib
open Options
open Statistics
open Logic_interface
open Instantiation_loop
open Resolution_loop


(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_mem
  | D_solve
  | D_shared_clauses
  | D_eq_types
  | D_reset 
  | D_sup_restart
  | D_sup_deep
  | D_stagnation
      
let dbg_gr_to_str = function 
  | D_trace -> "trace"	
  | D_mem -> "mem"
  | D_solve -> "solve"
  | D_shared_clauses -> "shared_clauses"
  | D_eq_types -> "eq_types"
  | D_reset -> "reset"
  | D_sup_restart -> "sup_restart"
  | D_sup_deep -> "sup_deep"
  | D_stagnation -> "stagnation"
      
let dbg_groups = [
  D_trace;  
  D_reset;
    
  (* D_mem; *)
  (* D_solve; *)
  D_shared_clauses;
  
  D_sup_restart;
    D_sup_deep;
    
(*  D_eq_types; *)
  
]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module PropSolver = Prop_solver_exchange.PropSolver

(* change Sched_Time_Out to PS_loop_timeout *)

exception PS_loop_time_out of int

let check_sched_time ~start_time ~time_limit full_loop_counter =
  match time_limit with
  | Undef -> ()
  | Def time_limit ->
    if Float.O.(Unix.gettimeofday () -. start_time > time_limit)
    then raise (PS_loop_time_out full_loop_counter)
    else ()


let time_to_string time =
  match time with
  | Def(float) -> string_of_float float
  | Undef -> "Unbounded"

(*--------  options ----------*)

(* last printed options *)
let last_used_options = ref None

(*KK clean!! *)

(* set options and print them *)
let save_and_print_options ~opts =
  (* save current options *)
  last_used_options := Some opts;
  (* print options *)
  (* out_str ((s_pref_str ())^"Current options:\n"^(options_to_str opts)^"\n\n"); *)
  out_str (s_pref_str ());
  printf_tptp "Current options:\n";
  options_to_stdout opts;
  print_string "\n\n";
  (* that's it *)
  ()

(* print options if they are changed *)
let print_options_if_new ~opts =
  (* if Option.equal ~eq:Poly.(=) !last_used_options opts then *)
  match !last_used_options with
  (* same options -- don't print *)
  | Some op when Poly.(op = opts) -> ()
  (* not the same options -- save and print *)
  | _ -> save_and_print_options ~opts


(*------------ proof search state --------------------*)
        

type ps_input_clauses = 
    {
     ps_clauses_with_eq_axioms : clause list;
     ps_clauses_no_eq_axioms   : clause list;
     ps_problem_properties     : Problem_properties.prob_props; (* currently used only to check if problem is equational *)
   }

let clauses_with_eq_axs_to_ps_input_clauses clause_list = 
  {
   ps_clauses_with_eq_axioms=clause_list;
   ps_clauses_no_eq_axioms=clause_list;
   ps_problem_properties= Problem_properties.get_prob_props clause_list;
 }



(* ---------------------------------------------*)
(* proof search loop: in a loop, from component list pick one with the maximal deficit and execute comp_main_fun *)
(* current componenets: inst/res/sup *)
(* to run ps_main_loop one needs to define functions specified ps_comp_funs *)
(* currently ps_comp_deficit_fun is based on the number of generated clauses by each components after each iteration *)
(* TODO: alternative version based on comp_cnt; this is similar to old implementation *)
(* ------------------------------------------------ *)

type inst_state_base = 
    {
     mutable inst_state : inst_state;
     mutable inst_learning_bound : int;
     mutable inst_learning_counter : int;
   }

type sup_state_base = 
    {
     mutable sup_state : Superposition.state; 

     mutable sup_restart_mult : int;  (* if mult <= 1 no restarts *)

     mutable sup_restart_bound : int; (* initially: mult*(num_input_clauses)  *)
                                      (* restart when num_current_clauses  > bound  *) 
                                      (* bound:=mult*bound  *)

(* iterative deepening *)
(*     mutable sup_deep_cl_init_bound   : int; *)(* size of clauses are initially limited to (sup_deep_cl_init_bound * max_size(input)) *)
     mutable sup_deep_cl_mult         : int; (* multiplier of sup_deep_cl_coeff_bound if saturation is reached *)     
     mutable sup_deep_cl_size_coeff   : int; (* current bound; reassigned when sat *)  
     mutable sup_deep_num_discared_cl : int; (* number of discarded clasues; if > 0 then assume that incomplete otherwise complete *)
   }


type component_base = 
  |Inst of inst_state_base
  |Res of res_state 
  |Sup of Superposition.state  (* pure superposition *)
  |Sup_extra of sup_state_base (* sup. with extras such iterative deepening *)

(*----------*)    
let comp_id_cnt = ref 0
    
(* components *)
type comp = 
    { 
      mutable comp_base           : component_base; 
      comp_name                   : string;
      comp_id                     : int;
      comp_mult                   : int; (* component multiplier (proporition) *)
      comp_opts                   : Options.options;
      mutable comp_time           : float; (* time taken by the component ps_comp_main_fun *)
      mutable comp_cnt            : int; (* how many times comp was called *)
      mutable last_deficit        : int; 
      mutable stagnation_cnt      : int; (* how many times comp was called without change in deficit *)
    }

let comp_to_string comp = comp.comp_name

type ps_state = 
    {
     mutable ps_components     : comp list; 
     ps_input_clauses          : ps_input_clauses;
     ps_start_time             : float;
     ps_time_limit             : float param; 
     ps_opts                   : Options.options;
 (*    mutable ps_comp_mult_sum  : int; *)(* sum of all current component multipliers; used to compute deficit *)
     ps_stagnation_limit       : int;
     mutable ps_cnt            : int;
   }


type ps_comp_funs = 
    {
     ps_pre_comp_fun : ps_state -> unit;
     ps_post_comp_fun : ps_state -> unit;

     ps_comp_deficit_fun : ps_state -> comp -> int;
       (* ps_scheduler executes component with the largest deficit *)
       (* change to float ? *)

     ps_comp_main_fun : ps_state -> comp -> comp option;    
       (* one iteration of component's reasoning loop *)          
       (* should also update state such that next call to deficit will be correctly computed *)

     ps_stagnation_fun : ps_state -> comp -> unit;
       (* stagnation occurs when deficit does not change for a component in ps_stagnation_limit calls of the comp. *)
       (* this can happen e.g. when component gets switched off (res limit) and does nothing after or clauses are added to active without producing extra clauses *)
   }

      
let comp_record_time comp f = 
  let t_start = Unix.gettimeofday() in
  let record_time () =    
    let t_end = Unix.gettimeofday() in
    comp.comp_time <- comp.comp_time +. t_end -. t_start;
  in  
    try    
      let res = f () in 
      record_time ();
      res
    with x -> 
      record_time ();
      raise_trace x

    
(*------  create componentes ------*)

let create_comp ~comp_base ~comp_mult ~comp_base_name ~opts = 
  let comp_id = !comp_id_cnt in 
  comp_id_cnt := !comp_id_cnt +1;
  let comp_name = sprintf "%s_%d" comp_base_name comp_id in
  { 
    comp_base;
    comp_name;
    comp_id;
    comp_mult;
    comp_opts = opts;
    comp_time = 0.;
    comp_cnt  = 0;
    last_deficit = 0;
    stagnation_cnt =0;
  }

let create_ps_state ~ps_components ~ps_input_clauses ~ps_start_time ~ps_time_limit ~ps_opts ~ps_stagnation_limit = 
   {
    ps_components; 
    ps_input_clauses;
    ps_start_time;
    ps_time_limit; 
    ps_opts;
(*    ps_comp_mult_sum = List.fold_left (fun rest comp -> rest + comp.comp_mult) 0 ps_components; *)
    ps_stagnation_limit;
    ps_cnt = 0;
  }


(*--- resolution ---*)
let create_res_state ~opts ps_input_clauses = 
   let res_state = res_create_state ~opts ~res_prep_only:false in
   res_add_clause_list res_state ps_input_clauses.ps_clauses_with_eq_axioms; 

   (if opts.share_sel_clauses
   then
     (let shared_clauses = BCSet.elements (Shared_clauses.get_all_shared_clauses ()) in 
     dbg D_shared_clauses (lazy ("res adding shared: "^(Clause.clause_list_to_string shared_clauses)));
     res_add_clause_list res_state shared_clauses;
     )
   );
   res_state

let create_res_comp ~opts ps_input_clauses = 
  let comp_base_name = "resolution_comp" in 
  let res_state = create_res_state ~opts ps_input_clauses in
  let res_comp = create_comp ~comp_mult:(opts.comb_res_mult) ~comp_base:(Res(res_state)) ~comp_base_name ~opts  in 
  res_comp
  
(*--- instantiation ---*)
let create_inst_state ~opts ps_input_clauses = 
  let inst_state = inst_create_state ~opts in
  inst_add_clause_list inst_state ps_input_clauses.ps_clauses_with_eq_axioms; 

  (if opts.share_sel_clauses
  then     
    (let shared_clauses = BCSet.elements (Shared_clauses.get_all_shared_clauses ()) in 
    dbg D_shared_clauses (lazy ("inst adding shared: "^(Clause.clause_list_to_string shared_clauses)));
    inst_add_clause_list inst_state shared_clauses;
    )
  );
  inst_state

let create_inst_state_base ~opts ~inst_learning_bound ps_input_clauses = 
  let inst_state = create_inst_state ~opts ps_input_clauses in
  
  let inst_state_base = 
    {
     inst_state;
     inst_learning_bound;
     inst_learning_counter = 0;
   }
  in 
  inst_state_base


let create_inst_comp ~opts ~inst_learning_bound ps_input_clauses = 
  let comp_base_name = "instantiation_comp" in 
  let inst_state_base = create_inst_state_base ~opts ~inst_learning_bound ps_input_clauses in

  let inst_comp = create_comp ~comp_mult:(opts.comb_inst_mult) ~comp_base:(Inst(inst_state_base)) ~comp_base_name ~opts  in 
  inst_comp
  
(* note that comp time is recorded in comp, and renew_inst_state_base does not change the running time of comp. *)
let renew_inst_state_base ~inst_learning_bound ~opts ps_input_clauses =    
  dbg D_reset @@ lazy ("renew_inst_state_base");

  Prop_solver_exchange.clear_soft_assumptions ();
  let new_inst_base = create_inst_state_base ~opts ~inst_learning_bound ps_input_clauses in 
  new_inst_base

(* let new_inst_comp = create_inst_comp ~opts ~inst_learning_bound ps_input_clauses in
  new_inst_comp
*)

(*--- superposition ---*)

let create_sup_state (* ~hook_passive *) ?(scores_frozen=None) ~opts ps_input_clauses =  (* problems properties are used for identifying eq sorts *)
  let sup_input_clauses = 
    if opts.share_sel_clauses then (
      let shared_clauses = BCSet.elements (Shared_clauses.get_all_shared_clauses ()) in
      dbg D_shared_clauses @@ lazy (sprintf "sup adding shared: %s" (Clause.clause_list_to_string shared_clauses));
      shared_clauses @ ps_input_clauses.ps_clauses_no_eq_axioms
    ) else (
      ps_input_clauses.ps_clauses_no_eq_axioms 
    )
  in

  (* Problem properties *)
  let prob_props = 
    if sup_input_clauses == ps_input_clauses.ps_clauses_no_eq_axioms then 
      ps_input_clauses.ps_problem_properties 
    else 
      Problem_properties.get_prob_props sup_input_clauses
  in 

  (* let demod_use_ground = opts.demod_use_ground in *)

  (* Selectively disable demod and subsumption (disabling subsumption deprecated due to unit index) *)
  let demod_flag = Problem_properties.has_eq prob_props in
  let subs_flag = true (* not @@ Problem_properties.is_unit_eq prob_props *) in
  let [@warning "-26"] ac_flag = Problem_properties.has_ac prob_props in (* KK not used; TODO incorporate *)
  let sim_flag = 
    match demod_flag, subs_flag with
    | true, true -> Superposition.AllSims
    | false, _ -> Superposition.NoDemod
    | true, false -> Superposition.NoSubs
  in

  (* Discover eq_types *)
  let eq_types = 
    let signature = Clause.clause_list_signature sup_input_clauses in
    dbg D_eq_types @@ lazy (sprintf "sig_eq_types:\n%s"
      (list_to_string Symbol.to_string (SSet.elements signature.sig_eq_types) "\n")
    );
    dbg D_eq_types @@ lazy (sprintf "sig_pure_dis_eq_types:\n%s"
      (list_to_string Symbol.to_string (SSet.elements signature.sig_pure_dis_eq_types) "\n")
    );
    SSet.diff signature.sig_eq_types signature.sig_pure_dis_eq_types
  in
  dbg D_eq_types @@ lazy (sprintf "Eq types:\n%s"
    (list_to_string Symbol.to_string (SSet.elements eq_types) "\n")
  );
  dbg_env D_eq_types (fun () -> 
    (* if not (SSet.elements eq_types = [Symbol.symb_default_type]) then  *)
    if not @@ List.X.equal ~eq:(==) (SSet.elements eq_types) [Symbol.symb_default_type] then 
      dbg D_eq_types @@ lazy "There exist some non-equality types"
  );

  let sup_options = Superposition.make_options (*~hook_passive *) opts ~sim_flag ~eq_types ~prob_props in
  let sup_state = Superposition.create ~scores_frozen sup_options in
  
  Superposition.add_input_clauses sup_state sup_input_clauses;
  sup_state  

let create_sup_comp ?(scores_frozen=None) ~opts ps_input_clauses = 
  let comp_base_name = "superposition_comp" in 
  let sup_state = create_sup_state (*~hook_passive:(fun c -> Some c) *) ~scores_frozen ~opts ps_input_clauses in (* trivial hook *)
  let sup_comp = create_comp ~comp_mult:(opts.comb_sup_mult) ~comp_base:(Sup(sup_state)) ~comp_base_name ~opts  in 
  sup_comp
 

(*------ superposition extra  extra -----*)

let sup_extra_assign_hook_passive sup_extra_state_base ps_input_clauses = 
  if sup_extra_state_base.sup_deep_cl_size_coeff <= 0 then
    () 
  else (
    let max_cl_size = 
      let max_cl = list_find_max_element Clause.cmp_num_symb ps_input_clauses.ps_clauses_no_eq_axioms in 
      Clause.num_of_symb max_cl 
    in
    assert (max_cl_size > 0);
    (* hook needs to be redefined and reassigned each time sup_deep_cl_coeff_bound is changed! *)
    let hook_passive c = 
      let cl_bound = sup_extra_state_base.sup_deep_cl_size_coeff * max_cl_size in
      if Clause.num_of_symb c > cl_bound then (
        dbg D_trace @@ lazy (sprintf "sup_extra:hook_passive: cl bound %d: clause eliminated: %s" cl_bound (Clause.to_string c));      
     
        sup_extra_state_base.sup_deep_num_discared_cl <- sup_extra_state_base.sup_deep_num_discared_cl +1;
        Statistics.bump_int_stat sup_deep_cl_discarded; 
        None
      ) else (
        Some c
      )
    in
    Superposition.assign_hook_passive hook_passive sup_extra_state_base.sup_state
  )


let create_sup_exrta_state_base ?(scores_frozen=None) ~opts ~sup_deep_cl_mult ~sup_deep_cl_size_coeff ps_input_clauses =
    
  let sup_state = create_sup_state ~scores_frozen ~opts ps_input_clauses in

  let num_of_input_clauses = ps_input_clauses.ps_problem_properties.Problem_properties.clauses in
  let sup_extra_state_base = 
    {
     sup_state; 
     
     sup_restart_mult= opts.sup_restarts_mult;  (* if mult <= 0 no restarts *)

     sup_restart_bound = num_of_input_clauses * opts.sup_restarts_mult; (* initially: mult*(num_input_clauses)  *)
   
(* iterative deepening *) (* TODO: add to options *)
(*     sup_deep_cl_init_bound = 2;  *)
     sup_deep_cl_mult;        (* multiplier of sup_deep_cl_size_coeff if saturation is reached *)     
     sup_deep_cl_size_coeff;  (* current bound; reassigned when sat *) 
     sup_deep_num_discared_cl  = 0; (* number of discarded clasues; if > 0 then assume that incomplete otherwise complete *)
   }
  in
  sup_extra_assign_hook_passive sup_extra_state_base ps_input_clauses;
  sup_extra_state_base

 
let create_sup_extra_comp ?(scores_frozen=None) ~opts ~sup_deep_cl_mult ~sup_deep_cl_size_coeff ps_input_clauses = 
  let comp_base_name = "superposition_extra_comp" in 
  let sup_extra_state_base = create_sup_exrta_state_base ~scores_frozen ~opts ~sup_deep_cl_mult ~sup_deep_cl_size_coeff ps_input_clauses in
  let sup_extra_comp = create_comp ~comp_mult:(opts.comb_sup_deep_mult) ~comp_base:(Sup_extra(sup_extra_state_base)) ~comp_base_name ~opts  in 
  sup_extra_comp


(*--- scores ---*)
let sup_select_scored_clauses ~opts sup_state = 
  let score_fun = Superposition_scores.score_type_to_fun opts.sup_score in
  let frac_top_score = opts.sup_share_score_frac in (* 90% *)
  let max_num_cl = opts.sup_share_max_num_cl in 
  let scores = Superposition.get_scores sup_state in
  Superposition_scores.select_top_clauses ~frac_top_score ~max_num_cl ~score_fun scores
    

(*--------*)

let create_comp_list ~opts ~ps_input_clauses =
  let inst_comp = 
    if opts.instantiation_flag && opts.comb_inst_mult > 0 then 
      Some(create_inst_comp ~opts ~inst_learning_bound:opts.inst_learning_start ps_input_clauses) 
    else 
      None
  in
  let res_comp = 
    if opts.resolution_flag && opts.comb_res_mult > 0 then 
      Some(create_res_comp ~opts ps_input_clauses)
    else
      None
  in
  let sup_comp =  (* run together with sup_extra_comp *)  (* Before: None when opts.sup_iter_deepening = 0 *)
    if opts.superposition_flag && opts.comb_sup_mult > 0 (* && opts.sup_iter_deepening = 0 *) then
      Some(create_sup_comp ~opts ps_input_clauses)
    else
      None
  in

  let sup_extra_comp = (* when opts.sup_iter_deepening > 0 *)
    if opts.superposition_flag && (opts.comb_sup_deep_mult > 0 && (opts.sup_iter_deepening > 0 || opts.sup_restarts_mult > 0)) then 
      let sup_deep_cl_mult = 2 in 
      let sup_deep_cl_size_coeff = opts.sup_iter_deepening in 
      Some(create_sup_extra_comp ~opts ~sup_deep_cl_mult ~sup_deep_cl_size_coeff ps_input_clauses)
    else
      None
  in
  let comp_opt_list = [inst_comp; res_comp; sup_comp; sup_extra_comp] in 
  let comp_list = List.filter_map id_fun comp_opt_list  in (* filters out None *)
  comp_list
        
  

(*----- ps_comp_main_fun returns Some(new state) or None if comp is deactivated -------*)


(*-------------*)
let ps_res_main_fun ~opts res_state =   
  let o = opts in
  if not o.resolution_flag then 
    None
  else (
    dbg D_trace @@ lazy ("resolution comp");
    (* resolution_counter:= !resolution_counter + 1; move to comp level *) 
    try
      assign_discount_time_limit (o.res_time_limit);
      assign_discount_start_time ();

      resolution_loop_exchange res_state;

      unassign_discount_time_limit ();
      Some (res_state)
    with Timeout ->
      unassign_discount_time_limit ();
      out_warning (pref_str^"Switching off resolution: loop timeout \n");
      o.resolution_flag <- false; 
      None
  )


(*------------------------------------*)

let ps_inst_main_fun ~comp_cnt ~opts ~ps_input_clauses ~inst_state_base =   
  let inst_state = inst_state_base.inst_state in 
  let o = opts in
  if not o.instantiation_flag then 
    None
  else (
    dbg D_trace @@ lazy ("inst comp");
    if not o.inst_learning_loop_flag 
    || inst_state_base.inst_learning_counter < inst_state_base.inst_learning_bound
    then (
      (* ps_state.ps_inst_cnt <- ps_state.ps_inst_cnt +1; 
      dbg D_trace (lazy ("ps_loop_inst: "^(string_of_int ps_state.ps_inst_cnt))); *)

      inst_state_base.inst_learning_counter <- inst_state_base.inst_learning_counter +1;
      incr_int_stat 1 inst_num_of_loops;
      inst_lazy_loop_body inst_state;
      Some inst_state_base
    ) else (
      (* reset inst state *)
      (* new inst_learning_bound *)
      let inst_learning_bound = inst_state_base.inst_learning_bound * o.inst_learning_factor in
      incr_int_stat 1 inst_num_of_learning_restarts;
      dbg D_trace @@ lazy (sprintf "inst comp: learning_restart: old inst_learning_bound: %d; new inst_learning_bound: %d"  inst_state_base.inst_learning_bound inst_learning_bound);

      let new_inst_state_base = renew_inst_state_base ~opts:o ~inst_learning_bound ps_input_clauses in

      dbg D_solve (lazy "ps_loop_inst:learning restart"); 
      if Prop_solver_exchange.solve ~reset:false () == PropSolver.Unsat then (
        (* Raise separate exception, solver is not in an invalid state
           and can be satisfiable without assumptions *)
        (* raise PropSolver.Unsatisfiable *)
        raise Unsatisfiable_gr
      );
            
      (* run solve on solver_sim to infer more units in solver_sim *)

      (* KK: clean! *)
      if get_val_stat inst_num_of_learning_restarts >= o.inst_start_prop_sim_after_learn then (
        let solver_sim_result = Prop_solver_exchange.solve ~solver_in:Prop_solver_exchange.solver_sim () in 
        assert (solver_sim_result != PropSolver.Unsat);
        (* solver_sim should not be unsat if solver is not unsat *)
      );

      Some new_inst_state_base
    )
  )



(*-----------*)
let ps_sup_main_fun ~opts sup_state = 
  let o = opts in
  if not o.superposition_flag then 
    None
  else (
    (* dbg D_trace @@ lazy (sprintf "ps_loop_sup: %d" ps_state.ps_sup_cnt); *)
    dbg D_trace @@ lazy ("sup comp");

(* add SMT shared clauses *)
    let smt_shared_clauses = Shared_clauses.smt_get_shared_clauses () in 
    
    BCSet.iter (Superposition.smt_add_clause sup_state) smt_shared_clauses;  
    Shared_clauses.smt_empty_shared_clauses ();
      
    Superposition.step sup_state;
    Some sup_state
  )



(*-----------*)
let ps_sup_extra_main_fun ~opts ~ps_input_clauses ~sup_extra_state_base = 
  let o = opts in
  if not o.superposition_flag then 
    None
  else (
    (* dbg D_trace @@ lazy (sprintf "ps_loop_sup: %d" ps_state.ps_sup_cnt); *)
    try
      dbg D_trace @@ lazy ("sup extra comp");
      if 
        sup_extra_state_base.sup_restart_mult > 1  (* restarts are on *)
        && (
          let num_of_clauses = Superposition.get_num_of_clauses sup_extra_state_base.sup_state in
          num_of_clauses > sup_extra_state_base.sup_restart_bound
        )
      then (  (* restart *)
        (* update bound*)
        Statistics.incr_int_stat 1 sup_num_of_restarts;
        if opts.share_sel_clauses then (
          let share_clauses  = sup_select_scored_clauses ~opts sup_extra_state_base.sup_state in 
          Shared_clauses.add_shared_clause_list share_clauses;
          Shared_clauses.add_shared_clause_list (Superposition.get_extra_clauses sup_extra_state_base.sup_state);
        );
        (* let extra_clauses = Superposition.get_extra_clauses sup_extra_state_base.sup_state in
        let ps_input_clauses_extra = 
          if o.sup_restart_extra && List.X.is_nonempty extra_clauses then
            extra_clauses @ ps_input_clauses
          else
            ps_input_clauses
        in *)

        let sup_restart_bound = sup_extra_state_base.sup_restart_bound * sup_extra_state_base.sup_restart_mult in
        dbg D_sup_restart @@ lazy (sprintf "sup_num_of_restart: %d; sup_restart_bound: %d" (Statistics.get_val_stat sup_num_of_restarts) sup_restart_bound);

        let sup_state = create_sup_state ~scores_frozen:(Some (Superposition.get_scores sup_extra_state_base.sup_state)) ~opts ps_input_clauses in
        let new_sup_extra_state_base = {sup_extra_state_base with 
          sup_state;
          sup_restart_bound
        } in
        Some new_sup_extra_state_base
      )
      else (  (* no restart *)
        Superposition.step sup_extra_state_base.sup_state;
        Some sup_extra_state_base
      )
    with Superposition.Sup_satisfiable m -> 
      dbg D_sup_deep @@ lazy "Sup_satisfiable";

      if sup_extra_state_base.sup_deep_num_discared_cl = 0 then (
        dbg D_sup_deep @@ lazy "sup_deep_num_discared_cl = 0: satisfiable";
        raise (Superposition.Sup_satisfiable m)
      ) else (
        incr_int_stat 1 sup_num_of_deepenings;
        let old_sup_deep_cl_size_coeff = sup_extra_state_base.sup_deep_cl_size_coeff in

        let sup_deep_cl_mult = sup_extra_state_base.sup_deep_cl_mult in 
        let sup_deep_cl_size_coeff = sup_deep_cl_mult * old_sup_deep_cl_size_coeff in
        dbg D_sup_deep @@ lazy (sprintf "sup_deep_cl_size_coeff: %d" sup_deep_cl_size_coeff);

        if opts.share_sel_clauses then (
          let share_clauses  = sup_select_scored_clauses ~opts sup_extra_state_base.sup_state in 
          Shared_clauses.add_shared_clause_list share_clauses;
          Shared_clauses.add_shared_clause_list (Superposition.get_extra_clauses sup_extra_state_base.sup_state);
        );
        (* let extra_clauses = Superposition.get_extra_clauses sup_extra_state_base.sup_state in
        let ps_input_clauses_extra = 
          if o.sup_restart_extra && List.X.is_nonempty extra_clauses then
            extra_clauses @ ps_input_clauses
          else
            ps_input_clauses
        in *)

        let sup_state = create_sup_state ~scores_frozen:(Some(Superposition.get_scores sup_extra_state_base.sup_state)) ~opts ps_input_clauses in
        (* let sup_state = create_sup_state ~opts ps_input_clauses in *)

        let new_sup_extra_state_base = {sup_extra_state_base with 
          sup_state;
          sup_deep_cl_size_coeff;
        } in
        (* let new_sup_extra_state_base = create_sup_exrta_state_base ~opts ~sup_deep_cl_mult ~sup_deep_cl_size_coeff ~ps_input_clauses in *)
        sup_extra_assign_hook_passive new_sup_extra_state_base ps_input_clauses; 
           
        Some new_sup_extra_state_base
      )
    | x -> raise_trace x  (* if e.g. unsat then we just raise it again *)
  )



(*-------------*)
(*
let ps_init_fun ps_state = 

  print_options_if_new ~opts:ps_state.ps_opts;  
  out_str "\n";
  print_string ((s_pref_str ())^"Proving...\n");
  flush stdout
*)
      
let ps_comp_main_fun ~comp ~ps_input_clauses = 
  comp_record_time comp @@ fun () -> 

  let new_comp_base_opt = 
    match comp.comp_base with 
    | Res(res_state) ->        
        Option.lift (fun x -> Res(x)) 
          (ps_res_main_fun ~opts:comp.comp_opts res_state)
          
    | Inst(inst_state_base) -> 
        Option.lift (fun x -> Inst(x)) 
          (ps_inst_main_fun ~comp_cnt:comp.comp_cnt ~opts:comp.comp_opts ~ps_input_clauses ~inst_state_base)
          
    | Sup(sup_state) -> 
        Option.lift (fun x -> Sup(x))   
          (ps_sup_main_fun ~opts:comp.comp_opts sup_state)
          
    | Sup_extra (sup_extra_state_base) -> 
        Option.lift (fun x -> Sup_extra(x))   
          (ps_sup_extra_main_fun ~opts:comp.comp_opts ~ps_input_clauses  ~sup_extra_state_base)
  in
  Option.lift (fun new_comp_base -> 
    comp.comp_base <- new_comp_base;
    comp.comp_cnt <- comp.comp_cnt + 1;
    comp      
  ) new_comp_base_opt


(*------ ps_comp_deficit based on generated clauses ---------*)
  
(* deficits based on number of clauses in components vs total number of clauses *)

let get_comp_base_num_of_clauses comp_base =
  try
    match comp_base with 
    |Res(_res_state) -> 
        Statistics.get_val_stat_fun res_num_of_clauses
    |Inst(_inst_state_base) -> 
        Statistics.get_val_stat_fun inst_num_of_clauses
    |Sup(sup_state) -> 
        Superposition.get_num_of_clauses sup_state
(*        Statistics.get_val_stat_fun sup_num_of_clauses *)

    |Sup_extra(sup_extra_state_base) -> (* TODO: different stat ?: the same stat in both sup comps *)
        Superposition.get_num_of_clauses sup_extra_state_base.sup_state
(*        Statistics.get_val_stat_fun sup_num_of_clauses *)
        
  with _-> 0

let get_comp_num_of_clauses comp = get_comp_base_num_of_clauses comp.comp_base

let get_total_num_of_clauses components =
  List.fold_left (fun rest comp -> rest + (get_comp_num_of_clauses comp)) 0 components

let get_comps_mult_sum components = 
  List.fold_left (fun rest comp -> rest + comp.comp_mult) 0 components

(* does not assign def. to comp *)  

let ps_comp_deficit_cl_based (* ~total_num_cl *) ~ps_state ~comp = 
  let comps_mult_sum = get_comps_mult_sum ps_state.ps_components in
  let total_num_cl = get_total_num_of_clauses ps_state.ps_components in  (* shift *)
  let comp_num_of_cl = get_comp_num_of_clauses comp in 

  let comp_deficit = ((comp.comp_mult * total_num_cl)/comps_mult_sum) - comp_num_of_cl in

  dbg D_trace @@ lazy (sprintf "total cl: %d; comp_cl: %d comp name: %s; comp_deficit: %d" total_num_cl comp_num_of_cl comp.comp_name comp_deficit);
  comp_deficit


(*------------ share clauses --------*)

let share_clauses_comp ~opts comp = 
  let share_clauses = 
    match comp.comp_base with (* only for superposition at the moment *)
    | Sup sup_state -> 
      sup_select_scored_clauses ~opts sup_state 
    | Sup_extra sup_extra_state_base -> 
      sup_select_scored_clauses ~opts sup_extra_state_base.sup_state 
    | _ -> []
  in
  Shared_clauses.add_shared_clause_list share_clauses
    
let share_clauses_comps ~opts comps = List.iter (share_clauses_comp ~opts) comps

(*------- prop_solver_per_cl_check --------*)
let prop_solver_per_cl_check ~opts = 
  if (opts.prop_solver_per_cl > 0 && 
      ((Statistics.get_val_stat Statistics.prop_num_of_clauses) >= 
      (opts.prop_solver_per_cl * (Statistics.get_val_stat_fun Statistics.prop_solver_calls))))
  then
    (
     dbg D_trace (lazy ("prop_solver call"));

     if Prop_solver_exchange.solve ~reset:false () == PropSolver.Unsat
     then
      (* Raise separate exception, solver is not in an invalid state
	 and can be satisfiable without assumptions *)
      (* raise PropSolver.Unsatisfiable *)
      raise Unsatisfiable_gr
    )  
  else
    ()

(*--------- ps_comp_funs_cl_based  ---------*)

let ps_pre_comp_fun ps_state = 
  (if (List.X.is_empty ps_state.ps_components) 
  then failwith "proof_search_loop: empty component list"
  );
  check_sched_time ~start_time:ps_state.ps_start_time ~time_limit:ps_state.ps_time_limit ps_state.ps_cnt;   
  prop_solver_per_cl_check ~opts:ps_state.ps_opts 

let ps_comp_main_fun ps_state comp = ps_comp_main_fun ~comp:comp ~ps_input_clauses:ps_state.ps_input_clauses

let ps_stagnation_fun ps_state comp = 
  let new_components =  ps_state.ps_components |>  List.map (ps_comp_main_fun ps_state)  |>  Option.filter_some in
  ps_state.ps_components <- new_components
  
  
let ps_comp_funs_cl_based = 
  {
   ps_pre_comp_fun;
   ps_post_comp_fun = (fun _ -> ());
   ps_comp_main_fun;
   ps_comp_deficit_fun = (fun ps_state comp ->ps_comp_deficit_cl_based ~ps_state ~comp);
   ps_stagnation_fun;
 }


(*============== alternative: time-based deficit =======*)

let time_to_int time = int_of_float (100000. *. time)  (* time in milliseconds *) 

let get_comp_time comp = time_to_int comp.comp_time
  

(*
let get_comp_base_time comp_base =
  assert Statistics.time_global_flag;

  match comp_base with 
  |Res(_res_state) -> 
      time_to_int (Statistics.get_float_val_stat res_time_total)
  |Inst(_inst_state_base) -> 
      time_to_int (Statistics.get_float_val_stat inst_time_total)
  |Sup(_sup_state) -> 
      time_to_int (Statistics.get_float_val_stat sup_time_total)

  |Sup_extra(_sup_extra_state_base) -> (* TODO: !!! different stat !!! *)
      time_to_int (Statistics.get_float_val_stat sup_time_total)

let get_comp_time comp_base = comp.comp_base

*)
 
let get_total_comps_time components = (* should not be overall total time but sum of comp. time *)
  List.fold_left (fun rest comp -> rest + (get_comp_time comp)) 0 components
    

let ps_comp_deficit_time_based (* ~total_num_cl *) ~ps_state ~comp = 
  let comps_mult_sum = get_comps_mult_sum ps_state.ps_components in
  let total_time = get_total_comps_time ps_state.ps_components in  (* shift *)
  let comp_time = get_comp_time comp in 

  let comp_deficit = ((comp.comp_mult * total_time)/comps_mult_sum) - comp_time in

  dbg D_trace @@ lazy (sprintf "total time: %d; comp_time: %d comp name: %s; comp_deficit: %d" total_time comp_time comp.comp_name comp_deficit);
  comp_deficit

let ps_comp_funs_time_based = 
  {
   ps_pre_comp_fun;
   ps_post_comp_fun = (fun _ -> ());
   ps_comp_main_fun;
   ps_comp_deficit_fun = (fun ps_state comp -> ps_comp_deficit_time_based ~ps_state ~comp);
   ps_stagnation_fun; (* stagnation should not happen with time ;) *)
 }


(*============== ps_loop_main =============*)

let ps_stagnation_limit = 1000

let _ = out_warning ("proof_search_loop: ps_stagnation_limit:"^(string_of_int ps_stagnation_limit))

(* ps_loop_main: in a loop, from component list pick one with the maximal deficit and execute comp_main_fun *)
(* all *_fun are ps_state -> unit *)
(* comp_list -- component list *)

let ps_loop_main ~time_limit ~ps_comp_funs ~opts input_clauses = 

(*  input_clauses are empty *)

  (if (List.X.is_empty input_clauses.ps_clauses_no_eq_axioms)
  then  raise (Resolution_loop.Res_satisfiable (BCMap.empty)));

  print_options_if_new ~opts;  
  out_str "\n";
  print_string ((s_pref_str ())^"Proving...\n");
  flush stdout;
    
  let ps_start_time = (Unix.gettimeofday ()) in  

  (* TODO: rework !!! *)
  if opts.sup_symb_ordering == Random then Lib.re_init_rnd_bits ();

  let comp_list = create_comp_list ~opts ~ps_input_clauses:input_clauses in
  
  let ps_state = 
    create_ps_state ~ps_components:comp_list ~ps_input_clauses:input_clauses ~ps_start_time ~ps_time_limit:time_limit ~ps_opts:opts ~ps_stagnation_limit in
  
  try
    while true 
    do
    
      ps_comp_funs.ps_pre_comp_fun ps_state;
      
      let comp_dfc_list = List.map (fun comp -> ((ps_comp_funs.ps_comp_deficit_fun ps_state comp), comp)) ps_state.ps_components in 
      
      
(*
    let (max_dfc, max_dfc_comp) = list_find_max_element 
  (fun (dfc1,_comp1) (dfc2,_comp2) ->  Poly.compare dfc1 dfc2)
  comp_dfc_list
  in 
*)
      
      let max_dfc_comps = list_find_all_max_elements 
        (fun (dfc1,_comp1) (dfc2,_comp2) -> Int.compare dfc1 dfc2)
        comp_dfc_list
      in       
      
      assert(max_dfc_comps != []);
      
      let f (max_dfc, max_dfc_comp) =
        dbg D_trace (lazy (sprintf "ps_cnt: %d; max_def_comp: %s; max deficit: %d" ps_state.ps_cnt max_dfc_comp.comp_name max_dfc));
        
        
        (if max_dfc_comp.last_deficit = max_dfc
        then 
        max_dfc_comp.stagnation_cnt <- max_dfc_comp.stagnation_cnt +1
      ); 
      begin
        if (max_dfc_comp.stagnation_cnt = ps_state.ps_stagnation_limit) 
        then 
          (
           dbg D_stagnation @@ lazy (sprintf "ps_loop_main: comp: %s; ps_stagnation_limit reached: %d; running ps_stagnation_fun" max_dfc_comp.comp_name ps_state.ps_stagnation_limit);      
(*            let _ = out_warning (sprintf "ps_loop_main: comp: %s; ps_stagnation_limit reached: %d; running ps_stagnation_fun" max_dfc_comp.comp_name ps_state.ps_stagnation_limit) in *)
            max_dfc_comp.stagnation_cnt <-0;

            ps_comp_funs.ps_stagnation_fun ps_state max_dfc_comp
           )
        else
          (
           max_dfc_comp.last_deficit <- max_dfc;
           
           (* run ps_comp_main_fun on max_dfc_comp and filter out if the resulting new comp. is None *)  
           let new_components = List.filter_map 
               (fun comp -> 
                 if comp == max_dfc_comp then 
                   ps_comp_funs.ps_comp_main_fun ps_state comp
                 else Some(comp)
               ) ps_state.ps_components 
           in  
           ps_state.ps_components <- new_components;
           ps_comp_funs.ps_post_comp_fun ps_state   
          )
      end;
    in
    List.iter f max_dfc_comps; 
    ps_state.ps_cnt <- ps_state.ps_cnt+1;
    done
  with 
    PS_loop_time_out cnt -> 
      (
       if opts.share_sel_clauses then
         share_clauses_comps ~opts ps_state.ps_components
      ); 
      raise (PS_loop_time_out cnt)



(*--------*)

(* TODO add deficit based on given_cl iterations *)

let ps_full_loop ~opts ~time_limit input_clauses =
  match opts.comb_mode with 
  |Options.Comb_clause_based ->   
      ps_loop_main ~time_limit ~ps_comp_funs:ps_comp_funs_cl_based ~opts input_clauses  

  |Options.Comb_time_based -> 
      ps_loop_main ~time_limit ~ps_comp_funs:ps_comp_funs_time_based ~opts input_clauses  
