(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2016 Konstantin Korovin and The University of Manchester. 
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
open Proof_search_loop
open Problem_properties

type res_model = Resolution_loop.res_model
type sup_model = Superposition.sup_model
type inst_pre_model = Instantiation_env.inst_pre_model

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"	

let dbg_groups =
  [
   D_trace 
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

(* setting hard time limit is problematic since the SAT solver can be interrupted *)
exception Schedule_Terminated

type time = float param

type schedule = (named_options * time) list

let time_to_string time =
  match time with
  | Def(float) -> string_of_float float
  | Undef -> "Unbounded"
	
type schedule_clauses = 
    {

(* clauses used for proof search *)
     mutable proof_clauses_with_eq_axioms : clause list; 

     mutable proof_clauses_no_eq_axioms : clause list; 

     mutable problem_properties : Problem_properties.prob_props;

(* clauses used for finite model search  (equality may be missing) *)
     mutable finite_model_clauses : clause list;  

(* clauses removed by sem_filter; needed for model building*)
     mutable filtered_out_inst_pre_model : inst_pre_model;  

   }

(* to proof search clauses *)
let schedule_clauses_to_ps_clauses sch_clauses = 
  {
   ps_clauses_with_eq_axioms = sch_clauses.proof_clauses_with_eq_axioms;
   ps_clauses_no_eq_axioms = sch_clauses.proof_clauses_no_eq_axioms;
   ps_problem_properties = sch_clauses.problem_properties;
 }


(* DT multi-pred parameter *)
let bmc1_multi_unsat_stop_prop = true

(* let _ = out_str ("DEFAULT: stop_after_prop_unsat = "^(string_of_bool bmc1_multi_unsat_stop_prop)) *)

(*----------- result handlers --------------------*)

(*
let szs_pref = "\n\n% SZS status "

let unknown_str () = szs_pref^"Unknown\n"
*)


(* hack *)
type store_incomplete_mode =
    {
     sat_inc_mode : bool;
     unsat_inc_mode : bool; 
   }

let store_current_inc_mode () = 
  {
   sat_inc_mode = !sat_incomplete_mode;
   unsat_inc_mode = !unsat_incomplete_mode; 
 }

let restore_inc_mode inc_mode =
  sat_incomplete_mode := inc_mode.sat_inc_mode;
  unsat_incomplete_mode := inc_mode.unsat_inc_mode

(* assign incomplete mode based on options 
   this is not exhaustive and currently very simple, so it is still upto user to ensure options are complete
 *)
 
let assign_incomplete_mode opts schedule_clauses =
  if Stdlib.(opts.demod_completeness_check = Options.Demod_check.Off)
  || List.X.mem ~eq:(==) Options.SupSimplificationSetup.FwConnectedness opts.sup_simplification_setup.full_fw
  || Problem_properties.(schedule_clauses.problem_properties.has_is_int_rat)
  then (
    sat_incomplete_mode := true
  );
  (* TODO extend*)

  if !sat_incomplete_mode then
    out_warning "sat_incomplete_mode true";
  
  if !unsat_incomplete_mode then
    out_warning "unsat_incomplete_mode true"
  
      
      
    
    
(* "PROVED\n" *)
let proved_str () =
  if !unsat_incomplete_mode 
  then 
     (
      out_str ("% shown unsat in suppress_unsat_res mode ");
      szs_unknown_str ();
     )
  else
    let input_problem_type = (get_some !Parser_types.input_problem_type) in
    if (input_problem_type == Parser_types.FOF || input_problem_type == Parser_types.TFF)
    && (get_val_stat num_of_input_neg_conjectures > 0)
    then
      szs_theorem_str ()
    else
      szs_unsat_str ()

(* "SATISFIABLE\n" *)
let satisfiable_str () =
  if !sat_incomplete_mode 
  then 
     (
      out_str ("% shown sat in suppress_sat_res mode ");
      szs_unknown_str ();
     )
  else
    if (get_some !Parser_types.input_problem_type) == Parser_types.FOF 
    && (get_val_stat num_of_input_neg_conjectures > 0)
    then
      (
       dbg D_trace @@ lazy (sprintf "satisfiable_str: num_of_input_neg_conjectures %d" (get_val_stat num_of_input_neg_conjectures));
       szs_counter_sat_str ()
      )
    else
      szs_sat_str ()




(*-------------- results after running a proof scehdule ----------------------------------*)

type ps_result = 
  | PS_result_empty_clause of clause 
  | PS_result_resolution_sat of res_model * inst_pre_model (* res_model * filtered_out_inst_pre_model *)
  | PS_result_prop_solver_unsat 
  | PS_result_prop_solver_unsat_na (* unsat without assumtions *)
  | PS_result_instantiation_sat of inst_pre_model * inst_pre_model  (* inst_pre_model * filtered_out_inst_pre_model *)
  | PS_result_unsat_multiple_cores of UnsatCore.unsat_core list
  | PS_result_superposition_sat of sup_model * inst_pre_model
  | PS_result_smt_unsat_na of clause list
  | PS_result_smt_sat_na of unit

	
(* result_handler_preprocess: if parsing/preprocessing already returns a result *)

(*let result_handler_basic_preprocess result = *)

(* put out statistics before *)

let process_empty_clause ~opts empty_clause = 
  dbg D_trace @@ lazy "process_empty_clause";

  let open TstpProof in 
  if opts.proof_out then	 
    begin
	 
     out_str (szs_start_cnfrefutation ());
     
     
     let proof = get_proof (clauses_to_pf [empty_clause]) in

     (* Proof output *)
     (Format.printf "%a@." pp_tstp_proof proof);

     out_str (szs_end_cnfrefutation ());

     let proof_graph = get_proof_graph proof in 

     (if Stdlib.(not (opts.proof_dot_file = ""))
     then
       (
        (if (List.X.is_nonempty opts.proof_reduce_dot) 
        then 
          (let filter_clause = Clause.proof_reduce_dot_list_to_filter opts.proof_reduce_dot in 
          let filter pf = 
            match pf with 
            |Ext_formula _ -> false
            |Int_clause c -> filter_clause c
          in
          let reduced_proof_graph = reduce_proof_graph ~filter ~proof_graph (get_roots proof) in 
          out_proof_dot opts.proof_dot_file reduced_proof_graph
          )
        else 
          (out_proof_dot opts.proof_dot_file proof_graph)
        )
       )
     );
     (if opts.interactive_mode
     then 
       (* TODO add proof with clausifier *)
       (
        let proof_clauses = proof_to_list_cls proof in                  
        Sockets.(send_proof (get_external_connection ()) proof_clauses)            
       )
     )
    end
  else ()

let out_szs_status ~opts szs_str =
  
  out_str (sprintf "\n\n%s\n" szs_str);

  (if opts.interactive_mode
  then 
    Sockets.(send_szs_result (get_external_connection ()).en_connection szs_str)
  )

let result_handler_basic ~opts result = 
(* disable_timeouts is need for proof output when run with short timeouts in ML scripts *)
  dbg D_trace @@ lazy "result_handler_basic";
  Signals.disable_time_outs (); 
  match result with 
  | PS_result_empty_clause empty_clause -> 
      (

       (* Output status *)
       out_szs_status ~opts (proved_str ());
        
       (* in this case the unsat is already without answer clauses *)
       if !answer_mode_ref then
         szs_out_answer ~answer_list:[];
       
       process_empty_clause ~opts empty_clause 
      
      )

  | PS_result_prop_solver_unsat | PS_result_prop_solver_unsat_na  ->
      (

       (* Output SZS status *)

       out_szs_status ~opts (proved_str ());
   
       (* Output backtrace of unsatisfiable exception *)
       ( if opts.dbg_backtrace
       then
	 Format.eprintf
	   "Unsatisfiable exception raised: after main.@\nBacktrace:@\n%s@\n@."
	   (Printexc.get_backtrace ());
	);
       
       (* Output answer first *)
       (if !answer_mode_ref then
         (
          let answer_list =  Prop_solver_exchange.get_answer () in
          szs_out_answer ~answer_list 
         )
       );
        
       (
	 if opts.proof_out
	 then
	   (
	    
            dbg D_trace @@ lazy "result_handler_basic: brefore: unsat_core ";
	    (* Get unsatisfiable core *)
            let uc = Prop_solver_exchange.get_unsat_core ~soft:false () in
            let unsat_core_clauses = UnsatCore.get_clauses uc in

            dbg D_trace @@ lazy "result_handler_basic: brefore: empty_clause";

(* create the empty clause *)
            let tstp_source = Clause.tstp_source_prop_impl_just unsat_core_clauses in
            let empty_clause = create_clause ~check_empty:false tstp_source [] in 
            dbg D_trace @@ lazy "result_handler_basic: brefore: empty_clause";
            process_empty_clause ~opts empty_clause 
            
           );
       );
           
      )

  | PS_result_unsat_multiple_cores _ ->
      out_str "Multiple usat cores generated during instantiation";

  | PS_result_resolution_sat (res_model, filtered_out_inst_pre_model)
    ->
      
      (* Output SZS status *)
      
      out_szs_status ~opts (satisfiable_str ());
        
      if opts.sat_out_model != Model_None
      || opts.sat_out_clauses
      then
         Model_res.out_res_model ~res_model:res_model ~filtered_out_inst_pre_model
      else ()
          
           
  | PS_result_instantiation_sat (all_clauses, filtered_out_clauses)
    ->
      (* Output SZS status *)
   
      out_szs_status ~opts (satisfiable_str ());         

       if opts.sat_out_model != Model_None
       then
         let model =
           Model_inst.build_model ~inst_pre_model:all_clauses  ~inst_pre_model_filtered_out:filtered_out_clauses
         in
         Model_inst.out_model ~sat_out_model_opt:opts.sat_out_model model
       else ()

  | PS_result_superposition_sat (sup_model, filtered_out_inst_pre_model)->

      out_szs_status ~opts (satisfiable_str ());
 
      if opts.sat_out_model != Model_None
      || opts.sat_out_clauses
      then
       Model_res.out_sup_model ~sup_model ~filtered_out_inst_pre_model
      else ()

  | PS_result_smt_unsat_na clauses ->
     
     out_szs_status ~opts (proved_str ());
     
    if !answer_mode_ref then (
      ()
    );
      if opts.proof_out
      then
	(
         let smt_state = !GlobalSMT.state in
         let smt_clauses = List.map (SMTSolver.clause_to_smt smt_state) clauses in
         let smt_problem = SMTSolver.Uc.make_problem smt_state in
         let smt_tags = SMTSolver.Uc.add_many smt_problem smt_clauses in
         let smt_result = SMTSolver.Uc.check smt_problem in
         assert (smt_result == SMTSolver.Unsat);
         
         let rec loop unsat_core_tags clauses tags =
           match clauses, tags with
           | [], [] -> []
           | hc::tc, ht::tt ->
               if List.X.mem ~eq:SMTSolver.Uc.tag_equal ht unsat_core_tags then                 
                 hc :: loop unsat_core_tags tc tt
               else
                 loop unsat_core_tags tc tt
           | [], _::_ | _::_, [] -> assert false
         in
         
         let unsat_core_tags = SMTSolver.Uc.unsat_core smt_problem in
         
         dbg D_trace @@ lazy (sprintf "SMT Unsat core: before loop: unsat_tags: %i; is_empty:%b" (List.length unsat_core_tags) (List.X.is_empty smt_tags));
         
         let unsat_clauses =
           if (not (List.X.is_empty unsat_core_tags)) then 
             loop unsat_core_tags clauses smt_tags
           else (* KK: check when this happens *)
             clauses
         in
         (* printf "Unsat core:\n%s\n"
      (List.X.to_string ~first:"" ~last:"" ~sep:"\n" Clause.to_string_tptp unsat_clauses) *)

         dbg D_trace @@ lazy (sprintf "SMT Unsat core: unsat_tags: %d unsat_core_clauses %d of %d" (List.length  unsat_core_tags) (List.length unsat_clauses) (List.length clauses));

(* create the empty clause *)
         let tstp_source = Clause.tstp_source_smt_impl_just unsat_clauses in
         let empty_clause = create_clause ~check_empty:false tstp_source [] in
         process_empty_clause ~opts empty_clause                    
        )


  | PS_result_smt_sat_na _ ->
    out_szs_status ~opts (satisfiable_str());

    ()  (* TODO model *)





let reset_external_score_cache () = 
  if (Sockets.external_is_connected ()) 
  then 
    (
     out_str "External: resetting score cache\n ";
     Sockets.(reset_score_cache (get_external_connection ()))
    )
  else 
    ()
      
	

(*-------- schedule run with exceptions -------------------------------------------*)
(*-------- later all proving exceptions are caught and converted to results -------*)

(* prover_functions by default are Undef, in some cases we want to continue with the same or modified externaly prover state which is represented by prover functions *)


let rec schedule_run_exceptions (* ?(prover_functions_param=Undef)*) schedule_clauses schedule =

  reset_external_score_cache ();
  
  (* (if !current_options.reset_solvers        *)
  (* then                                      *)
  (*   (Prop_solver_exchange.reset_solvers ()) *)
  (* );                                        *)
  match schedule with
  | (named_options, time_limit) :: tl ->
      let opts = named_options.options in
      (* TODO restore old incomplete mode in each iteration *)
      let _old_inc_mode = store_current_inc_mode () in
      assign_incomplete_mode opts schedule_clauses;      
  
      if (opts.sat_mode && opts.sat_finite_models)
      then 
        begin
      (* current_options:= named_options.options; *)

(*KK !remove! after rm: current_options *)	

(*	set_new_current_options named_options.options; *)

(*	init_sched_time time_limit; *)

	out_str ((s_pref_str ())^named_options.options_name
		 ^" Time Limit: "^(time_to_string time_limit)^"\n\n");
	
(*
	print_string ((s_pref_str ())^named_options.options_name
		      ^" Time Limit: "^(time_to_string time_limit)^"\n"^
		      (options_to_str !current_options)^"\n\n"
        ^(s_pref_str ())^"\n");
	flush stdout;
*)
	Finite_models_loop.finite_models_loop ~opts:(copy_options opts) schedule_clauses.finite_model_clauses
      end
    else 
      begin

	(if not (opts.prolific_symb_bound = named_options.options.prolific_symb_bound)
	then 
	  (Problem_properties.change_prolific_symb_input schedule_clauses.proof_clauses_with_eq_axioms;)
	);

      (* current_options:= named_options.options; *)

(* KK  rm after lcl options *)
(*	set_new_current_options named_options.options; *)
	out_str ((s_pref_str ())^named_options.options_name
		 ^" Time Limit: "^(time_to_string time_limit)^"\n\n");
(*
	print_string ((s_pref_str ())^named_options.options_name
		      ^" Time Limit: "^(time_to_string time_limit)^"\n"^
		      (options_to_str !current_options)^"\n\n"
		      ^(s_pref_str ())^"Proving...");
	flush stdout;
*)

      (* debug *)
	dbg D_trace (lazy ("In Schedule: input size "^(string_of_int (List.length schedule_clauses.proof_clauses_with_eq_axioms))));
      (* Clause.out_clause_list !input_clauses_ref; *)
      
      (* !current_options.out_options <- Out_All_Opt;                             *)
      (* out_str ("\n current options: "^(options_to_str !current_options)^"\n"); *)
      
(*	init_sched_time time_limit; *)

(*
	let new_prover_functions = 
	  match prover_functions_param with 
	  |Def(pf)  ->  pf (* continue with the old ones *)
	  |Undef -> Proof_search_loop.create_provers_current_options schedule_clauses.proof_clauses in
*)
	try
	 (
(*
      if List.X.is_nonempty opts.abstr_ref_under
      then
        Abstr_ref_under.solve ~opts:(copy_options opts) ~time_limit opts.abstr_ref_under schedule_clauses.proof_clauses_with_eq_axioms
      else(
        if List.X.is_empty opts.abstr_ref
        then Proof_search_loop.ps_full_loop ~opts:(copy_options opts) ~time_limit (schedule_clauses_to_ps_clauses schedule_clauses)
        else Abstr_ref.solve ~opts:(copy_options opts) ~time_limit opts.abstr_ref schedule_clauses.proof_clauses_with_eq_axioms
      )
*)
    let ar_cls = schedule_clauses.proof_clauses_with_eq_axioms in
    if (list_non_empty opts.abstr_ref) && (list_non_empty opts.abstr_ref_under) && (list_non_empty ar_cls) then
      Ar_combined_approx.solve (copy_options opts) time_limit ar_cls
    else if (list_non_empty opts.abstr_ref) && (list_non_empty ar_cls) then
      Ar_over_approx.solve (copy_options opts) time_limit ar_cls
    else if (list_non_empty opts.abstr_ref_under) && (list_non_empty ar_cls) then
      Ar_under_approx.solve (copy_options opts) time_limit ar_cls
    else
      Proof_search_loop.ps_full_loop ~opts:(copy_options opts) ~time_limit
        (schedule_clauses_to_ps_clauses schedule_clauses)

           (* if (!current_options.abstr_ref = []) 
            * then 
            *   (
            *     Proof_search_loop.ps_full_loop ~time_limit (schedule_clauses_to_ps_clauses schedule_clauses);
            *   )
            * else
            *   (Abstr_ref.solve ~time_limit !current_options.abstr_ref schedule_clauses.proof_clauses_with_eq_axioms;) *)

(*
           if !current_options.abstr_ref_arg_filter
           then
             (Abstr_ref_arg_filter.abstr_ref_gr_filter_loop ~time_limit schedule_clauses.proof_clauses;)
           else 
           
             if (!current_options.abstr_ref_sig || !current_options.abstr_ref_subs)
             then
               (Axiom_selection.axiom_selection_loop ~time_limit schedule_clauses.proof_clauses;)
             else
               (Proof_search_loop.ps_full_loop ~time_limit schedule_clauses.proof_clauses;)
*)
	  )
	with
	| Proof_search_loop.PS_loop_time_out(full_loop_counter) ->
            out_str ("Proof_search_loop: time out after: "^(string_of_int full_loop_counter)^" full_loop iterations\n");
(* TODO
            schedule_clauses.proof_clauses <- 
	      Proof_search_loop.simplify_input new_prover_functions schedule_clauses.proof_clauses;
*)
         (*   out_str (" \n\n commented: schedule_clauses.sat_clauses <- simplify_input prover_functions schedule_clauses.sat_clauses \n\n");	  *) 
        (* TODO: check why does not work  *)
(*         finite_model_clauses_ref  := simplify_input prover_functions !finite_model_clauses_ref; *)

       (*     provers_clear_and_remove_all (); *)
(*            Proof_search_loop.clear_all_provers new_prover_functions;*)
            clear_memory ();
            schedule_run_exceptions schedule_clauses tl

      (* One should be careful here,                     *)
      (* since if Inst.  Satisfiable the model is passed *)
      (* and resolution empty clause, proof  is passed, clearing provers should not *)
      (* destroy models/proofs (at the moment it does not) *)
      | Ar_common.AR_time_out -> 
          out_str ("AR_time_out");
(* KK: TODO: clean up to remove only AR assumptions form the current run *)
          Prop_solver_exchange.clear_solver_assumptions (); 
          clear_memory ();
          schedule_run_exceptions schedule_clauses tl
      | x ->
        (* cannot clear since exception can return context as model or unsat core *)
        (* provers_clear_and_remove_all ();    *)
        (* clear_all_provers prover_functions; *)
          (* For some reason backtraces only appear if I uncomment the line below *)
(*          out_str ("exception: "^(Printexc.to_string x));
          Printexc.print_backtrace stderr; 
*)
          raise_trace x
      end

  | [] -> raise Schedule_Terminated

let try_prop_solve () =
  (* try in assumpiton of the multiple unsat cores *)
  let rec try_solve_unsat () =
    Prop_solver_exchange.clear_soft_assumptions ();
    if Prop_solver_exchange.solve () == PropSolver.Unsat
    then (  (* collect unsat core and re-run *)
      Prop_solver_exchange.process_unsat_result ();
      try_solve_unsat ()
    )
    else (* SAT: return if we have unsat cores collected *)
      if bmc1_multi_unsat_stop_prop && (list_non_empty !Prop_solver_exchange.unsat_cores)
      then
        raise (Prop_solver_exchange.MultipleUnsat !Prop_solver_exchange.unsat_cores)
  in
  (* both multi-unsat and unsat are covered by this *)
  try_solve_unsat ()

(* schedule_run should not raise exceptions *)
let schedule_run (* ?(prover_functions_param = Undef)*)  schedule_clauses schedule = 
  try 

(*
    dbg D_trace (lazy ("schedule_run before solver"));

    List.iter Prop_solver_exchange.add_clause_to_solver schedule_clauses.proof_clauses_with_eq_axioms; 
    try_prop_solve ();
  
    dbg D_trace (lazy ("schedule_run after solver"));  
*)

    schedule_run_exceptions (* ~prover_functions_param *) schedule_clauses schedule; 

    failwith "schedule_run_exceptions: should happen"

  with 
        (* Unsatisfiable in propositional solver, not by empty clause in
       resolution *)
(*  | Prop_solver_exchange.Unsatisfiable

  | PropSolver.Unsatisfiable
  | Instantiation.Unsatisfiable ->
*)
      (* |Discount.Unsatisfiable *)

  | Unsatisfiable_gr       
    ->
      assert (Prop_solver_exchange.soft_assumptions_is_empty ());
      PS_result_prop_solver_unsat

  | Unsatisfiable_gr_na | Finite_models.Unsatisfiable_fm_na
    ->
(*      assert (Prop_solver_exchange.soft_assumptions_is_empty ()); *)
      PS_result_prop_solver_unsat_na

  | Prop_solver_exchange.MultipleUnsat unsat_cores ->
      PS_result_unsat_multiple_cores unsat_cores
     
  | Empty_clause (clause) ->      
      PS_result_empty_clause (clause)
      
	
  | Resolution_loop.Res_satisfiable res_model ->
      PS_result_resolution_sat (res_model, schedule_clauses.filtered_out_inst_pre_model)
      
	
  | Instantiation_loop.Inst_satisfiable inst_pre_model ->      
      PS_result_instantiation_sat (inst_pre_model, schedule_clauses.filtered_out_inst_pre_model)

  | Superposition.Sup_satisfiable sup_model ->
      PS_result_superposition_sat (sup_model, schedule_clauses.filtered_out_inst_pre_model) 
    
      	

(*--------------------------------------------------------*)

let is_large_theory ~opts problem_props =
  problem_props.clauses > opts.lt_threshold

let schedule_to_many_axioms_schedule ~opts problem_props schedule =
  if (is_large_theory ~opts problem_props)
      && ( problem_props.conjectures > 0)
  then
    (out_str (pref_str^"Large theory schedule adaptation \n");
     let f (opt, time) = ((Options.named_opt_to_many_axioms_named_opt1 opt), time)
     in List.map f schedule
    )
  else
    schedule

let strip_conj_schedule problem_props schedule =
  if (problem_props.conjectures = 0) 
  then
    (
     out_str (pref_str^"no conjectures: strip conj schedule \n");
    let f (opt, time) = ((Options.strip_conj_named_opt opt), time)
    in List.map f schedule
    )
  else schedule

let sup_off_non_eq problem_props schedule = 
  if (has_eq problem_props) then schedule 
  else
    (
     (* schedule  *)
     out_str (pref_str^"no equalities: superposition off \n");
     List.map (fun (named_opt,time) -> (
      {named_opt with options = Options.{named_opt.options with 
(* reduce superposition to minimum *)
                                         comb_sup_mult = if named_opt.options.comb_sup_mult > 0 then 1 else 0; 
                                         comb_sup_deep_mult = if named_opt.options.comb_sup_deep_mult > 0 then 1 else 0; 
                               (*  Options.superposition_flag = false *)
                               }}, time)) schedule
(*
out_str (pref_str^"no equalities: superposition off \n");
    List.map (fun (named_opt,time) -> ({named_opt with options = {named_opt.options with Options.superposition_flag = false}}, time)) schedule
*)
    )
     
let res_off_pure_eq problem_props schedule = 
  if not (problem_props.lits_eq = problem_props.lits) then schedule 
  else
    ( (* KK commented for now *)
(*
     out_str (pref_str^"pure equality problem: resolution off \n");
     List.map (fun (named_opt,time) -> 
       ({named_opt with 
         options = {named_opt.options with Options.resolution_flag = false}}, time)) schedule
*)
      schedule
    )
     
let inst_res_sat_off_ueq problem_props schedule = 
  if (problem_props.lits = problem_props.clauses) && (has_eq problem_props) then  
    (
(*
out_str (pref_str^"ueq problem: resolution/instantiation/sat off \n");
     List.map 
       (fun (named_opt,time) -> 
         ({named_opt with options = {named_opt.options with 

                                     Options.resolution_flag = true;
                                     Options.instantiation_flag = true;
                                     
                                     Options.prop_solver_per_cl = 0;
                                     Options.superposition_flag = true;
                                     Options.sup_to_prop_solver = Options.Sup_to_Solver_None;
                                     Options.sup_prop_simpl_new = false;
                                     Options.sup_prop_simpl_given = false;
                                   }}, time)) 
*)
       schedule
    )
      
  else
    schedule
(*
let is_large_theory () =
  (get_val_stat num_of_input_clauses) > !current_options.lt_threshold


(*--------------------------------------------------------*)

let schedule_to_many_axioms_schedule schedule =
  if (is_large_theory ())
      && (get_val_stat num_of_input_neg_conjectures > 0)
  then
    (out_str (pref_str^"Large theory \n");
     let f (opt, time) = ((Options.named_opt_to_many_axioms_named_opt1 opt), time)
     in List.map f schedule
    )
  else
    schedule


let strip_conj_schedule schedule =
  if (get_val_stat num_of_input_neg_conjectures = 0)
  then
    let f (opt, time) = ((Options.strip_conj_named_opt opt), time)
    in List.map f schedule
  else schedule

let sup_off_non_eq eq_flag schedule = 
  if eq_flag then schedule 
  else
    List.map (fun (opt,time) -> ({opt with Options.superposition_flag = false}, time)) schedule
*)



(* Aux function *)
let[@inline] add_ord flag n o = 
  if flag then 
    { o with options = { o.options with sup_ordering = TheoryN(n) } } 
  else 
    if n = 0 then o else { o with options = { o.options with sup_ordering = Options.Ordering.Func.LPO } }



(*returns (list_no_last,last)  *)
let get_last_elm_list list =
  let rec get_last_elm_list' rest list =
    match list with
    | h::[] -> ((List.rev rest), h)
    | h:: tl -> get_last_elm_list' (h:: rest) tl
    |[] -> failwith " iprover.ml: get_last_elm_list list is empty"
  in
  get_last_elm_list' [] list

let schedule_no_learinig_restarts schedule =
  let f (opt, time) =
    let new_opt =
      { opt with
	options = { opt.options with
		    inst_learning_start = 30000000
		  }
      } in (new_opt, time)
  in List.map f schedule
    
let schedule_no_learinig_restarts_between schedule =
  let (rest, last) = get_last_elm_list schedule in
  let new_rest = schedule_no_learinig_restarts rest in
  new_rest@[last]


let trivial_schedule () =
  [(input_named_options, Undef)]
 
(*   
let superposition_schedule () =
  [
   (superposition_named_options, Undef)
 ]
  *)  
	      
(* for now a schedule is defined manualy here and not in the options *)
let init_schedule1 () =
  let time1 = Def(10.) in
  let option1 = named_option_1 () in
  let time2 = Def(10.) in
  let option2 = named_option_2 () in
  let time3 = Def(10.) in
  let option3 = named_option_3 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1); (option2, time2); (option3, time3); (option_last, time_last)]

let init_schedule2 () =
  let time1 = Def(10.) in
  let option1 = named_option_1 () in
  let time2 = Def(10.) in
  let option2 = named_option_2 () in
  let time3 = Def(15.) in
  let option3 = named_option_4 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1); (option2, time2); (option3, time3); (option_last, time_last)]

let init_schedule3 () =
  let time1 = Def(15.) in
  let option1 = named_option_1 () in
  let time2 = Def(15.) in
  let option2 = named_option_2 () in
  let time3 = Def(15.) in
  let option3 = named_option_3 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1); (option2, time2); (option3, time3); (option_last, time_last)]

let init_schedule3_1 () =
  let time1 = Def(15.) in
  let option1 = named_option_1_1 () in
  let time2 = Def(15.) in
  let option2 = named_option_2_1 () in
  let time3 = Def(15.) in
  let option3 = named_option_3_1 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1); (option2, time2); (option3, time3); (option_last, time_last)]

(* like option 1 but with shorter times *)
let init_schedule4 () =
  let time1 = Def(15.) in
  let option1 = named_option_1 () in
  (* let time2 = Def(10.) in
     let option2 = named_option_2 () in
     let time3 = Def(10.) in
     let option3 = named_option_3 () in *)
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1);(*(option2,time2);(option3,time3);*)(option_last, time_last)]

let init_schedule5 () =
  let time1 = Def(25.) in
  let option1 = input_named_options in
  let time2 = Def(15.) in
  let option2 = named_option_1 () in
  let time3 = Def(15.) in
  let option3 = named_option_2 () in
  let time4 = Def(15.) in
  let option4 = named_option_3 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]

let init_schedule5_no_res_last () =
  let time1 = Def(10.) in
  let option1 = input_named_options in
  let time2 = Def(10.) in
  let option2 = named_option_1 () in
  let time3 = Def(10.) in
  let option3 = named_option_2 () in
  let time4 = Def(10.) in
  let option4 = named_option_3 () in
  let time_last = Undef in
  let option_last =
    { options_name = input_named_options.options_name^" \"--resolution_flag false\"";
      options = { input_named_options.options with resolution_flag = false }}
  in
  [(option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]

let init_schedule5_1 () =
  let time1 = Def(25.) in
  let option1 = input_named_options in
  let time2 = Def(15.) in
  let option2 = named_option_1_1 () in
  let time3 = Def(15.) in
  let option3 = named_option_2_1 () in
  let time4 = Def(15.) in
  let option4 = named_option_3_1 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]

let init_schedule5_2 () =
  let time1 = Def(25.) in
  let option1 = named_option_1_2 () in
  let time2 = Def(15.) in
  let option2 = named_option_1 () in
  let time3 = Def(15.) in
  let option3 = named_option_2 () in
  let time4 = Def(15.) in
  let option4 = named_option_3 () in
  let time_last = Undef in
  let option_last = named_option_1_2 () in
  [(option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]

let init_schedule5_inst_first () =
  let theory_flag = List.X.is_nonempty (Theory_orderings.get_ordering (Theory_db.get_global_record ())) in

(*
  let time_2 = Def(5.) in
  let option_2 =
    { options_name = input_named_options.options_name^" \"--resolution_flag false; --inst_lit_sel_side num_var\"";
      options = { input_named_options.options with 
                  resolution_flag = false;
                  inst_lit_sel_side = CM_num_var;}} in
*)
(*  let time_1 = Def(5.) 
  let option_1 =
    { options_name = input_named_options.options_name^" \"--resolution_flag false; --inst_lit_sel_side none\"";
      options = { input_named_options.options with 
                  resolution_flag = false;
                  inst_lit_sel_side = CM_none;}} in
*)
  let time0 = Def(10.) in
  let option0 =
    add_ord theory_flag 0 @@ { options_name = input_named_options.options_name^" \"--resolution_flag false --inst_lit_sel_side none\"";
      options = { input_named_options.options with 
                  resolution_flag = false;
                  inst_lit_sel_side = CM_none;
                  (* sup_symb_ordering = if theory_flag then options. *)
      };
    } 
  in
  let time1 = Def(15.) in

  let option1 = 
    (* add_ord 0 @@ *) { options_name = input_named_options.options_name^"\"1. --res_lit_sel adaptive --res_lit_sel_side num_symb\"";
                         options = 
                         
                         { input_named_options.options with 
                           res_lit_sel = Res_adaptive_max;
                           res_lit_sel_side = CM_num_symb; 
                           sup_symb_ordering = if theory_flag then input_named_options.options.sup_symb_ordering (* Options.Ordering.Symb.InvFreq *) else  Options.Ordering.Symb.Invfreq (*  Options.Ordering.Symb.Random; *)
                         };
    }
  in 

  (* Here, select theory ordering if it exists (lazy so it gets called at this point in the schedule) *)
  let theory_flag = lazy (List.X.is_nonempty (Theory_orderings.get_ordering (Theory_db.get_global_record ()))) in
    
  let time2 = Def(35.) in
  let option2 = add_ord (Lazy.force theory_flag) 0 @@ named_option_1 () in
  let time3 = Def(25.) in
  let option3 = add_ord (Lazy.force theory_flag) 1 @@ named_option_2 () in
  let time4 = Def(25.) in
  let option4 = add_ord (Lazy.force theory_flag) 1 @@ named_option_3 () in
  let time_last = Undef in
  let option_last = add_ord (Lazy.force theory_flag) 0 @@ input_named_options in
  [(*(option_2, time_2);*) (*(option_1, time_1);*)(option0, time0); (option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]


let init_schedule5_inst_first_sup () =

  let time0 = Def(10.) in
  let option0 =
    { options_name = input_named_options.options_name^" \"--resolution_flag false --inst_lit_sel_side none\"";
      options = { input_named_options.options with 
                  resolution_flag = false;
                  inst_lit_sel_side = CM_none;          
                }} 
  in
  let time1 = Def(25.) in

  let option1 = 
    { options_name = input_named_options.options_name^"\"2. --res_lit_sel adaptive --res_lit_sel_side num_symb\"";
      options =
             { input_named_options.options with 
                  res_lit_sel = Res_adaptive_max;  
                 res_lit_sel_side = CM_num_symb 
        };
        
    }
  in 

  let time2 = Def(15.) in
  let option2 = named_option_1 () in
  let time3 = Def(15.) in
  let option3 = named_option_2 () in
  let time4 = Def(15.) in
  let option4 = named_option_3 () in
  let time_last = Undef in
  let option_last = input_named_options in
  [(*(option_2, time_2);*) (*(option_1, time_1);*)(option0, time0); (option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]

(*
  let init_schedule () =
  out_str (pref_str^"Schedule 1 is on \n");
  init_schedule1 ()
 *)
(*
  let init_schedule () =
  out_str (pref_str^"Many Axioms, Schedule 1 is on \n");
  schedule_to_many_axioms_schedule (init_schedule1 ())
 *)

(*
  let init_schedule () =
  if num_of_input_clauses > !current_options.axioms_threshold
  then
  ( out_str (pref_str^"Schedule 3 is on, Many Axioms, no restarts \n");
  schedule_to_many_axioms_schedule (schedule_no_learinig_restarts (init_schedule3 ()))
  )
  else
  (out_str (pref_str^"Schedule 3 is on, no restarts between \n");
  schedule_no_learinig_restarts_between (init_schedule3 ()))

 *)

(*
  let init_schedule () =
  if num_of_input_clauses > !current_options.axioms_threshold
  then
  ( out_str (pref_str^"Schedule 3 is on, Many Axioms, no restarts \n");
  schedule_to_many_axioms_schedule (schedule_no_learinig_restarts (init_schedule3 ()))
  )
  else
  (out_str (pref_str^"Schedule 3 is on \n");
  (init_schedule3 ()))
 *)

(*
  let init_schedule () =
  if num_of_input_clauses > !current_options.axioms_threshold
  then
  ( out_str (pref_str^"Schedule 1 is on, Many Axioms, no restarts \n");
  schedule_to_many_axioms_schedule (schedule_no_learinig_restarts (init_schedule1 ()))
  )
  else
  (out_str (pref_str^"Schedule 1 is on \n");
  (init_schedule1 ()))
 *)

(*
  let init_schedule () =
  out_str (pref_str^"Schedule 5 is on \n");
  strip_conj_schedule
  (schedule_to_many_axioms_schedule (init_schedule5 ()))
 *)
(*
  let init_schedule () =
  out_str (pref_str^"Schedule 5_2 is on \n");
  strip_conj_schedule
  (schedule_to_many_axioms_schedule (init_schedule5_2 ()))
 *)

(*
  let init_schedule () =
  out_str (pref_str^"Schedule 5_1 is on \n");
  strip_conj_schedule
  (schedule_to_many_axioms_schedule (init_schedule5_1 ()))
 *)


let sat_schedule_gen problem_properties =
  out_str (pref_str^"Schedule Sat is on\n");
  let time1 = Def(30.) in
  let option1 = (named_opt_sat_mode_off input_named_options) in
  let time2 = Def(10.) in
  let option2 = named_option_1 () in
  let time3 = Def(10.) in
  let option3 = named_option_2 () in
  let time4 = Def(10.) in
  let option4 = named_option_3 () in
  let time_last = Undef in
  let option_last = named_option_finite_models() in
  strip_conj_schedule problem_properties [(option1, time1); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]

let epr_non_horn_non_eq_schedule_before_2018_06 () =
  out_str (pref_str^"Schedule EPR non Horn non eq is on\n");
  let time1 = Def(20.) in 
  let option1 = 
    let nopt3 = named_option_3 () in
    {options_name =  nopt3.options_name^" \"--instantiation_flag false\"";
     options = { nopt3.options with 
		 instantiation_flag = false; 
		 res_time_limit = 200.;
		 res_backward_subs = Subs_Subset;
		 res_backward_subs_resolution = false;
		 res_forward_subs_resolution = true;
	(*	 res_to_prop_solver = PS_result_to_Solver_None;
		 res_prop_simpl_new = false;
		 res_prop_simpl_given = false; 
*)
 
	       }} 
  in

  let time2 = Def(70.) in 
  let option2 =
    { 
      options_name = input_named_options.options_name^" \"--resolution_flag false\"";
      options = { input_named_options.options with 
		  inst_prop_sim_given = false;
		  resolution_flag = false;		 
                  inst_lit_sel = [Lit_Prop true; Lit_Num_of_Symb true;  Lit_Sign false; Lit_Num_of_Var true; ]; 
		}} in

(*  let time3 = Def(10.) in 
  let option3 = named_option_3 () in
 *)
  let time_last = Undef in
  let option_last = named_option_epr_non_horn_non_eq () in
  [(option2, time2);  (option1, time1);   (option_last, time_last)]

(*-----------------*)
let epr_non_horn_non_eq_schedule_single_opt () =
  out_str (pref_str^"Schedule EPR non Horn non eq is on\n");  
  let time_last = Undef in
  let option_last = named_option_epr_non_horn_non_eq () in  
  [(option_last, time_last)]
    
let epr_non_horn_non_eq_schedule () =
  epr_non_horn_non_eq_schedule_before_2018_06 () 

(*-----------------*)
let epr_non_horn_eq_schedule () =
  out_str (pref_str^"Schedule EPR non Horn eq is on\n");
  let time_last = Undef in
  let option_last = named_option_epr_non_horn_eq () in 
  [(option_last, time_last)]
    

(*-----------------*)
let epr_horn_non_eq_schedule () =
  out_str (pref_str^"Schedule EPR Horn non eq is on\n");
  let time_last = Undef in
  let option_last = named_option_epr_horn_non_eq () in 
  [(option_last, time_last)]
    

(*--------------*)
let ueq_schedule () =
  out_str (pref_str^"Schedule UEQ\n");
  let theory_flag = List.X.is_nonempty (Theory_orderings.get_ordering (Theory_db.get_global_record ())) in
  let time0 = Def(10.) in
  let option0 = add_ord theory_flag 0 @@ named_option_ueq () in
  let time0_ = Def(15.) in
  let option0_ = let o = named_option_ueq () in if theory_flag then o else 
  {o with options = {o.options with sup_symb_ordering = 
                     Options.Ordering.Symb.Invfreq
(* Options.Ordering.Symb.Random *)
}} in

  (* Here, select theory ordering if it exists (lazy so it gets called at this point in the schedule) *)
  let theory_flag = lazy (List.X.is_nonempty (Theory_orderings.get_ordering (Theory_db.get_global_record ()))) in
  let time2 = Def(40.) in
  let option2 = add_ord (Lazy.force theory_flag) 0 @@ named_option_ueq_1 () in
  let time3 = Def(30.) in
  let option3 = add_ord (Lazy.force theory_flag) 1 @@ named_option_ueq_2 () in
  let time4 = Def(30.) in
  let option4 = add_ord (Lazy.force theory_flag) 1 @@ named_option_ueq_3 () in
  let time_last = Undef in
  let option_last = add_ord (Lazy.force theory_flag) 0 @@ named_option_ueq () in 
  [(option0, time0); (option0_, time0_); (option2, time2); (option3, time3); (option4, time4); (option_last, time_last)]


(*let _ = out_str "\n Schedule 5 was the best before\n"*)

(*
  let init_schedule () =
  out_str (pref_str^"Schedule 5 is on \n");
  strip_conj_schedule
  (schedule_to_many_axioms_schedule (init_schedule5 ()))
 *)

(*let _ = out_str "\n now init_schedule5_no_res_last, change later!\n "*)

(* TODO fix has_theories: with theory symbols/types*)

let has_theories () =   
  match !Parser_types.input_problem_type with 
  | Some(FOF) | Some(CNF) -> false 
  |_ -> true


let dynamic_sched_5 problem_properties =
  if is_epr problem_properties && (not (is_horn problem_properties)) && not (has_eq problem_properties) then
    strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
			(epr_non_horn_non_eq_schedule ()))
      (*  [((named_option_epr_non_horn  ()),Undef)])    *)
  else if is_epr problem_properties && not (is_horn problem_properties) && has_eq problem_properties then
    strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
      (epr_non_horn_eq_schedule ()))
  else if is_epr problem_properties && is_horn problem_properties && not (has_eq problem_properties) then
    strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
      (epr_horn_non_eq_schedule ()))
      (*	[((named_option_epr_horn_non_eq ()), Undef)]) *)
  else
    if is_unit_eq problem_properties then
      ueq_schedule ()
    else (
      out_str (pref_str^"Schedule dynamic 5 is on \n");
      (* strip_conj_schedule
        (schedule_to_many_axioms_schedule (init_schedule5 ())) *)
      (* strip_conj_schedule
        (schedule_to_many_axioms_schedule (init_schedule5_no_res_last ())) *)
      (* strip_conj_schedule
        (schedule_to_many_axioms_schedule (init_schedule5 ())) *)
      (* -2012: init_schedule5_inst_first () *)
      strip_conj_schedule problem_properties
        ((* schedule_to_many_axioms_schedule *) (init_schedule5_inst_first ()))
    )


let dynamic_sched_5_sup problem_properties =
  if ((is_epr problem_properties) && (not (is_horn problem_properties)) &&  (not (has_eq problem_properties)))
  then
    strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
			   (epr_non_horn_non_eq_schedule ()))
      (*  [((named_option_epr_non_horn  ()),Undef)])    *)
  else
    if  ((is_epr problem_properties) && (not (is_horn problem_properties)) &&  (has_eq problem_properties))
    then
      strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
      (epr_non_horn_eq_schedule ()))
    else
      if ((is_epr problem_properties) && (is_horn problem_properties)) &&  (not (has_eq problem_properties))
      then
        strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
        (epr_horn_non_eq_schedule ()))
(*	[((named_option_epr_horn_non_eq ()), Undef)]) *)
      else
        (out_str (pref_str^"Schedule dynamic 5 is on \n");
         (*
	   strip_conj_schedule
	   (schedule_to_many_axioms_schedule (init_schedule5 ()))
	  *)
       (*
	 strip_conj_schedule
	 (schedule_to_many_axioms_schedule (init_schedule5_no_res_last ()))
	*)
         (*
	 strip_conj_schedule
	   (schedule_to_many_axioms_schedule (init_schedule5 ()))
	*)
       (* -2012: init_schedule5_inst_first () *)
       
         strip_conj_schedule problem_properties
	   ((* schedule_to_many_axioms_schedule *)(init_schedule5_inst_first_sup ()))	 
        )



let sat_schedule ~opts problem_properties =
  res_off_pure_eq problem_properties  (sup_off_non_eq problem_properties (
  if ((is_epr problem_properties) && (not (is_horn problem_properties)) &&  (not (has_eq problem_properties)) )
  then
    strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
    (epr_non_horn_non_eq_schedule ()))
      (*  [((named_option_epr_non_horn  ()),Undef)])    *)
  else
    if  ((is_epr problem_properties) && (not (is_horn problem_properties)) &&  (has_eq problem_properties))
    then
      strip_conj_schedule problem_properties ( (* schedule_to_many_axioms_schedule *)
      (epr_non_horn_eq_schedule ()))
    else
      
    if (is_epr problem_properties) && (is_horn problem_properties) && (not (has_eq problem_properties))
    then
      strip_conj_schedule problem_properties 
        (schedule_to_many_axioms_schedule ~opts problem_properties
			       (epr_horn_non_eq_schedule ()))
    else
      strip_conj_schedule problem_properties
	( schedule_to_many_axioms_schedule ~opts problem_properties (sat_schedule_gen problem_properties))
 ))


(* let _ = out_warning "default_schedule: commented: inst_res_sat_off_ueq" *)
let default_schedule problem_properties =
  (* inst_res_sat_off_ueq problem_properties *)
(* (* Turned out to be poor as many TFA are genuine EPR type problems *)
   if has_theories () then (* TODO fix has_theories: with theory symbols/types*)
    init_schedule5_inst_first () 
  else
*)
    (res_off_pure_eq problem_properties (sup_off_non_eq problem_properties (dynamic_sched_5 problem_properties)))
      
(*   (dynamic_sched_5 problem_properties) *)
    
let superposition_schedule problem_properties =
   inst_res_sat_off_ueq problem_properties (res_off_pure_eq problem_properties (sup_off_non_eq problem_properties (dynamic_sched_5_sup problem_properties)))
    

let verification_epr_schedule_old problem_properties =
  let time_last = Undef in
  let option_last = named_option_verification_epr_old () in
  sup_off_non_eq problem_properties (strip_conj_schedule problem_properties [(option_last, time_last)])

let verification_epr_schedule_tables problem_properties =
  let time_last = Undef in
  let option_last = named_option_verification_epr_tables () in
   sup_off_non_eq problem_properties (strip_conj_schedule problem_properties [(option_last, time_last)])

let verification_epr_schedule problem_properties =
  let time_last = Undef in
  let option_last = named_option_verification_epr () in
  sup_off_non_eq problem_properties (strip_conj_schedule problem_properties [(option_last, time_last)])


let change_first_option f sched = 
  let modif_schd = 
    match sched with 
    |[]     -> failwith "change_last_option: sched should not be empty"
    | h::tl -> (f h) :: tl 
  in
  modif_schd

let change_last_option f sched = 
 List.rev (change_first_option f (List.rev sched))

(*
  let rev_sched = List.rev sched in 
  let rev_modif = 
    match rev_sched with 
    |[]     -> failwith "change_last_option: sched should not be empty"
    | h::tl -> (f h) :: tl 
  in
  List.rev rev_modif
*)

let abstr_ref_change_named_opt b nopt = 
  if ( nopt.options.abstr_ref != [])
  then 
    nopt
  else 
    let abstr_ref = [Abstr_ref_sig;Abstr_ref_subs;Abstr_ref_arg_filter] in
    let new_nopt = 
      { 
        options_name = input_named_options.options_name^" \"--abstr_ref "^(abstr_ref_list_type_to_str abstr_ref)^" \"";
        options = { nopt.options with abstr_ref = abstr_ref}
      }
    in
    new_nopt
 (* if (b = nopt.options.abstr_ref_arg_filter) 
  then 
    nopt
  else    
    let new_nopt = 
      { 
        options_name = input_named_options.options_name^" \"--abstr_ref_arg_filter "^(string_of_bool b)^" \"";
        options = { nopt.options with abstr_ref_arg_filter = true}
      }
    in
    new_nopt
*)

let add_ar_concr_to_passive_queue q =
  let ar_concr_t = Cl_abstr_ref_concr true in
  let ar_concr_f = Cl_abstr_ref_concr false in
  let ar_concr_not_in_lst = not (List.X.mem ~eq:Poly.(=) ar_concr_t q || List.X.mem ~eq:Poly.(=) ar_concr_f q) in
  match q with
  | _ :: _ as l when ar_concr_not_in_lst -> (Cl_abstr_ref_concr true) :: l
  | [] -> [Cl_abstr_ref_concr true]
  | _ -> q

let abstr_ref_change_priority_queues schd =
  let (nopt, time) = schd in
  let add_ar_concr_pq = List.map add_ar_concr_to_passive_queue in
  let inst_pq = add_ar_concr_pq nopt.options.inst_passive_queues in
  let res_pq = add_ar_concr_pq nopt.options.res_passive_queues in
  let sup_pq = add_ar_concr_pq nopt.options.sup_passive_queues in
  let nopt' =
    { options_name = nopt.options_name ^ " Priority queues +ar_concr";
      options =
        { nopt.options with
          inst_passive_queues = inst_pq;
          res_passive_queues = res_pq;
          sup_passive_queues = sup_pq;
        }
    }
  in
  (nopt', time)


let abstr_ref_schedule problem_properties = 
  out_str (pref_str^"Schedule abstr_ref is on \n");
(*   let f (nopt, time) = ((abstr_ref_change_named_opt true nopt),time) in
 *
 * (\*  change_first_option f (default_schedule (problem_properties)) *\)
 *   change_last_option f (default_schedule (problem_properties))  *)
  List.map abstr_ref_change_priority_queues (default_schedule problem_properties)


let abstr_ref_sat_schedule ~opts problem_properties = 
  out_str (pref_str^"Schedule abstr_ref_sat is on \n");
  let f (nopt, time) = ((abstr_ref_change_named_opt true nopt),time) in

(*  change_first_option f (default_schedule (problem_properties)) *)
  change_last_option f (sat_schedule ~opts (problem_properties)) 


let smac_tmp_schedule () = 
   out_str (pref_str^"Schedule smac_tmp is on \n");
  [((named_option_smac_tmp ()), Undef)]

    
let opt_files_schedule opt_files = 
  let f (file_name,timeout) = 
    let named_option = Options.read_opt_file file_name in
    named_option.Options.options.time_out_real <- timeout; (* this is just for less confusing opt output *)
    let opt_sched_timeout = 
      if Float.O.(timeout >= 0.)
      then 
        Def(timeout)
      else
        Undef
    in
    (named_option,opt_sched_timeout)
  in
  List.map f opt_files

(*
let opt_files_schedule opt_files = 
  let named_options_list = Options.read_opt_files opt_files in 
  let get_time named_opt = 
    let real_time = named_opt.options.time_out_real in 
    if Float.O.(real_time >= 0.)
    then 
      Def(real_time)
    else
      Undef
  in
  List.map (fun nopt -> (nopt, get_time nopt)) named_options_list 
*)
