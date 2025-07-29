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

open Logic_interface

open Instantiation_env
open Resolution_loop
open Proof_search_schedule

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_elapsed
  | D_preprocess
  | D_trace
  | D_trace_prep
  | D_symb_reachability 
  | D_parse
  | D_sub_typing
  | D_axs_distinct 
  | D_axs_less_range
  | D_axs_eq
  | D_arith_axs
  | D_sem_filter
  | D_clausify
  | D_apply_after_parsing
  | D_after_parsing_exit
  | D_after_preprocessing_exit
  | D_theory
  | D_sockets 

let dbg_gr_to_str = function 
  | D_elapsed -> "elapsed"
  | D_preprocess -> "preprocess"	
  | D_trace -> "trace"
  | D_trace_prep -> "trace_preprocess"
  | D_symb_reachability -> "symb_reach"
  | D_parse -> "parse"
  | D_sub_typing -> "sub_typing"
  | D_axs_distinct  ->  "axs_distinct"
  | D_axs_less_range -> "axs_less_range"
  | D_axs_eq -> "axs_eq"
  | D_arith_axs -> "arith_axs"
  | D_sem_filter -> "sem_filter"
  | D_clausify -> "clausify"
  | D_apply_after_parsing -> "apply_after_parsing"
  | D_after_parsing_exit -> "after_parsing_exit"
  | D_after_preprocessing_exit ->"after_preprocessing_exit"
  | D_theory ->  "theory"
  | D_sockets -> "sockets" 

let dbg_groups = [
   D_trace_prep; 
  (* D_elapsed; *)
  D_trace;
(*  D_theory;*)
  (* D_symb_reachability; *)
  (* D_parse; *)
  (* D_sub_typing; *)
  (* D_clausify; *)
(*   D_axs_eq; *)
  (* D_clausify; *)
(*  D_arith_axs; *)
(*  D_preprocess; *)
(*  D_sockets; *)
  (* D_trace_prep; *)
  (* D_sem_filter; *)
  (* D_apply_after_parsing; *)
  (* D_after_parsing_exit; *)

  (* D_after_preprocessing_exit;  *)
]
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



exception Time_out_real = Signals.Time_out_real
exception Time_out_virtual = Signals.Time_out_virtual


 
(*------------Out source info -----------------------------*)
let out_source_info () = 
  let open Git_info in
  match git_info_opt with
  | Some git_info ->
      printf_tptp "%s iProver source info\n\n" (pref_str);
      printf_tptp "git: date: %s\n" (git_info.git_date);
      printf_tptp "git: sha1: %s\n" (git_info.git_sha1);
      printf_tptp "git: non_committed_changes: %B\n\n" (git_info.git_non_committed_changes)
  | None -> 
      printf_tptp "git: last_make_outside_of_git\n\n"

(*-----------------------------------------*)
let out_stat () = 
  Statistics.out_stat();
  if !global_options.stats_mem then (
    let words_to_bytes x = x * Sys.word_size / 8 in
    let stat = Gc.stat() in
    let mem_usage = words_to_bytes stat.live_words in
    let termdb_usage = words_to_bytes (Obj.reachable_words (Obj.repr !term_db_ref)) in
    let total_alloc = words_to_bytes (int_of_float stat.major_words) in
    printf_tptp "\n%s Memory\n\n" (s_pref_str());
    printf_tptp "Heap usage [kB]:           %8d\nTermDB usage [kB]:         %8d\nTotal heap allocated [kB]: %8d\n"
      (mem_usage / 1024) (termdb_usage / 1024) (total_alloc / 1024)
  );
  if !global_options.theory_stats_out then (
    printf_tptp "\n%s Theories\n\n" (s_pref_str());
    Theory_db.Record.to_list (Theory_db.get_global_record ()) |> List.iter (fun ((theory:Theory.t), subst_sym, _clauses) -> 
      printf_tptp "%-40s%s\n" (theory.name^":") (Theory_subst.to_string_sym subst_sym);
    )
  )

(*-----------------------------------------*)



(* print witness for the AIG mode verification *)
let bmc1_out_witness ~opts result =
  if opts.bmc1_incremental && opts.aig_mode && opts.bmc1_aig_witness_out
  then
    match result with
    (* SAT results in a witness *)
    | PS_result_resolution_sat (_res_model, _filtered_out_clauses_pre_inst_model)
      -> failwith "bmc1_out_witness: resoltion sat is not supported "

    | PS_result_superposition_sat (_sup_model, _filtered_out_clauses_pre_inst_model)
      -> failwith "bmc1_out_witness: superposition sat is not supported "
          
    | PS_result_smt_sat_na _ 
      -> failwith "bmc1_out_witness: smt sat is not supported "

    | PS_result_instantiation_sat (inst_pre_model, filtered_out_clauses_inst_pre_model)
      -> Bmc1Witness.print_witness ~inst_pre_model ~inst_pre_model_filtered_out:filtered_out_clauses_inst_pre_model Statistics.(get_val_stat bmc1_current_bound)

    (* UNSAT results in an unsat inform *)
    | PS_result_empty_clause _
    | PS_result_prop_solver_unsat
    | PS_result_prop_solver_unsat_na
    | PS_result_smt_unsat_na _
    | PS_result_unsat_multiple_cores _
      -> Bmc1Witness.print_unsat ()

let result_handler_prep ~opts result =
  if opts.bmc1_incremental then
    (* note that in the case of bmc1 here only preprocess results are handled; *)
    (* results during bmc1 search are handled in bmc1_loop  *)
    Bmc1_loop.result_handler_bmc1_preprocess ~opts result
  else
    result_handler_basic ~opts result;
  if not opts.bmc1_incremental
  || opts.bmc1_out_stat != BMC1_Out_Stat_None
  then 
    out_stat ()
  
    

let result_handler ~opts result =
  result_handler_basic ~opts result;
  if opts.bmc1_incremental then
      bmc1_out_witness ~opts result;
  if not opts.bmc1_incremental
  || opts.bmc1_out_stat != BMC1_Out_Stat_None
  then 
    out_stat ()



(*-------------------------------------------------*)
let clean_on_termination () = 
  dbg D_trace @@ lazy ("clean_on_termination");
  kill_all_child_processes ();
  flush stdout; 
  flush stderr
  
(*-------------------------------------------------*)

(* elapsed time code *)
(*-------------------------------------------------*)

(* keep the time *)
let last_timestamp = ref 0.0

(* set the timestamp *)
let timestamp () = last_timestamp := Unix.gettimeofday ()

(* helper: print the elapsed time, keep the time stamp *)
let elapsed_helper status =
  (* current time *)
  let current = Unix.gettimeofday () in
  (* report *)
  out_str (Format.sprintf "Timer report: %s: elapsed time %.3fs" status (current -. !last_timestamp));
  (* return current time *)
  current

(* print the elapsed time, keep the time stamp *)
let elapsed status = ignore (elapsed_helper status)

(* print the elapsed time, reset timer *)
let elapsed_ts status = last_timestamp := (elapsed_helper status)



(*----------------Top Function--------------------*)

(*-- run_iprover: initialises, ----------------*)
(*-- parses and then runs main on the preprocessed clauses------*)

let run_iprover () =

  (*-------------------*)
  out_source_info ();
  (* out_str (s_pref_str()); *)
  
(*  options_to_stdout !global_options; *)

  Options.validate !global_options;
  
  let filtered_out_inst_pre_model_ref = ref BCMap.empty in 
  
  try

    (*---------Set System Signals--------------------*)

    Signals.set_sys_signals ();
    Signals.set_time_out  ~time_out_real:!global_options.time_out_real ~time_out_virtual:!global_options.time_out_virtual;
    
    (*---------------Parse the input-------------*)

    (* we need to switch off type checking during parsing since vars are retyped during parsing *)
    (* restore after parsing *)

    let input_symbol_type_check = !global_options.symbol_type_check in

    !global_options.symbol_type_check <- false;
    
    (*------------- parsing ------------------*)

    dbg D_trace_prep (lazy ("parsing start"));
    dbg_env D_elapsed (fun () -> timestamp ());

    ParseFiles.parse ();

    dbg_env D_elapsed (fun () -> (elapsed_ts "parsing and AIG"));

    !global_options.symbol_type_check <- input_symbol_type_check;

  
    (* If just_parse = true, stop here. *)
    if !global_options.dbg_just_parse then
      exit 0;

    (* else ()
    ); *)

    (* tsar: do MC preprocessing: $traget, $init *)
    (* may be move to the end; after normal preprocessing *)

    (* bmc1_preprocess_input -> bmc1_input_transformation *)

    if !global_options.bmc1_incremental then (
      dbg D_trace_prep (lazy ("bmc1_input_transformation start"));
      Bmc1_loop.bmc1_input_transformation ~opts:!global_options; 
      dbg_env D_elapsed (fun () -> (elapsed_ts "bmc1_input_transformation"));
    );

    if !global_options.qbf_mode then 
      Qbf_fof.parse_and_process_qbf ~opts:!global_options;
    
    (*---- after parsing we need to calculate has_conj_symb/has_non_prolific_conj_symb ----------*)
  
    dbg D_trace_prep (lazy ("change_conj_symb_input start"));
    Parser_types.change_conj_symb_input ();
    dbg_env D_elapsed (fun () -> (elapsed_ts "change_conj_symb_input"));

    (*------ calculate symbol reachability based on father of map --------*)


    if !global_options.bmc1_symbol_reachability then (
      let reach_map_start_time = Unix.gettimeofday () in
       dbg D_trace_prep (lazy ("symbol_reach start"));
      (* symbol depth is assigned during Symbol_reach.symbol_reach *)
      let symb_reach_map = (Symbol_reach.symbol_reach !(Parser_types.neg_conjectures)) in

      dbg_env D_symb_reachability (fun () ->
    	  Symbol_reach.out_symb_reach_map symb_reach_map;
	   
  	    out_str (Format.sprintf
		      "%sTime for symbol reachability: %.3fs@\n@."
		      pref_str
		      (Unix.gettimeofday () -. reach_map_start_time)
		    );
	    );       
      dbg_env D_elapsed (fun () -> (elapsed_ts "symbol_reach"));
    );
    
    
    (*-------------------------------*)
    let current_clauses = ref (Parser_types.get_clauses_without_extra_axioms ()) in
    current_clauses := List.sort Clause.cmp_num_symb !current_clauses;

    (*-------------------------------*)
   
    (if !global_options.interactive_mode then 
      (
       let connection = Sockets.open_connection !global_options.external_ip_address !global_options.external_port in 
       Sockets.assign_external_connection connection
      )
    );   
    
    dbg_env D_sockets 
      (fun () -> 
        if !global_options.interactive_mode then 
          (
           let connection = Sockets.get_external_connection () in
           ignore (List.map (Sockets.get_clause_external_score connection) !current_clauses) 
          )
      );
      
    let clausify_out () = 
      let clause_list = !current_clauses in (* Parser_types.get_clauses_without_extra_axioms () in*)
      let (epr, non_epr) = List.partition Clause.is_epr clause_list in
      out_str ("% "^pref_str^"Clauses after clausification:\n\n");
      out_str ("% "^pref_str^"EPR clauses "^(string_of_int (List.length epr))^" :\n\n");
      Clause.out_clause_list_tptp epr;
      out_str ("\n\n"^"% "^pref_str^"non-EPR clauses "^(string_of_int (List.length non_epr))^" :\n\n");
      Clause.out_clause_list_tptp non_epr;
      out_str "\n\n";
      if !global_options.sig_cnt_out then Problem_properties.out_sig_cnt !current_clauses;
      (* exit(0);*)
    in
    if !global_options.clausify_out then (
      clausify_out ();
      exit(0);
    );
    dbg_env D_clausify (fun () -> clausify_out ());

    (*---debug-------*)
    dbg_env D_parse (fun () -> 
  	  Logic_interface.out_symbs ();
  	  Logic_interface.out_terms (); 
  	  Clause.out_clauses_param (Clause.CL_List !current_clauses)
  	);

    (*---------------- Theory detection (experimental) -----------------*)

    (* out_str @@ "";
    let theory_record = Theory_db.Record.create () in
    List.iter Theory_db.(search builtin theory_record) !current_clauses;
    out_str @@ "% Detected theories:";
    theory_record |> Theory_db.Record.output ~prefix:"% " stdout;
    out_str @@ "% End.\n"; *)
    (* Theory_db.print_report ~trunc:true ~heading:"Detected theories in input" !current_clauses; *)

    dbg D_trace_prep @@ lazy "Theory detection: input";
    List.iter Theory_db.(search builtin (get_global_record ())) !current_clauses;

    
    (*---------------- Theory detection end -----------------*)
   
    dbg D_trace_prep (lazy ("get_bv_axioms start"));
    let bv_axioms = Eq_axioms.get_bv_axioms () in
    current_clauses := List.rev_append bv_axioms !current_clauses;
    dbg_env D_elapsed (fun () -> (elapsed_ts "get_bv_axioms"));


    dbg_env D_apply_after_parsing (fun () -> 
      out_str "";
      out_str "Clauses before splitting_nvd:\n";         
      out_str (Clause.clause_list_to_string !current_clauses);
      current_clauses:=Splitting_nvd.split_clause_list Definitions.def_env_glb 3 !current_clauses;
      out_str "";
      out_str "Clauses after splitting_nvd:\n";
      out_str (Clause.clause_list_to_string !current_clauses);
    );

    dbg_env D_after_parsing_exit (fun () -> 
      out_str "";
      dbg D_after_parsing_exit (lazy "Parsed clauses:");
      out_str (Clause.clause_list_to_string !current_clauses);
      out_str "";
      (*--- some dbg on clauses after parsing----*)
         
      let def_map = Def_discovery.get_def_map !current_clauses in 
      dbg D_after_preprocessing_exit (lazy "Definiton discovery:");
      Def_discovery.out_def_map def_map;

      dbg D_after_preprocessing_exit (lazy "Definiton discovery: equiv defs:");
      let equiv_defs = Def_discovery.get_equiv_defs !current_clauses in
      Def_discovery.out_equiv_defs equiv_defs;
  
      dbg D_after_parsing_exit (lazy "done; exiting");
      exit 0;
    );


    (* At the moment Parsed_input_to_db.input_clauses_ref are not memory released! *)
    (* we can replace Parsed_input_to_db.input_clauses_ref with *)
    (* global  Parsed_input_to_db.current_clauses, which are gradually replaced by preprocessing but should be carefull how intput caluses are used below: finite_models eq_axioms etc. *)

    (* tsar: AIG does not contains equalities, so unflatten is not applicable *)
    if not !global_options.aig_mode 
    && !global_options.preprocessing_flag 
    && !global_options.prep_unflatten 
    then (
      dbg D_trace_prep (lazy "unflatten start");
      current_clauses := Inference_rules.unflatten !current_clauses;
      dbg_env D_elapsed (fun () -> elapsed_ts "unflatten");
    );

    
    (* TODO: make preprocessing that does not depend on prop_solver first *)

    (* we can not init prop_solver before subtyping bacause terms for grounding depend on types! *)

    dbg D_trace_prep (lazy "init_solver_exchange start");
    (*-------------------------------*)
    Prop_solver_exchange.init_solver_exchange ();
    (*-------------------------------*)
    dbg_env D_elapsed (fun () -> elapsed_ts "init_solver_exchange");

    (*----- assign decision variables hook for AIG-------*)
    (*
    (if (!current_options.aig_mode)
    then
      (* assign decision to be only input and latches *)
      let decision_var_test lit = 
    	(* AigClausifier.aig_is_latch_pred lit ||*) AigClausifier.aig_is_input_pred lit
      in
      Prop_solver_exchange.set_decision_var_test_hook decision_var_test;
    else()
    );
    *)

(*
    dbg D_trace_prep (lazy ("add_clause_to_solver start"));
    List.iter Prop_solver_exchange.add_clause_to_solver !current_clauses;
    dbg_env D_elapsed (fun () -> (elapsed_ts "add_clause_to_solver"));
  *)
  
    (* with sat_mode one should be careful with options!*)
    (* switch off resolution! *)
    (* out_str ("\n\n!!!!! Switch from Finite Models to SAT mode!!!\n\n");
       if !current_options.sat_mode
       then
       (current_options:= (named_option_finite_models ()).options;
       sat_mode !current_clauses )
       else
     *)

  
    (*--------- arimetic order numbers axioms ----*)  
    let arith_order_num_axioms_flag = true in 
    let _ = out_warning (sprintf "iprover.ml: arithm_order_num_axioms_flag: %b" arith_order_num_axioms_flag) in 
    let (int_axs, rat_axs, real_axs) = Arithmetic.order_axioms () in 
    dbg D_arith_axs @@ lazy (sprintf "int_axs: %s" (Clause.clause_list_to_string int_axs));
    dbg D_arith_axs @@ lazy (sprintf "rat_axs: %s" (Clause.clause_list_to_string rat_axs));
    dbg D_arith_axs @@ lazy (sprintf "real_axs: %s" (Clause.clause_list_to_string real_axs));
    
    current_clauses := int_axs @ rat_axs @ real_axs @ !current_clauses;

    (*----------------------- Preprocessing ------------------------*)

    dbg D_preprocess @@ lazy (
      sprintf "Clauses before preprocessing:\n%s\n" (Clause.clause_list_to_tptp !current_clauses)
    );

       (* temporarily unassign_is_essential_input_symb before preprocessing *)	
      dbg D_trace_prep (lazy ("unassign_is_essential_input_symb start"));
      Clause.unassign_is_essential_input_symb (Clause.CL_List !current_clauses); 
      dbg_env D_elapsed (fun () -> (elapsed_ts "unassign_is_essential_input_symb"));
      (* preprocess befroe eq axioms, otherwise can be expensive *)

      dbg D_trace_prep (lazy ("preprocess started"));

    (* do not need to include equality axioms as side clauses *)
    (*    let _= out_warning "dbg: iprover.ml: preprocessing commented! 1" in *)


    (* TODO: add  ~extra_side_atoms for bmc1 *)
    (* KK pass problem propreties ? *)

    let prob_props_initial = Problem_properties.get_prob_props !current_clauses in 
    let prep_opts = Preprocess.glb_sched_to_prep_options (prob_props_initial) in 
    let prep_state = 
      Preprocess.prep_create_state ~prep_opts ~clause_list:!current_clauses 
        (* ~side_clauses:(Eq_axioms.eq_axiom_list !current_clauses) *) ~extra_side_atoms:[]
    in
    (* TODO: fix add inst_pre_model *)
    if !global_options.preprocessing_flag then (
 
      Preprocess.preprocess_sim ~before_eq_axioms:true prep_state;

      Preprocess.preprocess_trans prep_state;

      current_clauses := Preprocess.prep_get_clauses prep_state;

      dbg D_trace @@ lazy "Theory detection: preprocessed";
      List.iter Theory_db.(search builtin (get_global_record ())) !current_clauses;
        
      (* TODO: move to after all preprocessing *)
      Preprocess.maybe_print_and_exit prep_state; 

      dbg D_trace_prep (lazy ("preprocess finished"));
      dbg_env D_elapsed (fun () -> (elapsed_ts "preprocess"));

      filtered_out_inst_pre_model_ref := Preprocess.prep_get_inst_pre_model prep_state;        
   
      dbg D_trace_prep (lazy ("preprocess finished"));
      dbg_env D_elapsed (fun () -> (elapsed_ts "preprocess")); 
  
      dbg_env D_after_preprocessing_exit (fun () -> 
        out_str "";
        dbg D_after_preprocessing_exit (lazy "Parsed clauses:");
        out_str (Clause.clause_list_to_string !current_clauses);
        out_str "";
        (*--- some dbg on clauses after parsing----*)
        
        let def_map = Def_discovery.get_def_map !current_clauses in 
        dbg D_after_preprocessing_exit (lazy "Definiton discovery:");
        Def_discovery.out_def_map def_map;

        dbg D_after_preprocessing_exit (lazy "Definiton discovery: equiv defs:");
        let equiv_defs = Def_discovery.get_equiv_defs !current_clauses in
        Def_discovery.out_equiv_defs equiv_defs;
        dbg D_after_preprocessing_exit (lazy "done; exiting");
        exit 0;
      );
  
      dbg D_preprocess @@ lazy (sprintf "Clauses after preprocessing:\n%s\n"
        (Clause.clause_list_to_tptp !current_clauses)
      );
      
    ) else (
      Preprocess.maybe_print_and_exit prep_state; 
    );

    Clause.assign_is_essential_input_symb (Clause.CL_List !current_clauses); 

    (*---------------- Theory detection (experimental) -----------------*)

    (* out_str @@ "";
    let theory_record = Theory_db.Record.create () in
    List.iter Theory_db.(search builtin theory_record) !current_clauses;
    out_str @@ "% Detected theories:";
    theory_record |> Theory_db.Record.output ~prefix:"% " stdout;
    out_str @@ "% End.\n"; *)
    dbg_env D_theory (fun () -> 
      Theory_db.print_report ~heading:"Detected theories after preprocessing" !current_clauses; 
                     );
    
    (*---------------- Extra axioms -----------------*)

    (*--- axs distinct ---*)
    let distinct_ax_list = Eq_axioms.distinct_ax_list () in
    dbg_env D_axs_distinct (fun () ->
  	  out_str "\n-----------Distinct Axioms:---------\n";
  	  out_str ((Clause.clause_list_to_tptp distinct_ax_list)^"\n\n");
  	  out_str "\n--------------------\n";
	  );

    (* Clauses are input clauses *)
    Clause.assign_is_essential_input_symb (Clause.CL_List distinct_ax_list);
    
    current_clauses := List.rev_append distinct_ax_list !current_clauses;

    (*--- axs less/range ---*)
      
    let less_range_axioms = Eq_axioms.less_range_axioms () in
     
    dbg_env D_axs_less_range (fun () ->
    	out_str "\n-----------Less Range Axioms:---------\n";
    	out_str ((Clause.clause_list_to_tptp less_range_axioms)^"\n\n");
    	out_str "\n--------------------\n";
	  );
	  
    current_clauses := List.rev_append less_range_axioms !current_clauses;

    (* Clauses are input clauses *)      
    Clause.assign_is_essential_input_symb (Clause.CL_List less_range_axioms);

     
    (*-------- Equality Axioms ----------------------*)
      
    let gen_equality_axioms = ref [] in

    (*---------  current_clauses_no_eq used in sat ---------- *)
    let current_clauses_no_eq = ref (!current_clauses) in

    (*-------------------------------*)	
    (* if not !current_options.superposition_flag && !current_options.schedule <> Schedule_superposition then *)
    if !global_options.brand_transform then (
      dbg_env D_axs_eq (fun () ->
        out_str "\n-----------Before Brand transform:---------\n";
        out_str ((Clause.clause_list_to_tptp !current_clauses)^"\n\n");
        out_str "\n--------------------\n";
      );
       
      current_clauses := 
        Splitting.splitting Definitions.def_env_glb ~out_progress:true 
          (Eq_axioms.flat_clause_list !current_clauses);

      current_clauses_no_eq := !current_clauses;

      dbg_env D_axs_eq (fun () ->
        out_str "\n-----------After Brand transform:---------\n";
        out_str ((Clause.clause_list_to_tptp !current_clauses)^"\n\n");
        out_str "\n--------------------\n";
      );

      gen_equality_axioms := Eq_axioms.eq_axioms_flatting !current_clauses;
       
      dbg_env D_axs_eq (fun () ->
        out_str "\n-----------Eq axs:---------\n";
        out_str ((Clause.clause_list_to_tptp !gen_equality_axioms)^"\n\n");
        out_str "\n--------------------\n";
      );          

      current_clauses := List.rev_append !gen_equality_axioms !current_clauses;                
    ) 

    else (         
      gen_equality_axioms := Eq_axioms.eq_axiom_list !current_clauses;           
      dbg_env D_axs_eq (fun () ->
        out_str "\n-----------Eq Axioms:---------\n";
        out_str ((Clause.clause_list_to_tptp !gen_equality_axioms)^"\n\n");
        out_str "\n--------------------\n";            
      );            

      current_clauses := List.rev_append !gen_equality_axioms !current_clauses
    );

    (*
    (*---------------in the case of superposition, transform to pure equality -------*)
      else (
        current_clauses := Superposition.EqualityTransformation.clauselist_to_eq !current_clauses
      );    
    *)

    (*---------------preprocess after adding equality -------*)
      
    if !gen_equality_axioms != [] && !global_options.preprocessing_flag then (
      dbg D_trace_prep (lazy "preprocess after eq ");

      (* KK pass problem propreties ? *)
      let prep_opts = Preprocess.glb_sched_to_prep_options (Problem_properties.get_prob_props !current_clauses) in 

      let prep_state_after_eq = 
        Preprocess.prep_create_state ~prep_opts ~clause_list:!current_clauses 
        (* ~side_clauses:(Eq_axioms.eq_axiom_list !current_clauses) *) ~extra_side_atoms:[] 
      in
    
      Preprocess.preprocess_sim ~before_eq_axioms:false prep_state_after_eq;
      current_clauses := Preprocess.prep_get_clauses prep_state_after_eq;
        
      filtered_out_inst_pre_model_ref := 
        Instantiation_env.inst_pre_model_union 
          !filtered_out_inst_pre_model_ref (Preprocess.prep_get_inst_pre_model prep_state_after_eq);

         dbg D_trace_prep (lazy ("preprocess after eq finished"));
    );

     
      
    if !global_options.dbg_out_stat (* && not !input_problem_props.epr *) then 
    	(* get statistics on non-cyclic/epr subtyping without taking into account pure EPR *)
    	ignore (Finite_models.init_fm_state ~opts:!global_options !current_clauses_no_eq);

    if !global_options.dbg_out_stat then
  	  failwith "Output satistics";
  
    (* TODO: move to here after all prep: Preprocess.maybe_print_and_exit prep_state; *)

    (*-------------------------------------------------*)
    out_str ("");
    out_str (pref_str^"Proving...");
    (*-------------------------------------------------*)

    dbg D_trace (lazy (sprintf "In run_iprover: input size = %d\n" (List.length !current_clauses)));

    dbg_env D_trace (fun () -> 
      dbg D_trace (lazy ("Clauses: before proving with equality \n"));
      Clause.out_clause_list_tptp !current_clauses;
    );

    dbg_env D_trace (fun () ->
      out_str "\n Clauses: before proving without equality  \n";
      out_str ((Clause.clause_list_to_tptp !current_clauses_no_eq)^"\n\n");
      (*
      out_str "\n--------------------\n";
      let eq_clauses = (EqualityTransformation.clauselist_to_eq !current_clauses_no_eq) in 
      out_str "\n Clauses: before proving eq transormed \n";
      out_str ((Clause.clause_list_to_tptp eq_clauses)^"\n\n");
      out_str "\n--------------------\n";
      *)
    );

    (* Clause.out_clause_list !current_clauses; *)      

    let finite_model_clauses_ref = current_clauses_no_eq in

    (*----- problem properties --------*)

    (* let problem_properties = (Problem_properties.get_prob_props !current_clauses) in *)
    let problem_properties = (Problem_properties.get_prob_props !current_clauses_no_eq) in 
    Problem_properties.prob_props_to_statistics problem_properties;      
    out_str (pref_str^"Problem Properties \n");
    out_str ("\n"^(Problem_properties.prob_props_to_string problem_properties)^"\n");

    (*----- solver call before schedules ------*)

    (* solver is called first time in the  Proof_search_schedule.schedule_run  *)



    (*------------------------ Proving part ---------------------*)

    let schedule_clauses = {
      Proof_search_schedule.proof_clauses_with_eq_axioms = !current_clauses;     
      Proof_search_schedule.proof_clauses_no_eq_axioms   = !current_clauses_no_eq;
      Proof_search_schedule.problem_properties = problem_properties ;
      Proof_search_schedule.finite_model_clauses         = !finite_model_clauses_ref;
      Proof_search_schedule.filtered_out_inst_pre_model  = !filtered_out_inst_pre_model_ref;
    } in

    if not !global_options.instantiation_flag 
    && not !global_options.resolution_flag 
    && not !global_options.superposition_flag 
    then
      failwith "No solver is selected: see --instantiation_flag, --resolution_flag, --superposition_flag";

    (* schedule depends on the proving method *)
    let schedule =
      match !global_options.schedule with

      | Schedule_default ->
        Proof_search_schedule.default_schedule (problem_properties)

      | Schedule_sat ->
        Proof_search_schedule.sat_schedule ~opts:!global_options (problem_properties)

      | Schedule_opt_files opt_files -> 
        Proof_search_schedule.opt_files_schedule opt_files (* (problem_properties) *)

      | Schedule_abstr_ref ->
        Proof_search_schedule.abstr_ref_schedule (problem_properties)

      | Schedule_abstr_ref_sat ->
        Proof_search_schedule.abstr_ref_sat_schedule ~opts:!global_options (problem_properties)

      | Schedule_superposition -> 
        Proof_search_schedule.superposition_schedule (problem_properties)

      | Schedule_none ->
        Proof_search_schedule.trivial_schedule ()

      | Schedule_verification_epr_old ->
        Proof_search_schedule.verification_epr_schedule_old (problem_properties)

      | Schedule_verification_epr_tables ->
        Proof_search_schedule.verification_epr_schedule_tables (problem_properties)

      | Schedule_verification_epr ->
        Proof_search_schedule.verification_epr_schedule (problem_properties)

      | Schedule_smac_tmp -> 
        Proof_search_schedule.smac_tmp_schedule ()
    in

    let sched_run sched =
      let opts = copy_options !global_options in 
      if !global_options.bmc1_incremental then
        (* create initial MC state *)
        let state = Bmc1_loop.bmc1_init_solver_state ~opts sched schedule_clauses in
        (* run BMC1 loop on that state *)
        if !global_options.bmc1_ucm then
          Bmc1_loop.bmc1_mp_loop_init state
        else
          Bmc1_loop.bmc1_loop ~opts state
      else
        Proof_search_schedule.schedule_run schedule_clauses sched
    in
      
    let result = sched_run schedule in
    dbg D_trace (lazy "Normal: exit with result");
    result_handler ~opts:!global_options result

  with
    
    (* preprocessing results *)
  
    | Unsatisfiable_gr 
    | Unsatisfiable_gr_na ->
      dbg D_trace (lazy "Prep: Unsat: gr/gr_na");
      result_handler_prep ~opts:!global_options (PS_result_prop_solver_unsat)      
       
    | Unsatisfiable_gr_smt_na clauses ->
      dbg D_trace (lazy "Prep: Unsat: smt_na");
      result_handler_prep ~opts:!global_options (PS_result_smt_unsat_na clauses) 

    | Empty_clause clause ->
      dbg D_trace (lazy "Prep: Empty");
      result_handler_prep ~opts:!global_options (PS_result_empty_clause (clause)) 

    | Superposition.Sup_satisfiable sup_model ->
      dbg D_trace (lazy "Prep: superposition sat");
      result_handler_prep ~opts:!global_options (PS_result_superposition_sat (sup_model, !filtered_out_inst_pre_model_ref)) 

    | Resolution_loop.Res_satisfiable res_model ->
      dbg D_trace (lazy "Prep: sat discount");
      result_handler_prep ~opts:!global_options (PS_result_resolution_sat (res_model, !filtered_out_inst_pre_model_ref))
	
    | Instantiation_loop.Inst_satisfiable inst_pre_model ->
      dbg D_trace (lazy "Prep: sat instantiation");
      result_handler_prep ~opts:!global_options (PS_result_instantiation_sat (inst_pre_model, !filtered_out_inst_pre_model_ref))

    | Satisfiable_gr_smt_na _ ->
      dbg D_trace (lazy "Prep: sat on preprocess");
      result_handler_prep ~opts:!global_options (PS_result_smt_sat_na ())

    | Termination_Signal ->
      out_str (szs_unknown_str ());
      out_str "\n Termination Signal\n";
      
      (* Do not output statistics in BMC1 mode with
         -- bmc1_out_stat none *)
      if not !global_options.bmc1_incremental 
      || !global_options.bmc1_out_stat != BMC1_Out_Stat_None 
      then
        out_stat ();
      clean_on_termination (); 

    | Time_out_real ->
      out_str (szs_unknown_str ());
      out_str "Time Out Real\n";
         
      (* Do not output statistics in BMC1 mode with
         -- bmc1_out_stat none *)
      if not !global_options.bmc1_incremental 
      || !global_options.bmc1_out_stat != BMC1_Out_Stat_None 
      then
        out_stat ();

    | Time_out_virtual ->
      clean_on_termination ();   
      out_str (szs_unknown_str ());
      out_str "Time Out Virtual\n";
      
      (* Do not output statistics in BMC1 mode with
         -- bmc1_out_stat none *)
      if not !global_options.bmc1_incremental 
      || !global_options.bmc1_out_stat != BMC1_Out_Stat_None 
      then
        out_stat ();

      clean_on_termination (); 

    | Proof_search_schedule.Schedule_Terminated ->
      out_str (szs_unknown_str ());
      out_str "Schedule_Terminated:  try an extended schedule or with an unbounded time limit";
      
      (* Do not output statistics in BMC1 mode with
         -- bmc1_out_stat none *)
      if not !global_options.bmc1_incremental 
      || !global_options.bmc1_out_stat != BMC1_Out_Stat_None 
      then
        out_stat ();
      clean_on_termination ();

  	(* Silently terminate after BMC1 maximal bound proved *)
    | Exit ->
      out_str "Exit: done with BMC";
      dbg D_trace (lazy "Exit: done with BMC");
      clean_on_termination ();   
  	
    | x ->
      Format.eprintf "iprover.ml: Unexpected exception: %s\n" (Printexc.to_string x);
      if !global_options.dbg_backtrace then (
        Format.eprintf "Backtrace: status: %b :@\n%s@\n@." (Printexc.backtrace_status ())  (Printexc.get_backtrace ())
      );

      out_str (szs_unknown_str ());
      out_stat ();
      (* clean_on_termination (); *)
      raise_trace x


  
let _ = run_iprover ()

(*---------------------------Commented----------------------------*)
