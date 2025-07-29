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

(* module for simplification sets of clauses *)


open Lib
open Options
open Statistics
open Logic_interface 


(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  |D_ss_add
  |D_subs_add
  |D_ss_rm
  |D_subs_rm
  |D_assign_dead_remove_from_indexes
  |D_fwd_ss
  |D_bwd_ss 
  |D_eq_res
  |D_taut
  |D_prop_subs
  |D_fwd_subs_res
  |D_fwd_subs
  |D_fwd_subs_strict
  |D_bwd_subs_res
  |D_bwd_subs
  |D_demodulation
  |D_candlength

let dbg_gr_to_str = function 
  |D_ss_add -> "ss_add"
  |D_subs_add -> "subs_add"
  |D_ss_rm -> "ss_rm"
  |D_subs_rm -> "subs_rm" 
  |D_assign_dead_remove_from_indexes -> "assign_dead_remove_from_indexes"
  |D_fwd_ss -> "forward_subset_subsume"
  |D_bwd_ss -> "backward_subset_subsume"
  |D_eq_res -> "equality_resolution"
  |D_taut -> "tautology"
  |D_prop_subs -> "prop_subs"
  |D_fwd_subs_res -> "fwd_subs_res"
  |D_fwd_subs -> "fwd_subs" 
  |D_fwd_subs_strict -> "fwd_subs_strict"
  |D_bwd_subs_res -> "bwd_subs_res"
  |D_bwd_subs -> "bwd_subs"
  |D_demodulation -> "demodulation"
  |D_candlength -> "candlength"

let dbg_groups = [
  (* D_subs_add; 
  D_subs_rm;
  D_ss_add; 
  D_ss_rm;
  D_subs_rm;
  D_assign_dead_remove_from_indexes;
  D_bwd_ss;
  D_eq_res;
  D_taut;

  D_fwd_subs_strict;
  D_fwd_subs;

  D_prop_subs;
  D_fwd_subs_res; 
  D_fwd_subs; 
  D_bwd_subs_res;
  D_bwd_subs;  *)
  D_demodulation; 
  D_candlength;
]
    
let module_name = "simplify"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
  
(*----- debug -----*)

(*----------- clause params ------------*)

(*
let sc_in_ss_index = 1   (* subset subsumtion index*)
let sc_in_subs_index = 2 (* subsumtion index *)
    
type sim_cl_param = 
    {
     mutable sc_bool_param : Bit_vec.bit_vec;
   }   
      
let get_sim_cl_bp param c_param = (* bp -- bool param*)
  Bit_vec.get param bv c_param.sc_bool_param 
    
let set_sim_cl_bp value bparam c_param =
  c_param.sc_bool_param <- Bit_vec.set value bparam c_param.sc_bool_param

type scp = sim_cl_param BCMap.t (* sim clause params *)
*)
  

(*-------------------------------*)
type sim_options = 
  {

   (* this is needed if the same clause is used in different contexts with separate indexes  *)
   (* since clause paremeters such as ss index are set during adding into the sim_state *)
 
   sim_add_to_prop_solver            : bool;
   sim_add_to_smt_solver             : bool;
   sim_use_ss_index                  : bool;
   sim_use_sub_index                 : bool; 
   sim_subs_bck_mult                 : int;
   sim_add_to_sub_index_test         : clause -> bool;
   (* sim_use_demod                     : bool;  *)
  }

(* all statistics currently is added to the resolution statistics
   
  type sim_stat = 
  {
  
  }
 *)

(* simplifications borrowed from discount.ml *)

(*----------- Subsumption index based on compressed features ------------------------*)
    
module SubsumptionIndexM = SubsumptionIndex.SCFVIndex
    
(*
type feature_list = SubsumptionIndexM.feature list

let get_feature_list clause = SubsumptionIndexM.get_feature_list clause
*)  
  
(* let subsumption_index_ref = ref (SubsumptionIndexM.create ()) *)

(*--------- sim state ----------------------*)  
type sim_state = 
    {
     sim_context : context; (* with each new sim_state a new context is created and non_dead clauses are added;
                               if a module uses sim_state then one should always add clauses via sim_state
                               otherwise clauses will not be added into simpl. indexes even at the next round *)

(*     
     mutable sim_clauses : BCSet.t; 
             (* we assume that clauses added to simplify are added to sim_cl_params *)
             (* the clause can be in sim_cl_params with all params false e.g. when the clause is dead *)
*)   
     sim_ss_index  : SubsetSubsume.index;
     sim_sub_index : SubsumptionIndexM.index;
     sim_opt : sim_options;

     (* Demodulation *)
     (* mutable sim_demod_equations : DemodulationIndex.t; *)
     (* List of clauses already encountered *)
     mutable sim_intermediate_seen: BCSet.t;
   }

let sim_create sim_options (* context *) =
  let sim_state = 
    {
     sim_context   = context_create ();
     (* sim_clauses   = BCSet.empty; *)
     sim_ss_index  = SubsetSubsume.create ();
     sim_sub_index = SubsumptionIndexM.create ();
     sim_opt = sim_options; 

     (* sim_demod_equations = DemodulationIndex.create ~eq_types:SSet.empty ~use_ground:true (); *)
     sim_intermediate_seen = BCSet.empty;
   }
  in
  sim_state

(*---------------------------*)

let sim_state_num_clauses sim_state ~non_dead = context_size sim_state.sim_context ~non_dead

(*-----------*)

(* can raise Not_found *)

let get_sim_state_clause sim_state clause = 
  context_find sim_state.sim_context clause


let sim_get_context sim_state = 
  sim_state.sim_context

let sim_is_dead sim_state c = 
  Clause.get_is_dead sim_state.sim_context c

(*
(*
  let eliminate_clause_from_indexes sim_state clause =
  Clause.assign_is_dead true clause;
  remove_clause sim_state clause
 *)

let get_non_dead_clauses_list sim_state = 
  let f c rest = 
    if (Clause.get_is_dead c) 
    then 
      rest
    else
      (Clause.copy_clause c)::rest 
  in 
  context_fold sim_state.sim_context f []
*)
(*----------*)

let sim_mem_clause sim_state c = 
  context_mem sim_state.sim_context c


(*---- add subset subsume ---*)

let add_to_ss_index sim_state clause =
  try
    dbg D_ss_add (lazy ("add_to_ss_index: "^(Clause.to_string_tptp clause)));
    SubsetSubsume.add_clause sim_state.sim_ss_index clause;
  with 
(* should not happen *)
  |SubsetSubsume.Subsumes -> 	 
      dbg D_ss_add (lazy ("add_to_ss_index: exception SubsetSubsume.Subsumes: "^(Clause.to_string_tptp clause)))
  |SubsetSubsume.Is_subsumed ->
      dbg D_ss_add (lazy ("add_to_ss_index: exception SubsetSubsume.Is_subsumed: "^(Clause.to_string_tptp clause)))


(*----add subsumption *)  
let add_to_sub_index sim_state clause =
  (*  let feature_list = (get_feature_list clause) in*)
  (*  let com_feature_list = compress_feature_list feature_list in*)
  (
   dbg D_subs_add (lazy (Clause.to_string_tptp clause));
   SubsumptionIndexM.add_clause sim_state.sim_sub_index clause
  )

(*
(** Add to list of unit equations *)
let add_to_demod_equations sim_state clause =
  DemodulationIndex.add_equation sim_state.sim_demod_equations clause

(** Add to list of unit equations *)
let add_to_bwd_demod_index sim_state clause =
  DemodulationIndex.add_bwd_clause sim_state.sim_demod_equations clause
*)



(*----- removes clause from sim_sate indexes (does not assign is_dead) *)

let remove_from_sub_index sim_state clause = 
  try
    (
     dbg D_subs_rm (lazy (Clause.to_string_tptp clause));
     SubsumptionIndexM.remove_clause
       sim_state.sim_sub_index clause;
    )
  with 
     Not_found -> ()

let remove_from_ss_index sim_state clause = 
  try
    (
     dbg D_ss_rm (lazy (Clause.to_string_tptp clause));
     SubsetSubsume.remove sim_state.sim_ss_index clause;
    )
  with 
    Not_found -> ()

(*
let remove_from_demod_index sim_state clause =
  begin try
    DemodulationIndex.elim_equation sim_state.sim_demod_equations clause
  with Not_found ->
    ()
  end;
  begin try
    DemodulationIndex.elim_bwd_clause sim_state.sim_demod_equations clause
  with Not_found ->
    ()
  end
*)

let remove_from_indexes sim_state clause = 
  if sim_state.sim_opt.sim_use_ss_index then (
    remove_from_ss_index sim_state clause;
  );
  if sim_state.sim_opt.sim_use_sub_index
  && sim_state.sim_opt.sim_add_to_sub_index_test clause
  then (
    remove_from_sub_index sim_state clause;
  );
  (* if sim_state.sim_opt.sim_use_demod then (
    remove_from_demod_index sim_state clause
  ); *)
  ()



         

(* TODO join with replaced_by *)
let assign_dead_and_remove_from_indexes sim_state clause = 
  dbg D_assign_dead_remove_from_indexes (lazy (Clause.to_string_tptp clause));
  try 
    let sim_clause = get_sim_state_clause sim_state clause in
    remove_from_indexes sim_state sim_clause;
    Clause.assign_is_dead sim_state.sim_context true sim_clause
  with 
    Not_found ->  ()

let remove_from_indexes_and_context sim_state clause = 
  try 
    let sim_clause = get_sim_state_clause sim_state clause in
    remove_from_indexes sim_state sim_clause;
    Clause.context_remove sim_state.sim_context sim_clause
  with 
    Not_found ->  ()


(*
let remove_from_cl_params sim_state clause = 
  sim_state.sim_cl_params <- SPC.remove clause sim_state.sim_cl_params
*)

(*
let remove_from_sim_state sim_state clause =
  try 
(*    let cl_param = SCP.find  clause sim_state.sim_cl_params in *)
(*    sim_state.sim_clauses <- BCSet.remove clause sim_state.sim_clauses; *)
    remove_from_indexes sim_state clause;
    
(*    remove_from_cl_params sim_state clause *)
  with 
    Not_found -> ()
*)

let remove_from_sim_state_and_context sim_state clause =
  remove_from_indexes sim_state clause;
  context_remove sim_state.sim_context clause (* context_remove is based on basic_clause *)

(*---------*)

let forward_subset_subsume_by sim_state clause = 
  try
    let by_clause = SubsetSubsume.is_subsumed sim_state.sim_ss_index clause in
(*    clause_register_subsumed_by ~by:by_clause clause; *)

    incr_int_stat 1 res_forward_subset_subsumed;
    dbg D_fwd_ss (lazy ("subsumed: "^(Clause.to_string_tptp clause)^" by: "^(Clause.to_string_tptp by_clause)));
    by_clause
  with
   (* SubsetSubsume.Subsumes -> raise Eliminated*)
  | Not_found -> raise Not_found

(*---------*)

let forward_subset_subsume sim_state clause = 
  try
    let _by = forward_subset_subsume_by sim_state clause in 
(* TODO: do we need reg. forward simplified ? *)
(*    clause_register_subsumed_by ~by clause; *)
    raise Eliminated
  with
  | Not_found -> clause


(* if simplify_light_backward simplifies some clauses then (copy of) main_clause is added to the sim_state *)
(* removes subsumed clauses from the indexes; assigned them dead; but keeps in the sim_state context *)


(* for backward simplifications we need to filter out clauses which are the same basic clause as the main clause *)
(* otherwise if we different backward simplifications with the same main_clause *)
(* at the first one it will be automatically added to the index and at the second one will simplify itself *)
(* which will lead to incorrectly throughing out both of them *)

let filter_different cl cl_list = 
  List.filter (fun x -> (not (Clause.equal_bc cl x))) cl_list
  

(* in order to keep subset subme indexes consistent we need to ss simplify forward and backward new clauses *)

(* this version of bwd_ss does not add main_clause to ss_index even when it subsumes; used  only as auxilary to bwd_ss and add_clause_to_sim  *)

let backward_subset_subsume_without_adding sim_state main_clause =
  try
    let subsumed_clauses = 
      filter_different main_clause (SubsetSubsume.find_subsumed sim_state.sim_ss_index main_clause) in 
    
    (if List.X.is_nonempty subsumed_clauses
    then
      (
       incr_int_stat (List.length subsumed_clauses) res_backward_subset_subsumed;    
       dbg D_bwd_ss (lazy ("subsumed: "^(Clause.clause_list_to_string subsumed_clauses)
			   ^" by: "^(Clause.to_string_tptp main_clause)));
       List.iter (assign_dead_and_remove_from_indexes sim_state) subsumed_clauses;
       (*out_str ("Is simpl"^(Clause.to_string main_clause)^"\n"); *)
(*       let sim_main = sim_add_feat_clause sim_state feature_list_opt main_clause in *)
(*       List.iter
	 (fun c -> 	    ()
	   clause_register_subsumed_by ~by:main_clause c 
	 ) subsumed_clauses; 
*)
      )
    else ());      
    subsumed_clauses
  with
    SubsetSubsume.No_subsumed -> []
	
(*

*)

(*--------------- adding to sim_state --------------------*)
(* feature_list_opt is Some if  sim_use_sub_index is true *)
    
let sim_add_clause ?(after_bwd_ss=false) sim_state clause = 
  check_empty_clause clause;
  (* if (sim_mem_clause sim_state clause) 
  then *)
  try
    let old_clause = context_find sim_state.sim_context clause in
    (old_clause,[])
  with Not_found ->
    try   
      let by = forward_subset_subsume_by sim_state clause in 
      (by,[]) 
    with Not_found -> (
      if sim_state.sim_opt.sim_add_to_prop_solver then (
        Prop_solver_exchange.add_clause_to_solver clause;
       );
      (* will be picked up later by sup. and added to smt *)
      if sim_state.sim_opt.sim_add_to_smt_solver then (
       Shared_clauses.smt_add_shared_clause clause;
       );
        
      let clause_to_add = clause in
      (* if sim_state.sim_opt.sim_copy_clauses 
      then
        Clause.copy_clause clause 
      else
        clause  
      in *)
      let context_added_clause = context_add sim_state.sim_context clause_to_add in
      let bwd_ss_subsumed =
        if after_bwd_ss then (
          [] 
        ) else (
          let subsumed_clauses =
            backward_subset_subsume_without_adding sim_state context_added_clause 
          in
          subsumed_clauses
        )
      in          
      if sim_state.sim_opt.sim_use_ss_index then (
        add_to_ss_index sim_state context_added_clause
      );
      if sim_state.sim_opt.sim_use_sub_index
      && sim_state.sim_opt.sim_add_to_sub_index_test context_added_clause
      then (
        add_to_sub_index sim_state context_added_clause
      );
      (* if sim_state.sim_opt.sim_use_demod then (
        dbg D_demodulation @@ lazy (Clause.to_string_tptp clause_to_add);
        add_to_demod_equations sim_state clause_to_add;
        add_to_bwd_demod_index sim_state clause_to_add;
      ); *)
      (context_added_clause, bwd_ss_subsumed)
    ) 
	

let backward_subset_subsume sim_state main_clause =
  let ss_subsumed = backward_subset_subsume_without_adding sim_state main_clause in
(*
  (if (not (ss_subsumed = []))
  then 
    (ignore (sim_add_clause ~after_bwd_ss:true sim_state main_clause);)
  else 
    ()
  );
*)
  ss_subsumed

(* input is of type context_list *)
(* let sim_create_from_context_list sim_options context = *)

let sim_create_from_context sim_options context = 
  let sim_state = sim_create sim_options in
  let f clause = 
    ignore(sim_add_clause sim_state clause)
  in
  context_iter context ~non_dead:true f;  
  sim_state


(*

let sim_create_from_context sim_options context =
  sim_create_from_context_list sim_options (Clause.CL_Context context) 


let sim_create_from_list sim_options clause_list =
  sim_create_from_context_list sim_options (Clause.CL_List clause_list) 

*)


(*------ simplifications can raise Eliminated/Empty_Clause *)  
    
(*-------- equality resolution simplification -------*)
    
(* can not use equality_resolution with axiomtic equality! only in preprocessing before eq axioms are added *)

let equality_resolution clause =
  let new_clause = (Inference_rules.unflatten_clause clause) in
  if Clause.Bc.(new_clause == clause)
  then
    clause
  else
    begin
      incr_int_stat 1 res_num_eq_res_simplified;
      dbg D_eq_res (lazy (Clause.to_string_tptp clause));
      new_clause
    end

let equality_resolution_simp clause =
  let new_clause = (Inference_rules.equality_resolution_simp clause) in
  if Clause.Bc.(new_clause == clause)
  then
    clause
  else
    begin
      incr_int_stat 1 res_num_eq_res_simplified;
      dbg D_eq_res (lazy (Clause.to_string_tptp clause));
      new_clause
    end


let tautology_elim clause = 
  if (Inference_rules.is_tautology clause)
  then
    ((* out_str_debug
	("Simplified tautology: "
	^(Clause.to_string clause));*)
      dbg D_taut (lazy (("tautology:")^(Clause.to_string_tptp clause)));
     incr_int_stat 1 res_tautology_del;
     raise Eliminated)
  else
    clause

(* can not use eq_tautology elim with axiomtic equality! only  in preprocessing before eq axioms are added *)
let eq_tautology_elim clause = 
  if (Inference_rules.is_eq_tautology clause)
  then
    ((* out_str_debug
	("Simplified tautology: "
	^(Clause.to_string clause));*)
      dbg D_taut (lazy (("eq tautology:")^(Clause.to_string_tptp clause)));
     incr_int_stat 1 res_tautology_del;
     raise Eliminated)
  else
    clause


(*---------self simplify-------------*)

(* tautology/equality resolution can raise Eliminated; only sound in preprocessing before adding axioms of eq. ! *)
let self_simplify_prep clause =
  let new_clause = equality_resolution clause in
  ignore(tautology_elim new_clause);
  eq_tautology_elim new_clause 
    


(*
let consistent_with_assumptions_some_lits lits = 
  List.exists    
    (fun l ->       
      let compl_l = (add_compl_lit l) in
      not (Prop_solver_exchange.mem_norm_assumptions compl_l)
    )
    lits
  *)

(* inconsistent with solver norm assumptions   *)
(* can raise Unsatisfiable_gr *)
(* eq_tras_flag is true then reverse non-eq -> to_eq transformaiton before checking assumptions *)

let get_lits_eq_trans ~eq_trans_flag clause = 
  if eq_trans_flag 
  then
    List.map EqualityTransformation.lit_to_eq_rev (Clause.get_lits clause)
  else
    (Clause.get_lits clause)
    
let inconsistent_with_solver_norm_assumptions ~eq_trans_flag clause = 
  if not (Prop_solver_exchange.is_empty_norm_assumptions ()) 
  then 
    (     
     let lits = get_lits_eq_trans ~eq_trans_flag clause in
     let (consist_lits, _inconsist) = 
       Prop_solver_exchange.split_consistent_with_assumptions ~soft:false ~sim:false lits in    

     if List.X.is_empty consist_lits (* inconsistent clause*)
     then
       (
        Prop_solver_exchange.add_clause_to_solver clause; (* adds both version eq_trans/trans_reversed. *)
        if Prop_solver_exchange.solve ~soft:false () == Prop_solver_exchange.PropSolver.Unsat 
        then 
          raise Unsatisfiable_gr
        else 
          (
           (failwith "inconsistent_with_assumptions: should be unsat")
          )
       )  
         )
  else
    ()
      

let prop_assumptions_tautology ~eq_trans_flag clause = 
  if not (Prop_solver_exchange.is_empty_norm_assumptions ()) 
  then     
    let lits = get_lits_eq_trans ~eq_trans_flag clause in
    if (List.exists Prop_solver_exchange.mem_norm_assumptions lits) 
    then 
      (                     
            dbg D_taut (lazy (("assumptions tautology:")^(Clause.to_string_tptp clause)));
            incr_int_stat 1 res_tautology_del;
            raise Eliminated      
         )
    else
      (
       clause
      )
  else
    clause

(*---------*)
let forward_prop_subsume (* sim_state *) clause = 
  let new_clause = Prop_solver_exchange.prop_subsumption (* (Def(sim_state.sim_context))*) clause in
  (if (not Clause.Bc.(new_clause == clause))
  then
    (
     dbg D_prop_subs (lazy (("old_clause: ")^(Clause.to_string_tptp clause)));
     dbg D_prop_subs (lazy (("new_clause: ")^(Clause.to_string_tptp new_clause)));
     check_empty_clause new_clause;     
    )
  );
  new_clause



(*
(* can raise Eliminated/Empty_Clause *)  
(*-----------------------------------*)
  let simplify_light_forward sim_state clause =
  if (is_subset_subsumed sim_state clause)
  then
  ((* out_str
      ("Subset_subsumed: "
      ^(Clause.to_string clause)); *)
  incr_int_stat 1 res_forward_subset_subsumed;
  raise Eliminated)
  else
  if sim_state.add_to_prop_solver
  then
  (
  Prop_solver_exchange.add_clause_to_solver clause;
  let new_clause = Prop_solver_exchange.prop_subsumption clause in
  if (not (new_clause == clause))
  then
  (
  Inference_rules.check_empty_clause new_clause;
  if (is_subset_subsumed sim_state new_clause)
  then
  (incr_int_stat 1 res_forward_subset_subsumed;
  raise Eliminated)
  else new_clause
  )
  else
  clause
  )
  else
  clause
 *)


(*-----------Forward subsumption resolution---------------*)

	
(* returns new list of lits which is obtained by all possible cuts*)
(* we also keep subsumed by list to add to history later *)
	
let rec forward_subs_res_list sim_state subs_by_list_ref tried_lits rest =
  match rest with
  | h:: tl ->
      let compl_h = add_compl_lit h in
      let tstp_source = Clause.tstp_source_tmp in (* replace later with lit_list*)
      let clause_to_try = Clause.create_clause_raw tstp_source (tried_lits@(compl_h:: tl)) in
      (* out_str ("clause_to_try: "^(Clause.to_string clause_to_try)^" "
	 ^(feature_list_to_string feature_list)^"\n");*)
      (match
	(SubsumptionIndexM.is_subsumed ~subs_bck_mult:sim_state.sim_opt.sim_subs_bck_mult
	   sim_state.sim_sub_index clause_to_try)
      with
      | Some((by_cl, _subst)) ->
	  (incr_int_stat 1 res_forward_subsumption_resolution;
	   subs_by_list_ref:= by_cl:: (!subs_by_list_ref);
	   forward_subs_res_list sim_state subs_by_list_ref tried_lits tl)
	    (* we do not need to retry tried lits after elimination of a literal *)
	    (*	   forward_subs_res_list subs_by_list_ref [] (tried_lits@tl))*)
      | None ->
	  forward_subs_res_list sim_state subs_by_list_ref (tried_lits@[h]) tl   (* TODO remove concatenation *)
      )
  |[] -> tried_lits
	
(*-----------------------------------*)
(* can rise Unsatisfiable, Eliminated*)
let forward_subs_res sim_state clause =
  (*  out_str ("Try: "^(Clause.to_string clause)^"\n");*)
  let lits = get_lits clause in
  let subs_by_list_ref = ref [] in
  let new_lits = forward_subs_res_list sim_state subs_by_list_ref [] lits in
  if List.X.is_nonempty !subs_by_list_ref
  then
    (
     let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause (!subs_by_list_ref) in
     let new_clause = create_clause tstp_source new_lits in

     assert (not (Clause.equal_bc new_clause clause));
     dbg D_fwd_subs_res (lazy ((Clause.to_string_tptp clause)^" by: "^(Clause.to_string_tptp new_clause)));     

(*     clause_register_subsumed_by ~by:new_clause clause;*)

     (* Clause.inherit_param_modif clause new_clause;
	Clause.set_bool_param true Clause.res_simplifying new_clause;*)
     (* Clause.assign_forward_subsumption_resolution_history
	new_clause clause (!subs_by_list_ref); *)
     (* Clause.assign_tstp_source_forward_subsumption_resolution
	new_clause clause (!subs_by_list_ref); *)
     (* out_str ("Elim: "^(Clause.to_string clause)^"\n");
	out_str ("New: "^(Clause.to_string new_clause)^"\n");
	out_str ("By: "^(Clause.clause_list_to_string !subs_by_list_ref)^"\n");*)

(* forward; should not assign dead without returing subsumed to the caller *)
(* 
     (if (not (Clause.equal_bc new_clause clause))
     then
       (assign_dead_and_remove_from_indexes sim_state clause;)
     );
*)
     new_clause
    )
  else
    clause
      

(*------------Forward Subsumption--------------------*)

let pre_cond_true_fun = BasicSubsumptionIndex.pre_cond_true_fun 
      
let forward_subs ?(pre_cond = pre_cond_true_fun)  sim_state clause = 

  (* do not need light simplifications since light backward *)
  assert (sim_state.sim_opt.sim_use_sub_index);
  match
    (SubsumptionIndexM.is_subsumed ~pre_cond ~subs_bck_mult:sim_state.sim_opt.sim_subs_bck_mult
       sim_state.sim_sub_index clause)
  with
  | Some((by_cl, _subst))  -> 
      (
       incr_int_stat 1 res_forward_subsumed;

(*       clause_register_subsumed_by ~by: by_cl clause; *)
       
       (*  Clause.set_bool_param true Clause.res_simplifying by_cl;*)
       (* we can eliminate since subs. is proper since light simplifications *)

(* forward; should not assign dead without returing subsumed to the caller *)
(*
       (if (not (Clause.equal_bc by_cl clause))
       then
	 assign_dead_and_remove_from_indexes sim_state clause;
       );
*)
       (*debug*)

       dbg D_fwd_subs (lazy ((Clause.to_string_tptp clause)^" by: "^(Clause.to_string_tptp by_cl)));

       (*end debug*)
       (*       out_str ("Is subsumed: "^(Clause.to_string clause)^"\n");*)
       raise Eliminated)
  | None   -> clause
	

let subs_strict_pre_cond ~cl_in ~cl_by = (Clause.length cl_by) < (Clause.length cl_in) 

let forward_subs_strict sim_state clause = 
  try
    forward_subs ~pre_cond:subs_strict_pre_cond sim_state clause   
  with 
    Eliminated ->
      ( dbg D_fwd_subs_strict (lazy (" subsumed: "^(Clause.to_string_tptp clause)));
        raise Eliminated
       )

(*
let forward_subs sim_state clause =
  forward_subs_feature sim_state (get_feature_list clause) clause
*)
 (* do subsumption only by clauses of length smaller or equal to a given length *)
 (* we assume that the first feature is always length! *)
(*
let forward_subs_by_length sim_state length feature_list clause =
  let new_feature_list =
    match feature_list with
    | _:: rest -> length:: rest
    | _ -> failwith "Discount: get_feature_for_unit_subs "
  in
  forward_subs_feature sim_state new_feature_list clause
  *)
  
(*---------------*)

(*------------Backward Subsumption Resolution--------------------*)
    
let rec remove_lit lit lits =
  List.filter (fun x -> not (x == lit)) lits
    
let apply_lit_cut sim_state given_clause subsumed_subst_list lit =
  let f subsumed_and_new_clause_list (subsumed, subst) =
    incr_int_stat 1 res_backward_subsumption_resolution;
    let lits = get_lits subsumed in
    let lit_to_cut = Subst.apply_subst_term term_db_ref subst lit in
    let new_lits = remove_lit lit_to_cut lits in
    
    let tstp_source = Clause.tstp_source_backward_subsumption_resolution subsumed [given_clause] in
    let new_clause = create_clause tstp_source new_lits in

    if (not (Clause.equal_bc new_clause subsumed))
    then
      begin
(*	let feature_list_new_clause_opt = Some (get_feature_list new_clause) in *)
	dbg D_bwd_subs_res (lazy ((Clause.to_string_tptp subsumed)^" by: "^(Clause.to_string_tptp new_clause)));
	
(* here subsumed is always different from new_clause since we cut a literal; therefore we do not need to check this *)

(*	clause_register_subsumed_by ~by:new_clause subsumed; *)
    
	assign_dead_and_remove_from_indexes sim_state subsumed;	
	
(*
	let (sim_new_clause,subset_subsumed_list) 
	    = sim_add_clause sim_state new_clause in
*)


(* we have to do backward_subset_subsume since otherwise we can not add sim_new_clause into ss_index, *)
(* raises exception Subsumes *)
(*	let subset_subsumed_list = backward_subset_subsume sim_state feature_list_new_clause_opt sim_new_clause in*)
    (* Clause.inherit_param_modif subsumed new_clause;*)
      
      (* out_str ("Back_subsed: "^(Clause.to_string subsumed)
	 ^" Lit to cut: "^(Term.to_string lit_to_cut)
	 ^"\nNew clause: "^(Clause.to_string new_clause));*)


(*	    ((subsumed::subset_subsumed_list), sim_new_clause):: subsumed_and_new_clause_list *)
	    (([subsumed]), new_clause):: subsumed_and_new_clause_list 
      end
    else
      subsumed_and_new_clause_list
  in
  List.fold_left f [] subsumed_subst_list
    
    (*(subsumed_list, new_clauses_list)*)
    
let rec backward_subs_res_list sim_state given_clause tried_lits rest =
  match rest with
  | h:: tl ->
      let compl_h = add_compl_lit h in
      let tstp_source = Clause.tstp_source_tmp in (* replace later with lit_list*)
(*      let clause_to_try = create_clause tstp_source (tried_lits@(compl_h:: tl)) in *)

 (* important to use raw cluase here otherwise the reference to compl_h gets mixed *)
      let clause_to_try = Clause.create_clause_raw tstp_source (tried_lits@(compl_h:: tl)) in 

(*      let feature_list = get_feature_list clause_to_try in *)
      (*      out_str ("backward clause_to_try: "^(Clause.to_string clause_to_try)^"\n");*)
      let subsumed_subst_list =
	(SubsumptionIndexM.find_subsumed_subst 
           ~subs_bck_mult:sim_state.sim_opt.sim_subs_bck_mult
	   sim_state.sim_sub_index clause_to_try) in

      let add_subsumed_and_new_clause_list =
	apply_lit_cut sim_state given_clause subsumed_subst_list compl_h in

      let rest_subsumed_and_new_clause_list =
	backward_subs_res_list sim_state given_clause (tried_lits@[h]) tl in
      add_subsumed_and_new_clause_list@rest_subsumed_and_new_clause_list

  | [] -> []


(* backward_subs_res returns list of pairs (subsumed_clause, new_clause)  *)
(* subsumed_clause is removed from indexes; declared dead; but remain in sim_sate *)	
(* new_clause is added to sim_state; so a copy may need to be created if put to other contexts *)

let backward_subs_res sim_state given_clause =
  assert(sim_state.sim_opt.sim_use_sub_index);
  
  let lits = get_lits given_clause in
  let subsumed_and_new_clause_list = backward_subs_res_list sim_state given_clause [] lits in
  subsumed_and_new_clause_list
(*
  let f (_subsumed, new_clause) =
    (* (* Clause.assign_backward_subsumption_resolution_history
	  new_clause [clause; subsumed]; *)
       Clause.assign_tstp_source_backward_subsumption_resolution
       new_clause [clause; subsumed];
       Clause.set_bool_param true Clause.res_simplifying new_clause;
       eliminate_clause subsumed;*)
    add_new_clause_to_passive new_clause
  in
  List.iter f subsumed_and_new_clause_list
  *)
  
    (*debug*)
    (* if not (subsumed_list = []) then
       ( out_str ("\n Back subsumed by: "^(Clause.to_string clause)^"\n");
       List.iter (fun c -> out_str ("Eliminated: "^(Clause.to_tptp c)^"\n"))
       subsumed_list;
       List.iter (fun c -> out_str ("New clauses: "^(Clause.to_tptp c)^"\n"))
       new_clauses)
       else ()
     *)
    
(*------------Backward Subsumption--------------------*)

(* returns list of subsumed clauses *)
    
let backward_subs_full sim_state clause =
  assert(sim_state.sim_opt.sim_use_sub_index);

  let b_subsumed_list =
(*------filter different-------*)
    filter_different clause (SubsumptionIndexM.find_subsumed            
                               ~subs_bck_mult:sim_state.sim_opt.sim_subs_bck_mult
			      sim_state.sim_sub_index clause) in
  if b_subsumed_list != []
  then
    (
       (*out_str ("Is simpl"^(Clause.to_string main_clause)^"\n"); *)
     let (sim_clause, ss_subsumed) = sim_add_clause sim_state clause in
 
     List.iter (fun subsumed ->       
       (* clause_register_subsumed_by ~by:sim_clause subsumed;	*)				

       dbg D_bwd_subs (lazy ((Clause.to_string_tptp subsumed)^" by: "^(Clause.to_string_tptp sim_clause)^(" Claus.bc_euqal:  ")^(string_of_bool (Clause.equal_bc subsumed sim_clause))));
(*       remove_from_indexes sim_state subsumed; *)
       assign_dead_and_remove_from_indexes sim_state subsumed; 
	       ) b_subsumed_list;

     incr_int_stat (List.length b_subsumed_list) res_backward_subsumed;
     ((filter_different sim_clause ss_subsumed)@b_subsumed_list)
    )
  else []
      
let backward_subs_by_length sim_state length clause =
  if ((Clause.length clause) <= length)
  then
    backward_subs_full sim_state clause
  else 
    []
  
(*    
let simplify_backward feature_list clause =
  (match !current_options.res_backward_subs with
  | Subs_Full -> backward_subs_full feature_list clause
  | Subs_By_Length (length) -> backward_subs_by_length length feature_list clause
  | Subs_Subset -> ()
  );
  if !current_options.res_backward_subs_resolution
  then
    backward_subs_res clause
  else ()
*)

(*
  let simplify_forward sim_state feature_list clause =
  let forward_subs =
  match sim_state.sim_options.sim_forward_subs
  with
  | Subs_Full ->
  forward_subs_feature sim_state feature_list clause
  | Subs_By_Length (length) -> failwith "Uncomment"
  
  (* uncomment later, compress subs. test *)
  (*
    forward_subs_by_length length feature_list prop_simplified
   *)
  | Subs_Subset -> clause (* assume that light simplification is already applied *)
  in
  if sim_state.sim_options.sim_forward_subs_resolution
  then forward_subs_res sim_state forward_subs
  else forward_subs
 *)    
(*

  let preprocess_new_clause clause =
  check_empty_clause clause;
  if (not (Clause.get_is_dead clause))
  then
  (
  (* (if (!current_options.res_prop_simpl_new
     || !current_options.res_prop_simpl_given
     || !current_options. !add_passive_to_exchange_flag) *)
  (match !current_options.res_to_prop_solver with
  | Res_to_Solver_Passive ->
  Prop_solver_exchange.add_clause_to_solver clause
  | _ -> ());
  let main_clause = simplify_light_forward_new clause in
  (*	let clause_ls = simplify_light_forward_new clause in
    let (_feat_list, main_clause) = all_simplifications clause_ls in *)
  (if (*!add_passive_to_exchange_flag &&*) (not (main_clause == clause))
  then
  (match !current_options.res_to_prop_solver with
  | Res_to_Solver_Passive ->
  Prop_solver_exchange.add_clause_to_solver clause
  | _ -> ())
  );
  simplify_light_backward_new main_clause;
  let added_clause = context_add !context main_clause in
  (*	Clause.assign_when_born (get_val_stat res_num_of_loops) added_clause;*)
  (* Clause.assign_conjecture_distance
     (Clause.get_min_conjecture_distance [added_clause; main_clause])
     added_clause;	*)
  add_to_ss_index added_clause;
  added_clause
  )
  else
  raise Eliminated

 *)



(* ------------ *)
(* Demodulation *)
(* ------------ *)

(********** Deprecated

(** Demodulate clause via equations in index. Returns [Some demodulated_clause] if possible, or [None] if no demodulation can be done *)
let demodulate_forward ~demod_completeness_check_type index (clause:clause) : clause option =
  dbg D_demodulation @@ lazy "demodulate_forward";

  let terms = ref TMap.empty in
  let global_check = ref true in  (* If any is nonequality then never need to check *)

  let add_to_terms terms l ~check =
    (* If global_check is false then don't waste time, it's going to be None anyways *)
    if Bool.O.(!global_check = false) then
      terms := TMap.add l None !terms
    else
      match check with
      (* If there is one place where check is not needed, then it's not needed at all *)
      | None -> 
        terms := TMap.add l None !terms
      (* If it is required, then update *)
      | Some t ->
        terms := TMap.update l (fun prev ->
          match prev with
          (* Was already found to be not needed *)
          | Some (None) -> prev
          (* Was already found to be needed *)
          | Some (Some t_prev) -> prev
          (* Didn't show up yet *)
          | None -> Some check
        ) !terms
  in

  Clause.get_lits clause |> List.iter (fun lit ->
    match Term.decompose_eq_atom @@ Term.get_atom lit with
    | [etype;l;r] -> 
      if Term.get_top_symb etype == Symbol.symb_bool_type then 
        global_check := false;
      if Term.get_top_symb etype != Symbol.symb_bool_type then (
        (* add_to_terms terms l ~check:(Some r);
        add_to_terms terms r ~check:(if Term.is_oriented_eq_lit lit then None else Some l); *)
        match is_oriented_kbo lit with
        | Orderings_cache.LeftGreater ->
          add_to_terms terms l ~check:(Some r);
          add_to_terms terms r ~check:(None);
        | Orderings_cache.RightGreater ->
          add_to_terms terms l ~check:(None);
          add_to_terms terms r ~check:(Some l);
        | Orderings_cache.NotOriented ->
          add_to_terms terms l ~check:(Some r);
          add_to_terms terms r ~check:(Some l);
      );
      l |> Term.iter_subterms_preorder_novar (fun x ->
        add_to_terms terms x ~check:None
      );
      r |> Term.iter_subterms_preorder_novar (fun x -> 
        add_to_terms terms x ~check:None
      )
    | [] -> 
      global_check := false;
      lit |> Term.iter_subterms_preorder_novar (fun x -> add_to_terms terms x ~check:None);
      (* failwith "add_clause: non-equality literal" *)
    | _ -> assert false
  );

  try
    !terms |> TMap.iter (fun s do_check ->
      let candidates = DemodulationIndex.get_fwd_candidates index s in
      candidates |> List.iter (fun (_, lst) ->
        dbg D_candlength @@ lazy (sprintf "length lst %d" (List.length lst));
        lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
          if not @@ Clause.equal_bc clause eq_clause then (
            match Term.decompose_eq_atom eq_lit with
            | [_;l;r] ->
              let l,r = Term.Eq.regularize_pos pos l r in
(*              let check_type = !current_options.demod_completeness_check in *)
              if !global_check then
                Inference_rules.demodulation ~demod_completeness_check_type ~do_check eq_clause eq_lit l r s clause
              else
                Inference_rules.demodulation ~demod_completeness_check_type ~do_check:None eq_clause eq_lit l r s clause
            | _ -> assert false
          )
        )
      )
    );
    None
  with Inference_rules.Return conclusion -> 
    dbg D_demodulation @@ lazy (sprintf "Forward demodulated %s" (Clause.to_string_tptp clause));
    dbg D_demodulation @@ lazy (sprintf "to %s" (Clause.to_string_tptp conclusion));
    Some conclusion

let demodulate_forward ~demod_completeness_check_type state clause =
  assert (state.sim_opt.sim_use_demod);
  match demodulate_forward ~demod_completeness_check_type state.sim_demod_equations clause with
  | Some new_clause -> new_clause
  | None -> clause

(*
(** Returns [Some simplified_clause] or [None] if tautology at any point *)
(** Returns simplified clause *)
let demodulate state clause =
  let rec loop clause =
    (* if BCSet.mem clause state.sim_intermediate_seen then (
      dbg D_demodulation @@ lazy "seen";
      Statistics.(incr_int_stat 1 sup_fw_demodulated);
      raise Eliminated
    ) else *) (

      (* state.sim_intermediate_seen <- BCSet.add clause state.sim_intermediate_seen; *)
      match demodulate_clause state.sim_demod_equations clause with
      | Some x -> 
        Statistics.(incr_int_stat 1 sup_fw_demodulated);
        loop x
      | None -> 
        dbg D_demodulation @@ lazy "done";
        clause
    )
  in
  loop clause
*)


(** Demodulate clauses in index via equality clause eq. Returns list of demodulated clauses *)
let demodulate_backward ~demod_completeness_check_type state eq_clause =
  assert (state.sim_opt.sim_use_demod);
  dbg D_demodulation @@ lazy "demodulate_backward";
  
  let demodulate_via old_ref new_ref eq_lit l r = 
    let candidates = DemodulationIndex.get_bwd_candidates state.sim_demod_equations l in
    candidates |> List.iter (fun (s, lst) ->
      dbg D_candlength @@ lazy (sprintf "length lst %d" (List.length lst));
      lst |> List.iter (fun (clause, do_check) ->
        if clause != eq_clause then (
          (* let conclusion = fix_point (fun x ->
            dbg D_demodulation @@ lazy (sprintf "iter x=%s" (Clause.to_string_tptp x));
            try
              Inference_rules.demodulation eq_clause eq_lit l r s x;
              x
            with Inference_rules.Return x' ->
              x'
          ) clause
          in *)

          begin try
            Inference_rules.demodulation ~demod_completeness_check_type ~do_check eq_clause eq_lit l r s clause;
            ()
          with Inference_rules.Return conclusion -> 
            dbg D_demodulation @@ lazy (sprintf "Backward demodulated %s" (Clause.to_string_tptp clause));
            dbg D_demodulation @@ lazy (sprintf "to %s" (Clause.to_string_tptp conclusion));
            (* Remove old clause from sim_state *)
            assign_dead_and_remove_from_indexes state clause;
            (* Add new clause to sim_state *)
            (* ignore @@ sim_add_clause state conclusion; *)
            (* Cons new clause onto new_ref *)
            ListExtra.cons_ref conclusion new_ref;
            ListExtra.cons_ref clause old_ref;
          end
        )
      )
    )
  in

  match Clause.get_lits eq_clause with
  | [eq_lit] ->
    begin match Term.decompose_eq_atom eq_lit with
    | [_;l;r] -> 
        let old_ref = ref [] in
        let new_ref = ref [] in

        begin match is_oriented_kbo eq_lit with
        | Orderings_cache.LeftGreater ->
          demodulate_via old_ref new_ref eq_lit l r;
        | Orderings_cache.RightGreater ->
          demodulate_via old_ref new_ref eq_lit r l;
        | Orderings_cache.NotOriented ->
          (* Could also choose not to try via unoriented equations *)
          demodulate_via old_ref new_ref eq_lit l r;
          demodulate_via old_ref new_ref eq_lit r l;
        end;

        !old_ref, !new_ref
    | _ -> ([],[])
    end
  | _ -> ([],[])

**********)
