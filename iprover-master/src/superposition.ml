open Lib
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr =
  | D_trace
  | D_trace2
  | D_selection
  | D_memoisation
  | D_orienting
  | D_candlength
  | D_prog_bar
  | D_prop_solver
  | D_fun_split
  | D_fun_split2
  | D_scores
  | D_ext_agent
  | D_add_to_smt_outside
      
let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_trace2 -> "trace:lv2"
  | D_selection -> "selection"
  | D_memoisation -> "memoisation"
  | D_orienting -> "orienting"
  | D_candlength -> "candlength"
  | D_prog_bar -> "prog_bar"
  | D_prop_solver -> "prop_solver"
  | D_fun_split -> "fun_split"
  | D_fun_split2 -> "fun_split"
  | D_scores -> "scores"
  | D_ext_agent -> "ext_agent"
  | D_add_to_smt_outside -> "add_to_smt_outside"
        
let dbg_groups = [
(*  D_add_to_smt_outside; *)
  
  D_trace;
  (*
  (* D_trace2; *)
  D_selection;
  (* D_memoisation; *)
  D_orienting;
  (* D_candlength; *)
  D_prop_solver;
(*  D_prog_bar; *)
  D_fun_split;
  D_ext_agent
    (* D_fun_split2; *)
    *)
  (***** D_scores; *****)
]



(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



(* Use optimised ordering *)
(* module Orderings = Orderings_opt *)

(* Use Simplify_new *)

module Simplify_old = Simplify 

module Simplify = Simplify_new



(*------------- prop solver assumptions (KK) ---------*)

(*----- we assume that we add only  EqualityTransformation.lit_to_eq_rev lits to prop solver assumptions *)

(* From old simplify.ml *)
(* check that consistent with solver assumptions *)
(* can raise Unsatisfiable_gr *)

let inconsistent_with_solver_norm_assumptions = 
  Simplify_old.inconsistent_with_solver_norm_assumptions 

(* This function may raise [Empty_clause] or [Unsatisfiable_gr]. *)
let check_empty_clause_return clause = 
  (* try  *)
  check_empty_clause clause;
  inconsistent_with_solver_norm_assumptions ~eq_trans_flag:false clause
  (* Simplify.Simplified clause
  with Eliminated -> 
    Simplify.Eliminated *)

let prop_assumptions_tautology clause = 
  try 
    Simplify.Simplified (Simplify_old.prop_assumptions_tautology ~eq_trans_flag:false clause)
  with Eliminated -> 
    Simplify.Eliminated []  (* TODO: No by_clauses *)

let get_lits_consist_assumptions clause =   
  if Prop_solver_exchange.is_empty_norm_assumptions () then 
    Clause.get_lits clause
  else begin
    check_empty_clause_return clause;
    let lits = (* List.map EqualityTransformation.lit_to_eq_rev *) (Clause.get_lits clause) in 
    let (consist_lits, inconsist_lits) = 
      Prop_solver_exchange.split_consistent_with_assumptions ~soft:false ~sim:false lits
    in
    dbg D_prop_solver @@ lazy (sprintf "consist_lits: %s" (Term.term_list_to_string consist_lits));
    dbg D_prop_solver @@ lazy (sprintf "inconsist_lits: %s" (Term.term_list_to_string inconsist_lits));
(*    ignore (check_empty_clause_return clause); *)
    assert (List.X.is_nonempty consist_lits); (* assume check_empty_clause_return was applied to the clause *)
    (* List.map EqualityTransformation.lit_to_eq *) consist_lits
  end

(* get_lits should still return all lits; e.g., fun_splitting would be incorrect in the presence of assumptions *)
(*
let get_lits clause = get_lits_consist_assumptions clause 
*)

(*----------------*)



(* --------- *)
(* Selection *)
(* --------- *)

module Selection = struct
  (* let is_split_eq t =
    match Term.decompose_eq_atom @@ Term.get_atom t with
    | [_;l;r] -> Term.is_split_lit l
    | [] -> false
    | _ -> assert false *)

  (** Returns one negative literal, or raises Not_found if no such literal exists *)
  let one_negative c =
    let lits = get_lits_consist_assumptions c in
    (* try
      List.find (fun x -> Term.is_neg_lit x && is_split_eq x) lits
    with Not_found -> *)
      List.find Term.is_neg_lit lits

  (** Returns all maximal literals *)
  let all_maximal ~order_lits c =
    let lits = get_lits_consist_assumptions c in
    (* let nonsplit_lits = List.filter (fun x -> not @@ is_split_eq x) lits in *)
    let result = 
      (* if nonsplit_lits <> [] then
        list_get_max_elements_v Orderings.simple_kbo nonsplit_lits 
      else *)
        ListExtra.max_elements_partial_ord order_lits lits 
    in
    dassert (fun () -> List.X.is_nonempty result);
    result

  (** Selects one negative literal, or else all maximal *)
  let negative_or_maximal ~order_lits c =
    try
      [one_negative c]
    with Not_found -> 
      all_maximal ~order_lits c



  (* Alternative method: uses quality ordering *)
  let comp_neq = 
    (* Ord.lift (fun x -> Term.Eq.right x != SystemDBs.top_term && Term.Eq.is_neg_eq x) Bool.compare *)
    Ord.lift Term.Eq.is_neg_eq Bool.compare

  let comp_weight =
    Ord.lift Term.get_num_of_symb_skipneg Int.compare

  let comp_neg =
    Ord.lift Term.is_neg_lit Bool.compare

  let comp_tiebreaker a b = 
    Term.compare b a  (* Intuition may be prefer later terms, which are probably more complex *)

  (* KK: check lits in assumptions for abstr_ref, bmc etc. *)
  (* can raise Unsatisfiable_gr *)
  (*
  let get_relevant_lits clause = 
    let lits = get_lits_consist_assumptions clause in 
    let (consist_lits, _inconsist) = Prop_solver_exchange.split_consistent_with_assumptions ~soft:false ~sim:false lits in

    (if List.X.is_empty consist_lits then 
      match (Prop_solver_exchange.solve ~soft:true ()) with
      | Prop_solver_exchange.PropSolver.Unsat ->
        dbg D_prop_solver (lazy "get_relevant_lits:Unsatisfiable_gr");
        raise Unsatisfiable_gr
      |_-> assert (false)
    );
    consist_lits 
    *)

  let quality_ordering = 
    lex_combination4 comp_neq comp_weight comp_neg comp_tiebreaker

  let quality_selection lits = 
    let result = list_find_max_element quality_ordering lits in
    (* let result' = list_get_max_elements_v quality_ordering clause in
    assert (
      match result' with
      | [x] -> result == x
      | _ -> false
    ); *)
    result



  let rec quality_selection_complete ~order_lits lits =
    let n = quality_selection lits in
    dbg D_selection @@ lazy (sprintf "Quality selection: %s" (Term.to_string n));
    if Term.is_neg_lit n then (
      [n]
    ) else (
      dbg D_selection @@ lazy (sprintf "Not neg");
      let maximal_set = ListExtra.max_elements_partial_ord (order_lits) lits in
      (* maximal_set |> List.iter (fun x -> 
        dbg D_trace @@ lazy (sprintf "maximal %s" (Term.to_string x));
        assert (not @@ List.exists (fun y -> dbg D_trace @@ lazy (sprintf "  %s" (Term.to_string y)); KBO.Lits.(y > x)) lits);
        assert (not @@ List.exists (fun y -> dbg D_trace @@ lazy (sprintf "  %s" (Term.to_string y)); KBO.Lits.(x < y)) lits);
      ); *)
      dbg D_selection @@ lazy (sprintf "Maximal set: %s" (list_to_string Term.to_string maximal_set " , "));
      let all_positive = List.for_all Term.is_pos_lit maximal_set in
      if all_positive && List.memq n maximal_set then
        maximal_set
      else if not all_positive then
        [quality_selection (List.filter Term.is_neg_lit maximal_set)]
      else
        quality_selection_complete ~order_lits (ListExtra.removeq n lits)
    )


  let quality_selection_complete ~order_lits clause =
    let lits = get_lits_consist_assumptions clause in (* get_relevant_lits clause in *) (* only consistent with solver assumptions *)
    match lits with 
    | [x] -> lits
    | _::_ -> quality_selection_complete ~order_lits lits
    | [] -> failwith (sprintf "quality_selection_complete: no consistenet with assumption lits: %s" (Clause.to_string clause));
        

end

let sel_clause = 
  (* Selection.negative_or_maximal *)
  try 
  Selection.quality_selection_complete
  with
    Simplify_new.Set.Is_mem -> failwith "sel_clause: Simplify_new.Set.Is_mem"


(* --------------- *)
(* Data structures *)
(* --------------- *)

(** Clause + extra data *)
module Clause' = struct
  type t = {
    clause: clause;
    sel: lit list;
  }

  let create ~order_lits c =
    let sel = sel_clause ~order_lits c in
    dbg D_trace @@ lazy (sprintf "Selection: %s" (list_to_string Term.to_string sel " | "));
    {
      clause = c;
      sel;
    }
end

type clause' = Clause'.t



(** Memoise calls, skipping terms already computed *)
(* Unused, probably worse performance *)
(* let memoise iter f (t:term) =
  let memo = ref TSet.empty in  
  dbg D_memoisation @@ lazy ("memoised function call:");
  iter (fun x ->
    dbg D_memoisation @@ lazy (sprintf "memo %s" (Term.to_string x));
    if not @@ TSet.mem x !memo then (
      memo := TSet.add x !memo;
      f x
    ) else (
      dbg D_memoisation @@ lazy (sprintf "skipped %s" (Term.to_string x))
    )
  ) t *)



(* Helper function *)
let[@inline] add_to_indices (* sim_state *) indices clause =
  (* indices |> List.iter (fun (module M : Simplify.Index) ->
    M.add sim_state clause
  ) *)
  (* indices |> List.iter (fun add ->
    add clause
  ) *)
  indices clause




(* ----- *)
(* State *)
(* ----- *)


type sim_flag = NoSubs | NoDemod | AllSims
type options = {
  order: ordering;

  mutable hook_passive : (clause -> (clause option)); (* hook_passive: is an outside function that is applied before adding to passive; if it returns None then the clause is discarded *)

  prep_sup_sim_sup     : bool;

  passive_queue_type   : Options.passive_queue_type;
  passive_queues       : Options.pass_queue_list_type;
  passive_queues_freq  : Options.passive_queue_freqs;
  score_type           : Options.sup_score_type;

  interactive_mode    : bool;

  sim_flag : sim_flag;
  demod_completeness_check : Options.Demod_check.t;

  sup_unprocessed_bound : int;

  to_prop_solver   : Options.sup_to_prop_solver_type;
  prop_simpl_new   : bool;
  prop_simpl_given : bool;
  fun_splitting : bool;

  simplification_setup : Options.SupSimplificationSetup.spec;
  eq_types : Symbol.sym_set;
  demod_use_ground : bool;
  (* was mutable *)

  (* preprocessing_flag: bool; *)

  symb_ordering: Options.Ordering.Symb.t;

  subs_bck_mult : int;

  prob_props: Problem_properties.prob_props;
}

 
let make_options ?(hook_passive=(fun c -> Some c)) (opts: Options.options) ~sim_flag ~eq_types ~prob_props = {
  order = Superposition_sim_spec.mk_order ~ordering:opts.sup_ordering ~symb_ordering:opts.sup_symb_ordering ~term_weight:opts.sup_term_weight ~with_var:true ~theory_record:(Theory_db.get_global_record ()) ();

  hook_passive;

  prep_sup_sim_sup = opts.prep_sup_sim_sup;

  passive_queue_type  = opts.sup_passive_queue_type;
  passive_queues      = opts.sup_passive_queues;
  passive_queues_freq = opts.sup_passive_queues_freq;

  score_type = opts.sup_score;

  interactive_mode = opts.interactive_mode;
 
  sim_flag;
  demod_completeness_check = opts.demod_completeness_check;

  sup_unprocessed_bound = opts.sup_unprocessed_bound;

  to_prop_solver   = opts.sup_to_prop_solver;
  prop_simpl_new   = opts.sup_prop_simpl_new;
  prop_simpl_given = opts.sup_prop_simpl_given;
  fun_splitting    = opts.sup_fun_splitting;

  simplification_setup = opts.sup_simplification_setup;
  eq_types;
  demod_use_ground = opts.demod_use_ground;

  (* preprocessing_flag = opts.preprocessing_flag; *)

  symb_ordering = opts.sup_symb_ordering;

  subs_bck_mult      = opts.subs_bck_mult;

  prob_props;
}



(** Group functions to change the state (add to passive, add to simplification, etc.) in this module *)
module State = struct
  type t = {
    (* Superposition options *)
    options: options;

    state_id : int;

    (* Statistics *)
    mutable iteration: int;

    (* Superposition index: encapsulates two indices internally *)
    mutable index: SuperpositionUnifIndex.t;

    (* Passive queues *)
    mutable passive : PassiveQueues.passive_queue;

    mutable active_set: term list BCMap.t;

    (* Unprocessed, waiting to be added to passive *)
    mutable unprocessed : clause list;

    (* Simplification state *)
    sim_state: Simplify.Set.t;
    mutable imsim_state: Simplify.Set.t;

    (* Simplification spec *)
    mutable spec: Superposition_sim_spec.spec;

    (* Default spec *)
    spec_default: Superposition_sim_spec.spec;
    (* Alternate spec for when we don't want demod *)
    spec_noimmeddemod: Superposition_sim_spec.spec;
    (* Alternate spec for when we don't want immed *)
    spec_noimmed: Superposition_sim_spec.spec;

    (* Enabled if joinability is checked for all clauses, so that we can block inferences between ac axioms. *)
    ac_flag: bool;
    ac_symbols: AC.Table.t ref;

    order: Orderings.t;
    order_oriented: Orderings.oriented;
    order_terms: Orderings.terms;
    order_lits: Orderings.terms;

    (* Map of clause scores *)
    scores: Superposition_scores.t; (* scores that are recored but not used in the passive queue *)
    scores_frozen: Superposition_scores.t option; (* these are scores that don't change during this run (obtained from previous runs) and used in the passive queue *)

    (* Clauses to be retreived from the outside, to be used in restarts *)
    mutable extra_clauses: clause list;
  }


(* if passive PQT_External_Agent then we do not use any scores and used external agent for passive/active *)
  let interactive_scores_flag options =
    options.interactive_mode
      &&
    (not (Stdlib.(options.passive_queue_type = Options.PQT_External_Agent)))
      
  (** Creates an empty state with the options in [options]. *)
  let create ?(scores_frozen=None) options = 
    (* let open Options in *)
    let passive_queue_type = options.passive_queue_type in
    let priorities = options.passive_queues in
    let mults = options.passive_queues_freq in
    (* KBO.set_compare_symb options.symb_ordering; *)
    
    (* dbg D_passive @@ lazy (
      sprintf "priorities: %s, freqs: %s" (pass_queues_type_to_str priorities) (passive_queue_freqs_to_str mults)
    ); *)

    dbg D_trace @@ lazy (match options.sim_flag with
      | NoSubs  -> "demod ON subs OFF"
      | NoDemod -> "demod OFF subs ON"
      | AllSims -> "demod ON subs ON"
    );

    let ac_symbols = Problem_properties.(options.prob_props.ac_symbols) in
    (* Flag is true if there is ac reasoning (joinability) enabled. If yes, then we don't need to combine ac axioms in superposition *)
    let ac_flag = 
      let open Options.SupSimplificationSetup in
      let has_joinability = List.exists (fun x -> x == ACNormalisation || x == ACJoinability) in
      AC.Table.has_ac ac_symbols 
      && (has_joinability options.simplification_setup.immed_fw_immed
      || has_joinability options.simplification_setup.immed_fw_main)
    in
    dbg D_trace @@ lazy (sprintf "AC reasoning is %s" (if ac_flag then "on" else "off"));

    let sim_options = Simplify.{
      order = options.order;
      demod_completeness_check = options.demod_completeness_check;
      eq_types = options.eq_types;
      demod_use_ground = options.demod_use_ground;
      cache_sim = options.simplification_setup.cache_sim;
      ac_symbols;
      smt_check_interval = options.simplification_setup.sup_smt_interval;
      bw_gjoin_interval = options.simplification_setup.sup_bw_gjoin_interval;
      subs_bck_mult = options.subs_bck_mult;
      context_data_structure = ContextMap;
    } in
    let sim_state = Simplify.Set.create sim_options in
    let imsim_state = Simplify.Set.create sim_options in

    (* let prop_simpl_given = options.prop_simpl_given in *)
    let spec = 
      (* default_spec ~prop_simpl_given  *)
      Superposition_sim_spec.mk_spec 
        ~demod_flag:(options.sim_flag != NoDemod)
        (* ~subs_flag:(options.sim_flag != NoSubs) *)
        ~ac_flag:(AC.Table.has_ac ac_symbols)
        ~sim_state
        ~imsim_state
        options.simplification_setup
    in
    let spec_default = spec in
    let spec_noimmeddemod = 
      if options.sim_flag != NoDemod then 
        Superposition_sim_spec.mk_spec_noimmeddemod 
          ~sim_state
          ~imsim_state
          options.simplification_setup
      else
        spec
    in
    let spec_noimmed = 
      Superposition_sim_spec.disable_immed spec
    in

    let external_score_cmp = 
      if (interactive_scores_flag options) then
        Sockets.cmp_clause_external
      else 
        cmp_const_eq
    in
    let passive = 
      match scores_frozen with 
      | None -> 
        let new_priorities = 
          let filter_score priority = List.filter (fun x -> match x with  Options.Cl_Score _ -> false | _ -> true) priority in 
          List.map filter_score priorities
        in
        PassiveQueues.create_passive_queue ~external_score_cmp passive_queue_type new_priorities mults            
      | Some scores_frozen -> 
        let score_cmp = Superposition_scores.score_type_to_cl_cmp options.score_type scores_frozen in
        (* let dbg_cmp c1 c2 = 
          let res = score_cmp c1 c2 in
          dbg D_scores @@ lazy (sprintf "score_cmp res: c1: %s c2: %s res: %d" (Clause.to_string c1) (Clause.to_string c2) res);
          res
        in *)
        PassiveQueues.create_passive_queue ~external_score_cmp ~score_cmp (* ~score_cmp:dbg_cmp *) passive_queue_type priorities mults
    in

    let state = {
      options;
      state_id = Logic_interface.next_proof_state_id ();
      iteration = 0;

      index = SuperpositionUnifIndex.create ~order:options.order ~eq_types:options.eq_types ();

      (* passive = PassiveQueues.create_passive_queue ~score_cmp passive_queue_type priorities mults; *)
      passive;

      active_set = BCMap.empty;
      unprocessed = [];

      sim_state;
      imsim_state;

      spec;
      (* spec = mk_spec spec' ~sim_state  ~imsim_state; *)
      (* (if options.demod_flag then default_spec ~prop_simpl_given else default_spec_no_demod ~prop_simpl_given)  *) 
      spec_default;
      spec_noimmeddemod;
      spec_noimmed;

      ac_flag;
      ac_symbols = ref ac_symbols;

      order_oriented = options.order.oriented;
      order_terms    = options.order.terms;
      order_lits     = options.order.lits;
      order          = options.order;

      scores = Superposition_scores.empty ();
      scores_frozen;

      extra_clauses = [];
    }
    in

    (* Statistics.(assign_fun_stat (fun () -> PassiveQueues.num_elem state.passive) sup_num_in_passive);
    Statistics.(assign_fun_stat (fun () -> get_val_stat_fun sup_num_in_passive + get_val_stat sup_num_in_active) sup_num_of_clauses); *)
    Statistics.(assign_fun_stat (fun () -> BCMap.cardinal state.active_set) sup_num_in_active);
    Statistics.(assign_fun_stat (fun () -> Simplify.Set.size_nondead state.sim_state) sup_num_of_clauses);
    Statistics.(assign_fun_stat (fun () -> get_val_stat_fun sup_num_of_clauses - get_val_stat_fun sup_num_in_active) sup_num_in_passive);

    state


  let get_state_id state = state.state_id

  let get_score_frozen state clause = 
    Option.lift2 Superposition_scores.get state.scores_frozen (Some clause) |> Option.join
      

  (** Add clause to prop_solver_exchange *)
  let add_clause_prop_solver_exchange state flag clause =    
    if state.options.to_prop_solver == flag then (
      Prop_solver_exchange.add_clause_to_solver clause
    )

  (** Add clause to prop_solver_exchange in active *)
  let add_clause_prop_solver_exchange_active state clause = 
    add_clause_prop_solver_exchange state Options.Sup_to_Solver_Active clause

  (** Add clause to prop_solver_exchange in active *)
  let add_clause_prop_solver_exchange_passive state clause = 
    add_clause_prop_solver_exchange state Options.Sup_to_Solver_Passive clause

  (** External agent communication *)
    
  let external_send_simplified state clauses = 
    if (List.X.is_nonempty clauses)  && state.options.interactive_mode
    then      
      Sockets.(
      send_simplified_clauses (get_external_connection ()) ~component:Sup ~component_id:state.state_id clauses
     )     
    else ()
        
  let external_send_given_clause state clause = 
    if state.options.interactive_mode     
    then 
      Sockets.(
      send_given_clause (get_external_connection ()) ~component:Sup ~component_id:state.state_id clause
     )
    else ()

  let external_update_interactive_scores state clauses = 
    (if (interactive_scores_flag state.options)  && (List.X.is_nonempty clauses) then 
      Sockets.(
       update_cache_external_scores (get_external_connection ()) ~component:Sup ~component_id:state.state_id clauses     
     )
    else ()
    ) 

  let external_send_passive state clauses =     
    (
     if (List.X.is_nonempty clauses) && state.options.interactive_mode
     then 
       Sockets.(
       send_passive_clauses (get_external_connection ()) ~component:Sup ~component_id:state.state_id clauses
     )
    else ()
    )
       

  let external_get_given_clause state =
    let given_opt = 
      Sockets.(
      get_given_clause (get_external_connection ()) ~component:Sup ~component_id:state.state_id          
     )
    in
    match given_opt with 
    |Some given_cl -> 
        dbg D_trace @@ lazy (sprintf "popped given external %s" (Clause.to_string_tptp given_cl));
        given_cl 
    |None -> 
        raise PassiveQueues.Passive_Empty
  
      
  (** Add clauses to indices *)
  let add_clause_active state ({clause;sel}:clause') =
    (* Statistics.(bump_int_stat sup_num_in_active); *)
    state.active_set <- BCMap.add clause sel state.active_set;
    sel |> List.iter (fun x ->
      SuperpositionUnifIndex.add_clause_with_sel state.index x clause)

  (** Removes a clause from the active indices. Ignores the case where there was no such clause. *)
  let remove_clause_active state clause =
    match BCMap.find_opt clause state.active_set with
    | Some sel ->
        external_send_simplified state [clause];

        sel |> List.iter (fun x ->  (* TODO ver *)
          SuperpositionUnifIndex.elim_clause_with_sel state.index x clause;
          (* Statistics.(incr_int_stat (-1) sup_num_in_active); *)
          state.active_set <- BCMap.remove clause state.active_set;
                         )
    | None -> ()


  (** If member of sim_state or if subset subsumed by existing, return [Eliminated]. Else return unchanged clause. *)
  let check_mem_sim sim_state clause =
    (* let Simplify.Simplified clause = Simplify.Orient.simplify clause in *)
    if Simplify.Set.mem_any sim_state clause 
    (* || Simplify.forward_subset_subsume sim_state clause != clause *)
    then (
      dbg D_trace @@ lazy "[existing] or subsumed";
      Statistics.(bump_int_stat sim_repeated);
      Simplify.Eliminated []  
      (* Explanation: clause scoring will work on basic_clauses. And even when we remove a poor scoring clause, 
         we will remove it from indices but keep it in the context. So it won't be the case that we remove a 
         poor scoring clause that appears many times and then that clause won't be blocked if it appears again.
         Therefore, there's no need to track anything here. *)
    ) else (
      Simplify.Simplified clause
    )

  (** [check_mem_sim] if [clause != old_clause], else return unchanged clause *)
  let[@inline] do_if_different f old_clause clause =
    if clause != old_clause then 
      f clause 
    else 
      Simplify.Simplified clause

  (** [check_mem_sim] if [clause != old_clause], else return unchanged clause *)
  let check_mem_sim_if_different sim_state old_clause clause =
    do_if_different (check_mem_sim sim_state) old_clause clause 



  (** [Simplify.FwSubsetSubsumption.simplify] if [clause != old_clause], else return unchanged clause *)
  let fw_subset_subsumption sim_state clause =
    Simplify.FwSubsetSubsumption.simplify sim_state clause

  (** [Simplify.FwSubsetSubsumption.simplify] if [clause != old_clause], else return unchanged clause *)
  let fw_subset_subsumption_if_different sim_state old_clause clause =
    if clause != old_clause then 
      fw_subset_subsumption sim_state clause
    else 
      Simplified clause

  (** Perform bw subset subsumption and handle with [remove] *)
  let bw_subset_subsumption ~remove sim_state clause =
    let open Simplify in
    BwSubsetSubsumption.simplify sim_state clause |> Bw_result.handle
      ~add:(fun _ -> assert false)
      ~remove;
    Simplified clause

  (** "Wrap" another simplification, that is: *)
  let wrap_subset_subsumption ~remove_bw sim_state (inner: clause -> Simplify.Fw_result.t) clause =
    let open Simplify.Fw_result.O in
    (* Do fw subset subsumption *)
    fw_subset_subsumption sim_state clause
    (* Then do the inner simplifications *)
    >>= inner
    (* If the clause is changed, do fw subset subsumption again, otherwise skip it*)
    >>= fw_subset_subsumption_if_different sim_state clause
    (* Then do bw subset subsumption *)
    >>= bw_subset_subsumption sim_state ~remove:remove_bw
    (* After this the resulting clause can safely be added to the indices, with no subset subsumption conflicts *)



  let scores_fw_simplified state clauses = 
    List.iter (Superposition_scores.incr_simplified state.scores) clauses

  let scores_bw_simplified state clause x = 
    Superposition_scores.incr_simplified_by state.scores clause x

  let scores_generated state clause = 
    (* dassert (fun () -> match Clause.get_tstp_source clause with Clause.() ) *)
    List.iter (Superposition_scores.incr_generated state.scores) (Clause.get_parents clause)


  exception Given_eliminated_by of clause list
  
  (* let rec simplify_bw_after_full state clause =
    let open Simplify.Fw_result.O in
    dbg D_trace @@ lazy (sprintf "Light simplify: %s" (Clause.to_string_tptp clause));

    let triv_result = 
      clause
      |>  check_mem_sim state.sim_state 
      >>= Simplify.Fw_result.fold state.spec.light_triv
    in

    let fw_result = 
      triv_result
      (* >>= wrap_subset_subsumption state.sim_state 
        ~remove_bw:(fun c -> 
          dbg D_trace @@ lazy (sprintf "Removing backward ss clause (light): %s" (Clause.to_string_tptp c));
          remove_clause_active state c
        ) (fun x -> 
          x 
          |>  Simplify.Fw_result.fold (state.spec.light_fw)
          >>= check_mem_sim_if_different state.sim_state clause
        ) *)
      >>= fw_subset_subsumption state.sim_state 

      >>= Simplify.Fw_result.fold (state.spec.light_fw)
      >>= check_mem_sim_if_different state.sim_state clause

      >>= fw_subset_subsumption_if_different state.sim_state clause
      >>= bw_subset_subsumption state.sim_state ~remove:(fun c -> 
            dbg D_trace @@ lazy (sprintf "Removing backward ss clause (light): %s" (Clause.to_string_tptp c));
            remove_clause_active state c
          )
    in

    (* Simplify.(match fw_result with Simplified c -> ignore @@ BwSubsetSubsumption.simplify state.sim_state c | Eliminated -> ()); *)

    dbg D_trace @@ lazy (sprintf "Light simplify: End");
    fw_result

  let rec simplify_bw_after_immed state given clause =
    let open Simplify.Fw_result.O in
    dbg D_trace @@ lazy (sprintf "Light simplify: %s" (Clause.to_string_tptp clause));

    let triv_result = 
      clause
      |>  check_mem_sim state.imsim_state 
      >>= check_mem_sim state.sim_state 
      (* KK *)
      >>= Simplify.Fw_result.fold state.spec.light_triv
    in

    let fw_result = 
      triv_result 
      >>= check_mem_sim_if_different state.imsim_state clause
      >>= check_mem_sim_if_different state.sim_state clause
      >>= fw_subset_subsumption state.imsim_state 
      >>= fw_subset_subsumption state.sim_state 
      >>= bw_subset_subsumption state.imsim_state ~remove:(fun c -> 
            dbg D_trace @@ lazy (sprintf "Removing backward ss clause (immed): %s" (Clause.to_string_tptp c));
            (* If given removed, raise *)
            if Clause.Bc.(c == given) then (
              dbg D_trace @@ lazy (sprintf "Given clause %s deleted by %s" (Clause.to_string_tptp given) (Clause.to_string_tptp clause));
              Statistics.(bump_int_stat sup_given_eliminated);
              (* raise_notrace (Given_eliminated_by clause) *)
            )
          )
      >>= bw_subset_subsumption state.sim_state ~remove:(fun c -> 
            dbg D_trace @@ lazy (sprintf "Removing backward ss clause (full by immed): %s" (Clause.to_string_tptp c));
            remove_clause_active state c
          )
    in

    dbg D_trace @@ lazy (sprintf "Light simplify: End");
    fw_result *)





  (* KK *)
  let when_born_iter_flag = true
  let _ = out_warning (sprintf "when_born_iter_flag: %B" when_born_iter_flag)

  
  (** Simply pushes the clause into the passive queue and records statistics *)
  let push_clause_passive state clause =
    dbg D_trace @@ lazy (sprintf "push_clause_passive: clause: %s" (Clause.to_string_tptp clause));
    if not (Clause.is_simplification_inf (Clause.get_tstp_source clause)) then ( 
      dbg D_trace @@ lazy (sprintf "Generated: when_born = %d" state.iteration);
      Clause.assign_ps_when_born state.iteration clause
    ) else (
      let parent_when_born = Clause.get_parents clause |> List.hd |> Clause.get_ps_when_born in
      dbg D_trace @@ lazy (sprintf "Simplified: when_born = %d" parent_when_born);
      Clause.assign_ps_when_born parent_when_born clause
    );
    if true then Theory_db.(search builtin (get_global_record ())) clause;  (* TODO: make into an option *)

(*    external_send_passive state [clause]; *)
    PassiveQueues.add_to_passive state.passive clause;
    ()

  (** Adds clause to passive queue and passive indices in simplification set. Does not simplify beforehand. *)
  let add_clause_passive state clause =
    match state.options.hook_passive clause with
    | Some clause' ->
      (* KK: add_to_indices can raise exceptions! *)
      (* begin try  *)
      dbg D_trace @@ lazy (
        if clause != clause' then 
          sprintf "add_clause_passive: clause before hook: %s : after hook: %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp clause') 
        else 
          sprintf "add_clause_passive: clause: %s" (Clause.to_string_tptp clause')
      );
      add_to_indices state.spec.indices_passive clause'; 
      (* with x -> 
         dbg D_trace @@ lazy (sprintf "add_clause_passive: exception: %s" (Printexc.to_string x));
         raise_trace x;
         end; *)
      add_clause_prop_solver_exchange_passive state clause';
      push_clause_passive state clause'
    | None -> 
      dbg D_trace @@ lazy (sprintf "add_clause_passive: clause elemintaed by hook_passive: %s" (Clause.to_string_tptp clause))

  (** Check if [clause] is an AC axiom. If yes, then add it as an assoc/commut
      symbol to [state.prob_props]. Also, if that axiom makes it a new AC 
      symbol, return [Some (sym, a_clause, c_clause)], else return [None]. *)
  let check_if_ac_axiom' state clause = 
    let ac_symbols', axiom = AC.Table.add !(state.ac_symbols) clause in
    state.ac_symbols := ac_symbols';
    axiom

  (** Run the previous function, and return the extra axioms that also need 
      to be added, if any. *)
  let check_if_ac_axiom state clause = 
    begin match check_if_ac_axiom' state clause with
    | Some (sym, a_clause, c_clause) -> 
      dbg D_trace @@ lazy (sprintf "Detected new AC symbol: %s" (Symbol.to_string sym));
      let parents = [a_clause; c_clause] in
      dassert (fun () -> Simplify.Set.(status state.sim_state a_clause != Dead));
      dassert (fun () -> Simplify.Set.(status state.sim_state c_clause != Dead));
      parents @ AC.extra_ac_axioms ~parents sym
    | None -> []
    end

  (** Adds clause to passive queue and simplification set, after performing light_simplify. *)
  (* let light_simp_and_add_clause_passive state clause =
    dbg D_trace @@ lazy (sprintf "light_simp_and_add (before): %s" (Clause.to_string_tptp clause));
    match light_simplify state clause with
    | Simplified clause' -> 
      dbg D_trace @@ lazy (sprintf "light_simp_and_add (after): %s" (Clause.to_string_tptp clause'));
      add_clause_passive state clause'
    | Eliminated ->
      dbg D_trace @@ lazy "eliminated";
      () *)





  (** Returns simplified clause or raises [Eliminated]. *)
  let full_simplify state clause =
    let open Simplify.Fw_result.O in
    (* let csim x = (* dbg D_cache_sim @@ lazy "sim"; *) Simplify.Fw_result.cache_sim state.sim_state x in *)
    let csim x = x in
    dbg D_trace @@ lazy (sprintf "Full simplify: %s" (Clause.to_string_tptp clause));
    dbg D_trace @@ lazy (sprintf "sim id %d" (Simplify.Set.unique_id state.sim_state));

    (* Apply all simplifications in [state.spec.full_triv] *)
    let triv_result =      
        clause
      |>  csim state.spec.full_triv
          >>= (fun x -> check_empty_clause_return x; Simplified x) 
          >>= prop_assumptions_tautology 
          >>= check_mem_sim_if_different state.sim_state clause     
    in

    (* Apply all simplifications in [state.spec.full_fw] *)
    let fw_result =    
      Statistics.(time sup_time_sim_fw_full) @@ fun () -> 
      triv_result >>= fun c_triv -> c_triv
      (* >>= fw_subset_subsumption state.sim_state *)

      |>  csim state.spec.full_fw 
      (* >>= do_if_different (state.spec.full_triv) c_triv *)
      >>= check_mem_sim_if_different state.sim_state clause

      >>= csim @@ fw_subset_subsumption_if_different state.sim_state clause
      >>= bw_subset_subsumption state.sim_state ~remove:(fun c -> 
            dbg D_trace @@ lazy (sprintf "Removing backward ss clause (full): %s" (Clause.to_string_tptp c));
        remove_clause_active state c
                                                        )
    
    in

    dbg D_trace @@ lazy (sprintf "Full fw: %s" (Simplify.Fw_result.to_string_tptp fw_result));

    (* After full_simplify is done, we must add (and immed_simplify) bw_clauses to passive. *)
    let bw_clauses = ref [] in
    (* Use simplified clause to delete backwards via [state.spec.full_bw] *)
    begin match fw_result with
    | Simplified clause' ->
      scores_fw_simplified state (Clause.get_simp_parents clause clause');
      Statistics.(time sup_time_sim_bw_full) @@ fun () -> 

        dbg D_trace @@ lazy (sprintf "add_clause_active: sim: %s" (Clause.to_string_tptp clause'));
                (try
                  add_to_indices state.spec.indices_active clause';
                   with
      |Simplify.Set.Is_mem -> failwith "dbg: full_simplify: Is_mem"
        ); 
      (* We should add [clause'] to the passive indices as well (it is a new clause; if we don't do this then it will just be in the "active" indices) *)
      if Clause.Bc.(clause != clause') then add_to_indices state.spec.indices_passive clause';

      (* Else, use the simplified clause to apply all bw simplifications in [state.spec.full_bw] *)
      let bw_results = state.spec.full_bw clause' in
      (* And add and remove the clauses *)
      let n_bw = ref 0 in
      bw_results |> Simplify.Bw_result.handle
        ~remove:(fun c -> 
          dbg D_trace @@ lazy (sprintf "Removing backward simplified clause (full): %s" (Clause.to_string_tptp c));
          incr n_bw;
          remove_clause_active state c
        )
        ~add:(fun c -> 
          dbg D_trace @@ lazy (sprintf "Adding backward simplified clause (full, immediate_simplify pending): %s" (Clause.to_string_tptp c));
          bw_clauses := c :: !bw_clauses
        );
      scores_bw_simplified state clause' !n_bw
     
    | Eliminated parents -> 
      scores_fw_simplified state parents
    end;

    dbg D_trace @@ lazy (sprintf "Full simplify: End");
    fw_result, !bw_clauses




  (*-------- KK: functional splitting *)

  let fun_split_st ~parent s t = 
  (* order by size; add to def smaller size *)
  (* make split only if the size of the def is strictly smaller than the size of the smaller term (defined term) *)
    dbg D_fun_split2 @@ lazy (sprintf "try to split: s: %s  t:%s " 
      (Term.to_string s) (Term.to_string t)
    ); 
    let l, r = 
      if Term.cmp_num_symb s t > 0 then s,t else t,s  
    in 
    let vl = Term.get_var_set l in 
    let vr = Term.get_var_set r in 
    let vinter = VSet.inter vl vr in 
    let r_def_term, r_def_clause = 
      Definitions.add_def Definitions.def_env_glb ~parent (VSet.elements vinter) [r]
    in
    if Term.cmp_num_symb r r_def_term > 0 then 
      let def_type = Term.get_term_type l in (* assume that types of l,r are the same *)
      let side_ueq = add_typed_equality_sym def_type l r_def_term in 
      let side_split_cl = Definitions.create_def_reduced_clause ~parent [side_ueq] in
      
      dbg D_fun_split2 @@ lazy (sprintf "def: %s side: %s" (Clause.to_string_tptp r_def_clause) (Clause.to_string_tptp side_split_cl)); 
      
      Some [r_def_clause; side_split_cl]
    else 
      None (* do not split if the size of the def is the same or larger *)

  (* check that set differences between term vars are not empty *)
  let var_diff_check t s = 
    let vt = Term.get_var_set t in 
    let vs = Term.get_var_set s in 
    not (VSet.is_empty (VSet.diff vt vs)) && not (VSet.is_empty (VSet.diff vs vt))

  let fun_split clause = 
    match get_lits clause with 
    | [eq_atom] ->
      dbg D_fun_split2 @@ lazy (sprintf "check orient: %s" (Term.to_string eq_atom));  
      begin match Term.Eq.decompose_atom eq_atom with 
      | Some (s,t) ->  (* Positive equality *)
        (* do not split if none of top is slit *)
        let is_non_var_split term = (not (Term.is_var term)) &&  (Symbol.is_definition (Term.get_top_symb term)) in
        if is_non_var_split s || is_non_var_split t then (
          None
        ) else (
        (* begin match Orderings_opt.kbo_terms s t with
            |Partial_order_result.INC -> *)

          (* split all non-orientable  *) (* TODO: other criteria like reduced vars etc *)
          (* if not (is_any_oriented_kbo eq_atom) *)
          (* split when both var differences are non-empty  *) (* TODO: other criteria like reduced vars etc *)
          if var_diff_check t s then (
            dbg D_fun_split2 @@ lazy (sprintf "non-orientable: %s" (Term.to_string eq_atom));
            fun_split_st ~parent:clause s t
          ) else (
            (* |_->   *)
            dbg D_fun_split2 @@ lazy (sprintf "orientable: %s" (Term.to_string eq_atom));
            None (* do not split if orientable *)   
          )
        )
      | None ->  (* Not positive equality *)
        None
      end 
    | _ -> None

  (*--- end KK -----*)



  exception Split of clause list

  (** All simplifications but only with immediate set. *)
  (** KK: can raise Split (clause_list) *)
  (* Does not return simplified clause, side-effect is that it adds it (as nondead) to imsim_state *)
  let rec immediate_simplify' ?(bw_main=false) state given_opt clause : unit =
    let open Simplify.Fw_result.O in
    (* let csim x = (* dbg D_cache_sim @@ lazy "sim"; *) Simplify.Fw_result.cache_sim state.sim_state x in *)
    let csim x = x in
    (* let cimsim x = (* dbg D_cache_sim @@ lazy "imsim"; *) Simplify.Fw_result.cache_sim state.imsim_state x in *)
    let cimsim x = x in
    dbg D_trace @@ lazy (sprintf "Immediate simplify: %s" (Clause.to_string_tptp clause));
    dbg D_trace @@ lazy (sprintf "sim   id %d" (Simplify.Set.unique_id state.sim_state));
    dbg D_trace @@ lazy (sprintf "imsim id %d" (Simplify.Set.unique_id state.imsim_state));

    let triv_result = 
      clause
      |>  check_mem_sim state.imsim_state
      >>= check_mem_sim state.sim_state
      >>= (* csim @@ *) state.spec.immed_triv
    in

    let fw_result = 
      Statistics.(time sup_time_sim_fw_immed) @@ fun () -> 

      triv_result >>= fun c_triv -> c_triv 
      |>  check_mem_sim_if_different state.imsim_state clause
      >>= check_mem_sim_if_different state.sim_state   clause
      >>= cimsim @@ fw_subset_subsumption state.imsim_state 
      >>=   csim @@ fw_subset_subsumption state.sim_state 

      >>= cimsim @@ state.spec.immed_fw_immed state.imsim_state
      >>=   csim @@ state.spec.immed_fw_main  (* state.sim_state *)  
      (* >>= do_if_different (state.spec.immed_triv) c_triv *)

      >>= check_mem_sim_if_different state.imsim_state clause
      >>= check_mem_sim_if_different state.sim_state clause
      >>= cimsim @@ fw_subset_subsumption_if_different state.imsim_state clause
      >>= csim   @@ fw_subset_subsumption_if_different state.sim_state   clause

      >>= fun c_after -> c_after
      |>  bw_subset_subsumption state.imsim_state (* ~remove:(fun _ -> ()) *) ~remove:(fun c -> 
            match given_opt with
            | Some given when Clause.Bc.(c == given) ->
              dbg D_trace @@ lazy (sprintf "Given clause %s deleted by %s" (Clause.to_string_tptp given) (Clause.to_string_tptp c_after));
              Statistics.(bump_int_stat sup_given_eliminated);
              raise_notrace @@ Given_eliminated_by [c_after]
            | _ -> ()
          )
      (* No bw_subset_subsumption state.sim_state *)
    in

    dbg D_trace @@ lazy (sprintf "Immediate fw: %s" (Simplify.Fw_result.to_string_tptp fw_result));

    begin match fw_result with
    | Simplified clause' ->
      scores_fw_simplified state (Clause.get_simp_parents clause clause');
      (*KK: try to split *)
      if state.options.fun_splitting then (
        match fun_split clause' with 
        | Some new_clauses -> 
          (* dbg D_fun_split @@ lazy (sprintf "Split into %s" (List.X.to_string ~first:"" ~last:"" ~sep:"\n" Clause.to_string_tptp new_clauses)); *)
          dbg_env D_fun_split (fun () ->
            new_clauses |> List.iter (fun x ->
              dbg D_fun_split @@ lazy (sprintf "Split into %s" (Clause.to_string_tptp x));
            )
          );
          raise_notrace (Split new_clauses)
          (* List.iter (add_clause_unprocessed state given) new_clauses  *)
        | None -> ()
      );
      (* end KK *)

      add_to_indices (state.spec.indices_immed state.imsim_state) clause';

      (* No bw simplification wrt. state.sim_state *)
      Statistics.(time sup_time_sim_bw_immed) @@ fun () ->   
      
      let bw_results = (state.spec.immed_bw_immed state.imsim_state) clause' in
      let n_bw = ref 0 in
      bw_results |> List.iter (fun ((* {add;remove} as *) bw_result (* Simplify.bw_result *)) -> 
        incr n_bw;
        (* If given removed, raise *)
        (* TODO: move to separate loop outside? *)
        begin match given_opt with
        | Some given ->
          begin match bw_result with
          | Simplify.BwEliminated c when Clause.Bc.(c == given)-> 
            dbg D_trace @@ lazy (sprintf "Given clause %s deleted by %s" (Clause.to_string_tptp given) (Clause.to_string_tptp clause'));
            Statistics.(bump_int_stat sup_given_eliminated);
            raise_notrace @@ Given_eliminated_by [clause']
          | Simplify.BwSimplified {from; into} when Clause.Bc.(from == given)-> 
            dbg D_trace @@ lazy (sprintf "Given clause %s deleted by %s" (Clause.to_string_tptp given) (Clause.to_string_tptp clause'));
            dbg D_trace @@ lazy (sprintf "with side premise %s" (Clause.to_string_tptp into));
            Statistics.(bump_int_stat sup_given_eliminated);
            raise_notrace @@ Given_eliminated_by [clause'; into]
          | _  -> ()
          end
        | None -> ()
        end;

        (* Else, handle bw_results as usual *)
        bw_result |> Simplify.Bw_result.handle_elt 
        ~remove:(fun c -> 
          dbg D_trace @@ lazy (sprintf "Removing backward simplified clause (immediate): %s" (Clause.to_string_tptp c));
        )
        ~add:(fun c -> 
          dbg D_trace @@ lazy (sprintf "Adding backward simplified clause (immediate): %s" (Clause.to_string_tptp c));
          immediate_simplify ~bw_main state given_opt c;
          (* |> tap (fun x -> dbg D_trace @@ lazy (sprintf "Added backward simplified clause (immediate): %s" (Simplify.Fw_result.to_string_tptp x))) |> ignore *)
          dbg D_trace @@ lazy (sprintf "Added backward simplified clause (immediate)");
        )
      );
      scores_bw_simplified state clause' !n_bw;
      if bw_main then bw_simplify_main state clause';
    | Eliminated parents -> 
      scores_fw_simplified state parents
    end;

    dbg D_trace @@ lazy (sprintf "Immediate simplify: End")

  (* Handle splits *)
  and immediate_simplify ?(bw_main=false) state given_opt clause = 
    try immediate_simplify' ~bw_main state given_opt clause 
    with Split new_clauses -> List.iter (immediate_simplify ~bw_main state given_opt) new_clauses

  (* Bw simplify main wrt. one clause *)
  and bw_simplify_main state clause = 
    Statistics.(time sup_time_sim_bw_immed) @@ fun () ->   

    dbg D_trace @@ lazy (sprintf "Bw simplify main: %s" (Clause.to_string_tptp clause));
    ignore @@ bw_subset_subsumption state.sim_state clause ~remove:(fun c -> 
      dbg D_trace @@ lazy (sprintf "Removing backward ss clause (full by immediate): %s" (Clause.to_string_tptp c));
      remove_clause_active state c
    );

    let n_bw = ref 0 in
    (state.spec.immed_bw_main (* state.sim_state *)) clause
    |> Simplify.Bw_result.handle
      ~remove:(fun c -> 
        dbg D_trace @@ lazy (sprintf "Removing backward simplified clause (full by immediate): %s" (Clause.to_string_tptp c));
        incr n_bw;
        remove_clause_active state c
      )
      ~add:(fun c ->
        dbg D_trace @@ lazy (sprintf "Adding backward simplified clause (full by immediate): %s" (Clause.to_string_tptp c));
        immediate_simplify ~bw_main:true state None c;
        (* |> tap (fun x -> dbg D_trace @@ lazy (sprintf "Added backward simplified clause (immediate): %s" (Simplify.Fw_result.to_string_tptp x))) |> ignore *)
        dbg D_trace @@ lazy (sprintf "Added backward simplified clause (full by immediate)");
      );
    scores_bw_simplified state clause !n_bw;

    dbg D_trace @@ lazy (sprintf "Bw simplify main: End")

  (* Bw simplify main wrt. nondead clauses in immed *)
  let bw_simplify_main_by_immed state given_opt = 
    dbg D_trace @@ lazy "will bw_simplify_main";
    (* state.spec <- state.spec_noimmed; *)
    (* Don't use given to simplify main *)
    let is_given = 
      match given_opt with
      | Some c -> fun x -> dassert (fun () -> (x==c) == Clause.Bc.(x==c)); x == c
      | None -> fun _ -> false
    in
    state.imsim_state |> Simplify.Set.iter (fun c is_dead ->
      if not is_dead && not (is_given c) then bw_simplify_main state c
    );
    (* state.spec <- state.spec_default; *)
    dbg D_trace @@ lazy "done bw_simplify_main"



  (** Does immediate simplification with the rest of the clauses in 
      unprocessed, and adds the clause to unprocessed. Note that 
      immediate_simplify may forward simplify/delete the clause, and backward 
      simplify/delete clauses in unprocessed. *)
  (* let rec add_clause_unprocessed state given_opt clause = 
    dbg D_trace @@ lazy (sprintf "add_clause_unprocessed: (before): %s" (Clause.to_string_tptp clause));
    try 
      match immediate_simplify state given_opt clause with
      | Simplified clause' ->
        dbg D_trace @@ lazy (sprintf "add_clause_unprocessed: (after): %s" (Clause.to_string_tptp clause'));
        (* add_to_indices (state.spec.indices_immed state.imsim_state) clause'; *)
        (* assert (Simplify.Set.mem_any state.imsim_state clause'); *)
        (* state.unprocessed <- clause' :: state.unprocessed *)
      | Eliminated -> 
        dbg D_trace @@ lazy (sprintf "add_clause_unprocessed: (after): eliminated");
        ()
    with Split new_clauses -> 
      List.iter (add_clause_unprocessed state given_opt) new_clauses *)

  let add_clause_unprocessed state clause = 
    (* Small cheap optimisation: identical clauses are often derived one after 
       the other. We can cheaply delete adjacent equal clauses in unprocessed. 
       This has also an advantage: if only 1 distinct clause is derived 
       (possibly many times) then this optimisation is guaranteed to put only 
       1 copy in unprocessed. This triggers the optimisation in [add_unprocessed_
       to_immed] where we skip constructing immediate simplification set if 
       there is only 1 clause *)
    match state.unprocessed with
    | [] -> 
      dbg D_trace @@ lazy (sprintf "add_clause_unprocessed: %s" (Clause.to_string_tptp clause));
      state.unprocessed <- [clause]
    | hd::_ ->
      if Clause.Bc.(clause != hd) then (
        dbg D_trace @@ lazy (sprintf "add_clause_unprocessed: %s" (Clause.to_string_tptp clause));
        state.unprocessed <- clause :: state.unprocessed
      ) else (
        dbg D_trace @@ lazy (sprintf "add_clause_unprocessed: same");
        Statistics.(bump_int_stat sim_repeated);
      )

  (** As [add_clause_unprocessed] but only if it is [Some clause]. *)
  let add_some_clause_unprocessed state (* given *) clause_opt = 
    match clause_opt with
    (* | Some clause -> add_clause_unprocessed state (Some given) clause *)
    | Some clause -> 
      scores_generated state clause;
      add_clause_unprocessed state clause
    | None -> ()

  (** Add clauses to passive, performing no simplifications *)
  (* let add_clauses_passive_nosimp state given =
    (* List.iter (add_clause_passive state) ((* List.rev *) state.unprocessed);
    state.unprocessed <- [] *)
    let n_dead = List.length @@ Simplify.Set.list_dead state.imsim_state in
    Statistics.(incr_int_stat n_dead sup_immediate_simplified);
    Simplify.Set.list_nondead state.imsim_state
    (* |> List.iter (fun x -> if x != given && not (Simplify.Set.is_dead state.sim_state x) then add_clause_passive state x) *)
    |> List.iter (fun x -> 
      if x != given then (
        try add_clause_passive state x 
        with Simplify.Set.Is_mem | Simplify.Set.Is_dead -> dbg D_trace @@ lazy (sprintf "%s already in set!" (Clause.to_string_tptp x)); ()
      )
    ) *)

  (* Dump unprocessed to immed simplification set *)
  let add_unprocessed_to_immed state given_opt = 
    (* Shortcut to avoid all overhead if no clauses derived *)
    begin match state.unprocessed with 
    | [] -> ()
    (* If single clause, we can also skip the overhead *)
    | [x] -> 
      state.spec <- state.spec_noimmed;
      immediate_simplify state given_opt x;
      state.spec <- state.spec_default;
      state.unprocessed <- []
    | _ -> 
      (* If no unit clauses in unprocessed, use spec_noimmeddemod rather than the usual spec *)
      (* But only check at all if demod is not already disabled *)
      if state.options.sim_flag != NoDemod then (
        let has_unit_eq = List.exists (Clause.is_unit_eq) state.unprocessed in
        if not has_unit_eq then (state.spec <- state.spec_noimmeddemod);
      );
      state.unprocessed 
      (* |> List.rev  (* Mimic previous behaviour *) *)
      (* |> List.sort (fun c1 c2 ->
        (* Heuristic to put smaller clauses first *)
        let cmp1 = Ord.lift Clause.length compare c1 c2 in
        if cmp1 <> Ord.eq then cmp1 else Ord.lift Clause.num_of_symb compare c1 c2
      ) *)
      (* |> List.X.shuffle *)
(* KK added *)  
      |> List.sort Clause.cmp_num_symb 
      |> List.iter (fun x ->
        immediate_simplify state given_opt x
      );
      state.spec <- state.spec_default;
      state.unprocessed <- []
    end;

    (* Now it's just bw wrt main which is missing *)
    bw_simplify_main_by_immed state given_opt;

    ()

  (* Get non-dead clauses in immed_set, and dump to passive *)
  let add_immed_to_passive state given_opt =
    let isnt_given c given_opt = 
      match given_opt with Some given -> dassert (fun () -> (c!=given) == Clause.Bc.(c!=given)); c != given | None -> true
    in
    let to_passive = ref [] in

    let[@inline] add_if_not_existing state c = 
      try 
        add_clause_passive state c         
      with Simplify.Set.Is_mem | Simplify.Set.Is_dead -> dbg D_trace @@ lazy (sprintf 
        "add_immed_to_passive: %s already in set! (as %s)" 
          (Clause.to_string_tptp c) (if Simplify.Set.is_dead state.sim_state c then "dead" else "nondead")
      )
    in
    let[@inline] add_dead_if_not_existing state c = 
      try Simplify.Set.add_dead state.sim_state c
      with Simplify.Set.Is_mem -> dbg D_trace @@ lazy (sprintf 
        "add_immed_to_passive: %s (dead) already in set! (as %s)" 
          (Clause.to_string_tptp c) (if Simplify.Set.is_dead state.sim_state c then "dead" else "nondead")
      )
    in
    
    (* let to_add = ref [] in *)
    (* dbg D_trace @@ lazy "will still bw_simplify_main"; *)
    let do_add_dead = state.options.simplification_setup.cache_sim != Options.SupSimplificationSetup.CacheSim.None in
    state.imsim_state |> Simplify.Set.iter (fun c is_dead -> 
      if is_dead then (
        (* This assert is not necessarily valid. Say we have inter-simplified immed. Then we 
           are missing bw simplification wrt. main. We do that, and a new clause comes, which 
           deletes given. It is not the case that we can apply given clause deletion and throw 
           away all clauses in immed (except the one which deleted the given) because those 
           clauses were needed to get that clause which deleted given. *)
        (* dassert (fun () -> isnt_given c given_opt); *)
        dbg_env D_trace (fun () -> if not (isnt_given c given_opt) then dbg D_trace @@ lazy "is_given_add_immed_to_passive");
        Statistics.(bump_int_stat sup_immediate_simplified);
        if do_add_dead then add_dead_if_not_existing state c
        (* dbg_env D_trace (fun () -> 
          if Simplify.Set.mem_any state.sim_state c then
            let was_dead = Simplify.Set.is_dead state.sim_state c in
            dbg D_trace @@ lazy (sprintf 
              "%s (dead) already in set! (as %s)" (Clause.to_string_tptp c) (if was_dead then "dead" else "nondead");
            )
        );
        Simplify.force_dead_and_remove state.sim_state c *)
      ) else (
        if isnt_given c given_opt then (
          (* bw_simplify_main state c; *)

          (* Detection of AC axioms is moved here *)
(*KK *)
(*          add_if_not_existing state c; *)
          to_passive := c::!to_passive;
          (* check_if_ac_axiom state c |> List.iter (fun x -> add_if_not_existing state x) *)
          state.extra_clauses <- check_if_ac_axiom state c @ state.extra_clauses
         )
       )
     );

    to_passive := List.sort Clause.cmp_num_symb !to_passive;
   
    external_update_interactive_scores state !to_passive;
    external_send_passive state !to_passive; 

    List.iter (add_if_not_existing state) !to_passive;

    (* !to_add |> List.X.shuffle |> List.iter (fun c -> ); *)
    state.imsim_state <- Simplify.Set.clear state.imsim_state

  let handle_given_eliminated state given by_clauses = 
    (* [by_clauses] are about to be added to passive, so they must be immediate-simplified, 
       both to avoid repeated/redundant clauses wrt. existing clauses to be added to 
       passive, and to inter-simplify the potentially many premises (but usually 0 or 1 *)
    state.unprocessed <- [];
    state.imsim_state <- Simplify.Set.clear state.imsim_state;
    begin match by_clauses with
    | [] -> ()
    | [x] -> 
      state.spec <- state.spec_noimmed;
      immediate_simplify state None x;
      state.spec <- state.spec_default;
    | _ -> 
      by_clauses |> List.iter (fun c -> 
        immediate_simplify state None c; 
        (* bw_simplify_main state c; *)
      );
      (* Simplify.Set.iter (fun c is_dead -> 
        if not is_dead then (bw_simplify_main state c; add_clause_passive state c)
      ) state.imsim_state; *)
    end;

    dassert (fun () -> not (Simplify.Set.mem_any state.imsim_state given));
    bw_simplify_main_by_immed state None;
    
    add_immed_to_passive state (Some given);
    Simplify.force_dead_and_remove state.sim_state given;
    (* state.imsim_state <- Simplify.Set.clear state.imsim_state(*;*) *)



  exception Empty_passive 

  (** Pops given clause from passive queue, performing "full" simplification *)
  let rec pop_given_clause state =
    try 
      let clause =
        
        (* external agent *)  
        if Stdlib.(state.options.passive_queue_type = Options.PQT_External_Agent)
        then 
          (external_get_given_clause state  (* can raise PassiveQueues.Passive_Empty *)
          )
        else 
          (* internal passive queues *)
          PassiveQueues.remove_from_passive state.passive 
      in
      
      dbg D_trace @@ lazy (sprintf "popped given %s" (Clause.to_string_tptp clause));
      
      if Simplify.Set.is_dead state.sim_state clause then (
        dbg D_trace @@ lazy "dead!";
        pop_given_clause state
       )
                    
      else
        begin
          if (BCMap.mem clause state.active_set) then 
            (
            dbg D_trace @@ lazy "already in active";
             pop_given_clause state
            )
       
          else 
            begin
              dbg D_trace2 @@ lazy (sprintf "given age: %d" (Clause.get_ps_when_born clause));
              match full_simplify state clause with
              | Simplified clause', bw_clauses ->
                  (* state.imsim_state <- Simplify.Set.create(); *)
                  
                  (* If no bw_clauses skip this entirely *)
                  begin match bw_clauses with
                  | [] -> ()
                  | _ -> 
                      (* Else fw simplify, and dump to passive, and re-clear state *)

          
                      List.iter (add_clause_unprocessed state (* None *)) bw_clauses;
                             
                      add_unprocessed_to_immed state None;  (* Here these clauses were simplified by given, so no chance that given is simplified by them *)
                        
                      add_immed_to_passive state (Some clause');
                      (* with Given_eliminated_by clauses -> handle_given_eliminated state clause' clauses *)                       
                  end;
                  
(*KK: sup_unprocessed_bound:  commented *)
                  dbg D_trace @@ lazy (sprintf "add_clause_active: imsim: %s" (Clause.to_string_tptp clause'));
(*
  add_to_indices (state.spec.indices_immed state.imsim_state) clause';
 *)
                  add_clause_prop_solver_exchange_active state clause';
                  
                  (* dbg D_trace @@ lazy (sprintf "sim_state address: %d" (Obj.magic state.sim_state : int)); *)
                  (* dbg D_trace @@ lazy (sprintf "imsim_state address: %d" (Obj.magic state.imsim_state : int)); *)
                  
                  external_send_given_clause state clause';

                  clause'
              
                
              | Eliminated _, _ -> 
                  dbg D_trace @@ lazy (sprintf "eliminated %s" (Clause.to_string_tptp clause));
                  external_send_simplified state [clause];
                  pop_given_clause state             
            end
        end
    with 
    | PassiveQueues.Passive_Empty -> raise Empty_passive

        

  (** Candidates for backward_superposition via a clause l=r *)
  let backward_superposition_candidates state l =
    SuperpositionUnifIndex.backward_superposition_candidates state.index l

  (** Candidates for forward superposition via a clause l[s]=r *)
  let forward_superposition_candidates state s =
    SuperpositionUnifIndex.forward_superposition_candidates state.index s



  (** Fully inter-simplifies input clauses, and adds to passive *)
  let add_input_clauses state clauses =
    Statistics.(time sup_time_total) @@ fun () -> 

    dassert (fun () -> PassiveQueues.num_elem state.passive = 0);

    state.imsim_state <- Simplify.Set.clear state.imsim_state; 

    let spec = 
      if state.options.prep_sup_sim_sup then (* full preprocessing *)
        Superposition_sim_spec.mk_spec ~demod_flag:true ~ac_flag:true ~sim_state:state.imsim_state ~imsim_state:state.imsim_state state.options.simplification_setup
      else (* minimally required; implicitly includes subsetsubsumption and trivrules *)
        Superposition_sim_spec.mk_spec ~demod_flag:false ~ac_flag:false ~sim_state:state.imsim_state ~imsim_state:state.imsim_state {
          state.options.simplification_setup with 
            input_triv = [Unflattening]; (* need Unflattening as abstr ref can add eq axioms *)
            input_fw = [];
            input_bw = [];
        } 
    in
    
    let clauses' = Preprocess.superposition_sim_only spec state.imsim_state clauses in

    state.imsim_state <- Simplify.Set.clear state.imsim_state;

    (* let add_ac_axioms l = 
      let rec loop acc = function [] -> acc | hd::tl -> loop (check_if_ac_axiom state hd) tl in
      loop [] l @ l
    in *)

    let add_ac_axioms l =  
      SMap.fold (fun sym (a,c) acc -> 
        dbg D_trace @@ lazy (sprintf "extra ac axioms for %s" (Symbol.to_string sym));
        List.rev_append (AC.extra_ac_axioms ~parents:[a;c] sym) acc
      ) !(state.ac_symbols).ac l
    in
    let clauses'' = 
      clauses'
    |> add_ac_axioms
    |> List.sort Clause.cmp_num_symb
    in

    external_update_interactive_scores state clauses''; 
    external_send_passive state clauses''; 

    clauses'' |> List.iter (fun c ->
      dbg D_trace @@ lazy (sprintf "Input clause (after simp): %s" (Clause.to_string_tptp c));
      (try add_clause_passive state c with Simplify.Set.Is_mem -> ())
                           )
    (*
    clauses'
    |> add_ac_axioms
    |> List.sort Clause.cmp_num_symb
        (* |> List.X.shuffle *)
    (* |> tap (fun x -> state.ac_symbols := AC.Table.populate !(state.ac_symbols) x) *)
    |> List.iter (fun c ->
        dbg D_trace @@ lazy (sprintf "Input clause (after simp): %s" (Clause.to_string_tptp c));
        (try add_clause_passive state c with Simplify.Set.Is_mem -> ())
                 )
        *)

  let add_clauses state clauses = 
    state.unprocessed <- clauses @ state.unprocessed
end

type state = State.t





(* ------------- *)
(* Superposition *)
(* ------------- *)

(** For a given literal l=r, grab all unification candidates for l and r 
    among subterms of active clauses, and try to do superposition. Returns 
    a list of all such inferences. *)
let backward_superposition_lit (state:state) given_lit given_clause =
  let is_given_ac_axiom = state.ac_flag && Clause.is_ac_axiom given_clause in
  let all_inferences pos_left l = 
    let candidates = State.backward_superposition_candidates state l in
    candidates |> List.iter (fun (s, lst) -> 
      dbg D_candlength @@ lazy (sprintf "length lst %d" (List.length lst));
      lst |> List.iter (fun (pos_right, right_lit, right_clause) ->
        (* if (
          Term.is_eq_atom right_lit &&
          let 
        ) 
        then *)
        if is_given_ac_axiom && Clause.is_ac_axiom right_clause then () else (
        let inference = Inference_rules.equality_superposition ~order:state.order pos_left pos_right s given_lit right_lit given_clause right_clause in
        if Option.is_some inference then Statistics.(bump_int_stat sup_bw_superposition);
        State.add_some_clause_unprocessed state (* given_clause *) inference
        )
      )
    )
  in

  let sign, atom = Term.split_sign_lit given_lit in
  if Bool.O.(sign = true) then (
    match Term.Eq.decompose_atom atom with
    | Some (l,r) ->
      begin match state.order_oriented given_lit with
      | GT ->
        all_inferences 1 l;
        dbg D_trace @@ lazy (sprintf "bw: skipped going of %s into %s" (Term.to_string given_lit) (Term.to_string r));
        (* assert (Orderings.(l > r)) *)
      | LT ->
        all_inferences 2 r;
        dbg D_trace @@ lazy (sprintf "bw: skipped going of %s into %s" (Term.to_string given_lit) (Term.to_string l));
        (* assert (Orderings.(l > r)) *)
      | INC ->
        all_inferences 1 l;
        all_inferences 2 r;
      | EQ -> failwith "backward_superposition: unsimplified s=s"
      end
    | None -> 
      (* failwith "unimplemented" *)
      all_inferences 0 atom
  ) else (
    ()
  )

(** For a given clause, does all backward superposition inferences with selected literals *)
let backward_superposition state (given_clause:clause') =
  dbg D_trace @@ lazy "backward_superposition";
  List.iter (fun x -> backward_superposition_lit state x given_clause.clause) (given_clause.sel)



(** For a given literal l=r, for all its subterms, grab all unification 
    candidates among left- and right-hand terms of equalities in active 
    clauses, and try to do superposition. Returns a list of all such 
    inferences. *)
let forward_superposition_lit (state:state) given_lit given_clause =
  let is_given_ac_axiom = state.ac_flag && Clause.is_ac_axiom given_clause in
  let f' ~at_top pos_given s =
    match s with
    | Term.Fun (sym, _, _) -> 
      let sort = Symbol.get_val_type_def sym in
      if not (at_top || SSet.mem sort state.options.eq_types) then () else
      let candidates = State.forward_superposition_candidates state s in
      candidates |> List.iter (fun (l', lst) ->
        dbg D_candlength @@ lazy (sprintf "length lst %d" (List.length lst));
        lst |> List.iter (fun (pos_right, right_lit, right_clause) ->
          (* match Term.decompose_eq_atom right_lit with 
          | [_;l;r] -> 
            let l,r = Term.Eq.regularize_pos pos_right l r in
            assert (l==l'); *)
          if is_given_ac_axiom && Clause.is_ac_axiom right_clause then () else (
            let inference = Inference_rules.equality_superposition ~order:state.order pos_right pos_given s right_lit given_lit right_clause given_clause in
            if Option.is_some inference then Statistics.(bump_int_stat sup_fw_superposition);
            State.add_some_clause_unprocessed state (* given_clause *) inference
          )
          (* | _ -> assert false *)
        )
      )
    | Term.Var _ -> ()
  in
  let[@inline] f     x y = f' ~at_top:false x y in
  let[@inline] f_top x y = f' ~at_top:true  x y in

  let sign, atom = Term.split_sign_lit given_lit in
  match Term.Eq.decompose_atom atom with
  | Some (tl,tr) ->
    (* if Term.Eq.is_pos_predicate_eq given_lit then ( *)
    (* If term is oriented, try only on subterms of left-hand side, else try both *)
    begin match state.order_oriented atom with
    | GT ->
      tl |> Term.iter_preorder_novar (f 1);
      dbg D_trace @@ lazy (sprintf "fw: skipped going of %s into %s" (Term.to_string given_lit) (Term.to_string tr));
      (* assert (Orderings.(tl > tr)) *)
    | LT ->
      tr |> Term.iter_preorder_novar (f 2);
      dbg D_trace @@ lazy (sprintf "fw: skipped going of %s into %s" (Term.to_string given_lit) (Term.to_string tl));
      (* assert (Orderings.(tl > tr)) *)
    | INC ->
      tl |> Term.iter_preorder_novar (f 1);
      tr |> Term.iter_preorder_novar (f 2);
    | EQ -> failwith "forward_superposition: unsimplified s=s or s!=s"
    end

  | None -> 
    if Bool.O.(sign = true) then (
      dbg D_trace @@ lazy "skipping top term in positive predicate";
    ) else (
      atom |> (f_top 0)
    );
    (* If problem has no equalities, don't bother traversing subterms at all *)
    if not @@ SSet.is_empty state.options.eq_types then (
      atom |> Term.iter_subterms_preorder_novar (f 0)
    )



(** For a given clause, does all forward superposition inferences with selected literals *)
let forward_superposition state (given_clause:clause') =
  dbg D_trace @@ lazy "forward_superposition";
  List.iter (fun x -> forward_superposition_lit state x given_clause.clause) (given_clause.sel)

(** For a given clause, does all superposition inferences with selected literals *)
let superposition state (given_clause:clause') =
  forward_superposition state given_clause;
  backward_superposition state given_clause



(* ------------------- *)
(* Equality Resolution *)
(* ------------------- *)

(** Tries equality resolution in all selected literals in a clause *)
let equality_resolution state (clause:clause') =
  dbg D_trace @@ lazy "equality_resolution";
  clause.sel |> List.iter (fun lit ->
    let inference = Inference_rules.equality_resolution lit clause.clause in
    State.add_some_clause_unprocessed state (* clause.clause *) inference
  )



(* ------------------ *)
(* Equality Factoring *)
(* ------------------ *)

(*KK: !! you do n^2 of factorings in place of n^2/2 *)

let equality_factoring (state:state) ({clause;sel}:clause') =
  dbg D_trace @@ lazy "equality_factoring";
  sel |> List.iter (fun first ->
    dbg D_selection @@ lazy ("first " ^ Term.to_string first);
    sel |> List.iter (fun second -> 
      if first != second then (
        dbg D_selection @@ lazy ("second " ^ Term.to_string second);
        let[@inline] do_inference pos1 pos2 = 
          Inference_rules.equality_factoring ~order:state.order pos1 pos2 first second clause
        in
        match Term.Eq.is_eq first, Term.Eq.is_eq second with
        (* Equality factoring *)
        | true, true -> 
          let inferences = 
            (* If oriented, no need to try with first equation flipped *)

            (* KK:  What if the second is oriented ? *)
            match state.order_oriented first with
            | GT -> [
              do_inference 1 1;
              do_inference 1 2;
            ]
            | LT -> [
              do_inference 2 1;
              do_inference 2 2;
            ]
            | INC -> [
              do_inference 1 1;
              do_inference 1 2;
              do_inference 2 1;
              do_inference 2 2;
            ]
            | EQ -> failwith "equality_factoring: unsimplified s=s or s!=s"
          in
          List.iter (fun x -> State.add_some_clause_unprocessed state (* clause *) x) inferences

        (* Resolution factoring *)
        | false, false -> 
          let inference = 
            do_inference 0 0
          in
          State.add_some_clause_unprocessed state inference

        | true, false | false, true -> ()
      )
    )
  )




type sup_model = term list BCMap.t (* active map *)

exception Sup_satisfiable of sup_model

(* --------- *)
(* Main loop *)
(* --------- *)

let main_loop (state:state) =
  Statistics.(time sup_time_total) @@ fun () -> 

  (* Uncomment for "progress bar" in output *)
  dbg_env D_prog_bar (fun () -> 
    let gc_stat = Gc.quick_stat() in
    eprintf "\rIt: %-5d | Num clauses: %-1d / %-1d = %-1d | Gc: %d/%d " 
      (state.iteration) 
      Statistics.(get_val_stat_fun sup_num_in_active)
      Statistics.(get_val_stat_fun sup_num_in_passive)
      Statistics.(get_val_stat_fun sup_num_of_clauses)
      gc_stat.minor_collections
      gc_stat.major_collections;
    flush stderr;
  );

  dbg_env D_prop_solver (fun () ->
    dbg D_prop_solver @@ lazy ("--------- Solver assumptions:   ");
    let ass_set =  Prop_solver_exchange.get_solver_fof_assumptions ~soft:false ~sim:false in
    Term.out_term_list (TSet.elements ass_set); 
    dbg D_prop_solver @@ lazy ("");
    dbg D_prop_solver @@ lazy ("--------- Solver assumptions ------");
  );


  begin try 
    begin match state.unprocessed with
    | [] -> 
        (dbg D_trace @@ lazy (sprintf "--UNPROCESSED: empty "))
    | unprocessed -> 
        if (List.length state.unprocessed) >= state.options.sup_unprocessed_bound then 
          (
           dbg D_trace @@ lazy (sprintf "--UNPROCESSED: sup_unprocessed_bound is reached %i: noving unprocessed -> immed -> passive" (List.length state.unprocessed));
           State.add_unprocessed_to_immed state None;
           State.add_immed_to_passive state None;
          )
        else (
          dbg D_trace @@ lazy (sprintf "--UNPROCESSED: sup_unprocessed_bound is not reached %i" (List.length state.unprocessed));
         )
    end;

    dbg D_trace @@ lazy (sprintf "--START %d superposition loop" state.iteration);

    let given : clause' = 
      try
        Clause'.create ~order_lits:state.order_lits @@ State.pop_given_clause state 
      with State.Empty_passive -> 
        begin match state.unprocessed with
        | [] -> raise (Sup_satisfiable (state.active_set))
        | _ -> 
            dbg D_trace @@ lazy (sprintf "-- passive is empty but unprocessed is not; moving unprocessed -> immed ");
            State.add_unprocessed_to_immed state None;
            State.add_immed_to_passive state None;        
            Clause'.create ~order_lits:state.order_lits @@ State.pop_given_clause state              
        end        
          (* move unprocessed to immediate *)
      |  
        Simplify_new.Set.Is_mem -> failwith "given: Simplify_new.Set.Is_mem"

    in
    
    State.add_clause_active state given;
    dbg D_trace @@ lazy (sprintf "--GIVEN: %s" (Clause.to_string_tptp given.clause));
    dbg D_trace @@ lazy (sprintf "--SELECTION: %s" (list_to_string Term.to_string given.sel " | "));
    dbg D_trace @@ lazy (sprintf "given score_frozen: %s"
      (Option.to_string Superposition_scores.score_to_string (State.get_score_frozen state given.clause))
    );   
    
    begin try
      let unprocessed_was_empty = List.X.is_empty state.unprocessed in

      Statistics.(time sup_time_generating) @@ (fun () -> 
        (* let new_clauses = ref [] in *)
        superposition state given;
        equality_resolution state given;
        equality_factoring state given;
        (* let new_clauses = (* List.rev *) !new_clauses in *)
      );
      (* need to surround with (); otherwise it will include everything below *)

      (if (List.length state.unprocessed) >= state.options.sup_unprocessed_bound then 
        (        
                 dbg D_trace @@ lazy (sprintf "-- sup_unprocessed_bound reached: %i" (List.length state.unprocessed));
                 
                  (if unprocessed_was_empty then 
                    (
                     add_to_indices (state.spec.indices_immed state.imsim_state) given.clause;
                     State.add_unprocessed_to_immed state (Some given.clause);
                     State.add_immed_to_passive state (Some given.clause) 
                    )
                  else
                    (State.add_unprocessed_to_immed state None;
                     State.add_immed_to_passive state None; 
                    )
                  );
                 )
      else 
        (
         dbg D_trace @@ lazy (sprintf "-- sup_unprocessed_bound was not reached: %i" (List.length state.unprocessed));
        )
      );

      dbg_env D_trace (fun () -> 
        dbg D_trace @@ lazy "New clauses:";
        Simplify.Set.list_nondead state.imsim_state 
        |> List.iter (fun x -> if x != given.clause then dbg D_trace @@ lazy (Clause.to_string_tptp x));
        dbg D_trace @@ lazy "End.";
      );

      dbg_env D_scores (fun () ->
        let l = Superposition_scores.fold (fun c elt acc -> (c,elt) :: acc) state.scores [] in
        let l = List.filter (fun (c,_) -> not (Simplify_new.Set.is_dead state.sim_state c)) l in
        let cmp_scores = Ord.lift (fun (_, elt) -> Superposition_scores.(float_of_int (elt.simplified + 1) /. float_of_int (elt.generated + 1))) Float.compare in
        (* let cmp_scores = Ord.lift (fun (_, elt) -> Superposition_scores.(elt.simplified)) Int.compare in *)
        let l = List.sort (Ord.reverse_f cmp_scores) l in
        dbg D_scores @@ lazy "Simplification scores:";
        l |> List.iter (fun (c, elt) -> 
          dbg D_scores @@ lazy Superposition_scores.(sprintf "%d sims, %d gens : %s" elt.simplified elt.generated (Clause.to_string_tptp c));
        );
        dbg D_scores @@ lazy "End";
      );
      
(*      State.add_immed_to_passive state (Some given.clause) (* new_clauses *); *)
      
    with State.Given_eliminated_by clauses -> 
      state.spec <- state.spec_default;
      State.handle_given_eliminated state given.clause clauses;
      State.remove_clause_active state given.clause;
      
    end;

    dbg D_trace @@ lazy (sprintf "--NUM_CLAUSES: %d / %d = %d"
      Statistics.(get_val_stat_fun sup_num_in_active) 
      Statistics.(get_val_stat_fun sup_num_in_passive) 
      Statistics.(get_val_stat_fun sup_num_of_clauses)
    );
    dbg D_trace @@ lazy "--END superposition loop";

    dassert (fun () -> Simplify.Set.size state.imsim_state = 0);
    Statistics.(bump_int_stat sup_num_of_loops);
    state.iteration <- succ state.iteration;

  with State.Empty_passive -> 
    (* Theory_db.print_report ~heading:"Detected theories after saturation" (Simplify.Set.list_all state.sim_state); *)
    raise (Sup_satisfiable (state.active_set))

  (* | e -> 
    Theory_db.print_report ~heading:"Detected theories after saturation" (Simplify.Set.list_all state.sim_state);
    raise e *)

  end;

  ()



(* Visible interface *)

let create = State.create
let add_input_clauses = State.add_input_clauses
let add_clauses = State.add_clauses
let assign_hook_passive hook_passive state = state.State.options.hook_passive <- hook_passive
let get_num_of_clauses state =  Simplify.Set.size_nondead state.State.sim_state

let get_scores state = state.State.scores
let get_extra_clauses state = state.State.extra_clauses

let step = main_loop

(*-----*)

let smt_add_clause state clause =
  dbg D_add_to_smt_outside @@ lazy (sprintf "%s" (Clause.to_string clause));
  Simplify.smt_add_outside_clause state.State.sim_state clause
    
   (* 
  try
    Simplify.SMTSetIndex.add state.State.sim_state clause
(*    Simplify.add_to_indices state.State.sim_state [Simplify.Index_tag.SMTSet] clause *)
  with
    Simplify.Set.Is_mem | Simplify.Set.Is_dead -> ()
(*      Simplify.Set.Context.Is_mem -> () *)
*)
