open Lib
open Logic_interface



(*---------------------------------------------------------------------------*)
(* concrete implementation of compressed features; based on this concrete subsumtion index is defined using MakeCF *)
(* trasnferred from discount.ml *)
(*---------------------------------------------------------------------------*)

module Elt = struct 
  type t = clause 
(*  include Clause *) (*KK*)
  include Clause.Bc (* KK *)
  let get_lits = Clause.get_literals
  let to_string = Clause.to_string_tptp
  let subsumes = Unif.subsumes
end

module FeatureCom = struct
  module F = BasicSubsumptionIndex.DefaultFeatureCom

  type t = F.t

  type elt = clause

  let get_feature_list clause = 
    F.get_feature_list (Clause.get_literals clause)

  let compare_pos = F.compare_pos
  let compare_val = F.compare_val

  let to_string = F.to_string
end



(* concrete implementation of a subsumtion index with fixed compressed features as defined above *)
module MakeSCFVIndex (Options : BasicSubsumptionIndex.Options) = struct
  include BasicSubsumptionIndex.MakeCom (Elt) (FeatureCom) (Options)
  (* let get_feature_list = get_feature_list *)



  (**********************************)
  (* Subsumption resolution (andré) *)
  (**********************************)

  (* Fw *)

  (* Returns new list of lits which is obtained by all possible cuts. We also 
     keep subsumed by list to add to history later. *)  
  let rec forward_subs_res_list ~subs_bck_mult index parents tried_lits rest =
    match rest with
    | hd::tl ->
      let compl_hd = add_compl_lit hd in
      let tstp_source = Clause.tstp_source_tmp in  (* TODO replace later with lit_list *)
      let clause_to_try = Clause.create_clause_raw tstp_source (tried_lits @ (compl_hd::tl)) in
      (* dbg D_fw_subs_res @@ lazy (sprintf "clause_to_try: %s %s" (Clause.to_string clause_to_try) (feature_list_to_string feature_list)); *)
      begin match is_subsumed ~subs_bck_mult index clause_to_try with
      | Some (by_cl, _subst) -> 
        forward_subs_res_list ~subs_bck_mult index (by_cl::parents) tried_lits tl
        (* We do not need to retry tried lits after elimination of a literal *)
        (* forward_subs_res_list parents' [] (tried_lits@tl))*)
      | None ->
        forward_subs_res_list ~subs_bck_mult index parents (tried_lits@[hd]) tl   (* TODO remove concatenation *)
      end
    | [] -> 
      tried_lits, parents

  let fw_subsres ~subs_bck_mult index clause =
    let lits = Clause.get_lits clause in
    if is_empty index then (lits,[]) else
    forward_subs_res_list ~subs_bck_mult index [] [] lits



  (* Bw *)

  (* Review, can be simplified I think *)
  let apply_lit_cut (* state *) given_clause subsumed_subst_list lit =
    let f (subsumed_list, new_list) (* as list *) (subsumed, subst) =  (* MY CHANGE *)
      let lits = Clause.get_lits subsumed in
      let lit_to_cut = Subst.apply_subst_term term_db_ref subst lit in
      let new_lits = ListExtra.removeq_all lit_to_cut lits in
      
      (* The actual clause creation is handled by the caller *)
      (* let tstp_source = Clause.tstp_source_backward_subsumption_resolution [given_clause; subsumed] in
      let new_clause = create_clause ~normalise_eqs:true tstp_source new_lits in *)

      (* if Clause.Bc.(new_clause != subsumed) then ( *)
      if not (List.X.equal ~eq:(==) new_lits lits) then (
        (* let feature_list_new_clause_opt = Some (get_feature_list new_clause) in *)
        (* dbg D_bw_subs_res @@ lazy (sprintf "%s by: %s" (Clause.to_string_tptp subsumed) (Clause.to_string_tptp new_clause)); *)
        (* set_dead_and_remove state subsumed; *)
        (subsumed :: subsumed_list, new_lits :: new_list)
      ) else (
        assert false (*; list *)
      )
    in
    List.fold_left f ([],[]) subsumed_subst_list

  let rec backward_subs_res_list ~subs_bck_mult index given_clause tried_lits rest =
    match rest with
    | hd::tl ->
      let compl_hd = add_compl_lit hd in
      let tstp_source = Clause.tstp_source_tmp in  (* replace later with lit_list *)
      (* important to use raw cluase here otherwise the reference to compl_hd gets mixed *)
      let clause_to_try = Clause.create_clause_raw tstp_source (tried_lits@(compl_hd::tl)) in 
      (* dbg D_bw_subs_res @@ lazy (sprintf "backward clause_to_try: %s" (Clause.to_string clause_to_try)); *)
      let subsumed_subst_list = find_subsumed_subst ~subs_bck_mult index clause_to_try in
      let add_subsumed_list, add_new_list = apply_lit_cut given_clause subsumed_subst_list compl_hd in (* MY CHANGE *)
      let rest_subsumed_list, rest_new_list = backward_subs_res_list ~subs_bck_mult index given_clause (tried_lits@[hd]) tl in
      (add_subsumed_list @ rest_subsumed_list, add_new_list @ rest_new_list)
    | [] ->
      ([],[])

  (* backward_subs_res returns list of pairs (subsumed_clause, new_clause)  *)
  (* subsumed_clause is removed from indexes; declared dead; but remain in sim_sate *)  
  (* new_clause is added to state; so a copy may need to be created if put to other contexts *)
  let bw_subsres ~subs_bck_mult state given_clause =
    (* assert(state.sim_opt.sim_use_sub_index); *)
    let lits = Clause.get_lits given_clause in
    backward_subs_res_list ~subs_bck_mult state given_clause [] lits
end

module SCFVIndex = MakeSCFVIndex (struct let track_clauses = true end)
module SCFVIndexNoTrack = MakeSCFVIndex (struct let track_clauses = false end)
