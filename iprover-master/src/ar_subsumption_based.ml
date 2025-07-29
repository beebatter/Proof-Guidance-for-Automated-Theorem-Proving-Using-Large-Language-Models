(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain

type details = {gnr_adom:AD.t; org_cls:LI.BCSet.t}

let empty_details = {gnr_adom = AD.(empty Over); org_cls = LI.BCSet.empty}

let map_lits_to_scls ld_lts cl accu =
  let lit_to_scls map lt =
    let new_scls =
      match LI.TMap.find_opt lt map with
      | Some scls -> LI.BCSet.add cl scls
      | None -> LI.BCSet.singleton cl
    in
    LI.TMap.add lt new_scls map
  in
  Clause.get_lits cl
  |> List.to_seq
  |> Seq.filter (fun lt -> not (List.mem lt ld_lts))
  |> Seq.fold_left lit_to_scls accu

let order_lits_and_map_to_cls ld_lts cls =
  LI.(BCSet.fold (map_lits_to_scls ld_lts) cls TMap.empty)
  |> LI.TMap.bindings
  |> List.fast_sort (fun (_,v1) (_,v2) -> LI.(BCSet.(cardinal v2 - cardinal v1)))

let add_abstr_cls_to_adom alts cl adom =
  let lts = Clause.get_lits cl in
  AD.ov_add Options.Abstr_ref_subs cl lts alts adom

let add_concr_cls_to_adom cl adom =
  let lts = Clause.get_lits cl in
  AD.ov_add Options.Abstr_ref_subs cl lts lts adom

let rec abstract_cls_prc ld_lts ((adom,rmn_cls,ults) as accu) = function
  | (_,_) :: _ when LI.BCSet.is_empty rmn_cls -> adom
    (* All clauses has been added to the abstract domain *)
  | (_,scls) :: tl when LI.BCSet.(is_empty (inter scls rmn_cls)) ->
    (* By pass if the set of clauses has been added to the abstract domain *)
    abstract_cls_prc ld_lts accu tl
  | (lt,scls) :: tl when LI.BCSet.cardinal scls > 1 ->
    (* Only abstract if the leading literal appears in more than one clause *)
    let adom' =
      let nld_lts = lt :: ld_lts in
      (* Avoiding trivial refutations *)
      if ld_lts = [] && LI.TSet.mem (LI.add_compl_lit lt) ults then
        order_lits_and_map_to_cls nld_lts scls
        |> abstract_cls_prc nld_lts (adom, scls, LI.TSet.empty)
      else
        let ccls = LI.BCSet.inter scls rmn_cls in
        LI.BCSet.fold (add_abstr_cls_to_adom nld_lts) ccls adom
    in
    let ults' = LI.TSet.add lt ults in
    let rcls = LI.BCSet.diff rmn_cls scls in
    abstract_cls_prc ld_lts (adom', rcls, ults') tl
  | (lt,scls) :: tl ->
    (* concretise other clauses *)
    let rcls = LI.BCSet.diff rmn_cls scls in
    let adom' = LI.BCSet.fold add_concr_cls_to_adom scls adom in
    abstract_cls_prc ld_lts (adom', rcls, ults) tl
  | [] -> LI.BCSet.fold add_concr_cls_to_adom rmn_cls adom

let abstract_cls adom ld_lts cls =
  order_lits_and_map_to_cls ld_lts cls
  |> abstract_cls_prc ld_lts (adom, cls, LI.TSet.empty)

let refine_acls uc_id adom =
  match AD.find_by_abstr_id uc_id adom with
  | Some aclw ->
    let adom' = AD.rm_by_abstr_id aclw.swp adom in
    abstract_cls adom' aclw.lits aclw.orig_cls
  | None -> adom

let abstract opts cls =
  Ar_dbg.dbg D_trace (lazy("\n\t* In Subsumption-based abstraction"));
  let adom = abstract_cls AD.(empty Over) [] cls in
  let dtls = {gnr_adom = adom; org_cls = cls} in
  Ar_dbg.dbg D_SBA (lazy(AD.to_string `V1 adom));
  (adom, dtls)

let refine adom dtls uc_ids cls =
  Ar_dbg.dbg D_trace (lazy("\n\t* In Subsumption-based refinement"));
  if LI.BCSet.equal cls dtls.org_cls then
    let adom' = LI.TSet.fold refine_acls uc_ids dtls.gnr_adom in
    Ar_dbg.dbg D_SBA (lazy("\n\t* Normal ref:" ^ AD.to_string `V1 adom'));
    let dtls' = {dtls with gnr_adom = adom'} in
    (adom', dtls')
  else
    let adom1, adom2 = AD.split_by_ids uc_ids dtls.gnr_adom in
    let adom1' = LI.TSet.fold refine_acls uc_ids adom1 in
    Ar_dbg.dbg D_SBA (lazy("\n\t* Subpart ref:" ^ AD.to_string `V1 adom1'));
    let gadom = AD.union adom2 adom1' in
    let dtls' = {dtls with gnr_adom = gadom} in
    (adom1', dtls')
