(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Lib
open Logic_interface


type aclw =
  { swp : term;
    orig_cls : BCSet.t;
    lits : lit list;
    abstr_cl : clause;
  }

type strong =
  { swp2aclw : aclw TMap.t; (* switch predicate to abstract clauses wrap *)
    alits2swp : term TMapList.t; (* abstract literals to switch predicate *)
    abstr_cls : BCSet.t;
    concr_cls : BCSet.t;
  }

type approx = Over | Under

type t =
  | Strong of strong
  | Weak of BCSet.t

let swp_cnt = ref 0

(* Creates an abstract clause wrap *)
let ov_create_abstr_cl typ clause alits =
  let swp_name = "$$iProver_ARSWP_" ^ (string_of_int !swp_cnt) in
  let swp = add_pred_0 ~is_special:true swp_name in
  swp_cnt := !swp_cnt + 1;
  let compl_swp = Logic_interface.add_neg_atom swp in
  let tstp_source =
    match typ with
    | Options.Abstr_ref_arg_filter -> Clause.tstp_source_arg_filter_abstr clause
    | Options.Abstr_ref_sig -> Clause.tstp_source_sig_abstr clause
    | Options.Abstr_ref_subs -> Clause.tstp_source_subs_abstr clause
    | Options.Abstr_ref_gen -> Clause.tstp_source_gen_abstr clause
    | _ -> failwith ("ar_abstract_domain.ml:ov_create_abstr_cl: " ^
                     "tstp_source; not supported abstraction")
  in
  let acl = create_clause tstp_source (compl_swp :: alits) in
  Clause.assign_ps_abstr_ref_concr false acl;
  Clause.inherit_conjecture clause acl;
  Ar_dbg.dbg D_trace (lazy ("ov_create_abstr_cl: "^(Clause.to_string acl)));
  {swp = swp; orig_cls = BCSet.singleton clause; lits = alits; abstr_cl = acl}

(* updates the set of concrete clauses in the abstract clause wrap *)
let ov_update_abstr_cl clause abstr_cl =
  let orig_cls = BCSet.add clause abstr_cl.orig_cls in
  {abstr_cl with orig_cls = orig_cls}

let ov_find_by_lits lits dom =
  match TMapList.find_opt lits dom.alits2swp with
  | None -> None
  | Some swp -> TMap.find_opt swp dom.swp2aclw

let ov_is_trivial_abstr lits abstr_lits =
  try List.for_all2 (==) lits abstr_lits
  with Invalid_argument _ -> false

let ov_add typ clause lits abstr_lits = function
  | Strong dom ->
    if ov_is_trivial_abstr lits abstr_lits then
      Strong {dom with concr_cls = BCSet.add clause dom.concr_cls}
    else begin
      match ov_find_by_lits abstr_lits dom with
      | None ->
        let acl = ov_create_abstr_cl typ clause abstr_lits in
        let aclw_m = TMap.add acl.swp acl dom.swp2aclw in
        let swp_m = TMapList.add abstr_lits acl.swp dom.alits2swp in
        let acls = BCSet.add acl.abstr_cl dom.abstr_cls in
        Strong {dom with swp2aclw = aclw_m; alits2swp = swp_m; abstr_cls = acls}
      | Some aclw ->
        let acl = ov_update_abstr_cl clause aclw in
        Strong {dom with swp2aclw=TMap.add acl.swp acl dom.swp2aclw}
    end
  | Weak _ -> failwith ("ar_abstract_domain.ml:ov_add: " ^
                        "expecting a strong abstract domain")

let un_add clause = function
  | Weak dom -> Weak (BCSet.add clause dom)
  | Strong _ -> failwith ("ar_abstract_domain.ml:un_add: " ^
                          "expecting a weak abstract domain")

let empty = function
  | Over -> Strong { swp2aclw = TMap.empty;
                     alits2swp = TMapList.empty;
                     abstr_cls = BCSet.empty;
                     concr_cls = BCSet.empty;
                   }
  | Under -> Weak BCSet.empty

let gamma_fun id = function
  | Weak _ -> BCSet.empty
  | Strong dom ->
    match TMap.find_opt id dom.swp2aclw with
    | Some aclw -> aclw.orig_cls
    | None -> BCSet.empty

let get_all_clauses = function
  | Strong dom ->
    BCSet.iter (Clause.assign_ps_abstr_ref_concr false) dom.abstr_cls;
    BCSet.iter (Clause.assign_ps_abstr_ref_concr true) dom.concr_cls;
    BCSet.union dom.abstr_cls dom.concr_cls |> BCSet.elements
  | Weak dom ->
    BCSet.iter (Clause.assign_ps_abstr_ref_concr true) dom;
    BCSet.elements dom

let get_abstr_ids = function
  | Strong dom -> TMap.fold (fun k v a -> k :: a) dom.swp2aclw []
  | Weak _ -> []

let get_all_symbols_by_abstr_id id = function
  | Strong dom -> begin
      match TMap.find_opt id dom.swp2aclw with
      | Some aclw ->
        Clause.find_all_sym ~is_relevant_symb:(fun _ -> true) aclw.abstr_cl
      | None -> SSet.empty
    end
  | Weak _ -> SSet.empty

let find_by_abstr_id id = function
  | Strong dom -> TMap.find_opt id dom.swp2aclw
  | Weak _ -> None

let rm_by_abstr_id id = function
  | Weak _ as w -> w
  | Strong dom ->
    match TMap.find_opt id dom.swp2aclw with
    | Some acl ->
      let aclw_m = TMap.remove acl.swp dom.swp2aclw in
      let swp_m = TMapList.remove acl.lits dom.alits2swp in
      let acls = BCSet.remove acl.abstr_cl dom.abstr_cls in
      Strong {dom with swp2aclw = aclw_m; alits2swp = swp_m; abstr_cls = acls}
    | None -> Strong dom

let split_by_ids ids = function
  | Weak _ -> (empty Under, empty Under)
  | Strong dom ->
    let aux id ((a1,a2) as a) =
      match TMap.find_opt id dom.swp2aclw with
      | Some acl ->
        let a1' = TSetList.add acl.lits a1 in
        let a2' = BCSet.add acl.abstr_cl a2 in
        (a1',a2')
      | None -> a
    in
    let alts, acls = TSet.fold aux ids (TSetList.empty, BCSet.empty) in
    let sa1, sa2 = TMap.partition (fun k _ -> TSet.mem k ids) dom.swp2aclw in
    let as1, as2 =
      TMapList.partition (fun k _ -> TSetList.mem k alts) dom.alits2swp
    in
    let ac1, ac2 = BCSet.partition (fun e -> BCSet.mem e acls) dom.abstr_cls in
    let adom1 = Strong { swp2aclw = sa1;
                         alits2swp = as1;
                         abstr_cls = ac1;
                         concr_cls = BCSet.empty
                       }
    in
    let adom2 = Strong { swp2aclw = sa2;
                         alits2swp = as2;
                         abstr_cls = ac2;
                         concr_cls = dom.concr_cls
                       }
    in
    (adom1, adom2)

let uissue m k v1 v2 =
  let mess =  "ar_abstract_domain.ml:uissue: Same key in " ^ m ^
              "; this should not happen"
  in
  failwith mess

let union adom1 adom2 =
  match (adom1,adom2) with
  | (Weak d1,Weak d2) -> Weak (BCSet.union d1 d2)
  | (Strong d1,Strong d2) ->
    let s2a = TMap.union (uissue "swp2aclw") d1.swp2aclw d2.swp2aclw in
    let a2s = TMapList.union (uissue "alits2swp") d1.alits2swp d2.alits2swp in
    let ac = BCSet.union d1.abstr_cls d2.abstr_cls in
    let cc = BCSet.union d1.concr_cls d2.concr_cls in
    Strong {swp2aclw = s2a; alits2swp = a2s; abstr_cls = ac; concr_cls = cc}
  | _ -> failwith ("ar_abstract_domain.ml:union: " ^
                   "Different types of abstract domains")

let concrete_domain_from_cset cset = function
  | Weak _ -> Weak cset
  | Strong dom -> Strong {dom with concr_cls = cset; abstr_cls = BCSet.empty}

let cset2str cset = Clause.clause_list_to_tptp (BCSet.elements cset)

let tmap2str m =
  let tmap2str' k v a =
    let vstring = Clause.to_string_tptp v.abstr_cl in
    let bs = "*-* " ^ Term.to_string k ^ " -> " ^  vstring in
    let abstracted_cls = "\n-- " ^ cset2str v.orig_cls ^ "--" in
    a ^ "\n" ^ bs ^ abstracted_cls
  in
  TMap.fold tmap2str' m ""

let to_string vl = function
  | Strong dom -> begin
    match vl with
    | `V0 ->
      "\n\t* Strong abstr cls: " ^ string_of_int (BCSet.cardinal dom.abstr_cls) ^
      "\n\t* Concrete clauses: " ^ string_of_int (BCSet.cardinal dom.concr_cls)
    | `V1 ->
      "\n\t* Strong abstr cls:\n" ^ cset2str dom.abstr_cls ^
      "\n\t* Concrete clauses:\n" ^ cset2str dom.concr_cls
    | `V2 ->
      "\n\t* Strong abstr cls:\n" ^ cset2str dom.abstr_cls ^
      "\n\t* Concrete clauses:\n" ^ cset2str dom.concr_cls ^
      "\n\t* SWP to Abstract clauses:" ^ tmap2str dom.swp2aclw
  end
  | Weak dom ->
    match vl with
    | `V0 -> "\n\t* Weak abstr cls: " ^ string_of_int (BCSet.cardinal dom)
    | `V1 | `V2 -> "\n\t* Weak abstr cls:\n" ^ cset2str dom
