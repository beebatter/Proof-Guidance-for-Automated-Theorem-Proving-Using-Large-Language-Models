(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(* TODO: monomorphic eq: open Lib  *)
module LI = Logic_interface
module AD = Ar_abstract_domain

type trgg_fn =
  | Empty
  | TrggFn of {map:LI.BCSet.t LI.SMap.t; prdtv:int LI.SMap.t; w:float}

type cone =
  | Empty
  | Cone of {e:LI.BCSet.t; ct:LI.SSet.t; avs:LI.SSet.t}

type details = {trgg_fn:trgg_fn; cone:cone}

let cnj_symbs = ref LI.SSet.empty

let size_cnj_symbs = ref 0.

let axcls_g = ref LI.BCSet.empty

let adom0 = ref (AD.empty Under)

let crs_g = ref LI.BCMap.empty

let rts_g = ref LI.BCMap.empty

let occ_g = ref LI.SMap.empty

let try_sat_g = ref false

let weight_inc = 10.

let empty_details = {trgg_fn = Empty; cone = Empty}

let should_try_sat () = !try_sat_g

let relevant_symbols =
  Clause.find_all_sym ~is_relevant_symb:Symbol.is_signature_symb

let fill_cnj_info cnj =
  cnj_symbs :=
    LI.BCSet.fold (fun c a -> LI.SSet.union (relevant_symbols c) a) cnj
      LI.SSet.empty;
  size_cnj_symbs := float_of_int (LI.SSet.cardinal !cnj_symbs)

let count_symbol symb accu =
  let aux e =
    match e with
    | Some c -> Some (c + 1)
    | None -> Some 1
  in
  if Symbol.is_signature_symb symb then LI.SMap.update symb aux accu
  else accu

let clause_calc cl (crs,rts,occ) =
  let rlv_symbs = relevant_symbols cl in
  let nmtr = LI.SSet.(cardinal (inter rlv_symbs !cnj_symbs)) in
  let r_a = float_of_int nmtr /. !size_cnj_symbs in
  let crs' = LI.BCMap.add cl rlv_symbs crs in
  let rts' = LI.BCMap.add cl r_a rts in
  let occ' = LI.SSet.fold count_symbol rlv_symbs occ in
  (crs', rts', occ')

let top_ord_lst is_asc = function
  | [] -> assert(false)
  | lst ->
    let ord (_,v1) (_,v2) = if is_asc then compare v1 v2 else compare v2 v1 in
    lst |> List.fast_sort ord |> List.hd |> snd

let select_uncommon_symbs tol occ rs =
  let occ_rs =
    LI.SMap.to_seq occ
    |> Seq.filter (fun (k,_) -> LI.SSet.mem k rs)
    |> LI.SMap.of_seq
  in
  let elgs, no_elgs = LI.SMap.partition (fun _ v -> v > 1) occ_rs in
  let tggc =
    (if LI.SMap.is_empty elgs then no_elgs else elgs)
    |> LI.SMap.bindings
  in
  let smll_occ = top_ord_lst true tggc |> float_of_int |> ( *. ) tol in
  List.filter (fun (_,v) -> smll_occ >= float_of_int v) tggc

let designate_trigger cl tf (tgg,_) =
  let aux v =
    match v with
    | Some s -> Some (LI.BCSet.add cl s)
    | None -> Some (LI.BCSet.singleton cl)
  in
  LI.SMap.update tgg aux tf

let clause_trigger_desgn w cl tf =
  assert (w >= 0. && not LI.(BCMap.is_empty !crs_g || BCMap.is_empty !rts_g ||
                             SMap.is_empty !occ_g));
  match LI.BCMap.(find_opt cl !crs_g, find_opt cl !rts_g) with
  | (Some rs, Some r_a) ->
    let tol = 1. +. (w *. r_a *. r_a) in
    select_uncommon_symbs tol !occ_g rs
    |> List.fold_left (designate_trigger cl) tf
  | _ -> tf

let trigger_function cls w =
  let map = LI.BCSet.fold (clause_trigger_desgn w) cls LI.SMap.empty in
  let prdtv = LI.SMap.map (fun v -> LI.BCSet.cardinal v) map in
  TrggFn {map = map; prdtv = prdtv; w = w}

let triggered_elts tf ss =
  let aux s ((trgs, tgdc) as a) =
    match LI.SMap.find_opt s tf with
    | Some cls -> (LI.SSet.add s trgs, LI.BCSet.union cls tgdc)
    | None -> a
  in
  LI.SSet.fold aux ss (LI.SSet.empty, LI.BCSet.empty)

let available_symbs trgs tgdc =
  let aux cl accu =
    match LI.BCMap.find_opt cl !crs_g with
    | Some rs -> LI.SSet.union rs accu
    | None -> accu
  in
  let cnlv_symbs = LI.BCSet.fold aux tgdc LI.SSet.empty in
  LI.SSet.diff cnlv_symbs trgs

let most_productive_tgrs prdtv trgs =
  assert(not (LI.SSet.is_empty trgs));
  let trgs_mem k _ = LI.SSet.mem k trgs in
  match LI.SMap.(filter trgs_mem prdtv |> bindings) with
  | [] -> LI.SSet.empty
  | lst ->
    let mpt = lst |> top_ord_lst false in
    List.filter (fun (_,v) -> mpt = v) lst
    |> List.fold_left (fun accu (k,_) -> LI.SSet.add k accu) LI.SSet.empty

let next_cone_level tf cone =
  match (tf, cone) with
  | (TrggFn {map}, Empty) ->
    let trgs, tgdc = triggered_elts map !cnj_symbs in
    let avs = available_symbs trgs tgdc in
    Cone {e = tgdc; ct = trgs; avs = avs}
  | (TrggFn _, (Cone {avs} as cone)) when LI.SSet.is_empty avs -> cone
  | (TrggFn {map; prdtv=p}, Cone {e; ct; avs}) ->
    let cnd_symbs, crr_trgs =
      if LI.SMap.is_empty p then (avs, LI.SSet.empty)
      else
        let mpt = most_productive_tgrs p avs in
        let rest_trgs = LI.SSet.diff mpt ct in
        let cnd_symbs = LI.SSet.diff avs rest_trgs in
        let crr_trgs = LI.SSet.union ct rest_trgs in
        (cnd_symbs, crr_trgs)
    in
    let trgs, tgdc = triggered_elts map cnd_symbs in
    let avs' = available_symbs trgs tgdc in
    let e' = LI.BCSet.union e tgdc in
    let ct' = LI.SSet.union crr_trgs trgs in
    Cone {e = e'; ct = ct'; avs = avs'}
  | (Empty, _) -> failwith ("ar_cone_symbols.ml:next_cone_level: " ^
                            "The trigger function is required")

let all_lits_pos_or_neg cls =
  let lits_lst = Clause.get_lits cls in
  List.(for_all Term.is_neg_lit lits_lst || for_all Term.is_pos_lit lits_lst)

let separate_neg_cnj cls =
  if LI.BCSet.exists Clause.is_negated_conjecture cls then
    LI.BCSet.partition Clause.is_negated_conjecture cls
(* KK:  can only use clauses from cls; e.g, subtyping could be applied to cls and input neg_conjecture won't be sutyped!; or generally might not be relevnt to these clauses *)
(*  else if !(Parser_types.neg_conjectures) <> [] then 
    (LI.BCSet.of_list !(Parser_types.neg_conjectures), cls)
*)
(* TODO: use sets not go via lists *)
  else 
    let (closest_to_conj, rest) = (Clause.split_smallest_dist_neg_conj (LI.BCSet.elements cls)) in 
    if (Lib.List.X.is_nonempty closest_to_conj) 
    then 
      (
       Ar_dbg.dbg D_trace (lazy ("closest_to_conj: "^(Clause.clause_list_to_string closest_to_conj)));
      (LI.BCSet.of_list closest_to_conj, LI.BCSet.of_list rest)    
      )
    else if LI.BCSet.exists Clause.has_non_prolific_conj_symb cls then
      LI.BCSet.partition Clause.has_non_prolific_conj_symb cls
    else if LI.BCSet.exists all_lits_pos_or_neg cls then
      LI.BCSet.partition all_lits_pos_or_neg cls
    else (cls, LI.BCSet.empty)
        
let abstract_domain_from_cone adom dtls = function
  | Cone {e} as cone ->
    let adom' = LI.BCSet.fold AD.un_add e adom in
    let dtls' = {dtls with cone = cone} in
    (adom', dtls')
  | Empty -> failwith ("ar_cone_symbols.ml:abstract_domain_from_cone: " ^
                       "Cone is empty")

let initial_cone tf =
  let dtls = {trgg_fn = tf; cone = Empty} in
  let cone = next_cone_level tf Empty in
  abstract_domain_from_cone !adom0 dtls cone

let prepare_abstract_domain ncnj axcls =
  adom0 := LI.BCSet.fold AD.un_add ncnj (AD.empty Under);
  if LI.BCSet.is_empty axcls then
    (!adom0, empty_details)
  else
    let accu = LI.(BCMap.empty, BCMap.empty, SMap.empty) in
    let crs, rts, occ = LI.BCSet.fold clause_calc axcls accu in
    crs_g := crs; rts_g := rts; occ_g := occ; axcls_g := axcls;
    let tf = trigger_function axcls 0. in
    initial_cone tf

let abstract opts cls =
  Ar_dbg.dbg D_trace (lazy("\n\t* In cone symbols abstraction"));
  try_sat_g := false;
  let ncnj, axcls = separate_neg_cnj cls in
  assert(not (LI.BCSet.is_empty ncnj));
  fill_cnj_info ncnj;
  let adom, dtls = prepare_abstract_domain ncnj axcls in
  Ar_dbg.dbg D_CSA (lazy(AD.to_string `V1 adom));
  (adom, dtls)

let reinit_abstract_domain tf e =
  let adom, dtls = initial_cone tf in
  let nc = next_cone_level dtls.trgg_fn dtls.cone in
  (match nc with
   | Cone {e=ne} when LI.BCSet.(cardinal ne = cardinal e) ->
     Ar_dbg.dbg D_CSA (lazy("\n\t* Change try_sat_g to true"));
     try_sat_g := true
   | Cone _ -> ()
   | Empty -> failwith ("ar_cone_symbols.ml:reinit_abstract_domain: " ^
                        "Empty cone; this should not happen")
  );
  abstract_domain_from_cone adom dtls nc

let refinement_strategies adom dtls =
  match (next_cone_level dtls.trgg_fn dtls.cone, dtls.cone, dtls.trgg_fn) with
  | Cone {e=e1}, Cone {e=e2}, TrggFn tf when
      LI.(BCSet.(cardinal e1 = cardinal e2) && not (SMap.is_empty tf.prdtv)) ->
    Ar_dbg.dbg D_CSA (lazy("\n\t* Use normal selection "));
    let tf' = TrggFn {tf with prdtv = LI.SMap.empty} in
    reinit_abstract_domain tf' e1
  | Cone {e=e1}, Cone {e=e2}, TrggFn tf when
      LI.BCSet.(cardinal e1 = cardinal e2) ->
    let nw = tf.w +. weight_inc in
    Ar_dbg.dbg D_CSA (lazy("\n\t* Change weight to " ^ string_of_float nw));
    let tf' = trigger_function !axcls_g nw in
    reinit_abstract_domain tf' e1
  | c, _, _ -> abstract_domain_from_cone adom dtls c

let refine adom dtls =
  Ar_dbg.dbg D_trace (lazy("\n\t* In cone symbols refinement"));
  let adom', dtls' =
    if dtls <> empty_details then
      refinement_strategies adom dtls
    else begin
      try_sat_g := true;
      (adom, dtls)
    end
  in
  Ar_dbg.dbg D_CSA (lazy(AD.to_string `V0 adom'));
  (adom', dtls')
