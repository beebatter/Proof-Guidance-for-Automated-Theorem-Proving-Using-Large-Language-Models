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



(* index is based on extensions of feature index of S. Schulz  *)

open Lib
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_trace2
  | D_set

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_trace2 -> "trace2"
  | D_set -> "set"

let dbg_groups = [
  D_trace;
  (* D_trace2; *)
  D_set;
]
    
let module_name = "basicSubsumptionIndex"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)





(* Abstract "clause" *)
module type Elt = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val subsumes : subs_bck_mult:int -> t -> t -> subst

  val to_string : t -> string
  val get_lits : t -> lit list
end

module type Feature = sig
  type t

  type elt

  val compare : t -> t -> int

  val get_feature_list : elt -> t list 
end

module type FeatureCom = sig
  type t

  type elt

  val compare_pos : t -> t -> int
  val compare_val : t -> t -> int

  val get_feature_list : elt -> t list

  val to_string : t -> string  (* for debug *)
end

module type Options = sig
  val track_clauses : bool
end



module type Index = sig
  type elt

  type feature

  type index

  val create : unit -> index

  val add_clause : index -> (* feature list ->*) elt -> unit

  val remove_clause : index -> (* feature list -> *) elt -> unit

  (* val in_subs_index : index -> elt -> bool *)

  val is_empty : index -> bool

  val is_subsumed :
    ?pre_cond:(cl_in:elt ->cl_by:elt -> bool) -> subs_bck_mult:int ->
    index (* -> feature list*) -> elt -> (elt * Subst.subst) option
    
  val is_subsumed_strict : 
    subs_bck_mult:int -> index -> elt -> (elt * Subst.subst) option

  val find_subsumed : 
    subs_bck_mult:int -> index -> (* feature list -> *) elt -> elt list

  val find_subsumed_subst :
    subs_bck_mult:int -> index -> (* feature list ->*) elt -> (elt * Subst.subst) list
end

module type IndexData = sig
  type elt

  type feature

  type 'a index

  val create : unit -> 'a index

  val add : 'a index -> elt -> 'a -> unit

  val filter : 'a index -> elt -> ('a -> bool) -> unit

  (* val is_empty : 'a index -> bool *)



  val is_subsumed : 
    subs_bck_mult:int -> 'a index -> elt -> (elt * Subst.subst * 'a list) option

  val is_subsumed_strict : 
    subs_bck_mult:int -> 'a index -> elt -> (elt * Subst.subst * 'a list) option

  val find_subsumed :
    subs_bck_mult:int -> 'a index -> elt -> (elt * Subst.subst * 'a list) list
end

let pre_cond_true_fun ~cl_in ~cl_by = true



module Internal (Elt : Elt) (FeatureCom : FeatureCom with type elt = Elt.t) = struct
  type elt = Elt.t
  
  type feature = FeatureCom.t
  
  module VIndexM = VectorIndex.MakeCom(FeatureCom)

  (* hashing features *)
  module FeatKey = struct
    type t = elt * ((feature list) param) (* (c,Undef) is used only for searching and is never added *)
    let equal (c1,_) (c2,_) = Elt.equal c1 c2
    let hash (c,_) = Elt.hash c
  end

  module WHFeat = Weak.Make(FeatKey)

  (*--------*)

  module ESet = Set.Make(Elt)
  module EMap = Map.Make(Elt)



  let to_string_feat_list l =
    list_to_string FeatureCom.to_string l ";"

  let get_clause_feature_list ind_feats clause = 
    try 
      let _c, feat_param = WHFeat.find ind_feats (clause, Undef) in 
      get_param_val feat_param
    with Not_found -> 
      let feat = FeatureCom.get_feature_list clause in 
      WHFeat.add ind_feats (clause, Def feat);
      feat
end



(* Based on Compressed index: see vectorIndex Compressed *)
module MakeCom (Elt : Elt) (FeatureCom : FeatureCom with type elt = Elt.t) (Options: Options) (* : Index *) = struct
  include Internal (Elt) (FeatureCom)

  type core_index = ESet.t VIndexM.index

  (* This index stores, in the leaf corresponding to a given feature vector, 
     a set of [elt]s which have that feature vector. *)
  type index = {
    mutable ind : core_index;
    mutable ind_clauses : ESet.t; (* indexed clauses *)  
    mutable ind_feats : WHFeat.t;
    mutable dbg_set : ESet.t
  }

  (* when debug then we store all clauses added to index in
     debug_set and check operations w.r.t. to that set *)
  
  let create () = {
    ind = VIndexM.create ();
    ind_clauses = ESet.empty; 
    ind_feats = WHFeat.create 1009;
    dbg_set = ESet.empty
  }

  let clear i = 
    i.ind <- VIndexM.create();
    i.ind_clauses <- ESet.empty;
    WHFeat.clear i.ind_feats;
    i.dbg_set <- ESet.empty;
    ()

  let in_subs_index index clause = 
    dassert (fun () -> Options.track_clauses);
    ESet.mem clause index.ind_clauses 

  let is_empty index = 
    (* ESet.is_empty index.ind_clauses *)
    VIndexM.is_empty index.ind



  let add_to_ind_clauses index c = 
    dassert (fun () -> Options.track_clauses);
    index.ind_clauses <- ESet.add c index.ind_clauses

  let remove_ind_clause_list index cl_list = 
    dassert (fun () -> Options.track_clauses);
    let new_ind_clauses = 
      List.fold_left (fun rest c -> ESet.remove c rest) index.ind_clauses cl_list 
    in
    index.ind_clauses <- new_ind_clauses



  let get_clause_feature_list index clause = 
    get_clause_feature_list index.ind_feats clause



  let add_clause_normal index clause =
    dbg D_trace @@ lazy (sprintf "add_clause_normal: %s" (Elt.to_string clause));
    if Options.track_clauses && in_subs_index index clause then (
      dbg D_trace @@ lazy (sprintf "already in index");
      ()
    ) else (
      let feature_list = get_clause_feature_list index clause in
      dbg D_trace @@ lazy (sprintf "             feat: %s" (to_string_feat_list feature_list));
      if Options.track_clauses then (
        add_to_ind_clauses index clause;
        dassert (fun () -> in_subs_index index clause);
      );
      let elem_ref = VIndexM.add index.ind feature_list in
      match !elem_ref with
      | Elem elem ->
        dbg D_trace2 @@ lazy (sprintf "wasn't in index, existing elem_ref");
        elem_ref := Elem(ESet.add clause elem)
      | Empty_Elem -> 
        dbg D_trace2 @@ lazy (sprintf "wasn't in index, empty elem_ref");
        elem_ref := Elem(ESet.singleton clause)
    );
    dbg_env D_set (fun () -> index.dbg_set <- ESet.add clause index.dbg_set)



  (*--------------Forward Subsumption-------------------*)

  (* aux function to return clause with subst if that clause is subsumed *)
  let is_subsumed_by ?(pre_cond = pre_cond_true_fun) ~subs_bck_mult clause d = 
    if pre_cond ~cl_in:clause ~cl_by:d then
      try
        dbg D_trace2 @@ lazy (sprintf "is_subsumed_by candidate: %s" (Elt.to_string d));
        let unif = Elt.subsumes ~subs_bck_mult d clause in
        dbg D_trace2 @@ lazy (sprintf "is_subsumed_by succeeds");
        Some (d, unif)
      with Unif.Subsumption_failed ->
        dbg D_trace2 @@ lazy (sprintf "is_subsumed_by fails");
        None
    else
      None

  (* stops when f is Some(e) and returns Some(e) otherwise returns None *)
  let set_find_f f set =
    let folder cl accum =
      match accum with
      | Some _ -> accum
      | None -> f cl
      in
    ESet.fold folder set None

  let is_subsumed_normal ?(pre_cond = pre_cond_true_fun) ~subs_bck_mult index clause = 
    if is_empty index then None else
    let feature_list = get_clause_feature_list index clause in
    let result = VIndexM.findf_leq index.ind (set_find_f (is_subsumed_by ~pre_cond ~subs_bck_mult clause)) feature_list in
    dbg_env D_set (fun () -> 
      let result_set = set_find_f (is_subsumed_by ~pre_cond ~subs_bck_mult clause) index.dbg_set in
      (* let feature_list = get_clause_feature_list index clause in *)
      match result, result_set with 
      | None, None | Some _, Some _ -> ()
      | None, Some (d, _) -> failwith (sprintf "Inconsistency detected in is_subsumed: %s subsumed by %s but not found"
        (Elt.to_string clause) (Elt.to_string d)
      )
      | Some (d, _), None -> failwith (sprintf "Inconsistency detected in is_subsumed: %s not subsumed by %s but found"
        (Elt.to_string clause) (Elt.to_string d)
      )
    );
    result



  (*-----------Backward Subsumption-------------------*)

  (* from the set of clauses returnes (list of subsumed, the rest as set)  *)
  let get_subsumed ~subs_bck_mult set clause = 
    (* folder that choose where to put clause *)
    let folder cl (subsumed, rest) =
      try (* if needed can get unif here*)
        ignore (Elt.subsumes ~subs_bck_mult clause cl);
        (* here cl is subsumed *)
        (cl::subsumed, rest)
      with Unif.Subsumption_failed ->
        (* no subsumption, keep the clause *)
        (subsumed, ESet.add cl rest)
    in
    (* apply folder to the set *)
    ESet.fold folder set ([], ESet.empty)
    
  let find_subsumed_normal ~subs_bck_mult index clause =
    let feature_list = get_clause_feature_list index clause in
    let f followed_key_list elem_ref =
      match !elem_ref with
      | Elem clause_set ->
        let subsumed, rest = get_subsumed ~subs_bck_mult clause_set clause in
        (* dassert (fun () -> List.for_all (fun c -> ESet.mem c index.ind_clauses) subsumed); *)
        (* dassert (fun () -> List.for_all (in_subs_index index) subsumed); *)
        if Options.track_clauses then remove_ind_clause_list index subsumed;
        (* put rest back *)
        if ESet.is_empty rest then (
          (*Here is the error!!!*)
          VIndexM.remove index.ind (List.rev followed_key_list)
        ) else (
          elem_ref := Elem rest
        );
        (* return the subsumed clauses *)
        subsumed
      | Empty_Elem -> []
    in
    let result = VIndexM.findf_all_geq index.ind f feature_list in
    dbg_env D_set (fun () -> 
      let find_subsumed_set ~subs_bck_mult index clause =
        let subsumed, rest = get_subsumed ~subs_bck_mult index.dbg_set clause in
        index.dbg_set <- rest;
        subsumed
      in
      let result_set = find_subsumed_set ~subs_bck_mult index clause in
      (* let feature_list = get_clause_feature_list index clause in *)
      if List.compare_lengths result result_set <> Ord.eq then (
        failwith (sprintf "Inconsistency detected in find_subsumed: %d clauses subsumed but only %d found"
          (List.length result_set) (List.length result)
        )
      )
    );
    result



  (*-----------Backward Subsumption Resolution-------------------*)
  
  (* like get_subsumed but also returns substitutions *)
  let get_subsumed_subst ~subs_bck_mult set clause =
    (* folder that choose where to put clause *)
    let folder cl (subsumed, rest) =
      try (* if needed can get unif here*)
        let subs = Elt.subsumes ~subs_bck_mult clause cl in
        (* here cl is subsumed *)
        ((cl,subs)::subsumed, rest)
      with Unif.Subsumption_failed ->
        (* no subsumption, keep the clause *)
        (subsumed, ESet.add cl rest)
    in
    (* apply folder to the set *)
    ESet.fold folder set ([], ESet.empty)

  (* output: ((subsumed,subst) list *)
  (* we need substitution for subs. resolution *)
  let find_subsumed_subst ~subs_bck_mult index clause =
    let feature_list = get_clause_feature_list index clause in
    (* let all_subsumed = ref [] in *)
    let f followed_key_list elem_ref =
      match !elem_ref with
      | Elem clause_set ->
        let subsumed, rest = get_subsumed_subst ~subs_bck_mult clause_set clause in

        let subsumed_clauses, _ = List.split subsumed in
        (* dassert (fun () -> List.for_all (in_subs_index index) subsumed_clauses); *)
        if Options.track_clauses then remove_ind_clause_list index subsumed_clauses;

        if ESet.is_empty rest then (
          VIndexM.remove index.ind (List.rev followed_key_list)
        ) else (
          elem_ref := Elem rest
        );
        subsumed
      | Empty_Elem -> []
    in
    let result = VIndexM.findf_all_geq index.ind f feature_list in
    dbg_env D_set (fun () -> 
      let find_subsumed_set ~subs_bck_mult index clause =
        let subsumed, rest = get_subsumed_subst ~subs_bck_mult index.dbg_set clause in
        index.dbg_set <- rest;
        subsumed
      in
      let result_set = find_subsumed_set ~subs_bck_mult index clause in
      (* let feature_list = get_clause_feature_list index clause in *)
      if List.compare_lengths result result_set <> Ord.eq then (
        (* let result_set' = List.map (fun (c, _) -> c) result_set in *)
        failwith (sprintf "Inconsistency detected in find_subsumed: %d clauses subsumed but only %d found"
          (List.length result_set) (List.length result)
        )
      )
    );
    result



  (*------Removes clause from index--------------*)
  let remove_clause index clause =
    dbg D_trace @@ lazy (sprintf "remove_clause: %s" (Elt.to_string clause));
    if Options.track_clauses then (
      (* dassert (fun () -> in_subs_index index clause); *)
      dbg_env D_trace (fun () -> if not (in_subs_index index clause) then dbg D_trace @@ lazy (sprintf "already not in index %s" (Elt.to_string clause)));
    );

    if Options.track_clauses && not (in_subs_index index clause) then () else (  (*KK *)
      let feature_list = get_clause_feature_list index clause in
      if Options.track_clauses then (
        remove_ind_clause_list index [clause];
        dassert (fun () -> not @@ in_subs_index index clause);
      );
      let elem_ref = VIndexM.find index.ind feature_list in
      begin match !elem_ref with
      | Elem clause_set ->
          let new_set = ESet.remove clause clause_set in
          if ESet.is_empty new_set then
            VIndexM.remove index.ind feature_list
          else
            elem_ref := Elem new_set
      | Empty_Elem -> 
          VIndexM.remove index.ind feature_list
      end;
      dbg_env D_set (fun () -> index.dbg_set <- ESet.remove clause index.dbg_set)
  )
  

  (* Use normal versions *)
  let add_clause = add_clause_normal
  let is_subsumed = is_subsumed_normal
  let is_subsumed_strict = is_subsumed_normal ~pre_cond:(fun ~cl_in ~cl_by -> cl_by != cl_in)
  let find_subsumed = find_subsumed_normal
end



(* Unfortunately some code has to be duplicated because this index has to store things... *)
module MakeComData (Elt : Elt) (FeatureCom : FeatureCom with type elt = Elt.t) (* : IndexData *) = struct
  include Internal (Elt) (FeatureCom)

  type 'a core_index = ('a list) EMap.t VIndexM.index
  
  type 'a index = {
    mutable ind : 'a core_index;
    mutable ind_feats : WHFeat.t;
    mutable dbg_set : ESet.t
  }

  (* when debug then we store all clauses added to index in
     debug_set and check operations w.r.t. to that set *)
  
  let create () = {
    ind = VIndexM.create ();
    ind_feats = WHFeat.create 1009;
    dbg_set = ESet.empty;
  }

  let add index clause data = 
    dbg D_trace @@ lazy (sprintf "add: %s" (Elt.to_string clause));
    let feature_list = get_clause_feature_list index.ind_feats clause in
    dbg D_trace @@ lazy (sprintf "             feat: %s" (to_string_feat_list feature_list));
    (* add_to_ind_clauses index clause; *)
    let elem_ref = VIndexM.add index.ind feature_list in
    begin match !elem_ref with
    | Elem elem ->
      dbg D_trace2 @@ lazy (sprintf "wasn't in index, existing elem_ref");
      let elem' = 
        elem |> EMap.update clause (fun x -> 
          match x with
          | Some l -> Some (data :: l)
          | None -> Some [data]
        )
      in
      elem_ref := Elem elem'
    | Empty_Elem -> 
      dbg D_trace2 @@ lazy (sprintf "wasn't in index, empty elem_ref");
      let elem' = EMap.singleton clause [data] in
      elem_ref := Elem elem'
    end;
    dbg_env D_set (fun () -> index.dbg_set <- ESet.add clause index.dbg_set)

  let filter index clause f = 
    dbg D_trace @@ lazy (sprintf "filter: %s" (Elt.to_string clause));
    let feature_list = get_clause_feature_list index.ind_feats clause in
    (* remove_ind_clause_list index [clause]; *)
    let elem_ref = VIndexM.find index.ind feature_list in
    begin match !elem_ref with
    | Elem elem ->
      let elem' = 
        elem |> EMap.update clause (fun x -> 
          match x with
          | Some l -> 
            let l' = List.filter (neg f) l in
            if List.X.is_empty l' then None else Some l'
          | None -> None
        )
      in
      if EMap.is_empty elem' then
        VIndexM.remove index.ind feature_list
      else
        elem_ref := Elem elem'
    | Empty_Elem -> 
      VIndexM.remove index.ind feature_list  (* assert false? *)
    end;
    dbg_env D_set (fun () -> index.dbg_set <- ESet.remove clause index.dbg_set)



  (* aux function to return clause with subst if that clause is subsumed *)
  let is_subsumed_by ?(pre_cond = pre_cond_true_fun) ~subs_bck_mult cl_in cl_by data_list = 
    if pre_cond ~cl_in ~cl_by then
      try
        dbg D_trace2 @@ lazy (sprintf "is_subsumed_by candidate: %s" (Elt.to_string cl_by));
        let unif = Elt.subsumes ~subs_bck_mult cl_by cl_in in
        dbg D_trace2 @@ lazy (sprintf "is_subsumed_by succeeds");
        Some (cl_by, unif, data_list)
      with Unif.Subsumption_failed ->
        dbg D_trace2 @@ lazy (sprintf "is_subsumed_by fails");
        None
    else
      None

  (* stops when f is Some(e) and returns Some(e) otherwise returns None *)
  let set_find_f f set =
    let folder cl data_list accum =
      match accum with
      | Some _ -> accum
      | None -> f cl data_list
      in
    EMap.fold folder set None

  let is_subsumed' ~pre_cond ~subs_bck_mult (index: 'a index) clause : (elt * subst * 'a list) option = 
    (* if is_empty index then None else *)  (* TODO *)
    let feature_list = get_clause_feature_list index.ind_feats clause in
    let result = VIndexM.findf_leq index.ind (set_find_f (is_subsumed_by ~pre_cond ~subs_bck_mult clause)) feature_list in
    (* TODO: debug set *)
    (* dbg_env D_set (fun () -> 
      let result_set = set_find_f (is_subsumed_by ~pre_cond ~subs_bck_mult clause) index.dbg_set in
      (* let feature_list = get_clause_feature_list index clause in *)
      match result, result_set with 
      | None, None | Some _, Some _ -> ()
      | None, Some (d, _) -> failwith (sprintf "Inconsistency detected in is_subsumed: %s subsumed by %s but not found"
        (Elt.to_string clause) (Elt.to_string d)
      )
      | Some (d, _), None -> failwith (sprintf "Inconsistency detected in is_subsumed: %s not subsumed by %s but found"
        (Elt.to_string clause) (Elt.to_string d)
      )
    ); *)
    result

  let[@inline] is_subsumed ~subs_bck_mult index clause = 
    is_subsumed' ~pre_cond:(fun ~cl_in ~cl_by -> true) ~subs_bck_mult index clause

  let[@inline] is_subsumed_strict ~subs_bck_mult index clause = 
    is_subsumed' ~pre_cond:(fun ~cl_in ~cl_by -> cl_in != cl_by) ~subs_bck_mult index clause



  (* like get_subsumed but also returns substitutions *)
  let get_subsumed ~subs_bck_mult set clause =
    (* folder that choose where to put clause *)
    let folder cl data_list (subsumed, rest) = 
      try
        let subs = Elt.subsumes ~subs_bck_mult clause cl in
        ((cl,subs,data_list)::subsumed, rest)
      with Unif.Subsumption_failed ->
        (subsumed, EMap.add cl data_list rest)
    in
    (* apply folder to the set *)
    EMap.fold folder set ([], EMap.empty)

  (* output: ((subsumed,subst) list *)
  (* we need substitution for subs. resolution *)
  let find_subsumed ~subs_bck_mult index clause =
    let feature_list = get_clause_feature_list index.ind_feats clause in
    (* let all_subsumed = ref [] in *)
    let f followed_key_list elem_ref =
      match !elem_ref with
      | Elem clause_set ->
        let subsumed, rest = get_subsumed ~subs_bck_mult clause_set clause in
        (* let subsumed_clauses, _ = List.split subsumed in *)
        (* remove_ind_clause_list index subsumed_clauses; *)

        if EMap.is_empty rest then (
          VIndexM.remove index.ind (List.rev followed_key_list)
        ) else (
          elem_ref := Elem rest
        );
        subsumed
      | Empty_Elem -> []
    in
    let result = VIndexM.findf_all_geq index.ind f feature_list in
    (* dbg_env D_set (fun () -> 
      let find_subsumed_set ~subs_bck_mult index clause =
        let subsumed, rest = get_subsumed ~subs_bck_mult index.dbg_set clause in
        index.dbg_set <- rest;
        subsumed
      in
      let result_set = find_subsumed_set ~subs_bck_mult index clause in
      (* let feature_list = get_clause_feature_list index clause in *)
      if List.compare_lengths result result_set <> Ord.eq then (
        (* let result_set' = List.map (fun (c, _) -> c) result_set in *)
        failwith (sprintf "Inconsistency detected in find_subsumed: %d clauses subsumed but only %d found"
          (List.length result_set) (List.length result)
        )
      )
    ); *)
    result
end





(*--------------Currently Not Used, Replaced by Compressed version Above-------*)

(*--------------Based on Uncompressed Feature Index-------------------------*)
(* all functions defined exactly as in the compressed index *)
(*but here we use uncompressed module, (with the same interface as compressed) *)

(********************************************************************************

(* COMMENTED START *)

module Make (Feature: Feature) = struct
  type feature = Feature.t
  
  module VIndexM = VectorIndex.Make (Feature)
  type core_index = (clause list) VIndexM.index


  (* hashing features *)
  module FeatKey = struct
    type t =  clause * ((feature list) param)  (* (c,Undef) is used only for searching and is never added *)
    let equal (c1,_) (c2,_) = c1 == c2
    let hash (c,_) = Clause.hash c
  end
      
  module WHFeat = Weak.Make(FeatKey)

  (*--------*)

  type index = {
    mutable ind : core_index;
    mutable ind_clauses : BCSet.t; (* indexed clauses *)
    mutable ind_feats : WHFeat.t;
    mutable dbg_list : clause list;
  }

  (* when debug then we store all clauses added to index in
     debug_set and check operations w.r.t. to that set *)
  
  (* let debug_set = ref BCSet.empty *)

  let create () = {
    ind = VIndexM.create ();
    ind_clauses = BCSet.empty; 
    ind_feats = WHFeat.create 1009;
    dbg_list = []
  }



  let in_subs_index index clause = BCSet.mem clause index.ind_clauses 

  let add_to_ind_clauses index c = 
    index.ind_clauses <- BCSet.add c index.ind_clauses
        
  let remove_ind_clause_list index cl_list = 
    let new_ind_clauses = 
      List.fold_left (fun rest c -> (BCSet.remove c rest)) index.ind_clauses cl_list in
    index.ind_clauses <- new_ind_clauses

  let get_clause_feature_list index clause = 
    try 
      let (_c, feat_param) = WHFeat.find index.ind_feats (clause, Undef) in 
      get_param_val feat_param
    with 
      Not_found ->
        let feat = Feature.get_feature_list clause in 
        WHFeat.add index.ind_feats (clause, Def(feat));
        feat

  (* when debug then we store all clauses added to index in
     debug_list and check operations w.r.t. to that list *)

  (* let (debug_list : (clause list) ref) = ref [] *)

  (* let create () = VIndexM.create () *)

  (* we have normal and debug versions  *)
  let add_clause_normal index clause =
    if in_subs_index index clause then 
      ()
    else (
      let feature_list = get_clause_feature_list index clause in
      (* let feature_list =  Feature.get_feature_list clause in *)
      let elem_ref = VIndexM.add index.ind feature_list in
      match !elem_ref with
      | Elem(elem) ->
        elem_ref := Elem(clause :: elem);
        add_to_ind_clauses index clause
        (* Clause.set_ps_in_subsumption_index true clause *)
      | Empty_Elem -> 
        elem_ref := Elem([clause])
    )

  let add_clause_debug index clause =
    add_clause_normal index clause;
    index.dbg_list <- clause::index.dbg_list 


  (*--------------Forward Subsumption-------------------*)

  let is_subsumed_normal ?(pre_cond = pre_cond_true_fun) ~subs_bck_mult index clause =
    (* let feature_list = Feature.get_feature_list clause in *)
    let feature_list = get_clause_feature_list index clause in
    let is_subsumed_by d =
      (* if not (Clause.get_is_dead d) then *)
      if pre_cond ~cl_in:clause ~cl_by:d then (
        try
          (* out_str ("Trying subs: "^(Clause.to_string clause)^" by "
             ^(Clause.to_string d)^"\n");*)
          let unif = Unif.subsumes ~subs_bck_mult d clause in
          Some ((d, unif))
        with Unif.Subsumption_failed ->
          (* out_str ("Clause to subume: "
          ^(Clause.to_string clause)^" "
          ^(to_string_feat_list feature_list)^"\n");
          out_str ("Failed by: "^(Clause.to_string d)^"\n");*)
          None
      ) else (
        None
      )
      (* else None *)
    in
    VIndexM.findf_leq index.ind (list_findf is_subsumed_by) feature_list

  (* List version *)
  let is_subsumed_list ?(pre_cond = pre_cond_true_fun) ~subs_bck_mult  index clause =
    let f d =
      if pre_cond ~cl_in:clause ~cl_by:d then (
        try
          let unif = Unif.subsumes ~subs_bck_mult d clause in
          Some ((d, unif))
        with Unif.Subsumption_failed -> 
          None
      ) else (
        None
      )
    in
    match list_findf f index.dbg_list with
    | Some(x) -> Some(x)
    | None -> None

  (* Debug version *)
  let is_subsumed_debug ?(pre_cond = pre_cond_true_fun)  ~subs_bck_mult index clause =
    match is_subsumed_normal ~pre_cond ~subs_bck_mult index clause with
    | Some(x) -> Some(x)
    | None -> (
      match is_subsumed_list ~pre_cond ~subs_bck_mult index clause with
      | Some((d, unif)) ->
        out_str (
          "\n Clause: "^(Clause.to_string clause)^"\n"
          ^" Subsumed by "^(Clause.to_string d)^"\n"
          ^"but not by a clause in index\n"
        );
        failwith "is_subsumed_debug: subsumed by list but not by index"
      | None -> None
    )
    
  (*-----------Backward Subsumption-------------------*)
  (*
    let eliminate_dead clauses =
    List.filter
    (fun c -> not (Clause.get_bool_param Clause.is_dead c))
  *)
  
  (* out_str ("\n Clause: "^(Clause.to_string clause)
     ^" "^(to_string_feat_list feature_list)^"\n"
     ^" Subsumed by "^(Clause.to_string cl)^"\n");*)
    
    (* from the list of clauses returnes (list of subsumed, the rest)  *)
  let rec get_subsumed ~subs_bck_mult clause_list clause = 
    match clause_list with
    | h:: tl -> (
      try  (* if needed can get unif here*)
        let _ = Unif.subsumes ~subs_bck_mult clause h in
        let rest_subsumed, rest = get_subsumed ~subs_bck_mult tl clause in
        (h :: rest_subsumed, rest)
      with Unif.Subsumption_failed ->
        let rest_subsumed, rest = get_subsumed ~subs_bck_mult tl clause in
        (rest_subsumed, h :: rest)
    )
    | [] -> ([],[])
    
  let find_subsumed_normal ~subs_bck_mult index clause =
    let feature_list = get_clause_feature_list index clause in
    (* let feature_list = Feature.get_feature_list clause in *)
    (* let all_subsumed = ref []in *)
    let f followed_key_list elem_ref =
      match !elem_ref with
      | Elem(clause_list) ->
        let (subsumed, rest) = get_subsumed ~subs_bck_mult clause_list clause in
        (*
        List.iter
          (Clause.set_ps_in_subsumption_index false) subsumed;
        *)
        remove_ind_clause_list index subsumed;

        (* (out_str
           (
           "Subsumer: "
           ^(Clause.to_string clause)^"\n"
           ^"Clauses in Leaf: "
           ^(Clause.clause_list_to_string clause_list)^"\n"
           ^"Subsumed: "
           ^(Clause.clause_list_to_string subsumed)^"\n"));*)
        if List.X.is_empty rest then (  (*Here is the error!!!*)
          VIndexM.remove index.ind (List.rev followed_key_list)
          (* out_str ("Remove empty leaf from VIndexM \n"
          ^"removed path: "^(to_string_feat_list (List.rev followed_key_list))^"\n" ) *)
        ) else (
          elem_ref := Elem(rest)(* (if (subsumed != []) then
          (out_str
          ("Old clause list"
          ^(Clause.clause_list_to_string clause_list)^"\n"
          ^"New clause list "
          ^(Clause.clause_list_to_string rest)^"\n");
          elem_ref:= Elem(rest))
          ) *)
        );
        subsumed
      | Empty_Elem -> []
    in
    VIndexM.findf_all_geq index.ind f feature_list

  (*-----List version------------------*)
  let find_subsumed_list ~subs_bck_mult index clause =
    let subsumed, rest = get_subsumed ~subs_bck_mult index.dbg_list clause in
    index.dbg_list <- rest;
    subsumed

  (*-----Debug version------------------*)
  let find_subsumed_debug ~subs_bck_mult index clause =
    let subsumed_from_index =
      find_subsumed_normal ~subs_bck_mult index clause
    in
    let subsumed_debug, rest_debug = get_subsumed ~subs_bck_mult index.dbg_list clause in
    (*debug*)
    (* if subsumed_from_index != []
    then
    out_str
    ("find_subsumed_debug: "
    ^(Clause.to_string clause)
    ^"Subsumes from index "
    ^(Clause.clause_list_to_string subsumed_from_index)^"\n"
    ^"Subsumes from debug_list"
    ^(Clause.clause_list_to_string subsumed_debug)^"\n"
    ^"Debug_list: "
    ^(Clause.clause_list_to_string !debug_list)^"\n"); *)
    if List.compare_lengths subsumed_from_index subsumed_debug <> Ord.eq then (
      out_str (
        "\n Subsumed in debug: "^"\n"
        ^"By Clause "^(Clause.to_string clause)^"\n"
        ^(list_to_string Clause.to_string subsumed_debug "\n")^"\n"
      );
      failwith "subsumptionIndex find_subsumed_debug: Not Complete !!!"
    ) else (
      index.dbg_list <- rest_debug;
      subsumed_from_index
    )



  (*-----------Backward Subsumption Resolution-------------------*)
  
  (* like get_subsumed but also returns substitutions *)
  let rec get_subsumed_subst ~subs_bck_mult clause_list clause = 
    match clause_list with 
    | h :: tl -> (
      try (* if needed can get unif here*)
        let subs = Unif.subsumes ~subs_bck_mult clause h in
        let (rest_subsumed, rest) = get_subsumed_subst ~subs_bck_mult tl clause in
        ((h, subs) :: rest_subsumed, rest)
      with Unif.Subsumption_failed ->
        let (rest_subsumed, rest) = get_subsumed_subst ~subs_bck_mult tl clause in
        (rest_subsumed, h:: rest)
    )
    | [] -> ([],[])
    
  (* output: ((subsumed,subst) list *)
  (* we need substitution for subs. resolution *)
  let find_subsumed_subst ~subs_bck_mult index clause =
    let feature_list = get_clause_feature_list index clause in
    (* let feature_list = Feature.get_feature_list clause in *)
    (*      let all_subsumed = ref []in*)
    let f followed_key_list elem_ref =
      match !elem_ref with
      | Elem(clause_list) ->
        let (subsumed, rest) = get_subsumed_subst ~subs_bck_mult clause_list clause in
        (* (out_str
        (
        "Subsumer: "
        ^(Clause.to_string clause)^"\n"
        ^"Clauses in Leaf: "
        ^(Clause.clause_list_to_string clause_list)^"\n"
        ^"Subsumed: "
        ^(Clause.clause_list_to_string subsumed)^"\n")); *)
        (*
        List.iter
          (fun (c, _) ->
            (Clause.set_ps_in_subsumption_index false c)) subsumed;
        *)

        let subsumed_clauses, _ = List.split subsumed in
        remove_ind_clause_list index subsumed_clauses;

        if List.X.is_empty rest then (
          VIndexM.remove index.ind (List.rev followed_key_list)
          (* out_str ("Remove empty leaf from VIndexM \n"
          ^"removed path: "^(to_string_feat_list (List.rev followed_key_list))^"\n" ) *)
        ) else (
          elem_ref := Elem(rest)
        );
        (* (if (subsumed != []) then
        (out_str
        ("Old clause list"
        ^(Clause.clause_list_to_string clause_list)^"\n"
        ^"New clause list "
        ^(Clause.clause_list_to_string rest)^"\n");
        elem_ref:= Elem(rest))
        )  *)
        subsumed
      | Empty_Elem -> []
  in
  VIndexM.findf_all_geq index.ind f feature_list

  (*-----List version------------------*)
  let find_subsumed_list_subst ~subs_bck_mult index clause =
    let (subsumed, rest) = get_subsumed_subst ~subs_bck_mult index.dbg_list clause in
    index.dbg_list <- rest;
    subsumed

  (*-----Debug version------------------*)
  let find_subsumed_subst_debug ~subs_bck_mult index clause =
    let subsumed_from_index =
      find_subsumed_subst ~subs_bck_mult index clause
    in
    let subsumed_debug, rest_debug = get_subsumed_subst ~subs_bck_mult index.dbg_list clause in
    (*debug*)
    (* if subsumed_from_index != []
    then
    out_str
    ("find_subsumed_debug: "
    ^(Clause.to_string clause)
    ^"Subsumes from index "
    ^(Clause.clause_list_to_string subsumed_from_index)^"\n"
    ^"Subsumes from debug_list"
    ^(Clause.clause_list_to_string subsumed_debug)^"\n"
    ^"Debug_list: "
    ^(Clause.clause_list_to_string !debug_list)^"\n"); *)
    
    if List.compare_lengths subsumed_from_index subsumed_debug <> Ord.eq then (
      let subsumed_debug_cl_list =
        List.map (fun (c, _) -> c) subsumed_debug 
      in
      out_str (
        "\n Subsumed in debug: "^"\n"
        ^"By Clause "^(Clause.to_string clause)^"\n"
        ^(list_to_string Clause.to_string subsumed_debug_cl_list "\n")^"\n" 
      );
      failwith "subsumptionIndex find_subsumed_subst_debug: Not Complete !!!"
    ) else (
      index.dbg_list <- rest_debug;
      subsumed_from_index
    )
  
  (*------Removes clause from index--------------*)
  let remove_clause index clause =
    let feature_list = get_clause_feature_list index clause in
    (* Clause.set_ps_in_subsumption_index false clause; *)
    remove_ind_clause_list index [clause];

    let elem_ref = VIndexM.find index.ind feature_list in
    match !elem_ref with
    | Elem(clause_list) ->
      let new_list =
        List.filter (fun c -> not (clause == c)) clause_list
      in
      if List.X.is_empty new_list then
        VIndexM.remove index.ind feature_list
      else
        elem_ref := Elem(new_list)
    | Empty_Elem -> 
      VIndexM.remove index.ind feature_list
    
  (*---- List version ---------------*)
  let remove_clause_list index clause =
    index.dbg_list <- List.filter (fun c -> clause != c) (index.dbg_list)
  
  (*---- Debug version ---------------*)
  let remove_clause_debug index clause =
    remove_clause index clause;
    remove_clause_list index clause



  (*-------------change debug/list/normal versions--------------------*)

  (*
  let () = out_str "\n !!!!!!!!!!!!!!!!!!Subsumption List!!!!!!!!!!!!!!\n"
  
  let add_clause _feature_list clause _index_ref = add_clause_list clause
  let is_subsumed _feature_list clause _index_ref = is_subsumed_list clause
  let find_subsumed _feature_list clause _index_ref = find_subsumed_list clause
  let find_subsumed_subst _ _ clause = find_subsumed_list_subst clause
  let remove_clause _ _ clause = remove_clause_list clause
  *)

  let () = out_str "\n !!!!!!!!!!!!!!!!!!Subsumption Debug!!!!!!!!!!!!!!\n"

  let add_clause = add_clause_debug
  let is_subsumed = is_subsumed_debug
  let find_subsumed = find_subsumed_debug
  let find_subsumed_subst = find_subsumed_subst_debug
  let remove_clause = remove_clause_debug

  (*
  let add_clause = add_clause_normal
  let is_subsumed = is_subsumed_normal
  let find_subsumed = find_subsumed_normal
  *)
end

(* COMMENTED END *)

****************************************************************************************)





(*---------------------------------------------------------------------------*)
(* concrete implementation of compressed features; based on this concrete subsumtion index is defined using MakeCF *)
(* trasnferred from discount.ml *)
(*---------------------------------------------------------------------------*)

(*--------------Generalised feature index--------*)

module DefaultFeatureCom = struct 
  type elt = lit list

  (* symbol hierchy: *)
  (* first is position of the  hierarchy level, second is the value:*)
  (* 1 (or number of ) if the clause contains a symbol from   *)
  (* the corresponding hierarchy level and 0 otherwise *)
  (* symbol information SymbF: *)
  (* first is the symbol group, second is depth, *)
  (* third is the number  of occurences of symbs of this group at this depth *)
  (* the lex. combination of group and depth is a generalised position *)
  (* these positions are greater than any of HierchF positions  *)
  (* this should be also reflected in the compare function in the feature module below*)
  type feature = 
    | HierF of int * int 
    | SymF of int * int * int

  let feature_to_string = function
    | HierF (pos,v) -> 
      sprintf "H(%d,%d)" pos v
    | SymF (sym_gr, depth, v) ->  
      sprintf "SymF(%d,%d,%d)" sym_gr depth v

  let feature_list_to_string flist = 
    list_to_string feature_to_string flist ";"

  (*--------------Group Hierachy for the feature index--------*)
  (* we build a compressed feature index where *) 
  (* all minimal values are compressed:    *)
  (* compressed feature vector is a list of pairs (p,v) where p is a  position *)
  (* with a non-zero value v in the original vector index *)
  (* p can be a "generalised position" as in SymF, the pairs (sym_gr,depth) *)
  (* are generalised positions (ordered lex.), *)
  (* these positions can be seen as an ordinal positions  *)


  (*-------Symbol groups------------------*)
  (* main features are based on occurrences of symbols, all symbols *)
  (* are partitioned into symbol groups (when are added into SymbolDB), *)
  (* the number of groups is Symbol.max_num_of_sym_groups *)
  (* in order for the signature to be extendable (and the feature vector flexible)*)
  (* we do not change the number of groups *)

  (*--------Symbol Hierarchy---------------*)
  (* in order to maximize sharing we build a signature group hierarchy:       *)
  (* 1 level: all symbols are partitioned into two groups,                    *)
  (* at the next level each subgroup is partitioned again into two subgroups  *)
  (* the first k bits of the binary representation of a group  encodes        *)
  (* the subgroup of the k-th level of the group hierarchy  it belongs to;    *)
  (* the first k bits  also correspond                                        *)
  (* to the (group hierarchy) index in the feature vector                     *)

  let group_hierarchy_depth = 3

  (* k >= 1 *)
  let rec bit_k_ones k = 
    (* if k > 0
    then
      succ ((bit_k_ones (pred k)) lsl 1)
    else 0 *)
    (* Equivalent *)
    1 lsl k - 1

  (* high n mask is assumed to be n 1's  *)
  let get_first_n_bits high_n_mask k = 
    high_n_mask land k

  let create_bit_high_mask_array k  = 
    let a = Array.make k 0 in 
    for i = 0 to k-1 do a.(i) <- bit_k_ones (succ i) done;
    a

  let bit_high_mask_array = 
    create_bit_high_mask_array (group_hierarchy_depth+1)

  (* we use a global array (group_array) to represent (part of) the feature vector *)
  (* corresponding to the symbol information of the considered clause *)
  (* group array is cleaned each time get_sym_group_compressed_features is called *)
  (* (it could be replaced with a local hash table) *)
  (* in the group array, places are reserved for the subgroup info *)
  (* e.g. if a group number i is in binary 101....then the symbol is in subgroups *)
  (*  1,10, 101, ..., the corresponding indexes in the group array are *)
  (* shit(1)+1, shif(2) +1,shift(3) + 5 where shift corresponds to the index where *)
  (* the hierarchy begins in the array, shift(i) = (\sum_{k=1}^{k} 2^k)-1 *)
  (* and numbers are just truncated i bits of the symbol_group; *)
  (* the boolean 0,1,  can be easily changed to the number of symbols*)
  (* in the corresponding subgroup *)



  let group_array_size = 
    bit_k_ones (group_hierarchy_depth+1)

  let group_array = 
    Array.make group_array_size 0 

  let clear_group_array () = 
    Array.fill group_array 0 group_array_size 0

  let group_array_to_list () = 
    let current_list  = ref [] in
    (* not to reverse the list we traverse the array in desc. order *)
    for i = (group_array_size-1) downto 0
    do
      if not (group_array.(i) = 0) then
        current_list := HierF(i,group_array.(i)) :: !current_list
    done;
    !current_list


  (* symbol info stored first in a map *)
  (* key is sym_group * depth *)
  module SymFKey = PairIntKey

  module SymFM = Map.Make (SymFKey)

  (* makes an ordered list from the map*)
  let symb_info_db_to_list symb_info_db = 
    let f (sym_group,depth) val_ref rest =
      SymF(sym_group,depth, !val_ref) :: rest 
    in
    SymFM.fold f symb_info_db []

  let get_sym_group_compressed_features lits = 
    clear_group_array ();
    (* let lits = Clause.get_literals clause in *)
    let symb_info_db = ref SymFM.empty in
    let f_t lit =   
      (* let is_neg = ref false in
      if Term.is_neg_lit lit
      then (is_neg:=true)
      else (is_neg:=false); *)
      let is_neg = Term.is_neg_lit lit in
      let f_sym depth sym = 
        if Symbol.symb_neg == sym || Symbol.symb_typed_equality == sym
        then () (* do not take into account equality or neg symbol *)
        else (
          let sym_group = Symbol.get_group sym in
          (*first fill group hierarchy*)  
          for i = 1 to group_hierarchy_depth do
            let i_ones = bit_high_mask_array.(i-1) in
            let shift = i_ones - 1 in
            let index = shift + get_first_n_bits i_ones sym_group in
            (* out_str ("Index: "^(string_of_int index)^"\n");*)
            group_array.(index) <- 1
                (*     (out_str ("sym_groups_start: "^(string_of_int sym_groups_start)
                 ^" i: "^(string_of_int i)
                 ^" shift: "^(string_of_int shift)
                 ^" index: "^(string_of_int index))
                 )*)
          done;

          (*now fill sym groups values*)
          let signed_depth = 
            if is_neg then -depth else depth 
          in
          try
            let v_ref = SymFM.find (sym_group,signed_depth) !symb_info_db in 
            v_ref := !v_ref+1
          with Not_found -> 
            symb_info_db := SymFM.add (sym_group,signed_depth) (ref 1) !symb_info_db 
        )
      in
      Term.iter_sym_depth f_sym lit
    in
    List.iter f_t lits;
    let symb_info_list = symb_info_db_to_list !symb_info_db in
    group_array_to_list () @ List.rev symb_info_list


  let get_feature_list clause = 
    let feats = get_sym_group_compressed_features clause in
    (*  out_str ("Clause: "^(Clause.to_string clause)^" "
        ^(feature_list_to_string feats)^"\n");*)
    feats

  (* ----- *)

  type t = feature

  (* compare position *)
  let compare_pos f1 f2 = 
    match f1, f2 with 
    | HierF (h1,_), HierF (h2,_) -> 
      Int.compare h1 h2
    | SymF (g1,d1,_), SymF (g2,d2,_) -> 
      (* (pair_compare_lex [@inlined]) int_compare int_compare (g1,d1) (g2,d2) *)
      let res1 : int = Int.compare g1 g2 in
      if res1 <> 0 then res1 else
      let res2 : int = Int.compare d1 d2 in
      res2
    (* SymF comes after HierF and therefore its positions are bigger *)
    | HierF _, SymF _ -> -1
    | SymF _, HierF _ ->  1

  (* compare the value *)
  let compare_val f1 f2 = 
    match f1, f2 with 
    | HierF (_,v1),  HierF (_,v2)  -> Int.compare v1 v2
    | SymF (_,_,v1), SymF (_,_,v2) -> Int.compare v1 v2
    | HierF _, SymF _ -> -1
    | SymF _, HierF _ ->  1

  let get_feature_list = get_feature_list

  let to_string  = feature_to_string
end
