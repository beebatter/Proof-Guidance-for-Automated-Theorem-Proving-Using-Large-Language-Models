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
open TermDB.Open

(* subst.ml *)
type bound = int
type subst = Subst.subst
type term = Term.term
type bound_term = Term.bound_term
type var = Var.var
type bound_var = Var.bound_var
module SMap = Symbol.Map
module VMap = Var.VMap
module VBMap = Var.BMap
type symbol      = Symbol.symbol

type term_db_ref = TermDB.termDB ref

(* type assignment = var*term *)
(* exception Subst_bound_var_already_def *)

module SubstKey =
  struct
    type t = bound_var
    let compare = Var.compare_bvar
  end

module SubstM = Map.Make (SubstKey)

include SubstM

type bound_subst = bound_term SubstM.t

let create() = SubstM.empty

(*
let add v t subst =
  if mem v subst
  then 
    raise Subst_bound_var_already_def
  else 
    SubstM.add v t subst
*)
(*
let remove = SubstM.remove
let find = SubstM.find
let map = SubstM.map
let iter = SubstM.iter
let fold = SubstM.fold
let is_empty = SubstM.is_empty 
let mem = SubstM.mem
*)

(* the subsitution is a proper insatiator for a bound var *)
let rec is_proper_var bsubst bv =
  try
    let (new_b, t) = SubstM.find bv bsubst in
    match t with
    | Term.Fun(_, _, _) -> true
    | Term.Var(new_v, _) -> is_proper_var bsubst (new_b, new_v)
  with
    Not_found -> false

(* the subsitution is a proper insatiator for a particular bound *)
(* exception True  (* Control flow exception *)
let is_proper_instantiator bsubst bound =
  let f (bv, v) (new_b, t) rest =
    if bv = bound then (
      match t with
      | Term.Fun (_, _, _) -> 
        raise_notrace True
      | Term.Var (new_v, _) ->
        if is_proper_var bsubst (new_b, new_v) then 
          raise_notrace True
        else
          false
    ) else (
      false
    )
  in
  try
    fold f bsubst false
  with True -> 
    true *)

(* Version without exceptions *)
let is_proper_instantiator bsubst bound =
  let proper (bv, v) (new_b, t) =
    if bv = bound then
      match t with
      | Term.Fun (_, _, _) -> 
        true
      | Term.Var (new_v, _) ->
        if is_proper_var bsubst (new_b, new_v) then 
          true
        else
          false
    else
      false
  in
  (* More readable and faster *)
  SubstM.exists proper bsubst 

(* find normalised bound var by subst we assume that there is no var       *)
(* cycles in subst                                                         *)

let rec find_vnorm b_x bound_subst =
  let b_x_val = SubstM.find b_x bound_subst in
  match b_x_val with
  | b_v, Term.Var(v, _) ->
    begin try 
      find_vnorm (b_v, v) bound_subst 
    with Not_found -> 
      b_x_val
    end
  | _ -> 
    b_x_val

(* returns right normalised subst *)
let right_vnorm_bsubst bsubst = 
  let f bv bt norm_bsubst = 
    let norm_bt = find_vnorm bv bsubst in
    SubstM.add bv norm_bt norm_bsubst
  in
  SubstM.fold f bsubst (create())

(* returns (proper,non_proper) subst after right_vnorm_bsubst *)
let split_proper_inst bsubst = 
  let norm_bs = right_vnorm_bsubst bsubst in 
  let f _bv (b,t) = 
    match t with 
    | Term.Fun _ -> true 
    | Term.Var _ -> false
  in
  SubstM.partition f norm_bs

let split_left_bound bsubst b = 
  let f (bv,_v) _bt = 
    if bv = b then true else false
  in
  SubstM.partition f bsubst


(* applying bound subsitiution to the bound terms adding to term_db we     *)
(* assume that all terms are in term_db (including terms in subst)!        *)

type renaming_env = {
  (* map from types to next un-used variable of this type *)
  mutable ren_max_used_vars : Var.fresh_vars_env;

  (* map from bvars -> var  *)  
  mutable ren_map : var VBMap.t;
  (*mutable ren_term_db_ref : TermDB.termDB ref;*)
}

let init_renaming_env term_db_ref = {
  ren_max_used_vars = Var.init_fresh_vars_env ();
  ren_map = VBMap.empty;
  (*ren_term_db_ref = term_db_ref*)
}

(* changes ren_env adding next_unused var to used *)
let get_next_unused_var ren_env vtype = 
  Var.get_next_fresh_var ren_env.ren_max_used_vars vtype
    
let find_renaming renaming_env b_v =
  try
    VBMap.find b_v renaming_env.ren_map
  with Not_found ->
    let bv_type = Var.get_bv_type b_v in
    let next_var = get_next_unused_var renaming_env bv_type in
    (*
	  let new_var_term =
	  TermDB.add_ref (Term.create_var_term next_var) renaming_env.ren_term_db_ref in
	  renaming_env.ren_map <- (VBMap.add b_v new_var_term renaming_env.ren_map);
	  new_var_term
	  *)
    renaming_env.ren_map <- VBMap.add b_v next_var renaming_env.ren_map;
    next_var

let in_renaming renaming_env bv =
  VBMap.mem bv renaming_env.ren_map 

(* returns mapping: v-> u iff (bound,v) -> u is in ren_env  *)
let project_renaming ren_env bound = 
  let f (bv,v) u p_ren_map = 
    if bv = bound then 
      VMap.add v u p_ren_map
    else
      p_ren_map
  in
  VBMap.fold f ren_env.ren_map VMap.empty
 
(*-----------------------*)
(*
let rec apply_bsubst_bterm'
    term_db_ref renaming_env bsubst bterm =
  let (b_t, term) = bterm in
  match term with
  | Term.Fun(sym, args, _) ->
      let new_args =
	Term.arg_map_left
	  (fun term' ->
	    apply_bsubst_bterm'
	      term_db_ref renaming_env bsubst (b_t, term')
	  ) args in
      let new_term = Term.create_fun_term_args sym new_args in
      TermDB.add_ref new_term term_db_ref
  | Term.Var(v, _) ->
      let b_v = (b_t, v) in
      try
	let new_bterm = find b_v bsubst in
	apply_bsubst_bterm'
	  term_db_ref	renaming_env bsubst new_bterm
      with
	Not_found ->
	  add_var_term term_db_ref (find_renaming renaming_env b_v)
*)

(*module BTMap = Term.BTMap
(* type replacement = bound_term BTMap.t *)
type bound_replacement = bound_term BTMap.t*)
(* type replacement = Term.position * term
type bound_replacement = Term.position * bound_term *)
type bound_replacement = (bound_term * bound_term)

(* let rec apply_replacement replacement bterm : Term.term Bind.t = 
  try 
    BTMap.find bterm replacement
  with Not_found -> 
    let (b, term) = bterm in
    match term with
    | Term.Fun(sym, args, _) ->
      let new_args = 
        Term.arg_map_left (fun term' ->
          apply_replacement replacement (b,term')
        ) args
      in
      (b, Term.create_fun_term_args sym new_args)
    | Term.Var _ -> bterm *)





(* without adding to term_db_ref *)
(* new version: add to term_db_ref *)
let rec apply_bsubst_bterm''
  term_db_ref renaming_env bsubst bterm =
  let (b_t, term) = bterm in

  match term with
  | Term.Fun(sym, args, _) ->
    if Term.is_ground term then term else
    let new_args =
      Term.arg_map_left (fun (*i*) term' ->
        apply_bsubst_bterm''
          term_db_ref renaming_env bsubst (b_t, term')
      ) args
    in
    (* If all arguments physically equal, return physically equal term *)
    if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
      term
    else
      add_fun_term_args term_db_ref sym new_args
      (* TermDB.add_ref (Term.create_fun_term_args sym new_args) term_db_ref *)
    (* Term.create_fun_term_args sym new_args  *)

  | Term.Var(v, _) ->
    let b_v = (b_t, v) in
    try
      let new_bterm = SubstM.find b_v bsubst in
      apply_bsubst_bterm''
        term_db_ref renaming_env bsubst new_bterm
    with Not_found ->
      add_var_term term_db_ref (find_renaming renaming_env b_v)
      (* Term.create_var_term (find_renaming renaming_env b_v) *)

(**** V1
let rec apply_bsubst_bterm''_replacement
  ~replacement term_db_ref renaming_env bsubst bterm =
  let did_replacement, (b_t, term) = 
    let replace_from, replace_to = replacement in
    printf "bterm=%s replace_from=%s\n" (Bind.to_string Term.to_string bterm) (Bind.to_string Term.to_string replace_from); 
    if Term.equal_bterm bterm replace_from then  (* performance sensitive *)
      true, replace_to
    else
      false, bterm
  in

  let term' = 
  match term with
  | Term.Fun(sym, args, _) ->
    let new_args =
      if did_replacement then
        Term.arg_map_left (fun (*i*) term' ->
          apply_bsubst_bterm''
            term_db_ref renaming_env bsubst (b_t, term')
        ) args
      else
        Term.arg_map_left (fun (*i*) term' ->
          apply_bsubst_bterm''_replacement
            ~replacement term_db_ref renaming_env bsubst (b_t, term')
        ) args
    in
    (* If all arguments physically equal, return physically equal term *)
    if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
      term
    else
      add_fun_term_args term_db_ref sym new_args
    (* Term.create_fun_term_args sym new_args  *)

  | Term.Var(v, _) ->
    let b_v = (b_t, v) in
    try
      let new_bterm = SubstM.find b_v bsubst in
      apply_bsubst_bterm''
        term_db_ref renaming_env bsubst new_bterm
    with Not_found ->
      add_var_term term_db_ref (find_renaming renaming_env b_v)
      (* Term.create_var_term (find_renaming renaming_env b_v) *)
  in

  (* Also try to do replacement *after* the substitution *)
  if term' != term then
    let bterm' = (b_t, term') in
    let replace_from, replace_to = replacement in
    if Term.equal_bterm bterm' replace_from then (
      printf "bterm'=%s replace_from=%s\n" (Bind.to_string Term.to_string bterm') (Bind.to_string Term.to_string replace_from); 
      (* Here, did replacement *)
      apply_bsubst_bterm'' term_db_ref renaming_env bsubst replace_to (*b_t, term'*)
    )
    else
      term'
  else
    term
****)

(* V2 *)
let rec apply_bsubst_bterm''_replacement
  ~replacement term_db_ref renaming_env bsubst bterm =
  let (b_t, term) = bterm in
  let term' = 
    match term with
    | Term.Fun(sym, args, _) ->
      (* if Term.is_ground term then term else *)
      let new_args =
        Term.arg_map_left (fun (*i*) term' ->
          apply_bsubst_bterm''_replacement
            ~replacement term_db_ref renaming_env bsubst (b_t, term')
        ) args
      in
      (* If all arguments physically equal, return physically equal term *)
      if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
        term
      else
        add_fun_term_args term_db_ref sym new_args
      (* Term.create_fun_term_args sym new_args  *)

    | Term.Var(v, _) ->
      let b_v = (b_t, v) in
      try
        let new_bterm = SubstM.find b_v bsubst in
        apply_bsubst_bterm''
          term_db_ref renaming_env bsubst new_bterm
      with Not_found ->
        add_var_term term_db_ref (find_renaming renaming_env b_v)
        (* Term.create_var_term (find_renaming renaming_env b_v) *)
  in

  let replace_from, replace_to = replacement in  
  if term' == replace_from then
    replace_to
  else
    term'




(* with adding to term_db_ref*)
(* let rec apply_bsubst_bterm' term_db_ref renaming_env bsubst bterm =
  TermDB.add_ref (apply_bsubst_bterm'' renaming_env bsubst bterm) term_db_ref
  
let rec apply_bsubst_bterm'_replacement ~replacement term_db_ref renaming_env bsubst bterm =
  TermDB.add_ref (apply_bsubst_bterm''_replacement ~replacement renaming_env bsubst bterm) term_db_ref *)

(* let apply_bsubst_bterm' = apply_bsubst_bterm''
  
let apply_bsubst_bterm'_replacement = apply_bsubst_bterm''_replacement *)

let rec apply_bsubst_bterm' term_db_ref renaming_env bsubst bterm =
  apply_bsubst_bterm'' term_db_ref renaming_env bsubst bterm
  
let rec apply_bsubst_bterm'_replacement ~replacement term_db_ref renaming_env bsubst bterm =
  (* Apply subst to both sides of the replacement *)
  let replacement_from, replacement_to = replacement in
  let replacement_from' = apply_bsubst_bterm'' term_db_ref renaming_env bsubst replacement_from in
  let replacement_to'   = apply_bsubst_bterm'' term_db_ref renaming_env bsubst replacement_to in
  let replacement' = (replacement_from', replacement_to') in
  apply_bsubst_bterm''_replacement ~replacement:replacement' term_db_ref renaming_env bsubst bterm


  
let apply_bsubst_btlist'
    term_db_ref renaming_env bsubst bterm_list =
  List.map
    (apply_bsubst_bterm' term_db_ref renaming_env bsubst)
    bterm_list

let apply_bsubst_btlist'_replacement
    ~replacement term_db_ref renaming_env bsubst bterm_list =
  List.map
    (apply_bsubst_bterm'_replacement ~replacement term_db_ref renaming_env bsubst)
    bterm_list

let apply_bsubst_bterm ?replacement term_db_ref bsubst bterm =
  let renaming_env = init_renaming_env() in
  match replacement with
  | Some rep -> 
    apply_bsubst_bterm'_replacement ~replacement:rep term_db_ref
      renaming_env bsubst bterm
  | None ->
    apply_bsubst_bterm' term_db_ref
      renaming_env bsubst bterm


let apply_bsubst_btlist ?replacement term_db_ref bsubst bterm_list =
  let renaming_env = init_renaming_env() in
  match replacement with
  | Some rep -> 
    apply_bsubst_btlist'_replacement ~replacement:rep term_db_ref 
      renaming_env bsubst bterm_list
  | None ->
    apply_bsubst_btlist' term_db_ref 
      renaming_env bsubst bterm_list





(**********apply subst and return both new clause *******)
(********and normalised subst restricted to bound********)
(***************************                    *********)

(* TODO review this section *)

(* without adding to term_db_ref *)
(* new version: add to term_db_ref *)
let rec apply_bsubst_bterm_norm_subst''
    term_db_ref renaming_env bsubst bound norm_subst_ref bterm =
  let (b_t, term) = bterm in

  match term with
  | Term.Fun(sym, args, _) ->
    if Term.is_ground term then term else 
    let new_args =
      Term.arg_map_left (fun (*i*) term' ->
        apply_bsubst_bterm_norm_subst''
          term_db_ref 
          renaming_env
          bsubst bound norm_subst_ref (b_t, term')
      ) args
    in
    if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
      term
    else
      add_fun_term_args term_db_ref sym new_args
    (* Term.create_fun_term_args sym new_args *)

  | Term.Var(v, _) ->
    let b_v = (b_t, v) in
    let normalize bound_var =
      try
        let new_bterm = SubstM.find bound_var bsubst in
        apply_bsubst_bterm_norm_subst''
          term_db_ref 
          renaming_env
          bsubst bound norm_subst_ref new_bterm
      with Not_found ->
        add_var_term term_db_ref (find_renaming renaming_env bound_var)            
        (* Term.create_var_term (find_renaming renaming_env bound_var) *)
    in
    if b_t = bound then
      let norm_subst', value = 
        !norm_subst_ref |> Subst.find_add v (fun () -> 
          (* TermDB.add_ref (normalize b_v) term_db_ref *)
          normalize b_v
        )
      in
      norm_subst_ref := norm_subst';
      value
    else
      normalize b_v

let rec apply_bsubst_bterm_norm_subst''_replacement
    ~replacement term_db_ref renaming_env bsubst bound norm_subst_ref bterm =
  let (b_t, term) = 
    if Term.equal_bterm (bterm: term Bind.t) (fst replacement) then
      snd replacement
    else
      bterm
  in

  match term with
  | Term.Fun(sym, args, _) ->
    (* if Term.is_ground term then term else *)
    let new_args =
      Term.arg_map_left (fun (*i*) term' ->
        apply_bsubst_bterm_norm_subst''_replacement
          ~replacement
          term_db_ref 
          renaming_env
          bsubst bound norm_subst_ref (b_t, term')
      ) args
    in
    if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
      term
    else
      add_fun_term_args term_db_ref sym new_args
      (* Term.create_fun_term_args sym new_args *)

  | Term.Var(v, _) ->
    let b_v = (b_t, v) in
    let normalize bound_var =
      try
        let new_bterm = SubstM.find bound_var bsubst in
        apply_bsubst_bterm_norm_subst''_replacement
          ~replacement 
          term_db_ref 
          renaming_env
          bsubst bound norm_subst_ref new_bterm
      with Not_found ->
        add_var_term term_db_ref (find_renaming renaming_env bound_var)            
        (* Term.create_var_term (find_renaming renaming_env bound_var) *)
    in
    if b_t = bound then
      let norm_subst', value = 
        !norm_subst_ref |> Subst.find_add v (fun () -> 
          normalize b_v
          (* TermDB.add_ref (normalize b_v) term_db_ref *)
        )
      in
      norm_subst_ref := norm_subst';
      value
    else
      normalize b_v

(* with adding to term_db_ref *)
let apply_bsubst_bterm_norm_subst'
    ?replacement term_db_ref renaming_env bsubst bound norm_subst_ref bterm =
  (* TermDB.add_ref *)
    apply_bsubst_bterm_norm_subst'' term_db_ref
       renaming_env bsubst bound norm_subst_ref bterm
  
let apply_bsubst_bterm_norm_subst'_replacement
    ~replacement term_db_ref renaming_env bsubst bound norm_subst_ref bterm =
  (* TermDB.add_ref *)
    apply_bsubst_bterm_norm_subst''_replacement ~replacement term_db_ref
       renaming_env bsubst bound norm_subst_ref bterm
  
let apply_bsubst_btlist_norm_subst'
    term_db_ref renaming_env
    bsubst bound norm_subst_ref bterm_list =
  List.map
    (apply_bsubst_bterm_norm_subst'
      term_db_ref renaming_env
      bsubst bound norm_subst_ref)
    bterm_list

let apply_bsubst_btlist_norm_subst'_replacement
    ~replacement term_db_ref renaming_env
    bsubst bound norm_subst_ref bterm_list =
  List.map
    (apply_bsubst_bterm_norm_subst'
      ~replacement 
      term_db_ref renaming_env
      bsubst bound norm_subst_ref)
    bterm_list

let apply_bsubst_btlist_norm_subst
    ?replacement term_db_ref bsubst bound bterm_list =
  let renaming_env = init_renaming_env () in
  let norm_subst_ref = ref (Subst.create ()) in

  let new_term_list =
    match replacement with
    | Some rep ->
      (* TODO: The `replacement` argument is unused! *)
      assert false
      (* apply_bsubst_btlist_norm_subst'_replacement
        ~replacement:rep 
        term_db_ref renaming_env
        bsubst bound norm_subst_ref bterm_list *)
    | None ->
      apply_bsubst_btlist_norm_subst'
        term_db_ref	renaming_env
        bsubst bound norm_subst_ref bterm_list
  in 
  (new_term_list, !norm_subst_ref)





(******************************)
(*to string *)

let to_stream s bound_subst =
  let item_to_str (b_v, v) (b_t, t) =
    s.stream_add_str " (";
    Var.to_stream s v;
    s.stream_add_char '_';
    s.stream_add_str (string_of_int b_v);
    s.stream_add_char ',';
    Term.to_stream s t;
    s.stream_add_char '_';
    s.stream_add_str (string_of_int b_t);
    s.stream_add_str "); "
  in
  SubstM.iter item_to_str bound_subst

let out = to_stream stdout_stream

let to_string =
  to_string_fun_from_to_stream_fun 30 to_stream

(* let to_string bound_subst = let item_to_str (b_v,v) (b_t,t) rest=       *)
(* rest^" ("^(Var.to_string v)^"_"^(string_of_int b_v)^","^                *)
(* (Term.to_string t)^"_"^(string_of_int b_t)^"); " in fold item_to_str    *)
(* bound_subst ""                                                          *)
