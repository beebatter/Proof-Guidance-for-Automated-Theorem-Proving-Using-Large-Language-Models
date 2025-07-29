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

(*  subst.ml *)
type term = Term.term
type var = Var.var
module SMap = Symbol.Map
module VMap = Var.VMap
module VSet = Var.VSet

type termDBref = TermDB.termDB ref

(*
    let bot_term = Parser_types.bot_term
*)


(*-------- flat_subst are used in model_inst and some constraints ------------*) 

type flat_subst = (var * term) list

let flat_subst_to_string fs =
  "["^(list_to_string (fun (v,t) -> "("^(Var.to_string v)^","^(Term.to_string t)^")") fs ";")^"]"	
  (* List.X.to_string (Pair.to_string Var.to_string Var.to_string) *)

let compare_vt (v1, t1) (v2, t2) =
  let v_cp = Var.compare v1 v2 in
  if v_cp = 0
  then
    Term.compare t1 t2
  else
    v_cp

(*[] is the smallest element, which is used to output [] first in maps and sets *)
let rec compare_flat_subst s1 s2 =
  match (s1, s2) with
  | (h1:: tl1, h2:: tl2) ->
      let h_cp = compare_vt h1 h2 in
      if h_cp = 0
      then
        compare_flat_subst tl1 tl2
      else
        h_cp
  | (h1:: tl,[]) -> 1
  | ([], h1:: tl) -> -1
  | ([],[]) -> 0

module FSKey = 
  struct 
    type t = flat_subst
    let compare = compare_flat_subst
  end 

module FSMap = Map.Make(FSKey)
module FSSet = Set.Make(FSKey)

(*--------------------------------*)												 
(*type assignment = var*term*)
exception Subst_var_already_def

module SubstKey =
  struct
    type t = var
    let compare = Var.compare
  end

module SubstM = Map.Make(SubstKey)
type subst = term SubstM.t
let create() = SubstM.empty

let is_empty = SubstM.is_empty

let mem = SubstM.mem

let add v t subst =
  (* Use update (1 traversal) rather than mem+add (2 traversals) *)
  subst |> SubstM.update v (function
    (* If exists, raise *)
    | Some _ -> raise_notrace Subst_var_already_def
    (* Else add *)
    | None -> Some t
  )

(** Use [add_keep_old v t subst] rather than [if not (mem v subst) then add v t subst] *)
let add_keep_old v t subst =
  (* Use update (1 traversal) rather than mem+add (2 traversals) *)
  subst |> SubstM.update v (fun x ->
    match x with
    | Some _ -> x
    | None -> Some t
  )

let update = SubstM.update

let find_add k f subst = 
  let value = ref (Obj.magic 0) in
  let subst' = 
    subst |> SubstM.update k (fun x ->
      match x with
      | Some v -> 
        value := v;
        x
      | None -> 
        let v' =  f() in
        value := v';
        Some v'
    )
  in
  subst', !value

let find = SubstM.find
let find_opt = SubstM.find_opt
let remove = SubstM.remove
let map = SubstM.map
let fold = SubstM.fold
let iter = SubstM.iter

let subst_to_flat_subst subst =
  let f v t rest = (v,t)::rest in
  let subst_list = fold f subst [] in 
  List.sort 
    (fun (v1, _) (v2, _)-> Var.compare v1 v2)
    subst_list

(* normalise var term by subst
   we assume that there is no var cycles in subst *)

let rec find_normalised subst t =
  let x = Term.get_var t in
  try
    let x_val = SubstM.find x subst in
    if Term.is_var x_val then
      begin try 
        find_normalised subst x_val 
      with Not_found -> 
        x_val
      end
    else 
      x_val
	(* match x_val with
	   | Term.Var(v, _) ->
	   (try find_normalised subst v with
	   Not_found -> x_val
	   )
	   | _ -> x_val
	 *)
  with Not_found -> t



(* normalise any term by subst
   we assume that there is no var cycles in subst *)

(* term is not added to the term_db *)
let rec normalise_term' subst t =
  if Term.is_ground t then 
    t
  else
    match t with
    | Term.Fun (symb, args, _) ->
      let args' = Term.arg_map (normalise_term' subst) args in
      Term.create_fun_term_args symb args'
    | Term.Var (v, _) ->
      begin try
        let subst_term = SubstM.find v subst in 
        normalise_term' subst subst_term
      with Not_found -> 
        t
      end

let normalise_term subst t = 
  if is_empty subst then t else normalise_term' subst t



(* returns term in which all varibles in_term are replaced by by_term *)
(* and adds this term to termDB *)
(* we assume that  by_term and in_term are in term_db*)
(* non typed version *)
(*
  let rec replace_vars term_db_ref by_term in_term =
  if (Term.is_ground in_term) then in_term
  else
  match in_term with
  | Term.Fun(sym, args, _) ->
  let new_args =
  Term.arg_map_left
  (fun in_term' ->
  (replace_vars term_db_ref by_term in_term')
  ) args in
  let new_term = Term.create_fun_term_args sym new_args in
  TermDB.add_ref new_term term_db_ref
  | _ -> by_term
 *)

(* by_term_map: maps vtypes -> terms ) *)
(* if there is no term in by_term_map then and default_term_opt = Some(default_term) then use default_term *)
(* if default term is None then do not instantiate this var *)
(* let replace_vars default_term_opt by_term_map in_term =
   if (Term.is_ground in_term)
   then in_term
   else
   begin
   let f v =
   let vtype = Var.get_type v in
   (try
   SMap.find vtype by_term_map
   with Not_found ->
   (match default_term_opt with
   | Some default_term -> default_term
   | None -> t
   )
(* raise Type_of_var_is_not_in_map *)
   )

   in
   Term.map f in_term
   end
 *)

let rec replace_vars default_term_opt by_term_map in_term =
  if Term.is_ground in_term then 
    in_term
  else
    match in_term with
    | Term.Fun (symb, args, _) ->
      Term.create_fun_term_args symb (Term.arg_map (replace_vars default_term_opt by_term_map) args)
    | Term.Var (v, _) ->
      let vtype = Var.get_type v in
      begin try
        SMap.find vtype by_term_map
      with Not_found ->
        begin match default_term_opt with
        | Some default_term -> default_term
        | None -> in_term
        end
      end
        (* raise Type_of_var_is_not_in_map *)

let grounding term_db_ref by_term_map in_term =
  let bot_term = add_fun_term term_db_ref Symbol.symb_bot [] in
  let grounded =
    TermDB.add_ref (replace_vars (Some(bot_term)) by_term_map in_term) term_db_ref in
(*  Term.assign_grounding grounded in_term; *)
  grounded

(* applies substituion and adds obtained term into term_db *)
(* nothing is renamed, see substBound for this  *)
(* we assume that all terms in subst and t are in term_db *)
let rec apply_subst_term' term_db_ref subst t =
  if Term.is_ground t then
    t
  else
    match t with
    | Term.Fun (sym, args, _) ->
      let new_args =
        Term.arg_map_left (fun t' ->
          apply_subst_term' term_db_ref subst t'
        ) args 
      in
      if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
        t
      else
        let new_term = Term.create_fun_term_args sym new_args in
        TermDB.add_ref new_term term_db_ref
    | Term.Var (v, _) ->
      begin try
        SubstM.find v subst
      with Not_found -> 
        t
      end

let rec apply_subst_term term_db_ref subst t =
  if is_empty subst then t else apply_subst_term' term_db_ref subst t

let var_renaming_to_subst term_db_ref var_ren = 
  let f v u subst = 
    let u_term = TermDB.add_ref (Term.create_var_term u) term_db_ref in 
    add v u_term subst 
  in
  VMap.fold f var_ren SubstM.empty

(*---------------*)

let rename_term_init away_var_list = 
  let fresh_vars_env = Var.init_fresh_vars_env_away away_var_list in
  let rename_subst = create() in
  (fresh_vars_env,rename_subst) 
 
(* get substitution that can be used for renaming the term *)
(*  fresh_vars_env is updated inplace *)
let rec update_renaming_subst term_db_ref fresh_vars_env rename_subst t = 
  match t with
  | Term.Fun (_, args, _) ->
    Term.arg_fold_left (update_renaming_subst term_db_ref fresh_vars_env) rename_subst args
  | Term.Var (v, _) ->
    rename_subst |> update v (fun x -> 
      match x with
      | Some _ -> x
      | None -> 
        let fresh_var = Var.get_next_fresh_var fresh_vars_env (Var.get_type v) in
        let fresh_var_term = TermDB.add_ref (Term.create_var_term fresh_var) term_db_ref in 
        Some fresh_var_term
    ) 


let rename_term_env term_db_ref fresh_vars_env rename_subst t = 
  let new_rename_subst = update_renaming_subst term_db_ref fresh_vars_env rename_subst t in
  let new_term = apply_subst_term term_db_ref new_rename_subst t in 
  (new_rename_subst, new_term)

let rename_term_list_env term_db_ref fresh_vars_env rename_subst term_list = 
  let f (current_sub, current_new_t_list) t =
    let new_sub, new_t = rename_term_env term_db_ref fresh_vars_env current_sub t in 
    (new_sub, new_t::current_new_t_list)
  in
  List.fold_left f (rename_subst,[]) term_list

(* returns (rename_subst, renamed_term_list) *)

let rename_term_list term_db_ref away_var_list term_list = 
  let (fresh_vars_env,rename_subst)  = rename_term_init away_var_list in
  rename_term_list_env term_db_ref fresh_vars_env rename_subst term_list



let is_renaming subst = 
  (* Injective substitution into vars *)
  let seen = ref VSet.empty in
  subst |> SubstM.for_all (fun v t ->
    match t with
    | Term.Fun _ -> false
    | Term.Var (u, _) -> 
      let seen' = !seen |> VSet.add u in
      if seen' == !seen then false else (seen := seen'; true)
  )



(*---------------------*)
let to_stream s subst =
  let item_to_str v t =
    Term.to_stream s t;
    s.stream_add_char '/';
    Var.to_stream s v;
    s.stream_add_str " ; "
  in
  s.stream_add_char '[';
  iter item_to_str subst;
  s.stream_add_str "]"

let out = to_stream stdout_stream

let to_string =
  to_string_fun_from_to_stream_fun 30 to_stream

(*
  let to_string subst =
  let item_to_str v t rest =
  rest^(Term.to_string t)^"/"^(Var.to_string v)^"; " in
  "["^fold item_to_str subst ""^"]\n"
 *)
