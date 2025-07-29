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



(* Unification/matching index of clauses for inst/res/demodulation implementation *)

open Lib
open Options
open Statistics
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_add
  | D_cnds

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_add -> "add" 
  | D_cnds -> "cnds"

let dbg_groups = [
  D_trace;
  D_add;
  D_cnds;
]

let module_name = "unifIndexDiscrTree"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



(* copy some definitions from UI_Map *)
let sort_candidates = UnifIndexMap.sort_unif_candidates

let add_elem_to_term_list_map = UnifIndexMap.add_elem_to_term_list_map

let remove_elem_from_term_list_map = UnifIndexMap.remove_elem_from_term_list_map

type 'a index_elem = 'a UnifIndexMap.map_term_to_list

(* init member for discrimitaion tree *)
module DTParam = struct let num_of_symb = 23 end

(* discrimination tree module *)
(* module DiscrTreeM = DiscrTree.Make(DTParam) *)
module DiscrTreeM = DiscrTree_func

(* generic *)
type 'a t = {
  mutable index : ('a index_elem DiscrTreeM.index) ref;
  mutable terms : TSet.t
}

(* create unification index: i/r *)
let create() =
  {
    index = ref (DiscrTreeM.create ());
    terms = TSet.empty
  }

(* clear unification index: i/r, like create *)
let clear ui =
  ui.index <- ref (DiscrTreeM.create());
  ui.terms <- TSet.empty

(* add elem with given literal: i/r *)
let add_elem_to_lit ui lit elem =  
  Statistics.(time unif_index_add_time) @@ fun () -> 

  let ind_elem = DiscrTreeM.add_term_path lit ui.index in
  ui.terms <- TSet.add lit ui.terms;
  (* debug check that if add not t then t is not in the index*)
  (* ( if (Term.is_neg_lit sel_lit)
    then
    let atom = Term.get_atom sel_lit in
    try
    let ind_elem = DiscrTreeM.find atom !unif_index_ref in
    out_str ("Compl. Lit is in Unif! Lit: "^(Term.to_string sel_lit)
    ^" Compl: "^(Term.to_string atom));
    with Not_found ->
    (
    out_str ("Compl. Lit is NOT in Unif! (ok) Lit: "^(Term.to_string sel_lit)
    ^" Compl: "^(Term.to_string atom));
    )
    else ()
    );*)
  (*end debug*)
  begin match !ind_elem with
  | Elem(old) ->
    ind_elem := Elem(add_elem_to_term_list_map lit elem old)
  | Empty_Elem ->
    ind_elem := Elem(add_elem_to_term_list_map lit elem TMap.empty)
  end


(* remove clause with given selection literal: i/r *)
(* Throws Not_found if sel_lit not found in unif index *)
let elim_filter_from_lit ui lit func =
  try 
    let ind_elem = DiscrTreeM.find lit !(ui.index) in (* uncaught throw: now caught *)
    match !ind_elem with
    | Elem(map) -> 
      (* old = [(L1,[C_1,.., Cn]), (L2,[C'_1,.., C'n']),..]
         old_clause_list = [C_1,.., Cn] corr to sel_lit *)
      (* get the old list corresponding to TERM *)
      let lit_binding = TMap.find lit map in
      (* filter out all instances that match func from the list *)
      let rest = List.filter (fun el -> not (func el)) lit_binding in
      (* check the result *)
      begin match rest with
      | [] ->
        (* last entity removed -- remove binding *)
        ui.terms <- TSet.remove lit ui.terms;
        let new_map = TMap.remove lit map in
        (* if new_map is empty then discard element *)
        if TMap.is_empty new_map
        then
          DiscrTreeM.remove_term_path lit ui.index
        else
          ind_elem := Elem(new_map)
      | _ ->
        (* something left -- save it back *)
        ind_elem := Elem(TMap.add lit rest map)
      end
    | Empty_Elem ->
      failwith "elim_clause_with_sel: unif index should not contain Empty_Elem"
  (* binding doesn't exists -- nothing to do *)
  (* Either DiscrTreeM.find didn't find the lit, or TMap.find didn't fint it in the leaf *)
  with Not_found -> 
    ()

(* remove clause with given selection literal: i/r *)
(* Throws Not_found if sel_lit not found in unif index *)
let elim_elem_from_lit ui lit elem =
  (* [@@deprecated "Uses (==)"] *)
  elim_filter_from_lit ui lit (fun x -> elem == x) 

(* eliminates all clauses indexed by all lits lit' such that lit'\Var = lit\Var 
(i.e. all variables in lit are replaced by a fresh constant Var) *)
(* from index and returns the list of all associated elements *)
(* can raise Not_found *)
let eliminate_lit_var ui lit =
  (* eliminated literals can be different form lit!!! lit\bot = lit_elim\bot *)
  try
    let ind_elem = DiscrTreeM.remove_term_path_ret lit ui.index in
    begin match !ind_elem with
    | Elem(map) ->
      (* elem = [(L1,[C_1,..,Cn]),(L2,[C'_1,..,C'n']),..] *)
      (* elem_clause_list = [C_1,..,Cn] corr to sel_lit*)
      (* for all elements in the map... *)
      let folder term elem elem_list =
        (* remove term from the referenced *)
        ui.terms <- TSet.remove term ui.terms;
        (* add element to a list *)
        elem@elem_list
      in
      TMap.fold folder map []
    | Empty_Elem ->
      failwith "eliminate_lit: unif index should not contain Empty_Elem"
    end
  with 
    DiscrTreeM.Not_in_discr_tree -> raise Not_found

(* eliminates lit from index and returns list of elements that was associated with this lit *)
(* can raise Not_found if lit is not in the index *)
let eliminate_lit ui lit =
  let ind_elem = DiscrTreeM.find lit !(ui.index) in (* uncaught throw *)
  match !ind_elem with
  | Elem(map) -> (
    (* old = [(L1,[C_1,.., Cn]), (L2,[C'_1,.., C'n']),..]
       old_clause_list = [C_1,.., Cn] corr to sel_lit *)
    try
      (* get the old list corresponding to TERM *)
      let lit_binding = TMap.find lit map in
      ui.terms <- TSet.remove lit ui.terms;
      let new_map = TMap.remove lit map in
          (* if new_map is empty then discard element *)
      (if TMap.is_empty new_map
      then
        DiscrTreeM.remove_term_path lit ui.index
      else
        ind_elem := Elem(new_map)
      );
      lit_binding      
    with
      (* binding doesn't exists -- nothing to do *)
      Not_found -> raise Not_found
  )
  | Empty_Elem ->
    failwith "eliminate_lit: unif index should not contain Empty_Elem"



(* get unification candidates for given selection literals, i/r *)
(* need them sorted to keep reasoning deterministic for different sizes *)
let get_unif_candidates ui sel_lit =
 Statistics.(time unif_index_cands_time) @@ fun () -> 

  (* get all candidates *)
  let list_of_maps = DiscrTreeM.unif_candidates !(ui.index) sel_lit in
 
  (* transform term->[cl...] into [(term,[cl])...] *)
  let combine term cl_list accum = (term,cl_list)::accum in
  
  (* transform single map into a list *)
  let unmap accum map = TMap.fold combine map accum in

  (* flat the list *)
  let unsorted = List.fold_left unmap [] list_of_maps in
  
  (* return sorted one *)
  let sorted_cnds = sort_candidates unsorted in 
  dbg_env D_cnds 
    (fun () ->
      let f (t,list) = (Term.to_string t)^":"^(string_of_int (List.length list)) in  (* here 'a list *)
      dbg D_cnds (lazy ("l:"^(Term.to_string sel_lit)^" : "^(list_to_string f (sorted_cnds) " ")));
    );
  sorted_cnds



(* get unification candidates for given selection literals, i/r *)
(* need them sorted to keep reasoning deterministic for different sizes *)
let get_gen_candidates ui sel_lit =
  Statistics.(time unif_index_cands_time) @@ fun () -> 

  (* get all candidates *)
  let list_of_maps = DiscrTreeM.generalisation_candidates !(ui.index) sel_lit in
  (* transform term->[cl...] into [(term,[cl])...] *)
  let combine term cl_list accum = (term,cl_list)::accum in
  (* transform single map into a list *)
  let unmap accum map = TMap.fold combine map accum in
  (* flat the list *)
  let unsorted = List.fold_left unmap [] list_of_maps in
  (* return sorted one *)
  let sorted_cnds = sort_candidates unsorted in 
  dbg_env D_cnds (fun () ->
    let f (t,list) = (Term.to_string t)^":"^(string_of_int (List.length list)) in  (* here 'a list *)
    dbg D_cnds (lazy ("l:"^(Term.to_string sel_lit)^" : "^(list_to_string f (sorted_cnds) " ")));
  );

  sorted_cnds



(* get unification candidates for given selection literals, i/r *)
(* need them sorted to keep reasoning deterministic for different sizes *)
let get_inst_candidates ui sel_lit =
  Statistics.(time unif_index_cands_time) @@ fun () -> 

  (* get all candidates *)
  let list_of_maps = DiscrTreeM.instantiation_candidates !(ui.index) sel_lit in
  (* transform term->[cl...] into [(term,[cl])...] *)
  let combine term cl_list accum = (term,cl_list)::accum in
  (* transform single map into a list *)
  let unmap accum map = TMap.fold combine map accum in
  (* flat the list *)
  let unsorted = List.fold_left unmap [] list_of_maps in
  (* return sorted one *)
  let sorted_cnds = sort_candidates unsorted in 
  dbg_env D_cnds (fun () ->
    let f (t,list) = (Term.to_string t)^":"^(string_of_int (List.length list)) in  (* here 'a list *)
    dbg D_cnds (lazy ("l:"^(Term.to_string sel_lit)^" : "^(list_to_string f (sorted_cnds) " ")));
  );
  sorted_cnds



(* get variants for given selection literals, i/r *)
(* need them sorted to keep reasoning deterministic for different sizes *)
let get_variants ui sel_lit =
  Statistics.(time unif_index_cands_time) @@ fun () -> 

  (* get all candidates *)
  let list_of_maps = DiscrTreeM.variants !(ui.index) sel_lit in
  (* transform term->[cl...] into [(term,[cl])...] *)
  let combine term cl_list accum = (term,cl_list)::accum in
  (* transform single map into a list *)
  let unmap accum map = TMap.fold combine map accum in
  (* flat the list *)
  let unsorted = List.fold_left unmap [] list_of_maps in
  (* return sorted one *)
  let sorted_cnds = sort_candidates unsorted in 
  dbg_env D_cnds (fun () ->
    let f (t,list) = (Term.to_string t)^":"^(string_of_int (List.length list)) in  (* here 'a list *)
    dbg D_cnds (lazy ("l:"^(Term.to_string sel_lit)^" : "^(list_to_string f (sorted_cnds) " ")));
  );

  sorted_cnds




(* check whether given literal is in the unif index *)
let mem ui lit = TSet.mem lit ui.terms

(* debug: print the whole index *)
(*
let print_unif_cand unif_cand =
  let cl_list_to_string clause_list =
    let p_cl str clause = str^","^(Clause.to_string clause) in
    match clause_list with
    | hd::tl -> (List.fold_left p_cl ("["^(Clause.to_string hd)) tl)^"]"
    | [] -> "[]"
  in
  let print_one_cand str (lit, clause_list) = str^(Term.to_string lit)^": "^(cl_list_to_string clause_list)^"; " in
  (List.fold_left print_one_cand "[ " unif_cand)^"]"

let print_map map =
  let print_uc lit clause_list = out_str(print_unif_cand [(lit, clause_list)]) in
  TMap.iter print_uc map

let db ui =
  let print_elem map_ref =
    match !map_ref with
    | Elem(elem) -> print_map elem
    | _ -> ()
  in
  DiscrTreeM.iter_elem print_elem !(ui.index)
*)








let get_unif_candidates_iter ui sel_lit f =
  (* Cannot measure time since it would also measure time spent in `f`. TODO: workaround that. *)
  (* let start_time = Unix.gettimeofday () in     *)

  DiscrTreeM.unif_candidates_iter !(ui.index) sel_lit (fun map ->
    map |> TMap.iter (fun key value -> 
      (* List.iter (fun x -> f key x) value *)
      f key value
    )
  )
 
  (* let end_time = Unix.gettimeofday () in
  add_float_stat (end_time -. start_time) unif_index_cands_time; *)

let get_gen_candidates_iter ui sel_lit f =
  (* Cannot measure time since it would also measure time spent in `f`. TODO: workaround that. *)
  (* let start_time = Unix.gettimeofday () in     *)

  DiscrTreeM.generalisation_candidates_iter !(ui.index) sel_lit (fun map ->
    map |> TMap.iter (fun key value -> 
      (* List.iter (fun x -> f key x) value *)
      f key value
    )
  )
 
  (* let end_time = Unix.gettimeofday () in
  add_float_stat (end_time -. start_time) unif_index_cands_time; *)

let get_inst_candidates_iter ui sel_lit f =
  (* Cannot measure time since it would also measure time spent in `f`. TODO: workaround that. *)
  (* let start_time = Unix.gettimeofday () in     *)

  DiscrTreeM.instantiation_candidates_iter !(ui.index) sel_lit (fun map ->
    map |> TMap.iter (fun key value -> 
      (* List.iter (fun x -> f key x) value *)
      f key value
    )
  )
 
  (* let end_time = Unix.gettimeofday () in
  add_float_stat (end_time -. start_time) unif_index_cands_time; *)
