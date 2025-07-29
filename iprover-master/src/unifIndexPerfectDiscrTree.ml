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
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = true

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



(* type 'a index_elem = 'a UnifIndexMap.map_term_to_list *)



type 'a t = {
  mutable index : (term * 'a list) PerfectDiscrTree.t;
  mutable terms : TSet.t
}

let create() = {
  index = PerfectDiscrTree.create ();
  terms = TSet.empty
}

let clear ui =
  ui.index <- PerfectDiscrTree.create ();
  ui.terms <- TSet.empty

let add_to_lit ui lit elem =  
  Statistics.(time_start unif_index_add_time);

  ui.index <- PerfectDiscrTree.update ui.index lit (function
    | Some old ->
      let old_lit, old_elems = old in
      assert (old_lit == lit);  
      (* So it's a bit redundant to store this here: this is always the key that we index with *)
      (* TODO: just make perfectDiscrTree return this *)
      Some (lit, elem :: old_elems);
    | None ->
      Some (lit, [elem])
  );
  ui.terms <- TSet.add lit ui.terms;

  Statistics.(time_end unif_index_add_time);
  ()

let filter_from_lit ui lit func =
  ui.index <- PerfectDiscrTree.update ui.index lit (function
    (* | Some old ->
      let old_lit, old_elems = old in
      assert (old_lit == lit);
      let new_elems = List.filter (fun el -> not (func el)) old_elems in
      if List.X.is_empty new_elems then ui.terms <- TSet.remove lit ui.terms;
      Some (lit, new_elems)
    | None -> 
      failwith "elim_clause_with_sel: unif index should not contain Empty_Elem" *)

    | Some old ->
      let old_lit, old_elems = old in
      assert (old_lit == lit);
      let new_elems = List.filter (fun el -> not (func el)) old_elems in
      if List.X.is_empty new_elems then (
        ui.terms <- TSet.remove lit ui.terms;
        None
      ) else (
        Some (lit, new_elems)
      )
    | None -> None
  )



let get_gen (ui: 'a t) sel_lit =
  Statistics.(time_start unif_index_cands_time);

  (* get all *)
  let result : ((term * 'a list) * Subst.subst) list = PerfectDiscrTree.generalisations ui.index sel_lit in

  (* dbg_env D_cnds (fun () ->
    let f (t,list) = (Term.to_string t)^":"^(string_of_int (List.length list)) in  (* here 'a list *)
    dbg D_cnds (lazy ("l:"^(Term.to_string sel_lit)^" : "^(list_to_string f (sorted_cnds) " ")));
  ); *)

  Statistics.(time_end unif_index_cands_time);
  result



let get_inst (ui: 'a t) sel_lit =
  Statistics.(time_start unif_index_cands_time);

  (* get all *)
  let result : ((term * 'a list) * Subst.subst) list = PerfectDiscrTree.instantiations ui.index sel_lit in

  (* dbg_env D_cnds (fun () ->
    let f (t,list) = (Term.to_string t)^":"^(string_of_int (List.length list)) in  (* here 'a list *)
    dbg D_cnds (lazy ("l:"^(Term.to_string sel_lit)^" : "^(list_to_string f (sorted_cnds) " ")));
  ); *)

  Statistics.(time_end unif_index_cands_time);
  result



let iter_gen (ui: 'a t) sel_lit f = 
  PerfectDiscrTree.generalisations_iter ui.index sel_lit (fun (term, l) subst ->
    f term l subst
  )

let iter_inst (ui: 'a t) sel_lit f = 
  PerfectDiscrTree.instantiations_iter ui.index sel_lit (fun (term, l) subst ->
    f term l subst
  )



(* check whether given literal is in the unif index *)
let mem ui lit = TSet.mem lit ui.terms
