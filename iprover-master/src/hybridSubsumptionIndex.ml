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
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = true

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
  D_trace;
]

let module_name = "hybridSubsumptionIndex"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module UI = UnitSubsumptionIndex
module NI = SubsumptionIndex.SCFVIndex

type t = {
  i_unit: UI.t;
  mutable i_nonunit: NI.index;
}

let create () = {
  i_unit = UI.create();
  i_nonunit = NI.create();
}

let clear x = 
  UI.clear x.i_unit;
  x.i_nonunit <- NI.create()

let add_clause index clause = 
  (* Nonunit subsumption index only gets non-unit clauses *)
  if Clause.is_unit clause then (
    UI.add_clause index.i_unit clause;
  ) else (
    UI.add_clause index.i_unit clause;
    NI.add_clause index.i_nonunit clause;
  )

let remove_clause index clause = 
  if Clause.is_unit clause then (
    UI.remove_clause index.i_unit clause;
  ) else (
    UI.remove_clause index.i_unit clause;
    (* (try NI.remove_clause index.i_nonunit clause with Not_found -> ()); *)
    NI.remove_clause index.i_nonunit clause;
  )

exception Subsumed
let is_subsumed ~subs_bck_mult index clause = 
  dbg D_trace @@ lazy (sprintf "is_subsumed: %s" (Clause.to_string_tptp clause));
  try
    (* If unit clause, this is all we need to do  *)
    if Clause.is_unit clause then
      let result_unit = UI.is_subsumed index.i_unit clause in
      match result_unit with
      | UI.Subs -> 
        dbg D_trace @@ lazy "subsumed by unit";
        raise_notrace Subsumed
      | UI.Nothing -> 
        Some clause
      | UI.SubsRes (parents, lits) -> 
        let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause parents in
        let new_clause = create_clause ~normalise_eqs:true tstp_source lits in
        dbg D_trace @@ lazy (sprintf "subsres by unit %s" (Clause.to_string_tptp clause));
        dbg D_trace @@ lazy (sprintf "             to %s" (Clause.to_string_tptp new_clause));
        dbg D_trace @@ lazy (sprintf "             by %s" (List.X.to_string ~first:"" ~last:"" Clause.to_string_tptp parents));
        dassert (fun () -> Clause.Bc.(new_clause != clause));
        Some new_clause
    (* If nonunit, then we need to check both, which is a bit more involved 
       to do properly: we must check by nonunit first, otherwise we can miss 
       some inferences! *)
    else 

    (* Check if subsumed/subsres by units *)
    let result_unit = UI.is_subsumed index.i_unit clause in
    match result_unit with
    | UI.Subs -> 
      dbg D_trace @@ lazy "subsumed by unit";
      raise_notrace Subsumed
    | _ -> 

    (* Now check if subsumed by nonunit *)
    let result_nonunit_subs = 
      NI.is_subsumed_strict ~subs_bck_mult index.i_nonunit clause
    in
    match result_nonunit_subs with
    | Some _ -> 
      dbg D_trace @@ lazy "subsumed by nonunit";
      raise_notrace Subsumed
    | None -> 

    (* Finally if subsumption res by nonunit *)
    let result_nonunit_subsres = NI.fw_subsres ~subs_bck_mult index.i_nonunit clause in
    let lits_nu, parents_nu = result_nonunit_subsres in

    (* Then, reassemble result from unit AND nonunit subs-res, if both applied *)
    let clause' = 
      if List.X.is_empty parents_nu then (
        match result_unit with

        (* NO unit and NO nonunit *)
        | UI.Nothing -> 
          clause

        (* SOME unit and NO nonunit *)
        | UI.SubsRes (parents_u, lits_u) -> 
          let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause parents_u in
          let new_clause = create_clause ~normalise_eqs:true tstp_source lits_u in
          dbg D_trace @@ lazy (sprintf "subsres by unit %s" (Clause.to_string_tptp clause));
          dbg D_trace @@ lazy (sprintf "             to %s" (Clause.to_string_tptp new_clause));
          dbg D_trace @@ lazy (sprintf "             by %s" (List.X.to_string ~first:"" ~last:"" Clause.to_string_tptp parents_u));
          dassert (fun () -> Clause.Bc.(new_clause != clause));
          Statistics.(incr_int_stat (List.length parents_u) sim_fw_subsumption_res);
          new_clause

        | UI.Subs -> assert false
      )
      else (
        match result_unit with

        (* NO unit and SOME nonunit *)
        | UI.Nothing -> 
          let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause parents_nu in
          let new_clause = create_clause ~normalise_eqs:true tstp_source lits_nu in
          dbg D_trace @@ lazy (sprintf "subsres by nonunit %s" (Clause.to_string_tptp clause));
          dbg D_trace @@ lazy (sprintf "                to %s" (Clause.to_string_tptp new_clause));
          dbg D_trace @@ lazy (sprintf "                by %s" (List.X.to_string ~first:"" ~last:"" Clause.to_string_tptp parents_nu));
          dassert (fun () -> Clause.Bc.(new_clause != clause));
          Statistics.(incr_int_stat (List.length parents_nu) sim_fw_subsumption_res);
          new_clause

        (* SOME unit and SOME nonunit *)
        | UI.SubsRes (parents_u, lits_u) -> 
          dbg D_trace @@ lazy (sprintf "subsres by nonunit %s" (Clause.to_string_tptp clause));
          dbg D_trace @@ lazy (sprintf "                to %s" (List.X.to_string ~first:"( " ~last: " )" ~sep:" | " Term.to_string lits_nu));
          dbg D_trace @@ lazy (sprintf "                by %s" (List.X.to_string ~first:"" ~last:"" Clause.to_string_tptp parents_nu));
          dbg D_trace @@ lazy (sprintf "subsres by unit %s" (Clause.to_string_tptp clause));
          dbg D_trace @@ lazy (sprintf "             to %s" (List.X.to_string ~first:"( " ~last: " )" ~sep:" | " Term.to_string lits_u));
          dbg D_trace @@ lazy (sprintf "             by %s" (List.X.to_string ~first:"" ~last:"" Clause.to_string_tptp parents_u));

          let lits = List.filter (fun x -> List.memq x lits_u) lits_nu in
          let parents = parents_nu @ parents_u in
          let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause parents in
          let new_clause = create_clause ~normalise_eqs:true tstp_source lits in
          dassert (fun () -> Clause.Bc.(new_clause != clause));
          Statistics.(incr_int_stat (List.length parents) sim_fw_subsumption_res);
          new_clause

        | UI.Subs -> assert false
      )
    in

    Some clause'

  with Subsumed -> 
    Statistics.(bump_int_stat sim_fw_subsumed);
    None

let subsumes ~subs_bck_mult index clause = 
  dbg D_trace @@ lazy (sprintf "subsumes: %s" (Clause.to_string_tptp clause));
  match Clause.get_lits clause with
  | [lit] ->
    let subs, subs_res = UI.subsumes index.i_unit lit in
    (* let subs = subs |> List.X.remove_all ~eq:(==) clause in
    Statistics.(bump_int_stat sim_bw_subsumed); *)
    let subs = subs |> List.filter (fun c -> 
      dbg D_trace @@ lazy (sprintf "foo %s" (Clause.to_string_tptp c));
      if not (Clause.is_unit c) then NI.remove_clause index.i_nonunit c;
      Statistics.(bump_int_stat sim_bw_subsumed);
      dassert (fun () -> c != clause); true
    ) in
    let subs_res = subs_res |> List.map (fun (from, into_lits) ->
      if not (Clause.is_unit from) then NI.remove_clause index.i_nonunit from;
      let tstp_source = Clause.tstp_source_backward_subsumption_resolution from [clause] in
      let into = create_clause ~normalise_eqs:true tstp_source into_lits in
      Statistics.(bump_int_stat sim_bw_subsumption_res);
      (from, into)
    ) in
    subs, subs_res

  | _ ->
    let was_in_index = NI.in_subs_index index.i_nonunit clause in
    let subs = 
      NI.find_subsumed ~subs_bck_mult index.i_nonunit clause
      (* |> List.X.removeq_all clause *)
      |> List.filter (fun c ->
        if c != clause then (
          UI.remove_clause index.i_unit c;
          Statistics.(bump_int_stat sim_bw_subsumed);
          true
        ) else false
      )
    in
    if was_in_index then NI.add_clause index.i_nonunit clause;
    (* Statistics.(incr_int_stat (List.length subs) sim_bw_subsumed); *)

    let remove, add = NI.bw_subsres ~subs_bck_mult index.i_nonunit clause in
    let subs_res = 
      List.map2 (fun from into_lits -> 
        (* ALREADY REMOVED FROM SUBS! *)
        dassert (fun () -> SubsumptionIndex.SCFVIndex.in_subs_index index.i_nonunit from == false);
        UI.remove_clause index.i_unit from;
        let tstp_source = Clause.tstp_source_backward_subsumption_resolution from [clause] in
        let into = create_clause ~normalise_eqs:true tstp_source into_lits in
        Statistics.(bump_int_stat sim_bw_subsumption_res);
        (from, into)
      ) remove add
    in
    dassert (fun () -> List.for_all (fun (from, _) -> not (List.memq from subs)) subs_res);

    subs, subs_res
