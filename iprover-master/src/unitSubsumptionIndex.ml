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

let dbg_flag = false

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
  D_trace;
]

let module_name = "unitSubsumptionIndex"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



(*  Subsumption: 
    Kept    | dir | New     | 
    --------------------
    unit    | ==> | unit    | is_subsumed 
    unit    | <== | unit    | 
    nonunit |     | unit    | 
    unit    |     | nonunit | 
    nonunit |     | nonunit | 
*)

type t = {
  unit_tree: (clause * bool) PerfectDiscrTree.t;
  nonunit_tree: (clause * bool) PerfectDiscrTree.t;
}

let create () = {
  unit_tree = PerfectDiscrTree.create();
  nonunit_tree = PerfectDiscrTree.create();
}

let clear x = 
  PerfectDiscrTree.clear x.unit_tree;
  PerfectDiscrTree.clear x.nonunit_tree

let add_clause index clause = 
  let add_clause_by_lit tree lit clause =
    let sign, atom = Term.split_sign_lit lit in
    PerfectDiscrTree.add tree atom (clause, sign)
  in
  let lits = Clause.get_lits clause in
  match lits with
  | [lit] -> 
    add_clause_by_lit index.unit_tree lit clause
  | _ -> 
    lits |> List.iter (fun lit ->
      add_clause_by_lit index.nonunit_tree lit clause
    )

(* Caution!! is silent if trying to remove something that doesn't exist *)
let remove_clause index clause = 
  let remove_clause_by_lit tree lit clause =
    let sign, atom = Term.split_sign_lit lit in
    PerfectDiscrTree.filter tree atom (fun (c, s) -> Clause.Bc.(c == clause) (* && s == sign *))
  in
  let lits = Clause.get_lits clause in
  match lits with
  | [lit] -> 
    remove_clause_by_lit index.unit_tree lit clause
  | _ -> 
    lits |> List.iter (fun lit ->
      remove_clause_by_lit index.nonunit_tree lit clause
    )

type subsumption_result = 
  | Nothing
  | Subs of clause 
  | SubsRes of clause list * lits

exception Return of clause
let is_subsumed index clause = 
  dbg D_trace @@ lazy (sprintf "is_subsumed: %s" (Clause.to_string_tptp clause));
  try
    let lits = Clause.get_lits clause in
    let parents = ref [] in
    let lits' = 
      lits |> List.filter (fun lit -> 
        let sign, atom = Term.split_sign_lit lit in
        let keep = ref true in
        PerfectDiscrTree.generalisations_iter index.unit_tree atom (fun atom' subst lst ->
          lst |> List.iter (fun (clause', sign') ->
            (* Only strict subsumption *)
            if clause' != clause then (
              dbg D_trace @@ lazy (sprintf "candidate: %s" (Clause.to_string_tptp clause'));
              (* If same sign: subsuming clause. Also must be strict. *)
              if Bool.O.(sign = sign') then (
                dbg D_trace @@ lazy "subsumption";
                raise_notrace @@ Return clause'
              )
              (* If different signs: subsumption resolution *)
              else (
                dbg D_trace @@ lazy "subsumption_res";
                if !keep then (
                  keep := false;
                  parents := clause' :: !parents;
                )
              )
            )
          )
        );
        !keep
      )
    in
    match !parents with
    | [] -> Nothing
    | _ -> SubsRes (!parents, lits')
  with Return c -> Subs c

exception Empty of clause
let subsumes index unit_lit = 
  dbg D_trace @@ lazy (sprintf "subsumes: ( %s )" (Term.to_string unit_lit));
  (* nas duas trees, procurar lits que sejam instances e botar nestas duas listas *)
  (* let subs : clause list ref = ref [] in
  let subs_res : (clause * lit list) list ref = ref [] in
  let subs_res_units : clause list ref = ref [] in *)
  let subs = ref BCSet.empty in
  let subs_res : TSet.t BCMap.t ref = ref BCMap.empty in
  let sign, atom = Term.split_sign_lit unit_lit in

  PerfectDiscrTree.instantiations_iter index.unit_tree atom (fun atom' subst lst ->
    lst |> List.iter (fun (clause', sign') -> 
      dbg D_trace @@ lazy (sprintf "candidate: %s" (Clause.to_string_tptp clause'));
      match Clause.get_lits clause' with 
      | [lit'] when lit' == unit_lit ->
        dbg D_trace @@ lazy "skipped clause itself"; ()
      | _ -> 
      (* If same sign: subsuming clause *)
      if Bool.O.(sign = sign') then (
        dbg D_trace @@ lazy "subsumption";
        subs := !subs |> BCSet.add clause'
      )
      (* If different signs: subsumption resolution *)
      else (
        dbg D_trace @@ lazy "subsumption_res";
        (* raise_notrace Empty_clause *)
        (* List.X.cons_ref (clause', []) subs_res *)
        subs_res := !subs_res |> BCMap.add clause' TSet.empty
      )
    )
  );

  PerfectDiscrTree.instantiations_iter index.nonunit_tree atom (fun atom' subst lst ->
    lst |> List.iter (fun (clause', sign') -> 
      dbg D_trace @@ lazy (sprintf "candidate: %s" (Clause.to_string_tptp clause'));
      (* If same sign: subsuming clause *)
      if Bool.O.(sign = sign') then (
        dbg D_trace @@ lazy "subsumption";
        subs := !subs |> BCSet.add clause'
      )
      (* If different signs: subsumption resolution *)
      (* But only if clause is not already subsumed (has to be done later, since a subsumption may happen after a subs res) *)
      else (
        dbg D_trace @@ lazy "subsumption_res";
        (* What if multiple literals were to be taken from that same clause? (solved below, may be inefficient) *)
        let new_lits = 
          Clause.get_lits clause' 
          |> List.filter (fun x -> let s,a = Term.split_sign_lit x in not (s == sign' && a == atom'))
          |> TSet.of_list
        in
        subs_res := !subs_res |> BCMap.update clause' (function
          | None -> Some (new_lits)
          | Some old_lits -> Some (TSet.inter old_lits new_lits)
        )
      )
    )
  );
  
  (* let subs = !subs in
  let subs_res = List.map (fun c -> (c, [])) !subs_res_units @ !subs_res in *)

  try
    let subs = !subs in
    let subs_res = !subs_res |> BCMap.filter (fun c l -> 
      if TSet.is_empty l then raise_notrace (Empty c);
      not (BCSet.mem c subs)
    ) in

    let subs = BCSet.elements subs in
    let subs_res = (* BCMap.bindings subs_res *)
      BCMap.fold (fun clause lits_set acc -> 
        remove_clause index clause;
        (clause, TSet.elements lits_set) :: acc
      ) subs_res []
    in

    (* Necessary because feature vector subsumption index also automatically 
       removes bw subsumed clauses in find_subsumed. TODO: change this behaviour 
       in both? *)
    List.iter (remove_clause index) subs;
    (* !! *)
    (* List.iter (fun (c, _) -> remove_clause index c) subs_res; *)  (* Duplicate? *)
    
    subs, subs_res

  with Empty c -> 
    [], [(c, [])]

  (* let subs_res = List.map (fun c -> (c, [])) !subs_res_units @ map_as_list in *)
