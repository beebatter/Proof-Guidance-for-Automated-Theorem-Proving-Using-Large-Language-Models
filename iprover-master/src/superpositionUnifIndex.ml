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

(* unification index of clauses for superposition *)

open Lib
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr =
  | D_trace
  | D_not_found
  | D_type

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_not_found -> "not_found"
  | D_type -> "type"

let dbg_groups = [
  D_trace;
  D_not_found;
  D_type;
]

let module_name = "superpositionUnifIndex"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module UnifIndex = UnifIndexDiscrTree 

module TypedUnifIndex = struct
  type 'a t = 'a UnifIndex.t SMap.t ref

  let create () = 
    ref SMap.empty

  let clear x = 
    x := SMap.empty

  let get_index_typ index typ = 
    dbg D_type @@ lazy (sprintf "  [selecting type %s]" (Symbol.to_string(*_full*) typ));
    match SMap.find_opt typ !index with  (* find_or_add *)
    | Some x -> x
    | None -> 
      let x = UnifIndex.create() in
      index := !index |> SMap.add typ x;
      x

  let get_index index t = 
    let typ = Term.get_term_type t in
    get_index_typ index typ
end

type t = {
  term_index: (int * lit * clause) TypedUnifIndex.t;
  subterm_index: (int * lit * clause) TypedUnifIndex.t;
  order: ordering;
  eq_types: SSet.t;
}





(* Helper functions *)
(* let term_type t = 
  match t with
  | Term.Var (x, _) -> 
    Var.get_type x 
  | Term.Fun (symb, _, _) -> 
    Symbol.get_val_type_def symb  *)

(* let term_index ui t =
  let typ = Term.get_term_type t in
  dbg D_trace @@ lazy (sprintf "selecting type %s" (Symbol.to_string(*_full*) typ));
  try 
    SMap.find typ ui.subterm_index
  with Not_found -> 
    let new_index = UnifIndex.create() in
    ui.subterm_index <- SMap.add typ new_index ui.subterm_index;
    new_index *)

let create ~order ~eq_types () =
  dbg D_trace @@ lazy (sprintf "creating superpositionUnifIndex with eq_types = %s" (SSet.elements eq_types |> List.X.to_string Symbol.to_string));
  {
    term_index = TypedUnifIndex.create();
    subterm_index = TypedUnifIndex.create();
    order;
    eq_types;
  }

let clear ui = 
  TypedUnifIndex.clear ui.term_index;
  TypedUnifIndex.clear ui.subterm_index

let add_clause_indexed_by index indexed_by pos lit clause =
  let index = TypedUnifIndex.get_index index indexed_by in
  UnifIndex.add_elem_to_lit index indexed_by (pos, lit, clause)

(* Handle uncaught throw at unifIndexDiscrTree:130 *)
let elim_clause_indexed_by index indexed_by clause =
  let index = TypedUnifIndex.get_index index indexed_by in
  UnifIndex.elim_filter_from_lit index indexed_by (fun (_,_,x) -> Clause.Bc.(x == clause))
  (* with Not_found -> dbg D_not_found @@ lazy (sprintf "No literal indexed by %s" (Term.to_string indexed_by)); *)





let add_clause_with_sel_subterms ui lit clause =
  dbg D_trace @@ lazy (sprintf "Add bw: lit: %s, clause: %s" 
    (Term.to_string lit) (Clause.to_string_tptp clause)
  );

  (* let terms = Array.create 3 (ref TSet.empty) in *)
  let terms_1 = ref TSet.empty in
  let terms_2 = ref TSet.empty in
  let add_clause_indexed_by' ui x pos lit clause =
    if SSet.mem (Term.get_term_type x) ui.eq_types then (
      dbg D_trace @@ lazy (sprintf " %s is eq (%s)" (Term.to_string x) (Symbol.to_string @@ Term.get_term_type x));
      (* TODO VER!!! *)
      let terms = if pos = 1 then terms_1 else terms_2 in
      let terms' = TSet.add x !terms in
      if !terms != terms' then (
        dbg D_trace @@ lazy (sprintf " (subterm): %s at pos=%d" (Term.to_string x) (pos));
        terms := terms';
        add_clause_indexed_by ui.subterm_index x pos lit clause
      ) 
    ) else (
      dbg D_trace @@ lazy (sprintf " %s not eq (%s)" (Term.to_string x) (Symbol.to_string @@ Term.get_term_type x));
    )
  in

  (* New version: no predicate equalities *)

  let sign, atom = Term.split_sign_lit lit in
  match Term.Eq.decompose_atom atom with
  (* If equality literal, index subterms of lhs and rhs *)
  | Some (l,r) -> 
    (* if r == SystemDBs.top_term then ( *)
    (* assert (Term.get_top_symb l != Symbol.symb_top); *)
    let loop ui lit clause s pos =
      s |> Term.iter_preorder_novar (fun x ->  (* TODO: Investigate performance of pre vs post order *)
        add_clause_indexed_by' ui x pos lit clause
      )
    in
    (* If oriented, index only greater side, else do both *)
    begin match ui.order.oriented lit with
    | GT ->
      loop ui lit clause l 1
    | LT ->
      loop ui lit clause r 2
    | INC ->
      loop ui lit clause l 1;
      loop ui lit clause r 2
    | EQ -> assert false
    end

  (* If nonequality literal, index subterms *)
  | None -> 
    (* lit |> Term.iter_preorder_novar (fun x -> add_clause_indexed_by ui x 0 lit clause) *)
    if Bool.O.(sign = true) then (
      dbg D_trace @@ lazy ("  skipping top term in positive predicate");
    ) else (
      dbg D_trace @@ lazy (sprintf " (subterm): %s at pos=%d" (Term.to_string atom) (0));
      add_clause_indexed_by ui.subterm_index atom 0 lit clause;
    );
    (* If problem has no equalities, don't bother indexing subterms at all *)
    if SSet.is_empty ui.eq_types then () else
    atom |> Term.iter_subterms_preorder_novar (fun x ->
      add_clause_indexed_by' ui x 0 lit clause
    )

let add_clause_with_sel_terms ui lit clause =
  dbg D_trace @@ lazy (sprintf "Add fw: lit: %s, clause: %s" 
    (Term.to_string lit) (Clause.to_string_tptp clause)
  );

  let sign, atom = Term.split_sign_lit lit in
  if Bool.O.(sign = false) then dbg D_trace @@ lazy "  skipping negative literal" else
  match Term.Eq.decompose_atom atom with
  (* If equality literal *)
  | Some (l,r) -> 
    begin match ui.order.oriented lit with
    | GT -> 
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string l));
      add_clause_indexed_by ui.term_index l 1 lit clause  (* TODO: Investigate performance of pre vs post order *)
    | LT -> 
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string r));
      add_clause_indexed_by ui.term_index r 2 lit clause
    | INC -> 
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string l));
      add_clause_indexed_by ui.term_index l 1 lit clause;
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string r));
      add_clause_indexed_by ui.term_index r 2 lit clause
    | EQ -> assert false
    end
  (* If nonequality literal *)
  | None -> 
    (* failwith "add_clause_with_sel: non-equality literal" *)
    add_clause_indexed_by ui.term_index atom 0 lit clause

let add_clause_with_sel ui lit clause =
  add_clause_with_sel_subterms ui lit clause;
  (* if Term.is_eq_atom lit then *)
  add_clause_with_sel_terms ui lit clause





let print_unif_cand unif_cand =
  let print_elem (pos,lit,clause) = 
    sprintf "%s [%d] @ %s" (Term.to_string lit) (pos) (Clause.to_string_tptp clause)
  in
  let print_candidate (term,l) = 
    sprintf "%s: %s" (Term.to_string term) (list_to_string print_elem l ~first:"[" ~last:"]" ", ")
  in
  list_to_string print_candidate unif_cand ~first:"[" ~last:"]" "; "

let get_unif_candidates_term index term =
  (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  (* let index = TypedUnifIndex.get_index index term in *)
  let candidates = UnifIndex.get_unif_candidates index term in
  dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print_unif_cand candidates));
  candidates

let get_unif_candidates index lit =
  dbg D_trace @@ lazy (sprintf "Candidates of lit: %s" (Term.to_string lit));

  (* let sign, atom = Term.split_sign_lit lit in
  let list = ref [] in
  begin match Term.decompose_eq_atom atom with
  (* If equality literal, get candidates of subterms of lhs and rhs *)
  | [_;l;r] -> 
    l |> Term.iter (fun x -> let cand = get_unif_candidates_term ui x in list := cand @ !list);
    r |> Term.iter (fun x -> let cand = get_unif_candidates_term ui x in list := cand @ !list)
  (* If nonequality literal, index subterms *)
  | [] -> 
    lit |> Term.iter (fun x -> let cand = get_unif_candidates_term ui x in list := cand @ !list)
  | _ -> failwith "should not happen"
  end;
  !list *)

  get_unif_candidates_term index lit





(* Raises Not_found if lit not found in unif index *)
let elim_clause_with_sel_terms ui lit clause =
  dbg D_trace @@ lazy (sprintf "Del fw: lit: %s, clause: %s" 
    (Term.to_string lit) (Clause.to_string_tptp clause)
  );

  let sign, atom = Term.split_sign_lit lit in
  if Bool.O.(sign = false) then dbg D_trace @@ lazy "  skipping negative literal" else
  match Term.Eq.decompose_atom atom with
  (* If equality literal *)
  | Some (l,r) -> 
    begin match ui.order.oriented lit with
    | GT -> 
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string l));
      elim_clause_indexed_by ui.term_index l clause
    | LT -> 
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string r));
      elim_clause_indexed_by ui.term_index r clause
    | INC -> 
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string l));
      elim_clause_indexed_by ui.term_index l clause;
      dbg D_trace @@ lazy (sprintf " (term) %s" (Term.to_string r));
      elim_clause_indexed_by ui.term_index r clause
    | EQ -> assert false
    end
  (* If nonequality literal *)
  | None -> 
    (* failwith "add_clause_with_sel: non-equality literal" *)
    elim_clause_indexed_by ui.term_index atom clause

let elim_clause_with_sel_subterms ui lit clause =
  dbg D_trace @@ lazy (sprintf "Del bw: lit: %s, clause: %s" 
    (Term.to_string lit) (Clause.to_string_tptp clause)
  );

  let terms = ref TSet.empty in
  let elim_clause_indexed_by' ui x clause =
    if SSet.mem (Term.get_term_type x) ui.eq_types then (
      dbg D_trace @@ lazy (sprintf " %s is eq (%s)" (Term.to_string x) (Symbol.to_string @@ Term.get_term_type x));
      let terms' = TSet.add x !terms in
      if !terms != terms' then (
        dbg D_trace @@ lazy (sprintf " (subterm): %s at pos=?" (Term.to_string x));
        terms := terms';
        elim_clause_indexed_by ui.subterm_index x clause
      )
    ) else (
      dbg D_trace @@ lazy (sprintf " %s not eq (%s)" (Term.to_string x) (Symbol.to_string @@ Term.get_term_type x));
    )
  in

  let sign, atom = Term.split_sign_lit lit in
  match Term.Eq.decompose_atom atom with
  (* If equality literal, index subterms of lhs and rhs *)
  | Some (l,r) -> 
    (* If a predicate, do not index top term for superposition *)
    (* if Term.Eq.is_pos_predicate_eq lit then ( *)
    (* Else, index top term and subterms *)
    let loop ui lit clause s pos =
      s |> Term.iter_preorder_novar (fun x ->  (* TODO: Investigate performance of pre vs post order *)
        elim_clause_indexed_by' ui x clause
      )
    in
    (* If oriented, index only greater side, else do both *)
    begin match ui.order.oriented lit with
    | GT ->
      loop ui lit clause l 1
    | LT ->
      loop ui lit clause r 2
    | INC ->
      loop ui lit clause l 1;
      loop ui lit clause r 2
    | EQ -> assert false
    end

  (* If nonequality literal, index subterms *)
  | None -> 
    (* failwith "add_clause_with_sel_subterms: non-equality literal" *)
    if Bool.O.(sign = true) then (
      dbg D_trace @@ lazy ("  skipping top term in positive predicate");
    ) else (
      dbg D_trace @@ lazy (sprintf " (subterm): %s at pos=0" (Term.to_string atom));
      elim_clause_indexed_by ui.subterm_index atom clause;
    );
    if SSet.is_empty ui.eq_types then () else
    atom |> Term.iter_subterms_preorder_novar (fun x ->
      elim_clause_indexed_by' ui x clause
    )

let elim_clause_with_sel ui lit clause =
  elim_clause_with_sel_subterms ui lit clause;
  (* if Term.is_eq_atom lit then *)
  elim_clause_with_sel_terms ui lit clause






(* (* eliminates all clauses indexed by lit from unif_index and returns *)
(* the eliminated clause list: i *)

let eliminate_lit ui lit =
  let removed_clauses = UnifIndex.eliminate_lit ui.unif_index lit in
  dbg D_trace (lazy ("Elim: lit: "^(Term.to_string lit)^", clauses: "^(Clause.clause_list_to_string removed_clauses)));
  (* return the list *)
  removed_clauses

 *)



let backward_superposition_candidates ui l =
  match l with
  (* If term, find in type index *)
  | Term.Fun _ ->
    let index = TypedUnifIndex.get_index ui.subterm_index l in
    get_unif_candidates_term index l
  (* If var, find in all indices whose type is a *sub*type of *)
  | Term.Var _ ->
    let typ = Term.get_term_type l in
    SMap.fold (fun t index acc -> 
      if Symbol.is_subtype t typ then
        List.rev_append (get_unif_candidates_term index l) acc
      else
        acc
    ) !(ui.subterm_index) []

let forward_superposition_candidates ui s =
  (* get_unif_candidates_term ui.term_index s *)
  match s with
  | Term.Fun _ -> 
    let index = TypedUnifIndex.get_index ui.term_index s in
    get_unif_candidates_term index s
  (* If var, find in all indices whose type is a *super*type of *)
  | Term.Var _ -> 
    let typ = Term.get_term_type s in
    SMap.fold (fun t index acc -> 
      if Symbol.is_subtype typ t then
        List.rev_append (get_unif_candidates_term index s) acc
      else
        acc
    ) !(ui.term_index) []



let backward_superposition_candidates_iter ui l f =
  match l with
  (* If term, find in type index *)
  | Term.Fun _ ->
    let index = TypedUnifIndex.get_index ui.subterm_index l in
    UnifIndex.get_unif_candidates_iter index l f
  (* If var, find in all indices whose type is a *sub*type of *)
  | Term.Var _ ->
    let typ = Term.get_term_type l in
    !(ui.subterm_index) |> SMap.iter (fun t index -> 
      if Symbol.is_subtype t typ then
        UnifIndex.get_unif_candidates_iter index l f
    )

let forward_superposition_candidates_iter ui s f =
  (* UnifIndex.get_unif_candidates_iter ui.term_index s f *)
  match s with
  (* If term, find in type index *)
  | Term.Fun _ ->
    let index = TypedUnifIndex.get_index ui.subterm_index s in
    UnifIndex.get_unif_candidates_iter index s f
  (* If var, find in all indices whose type is a *super*type of *)
  | Term.Var _ ->
    let typ = Term.get_term_type s in
    !(ui.subterm_index) |> SMap.iter (fun t index -> 
      if Symbol.is_subtype typ t then
        UnifIndex.get_unif_candidates_iter index s f
    )
