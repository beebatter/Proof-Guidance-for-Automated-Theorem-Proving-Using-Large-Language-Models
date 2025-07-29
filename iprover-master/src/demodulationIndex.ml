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
  | D_cache
  | D_cache_consist
  | D_subtypes

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_cache -> "cache"
  | D_cache_consist -> "cache_consist"
  | D_subtypes -> "subtypes"

let dbg_groups = [
  D_trace; 
  D_cache; 
  D_cache_consist;
  (* D_subtypes; *)
]


(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module Index = UnifIndexDiscrTree 

(* Caching *)
module Cache = struct
  type t = {
    mutable map: ((term * clause option) TMap.t) SMap.t;
  }

  let empty() = 
    { map = SMap.empty }


   let clear x = 
    x.map <- SMap.empty 

  let clear_sym symb x = 
    x.map <- x.map |> SMap.remove symb (* KK *)
  (*  x.map <- x.map |> SMap.add symb TMap.empty *)

  let find_opt t x = 
    match t with
    | Term.Fun (sym, _, _) ->
      begin match x.map |> SMap.find_opt sym with
      | Some y -> 
        y |> TMap.find_opt t
      | None -> 
        (* x.map <- x.map |> SMap.add sym TMap.empty; *)  (* KK *)
        None
      end
    | Term.Var _ -> invalid_arg "Should not query demod index with variables"

  let add t t' x =
    match t with
    | Term.Fun (sym, _, _) -> 
      x.map <- x.map |> SMap.update sym (function 
        | Some tmap -> Some (TMap.add t t' tmap)
        | None -> Some (TMap.singleton t t')
      )
    | Term.Var _ -> invalid_arg "Should not query demod index with variables"
end

type t = {
  (* mutable *) unit_index : (int * lit * clause) Index.t;
  (* In bwd_index, we store the clause and either the smaller right-hand side, to quickly do a demodulation check, or none, if no check is needed *)
  (* mutable *) bwd_index : (clause * lit option) Index.t;

  order: Orderings.t;

  (* This is the set of all equality types which are also not pure disequality. *)
  eq_types: Symbol.sym_set;
  use_ground: bool;

  cache: Cache.t;
}

let create ~order ~eq_types ~use_ground () =
  {
    unit_index = Index.create();
    bwd_index = Index.create();
    order;
    eq_types;
    use_ground;
    cache = Cache.empty();
  }

let clear ui = 
  Cache.clear ui.cache;
  Index.clear ui.unit_index;
  Index.clear ui.bwd_index

let add_equation_indexed_by ui indexed_by pos lit clause =
  dbg D_trace @@ lazy (sprintf "add_equation_indexed_by: ind_by: %s pos: %s lit: %s cl: %s"
    (Term.to_string indexed_by) (string_of_int pos) (Term.to_string lit) (Clause.to_string clause)
  );
  (* clear cache based on top symbol *)
  (match indexed_by with Term.Fun (sym,_,_) -> Cache.clear_sym sym ui.cache | Term.Var _ -> ());
  Index.add_elem_to_lit ui.unit_index indexed_by (pos, lit, clause)

let add_clause_indexed_by ui indexed_by ~check clause =
  Index.add_elem_to_lit ui.bwd_index indexed_by (clause, check)

let elim_equation_indexed_by ui indexed_by clause =
  dbg D_trace @@ lazy (sprintf "elim_equation_indexed_by: ind_by: %s cl: %s"
    (Term.to_string indexed_by) (Clause.to_string clause)
  );
  (* clear cache based on top symbol *)

  (match indexed_by with Term.Fun (sym,_,_) -> Cache.clear_sym sym ui.cache | Term.Var _ -> ());
  Index.elim_filter_from_lit ui.unit_index indexed_by (fun (_, _, x) -> Clause.Bc.(x == clause))

let elim_clause_indexed_by ui indexed_by clause =
  Index.elim_filter_from_lit ui.bwd_index indexed_by (fun (x, _) -> Clause.Bc.(x == clause))



(* KK: TODO: add only orientable eq *)
let fwd_only_orientable_flag = false


let _ = out_warning (sprintf "%s: fwd_only_orientable_flag: %B" module_name fwd_only_orientable_flag)

(* AC axioms prolific and not necessary for demod *)
let fwd_use_aci = false

let add_equation ui clause =
  if fwd_use_aci || not (Clause.is_ac_axiom clause) then
  match Clause.get_lits clause with
  | [lit] ->
    if ui.use_ground || not @@ Term.is_ground lit then
    begin match Term.Eq.decompose_atom_type lit with
    | Some (etype,l,r) -> 
      if Term.get_top_symb etype != Symbol.symb_bool_type then (
        dbg D_trace @@ lazy (
          sprintf "Added unit eq %s (%s)" 
            (Term.to_string lit)
            (match ui.order.oriented lit with GT -> "oriented >" | LT -> "oriented <" | INC -> "unoriented" | EQ -> assert false)
        );

        (* Flush cache if added *)
        (* KK *)
        (* Cache.clear ui.cache;
        (match l with Term.Fun (sym,_,_) -> Cache.clear_sym sym ui.cache | Term.Var _ -> ());
        (match r with Term.Fun (sym,_,_) -> Cache.clear_sym sym ui.cache | Term.Var _ -> ()); *)

        let open PartialOrd in
        match ui.order.oriented lit with
        | GT ->
          (* Cache.clear_sym (Term.get_top_symb l) ui.cache; *) (* KK shifted to add_equation_indexed_by*)
          add_equation_indexed_by ui l 1 lit clause;
        | LT ->
          (* Cache.clear_sym (Term.get_top_symb r) ui.cache; *)
          add_equation_indexed_by ui r 2 lit clause;
        | INC ->
          if not fwd_only_orientable_flag then (
            (* (match l with Term.Fun (sym,_,_) -> Cache.clear_sym sym ui.cache | Term.Var _ -> ()); *)
            
            if Term.var_subset r l then (* we can demodulate using l only if all vars in r are in l *)      
              add_equation_indexed_by ui l 1 lit clause;
            (* (match r with Term.Fun (sym,_,_) -> Cache.clear_sym sym ui.cache | Term.Var _ -> ()); *)
            if Term.var_subset l r then  (* we can demodulate using r only if all vars in l are in r *)   
              add_equation_indexed_by ui r 2 lit clause;
          )
        | EQ -> assert false
       )
    | None -> ()
    end
    else ()
  | _ -> ()
  else ()

let add_bwd_clause ui clause =
  dbg D_trace @@ lazy (sprintf "Add clause to bwd demod: %s" (Clause.to_string clause));

  let terms = ref TMap.empty in
  let global_check = ref true in  (* If any is nonequality then never need to check *)

  let add_clause_indexed_by' l ~check =
    (* Add a check if l is of eq type *)
    if SSet.mem (Term.get_term_type l) ui.eq_types then (
      dbg D_subtypes @@ lazy (sprintf "%s is eq (%s)" (Term.to_string l) (Symbol.to_string @@ Term.get_term_type l));
      (* If global_check is false then don't waste time, it's going to be None anyways *)
      if Bool.O.(!global_check = false) then
        terms := TMap.add l None !terms
      else
        match check with
        (* If there is one place where check is not needed, then it's not needed at all *)
        | None -> 
          terms := TMap.add l None !terms
        (* If it is required, then update *)
        | Some t ->
          terms := TMap.update l (fun prev ->
            match prev with
            (* Was already found to be not needed *)
            | Some (None) -> prev
            (* Was already found to be needed *)
            | Some (Some t_prev) -> prev
            (* Didn't show up yet *)
            | None -> Some check
          ) !terms
    ) else (
      dbg D_subtypes @@ lazy (sprintf "%s not eq (%s)" (Term.to_string l) (Symbol.to_string @@ Term.get_term_type l));
    )
  in
  (* For use at the top-level to avoid indexing variables *)
  let add_clause_indexed_by'' l ~check = 
    if not @@ Term.is_var l then add_clause_indexed_by' l ~check
  in

  Clause.get_lits clause |> List.iter (fun lit ->
    let sign, atom = Term.split_sign_lit lit in
    match Term.Eq.decompose_atom_type atom with
    (* If equality literal, index subterms of lhs and rhs *)
    | Some (etype,l,r) -> 
      if Term.get_top_symb etype == Symbol.symb_bool_type 
      || Bool.O.(sign = false)
      then 
        global_check := false;
      if Term.get_top_symb etype != Symbol.symb_bool_type then (
        (* add_clause_indexed_by' terms l ~check:(Some r);
        add_clause_indexed_by' terms r ~check:(if Term.is_oriented_eq_lit lit then None else Some l); *)
        match ui.order.oriented lit with
        | GT -> 
          add_clause_indexed_by'' l ~check:(Some r);
          add_clause_indexed_by'' r ~check:(None);
        | LT -> 
          add_clause_indexed_by'' l ~check:(None);
          add_clause_indexed_by'' r ~check:(Some l);
        | INC -> 
          add_clause_indexed_by'' l ~check:(Some r);
          add_clause_indexed_by'' r ~check:(Some l);
        | EQ -> assert false
      );
      l |> Term.iter_subterms_preorder_novar (fun x ->
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        add_clause_indexed_by' x ~check:None
      );
      r |> Term.iter_subterms_preorder_novar (fun x -> 
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        add_clause_indexed_by' x ~check:None
      )
    (* If nonequality literal, index subterms *)
    | None -> 
      global_check := false;
      lit |> Term.iter_subterms_preorder_novar (fun x -> add_clause_indexed_by' x ~check:None);
      (* failwith "add_clause: non-equality literal" *)
  );

  if !global_check then (
    !terms |> TMap.iter (fun t check -> 
      dbg D_trace @@ lazy (sprintf "add %s" (Term.to_string t));
      add_clause_indexed_by ui t ~check clause
    )
  ) else (
    !terms |> TMap.iter (fun t check -> 
      dbg D_trace @@ lazy (sprintf "add %s" (Term.to_string t));
      add_clause_indexed_by ui t ~check:None clause
    )
  )



let elim_equation ui clause =
  match Clause.get_lits clause with
  | [lit] ->
    if ui.use_ground || not @@ Term.is_ground lit then
    begin match Term.Eq.decompose_atom lit with
    | Some (l,r) -> 
      dbg D_trace @@ lazy (
        sprintf "Del unit eq %s (%s)" 
          (Term.to_string lit)
          (match ui.order.oriented lit with | GT | LT -> "oriented" | INC -> "unoriented" | EQ -> assert false)
      );

      begin match ui.order.oriented lit with
      | GT ->
        elim_equation_indexed_by ui l clause;
      | LT ->
        elim_equation_indexed_by ui r clause;
      | INC ->
        elim_equation_indexed_by ui l clause;
        elim_equation_indexed_by ui r clause;
      | EQ -> assert false
      end
    | None -> ()
    end
    else ()
  | _ -> ()

let elim_bwd_clause ui clause =
  dbg D_trace @@ lazy (sprintf "Del clause from bwd demod: %s" (Clause.to_string clause));

  let terms = ref TSet.empty in

  let elim_clause_indexed_by' terms l =
    terms := TSet.add l !terms
  in

  Clause.get_lits clause |> List.iter (fun lit ->
    match Term.Eq.decompose_atom_type @@ Term.get_atom lit with
    (* If equality literal, index subterms of lhs and rhs *)
    | Some (etype,l,r) -> 
      if Term.get_top_symb etype != Symbol.symb_bool_type then (
        elim_clause_indexed_by' terms l;
        elim_clause_indexed_by' terms r;
      );
      l |> Term.iter_preorder_novar (fun x ->
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        elim_clause_indexed_by' terms x
      );
      r |> Term.iter_preorder_novar (fun x -> 
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        elim_clause_indexed_by' terms x
      )
    (* If nonequality literal, index subterms *)
    | None -> 
      lit |> Term.iter_subterms_preorder_novar (fun x -> elim_clause_indexed_by' terms x);
      (* failwith "add_clause: non-equality literal" *)
  );

  !terms |> TSet.iter (fun t -> 
    dbg D_trace @@ lazy (sprintf "remove %s" (Term.to_string t));
    try
      elim_clause_indexed_by ui t clause
    with Not_found -> 
      dbg D_trace @@ lazy "(not there in the first place)";
      ()
  );
  dbg D_trace @@ lazy (sprintf "end remove")

let elim_clause ui clause =
  elim_equation ui clause;
  elim_bwd_clause ui clause






let print_candidates unif_cand =
  let print_elem (pos,lit,clause) = 
    sprintf "%s [%d] @ %s" (Term.to_string lit) (pos) (Clause.to_string clause)
  in
  let print_candidate (term,l) = 
    sprintf "%s: %s" (Term.to_string term) (list_to_string print_elem l ~first:"[" ~last:"]" ", ")
  in
  list_to_string print_candidate unif_cand ~first:"[" ~last:"]" "; "

let get_fwd_candidates ui term =
  if not @@ SSet.mem (Term.get_term_type term) ui.eq_types then [] else
  (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  let candidates = Index.get_gen_candidates ui.unit_index term in
  (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print_candidates candidates)); *)
  candidates

let get_fwd_candidates_caching ui term func =
  (* Idea is: if cached, return. Else, pass candidates to func (external 
     callback which implements the desired functionlity) and cache the 
     result *)

  let get_demod_result () = 
    (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
    let candidates = Index.get_gen_candidates ui.unit_index term in
    (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print_candidates candidates)); *)
    let result = func candidates term in
    ui.cache |> Cache.add term result;
    result

  in
  if not @@ SSet.mem (Term.get_term_type term) ui.eq_types then (term, None) else
  match ui.cache |> Cache.find_opt term with
  | Some cached -> 
    dbg D_cache @@ lazy (sprintf "Demod cache hit: %s" (Term.to_string term));
    dbg_env D_cache_consist (fun () -> 
      let demod_res = get_demod_result () in
      let (cached_t',_), (demod_t',_) = cached, demod_res in
      if cached_t' != demod_t' then
        failwith (sprintf "get_fwd_candidates_caching: t: %s cached_t': %s demod_t': %s"
          (Term.to_string term) (Term.to_string cached_t') (Term.to_string demod_t')
        );
    );
    cached
  | None -> 
    dbg D_cache @@ lazy (sprintf "Demod cache miss: %s" (Term.to_string term));
    get_demod_result ()

  (* (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  let candidates = Index.get_gen_candidates ui.unit_index term in
  (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print_candidates candidates)); *)
  let result = func candidates term in
  ui.cache |> Cache.add term result;
  result *)


let get_bwd_candidates ui term =
  (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  let candidates = Index.get_inst_candidates ui.bwd_index term in
  (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print_candidates candidates)); *)
  candidates



let fwd_candidates_iter ui term f =
  Index.get_gen_candidates_iter ui.unit_index term f

let bwd_candidates_iter ui term f =
  Index.get_inst_candidates_iter ui.bwd_index term f
