(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2023 Konstantin Korovin and The University of Manchester. 
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

  
(* Equational flattening for now of ground subterms:
   For each ground subterm f(t1,..,t_n) in a clause C[f(t1,..,t_n)] replace by
   C[d_k] and 
   f(u1,..,un) = d_k
   where u_1 ... u_n are new constant defs for ground t_1 .., tn and d_k is the constant def. for f(t1,..,tn)
     
 *)

open Lib
open Logic_interface


let dbg_flag = false

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"

	
let dbg_groups =
  [
   D_trace
 ]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)


(* TODO move *)
    
let change_conj_symb cls =
  let rec change_conj_symb_term is_conj t =
    match t with
    | Term.Fun (symb, args, info) ->
	(* if it is conjecture and symbol is plain (non-theory, neg, quant, etc) *)
	(*	    let stype = (Symbol.get_type symb) in                        *)
	(if (is_conj
	       &&
	     ((Symbol.is_fun symb) || (Symbol.is_pred symb))
	       (* &&
		  ((Symbol.get_property symb) = Symbol.Undef_Prop) *))
	then
	  Symbol.set_bool_param
	    true Symbol.is_conj_symb symb
	else()
	);
	Term.arg_iter (change_conj_symb_term is_conj) args;
	Term.assign_has_conj_symb t;
	Term.assign_has_non_prolific_conj_symb t
    | Term.Var _ -> ()
  in
  let change_conj_symb_clause is_conj c =
    Clause.iter (change_conj_symb_term is_conj) c;
    Clause.reset_has_conj_symb c;
    Clause.reset_has_non_prolific_conj_symb c
  in
  List.iter (change_conj_symb_clause true) cls
    
(*-------------------*)
    
type def_env =
    {
     mutable term_to_def : term TMap.t;
     mutable term_to_def_ax : clause TMap.t; 
   }

    
(* ext_..: map all ground non-constant terms to def. constants *)

let new_def_constant val_type term =
  assert(Term.is_ground term);
  let def_symb =
    SymbolDB.create_new_def_symb
      symbol_db_ref
      (Symbol.create_stype [] val_type)  
  in
  let def_const = add_fun_term def_symb [] in
  def_const

    
let rec ext_de_term de term =
  match term with  
  | Term.Fun (sym, args, _) ->
      let args = Term.arg_to_list args in
      if (List.X.is_empty args) || (TMap.mem term de.term_to_def) then ()
          (* do not add defs for constants *)
      else
        (
         List.iter (ext_de_term de) args;
         (if (Term.is_ground term) then
           begin
             let val_type = Symbol.get_val_type_def sym in
             let def_const = new_def_constant val_type term in
             de.term_to_def <- TMap.add term def_const de.term_to_def;
             let def_args =
               List.map (fun t -> try (TMap.find t de.term_to_def) with Not_found -> t) args in
             let l_def_term = add_fun_term sym def_args in (* f(u1,..,un) *)
             let def_type = val_type in
             let def_eq = add_typed_equality_sym def_type l_def_term def_const in (* f(u1,..,un) = d_k *)
             
             let tstp_source = Clause.tstp_source_definition in
             let def_ax = create_clause tstp_source [def_eq] in
             dbg D_trace @@ lazy (sprintf "def_ax: %s" (Clause.to_string def_ax));
             de.term_to_def_ax <- TMap.add term def_ax de.term_to_def_ax
           end
         )   
        )
  | Var _ -> ()

        
let ext_de_lit de lit =
  let atom = Term.get_atom lit in
  let relevant_terms = 
    match term_eq_view_type_term atom with
    |Def (Eq_type_term (_t_type, t, s)) -> [t;s]      
    | Undef -> (* non-equational atom *)
        (
         match atom with  (* do not add def. for the atom itself *)
         | Term.Fun (sym, args, _) -> Term.arg_to_list args 
         | Term.Var _ -> []
        )
  in
  List.iter (ext_de_term de) relevant_terms
    
let ext_de_clause de clause =
  Clause.iter (ext_de_lit de) clause

let ext_de_clause_list de cls =
  List.iter (ext_de_clause de) cls 

(*--------*)
(* apply defintions to clauses *)

let rec apply_de_to_term de  term =
  try
    TMap.find term de.term_to_def
  with
    Not_found -> 
      match term with  
      | Term.Fun (sym, args, _) ->
          let args = Term.arg_map (apply_de_to_term de) args in
          add_fun_term_args sym args
      | Term. Var _ -> term

let rec collect_def_axs de (axs,tried_terms) term =
  if (TSet.mem term tried_terms) then (axs, tried_terms)
  else
    let axs1 =
      try
        (TMap.find term de.term_to_def_ax)::axs
      with
        Not_found -> axs
    in
    let new_tried_terms = TSet.add term tried_terms in
    match term with  
    | Term.Fun (sym, args, _) ->
        let arg_list = Term.arg_to_list args in
        List.fold_left (collect_def_axs de) (axs1,new_tried_terms) arg_list 
        
    | Term. Var _ -> (axs1, tried_terms)


          
let apply_de_to_clause de tried_terms clause =
  let lits = Clause.get_lits clause in
  let new_lits = List.map (apply_de_to_term de) lits in
  let (def_axs,new_tried_terms) =
    List.fold_left (collect_def_axs de) ([],tried_terms) lits in
  dbg D_trace @@ lazy (sprintf "def_axs: %s" (Clause.clause_list_to_string def_axs));

  let tstp_source = Clause.tstp_source_demodulation ~main:clause ~eqs:def_axs in
  let is_negated_conjecture = Clause.is_negated_conjecture clause in
  let new_clause = create_clause ~is_negated_conjecture tstp_source new_lits in
  let new_clauses = new_clause::def_axs in
  (if is_negated_conjecture  then 
    change_conj_symb new_clauses);
    
  dbg D_trace @@ lazy (sprintf "orig: %s" (Clause.to_string clause));
  dbg D_trace @@ lazy (sprintf "flat: %s" (Clause.clause_list_to_string new_clauses));
  (new_clauses,new_tried_terms)


let apply_de_to_cl_list de tried_terms cls =
  List.fold_left (fun (cls_rest,tried_terms_rest) c ->
    let (new_cls,new_tried_terms) = (apply_de_to_clause de tried_terms_rest c) in
    (new_cls@cls_rest,new_tried_terms)
    ) ([],tried_terms) cls

(*----------*)

let flatten_clause_list cls =
  let de =
    {
     term_to_def = TMap.empty;
     term_to_def_ax = TMap.empty;  
   }
  in
  ext_de_clause_list de cls;
  let tried_terms = TSet.empty in
  let (new_cls, _new_tried_terms) = apply_de_to_cl_list de tried_terms cls in
  new_cls
  
(*    
let ext_eq_axs_tem 
  
    
  (* old *)
  let f de subterm =    
    if ((Term.is_const_term subterm)
      || (Term.is_var subterm)
      || (TMap.mem subterm def_env.term_to_def_map))
    then de
    else
      (
       let new_def_const = new_def_constant subterm in
       de.term_to_def_map <- TMap.add subterm new_def_const de.term_to_def_map;
       let def_atom = 
       let def_ax = Clause.create_clause_raw tstp_source (sorted_terms@[neg_def_atom]) in 

      )

        let new_de = List.fold_left (fun curr_de t -> ext_de_term curr_de tstp_source t) de args in        
let tstp_source = Clause.tstp_source_definition in
*)
