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


(* to generate interface *)
(* ocamlc -I obj/ -i src/logic_interface.ml > src/logic_interface.mli *)
open Lib
open Options



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_symb_cmp (* symbol order *)
      
let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_symb_cmp -> "symb_cmp" (* symbol order *)
        
let dbg_groups = [
  (*
  D_trace;
   *)
  D_symb_cmp
]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)


(* symbol *)
module SSet = Symbol.Set
module SMap = Symbol.Map
module SHtbl = Symbol.Hashtbl

(* var *)
module VSet = Var.VSet
module VMap = Var.VMap
module VHtbl = Var.VHashtbl

(* bound var*)
module BVSet = Var.BSet
module BVMap = Var.BMap
module BVHtbl = Var.BHashtbl

(* term *)
module TSet = Term.TSet
module TMap = Term.TMap
module THtbl = Term.THashtbl

(* bound term *)
module BTSet = Term.BTSet
module BTMap = Term.BTMap
module BTHtbl = Term.BTHashtbl

(* Map/Set for lits of terms *)
module TMapList = Term.TMapList
module TSetList = Term.TSetList

(* clause *)
module CSet = Clause.CSet
module CMap = Clause.CMap
module CHtbl = Clause.CHashtbl

(* basic clause as key *)
module BCSet = Clause.BCSet  
module BCMap = Clause.BCMap 
module BCHtbl = Clause.BCHashtbl

(* sup/inst/res states id counter
 TODO: currently used only in sup *)

let proof_state_cnt = ref 0

let next_proof_state_id () = 
  let id = !proof_state_cnt in 
  proof_state_cnt := !proof_state_cnt + 1; 
  id
  

type var    = Var.var
type symbol = Symbol.symbol
type stype = Symbol.stype
type term = Term.term
type args = Term.args
type atom = Term.atom
type lit = Term.lit
type lits = lit list
type literal = lit
type clause = Clause.clause
type tstp_source = Clause.tstp_source
type context = Clause.context
(* type proof_search_param = Clause.proof_search_param *)
type symbol_db_ref = SymbolDB.symbolDB ref
type subst = Subst.subst

type ordering = Orderings.t
type bit_vec = Bit_vec.bit_vec

type term_db_ref = TermDB.termDB ref

let symbol_db_ref = SystemDBs.symbol_db_ref
let term_db_ref = SystemDBs.term_db_ref

exception Empty_clause = Clause.Empty_clause 


(* let context = Parser_types.context *)
(* first when term_db_ref is a parameter *)

(*----------------- symbols ---------------------------------*)

(*let creat_const_symb symb_name ~symb_type =*)

let create_stype = Symbol.create_stype

let create_symbol ?(is_special=false) symbol_name symbol_stype =
  let symb =  
    SymbolDB.add_ref
      (Symbol.create_from_str_type symbol_name symbol_stype)
      symbol_db_ref
  in
  Symbol.set_is_special_symb is_special symb;
  symb

let create_symbol_property ?(is_special=false) name stype sproperty =
  let symb =  
    SymbolDB.add_ref
      (Symbol.create_from_str_type_property name stype sproperty)
      symbol_db_ref
  in
  Symbol.set_is_special_symb is_special symb;
  symb

let find_symbol symb_name = 
  SymbolDB.find
    (Symbol.create_template_key_symb symb_name) !symbol_db_ref 

let find_symbol' symb_name =
  try
    find_symbol symb_name 
  with
    Not_found ->
      out_str (sprintf "%s" (SymbolDB.to_string !symbol_db_ref));
      failwith (sprintf "%s: find_symbol': symb not found: %s" __MODULE__ symb_name)

    
(*
let special_symbols_set = SystemDBs.special_symbols_set
let is_special_symbol = SystemDBs.is_special_symbol
*)
let is_special_symb = Symbol.is_special_symb

let symbol_cmp_custom' ascending_lt fallback ascending_gt  = 

  let n = SymbolDB.size !symbol_db_ref (* - Symbol.get_num_types () *) in
  dbg D_symb_cmp (lazy (sprintf "db_size %i, get_greatest_key: %i" n (SymbolDB.get_greatest_key !symbol_db_ref)));  
  let arr = Array.make n (-1) in
  let fallback_list = 
    let set_ascending = SSet.union (SSet.of_list ascending_lt) (SSet.of_list ascending_gt) in
    SymbolDB.fold (fun sym acc -> 
      if not (SSet.mem sym set_ascending)
      && (match Symbol.get_property sym with Symbol.Type | Symbol.Placeholder -> false | _ -> true)
      then
        sym::acc
      else
        acc
    ) !symbol_db_ref []
  in
  (ascending_lt @ List.sort fallback fallback_list @ ascending_gt) |>
  List.iteri (fun i x ->
    dassert(fun () -> (Symbol.get_fast_key x) < n); 
    arr.(Symbol.get_fast_key x) <- i)
    ;
  fun x y ->
    dbg D_symb_cmp (lazy (sprintf "get_fast_key %s:  %i get_fast_key %s: %i"
                            (Symbol.to_string x) (Symbol.get_fast_key x)
                            (Symbol.to_string y) (Symbol.get_fast_key y)));
    dassert (fun () -> (Symbol.get_fast_key x) < n);
    dassert (fun () -> (Symbol.get_fast_key y) < n);
    dassert (fun () -> arr.(Symbol.get_fast_key x) <> -1);
    dassert (fun () -> arr.(Symbol.get_fast_key y) <> -1);
    Int.compare (arr.(Symbol.get_fast_key x)) (arr.(Symbol.get_fast_key y))
      
let symbol_cmp_custom ascending_lt fallback ascending_gt = 
  lex_combination2 Symbol.cmp_fst (symbol_cmp_custom' ascending_lt fallback ascending_gt)



(*---------------terms/lits----------------------------------*)
let add_fun_term symb args = 
  TermDB.add_ref (Term.create_fun_term symb args) term_db_ref

let add_fun_term_args symb args = 
  TermDB.add_ref (Term.create_fun_term_args symb args) term_db_ref

let add_var_term var = 
  TermDB.add_ref (Term.create_var_term var) term_db_ref

let add_term_db term = 
  TermDB.add_ref term term_db_ref

(* let normalise_eq_lit term = 
  TermDB.normalise_eq_lit term term_db_ref *)

let add_pred_0 ?(is_special=false) pred_name = 
  let pred_type = Symbol.create_stype [] Symbol.symb_bool_type in
  let pred_symb = create_symbol ~is_special pred_name pred_type in 
  add_fun_term pred_symb []


let add_typed_equality_term stype_term t s =
  let args = [stype_term; t; s] in
  add_fun_term Symbol.symb_typed_equality args

let add_typed_equality_sym eq_type_sym t s =
  let eq_type_term = (add_fun_term eq_type_sym []) in
  add_typed_equality_term eq_type_term t s


let add_typed_disequality_term eq_type t s =
  add_fun_term Symbol.symb_neg [(add_typed_equality_term eq_type t s)]

let add_typed_disequality_sym eq_type_sym t s =
  add_fun_term Symbol.symb_neg [(add_typed_equality_sym eq_type_sym t s)]

let add_neg_atom atom =
  let args = [atom] in
  add_fun_term Symbol.symb_neg args

let add_neg_atom_symb symb args = 
  add_neg_atom	(add_fun_term symb args)

let add_lit sign atom = 
  if sign 
  then 
    add_term_db atom
  else 
    add_neg_atom atom


let add_lit_symb sign symb args = 
  if sign 
  then 
    add_fun_term symb args 
  else 
    add_neg_atom_symb symb args

(* let add_lit_eq_term sign eq_type l r = *)
let add_lit_eq sign eq_type l r =
  (* add_lit_symb sign Symbol.symb_typed_equality [l;r] *)
  (* add_lit_symb sign Symbol.symb_default_type [l;r] *)
  if sign then
    add_typed_equality_term eq_type l r
  else
    add_typed_disequality_term eq_type l r


let add_compl_lit lit = 
  TermDB.add_ref (Term.compl_lit lit) term_db_ref 

let term_true = add_fun_term Symbol.symb_true []

let term_false = add_fun_term Symbol.symb_false []

let smallest_constant_of_type = 
  let map = ref SMap.empty in
  let f typ = 
    match !map |> SMap.find_opt typ with
    | Some c -> c
    | None -> 
      let symb = 
        let symb = 
          Symbol.create_from_str_type_property 
            (sprintf "$$iProver_smallest_%s" (Symbol.get_name typ)) 
            (Symbol.create_stype [] typ)
            (Symbol.Smallest)
        in
        (* Symbol.set_is_special_symb false symb; *)
        SymbolDB.add_ref symb symbol_db_ref
      in
      let c = add_fun_term symb [] in
      map := !map |> SMap.add typ c;
      c
  in
  f



let subst_apply = 
  Subst.apply_subst_term term_db_ref



(*-----------term views----------*)	
type eq_view_type_term = 
    Eq_type_term of term * term * term

type eq_view_type_symb = 
  | Eq_type_symb of symbol * term * term 

	
let term_eq_view_type_term t = 
  match t with
  |Term.Fun (sym, args, _info) ->
      if (sym == Symbol.symb_typed_equality)
      then 
	let (type_term_eq, t, s) = get_triple_from_list (Term.arg_to_list args) in
	Def(Eq_type_term (type_term_eq, t, s))
      else
	Undef
  |_-> Undef
	
(* should be used with care since assume that eq type is not a variable here *)	
let term_eq_view_type_symb t = 
  match t with
  |Term.Fun (sym, args, _info) ->
      if (sym == Symbol.symb_typed_equality)
      then 
	let (type_term_eq, t, s) = get_triple_from_list (Term.arg_to_list args) in
	let eq_type_symb = 
	  try
	    Term.get_top_symb type_term_eq
	  with 
	    Term.Var_term ->
	      failwith
		("term_eq_view_type_symb: equality type is a variable in"^(Term.to_string t))
	in				
	Def(Eq_type_symb (eq_type_symb, t, s))
      else
	Undef
  |_-> Undef
	
	
(*
  let dis_equality t s =
  neg_atom (equality_term t s)
 *)

(*
  let add_typed_dis_equality stype t s =
  add_neg_atom (add_typed_equality_term stype t s)
 *)
(* used for model output *)
let add_term_algebra_eq_term args = 
  let ta_eq_type_term = (add_fun_term Symbol.symb_term_algebra_type []) in
  add_fun_term Symbol.symb_typed_equality  (ta_eq_type_term::args)

(*--------clause---------------*)	
(*-----------empty clause check ------------*)
let check_empty_clause clause =
  if Clause.is_empty_clause clause then
    raise (Empty_clause clause)

(*let _ = out_warning "!!! logic_interface.ml: reate_clause ~normalise_eqs: false" *)

(*----------------*)
let create_clause ?(check_empty=true) ?(is_negated_conjecture=false)  ?(bc_imp_inh=bc_imp_inh_default_val) ?(normalise_eqs=false) tstp_source lits = 
(* Normalisation is done in Clause.create_clause *)
(*  let norm_lits = Clause.normalise_lit_list term_db_ref lits in *) 
(*  test: let clause = Clause.create_clause term_db_ref ~is_negated_conjecture ~bc_imp_inh ~normalise_eqs:false tstp_source (* norm_lits *) lits in *)

  let clause = Clause.create_clause term_db_ref ~is_negated_conjecture ~bc_imp_inh ~normalise_eqs tstp_source (* norm_lits *) lits in 
  if check_empty then
    check_empty_clause clause; 
  clause

let create_clause_context context ?(check_empty=true) ?(is_negated_conjecture=false) ?(bc_imp_inh=bc_imp_inh_default_val) tstp_source lits =
  let clause = create_clause ~check_empty ~is_negated_conjecture ~bc_imp_inh tstp_source lits in
  Clause.context_add context clause

let normalise_blitlist_list blitlist_list = 
  Clause.normalise_blitlist_list term_db_ref blitlist_list 		

let get_lits c = Clause.get_lits c

let clause_register_subsumed_by context ~by c = 
  (if  (not (by == c)) 
  then Clause.assign_is_dead context true c);
  if (Clause.equal_bc by c)
  then ()
  else
    ( 
(*      Clause.set_ps_simplifying true by; *)
(*      Clause.assign_replaced_by context (Def(Clause.RB_subsumption by)) c; *) (* TODO *)
      Clause.inherit_conjecture c by
     )

let pp_clause_with_source = Clause.pp_clause_with_source
    
let pp_clause_list_with_source = Clause.pp_clause_list_with_source
    

(*---------- context ----------------*)				
let context_create = Clause.context_create
let context_add  = Clause.context_add
let context_remove = Clause.context_remove  
let context_mem = Clause.context_mem 
(* let context_mem_lits = Clause.context_mem_lits *)
(* let context_reset = Clause.context_reset *)
let context_find = Clause.context_find
(* let context_find_lits = Clause.context_find_lits *)
let context_iter = Clause.context_iter 
let context_fold = Clause.context_fold 
let context_size = Clause.context_size

(* context_add_context from_cxt to_cxt *)
(* let context_add_context = Clause.context_add_context *)
    
(** replaces dead with simplified_by *)
(* let context_replace_by_clist = Clause.context_replace_by_clist *)


(*---- for aguments which are either context or list we can use --------*)

type context_list = Clause.context_list

let cl_iter = Clause.cl_iter
let cl_fold = Clause.cl_fold
let cl_size = Clause.cl_size

(*-------------------------*)
let is_ver_epr () = 
  (!global_options.aig_mode || (!global_options.bmc1_incremental)) 


(*----------------------------*) 

(* ------------------------ *)
(* Equality transformations *)
(* ------------------------ *)

module EqualityTransformation = struct
  let ttop = SystemDBs.top_term (* add_fun_term (Symbol.symb_top) [] *)
  let eq_type = add_fun_term Symbol.symb_bool_type []
  (* let eq_type = add_fun_term Symbol.symb_default_type [] *)

  let lit_to_eq lit =
(*    dbg D_trace @@ lazy (sprintf "Rectifying %s" (Term.to_string lit));*)
    if Term.Eq.is_eq lit then (
      lit
    ) else (
      let sign, atom = Term.split_sign_lit lit in
      add_lit_eq sign eq_type atom ttop
    )

  let litlist_to_eq l =
    List.map lit_to_eq l

(* checks if lit is the result of eq transformation *)
  let is_eq_tansformed_lit l = 
    let atom = Term.get_atom l in
    match (term_eq_view_type_term atom) with
    |Def(Eq_type_term(equality_type_term, t,s)) -> 
        if s == ttop then 
          true
        else
          false
    |Undef -> false
          
    
  let clause_to_eq clause =
    let lits = Clause.get_lits clause in
    (* If all literals in clause were equality then no transformation was made *)
    if List.for_all Term.is_eq_lit lits then
      clause
    (* Otherwise add modified clauses as an inference via [Clause.Predicate_to_equality] *)
    else
      let source = Clause.TSTP_inference_record (Clause.Predicate_to_equality, [clause]) in
      List.map lit_to_eq lits
      |> create_clause source 

  let clauselist_to_eq l =
    List.map clause_to_eq l

   (*------- reverse functions; used in Prop_solver_exhange---------*)   
      
  let lit_to_eq_rev lit = 
    let sign, atom = Term.split_sign_lit lit in
    let atom_rev = 
      match (term_eq_view_type_term atom) with
      |Def(Eq_type_term(equality_type_term, t,s)) -> 
          if s == ttop 
          then 
            t
          else
            atom
      |Undef -> lit 
    in
    if (atom_rev == atom) 
    then 
      lit 
    else 
      add_lit sign atom_rev

(* reverse eq transformation *)
  let clause_rev_eq_trans clause = 
    let lits = Clause.get_lits clause in
    if List.exists is_eq_tansformed_lit lits then
      let source = Clause.TSTP_inference_record (Clause.Predicate_to_equality_rev, [clause]) in
      List.map lit_to_eq_rev lits
      |> create_clause source    
    else
      clause
   

(* returns transform if the clause has non-eq lits or reverse transormation if cluase contains transformed lit *)
(* we assume that either transf was applied to all lits or no lits in the cluase *)
(* flipped is the same if no literal was transformed and no non-eq literals in the clause *)
(* flip is used in Prop_solver_exchange *)
  let flip_clause clause = 
(* first try to reverse *)
    let clause_rev = clause_rev_eq_trans clause in 
    if clause_rev == clause then 
      clause_to_eq clause
    else
      clause_rev
      
end

 

(*------ debug output (outdated) ----------*)

let out_symbs () =   
  let out_symb symb =
    out_str ("Symb: "
	     ^(Symbol.to_string symb)
	     ^" is conj symb: "
	     ^ (string_of_bool (Symbol.get_bool_param Symbol.is_conj_symb symb ))^"\n"
	     ^"has bound constant: "
	     ^(string_of_bool (Symbol.get_bool_param Symbol.is_bound_constant symb ))^"\n" );
  in
  out_str ("\n\n ------------------------------\n\n");
  SymbolDB.iter out_symb !symbol_db_ref;
  out_str ("\n\n ------------------------------\n\n")

let out_terms () =
  let out_term t =
    if (not (Term.is_var t))
    then
      (    out_str ("Term: "
	     ^(Term.to_string t)
	     ^" has conj symb: "
	     ^ (string_of_bool (Term.get_fun_bool_param Term.has_conj_symb t ))^" "
	     ^"has bound constant: "
	     ^(string_of_bool (Term.get_fun_bool_param Term.has_bound_constant t ))^"\n" )
          )
    else
      (    out_str ("Var Term: "
	            ^(Term.to_string t)
                   )
          )
  in       
  out_str ("\n\n ------------------------------\n\n");
  TermDB.iter out_term !term_db_ref;
  out_str ("\n\n ------------------------------\n\n")


let out_basic_mem () = 
  print_objsize "term_db" term_db_ref;
  print_objsize "symbol_db_ref" symbol_db_ref;
  Clause.out_mem ()





(* let kbo_map = ref Orderings_cache.EqMap.empty *)

(* let is_oriented_kbo = 
  let id = KBO.get_ordering_id () in
  Orderings_cache.is_oriented KBO.kbo_atoms id

let is_any_oriented_kbo = 
  let id = KBO.get_ordering_id () in
  Orderings_cache.is_any_oriented KBO.kbo_atoms id *)



(*------------- output SZS strings ---------------*)

let szs_pref = "% SZS status "


let get_szs_suff () = 
  let szs_for_str = 
    list_of_str_to_str (List.map Filename.basename !global_options.problem_files) ","
  in
  if List.X.is_empty !global_options.problem_files then 
    ""
  else
    " for "^szs_for_str

let szs_unknown_str ()     = szs_pref^"Unknown"^(get_szs_suff ())
let szs_theorem_str ()     = szs_pref^"Theorem"^(get_szs_suff ())
let szs_unsat_str   ()     = szs_pref^"Unsatisfiable"^(get_szs_suff ())
let szs_sat_str     ()     = szs_pref^"Satisfiable"^(get_szs_suff ())
let szs_counter_sat_str () = szs_pref^"CounterSatisfiable"^(get_szs_suff ())

let szs_start_model () = "% SZS output start Model"^(get_szs_suff ())^"\n"
let szs_end_model () = "% SZS output end Model"^(get_szs_suff ())^"\n"

let szs_start_saturation () = "% SZS output start Saturation"^(get_szs_suff ())^"\n"
let szs_end_saturation () = "% SZS output end Saturation"^(get_szs_suff ())^"\n"

let szs_start_cnfrefutation () = "% SZS output start CNFRefutation"^(get_szs_suff ())^"\n"
let szs_end_cnfrefutation () = "% SZS output end CNFRefutation"^(get_szs_suff ())^"\n"

let szs_start_listof () = "% SZS output start ListOfFOF"^(get_szs_suff ())^"\n"
let szs_end_listof () = "% SZS output end ListOfFOF"^(get_szs_suff ())^"\n"

let szs_out_answer_stream stream ~answer_list  = 
(*  let answer_list = get_answer () in *)
  let s = stream in
  let answer_atom_to_stream stream answer =
    let args = Term.arg_to_list (Term.get_args answer) in
    stream.stream_add_char '[';
    list_to_stream stream Term.to_stream args ",";
    stream.stream_add_char ']'
  in
  let answer_list_to_stream a_list =
    if List.compare_length_with a_list 1 = Ord.gt
    then
      begin
        s.stream_add_char '(';
        list_to_stream s answer_atom_to_stream a_list "|";
        s.stream_add_char ')';
      end
    else
      begin
        if List.X.is_singleton a_list
        then
          answer_atom_to_stream s (List.hd a_list)
        else ()
      end
  in
  s.stream_add_char '\n';
  s.stream_add_str "% SZS answers Tuple [";
  answer_list_to_stream answer_list;
  s.stream_add_str "]";
  s.stream_add_str " for ";
  list_to_stream s (fun stream string -> stream.stream_add_str string) !global_options.problem_files ",";
  s.stream_add_str "\n"
    

let szs_out_answer ~answer_list = szs_out_answer_stream stdout_stream ~answer_list

exception Unsatisfiable_gr_smt_na of clause list
exception Satisfiable_gr_smt_na of unit



