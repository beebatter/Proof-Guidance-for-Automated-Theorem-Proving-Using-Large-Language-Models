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
open Statistics
open Options
open Logic_interface


(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_prop
  | D_fd
  | D_term_cnt_map

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_prop -> "prop"
  | D_fd -> "fd"
  | D_term_cnt_map -> "term_cnt_map"

let dbg_groups =
  [
(*
   D_trace;
   D_prop;

   D_fd; *)
  D_term_cnt_map;
 ]
    
let module_name = "problem_properties"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)





(*---------------Probelm Properties---------------------*)

type prob_props =
    {
     mutable clauses : int;
     mutable conjectures : int;
     mutable epr : int;
     mutable horn : int;
     mutable ground : int;
     mutable unary : int;
     mutable binary : int;
     mutable lits : int;
     mutable lits_eq : int;

     (*------ finite domains -----*)
     mutable fd_pure         : int;
     mutable fd_pseudo       : int;
     mutable fd_cond         : int;
     mutable fd_pseudo_cond : int;

     (*---- AC symbols -------*)
     mutable ac_symbols: AC.Table.t;
     mutable theories: Theory_db.Record.t;

     (*---- Misc ----*)
     mutable has_is_int_rat: bool;
   }


(* initialisation is like this due to how epr/horn is reassigned *)
let empty_prob_props () =
  {
   clauses = 0;
   conjectures = 0;
   epr = 0;
   horn = 0;
   ground = 0;
   unary = 0;
   binary = 0;
   lits = 0;
   lits_eq = 0;

   fd_pure   = 0;
   fd_pseudo = 0;
   fd_cond   = 0;
   fd_pseudo_cond = 0;

   ac_symbols = AC.Table.empty;
   theories = Theory_db.Record.create();

   has_is_int_rat = false;
 }

let is_epr     prob_props = (prob_props.clauses = prob_props.epr)
let is_horn    prob_props = (prob_props.clauses = prob_props.horn)
let is_ground  prob_props = (prob_props.clauses = prob_props.ground)
let has_eq     prob_props = (prob_props.lits_eq > 0)
let is_unary   prob_props = (prob_props.clauses = prob_props.unary) 
let is_binary  prob_props = (prob_props.clauses = prob_props.unary + prob_props.binary) 
let is_pure_eq prob_props = (prob_props.lits_eq = prob_props.lits)
let is_unit_eq prob_props = (is_unary prob_props && is_pure_eq prob_props)
let has_ac     prob_props = AC.Table.has_ac prob_props.ac_symbols

let prob_props_to_string props =
  let props_list =
    [
     ("clauses", string_of_int props.clauses);
     ("conjectures", string_of_int props.conjectures);
     ("EPR", string_of_int props.epr);
     ("Horn", string_of_int props.horn);
     ("unary", string_of_int props.unary);
     ("binary", string_of_int props.binary);
     ("lits", string_of_int props.lits);
     ("lits eq", string_of_int props.lits_eq);

     ("fd_pure", string_of_int props.fd_pure);
     ("fd_pseudo", string_of_int props.fd_pseudo);
     ("fd_cond", string_of_int props.fd_cond);
     ("fd_pseudo_cond", string_of_int props.fd_pseudo_cond);
     
     ("AC symbols", string_of_int (SMap.cardinal props.ac_symbols.ac));
   ]
  in
  Options.opt_val_list_to_string props_list
    

let prob_props_to_statistics props = 
  assign_int_stat props.clauses clauses; 
  assign_int_stat props.conjectures conjectures; 
  assign_int_stat props.epr epr;
  assign_int_stat props.horn horn;
  assign_int_stat props.ground ground;
  assign_int_stat props.unary unary;
  assign_int_stat props.binary binary;
  assign_int_stat props.lits lits;
  assign_int_stat props.lits_eq lits_eq;
  assign_int_stat props.fd_pure fd_pure;
  assign_int_stat props.fd_pseudo fd_pseudo;
  assign_int_stat props.fd_cond fd_cond; 
  assign_int_stat props.fd_pseudo_cond fd_pseudo_cond;
  (* assign_int_stat (SSet.cardinal props.ac_symbols) ac_symbols; *)
  assign_fun_stat (fun () -> SMap.cardinal props.ac_symbols.ac) ac_symbols;
  ()



let add_prob_props prob_props cl = 
  let num_cl_lits = (Clause.length cl) in 
  let nun_cl_lits_eq = 
    if not (Clause.has_eq_lit cl)
    then 0 
    else 
      let lits = Clause.get_lits cl in
      List.X.count Term.is_eq_lit lits
  in
  let new_props = {
    (* assigning would be more efficient but could be error prone if new props are added *)
    prob_props with 
    clauses = prob_props.clauses + 1;
    conjectures = if (Clause.is_negated_conjecture cl) then (prob_props.conjectures +1) else prob_props.conjectures;
    epr = if (Clause.is_epr cl) then (prob_props.epr+1) else prob_props.epr;
    horn = if (Clause.is_horn cl) then (prob_props.horn+1) else prob_props.horn;
    ground = if (Clause.is_ground cl) then (prob_props.ground+1) else prob_props.ground;
    unary = if (num_cl_lits = 1) then (prob_props.unary + 1) else prob_props.unary;
    binary = if (num_cl_lits = 2) then (prob_props.binary + 1) else prob_props.binary;
    lits = prob_props.lits + num_cl_lits;
    lits_eq = prob_props.lits_eq + nun_cl_lits_eq;    

    ac_symbols = AC.Table.empty;
  }
  in
  new_props 



let populate_ac_symbols props clause_list = 
  dbg D_trace @@ lazy "populate_ac_symbols begin";
  let r = { props with ac_symbols = AC.Table.(populate empty) clause_list } in
  dbg D_trace @@ lazy "populate_ac_symbols end";
  r



let populate_is_int_rat props clause_list =
  let exists_is_int_rat =
    clause_list |> 
    List.exists (fun c ->
      Clause.get_lits c |>
      List.exists (fun l ->
        let s = Term.lit_get_top_symb l in        
        s == Symbol.symb_is_int_real
      || s == Symbol.symb_is_rat_real
      || s == Symbol.symb_is_int_rat
                  )
)
  in
  if exists_is_int_rat then
    props.has_is_int_rat <- true;
  props
   


let populate_theories props clause_list = 
  dbg D_trace @@ lazy "populate_theories begin";
  List.iter (Theory_db.search Theory_db.builtin (* props.theories *) (Theory_db.get_global_record ())) clause_list;
  dbg D_trace @@ lazy "populate_theories end";
  props



(*------------ finite domains --------*)

(* (x, t, t=x (or x=t)) *)
type fd_lits = (var * term * term) list

 
let fd_split_lits lits = 
  let f (fd_lits,rest) lit = 
    match (term_eq_view_type_term lit) with 
    |Def(Eq_type_term(_eq_type,t,s)) -> 
        if (Term.is_var t)
        then 
          ((Term.get_var t, s, lit)::fd_lits, rest)
        else
          if (Term.is_var s)
            then
            ((Term.get_var s, t, lit)::fd_lits,rest)
            else
            (fd_lits,lit::rest)
    |Undef -> (fd_lits,lit::rest)
  in
  List.fold_left f ([],[]) lits


type fd_cl = 
    {
     mutable fd_cl_pure    : clause list; (* finite domain (for type(x)): x=t1\/..\/x=tn  where t_i is a ground term *)
     mutable fd_cl_pseudo  : clause list; 
                 (* x_i=t1 \/....\/x_i=tn at least one t_i is non-ground and where x_i\not \in Var(ti) *)
     mutable fd_cl_cond        : clause list;  (* C\/D where D is fd_pure *)
     mutable fd_cl_pseudo_cond : clause list;  (* C\/D where D is fd_pseudo *)
   }

let fd_create () = 
  {
     fd_cl_pure         = [];
     fd_cl_pseudo       = [];
     fd_cl_cond         = [];
     fd_cl_pseudo_cond  = [];
 }

let fd_is_pure fd_lits = 
  List.for_all (fun (_v,t,_eq) -> Term.is_ground t) fd_lits 
    
let fd_is_pseudo fd_lits = 
  List.for_all (fun (v,t,_eq) -> not (Term.var_in v t)) fd_lits 

(* returns (fd_clauses, rest) *)
let fd_split clauses = 
  let fd_init = fd_create () in 
  let f (fd,rest_cl) clause = 
    let all_lits = Clause.get_lits clause in
    let (fd_lits,rest_lits) = fd_split_lits all_lits in 
    if (List.X.is_nonempty fd_lits) 
    then
      if (fd_is_pure fd_lits) 
      then 
        if (List.X.is_empty rest_lits)
        then
          ( 
            fd.fd_cl_pure <- clause::fd.fd_cl_pure;
            dbg D_fd @@ lazy (sprintf "fd_cl_pure: %s" (Clause.to_string clause));
            (fd,rest_cl)
           )
        else
          (
           fd.fd_cl_cond <- clause::fd.fd_cl_cond;
           dbg D_fd @@ lazy (sprintf "fd_cl_cond: %s" (Clause.to_string clause));
           (fd,rest_cl)
          )
      else
        (
         if (fd_is_pseudo fd_lits) 
         then 
           if (List.X.is_empty rest_lits)
           then
             ( 
               fd.fd_cl_pseudo <- clause::fd.fd_cl_pseudo;               
               dbg D_fd @@ lazy (sprintf "fd_cl_pseudo: %s" (Clause.to_string clause));
               (fd,rest_cl)
              )
           else
             (
              fd.fd_cl_pseudo_cond <- clause::fd.fd_cl_pseudo_cond;
              dbg D_fd @@ lazy (sprintf "fd_cl_pseudo_cond: %s" (Clause.to_string clause));
              (fd,rest_cl)
             )
         else
           (fd,clause::rest_cl)
        )  
    else
      (fd,clause::rest_cl)
  in
  List.fold_left f (fd_init,[]) clauses 
    

(*------*)
let assign_fd_cl_prob_props prob_prop fd_cl = 
  prob_prop.fd_pure        <- List.length fd_cl.fd_cl_pure;
  prob_prop.fd_pseudo      <- List.length fd_cl.fd_cl_pseudo;
  prob_prop.fd_cond        <- List.length fd_cl.fd_cl_cond;
  prob_prop.fd_pseudo_cond <- List.length fd_cl.fd_cl_pseudo_cond
  
let assign_fd_prob_props prob_prop clauses =
  let (fd_cl,_rest) = fd_split clauses in 
  assign_fd_cl_prob_props prob_prop fd_cl 

(*-----------------*)    
let get_prob_props clause_list =
  List.fold_left add_prob_props (empty_prob_props ()) clause_list
  |> (fun props -> populate_ac_symbols props clause_list) 
  |> (fun props -> populate_is_int_rat props clause_list) 
  (* |> (fun props -> populate_theories props clause_list)  *)
  |> tap (fun props -> assign_fd_prob_props props clause_list) (* TODO clean *)

(*-------------Symbol type check--------------------------*)
(* check if there is only one symbol name for each type  *)

module NameSymMap = Map.Make(String)

(* table from symb numbes to list of symbols with the same name *)

type symb_name_table = (((Symbol.symbol list) ref) NameSymMap.t)

let create_name_symb_table () =
  let f symb symb_table =
    if not (Symbol.is_input symb)
    then symb_table
    else
      (
       let symb_name = (Symbol.get_name symb) in
       try
	 let symb_list_ref = NameSymMap.find symb_name symb_table in
	 symb_list_ref:= symb:: (!symb_list_ref);
	 symb_table
       with
	 Not_found ->
	   NameSymMap.add symb_name (ref [symb]) symb_table
      )
  in
  SymbolDB.fold f !symbol_db_ref NameSymMap.empty

let check_symb_name_table snt =
  let ok = ref true in
  let f sname symb_list_ref =
    if List.compare_length_with !symb_list_ref 1 = Ord.gt then (
      ok := false;
      out_str (sprintf "%sType Check Faild on %s\n" pref_str sname);
      List.iter (fun symb ->
        Symbol.to_stream_full stdout_stream symb;
        out_str "\n"
      ) !symb_list_ref
    )
  in
  NameSymMap.iter f snt;
  (if !ok
  then ()
  else
    failwith "Type Check Faild, see help on option --symbol_type_check"
  )

let symb_type_check () =
  let snt = create_name_symb_table () in
  check_symb_name_table snt

let has_equality clause_list = 
   (!global_options.bmc1_incremental) || (Clause.has_eq_lit_clause_list clause_list) 



(*------Prolific Symbols change for large theories----------------*)

(* If prolific_symb_bound is changed in current_options *)
(* then we need to recalculate which terms/clauses contain prolific symbols *)
let rec change_prolific_symb_input input_clauses =
  let rec change_prolific_symb_term t =
    match t with
    | Term.Fun (symb, args, info) ->
	Term.arg_iter change_prolific_symb_term args;
	Term.assign_has_non_prolific_conj_symb t
    | Term.Var _ -> ()
  in
  let change_prolific_symb_clause c =
    Clause.iter change_prolific_symb_term c;
    Clause.reset_has_non_prolific_conj_symb c
  in
  List.iter change_prolific_symb_clause input_clauses

(*--------- signature map: symb -> cnt -----------*)

type sig_cnt_map = int SMap.t

let get_sym_cnt_cl map clause = 
  let f map_rest symb = SMap.update symb cnt_opt_update map_rest in
  Clause.fold_sym f map clause 


let get_sym_cnt_cl_list' map clause_list = 
  List.fold_left get_sym_cnt_cl map clause_list 
    
let get_sym_cnt_cl_list clause_list = 
  get_sym_cnt_cl_list' SMap.empty clause_list


let out_sig_cnt_map' sig_cnt_map = 
  let f symb cnt = 
    out_str ((Symbol.to_string symb)^" "^(string_of_int cnt))
  in 
  SMap.iter f sig_cnt_map 

let out_sig_cnt_map sig_cnt_map = 
  out_str "";
  out_str (s_pref_str()^" signature count start"); 
  out_sig_cnt_map' sig_cnt_map;
  out_str (s_pref_str()^" signature count end");
  out_str ""

let out_sig_cnt clause_list = 
  let map = get_sym_cnt_cl_list clause_list in 
  out_sig_cnt_map map



(*------ trigger cnt based on sig_cnt                    -------*)
(*------ count in how many clause as sybole is a trigger -------*)

type trigger_cnt_map = int SMap.t

let is_eligible_trig_symb ~elig_sk_split symb =
  if elig_sk_split 
  then true 
  else
    not ((Symbol.is_skolem symb) || (Symbol.is_vamp_split_symbol symb))
    
  
(* returns set of triggers *)    
let trigger_set ~elig_sk_split sig_cnt_map tolerance clause = 
 
  let is_trigger least_symb_occ symb =  
    let symb_cnt = SMap.find symb sig_cnt_map in 
    Float.O.(tolerance *. float_of_int least_symb_occ >= float_of_int symb_cnt)
  in

  let refresh_trigger_set new_least_symb_occ trigger_set = 
    SSet.filter 
      (fun symb -> 
        if (is_trigger new_least_symb_occ symb)
        then 
          true 
        else 
          false
      )
      trigger_set 
  in

  let f (least_symb_occ_opt, trigger_set) symb = 
    if (is_eligible_trig_symb ~elig_sk_split symb) 
    then
      let symb_cnt = SMap.find symb sig_cnt_map in 
      match least_symb_occ_opt with 
      |Some(least_symb_occ) ->
          if (least_symb_occ > symb_cnt) (* new trigger*)
          then 
            let new_least_symb_occ = symb_cnt in 
            let new_trigger_set = SSet.add symb (refresh_trigger_set new_least_symb_occ trigger_set) in 
            (Some(new_least_symb_occ), new_trigger_set)
          else 
            if (is_trigger least_symb_occ symb)
            then 
              (Some(least_symb_occ), (SSet.add symb trigger_set))
            else 
              (Some(least_symb_occ), trigger_set)
      |None -> 
          (Some(symb_cnt), (SSet.add symb trigger_set))
    else (* not eligible symbol *)
      (least_symb_occ_opt, trigger_set)  
  in
  let (_main_trigger_cnt,trigger_set) =  Clause.fold_sym f (None, SSet.empty) clause in 
  trigger_set


(* [tr_set_cl2;..;tr_set_cln]*)

let get_trigger_sets ~elig_sk_split sig_cnt_map tolerance clause_list =
  List.map (trigger_set ~elig_sk_split sig_cnt_map tolerance) clause_list

(* returns map: trigger -> in how many clauses this symb is a trigger *)

let trigger_cnt_map ~elig_sk_split sig_cnt_map tolerance clause_list = 
  let trigger_set_list = get_trigger_sets ~elig_sk_split sig_cnt_map tolerance clause_list in
  let extend_cnt_map_set trigger_set map = 
    let f_set symb map_rest = 
      try 
        let cnt = SMap.find symb map_rest in 
        SMap.add symb (cnt+1) map_rest 
      with 
        Not_found -> 
          SMap.add symb 1 map_rest 
    in
    SSet.fold f_set map trigger_set 
  in
  List.fold_left extend_cnt_map_set SMap.empty trigger_set_list 

let out_trigger_cnt_map trigger_cnt_map = 
  out_str "";
  out_str (s_pref_str()^"  trigger count start"); 
  out_sig_cnt_map' trigger_cnt_map; (* trigger map is also a sig_cnt_map *)
  out_str (s_pref_str()^"  trigger count end");
  out_str ""

let out_trigger_cnt ~elig_sk_split sig_cnt_map tolerance clause_list = 
  let trigger_cnt_map = trigger_cnt_map ~elig_sk_split sig_cnt_map tolerance clause_list in 
  out_trigger_cnt_map trigger_cnt_map


(* out both symbol cnt and triggers so not to recompute symbol cnt *)
let out_sig_trig_cnts opts clauses = 
  if (opts.sig_cnt_out || opts.trig_cnt_out)
  then 
    begin
      let symb_cnt_map = get_sym_cnt_cl_list clauses in
      (if opts.sig_cnt_out (* without trigger count *)
      then
        out_sig_cnt_map symb_cnt_map
      );
      (if opts.trig_cnt_out 
      then 
        out_trigger_cnt symb_cnt_map ~elig_sk_split:opts.trig_cnt_out_sk_spl opts.trig_cnt_out_tolerance clauses
      );
    end
  else ()


    
(*------------ transform clauses into abstract clauses by -----------------------*)
(*------------ collapsing symbols of the same arity (or other properties) -------*)

type abstr_sig_state = 
{
 mutable symb_to_abstr : symbol SMap.t; (* maps symbols to abstr symb *)
 mutable term_to_abstr : term TMap.t; (* maps terms to abstract terms *)  
 mutable clause_to_abstr : clause BCMap.t (* maps clauses to abstract clauses *)  
}

let create_abstr_sig_state () = 
  {
   symb_to_abstr = SMap.empty;
   term_to_abstr = TMap.empty;
   clause_to_abstr = BCMap.empty;
 }
    
let get_abstr_symb state symb = 
  try 
    SMap.find symb state.symb_to_abstr
  with Not_found ->  (* create new abstr symb*)
    let (arg_types, val_type) = Symbol.get_stype_args_val_def symb in
    let nargs = List.length arg_types in  
    let arg_types_str =
      String.concat "_" (List.map (fun stype -> remove_dollars_str (Symbol.to_string stype) ) arg_types) in
    let abstr_name = String.concat "_" [
      "abstr"; string_of_int nargs; arg_types_str; (Symbol.to_string val_type);
    ]
    in
    let stype = Symbol.get_type symb in
    let abstr_symb = create_symbol abstr_name stype in       
    state.symb_to_abstr <- SMap.add symb abstr_symb state.symb_to_abstr;
    abstr_symb



let rec get_abstr_term state term = 
  try 
    TMap.find term state.term_to_abstr
  with 
    Not_found -> 
      begin
        let new_term = 
          match term with
          |Term.Fun (sym, args, _info) ->
              let new_args = Term.arg_map_list (get_abstr_term state) args in
              let new_symb = get_abstr_symb state sym in
              add_fun_term new_symb new_args 
          |Term.Var _ -> term (* do not change var terms*)
        in
        state.term_to_abstr <- TMap.add term new_term state.term_to_abstr; 
        new_term
      end

let get_abstr_clause state clause = 
   try 
     BCMap.find clause state.clause_to_abstr
   with 
     Not_found -> 
       begin
         let lits = Clause.get_lits clause in 
         let abstr_lits_with_repetition  = List.map (get_abstr_term state) lits in                   
         let abstr_lits =  TSet.elements (TSet.of_list abstr_lits_with_repetition) in 
         let tstp_source = Clause.tstp_source_sig_abstr clause in
         let new_clause  = create_clause tstp_source abstr_lits in 
         state.clause_to_abstr <- BCMap.add clause new_clause state.clause_to_abstr;        
         new_clause
       end

let get_abstr_clause_list clause_list =
  let state = create_abstr_sig_state () in
  List.map (get_abstr_clause state) clause_list

(*--- clause count ------*) 

(* map that counts the same clauses in a list *)
type cl_cnt_map = int BCMap.t

let get_cl_cnt_map clause_list =  
  let f map_rest cl = BCMap.update cl cnt_opt_update map_rest in
  List.fold_left f BCMap.empty clause_list 

let out_cl_cnt_map' cl_cnt_map = 
  let f cl cnt = 
    out_str ((Clause.to_string cl)^" "^(string_of_int cnt))
  in 
  BCMap.iter f cl_cnt_map 

(*---- lit counts ----------*)    
type term_cnt_map = int TMap.t

let lit_cnt_map_update map clause = 
  let lits = Clause.get_lits clause in 
  let f map_rest lit = TMap.update lit cnt_opt_update map_rest in
  List.fold_left f map lits


let get_lit_cnt_map clause_list =    
  List.fold_left lit_cnt_map_update TMap.empty clause_list 

let out_term_cnt_map' term_cnt_map = 
  let f term cnt = 
    out_str ((Term.to_string term)^" "^(string_of_int cnt))
  in 
  TMap.iter f term_cnt_map 


(*--- term counts, including lits ----*)

let term_cnt_map_update map term =  
  dbg D_term_cnt_map (lazy ("term_cnt_map_update: "^(Term.to_string term)));
  Term.fold_subterms (fun map_rest sub_term -> TMap.update sub_term cnt_opt_update map_rest) map term 

let term_cnt_map_update_cl map clause = 
  let lits = Clause.get_lits clause in 
  let f map_rest lit =  term_cnt_map_update map_rest lit in
  List.fold_left f map lits
    
let get_term_cnt_map clause_list =     
  List.fold_left term_cnt_map_update_cl TMap.empty clause_list 



(*--------------*)    
let out_abstr_str_start ~str_type ~str_add = 
(*  out_str (s_pref_str()^" abstract clauses "^str^" start"); *)
  out_str (s_pref_str()^" abstract "^str_type^" "^str_add^" start")

let out_abstr_str_end ~str_type ~str_add = 
(*  out_str (s_pref_str()^" abstract clauses "^str^" start"); *)
  out_str (s_pref_str()^" abstract "^str_type^" "^str_add^" end")


let abstr_and_out ~str_add clause_list = 
  let abstr_cl_list_with_repetition = get_abstr_clause_list clause_list in   
  let cl_cnt_map = get_cl_cnt_map abstr_cl_list_with_repetition in
(*  let abstr_cl_list = BCSet.elements (BCSet.of_list abstr_cl_list_with_repetition) in *)
(*  let out_cl clause = out_str (Clause.to_string clause) in  *)

  let lit_cnt_map = get_lit_cnt_map abstr_cl_list_with_repetition in
  let term_cnt_map = get_term_cnt_map abstr_cl_list_with_repetition in

  out_str "";
(*  out_str (s_pref_str()^" abstract clauses "^str^" start"); *)
  out_abstr_str_start ~str_type:"clauses" ~str_add;  
(*  List.iter  out_cl abstr_cl_list; *)
  out_cl_cnt_map' cl_cnt_map;
  out_abstr_str_end ~str_type:"clauses" ~str_add;
  out_abstr_str_start ~str_type:"literals" ~str_add;  
  out_term_cnt_map' lit_cnt_map;
  out_abstr_str_end ~str_type:"literals" ~str_add;  
  out_abstr_str_start ~str_type:"terms" ~str_add;   
  out_term_cnt_map' term_cnt_map;
  out_abstr_str_end ~str_type:"terms" ~str_add;   
  out_str ""
    


let conj_split clause_list = 
(*  let (conj, axs) =  List.partition (fun c ->  (Clause.get_conj_dist c) < Clause.max_conjecture_dist) clause_list in  *)
   let (conj, axs) =  List.partition (fun c ->  Clause.( not (is_inf_conj_dist (get_conj_dist c)))) clause_list in  
  (conj, axs) (* conj includes also clauses that are inferred during preprocessing invling conj *)

let abstr_and_out_conj clause_list = 
  let (conj, axs) = conj_split clause_list in 
  abstr_and_out ~str_add:"conjecture" conj;
  abstr_and_out ~str_add:"axioms" axs
  
