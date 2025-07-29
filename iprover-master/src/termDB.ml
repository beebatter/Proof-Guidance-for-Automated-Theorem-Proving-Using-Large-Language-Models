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
    
type term = Term.term

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr =
  | D_trace
  | D_flip

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_flip -> "flip"

let dbg_groups = [
  D_trace;
  D_flip;
]

let module_name = "termDB"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



    
(*-------------------------Hashtbl-Based ----------*)

let get_hash term = 
  match term with 
  | Term.Fun (symb, args, _) -> 
    let arg_hash = 
      Term.arg_fold_left 
        (fun rest term -> hash_sum rest (Term.get_fast_key term)) 0 args
    in
    hash_sum arg_hash (Symbol.get_fast_key symb)
  | Term.Var (var, _) -> Var.hash var
  
  
module TermKey = struct
  type t = term

  (* (*return true if equal and raise Not_equal otherwise*)
  exception Not_equal *)
  let equal (t1: term) (t2: term) = 
    (t1 == t2) 
    || (
      match t1, t2 with 
      | Term.Fun(symb1, args1, _), Term.Fun(symb2, args2, _) -> 
        if symb1 == symb2 then (
          list_identical_elts (Term.arg_to_list args1) (Term.arg_to_list args2)
          (* try 
            ignore @@ list_find_not_identical 
              (Term.arg_to_list args1)
              (Term.arg_to_list args2) ;
            false
            with 
            | Not_found -> true *)
        ) else (
          false
        )
      | Term.Var(x,_), Term.Var(y,_) -> 
        Var.equal x y
      | _ -> 
        false
    )

  let hash = get_hash

  (* let hash = Term.assign_hash_full (*; Hashtbl.hash t*)*)

  (* let hash = Hashtbl.hash*)
  (* val init_num_of_keys : int*)
end

  (* let hash  =  Term.get_hash *)  



module TermDBM = Hashtbl.Make(TermKey) 
type termDB = term TermDBM.t


(*
module TermDBM = Weak.Make (TermKey)
type termDB = ((*term *) TermDBM.t)
*)

let name_ref = ref ""
let size_ref = ref 0

let create() = TermDBM.create 1000001

let create_name name = 
  name_ref := name; 
  create()

    


(* remove should not be used as can result in reassigning fast_key which will screw up many places *)
let[@deprecated] remove term term_db = 
  failwith "TermDB.remove: deprecated"

    (* (* let _= Term.assign_hash_full term in*)
  TermDBM.remove term_db term; 
  (match term with
  |Term.Fun(_sym, _args,fun_info) ->      
      Term.set_fun_info_bool_param false Term.fun_in_term_db fun_info;
  |Term.Var(_,var_info) -> 
      Term.set_var_info_bool_param false Term.var_in_term_db var_info;
  );
  term_db *)


    
(* not tested *)
(* can raise exception Not_found*)
let find t term_db  = 
  (* let _= Term.assign_hash_full term in*)
  match t with
  | Term.Fun (sym, args, fun_info) ->
    if Term.get_fun_info_bool_param Term.fun_in_term_db fun_info then 
      t
    else
      let new_args = Term.arg_map (fun t' -> TermDBM.find term_db t') args in
      (* If items are identical, then avoid constructing again *)
      let new_term = 
        if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then
          t
        else
          Term.create_fun_term_args sym new_args
      in
      TermDBM.find term_db new_term 

  | Term.Var (_, var_info) ->
    if Term.get_var_info_bool_param Term.var_in_term_db var_info then 
      t
    else
      TermDBM.find term_db t


let mem t term_db   = 
  try 
    ignore @@ find t term_db;
    true
  with Not_found -> false


let size term_db = !size_ref

 
(*let map = TermDBM.map*)

let get_name term_db = !name_ref


let fold f term_db a = 
  (* try *)
  TermDBM.fold (fun _key term a -> (f term a)) term_db a  
  (* with Term.Term_hash_undef -> failwith "termDB: Term_hash_undef1"*)

let iter f term_db  = 
  (* try *)
  (TermDBM.iter (fun _key term -> (f term)) term_db)  
  (* with Term.Term_hash_undef -> failwith "termDB: Term_hash_undef2"*)




(*-----------------------------*)
let to_stream s term_db = 
  s.stream_add_str 
    "\n%----------------------------------------------------------\n";
  s.stream_add_str "%Term DB ";
  s.stream_add_str ":\n";
  TermDBM.iter (fun _key elem -> 
    Term.to_stream s elem;
    s.stream_add_char '\n'
  ) term_db;
  s.stream_add_str 
    "\n%----------------------------------------------------------\n";
  s.stream_add_str "%Size of TermDB ";
  s.stream_add_str " is ";
  s.stream_add_str (string_of_int (size term_db));
  s.stream_add_char '\n'
    
let out = to_stream stdout_stream
    
let to_string = 
  to_string_fun_from_to_stream_fun 10000 to_stream



(*--------------bot up Hashtbl impl-------------------*)

(** Helper function: if term is l≐r and orientable, orient and set param *)
(* let orient_eq_lit = Term.orient_eq_lit (fun x y -> let r = Orderings.simple_kbo x y in r==cequal || r==cequal+1) *)
(* let orient_eq_lit = Term.orient_eq_lit (fun x y -> not @@ Orderings.(x >=! y)) *)
(*let orient_if_eq_lit = Term.orient_if_eq_lit (Orderings_opt.kbo_terms)*)
(* let orient_eq_lit t = 
  dbg D_orient @@ lazy (sprintf "Before orienting: %s" t);
  let t' = Term.orient_eq_lit (fun x y -> Orderings.(x >= y)) t in
  dbg D_orient @@ lazy (sprintf "After orienting: %s" t');
  t' *)

let assign_fast_key_and_add t db_ref = 
  Term.assign_fast_key t !size_ref;
  incr size_ref;
  TermDBM.add !db_ref t t

let add_fun_term t db_ref = 
  Term.assign_fun_all t;
  assign_fast_key_and_add t db_ref;
  Term.set_fun_bool_param true Term.fun_in_term_db t (* not fun_info! *)

let add_var_term t db_ref var_info = 
  Term.assign_var_all t;
  assign_fast_key_and_add t db_ref;
  Term.set_var_info_bool_param true Term.var_in_term_db var_info (* we add the orig var; so var_info OK *)

let rec add_ref t db_ref = 
  (* out_str("----------------\n");
  out_str ("Term to DB: "^(Term.to_string t)^"\n"); *)
  dbg D_trace @@ lazy (sprintf "Adding to termDB: %s" (Term.to_string t));
  match t with
  | Term.Fun (sym, args, fun_info) -> 
    if Term.get_fun_info_bool_param Term.fun_in_term_db fun_info then (
      dbg D_trace @@ lazy (sprintf "already in termDB: %s" (Term.to_string t));
      t
    ) else (
      dbg D_trace @@ lazy (sprintf "not in termDB: %s" (Term.to_string t));
      let new_args = Term.arg_map (fun t' -> add_ref t' db_ref) args in
      let new_term = 
        (* If new_args are the same as args (if they are all in termdb already), avoid creating term again *)
        if List.X.equal ~eq:(==) (Term.arg_to_list args) (Term.arg_to_list new_args) then (
          (* We already know *)
          t
        ) else (
          Term.create_fun_term_args sym new_args (* |> orient_if_eq_lit *)
        )
      in 
      (* add usefull info for terms *)
      (* Term.assign_hash new_term; *)

      try 
        (* don't use just find !*)
        dbg D_trace @@ lazy (sprintf "Finding: %s" (Term.to_string new_term));
        TermDBM.find !db_ref new_term 
      with Not_found -> 
        dbg D_trace @@ lazy (sprintf "Not_found");
        (* Handle equality specially: try to orient, and add both l=r and r=l to the hashtable, pointing to the oriented literal *)
        (* New version: just set flag(s) *)
        (* if !Options.current_options.superposition_flag then (
          Term.assign_oriented Orderings_opt.kbo_terms new_term
         ); *)
        add_fun_term new_term db_ref;
        dbg D_trace @@ lazy (sprintf "Added");
        new_term
    )
    
  | Term.Var (_, var_info) -> 
    if Term.get_var_info_bool_param Term.var_in_term_db var_info then (
      t
    ) else (
      try 
        TermDBM.find !db_ref t 
      with Not_found -> 
        add_var_term t db_ref var_info;
        t
    )



(*
(*---------------------Commented---------------------*)
(*-----------------top down Hashtbl impl-------------------*)



  let rec add_ref t db_ref = 
(*  out_str("----------------\n");
out_str ("Term to DB: "^(Term.to_string t)^"\n");*)
  try 
    find t !db_ref
  with 
    Not_found->
      (match t with
      |Term.Fun(sym, args,info) ->
    (let new_args = Term.arg_map (fun t' -> add_ref t' db_ref) args in
    let new_term = Term.create_fun_term_args sym new_args in 
(* add usefull info for terms*)   
    Term.assign_fun_all new_term;
    Term.assign_fast_key new_term !size_ref;
    size_ref:=!size_ref+1;
    TermDBM.add !db_ref new_term new_term;
    new_term)
      |Term.Var(_,_) ->
    (TermDBM.add !db_ref t t;
    Term.assign_var_all t;
    Term.assign_fast_key t !size_ref;
    size_ref:=!size_ref+1;
    t)
      )


*)



let add t db =
  let db_ref = ref db in
  let _ = add_ref t db_ref in
  !db_ref
 

(*  (try    term in () 
   with Term.Term_hash_undef -> failwith "hash undef here");
*)
(* debug *)
 let get_greatest_key term_db = !size_ref


(* interface that can be opend in other modules *)
module Open =
  struct 
   let add_fun_term term_db_ref symb terms = 
    add_ref (Term.create_fun_term symb terms) term_db_ref

   let add_fun_term_args  term_db_ref symb terms = 
   add_ref (Term.create_fun_term_args symb terms) term_db_ref

   let add_var_term term_db_ref var = 
    add_ref (Term.create_var_term var) term_db_ref
end


(*---------------end HashTbl impl.-------------------------*)




(*
(*------------Commented-----------*)
(*------------------Functional---------------------------*)

(*------------------top down functional----------------*)


module TermKey = 
  struct 
    type t       = term
(* old    let  compare = Term.compare_key*)

(*
    let  compare t s = 
      let ht = Term.assign_hash_full t in
      let hs = Term.assign_hash_full s in
      if ht = hs then
  Term.compare_key t s
      else 
  compare ht hs
    *)
    let  compare t s =  Term.compare_key t s
    let  assign_fast_key = Term.assign_fast_key
  end
    
module TermDBM =  AbstAssignDB.Make (TermKey)
type termDB  = TermDBM.abstDB
      
let create_name = TermDBM.create_name
let create () = create_name "Term_DB"
let mem    = TermDBM.mem   
let remove = TermDBM.remove
let find   = TermDBM.find
let size   = TermDBM.size
let map    = TermDBM.map
let get_name = TermDBM.get_name
let fold = TermDBM.fold
let iter = TermDBM.iter

let to_string term_db = 
  TermDBM.to_string Term.to_string "," term_db




(* add with sharing*)
    
let rec add_ref t db_ref = 
  try find t !db_ref
  with 
    Not_found->
      match t with
      |Term.Fun(sym, args,info) ->
    let new_args = Term.arg_map (fun t' -> add_ref t' db_ref) args in
    let new_term = Term.create_fun_term_args sym new_args in 
(* add usefull info for terms*)   
    let ()= Term.assign_fun_all new_term in
    db_ref:=TermDBM.add new_term !db_ref;
    new_term     
      |Term.Var(_,_) ->
    db_ref:=TermDBM.add t !db_ref;
    Term.assign_var_all t;
    t


*)



(*
(*---------------bottom up functional-------------- *)

module TermKey = 
  struct 
    type t       = term
(* old    let  compare = Term.compare_key*)
(*assume that *)


    let compare t1 t2 = 
      match (t1, t2) with
      | (Term.Fun(sym1,arg1,_),Term.Fun(sym2,arg2,_))
  -> 
    if sym1 == sym2 then
      try 
        let (t1',t2') = 
    list_find_not_identical (Term.arg_to_list arg1) (Term.arg_to_list arg2) in
      Term.compare_fast_key t1' t2'
      with Not_found -> cequal
    else
      Symbol.compare sym1 sym2
      | (Term.Fun _,Term.Var _) -> cless
      | (Term.Var _,Term.Fun _) -> cgreater 
      | (Term.Var(x,_),Term.Var(y,_)) -> Var.compare x y 


(*
    let  compare t s = 
      let ht = Term.assign_hash_full t in
      let hs = Term.assign_hash_full s in
      if ht = hs then
  Term.compare_key t s
      else 
  compare ht hs
*)
    
    let  assign_fast_key = Term.assign_fast_key
  end
    
module TermDBM =  AbstAssignDB.Make (TermKey)
type termDB  = TermDBM.abstDB
      
let create_name = TermDBM.create_name
let create () = create_name "Term_DB"
let mem    = TermDBM.mem   
let remove = TermDBM.remove
let find   = TermDBM.find
let size   = TermDBM.size
let map    = TermDBM.map
let get_name = TermDBM.get_name
let fold = TermDBM.fold
let iter = TermDBM.iter

let to_string term_db = 
  TermDBM.to_string Term.to_string "," term_db





let rec add_ref t db_ref = 
(*  out_str("----------------\n");
  out_str ("Term to DB: "^(Term.to_string t)^"\n");*)
  (match t with
      |Term.Fun(sym, args,info) ->
    (let new_args = Term.arg_map (fun t' -> add_ref t' db_ref) args in
    let new_term = Term.create_fun_term_args sym new_args in 
(* add usefull info for terms*)
(*    Term.assign_hash new_term;*)
    try 
      find new_term !db_ref
    with 
      Not_found->    
        (Term.assign_fun_all new_term;
         db_ref:=TermDBM.add new_term !db_ref;
         new_term)
    )
      |Term.Var _ ->
    ( 
      try 
        find t !db_ref
      with   
        Not_found->    
    (
     db_ref:=TermDBM.add t !db_ref;
     Term.assign_var_all t;
     t)
     )
  )



   
*)
 
(*
let add t db =
  let db_ref = ref db in
  let _ = add_ref t db_ref in
  !db_ref
    
(*debug*)
 let get_greatest_key = TermDBM.get_greatest_key

*)



(* let normalise_eq_lit term db_ref = 
  dbg D_flip @@ lazy (sprintf "normalise_eq_lit %s" (Term.to_string term));

  (* let set_new_term_params lr rl db_ref term =  *)

  (* If an equality, normalise it wrt Term.compare_eq_orient *)
  match Term.Eq.decompose_lit_type term with
  | Some (sign, typ,l,r) -> 
    let l' = add_ref l db_ref in
    let r' = add_ref r db_ref in
    let order = Term.compare_eq_orient l' r' in

    (* If in the correct order already, don't change *)
    if order >= Ord.eq then (
      dbg D_flip @@ lazy "unchanged";
      (* l and r were already in the termdb, so nothing at all to do *)
      if l == l' && r == r' then
        add_ref term db_ref
      (* l and r were not in the termdb, so we must build term' with the new l' and 'r *)
      else
        let term' = Term.create_fun_term Symbol.symb_typed_equality [typ;l';r'] in
        let term' = if sign then term' else Term.create_neg_lit term' in
        add_ref term' db_ref
    ) 

    else (
      dbg D_flip @@ lazy "must flip";
      (* term was already in the termdb *)
      (* if l == l' *)
      (* if Term.get_fun_bool_param Term.fun_in_term_db term then
        (* Must flip and lookup in termdb, but no need to recompute info (in particular, orderings!) *)
        (* TODO *)
        let term' = Term.flip_eq_lit term in
        add_ref term' db_ref
      else *)
      let term' = Term.create_fun_term Symbol.symb_typed_equality [typ;r';l'] in
      let term' = if sign then term' else Term.create_neg_lit term' in
      add_ref term' db_ref
    )
  | None -> add_ref term db_ref *)
