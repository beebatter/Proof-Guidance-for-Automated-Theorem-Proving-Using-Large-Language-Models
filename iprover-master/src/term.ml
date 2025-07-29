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
open Options



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_type_check
  | D_orient

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_type_check -> "type_check"
  | D_orient -> "orient"

let dbg_groups = [
  (* D_trace; *)
  (* D_type_check; *)
  D_orient;
]
    
let module_name = "term"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)


module VSet = Var.VSet
module SMap = Symbol.Map
module IntPairSet = Set.Make(struct type t = int * int let compare = Pair.compare Int.compare Int.compare end)

type var = Var.var
type symbol = Symbol.symbol

type fast_key = int
let no_fast_key = -1

type prop_key = TableArray.key

(*association list for counting the number of occurences of vars in a term*)
type var_ass_list = var num_ass_list

type fun_term_bool_param = int
type var_term_bool_param = int

let has_conj_symb = 0
let has_non_prolific_conj_symb = 1
let has_bound_constant = 2 (* used for BMC *)
let fun_in_term_db = 3 (* used in termDB *)
let var_in_term_db = 3 (* can be ok to repeat *)
(* let is_oriented_equality = 3 *)
(* let is_oriented_lr = 104 *)
(* let is_oriented_rl = 105 *)

type is_oriented = LeftGreater | RightGreater | NotOriented



type args = term list

and term =
  | Fun of symbol * args * fun_info
  | Var of var * var_info

and fun_info = {
  mutable fun_fast_key : fast_key;
  mutable fun_bool_param : Bit_vec.bit_vec;
  (* mutable fun_db_id : fast_key;*)
  mutable num_of_symb : int param;
  (* mutable var_list   : var_list param; *)
  (* can be very expensive and is used only in resol. for kbo, *)
  (* rewrite it there...*)
  mutable num_of_var : int param;   (* cheap *)

  mutable grounding : grounding param; (* Undef for ground terms; we need only prop_key *)

  (* mutable grounding_info : grounding_info param;*)

  (* grounding can change e.g. in finite models or bmc1 *)
  (* mutable fun_grounded : term param;*) (* term obtained after grounding *)

  (* mutable grounding_subst_id : int param;*) (* id of the the grounding substitution *)

  (* prop. key of term after grounding *)
  (* mutable prop_gr_key : prop_key param; *)

  (* prop. key of term (without grounding) used for simplifiactions *)
  (* mutable prop_key : prop_key param;*)

  (* prop. key of term (without grounding) used for simplifiactions or key of the ground term*)
  mutable prop_key : prop_key param;
  (* mutable fun_hash : int param *)

  (* Caching ordering results *)
  (* mutable cache_orderings : term option list; *)
  (* mutable orderings_cache : is_oriented IntMap.t; *)
  mutable orderings_cache : PartialOrd.t IntMap.t;  (* TODO: change to array *)

  (* Flat list of subterms *)
  (* This is cached: it starts at None, and upon being requested it will be memoised in this field *)
  mutable ac_subterms : term list option;
  
  (* Term is ac normalised wrt orderings with uid in this set *)
  mutable is_ac_normalised : IntPairSet.t;  (* TODO: change to array *)
}

and var_info = {
  mutable var_fast_key : fast_key;
  (* mutable var_db_id : fast_key; *)
  mutable var_grounding : grounding param;
  (* TODO: add bool_param and in_term_db *)
  mutable var_bool_param :  Bit_vec.bit_vec;
  (*
  (* grounding can change e.g. in finite models or bmc1 *)
  (* mutable var_hash : int param; *)
  mutable var_grounded : term param; (* term obtained after grounding*)
  mutable var_grounding_subst_id : int param; (* id of the the grounding substitution *)
  *)
}

and grounding =  {
  (* id of grounding sustitution that was used to obtain grounded_term *)
  mutable grounding_subst_id : int; 

  (* grounding can change e.g. in finite models or bmc1 *)
  mutable grounded_term : term; (* term obtained after grounding *) 
}

(*
(* when cluase is ground we only store prop_key and when it is non_ground we store current grounded term with the corresponding subst_id *)
and grounding_info =  Grounding_non_gr of grounding_non_gr | Ground (* of prop_key *)
*)

(* 
   mutable prop_gr_key : prop_key; (* prop_gr_key = prop_gr_key of grounded_term *)
  *)  


let empty_fun_info () = {
  fun_fast_key = no_fast_key;
  fun_bool_param = Bit_vec.false_vec;
  (* fun_db_id = Undef; *)
  num_of_symb = Undef;
  num_of_var = Undef;
  grounding = Undef;
  prop_key = Undef;
  (* prop_gr_key = Undef;*)
  (* fun_hash = Undef;*)
  orderings_cache = IntMap.empty;
  ac_subterms = None;
  is_ac_normalised = IntPairSet.empty;
}

let empty_var_info () = {
  var_fast_key = no_fast_key;
  var_bool_param = Bit_vec.false_vec;
  (* var_db_id = Undef; *)
  (* var_hash = Undef;*)
  var_grounding = Undef;
}

let get_fun_info term = 
  match term with
  | Fun (_, _, info) -> info
  | Var _ -> invalid_arg "Term.get_fun_info: given a var term"

let get_var_info term = 
  match term with
  | Var (_, info) -> info
  | Fun _ -> invalid_arg "Term.get_var_info: given a fun term"

exception Grounding_undef

(*
let get_grounding_info t = 
  match t with 
  |Fun (_,_, f_info) -> f_info.grounding_info 
  |Var (_,v_info) -> v_info.var_grounding_info
*)

let get_grounding t = 
  match t with 
  | Fun (_, _, {grounding = Def(grounding)}) -> grounding 
  | Var (_, {var_grounding = Def(grounding)}) -> grounding 
  | _ -> raise Grounding_undef

exception Num_of_var_undef
let get_num_of_var term =
  match term with
  | Fun(_, _, {num_of_var = Def(num)}) -> num
  | Var _ -> 1
  | _ -> raise Num_of_var_undef

let is_ground t = 
  get_num_of_var t = 0

let assign_grounding grounding term =
  assert (not (is_ground term));
  match term with
  | Fun(_, _, fun_info) -> (
    (*out_str ("ass grounding: "^(to_string term)^"\n");*)
    fun_info.grounding <- Def(grounding)
  ) | Var(_, var_info) ->
    var_info.var_grounding <- Def(grounding)

(*
(* assert that term is non-ground *)
let assign_grounding grounding_subst_id grounded_term t = 
  assert (not (is_ground t));

  let grounding =
   Def(      
       {
  grounding_subst_id = grounding_subst_id;
  grounded_term = grounded_term;
      }
   )
       
  in
  match t with 
  |Fun(_,_, f_info) -> f_info.grounding <- grounding 
  |Var(_, v_info)   -> v_info.var_grounding <- grounding

*)


(*
let assign_grounding_info grounding_info term =
  match term with
  | Fun(_, _, fun_info) ->
      ((*out_str ("ass grounding: "^(to_string term)^"\n");*)
  fun_info.grounding_info <- Def(grounding_info)
      )
  | Var(_, var_info) ->
      var_info.var_grounding_info <- Def(grounding_info)
*)



(* easy to reimpliment args as arrays*)

(*------- fun bool params--------*)
let set_fun_info_bool_param value param fun_info =
  fun_info.fun_bool_param <- Bit_vec.set value param fun_info.fun_bool_param

let get_fun_info_bool_param param fun_info =
  Bit_vec.get param fun_info.fun_bool_param

let set_fun_bool_param value param term =
  match term with
  | Fun(_, _, fun_info) ->
      fun_info.fun_bool_param <- Bit_vec.set value param fun_info.fun_bool_param
  | Var _ -> failwith "term set_fun_bool_param: it's a variable"

let get_fun_bool_param param term =
  match term with
  | Fun(_, _, fun_info) ->
      Bit_vec.get param fun_info.fun_bool_param
  | Var _ -> failwith "term get_fun_bool_param: it's a variable"


(*------- var bool params--------*)
let set_var_info_bool_param value param var_info =
  var_info.var_bool_param <- Bit_vec.set value param var_info.var_bool_param

let get_var_info_bool_param param var_info =
  Bit_vec.get param var_info.var_bool_param

let set_var_bool_param value param term =
  match term with
  | Fun _ ->
      failwith "term set_var_bool_param: this is not a var term"
  | Var(_,var_info) -> 
      var_info.var_bool_param <- Bit_vec.set value param var_info.var_bool_param


let get_var_bool_param param term =
  match term with
  | Fun _ ->
      failwith "term set_var_bool_param: this is not a var term"
  | Var(_,var_info) -> 
      Bit_vec.get param var_info.var_bool_param



type atom = term
type lit = term
type literal = lit

(* type term = {t : pure_term; info : info} *)

(* exception Term_fast_key_undef *)
(* exception Term_db_id_undef *)
(* exception Term_weight_undef *)
(* exception Term_grounding_undef of term *)
exception Term_assign_fkey_to_var

let create_var_term var = Var(var, (empty_var_info ()))
let create_var_term_info var var_info = Var(var, var_info)

exception Num_of_sym_undef

(*
exception Prop_gr_key_undef
let get_prop_gr_key term =
  match term with
  | Fun(_, _,{ prop_gr_key = Def(key) }) -> key
  | _ -> raise Prop_gr_key_undef
*)

exception Prop_key_undef
let get_prop_key term =
  match term with
  | Fun(_, _,{ prop_key = Def(key) }) -> key
  | _ -> raise Prop_key_undef



let get_orderings_cache_info key info =
  IntMap.find key info.orderings_cache

let set_orderings_cache_info key value info =
  info.orderings_cache <- IntMap.add key value info.orderings_cache

let get_ac_subterms_info info =
  info.ac_subterms

let set_ac_subterms_info value info =
  info.ac_subterms <- Some value

let get_is_ac_normalised_info info uid ops =
  info.is_ac_normalised |> IntPairSet.mem (uid, SMap.cardinal ops)

let set_is_ac_normalised_info info uid ops =
  info.is_ac_normalised <- info.is_ac_normalised |> IntPairSet.add (uid, SMap.cardinal ops)



(* can raise Grounding_undef *) 

let rec get_prop_gr_key t = 
  if is_ground t then 
    get_prop_key t
  else
    let grounding = get_grounding t in 
    get_prop_key grounding.grounded_term

(*
  let grounding_info = get_grounding_info t in 
  match grounding_info with 
  | Grounding_non_gr grounding_non_gr -> 
     get_prop_gr_key grounding_non_gr.grounded_term 
  | Ground -> grounding_info.prop_gr_key
*)


(*  can raise Grounding_info_undef/Undef_param   *)
(* exception Prop_key_undef *)
(* let get_prop_key t = *)
(*   let gr_info = get_grounding_info_def t in  *)
(*   match gr_info.prop_key with  *)
(*   |Def(prop_key) -> prop_key  *)
(*   |Undef -> raise Prop_key_undef *)


(*
  exception Term_hash_undef
  exception Term_var_hash_undef
  let get_hash (t: term) =
  match t with
  | Fun(_, _, fun_info) ->
  (
  match fun_info.fun_hash with
  | Def(hash) -> hash
  | _ -> raise Term_hash_undef
  )
  | Var(_, var_info) ->
  (
  match var_info.var_hash with
  | Def(hash) -> hash
  | _ -> raise Term_var_hash_undef
  )
 *)

(*-------------------to string's-------------------*)

(*val to_stream             : 'a string_stream -> symbol -> unit*)

(*
let var_int_to_stream s (v, int) =
  s.stream_add_char '(';
  Var.to_stream s v;
  s.stream_add_char ',';
  s.stream_add_str (string_of_int int);
  s.stream_add_char ')'

(*
  let var_int_to_string (v, int) =
  "("^(Var.to_string v)^","^(string_of_int int)^")"
 *)

let var_list_to_stream s vl =
  s.stream_add_char '[';
  list_to_stream s var_int_to_stream vl ",";
  s.stream_add_char ']'
*)
(*
  let var_list_to_string v_l =
  "["^(list_to_string var_int_to_string v_l ",")^"]"
 *)

let rec to_stream s t =
  match t with
  | Fun(sym, [], _) -> 
    Symbol.to_stream s sym
  | Fun(sym, args, _) -> (
    Symbol.to_stream s sym;
    s.stream_add_char '(';
    list_to_stream s to_stream args ",";
    s.stream_add_char ')'
  )
  | Var(v, _) -> Var.to_stream s v

let rec to_stream_tptp stream t =
  match t with
  | Fun (sym, [], _) -> 
    Symbol.to_stream stream sym

  | Fun (sym, [Fun(t, [_; l; r], _)], _)
  when sym == Symbol.symb_neg
  && t == Symbol.symb_typed_equality ->
    to_stream_tptp stream l;
    stream.stream_add_str " != ";
    to_stream_tptp stream r;

  (* Typed equation as = *)
  | Fun(sym, [_; l; r], _)
    when sym == Symbol.symb_typed_equality ->
      to_stream_tptp stream l;
      stream.stream_add_str " = ";
      to_stream_tptp stream r;

  (* Negation as ~P without parentheses *)
  | Fun (sym, [arg], _) when sym == Symbol.symb_neg ->
    stream.stream_add_char '~';
    to_stream_tptp stream arg;
  
  | Fun(sym, args, _) -> (
    (Symbol.to_stream stream sym);
    stream.stream_add_char '(';
    list_to_stream stream to_stream args ",";
    stream.stream_add_char ')'
  )

  | Var(v, _) -> 
    Var.to_stream stream v


let rec pp_term ppf = function
  | Fun (sym, [], _) ->
    Format.fprintf ppf "%a" 
      Symbol.pp_symbol sym

  | Fun (sym, args, _) ->
    Format.fprintf ppf "%a(%a)"
      Symbol.pp_symbol sym
      (pp_any_list pp_term ",") args

  | Var (v, _) ->
    Format.fprintf ppf "%a" 
      Var.pp_var v


let rec pp_term_tptp ppf = function
    
  (* Nullary symbol without arguments *)
  | Fun (sym, [], _) ->
    Format.fprintf ppf "%a" Symbol.pp_symbol_tptp sym
  
  (* Negated typed equation as != *)
  | Fun (s, [Fun(t, [_; l; r], _)], _)
  when s == Symbol.symb_neg 
  && t == Symbol.symb_typed_equality ->
    Format.fprintf ppf "%a != %a"
      pp_term_tptp l
      pp_term_tptp r
      (*
        (* Negated untyped equation as != *)
        | Fun (s, [Fun(t, [eq_type;l; r], _)], _)
        when s == Symbol.symb_neg &&
        t == Symbol.symb_equality ->
        Format.fprintf
        ppf
        "%a != %a"
        pp_term_tptp l
        pp_term_tptp r
       *)
    
  (* Negation as ~P without parentheses *)
  | Fun (s, [arg], _) when s == Symbol.symb_neg ->
    Format.fprintf ppf "~%a"
      pp_term_tptp arg
  
  (* Non-unary negation *)
  | Fun (s, _, _) when s == Symbol.symb_neg ->
    failwith "Term.pp_term_tptp: Non-unary negation"

(*  
  (* Untyped equation as = *)
  | Fun(s, [_; l; r], _)
    when s == Symbol.symb_typed_equality ->
      Format.fprintf
  ppf
  "%a = %a"
  pp_term_tptp l
  pp_term_tptp r
*)  

  (* Typed equation as = *)
  | Fun(s, [_; l; r], _)
  when s == Symbol.symb_typed_equality ->
    Format.fprintf ppf "%a = %a"
      pp_term_tptp l
      pp_term_tptp r
  
  (* Non-nullary symbol as s(_) *)
  | Fun (sym, args, _) ->
    Format.fprintf ppf "%a(%a)"
      Symbol.pp_symbol_tptp sym
      (pp_any_list pp_term_tptp ",") args
  
  (* Variable *)
  | Var (v, _) ->
    Format.fprintf ppf "%a" Var.pp_var v

let out = 
  if !global_options.tptp_safe_out then
    to_stream_tptp stdout_stream
  else
    to_stream stdout_stream

(*
  let to_string t =
  let s = create_buffer_stream 10 in
  to_stream s t;
  to_string_buffer_stream s
 *)

let to_string =
  if !global_options.tptp_safe_out then 
    to_string_fun_from_to_stream_fun 10 to_stream_tptp
  else
    to_string_fun_from_to_stream_fun 10 to_stream


(*
  let rec to_string_term t =
  match t with
  | Fun(sym,[], _) -> Symbol.to_string sym
  | Fun(sym, args, _) ->
  (Symbol.to_string sym)^"("^(list_to_string to_string_term args ",")^")"
  | Var(v, _) -> Var.to_string v

 *)

(*
  let to_string_with_var_list t =
  (to_string_term t)^"-"^(var_list_to_string (get_var_list t))
 *)

(*
  let to_string_with_num_of_sym t =
  (to_string_term t)^"-num_of_symb -"^(string_of_int (get_num_of_symb t))
 *)

(* let to_string = to_string_with_var_list *)

let term_list_to_stream s t_list =
  list_to_stream s to_stream t_list ";"

let out_term_list = term_list_to_stream stdout_stream

(*
  let term_list_to_string =
 *)

let term_list_to_string =
  to_string_fun_from_to_stream_fun 30 term_list_to_stream

(*
  let term_list_to_string t_list =
  let s = create_buffer_stream 30 in
  term_list_to_stream s t_list;
  to_string_buffer_stream s
 *)

(*
  let term_list_to_string t_list =
  list_to_string to_string t_list ";"
 *)

(*-----------------------end to_strings------------------------*)
(* type_check checks that val types of termlist respects types of symb *)
(* type_check is used for debugging *)


exception Var_term

let get_atom literal =
  match literal with
  | Fun (sym, [t], _) when sym == Symbol.symb_neg ->
    t
  | _ ->
    literal


let is_neg_lit lit =
  match lit with
  | Fun (s, _, _) -> s == Symbol.symb_neg
  | _ -> false

let is_pos_lit lit = 
  not (is_neg_lit lit)

let get_sign_lit lit = 
  is_pos_lit lit

let split_sign_lit lit = 
  (* (get_sign_lit lit, get_atom lit) *)
  match lit with
  | Fun (sym, [t], _) when sym == Symbol.symb_neg ->
    (false, t)
  | Fun _ ->
    (true, lit)
  | Var _ ->
    (* failwith "split_sign: is a variable" *)
    (true, lit)


let is_complementary l1 l2 =  (* andrepd: can be simplified, but unnecessary *)
  match l1 with
  | Fun(sym1, args1, _) ->
    if sym1 == Symbol.symb_neg then
      if l2 == (List.hd args1) then
        true
      else 
        false
    else ( (*l1 is positive*)
      match l2 with
      | Fun(sym2, args2, _) ->
        if sym2 == Symbol.symb_neg then
          if l1 == (List.hd args2) then
            true
          else 
            false
        else 
          false
      | _ -> failwith "term: is_compl it is not a literal"
    )
  | _ -> failwith "term: is_compl it is not a literal"

(* works but got rid of var_list
   exception Var_list_is_undef
   let get_var_list term =
   match term with
   | Fun(_, _, fun_info) ->
   ( match fun_info.var_list with
   | Def (vl) -> vl
   | Undef -> raise Var_list_is_undef
   )
   | Var(v, _) -> [(v,1)]

 *)

(* assume that term is a Var term*)

let get_var var_term =
  match var_term with
  | Var (v, _) -> v
  | _ -> failwith "term: get_var not a Var term"  (* andrepd: replace with a Not_var_term? *)

let get_top_symb t =
  match t with
  | Fun (sym, _, _) -> sym
  | _ -> raise Var_term

let lit_get_top_symb l =
  get_top_symb (get_atom l)

let get_args t =
  match t with
  | Fun (_, args, _) -> args
  | _ -> failwith "term: get_args is a var"  (* andrepd: replace with Var_term? *)

let rec var_in var term =
  match term with
  | Fun (_, args, _) ->
    List.exists (var_in var) args
  | Var (v, _) -> (Var.equal var v)

let get_term_type t =
  match t with
  | Fun(symb, _args, _inf) -> Symbol.get_val_type_def symb
  | Var(v, _) ->  Var.get_type v


(*--------- type check --------*)
let type_check_fail symb termlist = 
  try
    begin
      match (Symbol.get_stype_args_val symb) with 
      | Def((sym_arg_types,_val_type)) -> 
    let f symb_arg_type arg = 
      let arg_type = get_term_type arg in  
      if (Symbol.is_subtype arg_type symb_arg_type)
      then ()
      else 
        begin  
          dbg D_type_check (lazy (" type_check 1 "));
          dbg D_type_check (lazy ( 
          ("term: type_check failed, symbol "
           ^(Symbol.to_string symb)^" has an arg with type "^(Symbol.to_string symb_arg_type)
           ^" which is not a supertype of substituted term "^(to_string arg)
           ^" of type "^(Symbol.to_string arg_type) 
          )));
    
    failwith 
      ("term: type_check failed, symbol "
       ^(Symbol.to_string symb)^" has an arg with type "^(Symbol.to_string symb_arg_type)
       ^" which is not a supertype of substituted term "^(to_string arg)
       ^" of type "^(Symbol.to_string arg_type) 
      )
        end
    in 
    (
     try
       List.iter2 f sym_arg_types termlist
     with  
     |Invalid_argument _ ->
         begin     
     dbg D_type_check (lazy (" type_check 2 "));
     dbg D_type_check (lazy ( 
                 ("term: type_check failed, symbol "^(Symbol.to_string symb)
            ^" has arity "^(string_of_int (List.length sym_arg_types))
            ^" but the list of substituted arguments "
            ^(term_list_to_string termlist)
            ^" has length "^(string_of_int (List.length termlist)))    
          ));
     failwith ("term: type_check failed, symbol "^(Symbol.to_string symb)
         ^" has arity "^(string_of_int (List.length sym_arg_types))
         ^" but the list of substituted arguments "
         ^(term_list_to_string termlist)
         ^" has length "^(string_of_int (List.length termlist)))
         end
    )
      | Undef -> ()
    end
  with 
    x ->   
      (
       dbg D_type_check (lazy (" type_check 3 "));
       dbg D_type_check (lazy ("Exception: "^(Printexc.to_string x)));
       raise_trace x)



(* let _ = out_str "\n\n Term.symbol_type_check debug \n\n "*)

let create_fun_term symbol term_list =
  let fun_info = empty_fun_info() in

  if !global_options.symbol_type_check then 
    type_check_fail symbol term_list
  ;

  Fun (symbol, term_list, fun_info)

(* Convenient *)
let create_neg_lit atom = 
  match atom with
  | Fun (_, _, inner_info) ->
    let symb = Symbol.symb_neg in
    let args = [atom] in
    let fun_info = empty_fun_info() in
    if !global_options.symbol_type_check then (
      type_check_fail symb args
    );
    Fun (symb, args, fun_info)
  | Var _ -> invalid_arg "Term.create_neg_lit"




(*---------*)
(*
let get_term_type t =
  match t with
  | Fun(symb, _args, _inf) -> Symbol.get_val_type symb
  | Var(v, _) ->  Def(Var.get_type v)
*)

exception Type_check_failed
let rec type_check ?(allow_sub_types = true) term = 
  match term with 
  | Var (_, _) -> ()
  | Fun (symb, args, _) -> 
      begin        
        if (symb == Symbol.symb_typed_equality)
        then    
          let (type_term_eq, t, s) = get_triple_from_list args in 
    let eq_v_type =
      try
        get_top_symb type_term_eq
      with 
              Var_term -> 
                raise Type_check_failed
          in
          let t_type = (get_term_type t) in 
          let s_type = (get_term_type s) in 
          if (t_type == s_type) && (t_type == eq_v_type)
          then 
            (type_check ~allow_sub_types t; type_check ~allow_sub_types s)
          else 
            (raise Type_check_failed)
        else (* not equality *)
          begin
            (match (Symbol.get_stype_args_val symb) with 
            | Def((sym_arg_types,_val_type)) -> 
                let f symb_arg_type arg = 
            let arg_type = get_term_type arg in  
                  let are_compatible_types = 
                    if allow_sub_types
                    then
                      (Symbol.is_subtype arg_type symb_arg_type)
                    else
                      arg_type == symb_arg_type
       (*     if (Symbol.is_subtype arg_type symb_arg_type)*)
                  in
                  if are_compatible_types
            then ()
            else 
                    (raise Type_check_failed)
                in
                (try
            List.iter2 f sym_arg_types args
          with  
          |Invalid_argument _ -> 
              (raise Type_check_failed)
          )    
            |Undef -> ()
            );
            List.iter (type_check ~allow_sub_types) args
          end
      end

let is_well_typed_term ?(allow_sub_types = true) term = 
  try 
    type_check ~allow_sub_types term;
    true
  with 
    Type_check_failed -> false


(*------------------------------------------*)
(* checkes compatibility of value types: for now Symbol.symb_type_types and Symbol.symb_bot_type can unify with any other types *)

let compatible_val_types t s = 
  Symbol.compatible_basic_types (get_term_type t) (get_term_type s)

(*------------------------------------------*)
let create_fun_term_args = create_fun_term

let create_fun_term_info symbol term_list fun_info = Fun (symbol, term_list, fun_info)

let is_constant t =
  match t with
  | Fun (symb, [], _) -> Symbol.is_constant symb
  | _ -> false

let is_const_term t =
  match t with
  | Fun (_, [], _) -> true
  | _ -> false


(*
(* assert that term is ground *)
let assign_prop_gr_key prop_key ground_term =
  assert(is_ground ground_term);
  match ground_term with
  | Fun(_, _, info) -> 
      let grounding_info = 
  Def (
  Grounding_gr_key (prop_key)
       ) in
      info.grounding_info <- grounding_info
  | _ -> failwith "Prop_gr_key can not be assigned to vars"
*)


let assign_prop_key key term =
  match term with
  | Fun (_, _, info) -> 
    info.prop_key <- Def(key)
  | _ -> 
    failwith "Prop_key can not be assigned to vars"





(*
(* Get variables in term recursively *)
  let rec get_vars' accum = function
  
  (* No more subterms to recurse into *)
  | [] -> accum
  
  (* Term is a functional term *)
  | Fun(_, args, _) :: tl -> get_vars' accum (args @ tl)
  
  (* Term is a variable that has already been seen *)
  | Var (v, _) :: tl when List.mem v accum -> get_vars' accum tl
  
  (* Term is a variable that has not been seen *)
  | Var (v, _) :: tl -> get_vars' (v :: accum) tl

(* Get all variables occurring in term *)
  let get_vars term = get_vars' [] [term]
 *)


let compl_lit literal =
  match literal with
  | Fun(sym, args, _) ->
    if sym == Symbol.symb_neg then 
      List.hd args  (* andrepd: may fail, uncaught *)
    else 
      create_fun_term Symbol.symb_neg [literal]
  | _ -> failwith "term: compl_lit it is not a literal"


(* not efficient*)  (* andrepd: Use imperative map instead *)
let rec get_var_ass_list term =
  match term with
  | Fun (_, args, _) ->
      let f rest term = append_ass_list (+) (get_var_ass_list term) rest in
      List.fold_left f [] args
  | Var (v, _) -> [(v,1)]


let rec add_var_set vset term = 
  match term with
  | Fun (_, args, _) ->
      List.fold_left add_var_set vset args
  | Var (v, _) ->       
      VSet.add v vset
  
let get_var_set term = 
  add_var_set (VSet.empty) term

let get_vars term = 
  let vset = get_var_set term in
  VSet.elements vset

(* vars of t are subset of vars s *)
let var_subset t s = 
 VSet.subset (get_var_set t) (get_var_set s)

(*e.g. has_conj_symb, has_bound_constant *)


let get_bool_param_if_fun bool_param t =
  match t with
  | Fun (_, _, fun_info) -> 
    get_fun_info_bool_param bool_param fun_info
  | Var _ -> false


(* term with such bool_param true exists in the list *)
let get_fun_bool_param_term_list bool_param tlist =
  List.exists (get_bool_param_if_fun bool_param) tlist

(* let has_conj_symb_fun term =
   get_fun_term_bool_param has_conj_symb term

   let has_non_prolific_conj_symb_fun term =
   get_fun_term_bool_param has_non_prolific_conj_symb term

   let has_bound_constant_fun term =
   get_fun_term_bool_param has_bound_constant term
 *)

let get_num_of_symb term =
  match term with
  | Fun(_, _, {num_of_symb = Def(num)}) -> num
  | Var _ -> 1
  | _ -> raise Num_of_sym_undef

let get_num_of_symb_skipneg term =
  (* Calculate weight of atom *)
  let atom = get_atom term in
  let num = get_num_of_symb atom in
  (* Also for predicate equalities [p = Top], consider just weight of [p] *)
  let num = 
    match atom with
    | Fun (sym, [_;_;Fun(sym_r, [], _)], _) 
      when sym == Symbol.symb_typed_equality 
      && sym_r == Symbol.symb_top ->
      num - 2
    | _ -> 
      num
  in
  num

let to_stream_with_num_of_sym s t =
  to_stream s t;
  s.stream_add_str "-num_of_symb -";
  s.stream_add_str (string_of_int (get_num_of_symb t))

let get_num_of_symb_term_list term_list =
  let f rest term = rest + (get_num_of_symb term) in
  List.fold_left f 0 term_list

let get_num_of_var_term_list term_list =
  let f rest term = rest + (get_num_of_var term) in
  List.fold_left f 0 term_list

(* can be expensive since we do not store term depth *)
(* term depth of vars/constants is 1 *)
let rec get_term_depth_list term_list = 
  let f = fun current_max term ->
    let new_max = get_term_depth term in 
    if current_max >= new_max then 
      current_max 
    else 
      new_max
  in
  List.fold_left f 0 term_list 

and get_term_depth term = 
  match term with 
  | Fun (_, args, _) -> 
    let args_depth = get_term_depth_list args in 
    args_depth + 1
  | Var _ -> 1


  
(*
  let get_has_conj_symb_term_list term_list =
  List.exists has_conj_symb_fun term_list

  let get_has_non_prolific_conj_symb_term_list term_list =
  List.exists has_non_prolific_conj_symb_fun term_list

  let get_has_bound_constant_term_list term_list =
  List.exists has_bound_constant_fun term_list
 *)

(*
  let hash_comb rest term_hash =
(* abs((rest lsl 1) lxor term_hash)*)
  ((rest lsl 3) + term_hash) * 131
(* (rest + term_hash) * 131*)
(*  abs((rest lsl 3) + term_hash)*)
 *)

(*
  let get_hash_term_list init_hash term_list =
  let hash_fun rest term =
  let term_hash = get_hash term in
  hash_comb rest term_hash in
  List.fold_left hash_fun init_hash term_list
 *)


(*
let assign_grounding ground_term term =
  match term with
  | Fun(_, _, fun_info) ->
      ((*out_str ("ass grounding: "^(to_string term)^"\n");*)
  fun_info.fun_grounded <- Def(ground_term)
      )
  | Var(_, var_info) ->
      var_info.var_grounded <- Def(ground_term)
*)

(*
let assign_grounding grounding term =
  match term with
  | Fun(_, _, fun_info) ->
      ((*out_str ("ass grounding: "^(to_string term)^"\n");*)
  fun_info.grounding <- Def(grounding)
      )
  | Var(_, var_info) ->
      var_info.var_grounding <- Def(grounding)
*)
(*
let get_grounding term =
  if is_ground term 
  then term 
  else
    (
    let gr_info = get_grounding_def term in 
    gr_info.grounded_term
    )
*)

(* for literals use get_grounding_lit in place of get_grounded_term see below *)
let get_grounded_term term =
  if is_ground term then 
    term 
  else 
    let grounding = get_grounding term in 
    grounding.grounded_term


(* only atoms get assigned groundings *)
(* the ground literal needs to be added to term_db separately *)
let get_grounding_lit lit =
  if is_neg_lit lit then
    let atom = get_atom lit in
    let atom_gr = get_grounded_term atom in
    compl_lit atom_gr
  else
    get_grounded_term lit

(* we assume that all subterms of the term for assingments *)
(* have everything defined and parameters of the term itself are not!*)

let assign_num_of_symb term =
  match term with
  | Fun (_, args, fun_info) ->
      (* out_str("Num Symb: "^(string_of_int ((get_num_of_symb_term_list args)+1))^"\n"); *)
      fun_info.num_of_symb <- Def (get_num_of_symb_term_list args + 1)
  | _ -> ()

let assign_num_of_var term =
  match term with
  | Fun (_, args, fun_info) ->
      fun_info.num_of_var <- Def (get_num_of_var_term_list args)
  | _ -> ()

(*
  let assign_has_conj_symb term =
  match term with
  | Fun(symb, args, fun_info) ->
  let has_conj = ((Symbol.get_bool_param Symbol.is_conj_symb symb)
  || (get_has_conj_symb_term_list args)) in
  set_fun_info_bool_param has_conj has_conj_symb fun_info
  | _ -> ()
 *)

(* assign term_bool_param if there exists symbol with *)
(* corresponding symbol_bool_param in the term        *)

let assign_symbol_bool_param_term symb_bool_param term_bool_param term =
  match term with
  | Fun(symb, args, fun_info) ->
    let bool_param_val = 
      Symbol.get_bool_param symb_bool_param symb
      || get_fun_bool_param_term_list term_bool_param args 
    in
    set_fun_info_bool_param bool_param_val term_bool_param fun_info
  | _ -> ()

let assign_has_non_prolific_conj_symb term =
  match term with
  | Fun (symb, args, fun_info) ->
    let num_of_occ_in_input = (Symbol.get_num_input_occur symb) in
    let has_non_prolific_conj =
    Symbol.get_bool_param Symbol.is_conj_symb symb
    && num_of_occ_in_input > 0
    && num_of_occ_in_input < !global_options.prolific_symb_bound
    || get_fun_bool_param_term_list has_non_prolific_conj_symb args
    in
    set_fun_info_bool_param
      has_non_prolific_conj has_non_prolific_conj_symb fun_info
  | _ -> ()

(* we assume that subterms term cannot  have hash undef never redefine!*)
(*
  let assign_hash term =
  match term with
  | Fun(symb, args, fun_info) ->
  (let symb_hash = Symbol.get_hash symb in
  let fun_hash = (get_hash_term_list symb_hash args) in
  fun_info.fun_hash <- Def(fun_hash);
(*  out_str ("fun hash: "^(string_of_int fun_hash)^"\n")*)
  )

  | Var(v, var_info) ->
  (let var_hash = Var.hash(v) in
  var_info.var_hash <- Def(var_hash)
(*     out_str ("var hash: "^(string_of_int var_hash)^"\n")) *)
 *)

(*
  let rec assign_hash_full term =
  (try (get_hash term)
  with
  Term_hash_undef | Term_var_hash_undef ->
  (match term with
  | Fun(symb, args, fun_info) ->
  (let symb_hash = Symbol.get_hash symb in
  let fun_hash = (get_hash_term_list_full symb_hash args) in
  fun_info.fun_hash <- Def(fun_hash);
(* out_str ("Term: "^(to_string term)^"\n");
   out_str ("def fun hash: "^(string_of_int fun_hash)^"\n");*)
  fun_hash
  )
  | Var(v, var_info) ->
  (let var_hash = Var.hash(v) in
  var_info.var_hash <- Def(var_hash);
(* out_str ("Term: "^(to_string term)^"\n");
   out_str ("def var hash: "^(string_of_int var_hash)^"\n");*)
  var_hash)
  )
  )

  and get_hash_term_list_full symb_hash args =
  let hash_fun rest arg_term =
  let arg_term_hash = assign_hash_full arg_term in
(*was hash_comb rest arg_term_hash; not very good... *)
  hash_comb arg_term_hash rest in
  List.fold_left hash_fun symb_hash args

 *)

(* f: first arg  is depth and second is sym, f is applied from top to bottom*)
let rec iter_sym_depth' f current_depth term =
  match term with
  | Fun (sym, args, _) ->
    f current_depth sym;
    List.iter (iter_sym_depth' f (current_depth +1)) args
  | _ -> ()

let iter_sym_depth f term =
  iter_sym_depth' f 1 term

(*
  let assign_var_list term =
  match term with
  | Fun(_, args, fun_info) ->
  let f rest term = append_ass_list (+) (get_var_list term) rest in
  let var_list = List.fold_left f [] args in
  fun_info.var_list <- Def(var_list)
  | _ -> ()
 *)

(* all fun info added only when we add term to term db

   let create_fun_term symbol term_list =
   let fun_info = (empty_fun_info ()) in
   let () = fun_info.num_of_symb <- Def((get_num_of_symb_term_list term_list)) in
   let f rest term = append_ass_list (+) (get_var_list term) rest in
   let var_list = List.fold_left f [] term_list in
   let () = fun_info.var_list <- Def(var_list) in
   Fun(symbol, term_list, fun_info)
 *)


(* exception Term_fast_key_is_def *)

let assign_fast_key (t: term) (fkey: int) =
  dassert (fun () -> fkey <> no_fast_key);
  match t with
  | Fun (_, _, fun_info) ->
    dassert (fun () -> fun_info.fun_fast_key = no_fast_key);  (* Term_fast_key_is_def *)
    fun_info.fun_fast_key <- fkey
  | Var (_, var_info) ->
    dassert (fun () -> var_info.var_fast_key = no_fast_key);  (* Term_fast_key_is_def *)
    var_info.var_fast_key <- fkey

(*
exception Term_db_id_is_def

let assign_db_id = function
  (* Raise exception when db_id is already defined for functional term*)
  | Fun(_, _, { fun_db_id = Def _ }) ->
    (function _ -> raise Term_db_id_is_def)

  (* Raise exception when db_id is already defined for variable term *)
  | Var(_, { var_db_id = Def _ }) ->
    (function _ -> raise Term_db_id_is_def)

  (* Set db_id to defined value for functional term *)
  | Fun(_, _, fun_info) ->
    (function db_id -> fun_info.fun_db_id <- Def db_id)

  (* Set db_id to defined value for variable *)
  | Var(_, var_info) ->
    (function db_id -> var_info.var_db_id <- Def db_id)
*)

(*
  exception Term_hash_is_def
 *)

(* todo: in one iteration all these*)

let assign_has_conj_symb term =
  assign_symbol_bool_param_term Symbol.is_conj_symb has_conj_symb term

let assign_fun_all term =
  assign_num_of_symb term;
  assign_num_of_var term;
  assign_has_conj_symb term;
  (*  assign_symbol_bool_param_term Symbol.is_conj_symb has_conj_symb term;*)
  assign_symbol_bool_param_term Symbol.is_bound_constant has_bound_constant term;
  assign_has_non_prolific_conj_symb term

let assign_var_all term = ()
(*  assign_hash term *)

(* var_list is used in resolution but not in instantiation
   let assign_all term = assign_num_of_symb term; assign_var_list term *)

(*raise Term_assign_fkey_to_var*)

(* andrepd: put on module *)
let arg_map = List.map
let arg_map_list = List.map
let arg_map_left = list_map_left

let arg_fold_left = List.fold_left
let arg_fold_right = List.fold_right

(* more functions *)
let arg_mapi = List.mapi
let arg_mapi_list = List.mapi
let arg_mapi_left = list_mapi_left



let arg_fold_left2 f a l1 l2 =
  try List.fold_left2 f a l1 l2
  with
    Invalid_argument(_) ->
      failwith "term: fold_left2: argumets are of different length"

let arg_for_all2 = List.for_all2

let arg_iter = List.iter
let arg_iter2 f l1 l2 =
  try
    List.iter2 f l1 l2
  with
    Invalid_argument(_) ->
      failwith "term: iter2: argumets are of different length"

let arg_map2 f l1 l2 =
  try
    List.map2 f l1 l2
  with
    Invalid_argument(_) ->
      failwith "term: map2: argumets are of different length"

let arg_find = List.find

let arg_find2 = list_find2

(* arg_return_g_if_f2 f g args1 args2 -> first (g h1 h2) such that (f h1 h2) *)
let arg_return_g_if_f2 = list_return_g_if_f2

let arg_find_not_equal = list_find_not_equal

let arg_to_list x = x
let list_to_arg x = x

let is_empty_args x = 
  List.X.is_empty x

let get_fast_key (t: term) =
  match t with
  | Fun (_, _, {fun_fast_key=fkey}) 
  | Var (_,    {var_fast_key=fkey}) -> 
    dassert (fun () -> fkey <> no_fast_key);  (* Term_fast_key_undef *)
    fkey

let hash t = 
  get_fast_key t
    

let rec fold_left_var f v t =
  match t with
  | Fun (_, args, _) ->
    let res_arg = List.fold_left (fold_left_var f) v args in
    res_arg
  | Var (var,_) -> f v var 

(*
  let rec map f t =
  match t with
  | Fun(sym, args, _) ->
  let new_args =
  List.map (map f) args in
  f (create_fun_term sym new_args)
  | v -> f v
 *)

let rec fold_left f v t =
  match t with
  | Fun (_, args, _) ->
    let res_arg = List.fold_left (fold_left f) v args in
    f res_arg t
  | _ -> f v t



(* f: var-> term *)
let rec map_var f t =
  match t with
  | Fun (sym, args, _) ->
    let new_args = List.map (map_var f) args in
    create_fun_term sym new_args
  | Var (v, _) -> f v

(* true is s is a subterm of t *)
let is_subterm s t =  (* andrepd: inefficient, should do early stop (i.e. w/ exceptions) *)
  fold_left
    (fun v t -> (v || t == s)) false t

let iter f t =
  match t with
  | Fun (sym, args, _) ->
    List.iter f args;
    f t
  | v -> f v


exception Exists

let exists f t =
  let f' s =
    if f s then
      raise Exists
    else 
      ()
  in
  try
    iter f' t;
    false
  with
  | Exists -> true

(* fold_sym folds f for all symbols in the term *)
let rec fold_sym f a term =
  match term with
  | Fun(sym, args, _) ->
      List.fold_left (fold_sym f) (f a sym) args
  | _ -> a

let rec iter_sym f term =
  match term with
  | Fun(sym, args, _) ->
      (f sym;
       List.iter (iter_sym f) args
      )
  | _ -> ()


let rec fold_subterms f a t = 
  let result = f a t in
  match t with 
  | Fun (_, args, _) -> List.fold_left (fold_subterms f) result args
  | Var _ -> result 


  
(* replace all occurrences of subterm by byterm in t *)
(* we assume that t, p, and q are in the term db and therefore we are using == *)
(* the resulting term will need to be  added in term db separately *)

let rec replace subterm byterm t =
  if t == subterm then   
    byterm 
  else (
    match t with
    | Fun(sym, args, _) ->
      let new_args = List.map (replace subterm byterm) args in
      create_fun_term sym new_args
    | _ -> t
  )

    

(*
  does not work since map creates new terms without adding to term_db

  let replace subterm byterm t =
  let f xt =
  if xt == subterm
  then 
  byterm        
  else 
  xt
  in
  map f t 
  
 *)
(* if the type of the term is the same as type of the argument then replace it *)

(*let inst_all_vars_by_term_typed *)

(*
  map (subterm == t)

  if (subterm == t)
  then
  byterm
  else
  begin
  match t with
  | Fun(sym, args, _) ->
  let new_args =
  arg_map (replace ~subterm: subterm ~byterm: byterm) args in
  create_fun_term sym new_args
  | v -> v
  end
 *)

(* used in compare_key we assume that var alwyas greater than fun term*)
let compare_fun_var t s =
  match (t, s) with
  | (Fun _, Var _) -> cless
  | (Var _, Fun _) -> cgreater
  | _ -> cequal

(* old works but not very efficient
   let rec compare_key (t1: term)(t2: term) =
   match (t1, t2) with
   | (Fun(symbol1, args1, _), Fun(symbol2, args2, _)) ->
   let c = Symbol.compare symbol1 symbol2
   in
   if c = cequal then
   try (arg_find_not_equal compare_key args1 args2)
   with Not_found -> cequal
   else c
   | (Var(x, _), Var(y, _)) -> Var.compare x y
   | _ -> compare_fun_var t1 t2
 *)

let rec compare_key (t1: term)(t2: term) =
  match (t1, t2) with
  | (Fun(symbol1, args1, _), Fun(symbol2, args2, _)) ->
      let c = Symbol.compare symbol1 symbol2
      in
      if c = cequal then
  let f result s1 s2 =
    if not (result = cequal)
    then result
    else compare_key s1 s2
  in
  arg_fold_left2 f cequal args1 args2
      else c
  | (Var(x, _), Var(y, _)) -> Var.compare x y
  | _ -> compare_fun_var t1 t2

(* same as above, but all the vars assumed equal *)
let rec compare_key_modulo_var (t1: term)(t2: term) =
  match t1, t2 with
  | Fun(symbol1, args1, _), Fun(symbol2, args2, _) ->
    let c = Symbol.compare symbol1 symbol2 in
    if c = cequal then (
      let f result s1 s2 =
        if not (result = cequal) then 
          result
        else 
          compare_key_modulo_var s1 s2
      in
      arg_fold_left2 f cequal args1 args2
    ) else (
      c
    )
  | Var(x, _), Var(y, _) -> cequal
  | _ -> compare_fun_var t1 t2

let compare_fast_key (t1: term) (t2: term) =
  Int.compare (get_fast_key t1) (get_fast_key t2)

let compare = compare_fast_key

let compare_eq_orient l r = 
  match l, r with
  | Var _, Fun _ -> Ord.lt
  | Fun _, Var _ -> Ord.gt
  | Fun (sym1, _, info1), Fun (sym2, _, info2) ->
    let g = get_param_val in

    let cmp1 = Int.compare (g info1.num_of_symb) (g info2.num_of_symb) in
    if cmp1 <> Ord.eq then cmp1 else
    let cmp2 = Int.compare (g info1.num_of_var ) (g info2.num_of_var ) in
    if cmp2 <> Ord.eq then cmp2 else
    let cmp3 = Ord.lift (fun x -> x != Symbol.symb_top) Bool.compare sym1 sym2 in
    if cmp3 <> Ord.eq then cmp3 else
    let cmp4 = Symbol.compare_fast_key sym1 sym2 in
    if cmp4 <> Ord.eq then cmp4 else
    Int.compare info1.fun_fast_key info2.fun_fast_key

    (* lex_combination3
      (cmp_num_symb)
      (cmp_num_var)
      (Symbol.compare)
      (Ord.reverse_f get_fast_key) *)
  | Var (x, _), Var (y, _) -> 
    Var.compare x y

let apply_to_atom f lit =  (* andrepd: Call it map instead? *)
  match lit with
  | Fun (sym, [t], _) when sym == Symbol.symb_neg ->
    create_fun_term Symbol.symb_neg [(f t)]
  | _ -> 
    f lit

let is_eq_atom atom =
  match atom with
  | Fun (sym, _, _) ->
    sym == Symbol.symb_typed_equality
  | _ -> 
    false

let is_eq_lit lit =
  let atom = get_atom lit in
  is_eq_atom atom

let decompose_eq_atom atom = 
  match atom with
  | Fun (sym, ([etype;t;s] as args), _) when sym == Symbol.symb_typed_equality ->  (* andrepd: Avoid reconstructing with as-pattern *)
    args
  | _ -> 
    []

let is_clock_lit lit =
  match get_atom lit with
  | Fun (sym, _, _) ->
    Symbol.get_bool_param Symbol.is_clock sym
  | _ -> false

let is_less_lit lit =
  match get_atom lit with
  | Fun (sym, _, _) ->
    Symbol.get_bool_param Symbol.is_less sym
  | _ -> false

let is_range_lit lit =
  match get_atom lit with
  | Fun (sym, _, _) ->
    Symbol.get_bool_param Symbol.is_range sym
  | _ -> false

let is_next_state_lit lit =
  match get_atom lit with
  | Fun (sym, _, _) ->
    sym == Symbol.symb_ver_next_state
  | _ -> false

let is_reachable_state_lit lit =
  match get_atom lit with
  | Fun (sym, _, _) ->
    sym == Symbol.symb_ver_reachable_state
  | _ -> false

let is_var term =
  match term with
  | Var _ -> true
  | _ -> false


(*---------is epr if funct. are constants-*)
let is_epr_lit l =
  let atom = get_atom l in
  match atom with
  | Fun(symb, args, _) ->
    if List.exists (fun t -> (not (is_constant t)) && (not (is_var t))) args then 
      false
    else 
      true
  | _ -> 
    true

let is_prop_lit l = 
  let atom = get_atom l in 
  let args = get_args atom in 
  List.X.is_empty args

(* in OCaml true >= false and (compare true false) = 1 *)

(* compare two terms: *)

let cmp_ground t1 t2 =  (* andrepd: again, get rid of polymorphic compare *)
  Bool.compare (is_ground t1) (is_ground t2)

let cmp_num_symb t1 t2 =
  Int.compare (get_num_of_symb t1) (get_num_of_symb t2)

let cmp_num_var t1 t2 =
  Int.compare (get_num_of_var t1) (get_num_of_var t2)

let cmp_prop t1 t2 = 
  Bool.compare (is_prop_lit t1) (is_prop_lit t2)


(* neg is excluded in depth *)
let cmp_atom_depth t1 t2 = 
  let atom1 = get_atom t1 in 
  let atom2 = get_atom t2 in
  Int.compare (get_term_depth atom1) (get_term_depth atom2) 

(*pos bigger than neg*)
let cmp_sign t1 t2 =
  Bool.compare (is_neg_lit t2) (is_neg_lit t1)

let cmp_bool_param param t1 t2 =
  Bool.compare (get_fun_bool_param param t1) (get_fun_bool_param param t2)

let cmp_has_conj_symb t1 t2 =
  cmp_bool_param has_conj_symb t1 t2

let cmp_has_bound_constant t1 t2 =
  cmp_bool_param has_bound_constant t1 t2

let cmp_has_non_prolific_conj_symb t1 t2 =
  cmp_bool_param has_non_prolific_conj_symb t1 t2

let is_definition_lit t =
  let atom = get_atom t in
  match atom with
  | Fun(symb, _, _) ->
    Symbol.is_definition symb
  | _ -> false

let cmp_definition t1 t2 =
  Bool.compare (is_definition_lit t1) (is_definition_lit t2)

let cmp_top t1 t2 =
  let is_top t = 
    match t with
    | Fun (symb,_,_) when symb == Symbol.symb_top  -> true
    | _  -> false
  in
  Bool.compare (is_top t1) (is_top t2)

let cmp_is_eq_lit t1 t2 =
  Bool.compare (is_eq_lit t1) (is_eq_lit t2)

let cmp_is_clock_lit t1 t2 =
  Bool.compare (is_clock_lit t1) (is_clock_lit t2)

let cmp_is_less_lit t1 t2 =
  Bool.compare (is_less_lit t1) (is_less_lit t2)

let cmp_is_range_lit t1 t2 =
  Bool.compare (is_range_lit t1) (is_range_lit t2)

let cmp_is_next_state_lit t1 t2 =
  Bool.compare (is_next_state_lit t1) (is_next_state_lit t2)

let cmp_is_reachable_state_lit t1 t2 =
  Bool.compare (is_reachable_state_lit t1) (is_reachable_state_lit t2)

let lit_cmp_type_to_fun t =
  match t with
  | Options.Lit_Sign b -> compose_sign b cmp_sign
  | Options.Lit_Ground b -> compose_sign b cmp_ground
  | Options.Lit_Prop b ->  compose_sign b cmp_prop
  | Options.Lit_Num_of_Var b -> compose_sign b cmp_num_var
  | Options.Lit_Num_of_Symb b -> compose_sign b cmp_num_symb
  | Options.Lit_Atom_depth b -> compose_sign b cmp_atom_depth
  | Options.Lit_Definition b -> compose_sign b cmp_definition
  | Options.Lit_Id b -> compose_sign b compare_fast_key
  | Options.Lit_has_conj_symb b -> compose_sign b cmp_has_conj_symb
  | Options.Lit_has_bound_constant b -> compose_sign b cmp_has_bound_constant
  | Options.Lit_next_state b -> compose_sign b cmp_is_next_state_lit
  | Options.Lit_reachable_state b -> compose_sign b cmp_is_reachable_state_lit
  | Options.Lit_has_non_prolific_conj_symb b ->
      compose_sign b cmp_has_non_prolific_conj_symb
  | Options.Lit_eq b -> compose_sign b cmp_is_eq_lit
  | Options.Lit_clock b -> compose_sign b cmp_is_clock_lit
  | Options.Lit_less b -> compose_sign b cmp_is_less_lit
  | Options.Lit_range b -> compose_sign b cmp_is_range_lit

let lit_cmp_type_list_to_lex_fun l =
  lex_combination (List.map lit_cmp_type_to_fun l)

(*--------------------------*)

let lit_bool_prop_to_bool bp_opt t = 
  match bp_opt with 
  | Options.Lit_bp_sign b      -> Bool.O.(b = is_pos_lit t)
  | Options.Lit_bp_epr b       -> Bool.O.(b = is_epr_lit t)
  | Options.Lit_bp_eq b        -> Bool.O.(b = is_eq_lit t)
  | Options.Lit_bp_ground b    -> Bool.O.(b = is_ground t)
  | Options.Lit_bp_prop b      -> Bool.O.(b = is_prop_lit t)
  | Options.Lit_bp_conj_symb b -> Bool.O.(b = get_fun_bool_param has_conj_symb t)
      

let compose_bool_prop_opt_list bool_fun default_val opt_list lit = 
  let f = fun rest impl_unit_opt ->
    bool_fun (lit_bool_prop_to_bool impl_unit_opt lit) rest
  in
  List.fold_left f default_val opt_list
    

(*--------------------------*)

let flip_eq_lit term =
  let sign, atom = split_sign_lit term in
  match atom with
  | Fun (sym, [etype;t;s], info) when sym == Symbol.symb_typed_equality ->
    let sym' = sym in
    let args' = [etype;s;t] in
    (* let info' = {info with 
      fun_fast_key = no_fast_key;

    } in *)
    let info' = empty_fun_info() in
    let atom' = create_fun_term_info sym' args' info' in
    if sign then
      atom'
    else
      (* compl_lit atom' *)
      (* create_fun_term Symbol.symb_neg [atom'] *)
      create_neg_lit atom'
  | _ -> failwith "flip_eq_lit: not an equality literal"

(* let orient_eq_lit ord term = 
  let sign, atom = split_sign_lit term in
  (* If l=r *)
  if sign then (
    match atom with
    | Fun (sym, [etype;t;s], info) when sym == Symbol.symb_typed_equality ->
      let module R = PartialOrd in
      begin match ord t s with
      | R.GT ->
        dbg D_orient @@ lazy (sprintf "Oriented: %s" (to_string term));
        set_fun_info_bool_param true is_oriented_equality info;
        term
      | R.LT ->
        (* set_fun_info_bool_param true is_oriented_equality info; *)
        (* let term = Fun (sym, [etype;s;t], info) in *)
        let info' = empty_fun_info() in
        let term = create_fun_term_info sym [etype;s;t] info' in
        set_fun_info_bool_param true is_oriented_equality info';
        dbg D_orient @@ lazy (sprintf "Oriented (flipped): %s" (to_string term));
        term
      | R.EQ | R.INC ->
        dbg D_orient @@ lazy (sprintf "Not oriented: %s" (to_string term));
        set_fun_info_bool_param false is_oriented_equality info;
        term
      end
    | _ (* Var _ *) -> 
      (* term *)
      failwith "orient_eq_lit: not an equality literal"
  ) 
  (* If l!=r *)
  else (
    begin match atom with
    | Fun (sym, _, inner_info) when sym == Symbol.symb_typed_equality ->
      begin match term with
      | Fun (_, _, info) ->
        let inner_oriented = get_fun_info_bool_param is_oriented_equality inner_info in
        set_fun_info_bool_param inner_oriented is_oriented_equality info;
      | _ -> assert false
      end
    | _ -> failwith "orient_eq_lit: not an equality literal"
    end;
    dbg D_orient @@ lazy (sprintf "Neq: %s" (to_string term));
    term
  )

let orient_if_eq_lit ord term =
  if is_eq_lit term then 
    orient_eq_lit ord term 
  else 
    term

let is_oriented_eq_lit term =
  get_fun_term_bool_param is_oriented_equality term *)



(*--------------------------*)

(********** DEPRECATED

let assign_oriented ord term =
  let sign, atom = split_sign_lit term in
  (* Positive equality, orient *)
  if Bool.O.(sign = true) then (
    match atom with
    | Fun (sym, [_;t;s], info) when sym == Symbol.symb_typed_equality ->
      let module R = PartialOrd in
      begin match ord t s with
      | R.GT ->
        dbg D_orient @@ lazy (sprintf "Oriented lr: %s" (to_string term));
        set_fun_info_bool_param true is_oriented_lr info
      | R.LT ->
        dbg D_orient @@ lazy (sprintf "Oriented rl: %s" (to_string term));
        set_fun_info_bool_param true is_oriented_rl info
      | R.EQ | R.INC ->
        dbg D_orient @@ lazy (sprintf "Not oriented: %s" (to_string term));
        ()
      end
    | (* Var *) _ -> ()
  )
  (* Negative equality, assume its child term, the corresponding positive literal, is already assigned accordingly *)
  else (
    match atom with
    | Fun (sym, _, inner_info) when sym == Symbol.symb_typed_equality ->
      begin match term with
      | Fun (_, _, info) ->
        let lr = get_fun_info_bool_param is_oriented_lr inner_info in
        let rl = get_fun_info_bool_param is_oriented_rl inner_info in
        set_fun_info_bool_param lr is_oriented_lr info;
        set_fun_info_bool_param rl is_oriented_rl info;
      | Var _ -> assert false
      end
    | (* Var *) _ -> ()
  )

let get_oriented_lr term =
  get_fun_bool_param is_oriented_lr term

let get_oriented_rl term =
  get_fun_bool_param is_oriented_rl term

let set_oriented_lr term x =
  set_fun_bool_param x is_oriented_lr term

let set_oriented_rl term x =
  set_fun_bool_param x is_oriented_rl term

let get_oriented term = 
  (* let lr = get_fun_bool_param is_oriented_lr term |> bool_to_int in
  let rl = get_fun_bool_param is_oriented_rl term |> bool_to_int in
  (lr lsl 1) + rl *)
  match term with
  | Fun (_, _, info) ->
    let lr = get_fun_info_bool_param is_oriented_lr info |> Obj.magic in
    let rl = get_fun_info_bool_param is_oriented_rl info |> Obj.magic in
    (lr lsl 1) lor rl
  | Var _ -> failwith "Not an equality lit"

let set_oriented term x =
  let lr = (x land 2) = 2 in
  let rl = (x land 1) = 1 in
  set_fun_bool_param lr is_oriented_lr term;
  set_fun_bool_param rl is_oriented_rl term

**********)



(*--------------------------*)

let is_skolem_const term =
  try
    let symb = get_top_symb term in
    Symbol.is_skolem symb && Symbol.is_constant symb
  with
    Var_term -> false

let is_skolem_lit lit = 
  let atom = get_atom lit in 
  let top_symb = get_top_symb atom in 
  Symbol.is_skolem top_symb

let is_addr_const t =
  match t with
  | Fun (symb, args, _) ->
    list_is_empty (arg_to_list args)
    && Symbol.is_address_const_symb symb
  | _ -> false



(*---------------*)

module TKeyList = 
  struct 
    type t = term list 
    let compare = list_compare_lex compare_fast_key (* total order on lists of terms *)
  end
    
module TMapList = Map.Make(TKeyList)
module TSetList = Set.Make(TKeyList)



(*----------------------*)

module TKey =
  struct
    type t = term
    let equal = (==)
    let hash = get_fast_key
    let compare = compare_fast_key
  end

module TMap = Map.Make(TKey)

module TSet = Set.Make(TKey)

module THashtbl = Hashtbl.Make(TKey)

type term_set = TSet.t



(*----------------------*)

type bound_term = term bind

let compare_bterm =
  Bind.compare ~cmp:(compare: term -> term -> Ord.t)

let equal_bterm =
  Bind.equal ~eq:(==)

let hash_bterm (b,term) = 
  hash_sum b (get_fast_key term)

module BTKey = 
  struct
    type t      = bound_term
    let equal   = equal_bterm  
    let hash    = hash_bterm
    let compare = compare_bterm
  end

module BTMap = Map.Make(BTKey)
    
module BTSet = Set.Make(BTKey)

module BTHashtbl = Hashtbl.Make(BTKey)



(*----------------------*)

let term_list_to_set t_list =
  TSet.of_list t_list 
(*
  let f s t = 
    Set.add t s 
  in
  List.fold_left f Set.empty t_list 
*)


let remove_duplicate_terms term_list = 
  let term_set = TSet.of_list term_list in 
  TSet.elements term_set
        
 
(*
let rec replace subterm byterm t =
  if t == subterm 
  then   
    byterm 
  else
    match t with
    | Fun(sym, args, _) ->
  let new_args =
    List.map (replace subterm byterm) args in
  create_fun_term sym new_args
    | v -> v
*)
    
(* rplaces subterms according to the map topdown *)
let rec replace_map tmap term =
  try 
    TMap.find term tmap
  with 
  | Not_found -> 
    begin match term with
    | Fun(sym, args, _) ->
      let new_args =
      List.map (replace_map tmap) args in
      create_fun_term sym new_args
    | v -> v
    end


  
(* use prop_key!
   exception Term_porpos_id_not_positive
   exception Term_porpos_id_not_def
   let get_propos_id_term t =
   let id =
   (match t with
   | Fun(_, _,{ fun_fast_key = Def(id) }) -> id
   | Var(_,{ var_fast_key = Def(id) }) -> id
   | _ -> raise Term_porpos_id_not_def)
   in
   if id > 0 then id
   else raise Term_porpos_id_not_positive

   let get_propos_id lit =
   if (is_neg_lit lit)
   then - (get_propos_id_term (get_atom lit))
   else get_propos_id_term lit

   let get_propos_gr_id lit = get_propos_id (get_grounding lit)
 *)

(*
  let to_string = to_string_with_num_of_symb
 *)
(*
  let rec subterm subt t =
  if equal sumbt t then True
  else
  match t with
  | Fun(_, args, _) -> map_arg subterm args
  | _ -> False
 *)

type position = int list

let rec at_position term pos = 
  match pos with
  | [] -> term
  | 0::xs -> at_position term xs
  | x::xs -> 
    begin match term with
    | Fun(sym, args, _) ->
      at_position (List.nth args (x-1)) xs
    | Var _ -> raise Not_found
    end

(* Like iteri *)
let iter_position f t =
  let rec aux pos f t =
    match t with
    | Fun(_, args, _) ->
      f pos t;
      args |> List.iteri (fun i t' ->
        let pos' = (i+1)::pos in
        aux pos' f t'
      )
    | Var _ -> f pos t
  in
  aux [] f t

(* Like mapi *)
(* let map_position f t =
  let rec aux pos f t =
    match t with
    | Fun(_, args, _) ->
      f pos t;
      args |> List.iteri (fun i t' ->
        let pos' = (i+1)::pos in
        aux pos' f t'
      )
    | Var _ -> f pos t
  in
  aux [] f t
*) 




(* Iter through all subterms, preorder and postorder *)
let rec iter_preorder f t =
  match t with
  | Fun (_, args, _) ->
    f t;
    List.iter (iter_preorder f) args
  | Var _ -> 
    f t

let rec iter_preorder_novar f t =
  match t with
  | Fun (_, args, _) ->
    f t;
    List.iter (iter_preorder_novar f) args
  | Var _ -> 
    ()

let rec iter_postorder f t =
  match t with
  | Fun (_, args, _) ->
    List.iter (iter_postorder f) args;
    f t
  | Var _ -> 
    f t

let rec iter_postorder_novar f t =
  match t with
  | Fun (_, args, _) ->
    List.iter (iter_postorder f) args;
    f t
  | Var _ -> 
    ()

(* Same but skips term itself *)
let iter_subterms_preorder f t =
  match t with
  | Fun (_, args, _) ->
    List.iter (iter_preorder f) args;
  | Var _ -> 
    ()
let iter_subterms_preorder f t =
  match decompose_eq_atom (get_atom t) with
  | [_;l;r] ->
    iter_preorder f l; iter_preorder f r
  | _ ->
    iter_subterms_preorder f t

let iter_subterms_preorder_novar f t =
  match t with
  | Fun (_, args, _) ->
    List.iter (iter_preorder_novar f) args;
  | Var _ -> 
    ()
let iter_subterms_preorder_novar f t =
  match decompose_eq_atom (get_atom t) with
  | [_;l;r] ->
    iter_preorder_novar f l; iter_preorder_novar f r
  | _ ->
    iter_subterms_preorder_novar f t

let iter_subterms_postorder f t =
  match t with
  | Fun (_, args, _) ->
    List.iter (iter_postorder f) args;
  | Var _ -> 
    ()

let iter_bfs f t =
  let s : term Queue.t = Queue.create() in
  Queue.push t s;
  while not @@ Queue.is_empty s do
    let x = Queue.pop s in
    f x;
    match x with
    | Fun (_, args, _) ->
      List.iter (fun y -> Queue.push y s) args
    | Var _ -> ()
  done

(* let iter_bfs_novar f t =
  let s = Queue.create() in
  Queue.push t s;
  while not @@ Queue.is_empty s do
    let x = Queue.pop s in
    f x;
    match x with
    | Fun (_, args, _) ->
      List.iter (fun y -> match y with Fun _ -> Queue.push y s | Var _ -> ()) args
    | Var _ -> ()
  done *)

let iter_subterms_bfs f t =
  let s : term Queue.t = Queue.create() in
  begin match decompose_eq_atom (get_atom t) with
  (* Equality, do l and r *)
  | [_;l;r] ->
    Queue.push l s;
    Queue.push r s;
  | _ ->
    begin match t with
    | Fun (_, args, _) ->
      List.iter (fun y -> Queue.push y s) args;
    | Var _ -> ()
    end
  end;
  while not @@ Queue.is_empty s do
    let x = Queue.pop s in
    f x;
    begin match x with
    | Fun (_, args, _) ->
      List.iter (fun y -> Queue.push y s) args
    | Var _ -> ()
    end
  done



let get_1_args (x: args) =
  match x with 
  | [a] -> a
  | _ -> assert false

let get_2_args (x: args) =
  match x with 
  | [a;b] -> a,b
  | _ -> assert false

let get_3_args (x: args) =
  match x with 
  | [a;b;c] -> a,b,c
  | _ -> assert false



module Eq = struct
  (* let sign = is_pos_lit  *)

  let is_pos_eq = is_eq_atom

  let is_neg_eq x = 
    match x with
    | Fun (sym, [atom], _) -> 
      sym == Symbol.symb_neg && is_eq_atom atom
    | _ -> false

  let is_eq x = 
    is_pos_eq (get_atom x)



  let decompose_atom atom = 
    match atom with
    | Fun (sym, ([_;l;r]), _) when sym == Symbol.symb_typed_equality ->
      Some (l,r)
    | _ -> 
      None

  let decompose_lit lit = 
    let sign, atom = split_sign_lit lit in
    match atom with
    | Fun (sym, ([_;l;r]), _) when sym == Symbol.symb_typed_equality ->
      Some (sign, l,r)
    | _ -> 
      None

  let decompose_atom_type atom = 
    match atom with
    | Fun (sym, ([typ;l;r]), _) when sym == Symbol.symb_typed_equality ->
      Some (typ, l,r)
    | _ -> 
      None

  let decompose_lit_type lit = 
    let sign, atom = split_sign_lit lit in
    match atom with
    | Fun (sym, ([typ;l;r]), _) when sym == Symbol.symb_typed_equality ->
      Some (sign, typ, l,r)
    | _ -> 
      None

  let decompose_atom_exn atom = 
    match atom with
    | Fun (sym, ([_;l;r]), _) when sym == Symbol.symb_typed_equality ->
      (l,r)
    | _ -> 
      invalid_arg (sprintf "Term.Eq.decompose_atom: %s" (to_string atom))

  let decompose_lit_exn lit = 
    let sign, atom = split_sign_lit lit in
    let l,r = decompose_atom_exn atom in
    (sign, l,r)

  let decompose_atom_type_exn atom = 
    match atom with
    | Fun (sym, ([typ;l;r]), _) when sym == Symbol.symb_typed_equality ->
      (typ,l,r)
    | _ -> 
      invalid_arg "Term.Eq.decompose_atom"

  let decompose_lit_type_exn lit = 
    let sign, atom = split_sign_lit lit in
    let typ, l,r = decompose_atom_type_exn atom in 
    (sign, typ, l,r)



  let left  x = let (l,_) = decompose_atom_exn @@ get_atom x in l
  let right x = let (_,r) = decompose_atom_exn @@ get_atom x in r

  let eq_type x = let (t,_,_) = decompose_atom_type_exn @@ get_atom x in t

  let regularize_pos pos l r =
    match pos with
    | 1 -> l,r
    | 2 -> r,l
    | 0 -> failwith "unimplemented"
    | _ -> assert false


  let is_predicate_eq x = 
    let method1 = (
      try get_top_symb (right x) == (* SystemDBs.top_term *) Symbol.symb_top
      with Invalid_argument _ | Var_term -> false
    ) in 
    (* let method2 = (
      try get_top_symb (eq_type x) == Symbol.symb_bool_type
      with Invalid_argument _ -> false
    ) in
    assert (method1 = method2); *)
    method1

  let is_pos_predicate_eq x = 
    is_predicate_eq x && is_pos_lit x

  let is_neg_predicate_eq x = 
    is_predicate_eq x && is_neg_lit x

  let is_nonpredicate_eq x =
    (* Is not an equality -> false | Is a predicate equality -> false | Is a nonpredicate equality -> true *)
    try get_top_symb (right x) != Symbol.symb_top
    with Invalid_argument _ | Var_term -> false

  let is_pos_nonpredicate_eq x = 
    is_nonpredicate_eq x && is_pos_lit x

  let is_neg_nonpredicate_eq x = 
    is_nonpredicate_eq x && is_neg_lit x



  (********** DEPRECATED

  (* let is_oriented = is_oriented_eq_lit *)

  type is_oriented = LeftGreater | RightGreater | NotOriented

  let is_oriented term = 
    match term with
    | Fun (_, _, info) ->
      if get_fun_info_bool_param is_oriented_lr info then
        LeftGreater
      else if get_fun_info_bool_param is_oriented_rl info then
        RightGreater
      else
        NotOriented
    | Var _ -> failwith "Term.Eq.is_oriented: Not an equality literal"

  let oriented term =
    match term with
    | Fun (sym, [_;l;r], info) (* when sym == Symbol.symb_typed_equality *) ->
      if get_fun_info_bool_param is_oriented_lr info then
        Some (l,r)
      else if get_fun_info_bool_param is_oriented_rl info then
        Some (r,l)
      else
        None
    | _ -> failwith "Term.Eq.oriented: Not an equality literal"

  let is_any_oriented term = 
    match term with
    | Fun (_, _, info) ->
      if get_fun_info_bool_param is_oriented_lr info
      || get_fun_info_bool_param is_oriented_rl info then
        true
      else
        false
    | Var _ -> false

  let oriented_big term = 
    match term with
    | Fun (_, [_;l;r], info) ->
      if get_fun_info_bool_param is_oriented_rl info then
        r
      else
        l
    | _ -> failwith "Term.Eq.is_oriented: Not an equality literal"  

  let oriented_small term = 
    match term with
    | Fun (_, [_;l;r], info) ->
      if get_fun_info_bool_param is_oriented_rl info then
        l
      else
        r
    | _ -> failwith "Term.Eq.is_oriented: Not an equality literal"  

  **********)

  let oriented ord id lit =
    dassert (fun () -> is_eq lit);
    let atom = get_atom lit in
    match atom with
    | Fun (sym, args, info) ->
      dassert (fun () -> sym == Symbol.symb_typed_equality);
      (* let open PartialOrd in  *)
      begin try 
        get_orderings_cache_info id info 
      with Not_found ->
        let (_,l,r) = get_3_args args in
        let result = ord l r in
        set_orderings_cache_info id result info;
        result
      end
    | Var _ -> failwith "Term.Eq.is_oriented: Not an equality literal"

  (* let oriented ord id lit = 
    let open PartialOrd in
    match oriented' ord id lit with
    | GT -> GT
    | LT -> LT
    | INC -> INC
    | EQ -> GT *)

  let oriented_any ord id lit = 
    let open PartialOrd in
    match oriented ord id lit with
    | GT | LT -> true
    | EQ | INC -> false
end

