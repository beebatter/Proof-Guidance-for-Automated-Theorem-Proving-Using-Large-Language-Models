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

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  
let dbg_groups = [
  D_trace;
]
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)





type term = Term.term
type atom = Term.atom
type lit  = Term.lit
type symbol = Term.symbol



let rec contains var term = 
  match term with
  | Term.Var (x, _) -> 
    Var.O.(x == var)
  | Term.Fun (_, args, _) -> 
    not (Term.is_ground term) && contains_args var args

and contains_args var args = 
  List.exists (contains var) (Term.arg_to_list args)



let rec lpo_lex_eq ~symb_ordering s s_list t t_list =
  match s_list, t_list with
  | [], [] -> 
    EQ
  | s_hd::s_tl, t_hd::t_tl ->
    begin match lpo_terms ~symb_ordering s_hd t_hd with
    | EQ -> 
      lpo_lex_eq ~symb_ordering s s_tl t t_tl
    | GT -> 
      lpo_lex_gt ~symb_ordering s t_tl
    | LT -> 
      lpo_lex_lt ~symb_ordering s_tl t
    | INC -> 
      lpo_lex_inc ~symb_ordering s s_tl t t_tl
    end
  | _ -> failwith "lpo_lex: mismatched list length"

and lpo_lex_gt ~symb_ordering s t_list = 
  match t_list with
  | [] -> GT
  | t_hd::t_tl -> 
    begin match lpo_terms ~symb_ordering s t_hd with
    | GT -> lpo_lex_gt ~symb_ordering s t_tl
    | EQ | LT -> LT
    | INC -> if exists_ge ~symb_ordering t_tl s then LT else INC
    end

and lpo_lex_lt ~symb_ordering s_list t = 
  match s_list with
  | [] -> LT
  | s_hd::s_tl -> 
    begin match lpo_terms ~symb_ordering s_hd t with
    | LT -> lpo_lex_lt ~symb_ordering s_tl t
    | EQ | GT -> GT
    | INC -> if exists_ge ~symb_ordering s_tl t then GT else INC
    end

and lpo_lex_inc ~symb_ordering s s_list t t_list =   
  if exists_ge ~symb_ordering s_list t then
    GT
  else if exists_ge ~symb_ordering t_list s then
    LT
  else
    INC

and exists_ge ~symb_ordering list t = 
  list |> List.exists (fun x -> let r = lpo_terms ~symb_ordering x t in r == GT || r == EQ)



and lpo_terms ~symb_ordering s t = 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string s) (Term.to_string t));
  if s == t then
    EQ

  else
    match s,t with
    | Term.Var (x, _), Var (y, _) ->
      if Var.O.(x == y) then 
        EQ
      else
        INC

    | Term.Fun (sym, args, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      if contains_args y args then 
        GT
      else
        INC

    | Term.Var (x, _), Term.Fun (sym, args, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      if contains_args x args then 
        LT
      else
        INC

    | Term.Fun (ssymb, sargs, _), Term.Fun (tsymb, targs, _) ->
      let sargs = Term.arg_to_list sargs in
      let targs = Term.arg_to_list targs in
      begin match sargs, targs with
      | [sarg], [targ] when Symbol.equal ssymb tsymb ->
        lpo_terms ~symb_ordering sarg targ
      | _ ->
        let res = symb_ordering ssymb tsymb in
        dassert (fun () -> res = 0 || res = -1 || res = +1);
        if res = 0 then
          lpo_lex_eq ~symb_ordering s sargs t targs
        else if res = -1 then
          lpo_lex_lt ~symb_ordering sargs t
        else
          lpo_lex_gt ~symb_ordering s targs
      end



(* Compare terms, given a symbol ordering *)
let[@inline] lpo_terms ~symb_ordering = fun s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  dbg D_trace @@ lazy (sprintf "-- lpo_terms: %s , %s" (Term.to_string s) (Term.to_string t));
  Statistics.(bump_int_stat comparisons_done);
  let result = lpo_terms ~symb_ordering s t in
  dbg D_trace @@ lazy (sprintf "-- result terms: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  result

(* Compare lhs and rhs of an equation/disequation *)
let[@inline] mk_lpo_oriented uid lpo_terms = 
  fun lit -> Term.Eq.oriented lpo_terms uid lit

(* Compare atoms, given a comparison on terms *)
let[@inline] lpo_atoms lpo_terms = fun p q -> 
  dbg D_trace @@ lazy (sprintf "-- lpo_atoms %s , %s" (Term.to_string p) (Term.to_string q));
  let first =
    lex_combination1
      (Ord.reverse_f Term.cmp_top)
      (* (Ord.reverse_f Term.cmp_split); *)  (* Moved to symbol precedence *)
      (* (cmp_top_symb symb_precendence); *) 
      p q
  in
  (if first <> Ord.eq then
    PartialOrd.of_ord first
  else
    lpo_terms p q)
  |> tap (fun x -> dbg D_trace @@ lazy (sprintf "-- result atoms: %s %s %s" (Term.to_string p) (PartialOrd.to_string x) (Term.to_string q)))

(* Compare predicate literals, given a comparison on atoms *)
let[@inline] lpo_predlits lpo_atoms = fun sign1 atom1 sign2 atom2 -> 
  dbg D_trace @@ lazy (sprintf "-- lpo_predlits %c %s , %c %s" 
    (if sign1 then '+' else '-') (Term.to_string atom1) 
    (if sign2 then '+' else '-') (Term.to_string atom2)
  );
  (* First compare atoms, if equal compare sign *)
  (
  if atom1 != atom2 then 
    lpo_atoms atom1 atom2
  else 
    Bool.compare sign2 sign1 |> PartialOrd.of_ord 
  ) |> tap (fun x -> dbg D_trace @@ lazy (sprintf "-- result predlits: %s" (PartialOrd.to_string x)))

(* Compare literals, given a comparison on terms *)
let[@inline] lpo_lits lpo_terms lpo_oriented = fun l k -> 
  dbg D_trace @@ lazy (sprintf "-- lpo_lits %s , %s" (Term.to_string l) (Term.to_string k));
  let lpo_atoms = lpo_atoms lpo_terms in
  let lpo_predlits = lpo_predlits lpo_atoms in

  (* Small optimisation *)
  let is_pred rhs = 
    match rhs with 
    | Term.Fun (sym,_,_) -> sym == Symbol.symb_top
    | Term.Var _ -> false
  in

  (

  let sign1, atom1 = Term.split_sign_lit l in
  let sign2, atom2 = Term.split_sign_lit k in
  match Term.Eq.decompose_atom atom1, Term.Eq.decompose_atom atom2 with
  (* Case: both eqs *)
  | Some (l1,r1), Some (l2,r2) -> (
    (* Need to further check if they are predicates or not (syntactic equalities that are semantically non-equality) *)
    dbg D_trace @@ lazy "Eq case";
    match is_pred r1, is_pred r2 with
    | true, true -> 
      dbg D_trace @@ lazy "pred vs pred";
      lpo_predlits sign1 l1 sign2 l2

    | true, false -> 
      dbg D_trace @@ lazy "pred > equality";
      GT
    | false, true -> 
      dbg D_trace @@ lazy "equality < pred";
      LT

    | false, false ->
    (* if Term.Eq.is_predicate_eq l && not (Term.Eq.is_predicate_eq k) then (
      dbg D_trace @@ lazy "immediate";
      GT
    ) else if not (Term.Eq.is_predicate_eq l) && Term.Eq.is_predicate_eq k then (
      dbg D_trace @@ lazy "immediate";
      LT
    ) else ( *)
      dbg D_trace @@ lazy "eq vs eq";
      (* let sign1, (s,t) = Term.Eq.decompose_lit l in
      let sign2, (u,v) = Term.Eq.decompose_lit k in *)
      let s = l1 in
      let t = r1 in
      let u = l2 in
      let v = r2 in

      let mode1 = 
        (* Special cases that need only 1 comparison *)
        let fast_result = 
          if Bool.O.(sign1 = sign2) then
            if s == u then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string s) (Term.to_string u)
                (Term.to_string t) (Term.to_string v)
              );
              Some (lpo_terms t v)
            ) else if s == v then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string s) (Term.to_string v)
                (Term.to_string t) (Term.to_string u)
              );
              Some (lpo_terms t u)
            ) else if t == u then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string t) (Term.to_string u)
                (Term.to_string s) (Term.to_string v)
              );
              Some (lpo_terms s v)
            ) else if t == v then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string t) (Term.to_string v)
                (Term.to_string s) (Term.to_string u)
              );
              Some (lpo_terms s u)
            ) else (
              None
            )
          else
            None
        in

        match fast_result with 
        | Some x -> x
        | None ->
          (* let id = get_ordering_id() in *)
          let st = lpo_oriented (* lpo_terms id *) l in
          let uv = lpo_oriented (* lpo_terms id *) k in
          dbg D_trace @@ lazy (sprintf "%s %s"
            (PartialOrd.to_string st)
            (PartialOrd.to_string uv)
          );
          let su = (fun () -> lpo_atoms s u) in
          let tv = (fun () -> lpo_atoms t v) in
          let sv = (fun () -> lpo_atoms s v) in
          let tu = (fun () -> lpo_atoms t u) in

          (* Here we use the tree *)
          let open Orderings_eq_table in
          odd
          |> query_bin sign1
          |> query_bin sign2
          |> query_ord st
          |> query_ord uv
          |> query_ord_lazy su
          |> query_ord_lazy tv
          |> query_ord_lazy sv
          |> query_ord_lazy tu
          |> query_term
          (* Orderings_eq_table.get ~sign1 ~sign2 ~st ~uv ~su ~tv ~sv ~tu *)
      in

      (* If dbg_flag = true, also try with multiset ordering and assert that they are equal. *)
      let check_with_multiset = true in
      dbg_env D_trace @@ (fun () -> if check_with_multiset then (
        dbg D_trace @@ lazy (sprintf "mode1: %s" (PartialOrd.to_string mode1));

        let mode2 = 
          let multiset_of_lit sign l r =
            match sign with
            | false -> [l;l;r;r]
            | true  -> [l;r]
          in
          let l_multiset = multiset_of_lit sign1 s t in
          let k_multiset = multiset_of_lit sign2 u v in
          Multiset_ordering.ord lpo_atoms l_multiset k_multiset
        in
        dbg D_trace @@ lazy (sprintf "mode2: %s" (PartialOrd.to_string mode2));

        assert PartialOrd.O.(mode1 = mode2);
      ));

      mode1
    (* ) *)
  ) 

  (* Case: one is eq and other is predicate *)
  | Some (l1,r1), None -> 
    (* Still need to check if the (syntactic) eq is actually a (semantic) predicate *)
    if is_pred r1 then
      lpo_predlits sign1 l1 sign2 atom2
    else (
      dbg D_trace @@ lazy "equality < pred";
      LT
    )
  | None, Some (l2,r2) -> 
    (* Still need to check if the (syntactic) eq is actually a (semantic) predicate *)
    if is_pred r2 then
      lpo_predlits sign1 atom1 sign2 l2
    else (
      dbg D_trace @@ lazy "pred > equality";
      GT
    )

  (* Case: both predicates *)
  | None, None -> 
    dbg D_trace @@ lazy "pred vs pred";
    lpo_predlits sign1 atom1 sign2 atom2

  (* | _ -> assert false *)

  ) |> tap (fun x -> dbg D_trace @@ lazy (sprintf "-- result lits: %s %s %s" (Term.to_string l) (PartialOrd.to_string x) (Term.to_string k)))


(* Infix operators *)
(* Helper functor *)
(* module MakeInfix (S:sig type t val cmp : t -> t -> PartialOrd.t end) *)

(* module Lits  = PartialOrdMakeInfix(struct type t = Term.literal let partial_compare = lpo_lits  end)
module Atoms = PartialOrdMakeInfix(struct type t = Term.term    let partial_compare = lpo_atoms end)
module Terms = PartialOrdMakeInfix(struct type t = Term.term    let partial_compare = lpo_terms end) *)




(* ****************** *)
(* Variable orderings *)
(* ****************** *)

module VarOrder = Orderings.VarOrder

module Lpo_var = struct
  let orig_lpo_terms = lpo_terms

  let rec contains order_var var term = 
    match term with
    | Term.Var (x, _) -> 
      Var.O.(x == var) || (match VarOrder.query order_var x var with GT | EQ -> true | _ -> false)
    | Term.Fun (_, args, _) -> 
      not (Term.is_ground term) && contains_args order_var var args

  and contains_args order_var var args = 
    List.exists (contains order_var var) (Term.arg_to_list args)



  let rec lpo_lex_eq ~symb_ordering order_var s s_list t t_list =
    match s_list, t_list with
    | [], [] -> 
      EQ
    | s_hd::s_tl, t_hd::t_tl ->
      begin match lpo_terms ~symb_ordering order_var s_hd t_hd with
      | EQ -> 
        lpo_lex_eq ~symb_ordering order_var s s_tl t t_tl
      | GT -> 
        lpo_lex_gt ~symb_ordering order_var s t_tl
      | LT -> 
        lpo_lex_lt ~symb_ordering order_var s_tl t
      | INC -> 
        lpo_lex_inc ~symb_ordering order_var s s_tl t t_tl
      end
    | _ -> failwith "lpo_lex_var: mismatched list length"

  and lpo_lex_gt ~symb_ordering order_var s t_list = 
    match t_list with
    | [] -> GT
    | t_hd::t_tl -> 
      begin match lpo_terms ~symb_ordering order_var s t_hd with
      | GT -> lpo_lex_gt ~symb_ordering order_var s t_tl
      | EQ | LT -> LT
      | INC -> if exists_ge ~symb_ordering order_var t_tl s then LT else INC
      end

  and lpo_lex_lt ~symb_ordering order_var s_list t = 
    match s_list with
    | [] -> LT
    | s_hd::s_tl -> 
      begin match lpo_terms ~symb_ordering order_var s_hd t with
      | LT -> lpo_lex_lt ~symb_ordering order_var s_tl t
      | EQ | GT -> GT
      | INC -> if exists_ge ~symb_ordering order_var s_tl t then GT else INC
      end

  and lpo_lex_inc ~symb_ordering order_var s s_list t t_list =   
    if exists_ge ~symb_ordering order_var s_list t then
      GT
    else if exists_ge ~symb_ordering order_var t_list s then
      LT
    else
      INC

  and exists_ge ~symb_ordering order_var list t = 
    list |> List.exists (fun x -> let r = lpo_terms ~symb_ordering order_var x t in r == GT || r == EQ)



  and lpo_terms ~symb_ordering order_var s t = 
    dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string s) (Term.to_string t));
    if s == t then
      EQ

    else
      match s,t with
      | Term.Var (x, _), Var (y, _) ->
        dassert (fun () -> not Var.O.(x = y));
        VarOrder.query order_var x y

      | Term.Fun (sym, args, _), Term.Var (y, _) ->
        (* if Symbol.is_smallest sym then LT else *)
        if contains_args order_var y args then GT else INC

      | Term.Var (x, _), Term.Fun (sym, args, _) ->
        (* if Symbol.is_smallest sym then GT else *)
        if contains_args order_var x args then LT else INC

      | Term.Fun (ssymb, sargs, _), Term.Fun (tsymb, targs, _) ->
        let sargs = Term.arg_to_list sargs in
        let targs = Term.arg_to_list targs in
        begin match sargs, targs with
        | [sarg], [targ] when Symbol.equal ssymb tsymb ->
          lpo_terms ~symb_ordering order_var sarg targ
        | _ ->
          let res = symb_ordering ssymb tsymb in
          dassert (fun () -> res = 0 || res = -1 || res = +1);
          if res = 0 then
            lpo_lex_eq ~symb_ordering order_var s sargs t targs
          else if res = -1 then
            lpo_lex_lt ~symb_ordering order_var sargs t
          else
            lpo_lex_gt ~symb_ordering order_var s targs
        end

  (* Compare terms, given a symbol ordering *)
  let[@inline] lpo_terms ~symb_ordering = fun order_var s t ->
    Statistics.(time orderings_time) @@ fun () ->  
    Statistics.(bump_int_stat comparisons_done);
    dbg D_trace @@ lazy (sprintf "-- lpo_terms_var: %s , %s" (Term.to_string s) (Term.to_string t));
    dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var));
    let result = lpo_terms ~symb_ordering order_var s t in
    dassert (fun () -> 
      match orig_lpo_terms ~symb_ordering s t, result with
      | GT, GT | EQ, EQ | LT, LT | INC, _ -> true
      | _ -> false
    );
    dbg D_trace @@ lazy (sprintf "-- result terms_var: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
    result
end

module Lpo_var_gt = struct
  exception Return

  module VSet = Var.VSet

  let rec contains gt order_var var term = 
    match term with
    | Term.Var (x, _) -> 
      if Var.O.(x == var) || match VarOrder.query order_var x var with GT | EQ -> true | _ -> false then
        raise_notrace Return 
      else
        VSet.singleton x
    | Term.Fun (_, args, _) -> 
      if not (Term.is_ground term) then 
        contains_args gt order_var var args
      else
        VSet.empty

  and contains_args gt order_var var args = 
    List.fold_left (fun acc x -> VSet.union acc @@ contains gt order_var var x) VSet.empty (Term.arg_to_list args)



  let rec lpo_lex_eq ~symb_ordering order_var s s_list t t_list =
    match s_list, t_list with
    | [], [] -> 
      Seq.return (EQ, order_var)
    | s_hd::s_tl, t_hd::t_tl ->
      lpo_terms ~symb_ordering order_var s_hd t_hd |> Seq.concat_map (function  
      | EQ, order_var' -> 
        lpo_lex_eq ~symb_ordering order_var' s s_tl t t_tl
      | GT, order_var' -> 
        lpo_lex_gt ~symb_ordering order_var' s t_tl
      | LT, order_var' -> 
        lpo_lex_lt ~symb_ordering order_var' s_tl t
      | INC, order_var' -> 
        lpo_lex_inc ~symb_ordering order_var' s s_tl t t_tl
      )
    | _ -> failwith "lpo_lex_var: mismatched list length"

  and lpo_lex_gt ~symb_ordering order_var s t_list = 
    match t_list with
    | [] -> Seq.return (GT, order_var)
    | t_hd::t_tl -> 
      lpo_terms ~symb_ordering order_var s t_hd |> Seq.concat_map (function 
      | GT, order_var' -> lpo_lex_gt ~symb_ordering order_var' s t_tl
      | EQ, order_var' | LT, order_var' -> Seq.return (LT, order_var')
      | INC, order_var' -> exists_ge LT ~symb_ordering order_var' t_tl s (* Return -> Seq.return (LT, order_var') *)
      )

  and lpo_lex_lt ~symb_ordering order_var s_list t = 
    match s_list with
    | [] -> Seq.return (LT, order_var)
    | s_hd::s_tl -> 
      lpo_terms ~symb_ordering order_var s_hd t |> Seq.concat_map (function 
      | LT, order_var' -> lpo_lex_lt ~symb_ordering order_var' s_tl t
      | EQ, order_var' | GT, order_var' -> Seq.return (GT, order_var')
      | INC, order_var' -> exists_ge GT ~symb_ordering order_var' s_tl t (* Return -> Seq.return (GT, order_var') *)
      )

  and lpo_lex_inc ~symb_ordering order_var s s_list t t_list = (* fun () ->  *)
    let rec loop1 () = 
      loop2 (exists_ge GT ~symb_ordering order_var s_list t)
    and loop2 l = fun () -> 
      match l() with
      | Seq.Cons (x, next) -> 
        if snd x == order_var then Seq.return x () else Seq.Cons (x, loop2 next)
      | Seq.Nil -> 
        loop3 (exists_ge LT ~symb_ordering order_var t_list s) ()
    and loop3 l = fun () -> 
      match l() with
      | Seq.Cons (x, next) -> 
        if snd x == order_var then Seq.return x () else Seq.Cons (x, loop3 next)
      | Seq.Nil -> 
        Seq.Nil
    in
    loop1 ()
    (*
    match exists_ge GT ~symb_ordering order_var s_list t with
    | exception Return -> Seq.return (GT, order_var)
    | gt -> 
    match exists_ge LT ~symb_ordering order_var t_list s with
    | exception Return -> Seq.return (LT, order_var)
    | lt -> 
    Seq.(gt @ lt)
    *)

  and exists_ge gt ~symb_ordering order_var list t = 
(*    let (@) = Seq.(@) in *)
    (* Needs manual loop for early return *)
    (* TODO experiment with alternate traversal order? *)
    let rec loop_outer list = fun () -> 
      match list() with 
      | Seq.Nil -> Seq.return (INC, order_var) ()
      | Seq.Cons (x, next) ->
        let rec loop_inner list2 next = fun () -> 
          match list2() with
          | Seq.Nil -> 
            loop_outer next ()
          | Seq.Cons ((GT, order_var'), next2) 
          | Seq.Cons ((EQ, order_var'), next2) -> 
            if order_var' == order_var then Seq.return (gt, order_var) () else Seq.Cons ((gt, order_var'), loop_inner next2 next)
          | Seq.Cons (_, next2) -> 
            loop_inner next2 next ()
        in
        loop_inner (lpo_terms ~symb_ordering order_var x t) next ()
    in
    loop_outer (List.to_seq list)
    (*
    ( List.to_seq list |> Seq.concat_map (fun x -> 
      lpo_terms ~symb_ordering order_var x t |> Seq.filter_map (function 
        | GT, order_var' | EQ, order_var' -> if false && order_var' == order_var then raise_notrace Return else Some (gt, order_var') 
        | _ -> None
      )
    ) ) @ Seq.return (INC, order_var)
    *)



  and lpo_terms ~symb_ordering order_var s t = fun () -> 
    dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string s) (Term.to_string t)); (
    if s == t then
      Seq.return (EQ, order_var)

    else
      match s,t with
      | Term.Var (x, _), Var (y, _) ->
        dassert (fun () -> not Var.O.(x = y));
        begin match VarOrder.query order_var x y with
        | INC -> 
          [
            VarOrder.add_gt order_var x y |> Option.lift (fun o -> GT,o); 
            VarOrder.add_eq order_var x y |> Option.lift (fun o -> EQ,o); 
            VarOrder.add_gt order_var y x |> Option.lift (fun o -> LT,o); 
          ] |> List.X.filter_some |> List.to_seq
        | r -> Seq.return (r, order_var)
        end

      | Term.Fun (sym, args, _), Term.Var (y, _) ->
        (* if Symbol.is_smallest sym then LT else *)
        begin try
        let vset = contains_args GT order_var y args in
        if vset |> VSet.exists (fun var -> match VarOrder.query order_var var y with GT | EQ -> true | _ -> false) then
          assert false (* [GT, order_var] *)
        else
          let (@) = Seq.(@) in
          ( VSet.to_seq vset |> Seq.concat_map (fun var -> 
            match VarOrder.add_gt order_var var y, VarOrder.add_eq order_var var y with
            | Some order_var1, Some order_var2 -> List.to_seq [GT, order_var1; GT, order_var2]
            | Some order_var1, None            -> Seq.return (GT, order_var1)
            | None           , Some order_var2 -> Seq.return (GT, order_var2)
            | None           , None            -> Seq.empty
          ) ) @ Seq.return (INC, order_var)
        (* |> List.X.remove_duplicates ~eq:(fun (_,x) (_,y) -> x==y) *)
        with Return -> Seq.return (GT, order_var) end

      | Term.Var (x, _), Term.Fun (sym, args, _) ->
        (* if Symbol.is_smallest sym then GT else *)
        begin try
        let vset = contains_args LT order_var x args in
        if vset |> VSet.exists (fun var -> match VarOrder.query order_var var x with GT | EQ -> true | _ -> false) then
          assert false (* [GT, order_var] *)
        else
          let (@) = Seq.(@) in
          ( VSet.to_seq vset |> Seq.concat_map (fun var -> 
            match VarOrder.add_gt order_var var x, VarOrder.add_eq order_var var x with
            | Some order_var1, Some order_var2 -> List.to_seq [LT, order_var1; LT, order_var2]
            | Some order_var1, None            -> Seq.return (LT, order_var1)
            | None           , Some order_var2 -> Seq.return (LT, order_var2)
            | None           , None            -> Seq.empty
          ) ) @ Seq.return (INC, order_var)
        (* |> List.X.remove_duplicates ~eq:(fun (_,x) (_,y) -> x==y) *)
        with Return -> Seq.return (LT, order_var) end

      | Term.Fun (ssymb, sargs, _), Term.Fun (tsymb, targs, _) ->
        let sargs = Term.arg_to_list sargs in
        let targs = Term.arg_to_list targs in
        begin match sargs, targs with
        | [sarg], [targ] when Symbol.equal ssymb tsymb ->
          lpo_terms ~symb_ordering order_var sarg targ
        | _ ->
          let res = symb_ordering ssymb tsymb in
          dassert (fun () -> res = 0 || res = -1 || res = +1);
          if res = 0 then
            lpo_lex_eq ~symb_ordering order_var s sargs t targs
          else if res = -1 then
            lpo_lex_lt ~symb_ordering order_var sargs t
          else
            lpo_lex_gt ~symb_ordering order_var s targs
        end

    ) ()

  (* Compare terms, given a symbol ordering *)
  let[@inline] lpo_terms ~symb_ordering = fun order_var s t ->
    Statistics.(time orderings_time) @@ fun () ->  
    Statistics.(bump_int_stat comparisons_done);

    dbg D_trace @@ lazy (sprintf "-- lpo_terms_var_gt: %s , %s" (Term.to_string s) (Term.to_string t));
    dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var));
    let results = lpo_terms ~symb_ordering order_var s t in
    let found = Seq.find_map (fun x -> dbg D_trace @@ lazy "yield"; match x with GT, order_var' -> Some order_var' | _ -> None) (results |> Seq.take 16) in

    dassert (fun () -> 
      match found with
      | Some order_var' -> 
        dbg D_trace @@ lazy (sprintf "-- result terms_var_gt: %s GT %s" (Term.to_string s) (Term.to_string t));
        (* Exists order_var' ⊃ order_var such that s>t: assert that it does *)
        Lpo_var.lpo_terms ~symb_ordering order_var' s t == GT
      | _ -> 
        dbg D_trace @@ lazy (sprintf "-- result terms_var_gt: %s EQ|LT|INC %s" (Term.to_string s) (Term.to_string t));
        (* Doesn't exist order_var' ⊃ order_var such that s>t: assert no such order can exist *)
        let vars = VSet.union (Term.get_var_set s) (Term.get_var_set t) |> VSet.elements in
        Lpo_var.lpo_terms ~symb_ordering order_var s t != GT && 
        vars |> List.for_all (fun x ->
          vars |> List.for_all (fun y ->
            x == y || (
              (match VarOrder.add_gt order_var x y with Some order_var' when order_var' != order_var -> Lpo_var.lpo_terms ~symb_ordering order_var' s t != GT | _ -> true) &&
              (match VarOrder.add_eq order_var x y with Some order_var' when order_var' != order_var -> Lpo_var.lpo_terms ~symb_ordering order_var' s t != GT | _ -> true)
            )
          )
        )
    );
    found
end



let empty_terms_var    : Orderings.VarOrder.t -> term -> term -> PartialOrd.t                = fun _ _ _ -> failwith "terms_var not defined"
let empty_terms_var_gt : Orderings.VarOrder.t -> term -> term -> Orderings.VarOrder.t option = fun _ _ _ -> failwith "terms_var_gt not defined"

let make ~symb_ordering =
  let uid = Orderings.get_next_uid () in
  let terms = lpo_terms ~symb_ordering in
  let oriented = mk_lpo_oriented uid terms in
  let atoms = lpo_atoms terms in
  let lits = lpo_lits terms oriented in
  let terms_var = Lpo_var.lpo_terms ~symb_ordering in
  let terms_var_gt = Lpo_var_gt.lpo_terms ~symb_ordering in
  ({ uid; terms; atoms; lits; oriented; terms_var; terms_var_gt } : Orderings.t)

