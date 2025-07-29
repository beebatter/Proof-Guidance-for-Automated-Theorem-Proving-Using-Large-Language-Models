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

let symbol_db_ref = Logic_interface.symbol_db_ref
    
(*----- debug modifiable part-----*)

let dbg_flag = true

type dbg_gr = 
  | D_trace
  | D_internals
  | D_symb
  | D_no_var
  | D_so (* symbol order *)
      
let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_internals -> "internals"
  | D_symb -> "symb"
  | D_no_var -> "no_var"
  | D_so -> "so"
        
let dbg_groups = [
  
  D_trace;
  D_symb;
  D_no_var;
   
   
  D_internals;
(*  D_so; *)
]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)





open PartialOrd
type term = Term.term
type atom = Term.atom
type lit  = Term.lit
type symbol = Term.symbol
type var = Var.t

module VarOrder = Orderings.VarOrder
module VSet = Var.VSet
module SMap = Symbol.Map



(** Variable balance:
    When comparing [s] and [t], in each index i, we add +1 for each occurrence of i-th variable in [s], and -1 for each occurrence of the i-th variable in [t]. *)
module VarBalance = struct
  type t = {
    mutable arr: int array;
    mutable dirty: int;  (* Invariant: array is cleared (=0) from dirty (inclusive) till end *)
  }

  let make n : t = {
    arr = Array.make n 0;
    dirty = 0;
  }

  let resize n x = 
    dbg D_internals @@ lazy (sprintf "resize to %d" n);
    x.arr <- Array.make n 0;
    x.dirty <- 0

  let copy x = {
    arr = x.arr; dirty = x.dirty;
  }

  let length_hard x =
    dbg D_internals @@ lazy (sprintf "length_hard %d" (Array.length x.arr));
    Array.length x.arr

  let length_soft _ = 
    let result = Var.max_var_index() * (SymbolDB.get_num_types !symbol_db_ref) in
    dbg D_internals @@ lazy (sprintf "length_soft %d" result);
    result

  let clear x = 
    let n = x.dirty (* length_soft () *) in
    dbg D_internals @@ lazy (sprintf "clearing to %d" n);
    dassert (fun () -> for i = n to length_hard x - 1 do assert (x.arr.(i) = 0) done; true);
    if n <> 0 then (
      Array.fill x.arr 0 n 0;
      x.dirty <- 0
    )

  let extend x = 
    (* If global_balance needs to be extended, do so *)
    dbg_env D_internals (fun () -> 
      for i = 0 to (SymbolDB.get_num_types !symbol_db_ref) - 1 do
        dbg D_internals @@ lazy (sprintf "%d => %s" i (Symbol.to_string @@ SymbolDB.get_basic_type_id_rev !symbol_db_ref i))
      done
    );
    let length_soft = length_soft () in
    let length_hard = length_hard x in
    if length_soft >= length_hard then (
      dbg D_internals @@ lazy (sprintf "updated to size %d" (3 * length_soft));
      resize (3 * length_soft) x
    )
    (* else (VarBalance.clear x) *)



  (** Indexing operators which bound check with [length_soft] *)
  let (.%()) arr i = 
    dassert (fun () -> 0 <= i && i < length_soft ());
    Array.unsafe_get arr i

  let (.%()<-) arr i v = 
    dassert (fun () -> 0 <= i && i < length_soft ());
    Array.unsafe_set arr i v



  let[@inline] idx_of_var var = 
    let balance = () in
    let n_val = Var.get_var_val var in
    
    dbg D_internals @@ lazy (sprintf "idx_of_var: %s" (Var.to_string var));
    
    let n_type = 
      Var.get_type var 
      |> SymbolDB.get_basic_type_id !symbol_db_ref
    in
    let idx = n_type * Var.max_var_index() + n_val in
    dbg D_internals @@ lazy (sprintf "%d %d %d" n_type n_val idx);
    dbg D_internals @@ lazy (sprintf "max %d * %d = %d" 
      (Var.max_var_index()) (SymbolDB.get_num_types !symbol_db_ref) (length_soft balance)
    );
    dassert (fun () -> idx < length_soft balance);
    idx

  let[@inline] idx_to_var idx = 
    let max_var_index = Var.max_var_index() in
    let n_val  = idx mod max_var_index in
    let n_type = idx /   max_var_index in
    let result = Var.create (SymbolDB.get_basic_type_id_rev !symbol_db_ref n_type) n_val in
    dassert (fun () -> idx_of_var result = idx);
    result

  let change f balance var =
    let idx = idx_of_var var in
    balance.dirty <- Int.max balance.dirty (idx+1);
    let n = balance.arr.%(idx) in
    balance.arr.%(idx) <- f n

  let incr = change succ

  let decr = change pred

  let upd pos = change (fun x -> x + pos)


  (* Updates the variable balance for term and its subterms *)
  let rec update_lex balance pos terms =
    List.iter (update balance pos) terms

  and update balance pos term =
    match term with
    | Term.Var (var, _) ->
      upd pos balance var
    | Term.Fun (_, args, _) ->
      if not (Term.is_ground term) then
        update_lex balance pos (Term.arg_to_list args)

  (* Updates the variable balance, and returns true if a variable x is contained in s *)
  let rec update_count_lex balance pos x terms =
    match terms with
    | [] -> false
    | hd::tl -> 
      let occurs = update_count balance pos x hd in
      if not occurs then (
        update_count_lex balance pos x tl
      ) else (
        update_lex balance pos tl;  (* We can revert to the update without counting, because we know the result is [true], but we still need to update for the rest of the terms in tl *)
        true
      )

  and update_count balance pos x term =
    match term with
    | Term.Var (var, _) -> 
      upd pos balance var;
      Var.O.(x == var) (* |> tap (fun x -> dbg D_internals @@ lazy (sprintf "update_count %B" x)) *)
    | Term.Fun (_, args, _) ->
      if not (Term.is_ground term) then 
        update_count_lex balance pos x (Term.arg_to_list args)
      else
        false

  (* Variant with a var_order *)
  let rec update_count_lex_var balance order_var pos x terms =
    match terms with
    | [] -> false
    | hd::tl -> 
      let occurs = update_count_var balance order_var pos x hd in
      if not occurs then (
        update_count_lex_var balance order_var pos x tl
      ) else (
        update_lex balance pos tl;
        true
      )

  and update_count_var balance order_var pos x term =
    match term with
    | Term.Var (var, _) -> 
      upd pos balance var;
      (Var.O.(x == var) || match VarOrder.query order_var var x with GT | EQ -> true | _ -> false)
      (* |> tap (fun x -> dbg D_internals @@ lazy (sprintf "update_count_var %B" x)) *)
    | Term.Fun (_, args, _) ->
      if not (Term.is_ground term) then 
        update_count_lex_var balance order_var pos x (Term.arg_to_list args)
      else
        false

  (* Variant with a var_order being forced to be > *)
  let rec update_count_var_gt do_upd balance order_var pos x term =
    match term with
    | Term.Var (var, _) -> 
      if do_upd then upd pos balance var;
      VSet.singleton var
    | Term.Fun (_, args, _) ->
      if not (Term.is_ground term) then 
        List.fold_left (fun set t ->  (* TODO map_reduce *)
          VSet.union set (update_count_var_gt do_upd balance order_var pos x t)  (* TODO x not being used *)
        ) VSet.empty (Term.arg_to_list args)
      else
        VSet.empty



  let rec update_lex_sw ~wvar ~wsym balance pos terms =
    List.fold_left (fun acc t -> acc + update_sw ~wvar ~wsym balance pos t) 0 terms

  and update_sw ~wvar ~wsym balance pos term =
    match term with
    | Term.Var (var, _) ->
      if pos <> 0 then upd pos balance var;
      wvar
    | Term.Fun (symb, args, _) ->
      wsym symb + update_lex_sw ~wvar ~wsym balance pos (Term.arg_to_list args)

  let rec update_count_lex_sw ~wvar ~wsym balance pos x terms =
    match terms with
    | [] -> 0, false
    | hd::tl -> 
      let weight, occurs = update_count_sw ~wvar ~wsym balance pos x hd in
      if not occurs then (
        let weight', occurs' = update_count_lex_sw ~wvar ~wsym balance pos x tl in
        weight + weight', occurs'
      ) else (
        let weight' = update_lex_sw ~wvar ~wsym balance pos tl in  (* We can revert to the update without counting, because we know the result is [true], but we still need to update for the rest of the terms in tl *)
        weight + weight', true
      )

  and update_count_sw ~wvar ~wsym balance pos x term =
    match term with
    | Term.Var (var, _) -> 
      upd pos balance var;
      wvar, Var.O.(x == var) (* |> tap (fun x -> dbg D_internals @@ lazy (sprintf "update_count_sw %B" x)) *)
    | Term.Fun (symb, args, _) ->
      if not (Term.is_ground term) then 
        let weight, occurs = update_count_lex_sw ~wvar ~wsym balance pos x (Term.arg_to_list args) in
        wsym symb + weight, occurs
      else
        wsym symb + update_lex_sw ~wvar ~wsym balance pos (Term.arg_to_list args), false

  let rec update_count_lex_var_sw ~wvar ~wsym balance order_var pos x terms =
    match terms with
    | [] -> 0, false
    | hd::tl -> 
      let weight, occurs = update_count_var_sw ~wvar ~wsym balance order_var pos x hd in
      if not occurs then (
        let weight', occurs' = update_count_lex_var_sw ~wvar ~wsym balance order_var pos x tl in
        weight + weight', occurs'
      ) else (
        let weight' = update_lex_sw ~wvar ~wsym balance pos tl in
        weight + weight', true
      )

  and update_count_var_sw ~wvar ~wsym balance order_var pos x term =
    match term with
    | Term.Var (var, _) -> 
      upd pos balance var;
      wvar, (Var.O.(x == var) || match VarOrder.query order_var var x with GT | EQ -> true | _ -> false)
      (* |> tap (fun x -> dbg D_internals @@ lazy (sprintf "update_count_var_sw %B" x)) *)
    | Term.Fun (symb, args, _) ->
      if not (Term.is_ground term) then 
        let weight, occurs = update_count_lex_var_sw ~wvar ~wsym balance order_var pos x (Term.arg_to_list args) in
        wsym symb + weight, occurs
      else
        wsym symb + update_lex_sw ~wvar ~wsym balance pos (Term.arg_to_list args), false

  (* Variant with a var_order being forced to be > *)
  let rec update_count_var_gt_sw ~wvar ~wsym do_upd balance order_var pos x term =
    match term with
    | Term.Var (var, _) -> 
      if do_upd then upd pos balance var;
      wvar, VSet.singleton var
    | Term.Fun (symb, args, _) ->
      if not (Term.is_ground term) then 
        let weight, set = 
          List.fold_left (fun (w,set) t ->  (* TODO map_reduce *)
            let w', set' = update_count_var_gt_sw ~wvar ~wsym do_upd balance order_var pos x t in
            w + w', VSet.union set set'
          ) (0,VSet.empty) (Term.arg_to_list args)
        in
        wsym symb + weight, set
      else  (* no var, so no upd, so do_upd makes no difference *)
        wsym symb + update_lex_sw ~wvar ~wsym balance pos (Term.arg_to_list args), VSet.empty






  let for_all' p arr n =
    let rec loop i =
      if i >= n then 
        true
      else if p arr.%(i) then 
        loop (succ i)
      else 
        false
    in
    loop 0

  let for_alli' p arr n =
    let rec loop i =
      if i >= n then 
        true
      else if p i arr.%(i) then 
        loop (succ i)
      else 
        false
    in
    loop 0

  let no_neg x =
    (* x.neg_count = 0 *)
    x.dirty = 0 || for_all' (fun y -> y>=0) x.arr x.dirty

  let no_pos x =
    (* x.pos_count = 0 *)
    x.dirty = 0 || for_all' (fun y -> y<=0) x.arr x.dirty

  (* let for_all'' p arr i n = 
    let rec loop i =
      if i >= n then 
        n
      else if p arr.%(i) then 
        loop (succ i)
      else 
        i
    in
    loop 0 *) 

  (* exception Return of bool
  let[@inline] return x = raise_notrace (Return x) *)



  (* Auxiliary function, ensures relations in order_var are visited in topological order *)
  let iter_relations_topological_order order_var f = 
    let vars = 
      let tmp_vars = ref [] in
      VarOrder.iter_vars order_var (fun x -> tmp_vars @= List.cons x);
      List.X.topological_sort_simp (VarOrder.query order_var) !tmp_vars
    in
    dbg D_no_var @@ lazy (sprintf "vars %s" (List.X.to_string Var.to_string vars));
    let rec loop f l = 
      let rec loop' f x l = 
        match l with
        | hd::tl -> f x hd (VarOrder.query order_var x hd); loop' f x tl
        | [] -> ()
      in
      match l with
      | hd::tl -> loop' f hd tl; loop f tl
      | [] -> ()
    in
    loop f vars

  let no_neg_var order_var balance =
    let print_pair x = let x_bal = balance.arr.%(idx_of_var x) in Pair.to_string Var.to_string Int.to_string (x,x_bal) in
    dbg D_no_var @@ lazy (sprintf "no_neg_var START");
    balance.dirty = 0 || (
      let n = balance.dirty in
      let arr = balance.arr in
      for_all' (fun y -> y>=0) arr n || (
        let trail = ref [] in
        (* VarOrder.iter_relations order_var (fun x y rel ->  *)
        iter_relations_topological_order order_var (fun x y rel -> 
          match rel with 
          | GT -> 
            dbg D_no_var @@ lazy (sprintf "no_neg_var GT %s %s" (print_pair x) (print_pair y));
            let x, y = idx_of_var x, idx_of_var y in
            let x_bal = arr.%(x) in
            let y_bal = arr.%(y) in
            if x_bal > 0 && y_bal < 0 then (
              let d = Int.min x_bal (-y_bal) in
              trail @= List.cons (x,y,d);
              arr.%(x) <- x_bal - d;
              arr.%(y) <- y_bal + d;
            )
          | LT -> 
            dbg D_no_var @@ lazy (sprintf "no_neg_var LT %s %s" (print_pair x) (print_pair y));
            let x, y = idx_of_var x, idx_of_var y in
            let y_bal = arr.%(y) in
            let x_bal = arr.%(x) in
            if y_bal > 0 && x_bal < 0 then (
              let d = Int.min y_bal (-x_bal) in
              trail @= List.cons (y,x,d);
              arr.%(y) <- y_bal - d;
              arr.%(x) <- x_bal + d;
            )
          | EQ -> 
            dbg D_no_var @@ lazy (sprintf "no_neg_var EQ %s %s" (print_pair x) (print_pair y));
            let x, y = idx_of_var x, idx_of_var y in
            let x_bal = arr.%(x) in
            let y_bal = arr.%(y) in
            if x_bal > 0 && y_bal < 0 then (
              let d = Int.min x_bal (-y_bal) in
              trail @= List.cons (x,y,d);
              arr.%(x) <- x_bal - d;
              arr.%(y) <- y_bal + d;
            ) else if y_bal > 0 && x_bal < 0 then (
              let d = Int.min y_bal (-x_bal) in
              trail @= List.cons (y,x,d);
              arr.%(y) <- y_bal - d;
              arr.%(x) <- x_bal + d;
            )
          | INC -> (
              dbg D_no_var @@ lazy (sprintf "no_neg_var INC %s %s" (print_pair x) (print_pair y));
             )
        );
        let res = for_all' (fun y -> y>=0) arr n in
        !trail |> List.iter (fun (x,y,d) -> 
          dbg D_no_var @@ lazy (sprintf "trail %d %d %d" x y d);
          arr.%(x) <- arr.%(x) + d;
          arr.%(y) <- arr.%(y) - d;
        );
        res
      )
    )

  let no_pos_var order_var balance =
    let print_pair x = let x_bal = balance.arr.%(idx_of_var x) in Pair.to_string Var.to_string Int.to_string (x,x_bal) in
    balance.dirty = 0 || (
      let n = balance.dirty in
      let arr = balance.arr in
      for_all' (fun y -> y<=0) arr n || (
        let trail = ref [] in
        (* VarOrder.iter_relations order_var (fun x y rel ->  *)
        iter_relations_topological_order order_var (fun x y rel -> 
          match rel with 
          | GT -> 
            dbg D_no_var @@ lazy (sprintf "no_pos_var GT %s %s" (print_pair x) (print_pair y));
            let x, y = idx_of_var x, idx_of_var y in
            let x_bal = arr.%(x) in
            let y_bal = arr.%(y) in
            if x_bal < 0 && y_bal > 0 then (
              let d = Int.max x_bal (-y_bal) in
              trail @= List.cons (x,y,d);
              arr.%(x) <- x_bal - d;
              arr.%(y) <- y_bal + d;
            )
          | LT -> 
            dbg D_no_var @@ lazy (sprintf "no_pos_var LT %s %s" (print_pair x) (print_pair y));
            let x, y = idx_of_var x, idx_of_var y in
            let y_bal = arr.%(y) in
            let x_bal = arr.%(x) in
            if y_bal < 0 && x_bal > 0 then (
              let d = Int.max y_bal (-x_bal) in
              trail @= List.cons (y,x,d);
              arr.%(y) <- y_bal - d;
              arr.%(x) <- x_bal + d;
            )
          | EQ -> 
            dbg D_no_var @@ lazy (sprintf "no_pos_var INC %s %s" (print_pair x) (print_pair y));
            let x, y = idx_of_var x, idx_of_var y in
            let x_bal = arr.%(x) in
            let y_bal = arr.%(y) in
            if x_bal < 0 && y_bal > 0 then (
              let d = Int.max x_bal (-y_bal) in
              trail @= List.cons (x,y,d);
              arr.%(x) <- x_bal - d;
              arr.%(y) <- y_bal + d;
            ) else if y_bal < 0 && x_bal > 0 then (
              let d = Int.max y_bal (-x_bal) in
              trail @= List.cons (y,x,d);
              arr.%(y) <- y_bal - d;
              arr.%(x) <- x_bal + d;
            )
          | INC -> ()
        );
        let res = for_all' (fun y -> y<=0) arr n in
        !trail |> List.iter (fun (x,y,d) -> 
          dbg D_no_var @@ lazy (sprintf "trail %d %d %d" x y d);
          arr.%(x) <- arr.%(x) + d;
          arr.%(y) <- arr.%(y) - d;
        );
        res
      )
    )

  (* TODO need to benchmark these three functions to see if they're not being overly expensive *)
  let no_neg_var_gt order_var balance : (PartialOrd.t * VarOrder.t) list = 
    let arr = balance.arr in
    dbg D_no_var @@ lazy (sprintf "no_neg_var_gt: START");
    if no_neg_var order_var balance then (
      [GT, order_var]
    ) else (
      (* Build set of positive and negative variables *)
      let pos = ref Var.VMap.empty in
      let neg = ref Var.VMap.empty in
      for i = 0 to balance.dirty - 1 do
        let x = arr.%(i) in
        if x > 0 then
          pos @= Var.VMap.add (idx_to_var i) x
        else if x < 0 then
          neg @= Var.VMap.add (idx_to_var i) x
      done;

      (* Try to find some assignment, compatible with order_var, where negative variables are 
         made < or = to positive variables in a way that the no_neg_var condition is upheld. *)
      let (>>=) = Option.O.(>>=) in
      let print_list = List.X.to_string (Pair.to_string Var.to_string Int.to_string) in
      let rec loop_neg order_var pos neg = 
        dbg D_no_var @@ lazy (sprintf "loop_neg pos=%s neg=%s" (print_list pos) (print_list neg));
        match neg with
        (* No more neg variables, success *)
        | [] -> Some order_var
        (* Variable y_var with count y *)
        | (y_var,y)::tl -> 
          let rec loop_pos order_var pos_hd pos_tl neg_tl = 
            dbg D_no_var @@ lazy (sprintf "no_neg_var_gt: loop_pos pos_hd=%s pos_tl=%s neg_tl=%s" (print_list pos_hd) (print_list pos_tl) (print_list neg_tl));
            match pos_tl with
            (* No more pos variables (but still some neg), failure *)
            | [] -> None
            (* Variable x_var with count x: candidate to make x > y or x = y *)
            | ((x_var,x) as hd)::tl -> 
              dassert (fun () -> x > 0 && y < 0);
              (* Condition: |y|<|x| *)
              let x' = x + y in
              if (* -y <= x *) x' >= 0 then 
                (* Condition: y<x must be compatible with order_var *)
                match 
                  VarOrder.add_gt order_var x_var y_var >>= fun order_var' ->
                  loop_neg order_var' (List.rev pos_hd @ (if x' <> 0 then (x_var,x') :: tl else tl)) neg_tl
                with
                | Some _ as result -> result
                | None -> 
                (* Condition: y=x must be compatible with order_var *)
                match 
                  VarOrder.add_eq order_var x_var y_var >>= fun order_var' ->
                  loop_neg order_var' (List.rev pos_hd @ (if x' <> 0 then (x_var,x') :: tl else tl)) neg_tl
                with
                | Some _ as result -> result
                (* Neither compatible, we must try another x *)
                | None -> 
                  loop_pos order_var (hd::pos_hd) tl neg_tl
              (* No "space" under x, we must try another x *)
              else
                loop_pos order_var (hd::pos_hd) tl neg_tl
          in
          loop_pos order_var [] pos tl
      in
      match loop_neg order_var (Var.VMap.bindings !pos) (Var.VMap.bindings !neg) with
      | Some order_var' -> [GT, order_var']
      | None -> [INC, order_var]
    )



  (* Organise the updated versions of no_pos_var, no_neg_var, no_neg_var_gt *)
  module New = struct
    let print_list = List.X.to_string (Pair.to_string Var.to_string Int.to_string)
    let[@inline] ge order_var x y = match VarOrder.query order_var x y with GT | EQ -> true | _ -> false

    (* Auxiliary function: collects lists and counts of positive and negative variables *)
    let counts_pos_neg arr n = 
      let print_list = List.X.to_string (Pair.to_string Var.to_string Int.to_string) in
      let rec loop arr n pos pos_count neg neg_count i = 
        if i = n then
          pos, pos_count, neg, neg_count
        else
          let x = arr.%(i) in
          if x > 0 then
            loop arr n ((idx_to_var i,x)::pos) (pos_count+x) neg neg_count (i+1)
          else if x < 0 then
            loop arr n pos pos_count ((idx_to_var i,x)::neg) (neg_count+x) (i+1)
          else
            loop arr n pos pos_count neg neg_count (i+1)
      in
      loop arr n [] 0 [] 0 0 |> tap (fun (pos, pos_count, neg, neg_count) -> 
        dbg D_no_var @@ lazy (sprintf "counts_pos_neg: pos=%s (%d) neg=%s (%d)" (print_list pos) pos_count (print_list neg) neg_count)
      )

    (* Auxiliary function: remove members of pos that can cover *all* the members of neg which are smaller than it, repeating til fixpoint *)
    let reduce_pos_neg order_var dir pos neg = 
      let rec loop pos_seen pos neg changed = 
        let rec loop' x_var x x' neg_seen neg = 
          if (dir == true && x' < 0) || (dir == false && x' > 0) then
            None
          else
            match neg with
            | ((y_var,y) as hd)::tl -> 
              if ge order_var x_var y_var then
                loop' x_var x (x'+y) neg_seen tl
              else
                loop' x_var x x' (hd::neg_seen) tl
            | [] -> 
              if x = x' then None else Some (List.rev neg_seen, x')
        in
        match pos with
        | ((x_var,x) as hd)::tl -> 
          begin match loop' x_var x x [] neg with
          (* x_var can cover all variables which are ≤ to it *)
          | Some (neg', x') -> 
            (* TODO can skip in no_neg_var *)
            let pos_seen' = if x' <> 0 then (x_var,x')::pos_seen else pos_seen in
            if List.X.is_empty neg' then
              (List.rev_append pos_seen' tl), []
            else
              loop pos_seen' tl neg' true
          (* It cannot *)
          | None -> 
            loop (hd::pos_seen) tl neg changed
          end
        | [] -> 
          let pos = List.rev pos_seen in
          if not changed then 
            pos, neg
          else
            loop [] pos neg false
      in
      loop [] pos neg false |> tap (fun (pos, neg) -> 
        dbg D_no_var @@ lazy (sprintf "reduce_pos_neg: pos=%s neg=%s" (print_list pos) (print_list neg))
      )

    (* Backtracking loop for no_neg_var *)
    let no_neg_var_loop order_var pos neg = 
      let rec loop_neg order_var pos neg = 
        dbg D_no_var @@ lazy (sprintf "no_neg_var_loop: loop_neg pos=%s neg=%s" (print_list pos) (print_list neg));
        match neg with
        | [] -> true
        | (y_var,y)::tl -> 
          let rec loop_pos order_var pos_seen pos neg = 
            dbg D_no_var @@ lazy (sprintf "no_neg_var_loop: loop_pos pos_seen=%s pos=%s neg=%s" (print_list pos_seen) (print_list pos) (print_list neg));
            match pos with
            | [] -> false
            | ((x_var,x) as hd)::tl -> 
              dassert (fun () -> x > 0 && y < 0);
              if ge order_var x_var y_var then
                let d = Int.min x (-y) in
                let x' = x - d in
                let y' = y + d in
                let pos' = List.rev_append pos_seen (if x' <> 0 then (x_var,x')::tl else tl) in
                let neg' = if y' <> 0 then (y_var,y')::neg else neg in
                loop_neg order_var pos' neg' ||
                loop_pos order_var (hd::pos_seen) tl neg
              else
                loop_pos order_var (hd::pos_seen) tl neg
          in
          loop_pos order_var [] pos tl
      in
      loop_neg order_var pos neg

    let no_neg_var order_var balance =
      let n = balance.dirty in
      let arr = balance.arr in
      n = 0 || for_all' (fun y -> y>=0) arr n || (
        dbg D_no_var @@ lazy (sprintf "no_neg_var %d" n);
        if order_var == VarOrder.empty then false else
        (* Get lists and counts *)
        let pos, pos_count, neg, neg_count = counts_pos_neg arr n in
        (* Impossible to have no_neg_var under any order_var *)
        if pos_count < -neg_count then false else
        (* For all positive x such that all y with x>y have ∑|#y| < |#x|, remove them. Repeat until fixpoint. *)
        let pos, neg = reduce_pos_neg order_var true pos neg in
        (* All negative occurences covered *)
        if List.X.is_empty neg then true else
        (* At this point, if problem was not solved yet, we need to begin search with backtracking *)
        no_neg_var_loop order_var pos neg
      )

    (* Backtracking loop for no_pos_var *)
    let no_pos_var_loop order_var neg pos = 
      let rec loop_pos order_var neg pos = 
        dbg D_no_var @@ lazy (sprintf "no_pos_var_loop: loop_pos neg=%s pos=%s" (print_list neg) (print_list pos));
        match pos with
        | [] -> true
        | (y_var,y)::tl -> 
          let rec loop_neg order_var neg_seen neg pos = 
            dbg D_no_var @@ lazy (sprintf "no_pos_var_loop: loop_neg neg_seen=%s neg=%s pos=%s" (print_list neg_seen) (print_list neg) (print_list pos));
            match neg with
            | [] -> false
            | ((x_var,x) as hd)::tl -> 
              dassert (fun () -> x < 0 && y > 0);
              if ge order_var x_var y_var then
                let d = Int.min (-x) y in
                let x' = x + d in
                let y' = y - d in
                let neg' = List.rev_append neg_seen (if x' <> 0 then (x_var,x')::tl else tl) in
                let pos' = if y' <> 0 then (y_var,y')::pos else pos in
                loop_pos order_var neg' pos' ||
                loop_neg order_var (hd::neg_seen) tl pos
              else
                loop_neg order_var (hd::neg_seen) tl pos
          in
          loop_neg order_var [] neg tl
      in
      loop_pos order_var neg pos

    let no_pos_var order_var balance =
      let n = balance.dirty in
      let arr = balance.arr in
      n = 0 || for_all' (fun y -> y<=0) arr n || (
      dbg D_no_var @@ lazy (sprintf "no_pos_var %d" n);
      if order_var == VarOrder.empty then false else
        (* Get lists and counts *)
        let pos, pos_count, neg, neg_count = counts_pos_neg arr n in
        (* Impossible to have no_pos_var under any order_var *)
        if -neg_count < pos_count then false else
        (* For all negative x such that all y with x>y have ∑|#y| < |#x|, remove them. Repeat until fixpoint. *)
        let neg, pos = reduce_pos_neg order_var false neg pos in
        (* All positive occurences covered *)
        if List.X.is_empty pos then true else
        (* At this point, if problem was not solved yet, we need to begin search with backtracking *)
        no_pos_var_loop order_var neg pos
      )

    exception Return

    (* Backtracking loop for no_neg_var_gt: try to find some assignment, compatible with 
       order_var, where negative variables are made < or = to positive variables in a way 
       that the no_neg_var condition is upheld. *)
    let rec no_neg_var_gt_loop order_var pos neg : (PartialOrd.t * VarOrder.t) Seq.t = 
      let[@warning "-21"] rec loop_neg foo order_var pos neg : (PartialOrd.t * VarOrder.t) Seq.t = 
        dbg D_no_var @@ lazy (sprintf "no_neg_var_gt_loop: loop_neg pos=%s neg=%s" (print_list pos) (print_list neg));
        match neg with
        (* No more neg variables, success *)
        | [] ->   if false && order_var == foo then (dbg D_no_var @@ lazy "ret1"; assert false; raise_notrace Return) else Seq.return (GT, order_var)
        (* Variable y_var with count y *)
        | (y_var,y)::tl -> 
          let rec loop_pos foo order_var pos_seen pos neg : (PartialOrd.t * VarOrder.t) Seq.t = 
            dbg D_no_var @@ lazy (sprintf "no_neg_var_gt_loop: loop_pos pos_seen=%s pos=%s neg=%s" (print_list pos_seen) (print_list pos) (print_list neg));
            match pos with
            (* No more pos variables (but still some neg), failure *)
            | [] -> Seq.empty
            (* Variable x_var with count x: candidate to make x > y or x = y *)
            | ((x_var,x) as hd)::tl -> 
              dassert (fun () -> x > 0 && y < 0);
              (* Condition: |y|<|x| *)
              let d = Int.min x (-y) in
              let x' = x - d in
              let y' = y + d in
              (* Condition: y<x must be compatible with order_var *)
              let (@) = Seq.(@) in
              ( fun () -> 
                match VarOrder.add_gt order_var x_var y_var with
                | Some order_var' -> 
                  let pos' = List.rev_append pos_seen (if x' <> 0 then (x_var,x') :: tl else tl) in
                  let neg' = if y' <> 0 then (y_var,y')::neg else neg in
                  loop_neg foo order_var' pos' neg' ()
                | None -> Seq.empty ()
              ) @ (
              (* Condition: y=x must be compatible with order_var *)
              ( fun () -> 
                match VarOrder.add_eq order_var x_var y_var with
                | Some order_var' -> 
                  let pos' = List.rev_append pos_seen (if x' <> 0 then (x_var,x') :: tl else tl) in
                  let neg' = if y' <> 0 then (y_var,y')::neg else neg in
                  loop_neg foo order_var' pos' neg' ()
                | None -> Seq.empty ()
              ) @
              (* Neither compatible, we must try another x *)
                fun () -> loop_pos foo order_var (hd::pos_seen) tl neg ()
              )
          in
          fun () -> loop_pos foo order_var [] pos tl ()
      in
      loop_neg order_var order_var pos neg

    (* TODO need to benchmark these three functions to see if they're not being overly expensive *)
    let no_neg_var_gt order_var balance : (PartialOrd.t * VarOrder.t) Seq.t = 
      let n = balance.dirty in
      let arr = balance.arr in
      dbg D_no_var @@ lazy (sprintf "no_neg_var_gt %d" n);
      (* If satisfied regardless of order_var *)
      if n = 0 || for_all' (fun y -> y>=0) arr n then
        (      dbg D_no_var @@ lazy (sprintf "no_neg_var_gt: all y>=0");
               Seq.return (GT, order_var)
              )
      else 
        (* If satisfied in this order_var without extension *)
        let pos, pos_count, neg, neg_count = counts_pos_neg arr n in
        if order_var != VarOrder.empty 
        && pos_count >= -neg_count  
        && (
          let pos, neg = reduce_pos_neg order_var true pos neg in
          List.X.is_empty neg || no_neg_var_loop order_var pos neg
        )
        then
          Seq.return (GT, order_var)
        (* If not satisfied in this order_var *)
        else
          (* let rec uniq l () = 
            let eq = Pair.equal (==) (==) in
            match l() with
            | Nil -> 
            | Cons (x, next) -> Cons (hd, uniq (List.X.remove_all ~eq hd tl))
          in *)
          (* begin try *)
          no_neg_var_gt_loop order_var pos neg
          (* |> Seq.take 8 *)
          (* |> List.of_seq *)
          (* |> uniq *)
          (* with Return -> dbg D_no_var @@ lazy "catch ret1"; Seq.return (GT, order_var) end *)
  end
end

let global_balance = VarBalance.make 100



(***************)
(* Term weight *)
(***************)

(* Num of symbols (all symbols same weight) *)
(* let get_weight = Term.get_num_of_symb *)

(*********************)
(* Symbol comparison *)
(*********************)

(* TODO: compare by number of occurrences *)

(* Hack for now: a global flag *)
(***
let compare_symb_fun = ref compare_symb_arity_random

let set_compare_symb x =
  let open Options.Symb_ordering in
  match x with
  | Arity       -> compare_symb_fun := compare_symb_arity
  | ArityRev    -> compare_symb_fun := compare_symb_arity_rev
  | ArityRandom -> compare_symb_fun := compare_symb_arity_random
  | Random      -> compare_symb_fun := compare_symb_random

let get_compare_symb () = 
  let open Options.Symb_ordering in
  let x = !compare_symb_fun in
  if x == compare_symb_arity then
    Arity
  else if x == compare_symb_arity_rev then
    ArityRev
  else if x == compare_symb_arity_random then
    ArityRandom
  else if x == compare_symb_random then
    Random
  else
    assert false

let get_ordering_id () = 
  let x = !compare_symb_fun in
  if x == compare_symb_arity        then 0 else
  if x == compare_symb_arity_rev    then 1 else
  if x == compare_symb_arity_random then 2 else
  if x == compare_symb_random       then 3 else 
  assert false
  
let compare_symb x y = 
  let r = (!compare_symb_fun) x y in
  dbg D_symb @@ lazy (sprintf "compare_symb: %s %s %s" (Symbol.to_string x) (Ord.to_sign r) (Symbol.to_string y));
  r
***)





let rec kbo_lex ~weight ~symb_ordering balance t_list s_list =
  match t_list, s_list with
  | [], [] -> 
    EQ
  | t_hd::t_tl, s_hd::s_tl ->
    let res = kbo_terms ~weight ~symb_ordering balance t_hd s_hd in
    if PartialOrd.O.(res = EQ) then (
      kbo_lex ~weight ~symb_ordering balance t_tl s_tl
    ) else (
      (* Update the remaining terms *)
      VarBalance.update_lex balance (+1) t_tl;
      VarBalance.update_lex balance (-1) s_tl;
      res
    )
  | _ -> failwith "kbo_lex: mismatched list length"

and kbo_terms ~weight ~symb_ordering balance t s = 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string t) (Term.to_string s));
  if t == s then
    EQ

  else
    match t,s with
    | Term.Var (x, _), Var (y, _) ->
      VarBalance.incr balance x;
      VarBalance.decr balance y;
      dassert (fun () -> not Var.O.(x = y));  (* TODO: Asserts that x!=y ⇒ Var.O.(x <> y) ! *)
      INC

    | Term.Fun (sym, _, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      let occurs = VarBalance.update_count balance (+1) y t in
      VarBalance.decr balance y;
      if occurs then 
        GT
      else
        INC

    | Term.Var (x, _), Term.Fun (sym, _, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      let occurs = VarBalance.update_count balance (-1) x s in
      VarBalance.incr balance x;
      if occurs then 
        LT
      else
        INC

    | Term.Fun (tsymb, targs, _), Term.Fun (ssymb, sargs, _) ->
      let targs = Term.arg_to_list targs in
      let sargs = Term.arg_to_list sargs in
      begin match targs, sargs with
      | [targ], [sarg] when Symbol.equal tsymb ssymb ->
        kbo_terms ~weight ~symb_ordering balance targ sarg
      | _ ->
        let[@inline] update() = 
          if not (Term.is_ground t) then VarBalance.update_lex balance (+1) targs;
          if not (Term.is_ground s) then VarBalance.update_lex balance (-1) sargs;
        in
        let[@inline] lex() = dassert (fun () -> tsymb == ssymb); kbo_lex ~weight ~symb_ordering balance targs sargs in
        let[@inline] gt_or_inc() = if VarBalance.no_neg balance then GT else INC in
        let[@inline] lt_or_inc() = if VarBalance.no_pos balance then LT else INC in
        let weight_balance = weight t - weight s in
        if weight_balance > 0 then
          (update(); gt_or_inc())
        else if weight_balance < 0 then
          (update(); lt_or_inc())
        else 
        let symb_balance = symb_ordering tsymb ssymb in
        if symb_balance > 0 then
          (update(); gt_or_inc())
        else if symb_balance < 0 then
          (update(); lt_or_inc())
        else 
          match lex() with
          | EQ -> EQ
          | GT -> gt_or_inc()
          | LT -> lt_or_inc()
          | INC -> INC
      end



let kbo_terms_inc_criteria result smallest ord = fun s t ->
  (* let ( let* ) v f = if v != INC then v else f () in *)
  let rec map vset t = 
    match t with
    | Term.Fun (sym, args, _) -> 
      let args = Term.arg_to_list args in
      let args' = List.map (map vset) args in
      if List.X.equal ~eq:(==) args args' then t else (* add_fun_term sym args' *) TermDB.add_ref (Term.create_fun_term sym args') SystemDBs.term_db_ref
    | Term.Var (var, _) -> 
      if VSet.mem var vset then smallest |> SMap.find (Var.get_type var) else t
  in
  if result != INC then result else
  let vars_s = Term.get_var_set s in
  let vars_t = Term.get_var_set t in
  let diff_st = VSet.diff vars_s vars_t in
  let diff_ts = VSet.diff vars_t vars_s in
  let result = 
    if VSet.is_empty diff_st then INC else (
      let s = map diff_st s in
      dbg D_trace @@ lazy (sprintf "-- inc_criteria: %s , %s" (Term.to_string s) (Term.to_string t));
      let result = ord s t in 
      VarBalance.clear global_balance;
      result
    )
  in
  if result == GT then (
    dbg D_trace @@ lazy (sprintf "-- inc_criteria: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
    Statistics.(bump_int_stat comparisons_inc_criteria);
    result
  ) else (
    let result = 
      if VSet.is_empty diff_ts then INC else (
        let t = map diff_ts t in
        dbg D_trace @@ lazy (sprintf "-- inc_criteria: %s , %s" (Term.to_string s) (Term.to_string t));
        let result = ord s t in 
        VarBalance.clear global_balance;
        result
      )
    in
    if result == LT then (
      dbg D_trace @@ lazy (sprintf "-- inc_criteria: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
      Statistics.(bump_int_stat comparisons_inc_criteria);
      result
    ) else (
      INC
    )
  )
  
let kbo_terms_inc_criteria_gt found smallest ord = fun s t ->
  (* let ( let* ) v f = if v != INC then v else f () in *)
  let rec map vset t = 
    match t with
    | Term.Fun (sym, args, _) -> 
      let args = Term.arg_to_list args in
      let args' = List.map (map vset) args in
      if List.X.equal ~eq:(==) args args' then t else (* add_fun_term sym args' *) TermDB.add_ref (Term.create_fun_term sym args') SystemDBs.term_db_ref
    | Term.Var (var, _) -> 
      if VSet.mem var vset then smallest |> SMap.find (Var.get_type var) else t
  in
  if found != None then found else
  let vars_s = Term.get_var_set s in
  let vars_t = Term.get_var_set t in
  let diff_st = VSet.diff vars_s vars_t in
  if VSet.is_empty diff_st then None else (
    let s = map diff_st s in
    dbg D_trace @@ lazy (sprintf "-- inc_criteria: %s , %s" (Term.to_string s) (Term.to_string t));
    let results = ord s t in 
    let found = Seq.find_map (fun x -> (* incr count; *) dbg D_trace @@ lazy "yield"; match x with GT, order_var' -> Some order_var' | _ -> None) results in
    VarBalance.clear global_balance;
    if found != None then (
      dbg D_trace @@ lazy (sprintf "-- inc_criteria: %s GT %s" (Term.to_string s) (Term.to_string t));
      Statistics.(bump_int_stat comparisons_inc_criteria);
    );
    found
  )
  


(* Compare terms, given a weight and a symbol ordering *)
let[@inline] kbo_terms ?inc_criteria ~weight ~symb_ordering = fun s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  Statistics.(bump_int_stat comparisons_done);

  dbg D_trace @@ lazy (sprintf "-- kbo_terms: %s , %s" (Term.to_string s) (Term.to_string t));
  VarBalance.extend global_balance;
  let result = kbo_terms ~weight ~symb_ordering global_balance s t in
  VarBalance.clear global_balance;

  (* INC criteria: if INC, try to see if variables occurring on one side only, when set to the smallest constant of that type, can be  *)
  let result = 
    match inc_criteria with
    | None -> result
    | Some m -> kbo_terms_inc_criteria result m (kbo_terms ~weight ~symb_ordering global_balance) s t
  in

  dbg D_trace @@ lazy (sprintf "-- result terms: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  result

(* Compare lhs and rhs of an equation/disequation *)
let[@inline] mk_kbo_oriented uid kbo_terms = 
  fun lit -> Term.Eq.oriented kbo_terms uid lit

(* Compare atoms, given a comparison on terms *)
let[@inline] kbo_atoms kbo_terms = fun p q -> 
  dbg D_trace @@ lazy (sprintf "-- kbo_atoms %s , %s" (Term.to_string p) (Term.to_string q));
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
    kbo_terms p q)
  |> tap (fun x -> dbg D_trace @@ lazy (sprintf "-- result atoms: %s %s %s" (Term.to_string p) (PartialOrd.to_string x) (Term.to_string q)))

(* Compare predicate literals, given a comparison on atoms *)
let[@inline] kbo_predlits kbo_atoms = fun sign1 atom1 sign2 atom2 -> 
  dbg D_trace @@ lazy (sprintf "-- kbo_predlits %c %s , %c %s" 
    (if sign1 then '+' else '-') (Term.to_string atom1) 
    (if sign2 then '+' else '-') (Term.to_string atom2)
  );
  (* First compare atoms, if equal compare sign *)
  (
  if atom1 != atom2 then 
    kbo_atoms atom1 atom2
  else 
    Bool.compare sign2 sign1 |> PartialOrd.of_ord 
  ) |> tap (fun x -> dbg D_trace @@ lazy (sprintf "-- result predlits: %s" (PartialOrd.to_string x)))

(* Compare literals, given a comparison on terms *)
let[@inline] kbo_lits kbo_terms kbo_oriented = fun l k -> 
  dbg D_trace @@ lazy (sprintf "-- kbo_lits %s , %s" (Term.to_string l) (Term.to_string k));
  let kbo_atoms = kbo_atoms kbo_terms in
  let kbo_predlits = kbo_predlits kbo_atoms in

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
      kbo_predlits sign1 l1 sign2 l2

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
              Some (kbo_terms t v)
            ) else if s == v then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string s) (Term.to_string v)
                (Term.to_string t) (Term.to_string u)
              );
              Some (kbo_terms t u)
            ) else if t == u then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string t) (Term.to_string u)
                (Term.to_string s) (Term.to_string v)
              );
              Some (kbo_terms s v)
            ) else if t == v then (
              dbg D_trace @@ lazy (sprintf "fast: %s == %s : res = %s v %s" 
                (Term.to_string t) (Term.to_string v)
                (Term.to_string s) (Term.to_string u)
              );
              Some (kbo_terms s u)
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
          let st = kbo_oriented (* kbo_terms id *) l in
          let uv = kbo_oriented (* kbo_terms id *) k in
          dbg D_trace @@ lazy (sprintf "%s %s"
            (PartialOrd.to_string st)
            (PartialOrd.to_string uv)
          );
          let su = (fun () -> kbo_atoms s u) in
          let tv = (fun () -> kbo_atoms t v) in
          let sv = (fun () -> kbo_atoms s v) in
          let tu = (fun () -> kbo_atoms t u) in

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
          Multiset_ordering.ord kbo_atoms l_multiset k_multiset
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
      kbo_predlits sign1 l1 sign2 atom2
    else (
      dbg D_trace @@ lazy "equality < pred";
      LT
    )
  | None, Some (l2,r2) -> 
    (* Still need to check if the (syntactic) eq is actually a (semantic) predicate *)
    if is_pred r2 then
      kbo_predlits sign1 atom1 sign2 l2
    else (
      dbg D_trace @@ lazy "pred > equality";
      GT
    )

  (* Case: both predicates *)
  | None, None -> 
    dbg D_trace @@ lazy "pred vs pred";
    kbo_predlits sign1 atom1 sign2 atom2

  (* | _ -> assert false *)

  ) |> tap (fun x -> dbg D_trace @@ lazy (sprintf "-- result lits: %s %s %s" (Term.to_string l) (PartialOrd.to_string x) (Term.to_string k)))


(* Infix operators *)
(* Helper functor *)
(* module MakeInfix (S:sig type t val cmp : t -> t -> PartialOrd.t end) *)

(* module Lits  = PartialOrdMakeInfix(struct type t = Term.literal let partial_compare = kbo_lits  end)
module Atoms = PartialOrdMakeInfix(struct type t = Term.term    let partial_compare = kbo_atoms end)
module Terms = PartialOrdMakeInfix(struct type t = Term.term    let partial_compare = kbo_terms end) *)

(* TODO unit testing for this module *)





(* ****************** *)
(* Variable orderings *)
(* ****************** *)

(** Version that takes into account [order_var]. *)
let rec kbo_lex_var ~weight ~symb_ordering balance order_var t_list s_list =
  match t_list, s_list with
  | [], [] -> 
    EQ
  | t_hd::t_tl, s_hd::s_tl ->
    let res = kbo_terms_var ~weight ~symb_ordering balance order_var t_hd s_hd in
    if PartialOrd.O.(res = EQ) then (
      kbo_lex_var ~weight ~symb_ordering balance order_var t_tl s_tl
    ) else (
      (* Update the remaining terms (note: still need to do this even if LT or INC) *)
      VarBalance.update_lex balance (+1) t_tl;
      VarBalance.update_lex balance (-1) s_tl;
      res
    )
  | _ -> failwith "kbo_lex_var: mismatched list length"

and kbo_terms_var ~weight ~symb_ordering balance order_var t s = 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string t) (Term.to_string s));
  if t == s then
    EQ

  else
    match t,s with
    | Term.Var (x, _), Var (y, _) ->
        VarBalance.incr balance x;
        VarBalance.decr balance y;
        dassert (fun () -> not Var.O.(x = y));
        dbg D_trace @@ lazy (sprintf "-- kbo_terms_var order: %s %s %s"
                               (Var.to_string x)
                               (PartialOrd.to_string (VarOrder.query order_var x y))
                               (Var.to_string y));
        VarOrder.query order_var x y

    | Term.Fun (sym, _, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      let occurs = VarBalance.update_count_var balance order_var (+1) y t in
      VarBalance.decr balance y;
      if occurs then 
        GT
      else
        INC

    | Term.Var (x, _), Term.Fun (sym, _, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      let occurs = VarBalance.update_count_var balance order_var (-1) x s in
      VarBalance.incr balance x;
      if occurs then 
        LT
      else
        INC

    | Term.Fun (tsymb, targs, _), Term.Fun (ssymb, sargs, _) ->
      let targs = Term.arg_to_list targs in
      let sargs = Term.arg_to_list sargs in
      begin match targs, sargs with
      | [targ], [sarg] when Symbol.equal tsymb ssymb ->
        kbo_terms_var ~weight ~symb_ordering balance order_var targ sarg
      | _ ->
        let[@inline] update() = 
          if not (Term.is_ground t) then VarBalance.update_lex balance (+1) targs;
          if not (Term.is_ground s) then VarBalance.update_lex balance (-1) sargs;
        in
        let[@inline] lex() = dassert (fun () -> tsymb == ssymb); kbo_lex_var ~weight ~symb_ordering balance order_var targs sargs in
        let[@inline] gt_or_inc() = if VarBalance.New.no_neg_var order_var balance then GT else INC in
        let[@inline] lt_or_inc() = if VarBalance.New.no_pos_var order_var balance then LT else INC in
        let weight_balance = weight t - weight s in
        if weight_balance > 0 then
          (update(); gt_or_inc())
        else if weight_balance < 0 then
          (update(); lt_or_inc())
        else 
        let symb_balance = symb_ordering tsymb ssymb in
        if symb_balance > 0 then
          (update(); gt_or_inc())
        else if symb_balance < 0 then
          (update(); lt_or_inc())
        else 
          match lex() with
          | EQ -> EQ
          | GT -> gt_or_inc()
          | LT -> lt_or_inc()
          | INC -> INC
      end

let[@inline] kbo_terms_var ?inc_criteria ~weight ~symb_ordering = fun order_var s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  Statistics.(bump_int_stat comparisons_done);

  dbg D_trace @@ lazy (sprintf "-- kbo_terms_var: %s , %s" (Term.to_string s) (Term.to_string t));
  dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var));
  VarBalance.extend global_balance;
  let result = kbo_terms_var ~weight ~symb_ordering global_balance order_var s t in
  VarBalance.clear global_balance;

  (* INC criteria: if INC, try to see if variables occurring on one side only, when set to the smallest constant of that type, can be  *)
  dbg D_trace @@ lazy (sprintf "-- result terms_var0 Before inc_criteria: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  let result = 
    match inc_criteria with
    | None -> result
    | Some m -> kbo_terms_inc_criteria result m (kbo_terms_var ~weight ~symb_ordering global_balance order_var) s t
  in

  dbg D_trace @@ lazy (sprintf "-- result terms_var1: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  dassert (fun () -> 
    match kbo_terms ?inc_criteria ~weight ~symb_ordering s t, result with
    | GT, GT | EQ, EQ | LT, LT | INC, _ -> true
    | _ -> false
  );
  dbg D_trace @@ lazy (sprintf "-- result terms_var2: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  result



(** Version that attempts to orient t>s by some extension of the given total ordering on variables 
    (need to duplicate all this code since we want to keep the normal kbo intact for everything 
    else) *)
let rec kbo_lex_var_gt ?(upd=true) ~weight ~symb_ordering balance order_var t_list s_list = fun () -> 
  match t_list, s_list with
  | [], [] -> 
    Seq.return (EQ, order_var) ()
  | t_hd::t_tl, s_hd::s_tl ->
    kbo_terms_var_gt ~upd ~weight ~symb_ordering balance order_var t_hd s_hd 
    (* |> List.concat_map (fun (res, order_var') ->  *)
    (* |> List.hd |> (fun (res, order_var') ->  *)
    (* |> (function [] -> [] | (res, order_var')::_ ->  (* TODO *) *)
    () |> (function 
      | Nil -> Seq.empty ()
      | Cons ((res, order_var'), tl) -> 
        let hd' = 
          if PartialOrd.O.(res = EQ) then (
            (* dassert (fun () -> order_var == order_var'); *)
            kbo_lex_var_gt ~upd ~weight ~symb_ordering balance order_var' t_tl s_tl
          ) else (
            (* Update the remaining terms (note: still need to do this even if LT or INC) *)
            if upd then VarBalance.update_lex balance (+1) t_tl;
            if upd then VarBalance.update_lex balance (-1) s_tl;
            if res == GT then Seq.return (res, order_var') else Seq.empty
          )
        in
        let tl' = 
          tl |> Seq.concat_map (fun (res, order_var') -> 
            if PartialOrd.O.(res = EQ) then (
              kbo_lex_var_gt ~upd:false ~weight ~symb_ordering balance order_var' t_tl s_tl
            ) else (
              if res == GT then Seq.return (res, order_var') else Seq.empty
            )
          )
        in
        Seq.(hd' @ tl') ()
    )
  | _ -> failwith "kbo_lex_var_gt: mismatched list length"

(* TODO optimise to skip the paths where only LT or INC can result, since this will never result in GT *)
and kbo_terms_var_gt ?(upd=true) ~weight ~symb_ordering balance order_var t s : (PartialOrd.t * VarOrder.t) Seq.t = fun () -> 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string t) (Term.to_string s));
  (
  if t == s then
    Seq.return (EQ, order_var)

  else
    match t,s with
    | Term.Var (x, _), Var (y, _) ->
      if upd then VarBalance.incr balance x;
      if upd then VarBalance.decr balance y;
      dassert (fun () -> not Var.O.(x = y));
      (* [VarOrder.add_gt order_var x y, VarOrder.add_eq order_var x y] *)
      begin match VarOrder.add_gt order_var x y, VarOrder.add_eq order_var x y with
      | Some order_var1, Some order_var2 -> List.to_seq [GT, order_var1; EQ, order_var2]
      | Some order_var1, None            -> Seq.return (GT, order_var1)
      | None           , Some order_var2 -> Seq.return (EQ, order_var2)
      | None           , None            -> Seq.empty
      end 

    | Term.Fun (sym, _, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      let occurs = VarBalance.update_count_var_gt upd balance order_var (+1) y t in
      if upd then VarBalance.decr balance y;
      dbg D_trace @@ lazy (sprintf "upd=%b" upd);
      if VSet.exists (fun var -> match VarOrder.query order_var var y with GT | EQ -> true | _ -> false) occurs then
        Seq.return (GT, order_var)
      else
        VSet.to_seq occurs |> Seq.concat_map (fun var -> 
          match VarOrder.add_gt order_var var y, VarOrder.add_eq order_var var y with
          | Some order_var1, Some order_var2 -> List.to_seq [GT, order_var1; GT, order_var2]
          | Some order_var1, None            -> Seq.return (GT, order_var1)
          | None           , Some order_var2 -> Seq.return (GT, order_var2)
          | None           , None            -> Seq.empty
        )

    | Term.Var (x, _), Term.Fun (sym, args, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      (* let occurs = VarBalance.update_count_var_gt upd balance order_var (-1) x s in
      if upd then VarBalance.incr balance x;
      VSet.elements occurs |> List.concat_map (fun var -> 
        match VarOrder.add_gt order_var var x, VarOrder.add_eq order_var var x with
        | Some order_var1, Some order_var2 -> [LT, order_var1; LT, order_var2]
        | Some order_var1, None            -> [LT, order_var1                ]
        | None           , Some order_var2 -> [                LT, order_var2]
        | None           , None            -> []
      ) *)
      if upd && not (Term.is_ground s) then VarBalance.update_lex balance (-1) (Term.arg_to_list args);
      if upd then VarBalance.incr balance x;
      Seq.empty

    | Term.Fun (tsymb, targs, _), Term.Fun (ssymb, sargs, _) ->
      let targs = Term.arg_to_list targs in
      let sargs = Term.arg_to_list sargs in
      begin match targs, sargs with
      | [targ], [sarg] when Symbol.equal tsymb ssymb ->
        kbo_terms_var_gt ~upd ~weight ~symb_ordering balance order_var targ sarg
      | _ ->
        let[@inline] update() =
          if upd && not (Term.is_ground t) then VarBalance.update_lex balance (+1) targs;
          if upd && not (Term.is_ground s) then VarBalance.update_lex balance (-1) sargs;
        in
        let[@inline] lex() = 
          dassert (fun () -> tsymb == ssymb); 
          kbo_lex_var_gt ~upd ~weight ~symb_ordering balance order_var targs sargs 
        in
        let[@inline] gt_or_inc ord = 
          VarBalance.New.no_neg_var_gt ord balance
        in
        let[@inline] lt_or_inc ord = 
          Seq.empty (* [(if VarBalance.New.no_pos_var ord balance then LT else INC), ord] *)
        in
        let weight_balance = weight t - weight s in
        if weight_balance > 0 then
          (update(); gt_or_inc order_var)
        else if weight_balance < 0 then
          (update(); lt_or_inc order_var)
        else 
        let symb_balance = symb_ordering tsymb ssymb in
        if symb_balance > 0 then
          (update(); gt_or_inc order_var)
        else if symb_balance < 0 then
          (update(); lt_or_inc order_var)
        else 
          lex() |> Seq.concat_map (fun ((lex, order_var') as r) -> 
            match lex with
            | EQ -> Seq.return r
            | GT -> gt_or_inc order_var'
            | LT -> lt_or_inc order_var'
            | INC -> Seq.empty (* [r] *)
          )
      end
  ) () |> tap (fun r -> dbg D_trace @@ lazy (sprintf "stepping outo %s / %s […]" (Term.to_string t) (Term.to_string s) (*List.X.to_string (fun (x,_) -> PartialOrd.to_string x) r*)))

let[@inline] kbo_terms_var_gt ?inc_criteria ~weight ~symb_ordering = fun order_var s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  Statistics.(bump_int_stat comparisons_done);

  dbg D_trace @@ lazy (sprintf "-- kbo_terms_var_gt: %s , %s" (Term.to_string s) (Term.to_string t));
  dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var));
  VarBalance.extend global_balance;
  let results = kbo_terms_var_gt ~weight ~symb_ordering global_balance order_var s t (* |> Seq.take 16 *) in
  let found = Seq.find_map (fun x -> dbg D_trace @@ lazy "yield"; match x with GT, order_var' -> Some order_var' | _ -> None) results in
  VarBalance.clear global_balance;

  (* INC criteria: if INC, try to see if variables occurring on one side only, when set to the smallest constant of that type, can be  *)
  let found =   
    match inc_criteria with
    | None -> found
    | Some m -> kbo_terms_inc_criteria_gt found m (kbo_terms_var_gt ~weight ~symb_ordering global_balance order_var) s t
  in   
  dbg_env D_trace (fun () -> 
    match found with
    | Some order_var' -> 
      (* let i,j = List.X.find_index (fun (x,_) -> x==GT) results, List.length results in *)
      dbg D_trace @@ lazy (sprintf "-- result terms_var_gt: %s GT %s [?/?]" (Term.to_string s) (Term.to_string t));
      dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var'));
    | None -> 
      (* let j = List.length results in *)
      dbg D_trace @@ lazy (sprintf "-- result terms_var_gt: %s EQ|LT|INC %s [?]" (Term.to_string s) (Term.to_string t));
  );
  dassert (fun () -> 
    match found with
    | Some order_var' -> 
        (* Exists order_var' ⊃ order_var such that s>t: assert that it does *)
(*          VarBalance.clear global_balance; *)

        dbg D_trace @@ lazy (sprintf "kbo_terms_var_gt: Some: ... s t == %s " (PartialOrd.to_string (kbo_terms_var ?inc_criteria ~weight ~symb_ordering order_var' s t)));
(*          VarBalance.clear global_balance; *)

        kbo_terms_var ?inc_criteria ~weight ~symb_ordering order_var' s t == GT
    | None -> 
      (* Doesn't exist order_var' ⊃ order_var such that s>t: assert no such order can exist *)
        let vars = VSet.union (Term.get_var_set s) (Term.get_var_set t) |> VSet.elements in
        dbg D_trace @@ lazy (sprintf "kbo_terms_var_gt:None");
        kbo_terms_var ?inc_criteria ~weight ~symb_ordering order_var s t != GT && 
        vars |> List.for_all (fun x ->
          vars |> List.for_all (fun y ->            
            x == y || (
            dbg D_trace @@ lazy (sprintf "kbo_terms_var_gt: x: %s y: %s" (Var.to_string x) (Var.to_string y));
            (match VarOrder.add_gt order_var x y with Some order_var' when order_var' != order_var -> kbo_terms_var ~weight ~symb_ordering order_var' s t != GT | _ -> true) &&
            (match VarOrder.add_eq order_var x y with Some order_var' when order_var' != order_var -> kbo_terms_var ~weight ~symb_ordering order_var' s t != GT | _ -> true)
         )
        )
      )
  );
  found





(* ************** *)
(* Symbol weights *)
(* ************** *)

let rec kbo_lex_sw ~wvar ~wsym ~symb_ordering balance t_list s_list =
  match t_list, s_list with
  | [], [] -> 
    EQ, 0, 0
  | t_hd::t_tl, s_hd::s_tl ->
    let res, wt, ws = kbo_terms_sw ~wvar ~wsym ~symb_ordering balance t_hd s_hd in
    if PartialOrd.O.(res = EQ) then (
      let res', wt', ws' = kbo_lex_sw ~wvar ~wsym ~symb_ordering balance t_tl s_tl in
      res', wt + wt', ws + ws'
    ) else (
      (* Update the remaining terms *)
      let wt' = VarBalance.update_lex_sw ~wvar ~wsym balance (+1) t_tl in
      let ws' = VarBalance.update_lex_sw ~wvar ~wsym balance (-1) s_tl in
      res, wt + wt', ws + ws'
    )
  | _ -> failwith "kbo_lex_sw: mismatched list length"

and kbo_terms_sw ~wvar ~wsym ~symb_ordering balance t s = 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string t) (Term.to_string s));
  if t == s then
    let weight = VarBalance.update_sw ~wvar ~wsym balance 0 t in
    EQ, weight, weight

  else
    match t,s with
    | Term.Var (x, _), Var (y, _) ->
      VarBalance.incr balance x;
      VarBalance.decr balance y;
      dassert (fun () -> not Var.O.(x = y));  (* TODO: Asserts that x!=y ⇒ Var.O.(x <> y) ! *)
      INC, wvar, wvar

    | Term.Fun (sym, _, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      let weight, occurs = VarBalance.update_count_sw ~wvar ~wsym balance (+1) y t in
      VarBalance.decr balance y;
      (if occurs then GT else INC), weight, wvar

    | Term.Var (x, _), Term.Fun (sym, _, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      let weight, occurs = VarBalance.update_count_sw ~wvar ~wsym balance (-1) x s in
      VarBalance.incr balance x;
      (if occurs then LT else INC), wvar, weight

    | Term.Fun (tsymb, targs, _), Term.Fun (ssymb, sargs, _) ->
      let targs = Term.arg_to_list targs in
      let sargs = Term.arg_to_list sargs in
      begin match targs, sargs with
      | [targ], [sarg] when Symbol.equal tsymb ssymb ->
        kbo_terms_sw ~wvar ~wsym ~symb_ordering balance targ sarg
      | _ ->
        let lex, w_targs, w_sargs = 
          if tsymb == ssymb then
            kbo_lex_sw ~wvar ~wsym ~symb_ordering balance targs sargs
          else
            INC, 
            VarBalance.update_lex_sw ~wvar ~wsym balance (+1) targs, 
            VarBalance.update_lex_sw ~wvar ~wsym balance (-1) sargs
        in
        let wt = wsym tsymb + w_targs in
        let ws = wsym ssymb + w_sargs in
        let[@inline] gt_or_inc() = if VarBalance.no_neg balance then GT else INC in
        let[@inline] lt_or_inc() = if VarBalance.no_pos balance then LT else INC in
        
        if wt > ws then
          gt_or_inc(), wt, ws
        else if wt < ws then
          lt_or_inc(), wt, ws
        else 
        let symb_balance = symb_ordering tsymb ssymb in
        if symb_balance > 0 then
          gt_or_inc(), wt, ws
        else if symb_balance < 0 then
          lt_or_inc(), wt, ws
        else 
          match dassert (fun () -> tsymb == ssymb); lex with
          | EQ -> EQ, wt, ws
          | GT -> gt_or_inc(), wt, ws
          | LT -> lt_or_inc(), wt, ws
          | INC -> (* assert false; *) INC, wt, ws
      end

(* Compare terms, given a weight and a symbol ordering *)
let[@inline] kbo_terms_sw ~wvar ~wsym ~symb_ordering = fun s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  Statistics.(bump_int_stat comparisons_done);

  dbg D_trace @@ lazy (sprintf "-- kbo_terms_sw: %s , %s" (Term.to_string s) (Term.to_string t));
  VarBalance.extend global_balance;
  let result, ws, wt = kbo_terms_sw ~wvar ~wsym ~symb_ordering global_balance s t in
  VarBalance.clear global_balance; 
  
  dbg D_trace @@ lazy (sprintf "-- result terms_sw: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  dassert (fun () -> 
    dbg D_internals @@ lazy (sprintf "dassert %d %d %d %d" ws wt (VarBalance.update_sw ~wvar ~wsym global_balance 0 s) (VarBalance.update_sw ~wvar ~wsym global_balance 0 t));
    (* KK
    (VarBalance.update_sw ~wvar ~wsym global_balance 0 s = ws) &&
      (VarBalance.update_sw ~wvar ~wsym global_balance 0 t = wt)
      *) true
          );

  result



let rec kbo_lex_var_sw ~wvar ~wsym ~symb_ordering balance order_var t_list s_list =
  match t_list, s_list with
  | [], [] -> 
    EQ, 0, 0
  | t_hd::t_tl, s_hd::s_tl ->
    let res, wt, ws = kbo_terms_var_sw ~wvar ~wsym ~symb_ordering balance order_var t_hd s_hd in
    if PartialOrd.O.(res = EQ) then (
      let res', wt', ws' = kbo_lex_var_sw ~wvar ~wsym ~symb_ordering balance order_var t_tl s_tl in
      res', wt + wt', ws + ws'
    ) else (
      (* Update the remaining terms (note: still need to do this even if LT or INC) *)
      let wt' = VarBalance.update_lex_sw ~wvar ~wsym balance (+1) t_tl in
      let ws' = VarBalance.update_lex_sw ~wvar ~wsym balance (-1) s_tl in
      res, wt + wt', ws + ws'
    )
  | _ -> failwith "kbo_lex_var_sw: mismatched list length"

and kbo_terms_var_sw ~wvar ~wsym ~symb_ordering balance order_var t s = 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string t) (Term.to_string s));
  if t == s then
    let weight = VarBalance.update_sw ~wvar ~wsym balance 0 t in
    EQ, weight, weight

  else
    match t,s with
    | Term.Var (x, _), Var (y, _) ->
      VarBalance.incr balance x;
      VarBalance.decr balance y;
      dassert (fun () -> not Var.O.(x = y));
      VarOrder.query order_var x y, wvar, wvar

    | Term.Fun (sym, _, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      let weight, occurs = VarBalance.update_count_var_sw ~wvar ~wsym balance order_var (+1) y t in
      VarBalance.decr balance y;
      (if occurs then GT else INC), weight, wvar

    | Term.Var (x, _), Term.Fun (sym, _, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      let weight, occurs = VarBalance.update_count_var_sw ~wvar ~wsym balance order_var (-1) x s in
      VarBalance.incr balance x;
      (if occurs then LT else INC), wvar, weight

    | Term.Fun (tsymb, targs, _), Term.Fun (ssymb, sargs, _) ->
      let targs = Term.arg_to_list targs in
      let sargs = Term.arg_to_list sargs in
      begin match targs, sargs with
      | [targ], [sarg] when Symbol.equal tsymb ssymb ->
        kbo_terms_var_sw ~wvar ~wsym ~symb_ordering balance order_var targ sarg
      | _ ->
        let lex, w_targs, w_sargs = 
          if tsymb == ssymb then
            kbo_lex_var_sw ~wvar ~wsym ~symb_ordering balance order_var targs sargs
          else
            INC, 
            VarBalance.update_lex_sw ~wvar ~wsym balance (+1) targs, 
            VarBalance.update_lex_sw ~wvar ~wsym balance (-1) sargs
        in
        let wt = wsym tsymb + w_targs in
        let ws = wsym ssymb + w_sargs in
        let[@inline] gt_or_inc() = if VarBalance.New.no_neg_var order_var balance then GT else INC in
        let[@inline] lt_or_inc() = if VarBalance.New.no_pos_var order_var balance then LT else INC in

        if wt > ws then
          gt_or_inc(), wt, ws
        else if wt < ws then
          lt_or_inc(), wt, ws
        else 
        let symb_balance = symb_ordering tsymb ssymb in
        if symb_balance > 0 then
          gt_or_inc(), wt, ws
        else if symb_balance < 0 then
          lt_or_inc(), wt, ws
        else 
          match dassert (fun () -> tsymb == ssymb); lex with
          | EQ -> EQ, wt, ws
          | GT -> gt_or_inc(), wt, ws
          | LT -> lt_or_inc(), wt, ws
          | INC -> (* assert false; *) INC, wt, ws
      end

let[@inline] kbo_terms_var_sw ~wvar ~wsym ~symb_ordering = fun order_var s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  Statistics.(bump_int_stat comparisons_done);

  dbg D_trace @@ lazy (sprintf "-- kbo_terms_var_sw: %s , %s" (Term.to_string s) (Term.to_string t));
  dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var));
  VarBalance.extend global_balance;
  let result, ws, wt = kbo_terms_var_sw ~wvar ~wsym ~symb_ordering global_balance order_var s t in
  VarBalance.clear global_balance;

  dassert (fun () -> 
    dbg D_internals @@ lazy (sprintf "dassert %d %d %d %d" ws wt (VarBalance.update_sw ~wvar ~wsym global_balance 0 s) (VarBalance.update_sw ~wvar ~wsym global_balance 0 t));
    (* KK
    (VarBalance.update_sw ~wvar ~wsym global_balance 0 s = ws) &&
      (VarBalance.update_sw ~wvar ~wsym global_balance 0 t = wt)
      *) true
  );
  dassert (fun () -> 
    match kbo_terms_sw ~wvar ~wsym ~symb_ordering s t, result with
    | GT, GT | EQ, EQ | LT, LT | INC, _ -> true
    | _ -> false
  );
  dbg D_trace @@ lazy (sprintf "-- result terms_var3: %s %s %s" (Term.to_string s) (PartialOrd.to_string result) (Term.to_string t));
  result



let rec kbo_lex_var_gt_sw ?(upd=true) ~wvar ~wsym ~symb_ordering balance order_var t_list s_list =
  match t_list, s_list with
  | [], [] -> 
    Seq.return (EQ, order_var), 0, 0
  | t_hd::t_tl, s_hd::s_tl ->
    let seq, wt, ws = kbo_terms_var_gt_sw ~upd ~wvar ~wsym ~symb_ordering balance order_var t_hd s_hd in
    seq () |> (function 
      | Seq.Nil ->         
        let wt' = VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then +1 else 0) t_tl in
        let ws' = VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then -1 else 0) s_tl in
        Seq.empty, wt+wt', ws+ws'
      | Seq.Cons ((res, order_var'), tl) -> 
        let hd', wt', ws' = 
          if PartialOrd.O.(res = EQ) then (
            kbo_lex_var_gt_sw ~upd ~wvar ~wsym ~symb_ordering balance order_var' t_tl s_tl
          ) else (
            (* Update the remaining terms (note: still need to do this even if LT or INC) *)
            let wt' = VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then +1 else 0) t_tl in
            let ws' = VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then -1 else 0) s_tl in
            (if res == GT then Seq.return (res, order_var') else Seq.empty), wt', ws'
          )
        in
        let tl' = 
          tl |> Seq.concat_map (fun (res, order_var') -> 
            if PartialOrd.O.(res = EQ) then (
              let seq, wt'', ws'' = kbo_lex_var_gt_sw ~upd:false ~wvar ~wsym ~symb_ordering balance order_var' t_tl s_tl in
              dassert (fun () -> wt' = wt'');
              dassert (fun () -> ws' = ws'');
              seq
            ) else (
              dassert (fun () -> wt' = VarBalance.update_lex_sw ~wvar ~wsym balance 0 t_tl);
              dassert (fun () -> ws' = VarBalance.update_lex_sw ~wvar ~wsym balance 0 s_tl);
              if res == GT then Seq.return (res, order_var') else Seq.empty
            )
          )
        in
        Seq.(hd' @ tl'), wt+wt', ws+ws'
    )
  | _ -> failwith "kbo_lex_var_gt_sw: mismatched list length"

(* TODO optimise to skip the paths where only LT or INC can result, since this will never result in GT *)
and kbo_terms_var_gt_sw ?(upd=true) ~wvar ~wsym ~symb_ordering balance order_var t s : (PartialOrd.t * VarOrder.t) Seq.t * int * int = 
  dbg D_trace @@ lazy (sprintf "stepping into %s / %s" (Term.to_string t) (Term.to_string s));
  (
  if t == s then
    let weight = VarBalance.update_sw ~wvar ~wsym balance 0 t in
    Seq.return (EQ, order_var), weight, weight

  else
    match t,s with
    | Term.Var (x, _), Var (y, _) ->
      if upd then VarBalance.incr balance x;
      if upd then VarBalance.decr balance y;
      dassert (fun () -> not Var.O.(x = y));
      (* [VarOrder.add_gt order_var x y, VarOrder.add_eq order_var x y] *)
      begin match VarOrder.add_gt order_var x y, VarOrder.add_eq order_var x y with
      | Some order_var1, Some order_var2 -> List.to_seq [GT, order_var1; EQ, order_var2], wvar, wvar
      | Some order_var1, None            -> Seq.return (GT, order_var1), wvar, wvar
      | None           , Some order_var2 -> Seq.return (EQ, order_var2), wvar, wvar
      | None           , None            -> Seq.empty, wvar, wvar
      end

    | Term.Fun (sym, _, _), Term.Var (y, _) ->
      (* if Symbol.is_smallest sym then LT else *)
      let weight, occurs = VarBalance.update_count_var_gt_sw ~wvar ~wsym upd balance order_var (+1) y t in
      if upd then VarBalance.decr balance y;
      if VSet.exists (fun var -> match VarOrder.query order_var var y with GT | EQ -> true | _ -> false) occurs then
        Seq.return (GT, order_var), weight, wvar
      else
        VSet.to_seq occurs |> Seq.concat_map (fun var -> 
          match VarOrder.add_gt order_var var y, VarOrder.add_eq order_var var y with
          | Some order_var1, Some order_var2 -> List.to_seq [GT, order_var1; GT, order_var2]
          | Some order_var1, None            -> Seq.return (GT, order_var1)
          | None           , Some order_var2 -> Seq.return (GT, order_var2)
          | None           , None            -> Seq.empty
        ), weight, wvar

    | Term.Var (x, _), Term.Fun (sym, args, _) ->
      (* if Symbol.is_smallest sym then GT else *)
      (* let weight, occurs = VarBalance.update_count_var_gt_sw ~wvar ~wsym upd balance order_var (-1) x s in
      if upd then VarBalance.incr balance x;
      VSet.elements occurs |> List.concat_map (fun var -> 
        match VarOrder.add_gt order_var var x, VarOrder.add_eq order_var var x with
        | Some order_var1, Some order_var2 -> [LT, order_var1; LT, order_var2]
        | Some order_var1, None            -> [LT, order_var1                ]
        | None           , Some order_var2 -> [                LT, order_var2]
        | None           , None            -> []
      ), wvar, weight *)
      let weight = wsym sym + VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then -1 else 0) (Term.arg_to_list args) in
      if upd then VarBalance.incr balance x;
      Seq.empty, wvar, weight

    | Term.Fun (tsymb, targs, _), Term.Fun (ssymb, sargs, _) ->
      let targs = Term.arg_to_list targs in
      let sargs = Term.arg_to_list sargs in
      begin match targs, sargs with
      | [targ], [sarg] when Symbol.equal tsymb ssymb ->
        kbo_terms_var_gt_sw ~upd ~wvar ~wsym ~symb_ordering balance order_var targ sarg
      | _ ->
        let lex, w_targs, w_sargs = 
          if tsymb == ssymb then
            kbo_lex_var_gt_sw ~upd ~wvar ~wsym ~symb_ordering balance order_var targs sargs
          else
            Seq.empty (* Seq.return (INC, order_var) *), 
            VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then +1 else 0) targs, 
            VarBalance.update_lex_sw ~wvar ~wsym balance (if upd then -1 else 0) sargs
        in
        let wt = wsym tsymb + w_targs in
        let ws = wsym ssymb + w_sargs in
        let[@inline] gt_or_inc ord = 
          VarBalance.New.no_neg_var_gt ord balance
        in
        let[@inline] lt_or_inc ord = 
          Seq.empty (* [(if VarBalance.New.no_pos_var ord balance then LT else INC), ord] *)
        in

        if wt > ws then
          gt_or_inc order_var, wt, ws
        else if wt < ws then
          lt_or_inc order_var, wt, ws
        else 
        let symb_balance = symb_ordering tsymb ssymb in
        if symb_balance > 0 then
          gt_or_inc order_var, wt, ws
        else if symb_balance < 0 then
          lt_or_inc order_var, wt, ws
        else 
          (dassert (fun () -> tsymb == ssymb); lex) |> Seq.concat_map (fun ((lex, order_var') as r) -> 
            match lex with
            | EQ -> Seq.return r
            | GT -> gt_or_inc order_var'
            | LT -> lt_or_inc order_var'
            | INC -> Seq.empty (* [r] *)
          ), wt, ws
      end
  ) |> tap (fun (r,_,_) -> dbg D_trace @@ lazy (sprintf "stepping outo %s / %s […]" (Term.to_string t) (Term.to_string s) (*List.X.to_string (fun (x,_) -> PartialOrd.to_string x) r*)))

let[@inline] kbo_terms_var_gt_sw ~wvar ~wsym ~symb_ordering = fun order_var s t ->
  Statistics.(time orderings_time) @@ fun () ->  
  Statistics.(bump_int_stat comparisons_done);

  dbg D_trace @@ lazy (sprintf "-- kbo_terms_var_gt_sw: %s , %s" (Term.to_string s) (Term.to_string t));
  dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var));
  VarBalance.extend global_balance;
  let results, ws, wt = kbo_terms_var_gt_sw ~wvar ~wsym ~symb_ordering global_balance order_var s t in
  let found = Seq.find_map (fun x -> dbg D_trace @@ lazy "yield"; match x with (GT, order_var') -> Some order_var' | _ -> None) results in
  VarBalance.clear global_balance;

  dassert (fun () -> 
    dbg D_internals @@ lazy (sprintf "dassert %d %d %d %d" ws wt (VarBalance.update_sw ~wvar ~wsym global_balance 0 s) (VarBalance.update_sw ~wvar ~wsym global_balance 0 t));
    (* KK
    (VarBalance.update_sw ~wvar ~wsym global_balance 0 s = ws) &&
      (VarBalance.update_sw ~wvar ~wsym global_balance 0 t = wt)
     *)
    true
  );
  dbg_env D_trace (fun () -> 
    match found with
    | Some order_var' -> 
      (* let i,j = List.X.find_index (fun (x,_) -> x==GT) results, List.length results in *)
      dbg D_trace @@ lazy (sprintf "-- result terms_var_gt_sw: %s GT %s [?/?]" (Term.to_string s) (Term.to_string t));
      dbg D_trace @@ lazy (sprintf "-- with: %s" (VarOrder.to_string_dbg order_var'));
    | None -> 
      (* let j = List.length results in *)
      dbg D_trace @@ lazy (sprintf "-- result terms_var_gt_sw: %s EQ|LT|INC %s [?]" (Term.to_string s) (Term.to_string t));
  );
  dassert (fun () -> 
    match found with
    | Some order_var' -> 
      (* Exists order_var' ⊃ order_var such that s>t: assert that it does *)
      kbo_terms_var_sw ~wvar ~wsym ~symb_ordering order_var' s t == GT
    | None -> 
      (* Doesn't exist order_var' ⊃ order_var such that s>t: assert no such order can exist *)
      let vars = VSet.union (Term.get_var_set s) (Term.get_var_set t) |> VSet.elements in
      kbo_terms_var_sw ~wvar ~wsym ~symb_ordering order_var s t != GT &&
      vars |> List.for_all (fun x ->
        vars |> List.for_all (fun y ->
          x == y || (
            (match VarOrder.add_gt order_var x y with Some order_var' when order_var' != order_var -> kbo_terms_var_sw ~wvar ~wsym ~symb_ordering order_var' s t != GT | _ -> true) &&
            (match VarOrder.add_eq order_var x y with Some order_var' when order_var' != order_var -> kbo_terms_var_sw ~wvar ~wsym ~symb_ordering order_var' s t != GT | _ -> true)
          )
        )
      )
  );
  found





let empty_terms_var    : VarOrder.t -> term -> term -> PartialOrd.t      = fun _ _ _ -> failwith "terms_var not defined"
let empty_terms_var_gt : VarOrder.t -> term -> term -> VarOrder.t option = fun _ _ _ -> failwith "terms_var_gt not defined"

let make ?(with_var=true) ?(inc_criteria=false) ~weight ~symb_ordering () =
  dbg_env D_so
    (fun () ->
      (* decreasing order *)
      dbg D_so (lazy (sprintf "make: start so out"));
      dbg D_so (lazy (sprintf "db_size %i" (SymbolDB.size !SystemDBs.symbol_db_ref)));        
      let symb_list_sorted = List.rev (List.sort symb_ordering (SymbolDB.to_list !SystemDBs.symbol_db_ref)) in 
      dbg D_so (lazy (sprintf "decending order: %s" (list_to_string Symbol.to_string symb_list_sorted "\n")));        
      dbg D_so (lazy (sprintf "make: end so out"));
    );
  let inc_criteria = 
    None
    (* if inc_criteria then
      let map: term SMap.t = 
        SMap.empty
        |> SymbolDB.fold (fun sym map -> 
          try 
            if not(Symbol.get_arity sym = 0 && Symbol.get_property sym != Symbol.Placeholder) then map else
            map |> SMap.update (Symbol.get_val_type_def sym) (function
              | Some l ->  Some (sym::l)
              | None -> Some [sym]
            )
          with Symbol.Arity_undef | Failure _ -> map
        ) !SystemDBs.symbol_db_ref
        |> SMap.map (List.X.min symb_ordering)
        |> SMap.map (fun s -> TermDB.add_ref (Term.create_fun_term s []) SystemDBs.term_db_ref)
      in
      Some map
    else
      None *)
  in
  let uid = Orderings.get_next_uid () in
  let terms = kbo_terms ?inc_criteria ~weight ~symb_ordering in
  let oriented = mk_kbo_oriented uid terms in
  let atoms = kbo_atoms terms in
  let lits = kbo_lits terms oriented in
  let terms_var = if with_var then kbo_terms_var ?inc_criteria ~weight ~symb_ordering else empty_terms_var in
  let terms_var_gt = if with_var then kbo_terms_var_gt ?inc_criteria ~weight ~symb_ordering else empty_terms_var_gt in
  ({ uid; terms; oriented; atoms; lits; terms_var; terms_var_gt } : Orderings.t)
  (* Orderings.make ~terms ~atoms ~lits ~oriented *)

let make_sw ?(with_var=true) ~wvar ~wsym ~symb_ordering () =
  dbg_env D_so
    (fun () ->
      (* decreasing order *)
      dbg D_so (lazy (sprintf "make_sw: start so out"));
      let symbs = List.filter (fun x -> (not (Symbol.is_definition x)) && (not (Symbol.is_placeholder_symb x)))
          (SymbolDB.to_list !SystemDBs.symbol_db_ref) in
      let symb_list_sorted = List.rev (List.sort symb_ordering symbs) in
      dbg D_so (lazy (sprintf "%s" (list_to_string Symbol.to_string symb_list_sorted "\n")));
      dbg D_so (lazy (sprintf "make_sw: end so out"));
    );
  let uid = Orderings.get_next_uid () in
  let terms = kbo_terms_sw ~wvar ~wsym ~symb_ordering in
  let oriented = mk_kbo_oriented uid terms in
  let atoms = kbo_atoms terms in
  let lits = kbo_lits terms oriented in
  let terms_var = if with_var then kbo_terms_var_sw ~wvar ~wsym ~symb_ordering else empty_terms_var in
  let terms_var_gt = if with_var then kbo_terms_var_gt_sw ~wvar ~wsym ~symb_ordering else empty_terms_var_gt in
  ({ uid; terms; oriented; atoms; lits; terms_var; terms_var_gt } : Orderings.t)
  (* Orderings.make ~terms ~atoms ~lits ~oriented *)
