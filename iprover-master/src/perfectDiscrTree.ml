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

let module_name = "perfectDiscrTree"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



(* Needs a custom trie to store jumplists+terms *)
module Trie = struct
  type key = Sym of symbol | Var of var

  let key_to_string_dbg x = 
    match x with
    | Sym x -> Symbol.to_string x
    | Var x -> Var.to_string x

  module KMap = Map.Make (struct
    type t = key

    let compare t s =
      match t,s with 
      | Sym _, Var _ -> Ord.lt
      | Var _, Sym _ -> Ord.gt
      | Sym x, Sym y -> Symbol.compare x y
      | Var x, Var y -> Var.compare x y
  end)

  type 'a t = 
    | Node of 'a entry
    | Leaf of term * 'a list ref
    (* | Empty *)
  and 'a entry = {
    mutable next: 'a t KMap.t;  (* Map from keys to children *)
    mutable skip: 'a t TMap.t;  (* Map terms -> jump nodes *)
  }

  let print_dbg out x = 
    let rec loop indent out x = 
      let i() = for i = 1 to indent do print_char ' ' done in
      i();
      fprintf out "[%x] " (addressof ~bits:24 x);
      match x with
      | Node x -> 
        (* fprintf out "Node\n";
        i();
        fprintf out "skips: %s\n" (List.X.to_string ~first:"" ~last:"" ~sep:" , " *)
        fprintf out "Node: skips: %a\n" 
          (fun out x -> x |> TMap.iter (fun term index -> fprintf out "%s [%x] " (Term.to_string term) (addressof ~bits:24 index)))
          x.skip
        ;
        x.next |> KMap.iter (fun k v ->
          i();
          fprintf out "%s\n" (key_to_string_dbg k);
          loop (indent+2) out v
        )
      | Leaf (t, l) -> fprintf out "Leaf: %s (%d elements)\n" (Term.to_string t) (List.length !l)
    in
    loop 0 out x

  let empty_node () = 
    Node {next=KMap.empty; skip=TMap.empty}

  let empty_leaf term = 
    Leaf (term, ref [])

  let empty = empty_node

  (* let is_empty x = 
    x == Empty *)

  let add_next_entry entry key value = 
    entry.next <- entry.next |> KMap.add key value

  let add_skip_entry entry key value = 
    entry.skip <- entry.skip |> TMap.add key value

  let update index term k = 
    let update_jumps jumps (* curr_node *) = 
      let ret = ref [] in
      jumps := !jumps |> List.filter_map (fun (entry, term, count) ->
        if count = 1 then (
          (* dbg D_trace @@ lazy (sprintf "added jump %s -> %x" (Term.to_string term) (addressof ~bits:24 curr_node )); *)
          (* entry.skip <- (term, curr_node) :: entry.skip; *)
          ret := (entry, term) :: !ret;
          None
        ) else (
          Some (entry, term, count-1)
        )
      );
      !ret
    in
    let add_jumps jumps entry term = 
      jumps := (entry, term, Term.get_num_of_symb term) :: !jumps;
      dbg D_trace @@ lazy (sprintf "jumps: %s" 
        (List.X.to_string (fun (_, term, count) -> sprintf " %s,%d " (Term.to_string term) (count)) !jumps)
      )
    in

    (* let add_node index key =
      match index with
      | Empty -> 
        Node {next = KMap.singleton key }
      | Node x -> 
        Node (x with next = x.next |> KMap.add key)
      | Leaf _ -> assert false *)

    let get_entry index = 
      match index with
      | Node x -> x
      | Leaf _ -> assert false
    in

    let rec loop index terms (k: term -> 'a list ref -> unit) jumps = 
      dbg D_trace @@ lazy (sprintf "terms: %s" 
        (List.X.to_string Term.to_string terms)
      );
      match terms with
      | hd::tl ->
        let entry = get_entry index in

        let skips = update_jumps jumps (*index''*)(*'*) in
        add_jumps jumps entry hd;

        let key, terms' = 
          match hd with 
          | Term.Fun (sym, args, _) -> Sym sym, (Term.arg_to_list args @ tl)
          | Term.Var (var, _) -> Var var, tl
        in
        let index' = 
          match entry.next |> KMap.find_opt key with
          | Some x -> x
          | None -> 
            let new_index = match terms' with _::_ -> empty_node() | [] -> empty_leaf term in
            entry.next <- entry.next |> KMap.add key new_index;
            new_index
        in

        loop index' terms' k jumps;

        skips |> List.iter (fun (entry, term) -> entry.skip <- entry.skip |> TMap.add term index)

      | [] -> 
        let skips = update_jumps jumps (* index' *) in
        begin match index with
        | Leaf (term, list) -> k term list
        | Node _ -> assert false
        end;
        skips |> List.iter (fun (entry, term) -> entry.skip <- entry.skip |> TMap.add term index)
    in

    dbg D_trace @@ lazy (sprintf "Trie.update: %s" (Term.to_string term));
    (* dbg_env D_trace (fun () -> print_dbg stdout index); *)
    let jumps = ref [] in
    loop index [term] k jumps;
    dbg D_trace @@ lazy "Trie.update (result):";
    dbg_env D_trace (fun () -> print_dbg stdout index);
    ()

  let next index key = 
    match index with
    | Node x -> x.next |> KMap.find_opt key
    | Leaf _ -> invalid_arg "Trie.next"

  let iter_all_next f index = 
    match index with
    | Node x -> KMap.iter f x.next
    | Leaf _ -> invalid_arg "Trie.next"

  let skip index = 
    match index with
    | Node x -> x.skip
    | Leaf _ -> invalid_arg "Trie.skip"

  let get_from_leaf index = 
    match index with
    | Leaf (term, list) -> (term, list)
    | Node _ -> invalid_arg "Trie.get_from_leaf"

  let get_from_leaf_opt index = 
    match index with
    | Leaf (term, list) -> Some (term, list)
    | Node _ -> invalid_arg "Trie.get_from_leaf"
end



(* type 'a t = ('a option ref) Trie.t *)
type 'a t = 'a Trie.t

let create () = 
  dbg D_trace @@ lazy "create";
  Trie.empty ()

let clear x = 
  let open Trie in
  match x with 
  | Node x -> 
    x.next <- KMap.empty;
    x.skip <- TMap.empty;
  | Leaf _ -> assert false

let update index term k = 
  Trie.update index term k

let add index term x = 
  update index term (fun _term l ->
    dassert (fun () -> _term == term);
    l := x :: !l
  )

let filter index term f = 
  update index term (fun _term l ->
    dassert (fun () -> _term == term);
    l := List.filter (fun x -> not (f x)) !l
  )


(* TODO: check if efficient? *)
(* let rec skip' n index : 'a index list = 
  if n = 0 then
    [index]
  else
    DTM.fold_level0 (fun key index' acc -> 
      let m = 
        match key with
        | Sym s -> Symbol.get_arity s
        | Var _ -> 0
      in
      skip' (n - 1 + m) index' @ acc
    ) index []

let skip = skip' 0

(* Alternative, also build term (needed for substs) *)
let rec skip_and_term' n index : (term * 'a index) list = 
  if n = 0 then
    [index]
  else
    DTM.fold_level0 (fun key index' acc -> 
      match key with
      | Sym s -> 
        let arity = Symbol.get_arity s
        let n' = n - 1 + 


      let m = 
        match key with
        | Sym s -> Symbol.get_arity s
        | Var _ -> 0
      in
      skip' (n - 1 + m) index' @ acc
    ) index []

let skip = skip' 0

  if n = 0 then *)



(* Aux functions: var and symbol have same type *)
let compatible_type_vs var sym = 
  Symbol.compatible_basic_types (Var.get_type var) (Symbol.get_val_type_def sym)

let compatible_type_vv var1 var2 = 
  Symbol.compatible_basic_types (Var.get_type var1) (Var.get_type var2)
  
let compatible_type_vt var t = 
  Symbol.compatible_basic_types (Var.get_type var) (Term.get_term_type t)
  (* match t with
  | Term.Fun (sym, _, _) -> compatible_type_vs var sym
  | Term.Var (var', _) -> compatible_type_vv var var *)



let rec instantiations' k subst (index: 'a t) terms =
  dbg D_trace @@ lazy (sprintf "terms: %s at: %x" (List.X.to_string Term.to_string terms) (addressof ~bits:24 index));
  match terms with
  | Term.Fun (sym, args, _) :: tl -> 
    begin match Trie.next index (Sym sym) with
    | Some index' -> 
      let terms' = Term.arg_to_list args @ tl in
      instantiations' k subst index' terms'
    | None -> ()  (* No valid element *)
    end

  | Term.Var (var, _) :: tl ->
    begin match Subst.find_opt var subst with
    | Some y -> (* *)
      (* Already bound to [y]: must match this exact term *)
      begin match Trie.skip index |> TMap.find_opt y with
      | Some index' -> 
        let terms' = tl in
        instantiations' k subst index' terms'
      | None -> ()
      end

    | None ->
      (* Unbound: for all possible bindings in this node, try with x bound 
         to that variable *)
      (* DTM.fold_level0 (fun key trie' acc -> *)
      Trie.skip index |> TMap.iter (fun term index' ->
        dbg D_trace @@ lazy (sprintf "adding subst %s / %s" (Var.to_string var) (Term.to_string term));
        let subst' = subst |> Subst.add var term in
        let terms' = tl in
        instantiations' k subst' index' terms';
        dbg D_trace @@ lazy (sprintf "undoing subst %s / %s" (Var.to_string var) (Term.to_string term));
      )
    end

  | [] -> 
    let (term, list) = Trie.get_from_leaf index in
    k term subst list
    (* k (Trie.get_from_leaf index) subst *)

(* Unroll first iteration, to do type-checking *) 
let instantiations'_aux k subst (index: 'a t) terms =
  dbg D_trace @@ lazy (sprintf "terms: %s at: %x" (List.X.to_string Term.to_string terms) (addressof ~bits:24 index));
  match terms with
  | Term.Fun (sym, args, _) :: tl -> 
    begin match Trie.next index (Sym sym) with
    | Some index' -> 
      let terms' = Term.arg_to_list args @ tl in
      instantiations' k subst index' terms'
    | None -> ()
    end

  | Term.Var (var, _) :: tl ->
      Trie.skip index |> TMap.iter (fun term index' ->
        if compatible_type_vt var term then (
          dbg D_trace @@ lazy (sprintf "adding subst %s / %s" (Var.to_string var) (Term.to_string term));
          let subst' = subst |> Subst.add var term in
          let terms' = tl in
          instantiations' k subst' index' terms';
          dbg D_trace @@ lazy (sprintf "undoing subst %s / %s" (Var.to_string var) (Term.to_string term));
        )
      )

  | [] -> assert false

let instantiations index term = 
  dbg D_trace @@ lazy (sprintf "instantiations: %s" (Term.to_string term));
  dbg_env D_trace (fun () -> Trie.print_dbg stdout index);
  let l = ref [] in
  (instantiations'_aux [@inlined]) (fun term subst list -> List.X.cons_ref (term,subst,!list) l) (Subst.create()) index [term];
  dbg D_trace @@ lazy (sprintf "instantiations (result): %s" (List.X.to_string (fun (_,x,_) -> Subst.to_string x) !l));
  !l



let rec generalisations' k subst index terms = 
  dbg D_trace @@ lazy (sprintf "terms: %s at: %x" (List.X.to_string Term.to_string terms) (addressof ~bits:24 index));
  match terms with
  | (Term.Fun (sym, args, _) as hd) :: tl -> 
    index |> Trie.iter_all_next (fun key index' -> 
      match key with
      | Sym sym' ->
        if sym' == sym then
          let terms' = Term.arg_to_list args @ tl in
          generalisations' k subst index' terms'
      | Var var -> 
        begin match Subst.find_opt var subst with
        | Some y -> 
          if y == hd then
            let terms' = tl in
            generalisations' k subst index' terms'
        | None -> 
          let subst' = subst |> Subst.add var hd (*term*) in
          let terms' = tl in
          generalisations' k subst' index' terms'
        end
    )

  | (Term.Var (var, _) as hd) :: tl -> 
    index |> Trie.iter_all_next (fun key index' -> 
      match key with
      | Sym sym' ->
        ()
      | Var var' -> 
        begin match Subst.find_opt var' subst with
        | Some y -> 
          if y == hd then
            let terms' = tl in
            generalisations' k subst index' terms'
        | None -> 
          let subst' = subst |> Subst.add var' hd (*term*) in
          let terms' = tl in
          generalisations' k subst' index' terms'
        end
    )

  | [] -> 
    let (term, list) = Trie.get_from_leaf index in
    k term subst list
    (* k (Trie.get_from_leaf index) subst *)

let generalisations'_aux k subst index terms = 
  dbg D_trace @@ lazy (sprintf "terms: %s at: %x" (List.X.to_string Term.to_string terms) (addressof ~bits:24 index));
  match terms with
  | (Term.Fun (sym, args, _) as hd) :: tl -> 
    index |> Trie.iter_all_next (fun key index' -> 
      match key with
      | Sym sym' ->
        if sym' == sym then
          let terms' = Term.arg_to_list args @ tl in
          generalisations' k subst index' terms'
      | Var var -> 
        if compatible_type_vs var sym then
          let subst' = subst |> Subst.add var hd (*term*) in
          let terms' = tl in
          generalisations' k subst' index' terms'
    )

  | (Term.Var (var, _) as hd) :: tl -> 
    index |> Trie.iter_all_next (fun key index' -> 
      match key with
      | Sym sym' ->
        ()
      | Var var' -> 
        if compatible_type_vv var var' then
          let subst' = subst |> Subst.add var' hd (*term*) in
          let terms' = tl in
          generalisations' k subst' index' terms'
    )

  | [] -> assert false

let generalisations index term = 
  dbg D_trace @@ lazy (sprintf "generalisations: %s" (Term.to_string term));
  dbg_env D_trace (fun () -> Trie.print_dbg stdout index);
  let l = ref [] in
  (generalisations'_aux [@inlined]) (fun term subst list -> List.X.cons_ref (term,subst,!list) l) (Subst.create()) index [term];
  dbg D_trace @@ lazy (sprintf "generalisations (result): %s" (List.X.to_string (fun (_,x,_) -> Subst.to_string x) !l));
  !l



let generalisations_iter index term f = 
  dbg D_trace @@ lazy (sprintf "generalisations: %s" (Term.to_string term));
  (generalisations'_aux [@inlined]) (fun term' subst list -> 
    dbg D_trace @@ lazy (sprintf "generalisations (result): %s @ %s" (Term.to_string term') (Subst.to_string subst));
    dassert (fun () -> Subst.apply_subst_term term_db_ref subst term' == term);
    f term' subst !list
  ) (Subst.create()) index [term]

let instantiations_iter index term f = 
  dbg D_trace @@ lazy (sprintf "instantiations: %s" (Term.to_string term));
  (instantiations'_aux [@inlined]) (fun term' subst list -> 
    dbg D_trace @@ lazy (sprintf "instantiations (result): %s @ %s" (Term.to_string term') (Subst.to_string subst));
    dassert (fun () -> Subst.apply_subst_term term_db_ref subst term == term');
    f term' subst !list
  ) (Subst.create()) index [term]





(* let test () = 
  let f = create_symbol "f" in
  let g = create_symbol "f" in *)
