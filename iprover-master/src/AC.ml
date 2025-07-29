open Lib
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_trace2
  | D_ac_norm
  | D_subterms
  | D_dag

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_trace2 -> "trace2"
  | D_ac_norm -> "ac_norm"
  | D_subterms -> "subterms"
  | D_dag -> "dag"

let dbg_groups = [
  D_trace;
  (* D_trace2; *)
  D_ac_norm;
  D_subterms;
  (* D_dag; *)
]
    
let module_name = "AC"

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)





(*** Detection of AC axioms ***)

let is_assoc_axiom clause =
  match Clause.get_lits clause with
  | [lit] ->
    if not (Term.get_num_of_symb lit = 12 && Term.get_num_of_var lit = 6) then None else
    begin match Term.Eq.decompose_atom lit with
    | Some (l,r) -> 
      if not (Term.get_num_of_symb l = 5 && Term.get_num_of_var l = 3 
      && Term.get_num_of_symb r = 5 && Term.get_num_of_var r = 3) then None else
      begin match l,r with
      (* f(x,f(y,z)) = f(f(x,y),z) *)
      | Term.Fun (sym1, args1, _), Term.Fun(sym2, args2, _) ->
        if sym1 == sym2 then (
          let args1 = Term.arg_to_list args1 in 
          let args2 = Term.arg_to_list args2 in 
          match args1, args2 with
          | [Term.Var (x1, _); Term.Fun (sym1', args1', _)], [Term.Fun (sym2', args2', _); Term.Var (z2, _)]
          | [Term.Fun (sym2', args2', _); Term.Var (z2, _)], [Term.Var (x1, _); Term.Fun (sym1', args1', _)] ->
            if sym1' == sym2' 
            && sym1 == sym1'
            then (
              let args1' = Term.arg_to_list args1' in 
              let args2' = Term.arg_to_list args2' in 
              match args1', args2' with
              | [Term.Var (y1, _); Term.Var (z1, _)], [Term.Var (x2, _); Term.Var (y2, _)] ->
                if Var.O.(x1 == x2 && y1 == y2 && z1 == z2) 
                && Var.O.(x1 != y1 && y1 != z1 && z1 != x1)
                then (
                  Clause.assign_is_ac_axiom true clause;
                  Some sym1
                )
                else None
              | _ -> None
            ) else None
          | _ -> None
        ) else None
      | _ -> None
      end
    | None -> None
    end
  | _ -> None

let is_assoc_axiom clause =
  dbg D_trace2 @@ lazy "Checking assoc";
  is_assoc_axiom clause
  |> tap (fun x -> 
    if Option.is_some x then dbg D_trace @@ lazy (sprintf "Assoc axiom: %s" (Clause.to_string_tptp clause))
  )

let is_commut_axiom clause =
  match Clause.get_lits clause with
  | [lit] ->
    if not (Term.get_num_of_symb lit = 8 && Term.get_num_of_var lit = 4) then None else
    begin match Term.Eq.decompose_atom lit with
    | Some (l,r) ->
      if not (Term.get_num_of_symb l = 3 && Term.get_num_of_var l = 2 
      && Term.get_num_of_symb r = 3 && Term.get_num_of_var r = 2) then None else
      begin match l,r with
      (* f(x,y) = f(y,x) *)
      | Term.Fun (sym1, args1, _), Term.Fun(sym2, args2, _) ->
        if sym1 == sym2 then (
          let args1 = Term.arg_to_list args1 in 
          let args2 = Term.arg_to_list args2 in 
          match args1, args2 with
          | [Term.Var (x1, _); Term.Var (y1, _)], [Term.Var (y2, _); Term.Var (x2, _)] ->
            if Var.O.(x1 == x2 && y1 == y2) 
            && Var.O.(x1 != y1)
            then (
              Clause.assign_is_ac_axiom true clause;
              Some sym1
            )
            else None
          | _ -> None
        ) else None
      | _ -> None
      end
    | None -> None
    end
  | _ -> None

let is_commut_axiom clause =
  dbg D_trace2 @@ lazy "Checking commut";
  is_commut_axiom clause
  |> tap (fun x -> 
    if Option.is_some x then dbg D_trace @@ lazy (sprintf "Commut axiom: %s" (Clause.to_string_tptp clause))
  )



module Table = struct
  type t = {
    ac: (clause * clause) SMap.t;
    assoc: clause SMap.t;
    commut: clause SMap.t;
    axiom_list: clause list;
  }

  let empty = {
    ac = SMap.empty;
    assoc = SMap.empty;
    commut = SMap.empty;
    axiom_list = [];
  }

  let add table clause = 
    dbg D_trace2 @@ lazy (sprintf "Checking ac axiom: %s" (Clause.to_string_tptp clause));
    match is_assoc_axiom clause with
    | Some s -> 
      let assoc = table.assoc |> SMap.add s clause in
      begin match table.commut |> SMap.find_opt s with
      | Some c_clause -> 
        dbg D_trace @@ lazy (sprintf "New ac symbol: %s" (Symbol.to_string s));
        (* dbg D_trace @@ lazy (sprintf "Added onto 0x%x" (Obj.magic prob_props : int)); *)
        let ac = table.ac |> SMap.add s (clause, c_clause) in
        let axiom_list = clause :: c_clause :: table.axiom_list in
        { table with assoc; ac; axiom_list }, Some (s, clause, c_clause) 
      | None -> 
        { table with assoc }    , None
      end
    | None ->
    match is_commut_axiom clause with
    | Some s -> 
      let commut = table.commut |> SMap.add s clause in
      begin match table.assoc |> SMap.find_opt s with
      | Some a_clause -> 
        dbg D_trace @@ lazy (sprintf "New ac symbol: %s" (Symbol.to_string s));
        (* dbg D_trace @@ lazy (sprintf "Added onto 0x%x" (Obj.magic prob_props : int)); *)
        let ac = table.ac |> SMap.add s (a_clause, clause) in
        let axiom_list = a_clause :: clause :: table.axiom_list in
        { table with commut; ac; axiom_list }, Some (s, a_clause, clause) 
      | None -> 
        { table with commut }    , None
      end
    | None -> 
      table, None

  let populate table clauses = 
    dbg D_trace @@ lazy "populate";
    let table' = 
      clauses |> List.fold_left (fun table c ->
        dbg D_trace2 @@ lazy (sprintf "Checking ac axiom: %s" (Clause.to_string_tptp c));
        match is_assoc_axiom c with
        | Some s -> 
          { table with assoc = table.assoc |> SMap.add s c }
        | None ->
        match is_commut_axiom c with
        | Some s -> 
          { table with commut = table.commut |> SMap.add s c }
        | None -> 
          table
      ) table
    in
    let axioms_ref = ref [] in
    let ac = 
      SMap.merge (fun s a_clause c_clause ->
        match a_clause, c_clause with
        | Some a, Some c -> 
          dbg D_trace @@ lazy (sprintf "New ac symbol: %s" (Symbol.to_string s));
          axioms_ref := a :: c :: !axioms_ref;
          Some (a,c)
        | _ -> None
      ) table'.assoc table'.commut
    in
    let axiom_list = !axioms_ref in
    let table'' = { table' with ac; axiom_list } in
    table''

  let has_ac table = 
    dassert (fun () -> 2 * SMap.cardinal table.ac = List.length table.axiom_list);
    List.X.is_nonempty table.axiom_list
end



let extra_ac_axioms ~parents sym =
  let typ = Symbol.get_val_type_def sym in
  let x = Term.create_var_term (Var.create typ 0) in
  let y = Term.create_var_term (Var.create typ 1) in
  let z = Term.create_var_term (Var.create typ 2) in
  let app = Term.create_fun_term sym in
  let source = Clause.TSTP_inference_record (Clause.Superposition, parents) in
  let make_clause l r = 
    create_clause source [add_typed_equality_sym typ l r]
    |> tap (Clause.assign_is_ac_axiom true)
  in

  let l = app [x ; app [y ; z]] in
  let axiom1 =
    let r = app [y ; app [x ; z]] in
    make_clause l r
  in
  (* let axiom2 =
    let r = app [z ; app [y ; x]] in
    make_clause l r
  in
  let axiom3 =
    let r = app [y ; app [z ; x]] in
    make_clause l r
  in *)

  [axiom1; (* axiom2; axiom3 *)]
  |> tap (fun x -> dbg D_trace @@ lazy (sprintf "Adding ac axioms %s" (List.X.to_string Clause.to_string_tptp ~first:"" ~last:"" ~sep:" " x)))



(*** AC normalisation ***)

type ac_operator_set = (clause*clause) SMap.t

(* Helper function: find and collect terms in a tree of f applications, e.g.
   f(a,f(b,c),d,f(f(e,g),h))  ->  [a;b;c;d;e;g;h] *)
let ac_subterms' ac_sym t =
  (* TODO change to functional *)
  let l = ref [] in
  let rec loop t = 
    match t with 
    | Term.Var _ -> 
      l := List.cons t !l
    | Term.Fun (sym, args, _) -> 
      if sym == ac_sym then
        let args = Term.arg_to_list args in
        List.iter loop args
      else
        l := List.cons t !l 
  in
  loop t;
  List.rev !l

(* Memoised in term *)
let ac_subterms t = 
  dbg D_subterms @@ lazy (sprintf "ac_subterms: %s" (Term.to_string t));
  match t with
  | Term.Fun (sym, _, info) (* when sym == ac_sym *) -> 
    begin match Term.get_ac_subterms_info info with
    | Some terms -> 
      dbg D_subterms @@ lazy (sprintf "memoised: %s" (List.X.to_string Term.to_string terms));
      terms
    | None -> 
      (* Optimisation, only store for binary symbols of type α × α → α *)
      let terms = 
        if Symbol.get_arity sym = 2 
        && (match Symbol.get_stype_args_val_def sym with ([a;b],c) when a == b && b == c -> true | _ -> false)
        then (
          let terms = ac_subterms' sym t in
          dbg D_subterms @@ lazy (sprintf "computed: %s" (List.X.to_string Term.to_string terms));
          terms
        ) else (
          [t]
        )
      in
      Term.set_ac_subterms_info terms info;
      terms
    end
  | _ -> 
    invalid_arg "AC.ac_subterms: not a function term"
    (* [t] *)



let rec ac_fold_right f l = 
  match l with
  | [a;b] -> 
    f a b
  | hd::tl -> 
    f hd (ac_fold_right f tl)
  | [] -> 
    invalid_arg "AC.ac_fold_right: l has length < 2"

let mk_term sym subterms = 
  let f x y = Term.create_fun_term sym [x;y] in
  match subterms with
  | [x] -> x
  | _::_ -> add_term_db @@ ac_fold_right f subterms
  | [] -> assert false



(* Main function to AC-normalise a term, and its subterms recursively, 
   parametrised by a [sort] function. *)
let rec normalise_ac' ~sort ~order_uid operators t =
  match t with
  | Term.Var _ -> t
  | Term.Fun (sym, args, info) ->
    if order_uid <> -1 && Term.get_is_ac_normalised_info info order_uid operators then (
      dbg D_trace2 @@ lazy (sprintf "Already normalised: %s" (Term.to_string t));
      t 
    ) else (
      let t' = 
        let args = Term.arg_to_list args in
        if not @@ SMap.mem sym operators then (
          let args' = List.map (normalise_ac' ~sort ~order_uid operators) args in
          (* TODO this patttern should probably be a function in Logic_interface *)
          if List.for_all2 (==) args args' then
            t (* unchanged *)
          else
            add_fun_term sym args'
        ) else (
          let subterms = 
            ac_subterms t
            |> tap (fun x -> dbg D_trace @@ lazy (sprintf "Collected subterms: %s" (List.X.to_string Term.to_string x)))
            |> List.map (normalise_ac' ~sort ~order_uid operators)
            |> sort
            |> tap (fun x -> dbg D_trace @@ lazy (sprintf "Sorted subterms: %s" (List.X.to_string Term.to_string x)))
          in
          (* If [sym] is AC, then it has at arity 2, so [subterms] is at least length 2. So we 
            can disable the exhaustiveness warning in the following line. *)
          (* let[@warning "-8"] first :: rest = subterms in
          let t' = List.fold_right (fun x y -> add_fun_term sym [x;y]) rest first in *)
          (* If unchanged, shortcut and don't try to add term *)
          if List.X.for_all2' (==) subterms args then (
            t
          ) else (
            let t' = mk_term sym subterms in
            let store_ac_subterms subterms t = 
              match t with Term.Fun (_, _, info) -> Term.set_ac_subterms_info subterms info | Term.Var _ -> assert false
            in
            store_ac_subterms subterms t';  (* Optimisation *)
            dbg D_trace @@ lazy (sprintf "Normalised %s" (Term.to_string t));
            dbg D_trace @@ lazy (sprintf "to         %s" (Term.to_string t'));
            add_term_db t'
          )
        )
      in
      Term.set_is_ac_normalised_info (Term.get_fun_info t') order_uid operators;
      t'
    )



(* Concrete versions *)

(* Total order extension of normal term ordering *)
(* let compare_total_ext order_terms = 
  PartialOrd.partial_cmp_to_total 
    order_terms (lex_combination3 
      (Ord.lift Term.get_num_of_symb Int.compare)
      (Ord.lift Term.is_ground (Ord.reverse_f Bool.compare))
      (Term.compare_fast_key)
    ) *)

(* let normalise_ac_incomplete ~order_terms operators t =
  let sort = List.sort (compare_total_ext order_terms) in
  normalise_ac' ~sort operators t *)

let normalise_ac_complete_old ~order_terms ~order_uid operators t =
  (* Bubble sort, don't move adjacent elements if they are INC *)
  let rec bubble_sort partial_cmp l = 
    let changed = ref false in
    let rec inner partial_cmp l =  
      match l with
      | a::(b::rest as tl) -> 
        if partial_cmp a b == PartialOrd.GT then (
          changed := true;
          b :: inner partial_cmp (a :: rest)
        ) else (
          a :: inner partial_cmp (tl)
        )
      | [_] | [] -> l
    in 
    match l with
    | _::_::_ -> 
      let l' = inner partial_cmp l in
      if !changed then
        let[@warning "-8"] hd::tl = l' in
        hd :: bubble_sort partial_cmp tl
      else
        l	
    | [_] | [] -> l
  in
  let sort = bubble_sort order_terms in
  normalise_ac' ~sort ~order_uid operators t



(* let normalise_ac_complete_old ~order_terms operators t =
  let cmp x y = 
    let open PartialOrd in
    match order_terms x y with
    | EQ -> Ord.eq
    | GT -> Ord.gt
    | LT -> Ord.lt
    | INC -> Ord.eq  (* Stable sort won't change the order of elements that compare to eq, which is what we want *)
  in
  let sort = List.stable_sort cmp in 
  normalise_ac' ~sort operators t *)



(* This builds a dag that represents the partial ordering among a number of 
   terms. This ensures worst case is n*(n-1)/2 comparisons. It also avoids 
   comparisons when it can: for example if a>b>c, and adds a d<c, then also
   adds d<b, d<a. *)
module Graph = Ordering_graph.Make(TMap)

(* NOT USED *)  
let normalise_ac_complete_v1 ~order_terms ~order_uid operators t = 
  (* let rec sort l = 
    match l with
    | hd::tl ->
      (* We always need to compare all pairs  *)
      let minimal_tl = List.X.min_elements_partial_ord order_terms tl in
      begin match minimal_tl |> List.find_opt (fun x -> order_terms x hd == PartialOrd.LT) with
      | Some x -> 
        let tl' = hd :: List.X.removeq x tl in
        x :: sort_total_ext tl'
      | None ->
        hd :: sort tl
      end
    | [] -> []
  in *)

  let sort l = 
    (* Sort ground part in nlog(n) *)
    let ground, nonground = l |> List.partition (Term.is_ground) in
    let ground_sorted = 
      ground |> List.sort (fun x y -> PartialOrd.to_ord (order_terms x y))
    in
    (* If there is no nonground part, we are done *)
    match nonground with
    | [] -> ground_sorted
    (* Otherwise, use the dag *)
    | _::_ ->
      let g = Graph.make l in
      Graph.add_sorted g ground_sorted;
      Graph.add_with_many g order_terms nonground;
      dbg D_dag @@ lazy (sprintf "Graph %s:\n" (Graph.to_string_dbg Term.to_string g));
      (* Then run the same algorithm as below, but now all comparisons are cached *)
      let order_terms_cached = Graph.get_unsafe g in
      let rec loop l = 
        match l with
        | hd::tl ->
          (* OLD *) 
          (* let minimal_tl = List.X.min_elements_partial_ord order_terms_cached tl in
          begin match minimal_tl |> List.find_opt (fun x -> order_terms_cached x hd == PartialOrd.LT) with
          | Some x -> 
            let tl' = hd :: List.X.removeq x tl in
            let sort_total_ext = List.sort (compare_total_ext order_terms_cached) in
            x :: sort_total_ext tl'
          | None ->
            hd :: loop tl
          end *)

          (* KK *)
          let min = List.X.min_below_partial_ord order_terms_cached hd tl in 
          (* simple version without dag for order cache *) 
          (* let min = List.X.min_below_partial_ord order_terms hd tl in *)
          if hd == min then  (* there is no el in tl smaller than hd *)
            hd :: loop tl
          else
             let tl' = hd :: List.X.removeq min tl in
             min :: loop tl'
        | [] -> []
      in
      loop l
  in

  (* For 2 and 3 elements (occurs commonly), write special case to avoid overheads *)
  let sort l = 
    match l with
    | [] | [_] -> 
      assert false
    | [a;b] -> 
      if order_terms b a == LT then [b;a] else l
    (* | [a;b;c] ->
      if order_terms b a == LT then 
        if (* compare_total_ext *) order_terms a c == LT then [b;a;c] else [b;c;a]
      else if order_terms c a == LT then
        [c;a;b]
      else
        if order_terms c b == LT then [a;c;b] else l *)
    | _ -> 
      sort l
  in
  normalise_ac' ~sort ~order_uid operators t


(* guarantees a canonical AC normal form *)
(* assumes order_terms is total on ground terms *)
let normalise_ac_complete_v2 ~order_terms ~order_uid operators t = 
  let sort l = 
    dbg D_ac_norm @@ lazy (sprintf "sort: %s" (Term.term_list_to_string l));
    try 
      let result = List.sort (fun x y -> PartialOrd.to_ord (order_terms x y)) l in
      (* if there are incomparable elementes sorting will always raise Incomparable; *)
      (* for sanity we can assert (is_linearly_sorted result) which check for INC *)
      dassert (fun () -> List.X.is_linearly_sorted order_terms result);
      result
    with PartialOrd.Incomparable -> 
      let ground, nonground = l |> List.partition (Term.is_ground) in
      let ground_sorted = 
        ground |> List.sort (fun x y -> PartialOrd.to_ord (order_terms x y))
      in
      (* match nonground with [] -> ground_sorted | _ -> *) (* not needed as it will be comparable and catched by above *)
      (* DAG for caching partial order comparisions: order_terms to order_terms_cached  *)
      let g = Graph.make l in
      Graph.add_sorted g ground_sorted;
      Graph.add_with_many g order_terms nonground;
      dbg D_dag @@ lazy (sprintf "Graph %s:\n" (Graph.to_string_dbg Term.to_string g));
      (* Then run the same algorithm as below, but now all comparisons are cached *)
      let order_terms_cached = Graph.get_unsafe g in
      
      if List.X.is_topologically_sorted_simp order_terms_cached l then 
        l (* should not change in any way to preserve completness *)
      else
        let pre_norm = List.sort Term.compare_fast_key l in (* this gives some canonical representation *)
        List.X.topological_sort_simp order_terms_cached pre_norm (* returns canonical AC as we start with a canonical form and  topological_sort_simp is deteministic *)
  in
  if SMap.is_empty operators then t else  (* This is already checked by the caller; still keep in case called in place when it is not checked *)
  normalise_ac' ~sort ~order_uid operators t



(* Issues with the above: will not necessarily yield a smaller form; doesn't cache some comparisons leading to wasted work *)
let normalise_ac_complete_v3 ~order_terms ~order_uid operators t = 
  let sort l = 
    dbg D_ac_norm @@ lazy (sprintf "sort: %s" (Term.term_list_to_string l));
    let g = Graph.make l in
    let order_terms_cached = Graph.wrap g order_terms in
    try 
      let result = List.sort (fun x y -> PartialOrd.to_ord (order_terms_cached x y)) l in
      dassert (fun () -> List.X.is_linearly_sorted order_terms result);
      dbg D_dag @@ lazy (sprintf "Graph:\n%s" (Graph.to_string_dbg Term.to_string g));
      dbg D_ac_norm @@ lazy (sprintf "linearly sorted: %s" (Term.term_list_to_string result));
      result
    with PartialOrd.Incomparable -> 
      dbg D_dag @@ lazy (sprintf "Graph:\n%s" (Graph.to_string_dbg Term.to_string g));
      let rec loop l = 
        match l with
        | hd::tl -> 
          let minimals = 
            List.filter (fun x -> order_terms_cached x hd == LT) tl 
            |> List.X.min_elements_partial_ord order_terms_cached
          in
          begin match minimals with
          (* No minimal element, recurse *)
          | [] -> 
            dbg D_trace2 @@ lazy (sprintf "No smaller than %s" (Term.to_string hd));
            hd :: loop tl
          (* Minimal elements. Now to be consistent we need to choose e.g. the smallest wrt.\ fast_key (note of course these are not necessarily comparable wrt order_terms) *)
          | _::_ -> 
            let minimal = List.X.min (Term.compare_fast_key) minimals in
            dbg D_trace2 @@ lazy (sprintf "Swapping %s and smaller %s (among %s)" (Term.to_string hd) (Term.to_string minimal) (List.X.to_string Term.to_string minimals));
            let tl' = 
              hd :: List.X.removeq minimal tl
              |> List.sort (Term.compare_fast_key)
              |> List.X.topological_sort_simp (order_terms_cached)
            in
            (* dbg D_ac_norm @@ lazy (sprintf "preorder yields %s" (List.X.to_string Term.to_string pre_norm)); *)
            minimal :: tl'
          end
        | [] -> []
      in
      let result = loop l in
      dbg D_dag @@ lazy (sprintf "Graph:\n%s" (Graph.to_string_dbg Term.to_string g));
      dbg D_ac_norm @@ lazy (sprintf "manually sorted: %s" (Term.term_list_to_string result));
      result
  in
  let sort l = 
    match l with
    | [x;y] ->
      if order_terms y x == LT then [y;x] else [x;y]
    | _ -> 
      sort l
  in
  dbg D_ac_norm @@ lazy (sprintf "normalise_ac_complete: %s" (Term.to_string t));
  let t' = 
    if SMap.is_empty operators then t else  (* This is already checked by the caller; still keep in case called in place when it is not checked *)
    normalise_ac' ~sort ~order_uid operators t
  in
  dbg D_ac_norm @@ lazy (sprintf "normalise_ac_c result: %s" (Term.to_string t'));
  t'


(* check with KK v4 previous commit *)
(* let normalise_ac_complete = normalise_ac_complete_v4 *)

let normalise_ac_complete = normalise_ac_complete_v3 

let normalise_ac_fastkey operators t =
  normalise_ac' ~sort:(List.sort Term.compare_fast_key) ~order_uid:(-1) operators t



(* Faster way to check equality modulo AC, without normalising both terms (which
   may be expensive because of inserting into global dag, etc.) *)
let rec equal_mod_ac operators s t =
  s == t || 
  match s, t with 
  | Term.Fun _, Term.Var _ 
  | Term.Var _, Term.Fun _ ->
    false
  | Term.Fun (sym1, args1, _), Term.Fun (sym2, args2, _) ->
    (* If different operators, not equal *)
    if sym1 != sym2 
    (* Precondition!! Weights equal, vars equal *)
    || Term.get_num_of_symb s != Term.get_num_of_symb t
    || Term.get_num_of_var  s != Term.get_num_of_var  t
    then (
      false
    ) 
    (* If non-AC operator, simply recurse for all arguments *)
    else if not @@ SMap.mem sym1 operators then (
      List.for_all2 (equal_mod_ac operators) 
        (Term.arg_to_list args1) 
        (Term.arg_to_list args2)
    )
    (* If AC *)
    else (
      let subterms_s = 
        ac_subterms s 
        |> List.map (normalise_ac_fastkey operators)
        |> List.sort Term.compare_fast_key
      in
      let subterms_t = 
        ac_subterms t
        |> List.map (normalise_ac_fastkey operators)
        |> List.sort Term.compare_fast_key
      in
      List.X.equal ~eq:(==) subterms_s subterms_t
    )
  | Term.Var (v1, _), Term.Var (v2, _) ->
    Var.O.(v1 == v2)
