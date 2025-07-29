open Lib
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_contents

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_contents -> "contents"

let dbg_groups = [
  D_trace;
  D_contents; 
]
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
  
(*----- debug -----*)



(* -------------- *)
(* Data structure *)
(* -------------- *)

type elt = {
  value: term;
  equalities: BCSet.t;
}

(* type t = THtbl *)
type t = {
  mutable table: elt TMap.t;
  (* trie: (term ref) UnifIndexDiscrTree.t; *)
  order: ordering;
  eq_types: Symbol.sym_set;
  (* For each subterm of the equalities, index the lhs of the equation it belongs to *)
  bwd_index: term UnifIndexDiscrTree.t;
}

let fwd_use_ac = false

let create ~order ~eq_types () = {
  table = TMap.empty;
  (* trie = UnifIndexDiscrTree.create(); *)
  order;
  eq_types;
  bwd_index = UnifIndexDiscrTree.create();

}

let clear x =
  x.table <- TMap.empty;
  UnifIndexDiscrTree.clear x.bwd_index

(* For debugging *)
let to_string x =
  let buf = Buffer.create 4000 in
  x.table |> TMap.iter (fun l v -> 
    let {value=r;_} = v in
    Buffer.add_string buf (sprintf "%s → %s\n" (Term.to_string l) (Term.to_string r))
  );
  Buffer.contents buf

(* TODO: maybe make this toggleable with commandline option *)
let use_non_ground = true

let _ = out_warning (module_name^":use_non_ground "^(string_of_bool use_non_ground))

(*KK: rename *)
let use_term x =
  if use_non_ground then
    true
  else
    Term.is_ground x



(* Helper function, adds [new_set] to [!old_set] *)
let union_ref old_set new_set =
  old_set := BCSet.union !old_set new_set

let cset_to_string x =
  List.X.to_string Clause.to_string_tptp ~first:"" ~last:"" ~sep:" , " (BCSet.elements x)



(* ---------------- *)
(* Basic operations *)
(* ---------------- *)

(** Look up term *)
let get index equalities_ref term =
  try
    dbg D_trace @@ lazy (sprintf "Look %s" (Term.to_string term));
    let {equalities; value=term'} = TMap.find term index.table in
    dbg D_trace @@ lazy (sprintf "Hit  %s" (Term.to_string term'));
    union_ref equalities_ref equalities;
    dbg D_trace @@ lazy (sprintf "By   %s" (cset_to_string equalities));
    term'
  with Not_found ->
    term

(** Same, but skip same clause *)
let get' index equalities_ref term rhs =
  try
    dbg D_trace @@ lazy (sprintf "Look %s" (Term.to_string term));
    let {equalities; value=term'} = TMap.find term index.table in
    if (* BCSet.mem clause equalities *) term' == rhs then (
      dbg D_trace @@ lazy (sprintf "Skip %s" (Term.to_string term'));
      term
    ) else (
      dbg D_trace @@ lazy (sprintf "Hit  %s" (Term.to_string term'));
      union_ref equalities_ref equalities;
      dbg D_trace @@ lazy (sprintf "By   %s" (cset_to_string equalities));
      term'
    )
  with Not_found ->
    term

let normalise_term index term =
  try
    let {equalities; value=term'} = TMap.find term index.table in
    term', equalities
  with Not_found ->
    term, BCSet.empty



(** Add l to bwd_index, indexed by all subterms of l=r *)
let add_to_bwd index l r = 
  l |> Term.iter_preorder_novar (fun s ->
    UnifIndexDiscrTree.add_elem_to_lit index.bwd_index s l
  );
  r |> Term.iter_preorder_novar (fun s ->
    UnifIndexDiscrTree.add_elem_to_lit index.bwd_index s l
  )

(** Remove l to bwd_index, indexed by all subterms of l=r *)
let remove_from_bwd index l r = 
  l |> Term.iter_preorder_novar (fun s ->
    UnifIndexDiscrTree.elim_filter_from_lit index.bwd_index s (fun x -> x == l)
  );
  r |> Term.iter_preorder_novar (fun s ->
    UnifIndexDiscrTree.elim_filter_from_lit index.bwd_index s (fun x -> x == l)
  )



(** Straight add, no updating or conflict resolution (but also add to bwd index). *)
let add_bare' index eq_clause l r =
  dbg D_trace @@ lazy (sprintf "Add %s → %s (no conflict resolution)" (Term.to_string l) (Term.to_string r));
  index.table <- TMap.add l {value=r; equalities=BCSet.singleton eq_clause} index.table

let add_bare index eq_clause =
  if fwd_use_ac || not (Clause.is_ac_axiom eq_clause) then 
  match Clause.get_lits eq_clause with
  | [eq_term] -> 
    begin match Term.Eq.decompose_atom eq_term with
    (* Positive equality *)
    | Some (l,r) ->
      (* Also must be eligible as per [use_term] *)
      if use_term eq_term then (
        match index.order.oriented eq_term with
        | GT ->
          add_bare' index eq_clause l r
        | LT ->
          add_bare' index eq_clause r l
        | INC ->
          ()
        | EQ -> invalid_arg "Adding s=s to lightnorm index"
      )
    | None -> ()
    end
  | _ -> ()



(** Search for all occurrences of [before], in the index, and replace with [after] *)
let update index eq_clause before after : unit = 
  dbg D_trace @@ lazy (sprintf "Update: %s to %s" (Term.to_string before) (Term.to_string after));
  let candidates = UnifIndexDiscrTree.get_variants index.bwd_index before in
  candidates |> List.iter (fun (s, lst) ->
    dassert (fun () -> s == before);
    lst |> List.iter (fun l ->
      let {equalities; value=r} = TMap.find l index.table in
      (* Rule l=r has subterm [before] in l or r *)
      let l' = add_term_db @@ Term.replace before after l in
      let r' = add_term_db @@ Term.replace before after r in
      (* Remove l=r and add l'=r' *)
      (* Don't forget to add [eq_clause] to the set of equalities that give rule l'=r' *)
      index.table <- index.table |> TMap.remove l;
      index.table <- index.table |> TMap.add l' {value=r'; equalities=BCSet.add eq_clause equalities};
      dbg D_trace @@ lazy (sprintf "Amended %s  %s" (Term.to_string l)  (Term.to_string r) );
      dbg D_trace @@ lazy (sprintf "to      %s  %s" (Term.to_string l') (Term.to_string r'));
    )
  )

(** Add l → r to index and resolve conflict with existing l → r' *)
let add_resolve_conflict index eq_clause l r =
  let flag = ref None in
  dbg D_trace @@ lazy (sprintf "Add %s → %s" (Term.to_string l) (Term.to_string r));
  (* Add but check if l → r' already exists (i.e. a rule with l on the left-hand side) *)
  index.table <- index.table |> TMap.update l (fun original ->
    match original with
    (* If yes, compare r and r' *)
    | Some {value=r'; equalities} ->
      dbg D_trace @@ lazy (sprintf "Conflict with %s → %s" (Term.to_string l) (Term.to_string r'));

      (*KK: Orderings_opt ? should it be Orderings_cache ? *)

      begin match index.order.terms r r' with
      (* If r < r', then replace it with the new binding *)
      | LT -> 
        (* And add r' -> r *)
        flag := Some (r', r);
        dbg D_trace @@ lazy (sprintf "New kept");
        Some {value=r; equalities=BCSet.singleton eq_clause}
      (* If r > r', then keep the old binding *)
      | GT -> 
        (* And add r -> r' *)
        flag := Some (r, r');
        dbg D_trace @@ lazy (sprintf "Old kept");
        original
      (* If INC, or if EQ, keep old and don't add new. *)
      | INC ->
        dbg D_trace @@ lazy (sprintf "Inc kept");
        original
      | EQ ->
        dbg D_trace @@ lazy (sprintf "Eq kept");
        original
      end
    (* If not, add as usual *)
    | None ->
      dbg D_trace @@ lazy (sprintf "No conflict");
      Some {value=r; equalities=BCSet.singleton eq_clause}
  );
  match !flag with
  | Some (a,b) ->
    dbg D_trace @@ lazy (sprintf "Connected %s → %s" (Term.to_string a) (Term.to_string b));
    index.table <- index.table |> TMap.add a {value=b; equalities=BCSet.singleton eq_clause}
  | None -> ()



(** Normalise all existing rules via l → r, then add, resolving conflicts (existing l → r') by choosing the smallest right-hand side. *)
let add_and_update' index eq_clause l r =
  dbg D_trace @@ lazy (sprintf "add_and_update %s to %s" (Term.to_string l) (Term.to_string r));
  update index eq_clause l r;
  add_resolve_conflict index eq_clause l r

let add_and_update index eq_clause =
  if fwd_use_ac || not (Clause.is_ac_axiom eq_clause) then 
  match Clause.get_lits eq_clause with
  | [eq_term] -> 
    begin match Term.Eq.decompose_atom eq_term with
    | Some (l,r) -> 
      (* Also must be eligible as per [use_term] *)
      if use_term eq_term then (
        match index.order.oriented eq_term with
        | GT ->
          add_and_update' index eq_clause l r
        | LT ->
          add_and_update' index eq_clause r l
        | INC ->
          ()
        | EQ -> invalid_arg "Adding s=s to lightnorm index"
      )
    | None -> ()
    end
  | _ -> ()



(** Remove l → r *)
let remove' index eq_clause l r =
  (* Only remove l=r, if l is bound to an r' different from r, don't remove it. *)
  index.table <- index.table |> TMap.update l (fun original ->
    match original with
    | Some {value=r'; _} ->
      if r' == r then
        None
      else 
        original
    | None -> None
  )

let remove index eq_clause =
  match Clause.get_lits eq_clause with
  | [eq_term] -> 
    begin match Term.Eq.decompose_atom eq_term with
    | Some (l,r) -> 
      if true then (
        match index.order.oriented eq_term with
        | GT ->
          remove' index eq_clause l r
        | LT ->
          remove' index eq_clause r l
        | INC ->
          ()
        | EQ -> invalid_arg "Removing s=s from lightnorm index"
      )
    | None -> ()
    end
  | _ -> ()





(* ------------- *)
(* Normalisation *)
(* ------------- *)

let list_equal a b =
  List.X.for_all2' (==) a b



(** Recursively normalise a term and all its subterms.
    The idea: 
      * For a [term] (of form f(args..), not a variable), look it up in the 
        index to get [term']. 
      * If [term == term'], recurse into its arguments, [args], to get 
        [args']. 
        - If all [args] are == to [args'], then f(args) == f(args'), so 
          return [term] and we are done. 
        - Otherwise, construct [term'] = f(args'), AND look *it* up in the 
          index. 
          - Here we either assume that right-hand sides are already reduced, 
            and so we just look f(args) itself in the index, that is, call 
            [get], 
          - or we don't assume this, and we must look up f(args) and its 
            subterms in the index, that is, call [normalise'] recursively. 
      * If [term != term'], 
        - Here again we either assume that right-hand sides in the index are 
          reduced, and we merely return [term'], 
        - or we don't and we must recurse also into [term']. *)

let normalise_term_before_args = true
let normalise_term_after_args  = true
let recurse_into_normalised_subterms = true
let assert_we_get_fixpoint = false

let rec normalise_subterms index equalities_ref term =
  match term with
  | Term.Fun (sym, args, _) ->
    dbg D_trace @@ lazy (sprintf "normalise_subterms: %s" (Term.to_string term));
    (* First, normalise the term itself *)
    let term' = 
      if normalise_term_before_args 
      && use_term term 
      && SSet.mem (Term.get_term_type term) index.eq_types 
      then
        get index equalities_ref term
      else
        term
    in
    if term' != term then
      if recurse_into_normalised_subterms then
        normalise_subterms index equalities_ref term'
      else
        term'  (* No recursion here ONLY because we assume rhs are reduced *)
    else
      let args = dassert (fun () -> term' == term); Term.arg_to_list args in
      let args' = List.map (normalise_subterms index equalities_ref) args in
      if list_equal args args' then
        term
      else
        let term' = add_term_db @@ Term.create_fun_term sym args' in
        let term'' = 
          if normalise_term_after_args then
            get index equalities_ref term'
          else
            term'
        in
        if recurse_into_normalised_subterms && term'' != term' then
          normalise_subterms index equalities_ref term'
        else
          term'
  | Term.Var _ ->
    term


(* KK: why do you need normalise_args instead of normalise ? *)

(** Normalise proper subterms of [term], but not itself. If [term] is 
    normalised to [term'], also do not normalise [term'] (leave it 
    separate). *)
let rec normalise_args index equalities_ref term =
  dbg D_trace @@ lazy (sprintf "normalise_args: %s" (Term.to_string term));
  match term with
  | Term.Fun (sym, args, _) ->
    let args = Term.arg_to_list args in
    let args' = List.map (normalise_subterms index equalities_ref) args in
    if list_equal args args' then
      term
    else
      let term' = add_term_db @@ Term.create_fun_term sym args' in
      (* get index equalities_ref *) term'
  | Term.Var _ ->
    term

(** Normalise only the term itself, but do not recurse into subterms. *)
let normalise_top index equalities_ref term =
  dbg D_trace @@ lazy (sprintf "normalise_top: %s" (Term.to_string term));
  if use_term term 
  && SSet.mem (Term.get_term_type term) index.eq_types 
  then
    get index equalities_ref term
  else
    term

let normalise_top_check index equalities_ref term rhs =
  dbg D_trace @@ lazy (sprintf "normalise_top_check: %s" (Term.to_string term));
  if use_term term 
  && SSet.mem (Term.get_term_type term) index.eq_types 
  then
    get' index equalities_ref term rhs
  else
    term



(** Has to be repeated from simplify_new.ml due to circular dependency *)
type completeness = L of term | R of term | LR of term * term | None

let normalise index ~no_check term = 
  dbg D_trace @@ lazy (sprintf "normalise: %s" (Term.to_string term));
  dbg D_trace @@ lazy (sprintf "via n=%d" (TMap.cardinal index.table));
  dbg D_contents @@ lazy (sprintf "Rules--\n%s-------" (to_string index));

  let eqs = ref BCSet.empty in

  let term' = 
    match Term.Eq.decompose_lit_type term with
    (* Equality literal *)
    | Some (sign, typ,l,r) ->
      (* Rewrites at top *)
      let l' = normalise_top_check index eqs l r in
      let r' = normalise_top_check index eqs r l in

      (* Ordering checks summary ******

         Positive literal:
         l>r
           l=r via l=l' to l'=r
             needs l' < r  (since l'<l is guaranteed)
           l=r via r=r' to l=r'
             needs nothing  (since r'<r<l is guaranteed)
           l=r via l=l' and r=r' to l'=r'
             needs l' < r  (since r'<r<l is guaranteed)
         l?r
           l=r via l=l' to l'=r
             needs l' < r  (since l'<l is guaranteed)
           l=r via r=r' to l=r'
             needs r' < l  (since r'<r is guaranteed)
           l=r via l=l' and r=r' to l'=r'
             needs l' < r and r' < l  (since l'<l and r'<r is guaranteed)

         Negative literal:
         l>r
           l≠r via l=l' to l'≠r
             needs nothing  (since l'<l is guaranteed and {l,l,r,r} > {l,l'} <=> {l,r,r} > {l'} <=> true)
           l≠r via r=r' to l≠r'
             needs nothing  (since r'<r<l is guaranteed)
           l≠r via l=l' and r=r' to l'≠r'
             needs nothing  (since l'<l and r'<r is guaranteed and {l,l,r,r} > {l,l'} <=> {l,r,r} > {l'} <=> true)
         l?r
           l≠r via l=l' to l'≠r
             needs nothing  (since l'<l is guaranteed and {l,l,r,r} > {l,l'} <=> {l,r,r} > {l'} <=> true)
           l≠r via r=r' to l≠r'
             needs nothing  (since r'<r is guaranteed and {l,l,r,r} > {r,r'} <=> {l,l,r} > {r'} <=> true)
           l≠r via l=l' and r=r' to l'≠r'
             needs nothing  (since l'<l is guaranteed and {l,l,r,r} > {l,l'} <=> {l,r,r} > {l'} <=> true
                               and r'<r is guaranteed and {l,l,r,r} > {r,r'} <=> {l,l,r} > {r'} <=> true)
      *)

      (* Ordering checks *)
      let l', r' = 
        (* If unchanged, or no_check==true, no need to check *)
        if l' == l && r' == r 
        then
          l', r'
        (* If at least one has changed, then we need to see if it was on a smaller side or not *)
        else
          match Lazy.force_val no_check with
          | None ->
              dbg D_trace @@ lazy "explicit ordering check None";
            l', r'
          (* If l>r, then must l=r > l=l' => r>r'. If not, we cannot rewrite l. *)
          | L r0 -> 
            dbg D_trace @@ lazy "explicit ordering check for lhs";
            if l' == l || index.order.terms l' r0 == LT then
              l', r'
            else (
              dbg D_trace @@ lazy "failed";
              l , r'
            )
          (* Symmetric of above *)
          | R l0 -> 
            dbg D_trace @@ lazy "explicit ordering check for rhs";
            if r' == r || index.order.terms r' l0 == LT then
              l', r'
            else (
              dbg D_trace @@ lazy "failed";
              l', r
            )
          (* If l?r (incomparable) then in order to rewrite l, l'<r, 
             and to rewrite r, r'<l. *)
          | LR (l0,r0) -> 
            dbg D_trace @@ lazy "explicit ordering check for lhs/rhs";
            let l_okay = l' == l || index.order.terms l' r0 == LT in
            let r_okay = r' == r || index.order.terms r' l0 == LT in
            (if l_okay then l' else (dbg D_trace @@ lazy "failed for lhs"; l)), 
            (if r_okay then r' else (dbg D_trace @@ lazy "failed for rhs"; r))
      in

      let l' = 
        if recurse_into_normalised_subterms && l' != l then
          fix_point (normalise_top index eqs) l'
        else
          l'
      in
      let r' = 
        if recurse_into_normalised_subterms && r' != r then
          fix_point (normalise_top index eqs) r'
        else
          r'
      in

      (* Check if literal was rewritten to s!=s and return straight away *)
      if l' == r' then add_lit_eq sign typ l' r' else
      
      (* Inner rewrites *)
      let l'' = normalise_args index eqs l' in
      let r'' = normalise_args index eqs r' in
      
      if l'' == r'' then add_lit_eq sign typ l'' r'' else
      
      (* Rewrites at top *)
      (* These are always fine: either l''=r'' is the same as the original 
         l=r, and so we don't need to do anything because we already tried to 
         rewrite l=r at the top, before, or l''=r'' is smaller than the 
         original l=r, and so we can safely rewrite and it will always be by
         smaller clauses than the original. *)
      let l''' = 
        if l'' == l' then 
          l' 
        else if recurse_into_normalised_subterms then
          fix_point (normalise_subterms index eqs) l''
        else
          normalise_top index eqs l'' 
      in
      let r''' = 
        if r'' == r' then 
          r' 
        else if recurse_into_normalised_subterms then
          fix_point (normalise_subterms index eqs) r''
        else
          normalise_top index eqs r'' 
      in

      if l''' == l && r''' == r then
        term
      (* else if l' == r && r' = r 
           || l' == l && r' = l then
        term *)
      else
        add_lit_eq sign typ l''' r'''

    (* Nonequality literal *)
    | None ->
      normalise_args index eqs term
  in

  if term' == term then (
    dbg D_trace @@ lazy "Normal form"
  ) else (
    dbg D_trace @@ lazy (sprintf "→NF %s" (Term.to_string term'))
  );
  term' , !eqs
