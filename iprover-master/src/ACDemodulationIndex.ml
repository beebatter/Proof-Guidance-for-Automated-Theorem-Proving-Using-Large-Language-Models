open Lib
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr =
  | D_trace
  | D_special

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_special -> "special"

let dbg_groups = [
  D_trace;
  D_special;
]


(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



(** This uses a BasicSubsumptionIndex with the default features (tuned for 
    subsumption, not necessarily good for AC demod). *)
module Elt (* : BasicSubsumptionIndex.Elt with type t = term *) = struct 
  type t = term
 
  let subsumes ~subs_bck_mult a b = 
    let a_terms = AC.ac_subterms a in
    let b_terms = AC.ac_subterms b in
    Unif.subsumes_lits ~subs_bck_mult a_terms b_terms

  let get_lits = AC.ac_subterms

  let equal = (==)
  let compare = Term.compare
  let hash = Term.hash

  let to_string = Term.to_string
end

module FeatureCom = struct
  module F = BasicSubsumptionIndex.DefaultFeatureCom

  type t = F.t

  type elt = term

  let get_feature_list x = 
    F.get_feature_list (AC.ac_subterms x)

  let compare_pos = F.compare_pos
  let compare_val = F.compare_val

  let to_string = F.to_string
end

module Index = BasicSubsumptionIndex.MakeComData (Elt) (FeatureCom)



(** This module deals with special equations where a stronger demodulation 
    rule applies, compared to the general rule. *)
module SpecialEqs = struct
  (* f(x,x) = x *)
  let is_idempotence l r = 
    match l,r with
    | Term.Fun (sym, args, _), Term.Var(x, _) ->
      let args = Term.arg_to_list args in 
      begin match args with
      | [Term.Var (x1, _); Term.Var (x2, _)] when Var.O.(x == x1 && x == x2) ->
        (* Clause.assign_is_ac_axiom true clause; *)
        Some sym
      | _ -> None
      end
    | _ -> None

  (* f(x,f(x,y)) = f(x,y) *)
  let is_idempotence_2 l r = 
    match l,r with
    | Term.Fun (sym1, args1, _), Term.Fun (sym2, args2, _) -> 
      if sym1 == sym2 then (
        let args1 = Term.arg_to_list args1 in 
        match args1 with
        | [Term.Var (x, _); (Term.Fun (sym, args3, _) as t)] -> 
          if t == r then (
            let args2 = Term.arg_to_list args2 in 
            let args3 = Term.arg_to_list args3 in 
            match args2, args3 with
            | [Term.Var (x1, _); Term.Var (y1, _)], [Term.Var (x2, _); Term.Var (y2, _)] ->
              if Var.O.(x == x1 && x1 == x2 && y1 == y2) then
                Some sym1
              else
                None
            | _ -> None
          ) else None
        | _ -> None
      ) else None
    | _ -> None

  let[@inline] f_clause f_lit clause = 
    match Clause.get_lits clause with
    | [lit] -> 
      begin match Term.Eq.decompose_atom lit with
      | Some (l,r) -> f_lit l r
      | None -> None
      end
    | _ -> None

  let is_idempotence = f_clause is_idempotence
  let is_idempotence_2 = f_clause is_idempotence_2



  module Map = struct
    type t = (clause * Bit_vec.bit_vec) SMap.t

    let empty = SMap.empty

    let add sym c n = 
      SMap.update sym (function 
        | Some (c, x) -> Some (c, x |> Bit_vec.set true n)
        | None        -> Some (c, Bit_vec.false_vec |> Bit_vec.set true n)
      )

    let remove sym n = 
      SMap.update sym (function 
        | Some x -> Some (x |> Bit_vec.set false n)
        | None -> None
      )
  end

  type t = {
    idempotence: Map.t;
  }

  let empty = {
    idempotence = Map.empty;
  }

  let[@warning "-23"] add c idx = 
    dbg D_special @@ lazy (sprintf "add: %s" (Clause.to_string_tptp c));
    match is_idempotence c with
    | Some sym -> 
        {idx with idempotence = idx.idempotence |> Map.add sym c 0}
    | None -> 
    match is_idempotence_2 c with
    | Some sym -> 
        {idx with idempotence = idx.idempotence |> Map.add sym c 1}
    | None -> 
      idx

  let has_idempotence sym idx = 
    (* let all = Bit_vec.false_vec |> Bit_vec.set true 0 |> Bit_vec.set true 1 in *)
    match idx.idempotence |> SMap.find_opt sym with
    | Some (c, x) -> (* if x == all then *) Some c (* else None *)
    | None -> None
end



type t = {
  (* unit_index stores lhs->rhs + clause, indexed by lhs, for each AC symbol *)
  mutable unit_index: (term * term * clause) Index.index SMap.t;
  (* unit_index stores term + clause + optionally other side for completeness check, for all f-subterms, indexed by term, for each AC symbol f *)
  mutable bwd_index: (term * clause * term option) Index.index SMap.t;

  ac_syms: SSet.t;
  order: Orderings.t;
  subs_bck_mult: int;
  complete: bool;
  mutable special_eqs: SpecialEqs.t;
}

let create ~(ac_symbols: AC.Table.t) ~order ~subs_bck_mult ?(complete=true) () = {
  unit_index = SMap.empty;
  bwd_index = SMap.empty;
  ac_syms = ac_symbols.ac |> SMap.to_seq |> Seq.map fst |> SSet.of_seq;
  order;
  subs_bck_mult;
  complete;
  special_eqs = SpecialEqs.empty;
}

let clear index = 
  index.unit_index <- SMap.empty;
  index.bwd_index  <- SMap.empty;
  index.special_eqs <- SpecialEqs.empty;
  ()



let add_equation_indexed_by index sym lhs rhs clause = 
  (* Here, we see if there already exists a subsuming equation *)
  let subsumed idx sym lhs rhs clause = 
    match Index.is_subsumed ~subs_bck_mult:index.subs_bck_mult idx lhs with
    (* | Some (_, subst, (lhs', rhs', clause')::_) ->  *)
    | Some (_, subst, l) -> 
      if Subst.is_renaming subst then (
        let lhs', rhs', clause' = List.hd l in
        let[@inline] ac1 x = AC.ac_subterms x in
        let[@inline] ac2 x = match Term.get_top_symb x with s when s == sym -> AC.ac_subterms x  | _ | exception Term.Var_term -> [x] in
        let[@inline] minus x y = List.X.multiset_minus ~cmp:Term.compare_fast_key x y in
        if List.X.equal ~eq:(==) (minus (ac1 lhs) (ac1 lhs')) (minus (ac2 rhs) (ac2 rhs')) then (
          dbg D_trace @@ lazy (sprintf "%s acsubsumed by %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp clause'));
          true
        ) else (
          false
        )
      ) else (
        false
      )
    | _ -> 
      false
  in
  dassert (fun () -> sym == Term.get_top_symb lhs);
  (* dbg D_trace @@ lazy "ac, really adding"; *)
  let special_eqs' = index.special_eqs |> SpecialEqs.add clause in
  if special_eqs' != index.special_eqs then
    index.special_eqs <- special_eqs'
  else
    index.unit_index <- index.unit_index |> SMap.update sym (fun x ->
      match x with
      | None -> 
        let idx = Index.create () in
        Index.add idx lhs (lhs, rhs, clause);
        Some idx
      | Some idx -> 
        if not @@ subsumed idx sym lhs rhs clause then
          Index.add idx lhs (lhs, rhs, clause);
        x
    )

let add_clause_indexed_by index term clause do_check = 
  let sym = Term.get_top_symb term in
  index.bwd_index <- index.bwd_index |> SMap.update sym (fun x ->
    match x with
    | None -> 
      let idx = Index.create () in
      Index.add idx term (term, clause, do_check);
      Some idx
    | Some idx -> 
      Index.add idx term (term, clause, do_check);
      x
  )

let elim_equation_indexed_by index lhs rhs clause = 
  let sym = Term.get_top_symb lhs in
  index.unit_index <- index.unit_index |> SMap.update sym (fun x ->
    begin match x with
    | None -> 
      dbg D_trace @@ lazy "attempted to remove nonexisting equation"
    | Some idx -> 
      (* dassert (fun () lhs == lhs' == rhs == rhs' && clause *)  (* TODO *)
      try Index.filter idx lhs (fun (lhs', rhs', clause') -> Clause.Bc.(clause == clause')) 
      with Not_found -> dbg D_trace @@ lazy "attempted to remove nonexisting equation"
    end;
    x
  )

let elim_clause_indexed_by index term clause = 
  let sym = Term.get_top_symb term in
  index.bwd_index <- index.bwd_index |> SMap.update sym (fun x ->
    begin match x with
    | None -> 
      dbg D_trace @@ lazy "attempted to remove nonexisting clause"
    | Some idx -> 
      try Index.filter idx term (fun (term', clause', do_check) -> Clause.Bc.(clause == clause'))
      with Not_found -> dbg D_trace @@ lazy "attempted to remove nonexisting clause"
    end;
    x
  )



let is_subsumed_strict index term = 
  match index.unit_index |> SMap.find_opt (Term.get_top_symb term) with
  | Some idx -> Index.is_subsumed_strict ~subs_bck_mult:index.subs_bck_mult idx term 
  | None -> None
  | exception Term.Var_term -> None

let find_subsumed index term = 
  match index.bwd_index |> SMap.find_opt (Term.get_top_symb term) with
  | Some idx -> Index.find_subsumed ~subs_bck_mult:index.subs_bck_mult idx term 
  | None -> []
  | exception Term.Var_term -> []



(* Currently only uses oriented equations *)
let condition_to_add syms sym l r = 
  SSet.mem (sym) syms 
  && Term.get_num_of_symb l > Term.get_num_of_symb r  (* TODO: replace by actual weight *)

(* Invariant: assumes it is an AC equation *)
let add_equation index clause = 
  dbg D_trace @@ lazy (sprintf "Add clause to fwd ACdemod: %s" (Clause.to_string_tptp clause));
  if Clause.is_ac_axiom clause then () else
  match Clause.get_lits clause with
  | [lit] ->
    begin match Term.Eq.decompose_atom_type lit with
    | Some (etype,l,r) ->
      if Term.get_top_symb etype != Symbol.symb_bool_type then (
        dbg D_trace @@ lazy (sprintf "AC syms: %s" (List.X.to_string Symbol.to_string (SSet.elements index.ac_syms)));
        match index.order.oriented lit with
        | GT -> 
          let sym = Term.get_top_symb l in
          if condition_to_add index.ac_syms sym l r then (
            dbg D_trace @@ lazy (sprintf "Added unit eq %s (oriented >)" (Term.to_string lit));
            add_equation_indexed_by index sym l r clause
          )
        | LT -> 
          let sym = Term.get_top_symb r in
          if condition_to_add index.ac_syms sym r l then (
            dbg D_trace @@ lazy (sprintf "Added unit eq %s (oriented <)" (Term.to_string lit));
            add_equation_indexed_by index sym r l clause
          )
        | EQ -> 
          assert false
        | _ ->
          ()
       )
    | None -> ()
    end
  | _ -> ()

let elim_equation index clause = 
  dbg D_trace @@ lazy (sprintf "Remove clause from fwd ACdemod: %s" (Clause.to_string_tptp clause));
  match Clause.get_lits clause with
  | [lit] ->
    begin match Term.Eq.decompose_atom_type lit with
    | Some (etype,l,r) ->
      if Term.get_top_symb etype != Symbol.symb_bool_type then (
        (* dbg D_trace @@ lazy (sprintf "AC syms: %s" (List.X.to_string Symbol.to_string (SMap.bindings syms |> List.map fst)));/ *)
        match index.order.oriented lit with
        | GT -> 
          let sym = Term.get_top_symb l in
          if condition_to_add index.ac_syms sym l r then (
            dbg D_trace @@ lazy (sprintf "Del unit eq %s (oriented >)" (Term.to_string lit));
            elim_equation_indexed_by index l r clause
          )
        | LT -> 
          let sym = Term.get_top_symb r in
          if condition_to_add index.ac_syms sym r l then (
            dbg D_trace @@ lazy (sprintf "Del unit eq %s (oriented <)" (Term.to_string lit));
            elim_equation_indexed_by index r l clause
          )
        | EQ -> 
          assert false
        | _ ->
          ()
       )
    | None -> ()
    end
  | _ -> ()



let rec bwd_loop_terms syms t_set t = 
  match t with
  | Term.Fun (sym, args, _) -> 
    (* if Symbol.get_arity sym = 2 then ( *)
    if SSet.mem sym syms then (
      dbg D_trace @@ lazy (sprintf "loop: %s" (Term.to_string t));
      let subterms = AC.ac_subterms t in
      t_set @= TMap.add t None;
      List.iter (bwd_loop_terms syms t_set) subterms
    ) else (
      List.iter (bwd_loop_terms syms t_set) (Term.arg_to_list args)
    )
  | Term.Var _ -> ()

let bwd_loop_args syms t_set t = 
  match t with
  | Term.Fun (_, args, _) ->
    List.iter (bwd_loop_terms syms t_set) (Term.arg_to_list args)
  | Term.Var _ -> ()

let bwd_loop ~(order: Orderings.t) syms complete clause = 
  let terms = ref TMap.empty in
  let no_check = Clause.demod_no_check clause in
  Clause.get_lits clause |> List.iter (fun lit ->
    let sign, atom = Term.split_sign_lit lit in
    match Term.Eq.decompose_lit_type atom with
    | Some (sign, etype, l, r) ->
      if complete then (
        let[@inline] loop_ac syms terms l r = 
          dbg D_trace @@ lazy (sprintf "loop: %s %s" (Term.to_string l) (Option.to_string Term.to_string r));
          let subterms = AC.ac_subterms l in
          terms @= TMap.add l r;
          List.iter (bwd_loop_terms syms terms) subterms
        in
        let[@inline] loop_nonac syms terms args = 
          List.iter (bwd_loop_terms syms terms) (Term.arg_to_list args)
        in
        match l, r with
        | Term.Fun (sym1, args1, _), Term.Fun (sym2, args2, _) -> 
          let is_ac_l = SSet.mem sym1 syms in
          let is_ac_r = SSet.mem sym2 syms in
          if not is_ac_l && not is_ac_r then (
            (* None is AC, just go to subterms without checking orientedness *)
            loop_nonac syms terms args1;
            loop_nonac syms terms args2;
          ) else (
            (* Some top level rewrite, must check *)
            begin match order.oriented lit with
            | GT -> 
              let check_l = if no_check then None else Some r in
              if is_ac_l then loop_ac syms terms l (check_l) else loop_nonac syms terms args1;
              if is_ac_r then loop_ac syms terms r (None)    else loop_nonac syms terms args2;
            | LT -> 
              let check_r = if no_check then None else Some l in
              if is_ac_l then loop_ac syms terms l (None)    else loop_nonac syms terms args1;
              if is_ac_r then loop_ac syms terms r (check_r) else loop_nonac syms terms args2;
            | INC -> 
              let check_l = if no_check then None else Some r in
              let check_r = if no_check then None else Some l in
              if is_ac_l then loop_ac syms terms l (check_l) else loop_nonac syms terms args1;
              if is_ac_r then loop_ac syms terms r (check_r) else loop_nonac syms terms args2;
            | EQ -> assert false
            end
          )
        | Term.Fun (sym, args, _), Term.Var _ -> 
          (* l cannot be < r, so *)
          if SSet.mem sym syms then loop_ac syms terms l (Some r) else loop_nonac syms terms args;
        | Term.Var _, Term.Fun (sym, args, _) -> 
          (* r cannot be < l, so *)
          if SSet.mem sym syms then loop_ac syms terms r (Some l) else loop_nonac syms terms args;
        | Term.Var _, Term.Var _ -> 
          ()
      ) else (
        bwd_loop_terms syms terms l;
        bwd_loop_terms syms terms r;
      )
    (* If nonequality literal, index subterms *)
    | None ->
      bwd_loop_args syms terms lit
  );
  !terms

let add_bwd_clause index clause = 
  dbg D_trace @@ lazy (sprintf "Add clause to bwd ACdemod: %s" (Clause.to_string_tptp clause));
  if Clause.is_ac_axiom clause then () else (
    let terms = bwd_loop ~order:index.order index.ac_syms index.complete clause in
    dbg D_trace @@ lazy ("traversal complete, actually adding");
    terms |> TMap.iter (fun t do_check ->
      dbg D_trace @@ lazy (sprintf "add %s" (Term.to_string t));
      add_clause_indexed_by index t clause do_check;
    )
  )

let elim_bwd_clause index clause = 
  dbg D_trace @@ lazy (sprintf "Remove clause to bwd ACdemod: %s" (Clause.to_string_tptp clause));
  let terms = bwd_loop ~order:index.order index.ac_syms false clause in  (* Skip completion checks, faster to simply remove *)
  dbg D_trace @@ lazy ("traversal complete, actually removing");
  terms |> TMap.iter (fun t do_check ->
    dbg D_trace @@ lazy (sprintf "remove %s" (Term.to_string t));
    elim_clause_indexed_by index t clause (* do_check *);
  )



let get_fwd index term = 
  match is_subsumed_strict index term with
  | Some (term, subst, l) -> 
    let lhs, rhs, clause = List.hd l in  (* TODO: Check! *)
    dassert (fun () -> term == lhs);
    Some (lhs, rhs, subst, clause)
  | None -> None

let get_bwd index term = 
  find_subsumed index term
  |> List.map (fun (term, subst, l) ->
    let l' = 
      l |> List.map (fun (t, clause, do_check) ->
        dassert (fun () -> term == t);
        (clause, do_check)
      )
    in
    (term, subst, l')
  )

let get_fwd_special index term = 
  let sym = Term.get_top_symb term in
  (* match index.special_eqs |> SpecialEqs.has_idempotence sym with *)
  match index.special_eqs.idempotence |> SMap.find_opt sym with
  | Some (parent, bv) -> 
    (* let bv_all = Bit_vec.false_vec |> Bit_vec.set true 0 |> Bit_vec.set true 1 in *)
    (* if bv == bv_all || (dassert (fun () -> bv == Bit_vec.false_vec |> Bit_vec.set true 0); ) *)
    let terms = AC.ac_subterms term in
    let any_changed = ref false in
    let terms' = 
      terms |> List.fold_left (fun acc x -> 
        let acc' = TSet.add x acc in
        if acc == acc' then any_changed := true;
        acc'
      ) TSet.empty
    in
    if !any_changed then
      Some (AC.mk_term sym (TSet.elements terms'), parent)
    else
      None
  | None -> None

let is_special_axiom clause = 
  Option.is_some (SpecialEqs.is_idempotence clause) 
  || Option.is_some (SpecialEqs.is_idempotence_2 clause) 
