open Lib
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr =
  | D_trace
  | D_trace2
  | D_selection

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_trace2 -> "trace:lv2"
  | D_selection -> "selection"

let dbg_groups = [
  D_trace;
  D_selection;
]

let module_name = "superposition_basic"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =  (* andrepd: this puts \n after *)
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)





(* Type checking, will organise better later *)
let type_check t s = 
  let t_type = Term.get_term_type t in 
  let s_type = Term.get_term_type s in
  t_type == s_type
  (* || t_type == Symbol.symb_type_types 
  || s_type == Symbol.symb_type_types
  || t_type == Symbol.symb_bot_type   
  || s_type == Symbol.symb_bot_type *)

(**  *)
let orient_lit lit = 
  let sign, atom = Term.split_sign_lit lit in 
  match Term.decompose_eq_atom lit with
  | [_;l;r] -> 
    if Orderings.simple_kbo l r == cequal+1 then
      lit
    else if Orderings.simple_kbo r l == cequal+1 then
      add_lit_eq sign (r,l)
    else if Term.get_fast_key l > Term.get_fast_key r then
      lit
    else
      add_lit_eq sign (r,l)
  | _ -> lit

let orient_lit_list l =
  List.map orient_lit l




(* --------------- *)
(* Data structures *)
(* --------------- *)

(** Clause + extra data *)
type clause' = {
  clause: clause;
  sel: lit list;
}

(** Returns maximal literals in clause *)
let sel_clause c = 
  let lits = Clause.get_lits c in
  let selecteds = list_get_max_elements_v Orderings.simple_kbo lits in
  match selecteds with
  | [] -> lits
  | _ -> selecteds

(** Creates a clause' from a clause *)
let create_clause' c =
  {
    clause = c;
    sel = sel_clause c;
  }



(* ------------ *)
(* Demodulation *)
(* ------------ *)

let rec term_iter f t =
  match t with
  | Term.Fun (sym, args, _) ->
    List.iter (term_iter f) (Term.arg_to_list args);
    f t
  | v -> f v


(** Lifts an ordering on terms to clauses *)
let compare_clause ~cmp = 0

(* 
  Preconditions: 
    l > r
    l=r > target_Clause 
  Returns:
    [Some (demodulation conclusion)] if demodulation can be applied
    [None] if not
*)
let demodulation_inner eq l r target_clause : clause option =
  let exception Return of clause in
  try
    target_clause |> Clause.iter (fun lit ->
      lit |> term_iter (fun s ->
        dbg D_trace @@ lazy (Term.to_string s);
        (* if l == s then *)
        try
          let mgu = Unif.matches l s in
          let r' = Subst.apply_subst_term term_db_ref mgu r in
          let source = Clause.TSTP_inference_record (Clause.Demodulation, [target_clause]) in
          let lits' = 
            Clause.get_lits target_clause
            |> List.map (fun x -> add_term_db @@ Term.replace s r' x)
          in
          let eq' = Subst.apply_subst_term term_db_ref mgu eq in
          if List.for_all (fun x -> Orderings.simple_kbo eq' x == cequal+1) (lits') then
            raise (Return (create_clause source lits'))
        with Unif.Matching_failed -> ()
      )
    );
    None
  with Return clause -> Some clause

(* 
  Preconditions: 
    equations are oriented if possible
  Returns:
    [Some (demodulation conclusion)] if demodulation can be applied
    [None] if not
*)
let demodulation eq target_clause =
  match Term.decompose_eq_atom eq with
  | [_;l;r] ->
    (* if Orderings.simple_kbo l r == cequal+1  *)
    assert (Orderings.simple_kbo l r == cequal+1);
    (* && multiset_exstion unit_clause target_clause == cequal+1  *)
    (* if List.for_all (fun x -> Orderings.simple_kbo eq x == cequal+1) (Clause.get_lits target_clause) *)
    (* then *)
      demodulation_inner eq l r target_clause
    (* else *)
      (* None *)
  | _ -> None


(** Simplifies [clause] via unit clauses in [state] as much as possible *)
let demodulation_loop demodulation_equalities clause =
  (** Simplify one clause via one target until we can't anymore *)
  let rec loop target unit =
    match demodulation unit target with
    | Some clause ->
      dbg D_trace @@ lazy (sprintf "Intermediate_demod %s" (Clause.to_string clause));
      loop clause unit
    | None ->
      target
  in
  (* Debug hooks *)
  let loop target unit c =
    let result = loop target unit in
    if result != target then (
      dbg D_trace @@ lazy (sprintf "Demodulated %s" (Clause.to_string target));
      dbg D_trace @@ lazy (sprintf "to %s" (Clause.to_string result));
      dbg D_trace @@ lazy (sprintf "via %s" (Term.to_string unit));
      dbg D_trace @@ lazy (sprintf "in %s" (Clause.to_string c));
    );
    result
  in
  let r = List.fold_left (fun current (term,c) -> 
    dbg D_trace @@ lazy (sprintf "trying %s via %s" (Clause.to_string current) (Term.to_string term));
    if not @@ Clause.equal clause c then 
      loop current term c 
    else (
      dbg D_trace @@ lazy "same clause";
      current
    )
  ) clause demodulation_equalities in
  dbg D_trace @@ lazy "-----";
  r
  
  (* List.fold_left loop clause (List.filter_map (demodulation_equalities *)
  (* List.fold_left (fun current (term,from_clause) -> if x!=c then loop x t else x) clause demodulation_equalities *)
  (* List.fold_left loop clause demodulation_equalities *)





(* ----- *)
(* State *)
(* ----- *)

(* Barebones implementation; can swap underlying data structures for active/passive later *)
type state = {
  mutable active : clause' list;
  mutable passive : clause' Queue.t;

  mutable simplification_set : BCSet.t;

  mutable iteration : int;

  (* mutable demodulation_equalities : term list; *)
  mutable demodulation_equalities : (term*clause) list;
}

let create_state() =
  let r = {
    active = []; 
    passive = Queue.create();

    simplification_set = BCSet.empty;

    iteration = 0;

    demodulation_equalities = [];
  }
  in
  (* List.iter (fun x -> Queue.add (create_clause' x) r.passive) input_clauses; *)
  r

let tautology clause =
  let eq x = 
    match Term.decompose_eq_atom x with
    | [_;l;r] when l == r -> 
      dbg D_trace @@ lazy "[tautology]";
      true
    | _ -> 
      false
  in
  Clause.get_lits clause
  |> List.exists eq

let add_clauses_passive state clauses =
  (* Accept if not existing and if not a tautology *)
  let accept clause =
    dbg D_trace @@ lazy (sprintf "Adding to passive: %s" (Clause.to_string clause));
    let is_new = not @@ BCSet.mem clause state.simplification_set in
    if is_new then
      state.simplification_set <- BCSet.add clause state.simplification_set
    else
      dbg D_trace @@ lazy "[Skipped existing]";
    is_new && not (tautology clause)
  in
  let clauses = List.filter accept clauses in

  (* Checks if it is an oriented (l>r) unit equality *)
  let is_orientable_unit_equality clause = 
    match Clause.get_lits clause with
    | [eq] ->
      begin match Term.decompose_eq_atom eq with
      | [_;l;r] -> 
        if Orderings.simple_kbo l r == cequal+1 then
          Some eq
        else if Orderings.simple_kbo r l == cequal+1 then
          Some (add_lit_eq true (r,l))
        else
          None
      | _ -> None
      end
    | _ -> None
  in

  (* Add unit equalities to demodulation list *)
  List.iter (fun x -> 
    (* if is_unit_equality x then *)
    match is_orientable_unit_equality x with
    | Some y -> 
      state.demodulation_equalities <- (y,x) :: state.demodulation_equalities
    | None -> ()
  ) clauses;

  (* Add to queue, demodulating eagerly *)
  List.iter (fun x -> 
    let x = demodulation_loop state.demodulation_equalities x in
    Queue.add (create_clause' x) state.passive;
  ) clauses

let add_clause_active state clause =
  state.active <- clause :: state.active

exception Empty_passive 
let pop_given_clause state =
  try Queue.pop state.passive
  with Queue.Empty -> raise Empty_passive





(* type replacement = term Bind.t BTMap.t *)
type replacement = SubstBound.bound_replacement

(** Returns list of superposition inferences between [left_clause] and [right_clause] and literals [left_lit] [right_lit] *)
let nonequality_superposition 
    (left_lit: lit) (right_lit: lit) 
    (left_clause: clause) (right_clause: clause) 
    : clause list =
  dbg D_trace @@ lazy (sprintf "nonequality_superposition: %s / %s" (Term.to_string left_lit) (Term.to_string right_lit));

  (* Assert preconditions *)
  assert (Term.is_eq_lit left_lit);
  assert (not @@ Term.is_eq_lit right_lit);

  (* Decompose left side *)
  let [_;l;r] = Term.decompose_eq_atom left_lit in
  (* let Eq_type_symb (_, l, r) = term_eq_view_type_symb left_lit in *)

  (** Succeds with [Some (mgu,replacement)] if l and s are unifiable and l subst >= r subst *)
  let unify l r s : (Unif.bound_subst * replacement) option =
    try
      (* Find mgu *)
      let bound_l = (1,l) in
      let bound_r = (1,r) in
      let bound_s = (2,s) in
      let mgu = Unif.unify_bterms bound_l bound_s in

      (* Apply to l and r *)
      let env = SubstBound.init_renaming_env() in
      let l' = SubstBound.apply_bsubst_bterm' term_db_ref env mgu bound_l in
      let r' = SubstBound.apply_bsubst_bterm' term_db_ref env mgu bound_r in
      
      (* If l > r, success, else fail *)
      if Orderings.simple_kbo r' l' = cequal-1 then
        Some (mgu, (bound_s, bound_r))
      else
        None
    with
    | Unif.Unification_failed | Unif.Unif_type_check_failed -> None
  in

  (** Succeeds with [Some (mgu,replacement)] if a superposition inference
        l=r ∨ C₁    P[s] ∨ C₂
      can be made for s or any subterm. 
   *)
  let rec unify_subterms (s: term) : (Unif.bound_subst * replacement) list =
    match s with
    | Term.Fun (name, args, _) -> 
      let unif_lr = unify l r s in
      let unif_rl = unify r l s in
      let unif_subterms = List.concat @@ List.map (fun term' ->
        unify_subterms term'
      ) (Term.arg_to_list args)
      in

      begin match unif_lr, unif_rl with
      | Some lr, Some rl -> lr :: rl :: unif_subterms 
      | Some lr, None    -> lr       :: unif_subterms 
      | None   , Some rl ->       rl :: unif_subterms 
      | None   , None    ->             unif_subterms 
      end

    | Term.Var _ -> []
  in 

  (** We want to start with the subterms of a literal (skipping the atom itself and a possible negation) *)
  let unify_subterms (lit: lit) =
    let arg_list = Term.arg_to_list @@ Term.get_args @@ Term.get_atom lit in
    List.concat @@ List.map unify_subterms arg_list
  in

  let source = Clause.TSTP_inference_record (Clause.Superposition, [left_clause; right_clause]) in

  (* Map the admissible unifications to inferences *)
  unify_subterms right_lit |> List.map (fun (mgu, replacement) ->
    let env = SubstBound.init_renaming_env() in
    let resolvent' = SubstBound.apply_bsubst_bterm'_replacement ~replacement term_db_ref env mgu (2,right_lit) in

    let left_clause' = 
      Clause.get_lits left_clause
      |> ListExtra.removeq left_lit
      |> Bind.bind 1
      |> Bind.propagate_to_list
      |> SubstBound.apply_bsubst_btlist' term_db_ref env mgu 
    in
    let right_clause' = 
      Clause.get_lits right_clause
      |> ListExtra.removeq right_lit
      |> Bind.bind 2
      |> Bind.propagate_to_list
      |> SubstBound.apply_bsubst_btlist' term_db_ref env mgu 
    in
    let conclusion = 
      (orient_lit resolvent') :: left_clause' @ right_clause'
      |> create_clause source
    in
    conclusion
  )



(** Returns [Some conclusion] if superposition was successful, or [None] *)
let equality_superposition 
    (left_lit: lit) (right_lit: lit) 
    (left_clause: clause) (right_clause: clause) 
    : clause list =
  dbg D_trace @@ lazy (sprintf "equality_superposition: %s / %s" (Term.to_string left_lit) (Term.to_string right_lit));
  (* Assert preconditions *)
  assert (Term.is_eq_atom left_lit);
  assert (Term.is_eq_lit right_lit);

  (* Decompose left side *)
  let [_;l;r] = Term.decompose_eq_atom left_lit in
  (* Decompose right side *)
  let tsign, tatom = Term.split_sign_lit right_lit in
  let [_;tl;tr] = Term.decompose_eq_atom tatom in

  (** Succeds with [Some (mgu,term)] if l and s are unifiable and tl subst >= tr subst *)
  let unify l r tl tr s : (Unif.bound_subst * replacement) option =
    try
      dbg D_trace2 @@ lazy (Printf.sprintf 
        "  %s=%s / %s[%s]=%s"
        (Term.to_string l) (Term.to_string r) 
        (Term.to_string tl) (Term.to_string s) 
        (Term.to_string tr)
      );
      assert (not @@ Term.is_var s);

      (* Find mgu *)
      let bound_l = (1,l) in
      let bound_r = (1,r) in
      let bound_tl = (2,tl) in
      let bound_tr = (2,tr) in
      let bound_s = (2,s) in
      let mgu = Unif.unify_bterms bound_l bound_s in
      dbg D_trace2 @@ lazy (Printf.sprintf "%s and %s unfiable" (Term.to_string l) (Term.to_string s));

      (* Apply to tl and tr *)
      let env = SubstBound.init_renaming_env() in
      let tl' = SubstBound.apply_bsubst_bterm' term_db_ref env mgu bound_tl in
      let tr' = SubstBound.apply_bsubst_bterm' term_db_ref env mgu bound_tr in

      if Orderings.simple_kbo tr' tl' = cequal-1 then (
        (* dbg D_trace2 @@ lazy (Printf.sprintf "%s !>= %s" (Term.to_string tr') (Term.to_string tl')); *)
        Some (mgu, (bound_s, bound_r))
      ) else (
        (* dbg D_trace2 @@ lazy (Printf.sprintf "%s >= %s" (Term.to_string tr') (Term.to_string tl')); *)
        None
      )
    with
    | Unif.Unification_failed | Unif.Unif_type_check_failed -> None
  in

  (** Succeeds with [Some term] if a superposition inference
        l=r ∨ C₁    tl[s]=tr ∨ C₂
      can be made for s or any subterm. 
   *)
  let rec unify_subterms tl tr (s: term) : (Unif.bound_subst * replacement) list =
    match s with
    | Term.Fun (name, args, _) -> 
      let unif_l = unify l r tl tr s in 
      let unif_r = unify r l tl tr s in
      let unif_subterms = List.concat @@ List.map (fun term' ->
        unify_subterms tl tr term'
      ) (Term.arg_to_list args)
      in
      
      begin match unif_l, unif_r with
      | Some l, Some r -> l :: r :: unif_subterms 
      | Some l, None   -> l      :: unif_subterms 
      | None  , Some r ->      r :: unif_subterms 
      | None  , None   ->           unif_subterms 
      end

    | Term.Var _ -> []
  in 
  let unify_subterms tl tr = 
    if Term.is_var tl || Orderings.simple_kbo tr tl = cequal-1 then
      unify_subterms tl tr tl 
    else
      []
  in

  let source = Clause.TSTP_inference_record (Clause.Superposition, [left_clause; right_clause]) in

  (** Return the conclusion, given an mgu and a substitution env *)
  let create_conclusion env mgu resolvent' =
    let left_clause' = 
      Clause.get_lits left_clause
      |> ListExtra.removeq left_lit
      |> Bind.bind 1
      |> Bind.propagate_to_list
      |> SubstBound.apply_bsubst_btlist' term_db_ref env mgu
    in
    let right_clause' = 
      Clause.get_lits right_clause
      |> ListExtra.removeq right_lit
      |> Bind.bind 2
      |> Bind.propagate_to_list
      |> SubstBound.apply_bsubst_btlist' term_db_ref env mgu
    in
    let conclusion = 
      (orient_lit resolvent') :: left_clause' @ right_clause'
      |> create_clause source
    in
    conclusion
  in
  
  (* Perform inferences with the both literals in right-side (i.e. try tl=tr and tr=tl) *)
  (
    unify_subterms tl tr |> List.map (fun (mgu, replacement) ->
      let env = SubstBound.init_renaming_env() in
      let tl' = SubstBound.apply_bsubst_bterm'_replacement ~replacement term_db_ref env mgu (2,tl) in
      let tr' = SubstBound.apply_bsubst_bterm'              term_db_ref env mgu (2,tr) in
      let resolvent' = add_lit_eq tsign (tl',tr') in
      create_conclusion env mgu resolvent'
    )
  ) @ (
    unify_subterms tr tl |> List.map (fun (mgu, replacement) ->
      let env = SubstBound.init_renaming_env() in
      let tl' = SubstBound.apply_bsubst_bterm'              term_db_ref env mgu (2,tl) in
      let tr' = SubstBound.apply_bsubst_bterm'_replacement ~replacement term_db_ref env mgu (2,tr) in
      let resolvent' = add_lit_eq tsign (tl',tr') in
      create_conclusion env mgu resolvent'
    )
  )



(** Apply all superposition inferences between [given] and [second], under selection function [sel] *)
let superposition_clause (given: clause') (second: clause') : clause list =
  dbg D_selection @@ lazy ("given " ^ Clause.to_string given.clause);
  dbg D_selection @@ lazy ("second " ^ Clause.to_string second.clause);

  let f lit1 lit2 clause1 clause2 =
    if Term.is_eq_atom lit1 then
      if Term.is_eq_lit lit2 then
        equality_superposition (lit1) (lit2) clause1 clause2
      else
        nonequality_superposition (lit1) (lit2) clause1 clause2
    else
      []
  in

  given.sel |> ListExtra.concat_map (fun given_sel ->
    dbg D_selection @@ lazy ("given_sel: " ^ Term.to_string given_sel);
    second.sel |> ListExtra.concat_map (fun second_sel ->
      dbg D_selection @@ lazy ("second_sel: " ^ Term.to_string second_sel);
      (* Try both ways *)
      f given_sel second_sel given.clause second.clause
      @
      f second_sel given_sel second.clause given.clause
    )
  )

(** Apply all superposition inferences between [given] and all the clauses in [set], under selection function [sel] *)
let superposition (given: clause') (set: clause' list) : clause list =
  dbg D_trace @@ lazy "superposition";
  ListExtra.concat_map (fun x -> superposition_clause given x) set



(* ----- *)



(** Returns the resulting clause if lit can be eliminated from clause via equality resolution, otherwise returns None *)
let equality_resolution_inner lit clause : clause option =
  if Term.is_neg_lit lit then (
    match Term.decompose_eq_atom (Term.get_atom lit) with
    (* Is equality *)
    | [_;l;r] ->
      begin try
        dbg D_trace2 @@ lazy (Printf.sprintf "%s in %s" (Term.to_string lit) (Clause.to_string clause));
        (* let mgu = Unif.matches l r in *)
        let mgu = Unif.unify_bterms (1,l) (1,r) in
        dbg D_trace2 @@ lazy (Printf.sprintf "%s and %s unifiable" (Term.to_string l) (Term.to_string r));
        (* dbg D_trace2 @@ lazy (Printf.sprintf "into %s and %s" 
          (Term.to_string @@ Subst.apply_subst_term term_db_ref mgu l) (Term.to_string @@ Subst.apply_subst_term term_db_ref mgu r)); *)
        let new_clause = 
          Clause.get_lits clause
          |> ListExtra.removeq lit 
          (* |> List.map (Subst.apply_subst_term term_db_ref mgu) *)
          |> Bind.bind 1 |> Bind.propagate_to_list
          |> SubstBound.apply_bsubst_btlist term_db_ref mgu
        in
        dbg D_trace2 @@ lazy (Printf.sprintf "yielding %s" (Term.term_list_to_string new_clause));
        let source = Clause.TSTP_inference_record (Clause.Equality_resolution, [clause]) in
        Some (create_clause source new_clause)
      with 
      | Unif.Unification_failed | Unif.Unif_type_check_failed -> None
      end
    (* Isn't equality *)
    | _ -> None
  ) else (
    None
  )  

let equality_resolution clause : clause list =
  dbg D_trace @@ lazy "equality_resolution";
  clause.sel |> ListExtra.filter_map (fun lit ->
    equality_resolution_inner lit clause.clause
  )



let equality_factoring_inner lit1 lit2 clause : clause option =
  (* Decompose *)
  match Term.decompose_eq_atom lit1, Term.decompose_eq_atom lit2 with
  (* Both equality *)
  | [_;s;t], [_;s';t'] -> 
    (* TD *)
    begin try
      (* Find mgu *)
      let mgu = Unif.matches s s' in
      let subst_s = Subst.apply_subst_term term_db_ref mgu s in
      let subst_t = Subst.apply_subst_term term_db_ref mgu t in
      let subst_t' = Subst.apply_subst_term term_db_ref mgu t' in

      if Orderings.simple_kbo subst_t  subst_s = cequal-1 
      && Orderings.simple_kbo subst_t' subst_t = cequal-1
      then (
        let new_clause = 
          Clause.get_lits clause 
          |> ListExtra.removeq lit2
          (* |> List.cons (add_neg_atom @@ add_fun_term Symbol.symb_typed_equality [t;t']) *)
          |> List.cons (orient_lit @@ add_lit_eq false (t,t'))
          |> List.map (Subst.apply_subst_term term_db_ref mgu)
        in
        let source = Clause.TSTP_inference_record (Clause.Equality_factoring, [clause]) in
        Some (create_clause source new_clause)
      )
      else
        None
    with
    | Unif.Matching_failed -> None    
    end
  (* Non-equality *)
  | _, _ -> None


let equality_factoring clause : clause list =
  dbg D_trace @@ lazy "equality_factoring";
  clause.sel |> ListExtra.concat_map (fun first ->
    dbg D_selection @@ lazy ("first " ^ Term.to_string first);
    clause.sel |> ListExtra.filter_map (fun second ->
      if first == second then
        None
      else (
        dbg D_selection @@ lazy ("second " ^ Term.to_string second);
        equality_factoring_inner first second clause.clause
      )
    )
  )





(* --------- *)
(* Main loop *)
(* --------- *)

let main_loop state =
  (* eprintf "It: %4d  Num clauses: %6d\n" (state.iteration) (BCSet.cardinal state.simplification_set); flush stderr; *)
  dbg D_trace @@ lazy (sprintf "--START %d superposition loop" state.iteration);
  let given = pop_given_clause state in
  add_clause_active state given;
  dbg D_trace @@ lazy (sprintf "--GIVEN: %s)" (Clause.to_string given.clause));

  let new_clauses = 
    superposition given state.active
    @ equality_resolution given
    @ equality_factoring given
  in

  dbg_env D_trace (fun () -> 
    dbg D_trace @@ lazy "New clauses:";
    List.iter (fun x -> dbg D_trace @@ lazy (Clause.to_string x)) new_clauses;
    dbg D_trace @@ lazy "End.";
  );

  (* let new_clauses = List.map (demodulation_loop state) new_clauses in *)

  dbg_env D_trace (fun () -> 
    dbg D_trace @@ lazy "New clauses:";
    List.iter (fun x -> dbg D_trace @@ lazy (Clause.to_string x)) new_clauses;
    dbg D_trace @@ lazy "End.";
  );

  add_clauses_passive state new_clauses;

  dbg D_trace @@ lazy (sprintf "--NUM_CLAUSES: %d" (BCSet.cardinal state.simplification_set));
  dbg D_trace @@ lazy "--END superposition loop";

  state.iteration <- succ state.iteration;

  ()








(* --- Equality transformation --- *)

let tt = add_fun_term (Symbol.symb_top) []

let lit_to_eq lit =
  dbg D_trace @@ lazy (sprintf "Rectifying %s" (Term.to_string lit));
  if Term.is_eq_lit lit then (
    lit
  ) else (
    let sign, atom = Term.split_sign_lit lit in
    add_lit_eq sign (atom,tt)
  )

let litlist_to_eq l =
  List.map lit_to_eq l

let clause_to_eq clause =
  let lits = Clause.get_lits clause in
  (** If all literals in clause were equality then no transformation was made *)
  if List.for_all Term.is_eq_lit lits then
    clause
  (** Otherwise add modified clauses as an inference via [Clause.Predicate_to_equality] *)
  else
    let source = Clause.TSTP_inference_record (Clause.Predicate_to_equality, [clause]) in
    List.map lit_to_eq lits
    |> create_clause source 

let clauselist_to_eq l =
  List.map clause_to_eq l
