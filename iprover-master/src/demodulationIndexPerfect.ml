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
  | D_cache
  | D_cache_consist
  | D_subtypes
      
let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_cache -> "cache"
  | D_cache_consist -> "cache_consist"
  | D_subtypes -> "subtypes"
      
let dbg_groups = [  
    D_trace; 
    D_cache;
    
   D_cache_consist;
   
  (* D_subtypes; *)
]


(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module Index = PerfectDiscrTree 


type result = term * ((clause * Subst.subst) option)
  
(* Cache based on top symbol *)
module CacheTop = struct
  type t = {
    mutable map: (result TMap.t) SMap.t;
  }

  let empty() = 
    { map = SMap.empty }

  (* KK *)
   let clear x = 
    x.map <- SMap.empty 

(* clears all terms that t are instances of t, overapprox based on top symb and type in case of var *)        
  let clear_inst t x =
    dbg D_cache @@ lazy (sprintf "clear_inst: %s" (Term.to_string t)); 
    match t with
    |Term.Fun (sym,_,_) ->  x.map <- x.map |> SMap.remove sym
    |Term.Var (var, _) ->
        x.map <- x.map |> SMap.filter (fun sym _ -> Symbol.get_val_type_def sym != (Var.get_type var))
          
 
  let find_opt t x =
    dbg D_cache @@ lazy (sprintf "find_opt: %s" (Term.to_string t)); 
    match t with
    | Term.Fun (sym, _, _) ->
      begin match x.map |> SMap.find_opt sym with
      | Some y -> 
        y |> TMap.find_opt t
      | None -> 
        (* x.map <- x.map |> SMap.add sym TMap.empty; *)  (* KK *)
        None
      end
    | Term.Var _ -> invalid_arg "Should not query demod index with variables"

(* only add s -> t when top(s) = top(t); *)
(* this due to: if we add rule t -> t' then we need also be able to remove s -> t *)
          
  let add t t' x =    
    dbg D_cache @@ lazy (sprintf "add: t: %s t':%s" (Term.to_string t) (Term.to_string (fst t')));    
    
    match (t, (fst t')) with
    | (Term.Fun (sym, _, _),Term.Fun (sym', _, _)) ->
        if sym == sym' then
            begin
              x.map <- x.map |> SMap.update sym (function 
                | Some tmap -> Some (TMap.add t t' tmap)
                | None -> Some (TMap.singleton t t')
                                                )
            end
        else
          (dbg D_cache @@ lazy (sprintf "skip: adding as tops are differnt t: %s t':%s" (Term.to_string t) (Term.to_string (fst t')))           
          )
            
    | (Term.Var _,_) -> invalid_arg "Should not query demod index with variables"
    | _ -> ()
end


(* Cache based on term features *)
(* !TODO rework pos hits; at the moment not correct *)
(* if we have s -> t in the cache  *)
(* and then add rule t -> u (or t' -> u where unif(t,t'))
   to demod then we need also be able to remove s -> t *)

    
(* A feature-based index for caching demodulations;
   using which we eliminate related cache parts to inserted equations that can potentially be demodulated *) 
(* "smaller" can potentially demodulate bigger or equal but bigger can not demodulate smaller *)
(* TODO: compress bool features to single int. as bv *)
(* TODO: compare top symbols,
   Bloom filter approach: encode each symbols, pos in a term as id -> (bit index) modulo max_int ?*)
(* half way still should work i.e. store top symbol id (or -id) then *)   

(* add +type_id; -type_id; top_id and -1 if var *)    
let get_top_sym_feats t =
  match t with
  | Term.Fun (sym, _, _) ->
      let sym_id = Symbol.get_fast_key sym in
      [sym_id; -sym_id]
  | Term.Var _ -> [-max_int;-max_int]

let feat_skip_var f t = if (Term.is_var t) then -max_int else f t
let feat_skip_var_bool f t = if (Term.is_var t) then -max_int else Bool.to_int (f t)
    
let term_to_feats t =
  let open Bool in
  let open Term in
  let feats = 
    [
     get_num_of_symb t; (*  *)
     to_int (is_ground t);  
     (feat_skip_var_bool (get_fun_bool_param has_conj_symb) t);
     (feat_skip_var_bool (get_fun_bool_param has_non_prolific_conj_symb) t);
   ]
    @ (get_top_sym_feats t)
  in
  dbg D_cache @@ lazy (sprintf "term t: %s \n feats: [%s]" (Term.to_string t) (list_to_string string_of_int feats ",")); 
  feats
                         
module FCM = Feature_cache.Make(Int)
    
module CacheFeat = struct
  
  type t = {
      mutable map: (result TMap.t) FCM.index;
    }

  let empty() = 
    { map = FCM.empty }

  let clear x = 
    x.map <- FCM.empty 

  let tmap_to_str tmap =
    let str_list = 
      TMap.fold 
        (fun t (t',_) str_list ->
          (sprintf "%s->%s" (Term.to_string t) (Term.to_string t'))::str_list
        )
        tmap
        []
    in
    "["^(list_to_string id_fun str_list ",")^"]"
    
  let out_cache x =
    FCM.iter
      (fun tmap ->
        dbg D_cache @@ lazy (sprintf "%s\n" (tmap_to_str tmap));             
      ) x
      
  let clear_inst t x =
    dbg D_cache @@ lazy (sprintf "clear_inst: %s" (Term.to_string t));
    dbg D_cache @@ lazy (sprintf "Cache before clear:\n");
    dbg_env D_cache (fun () -> out_cache x.map);
    x.map <- FCM.get_less (term_to_feats t) x.map;
    dbg D_cache @@ lazy (sprintf "\n\nCache after clear:\n");
    dbg_env D_cache (fun () -> out_cache x.map)
   
  let find_opt t x =
    dbg D_cache @@ lazy (sprintf "find_opt: %s" (Term.to_string t)); 
    try
      x.map |> FCM.find (term_to_feats t) |>
      TMap.find_opt t
    with
      Not_found -> None

            

  let add t t' x =
    dbg D_cache @@ lazy (sprintf "add: t: %s t':%s" (Term.to_string t) (Term.to_string (fst t'))); 
    x.map <-
      x.map |> FCM.app_leaf
        (fun opt ->
          match opt with
          |Some tmap -> Some(TMap.add t t' tmap)
          |None -> Some(TMap.singleton t t')
        ) (term_to_feats t) |> Option.get 
    
end

module Cache=CacheTop

(* !TODO rework pos hits; CacheFeat  at the moment not correct *)    
(*  module Cache=CacheFeat *)
    
type t = {
  (* mutable *) unit_index : (int * lit * clause) Index.t;
  (* In bwd_index, we store the clause and either the smaller right-hand side, to quickly do a demodulation check, or none, if no check is needed *)
  (* mutable *) bwd_index : (clause * lit option) Index.t;

  order: Orderings.t;

  (* This is the set of all equality types which are also not pure disequality. *)
  eq_types: Symbol.sym_set;
  use_ground: bool;
  reverse: bool;
  ac_table: AC.Table.t;

  cache: Cache.t;
}

let create ~order ~eq_types ~use_ground ~ac_table ?(reverse=false) () =
  {
    unit_index = Index.create();
    bwd_index = Index.create();
    order;
    eq_types;
    use_ground;
    reverse;
    ac_table;
    cache = Cache.empty();
  }

let clear ui = 
  Cache.clear ui.cache;
  Index.clear ui.unit_index;
  Index.clear ui.bwd_index

let add_equation_indexed_by ui indexed_by pos lit clause =
  dbg D_trace @@ lazy (sprintf "add_equation_indexed_by: ind_by: %s pos: %s lit: %s cl: %s"
    (Term.to_string indexed_by) (string_of_int pos) (Term.to_string lit) (Clause.to_string clause)
  );
  (* clear cache based on top symbol *)
  Cache.clear_inst indexed_by ui.cache;
  Index.add ui.unit_index indexed_by (pos, lit, clause)

let add_clause_indexed_by ui indexed_by ~check clause =
  Index.add ui.bwd_index indexed_by (clause, check)

let elim_equation_indexed_by ui indexed_by clause =
  dbg D_trace @@ lazy (sprintf "elim_equation_indexed_by: ind_by: %s cl: %s"
    (Term.to_string indexed_by) (Clause.to_string clause)
  );
  (* clear cache based on top symbol *)
  Cache.clear_inst indexed_by ui.cache;

  Index.filter ui.unit_index indexed_by (fun (_, _, x) -> Clause.Bc.(x == clause))

let elim_clause_indexed_by ui indexed_by clause =
  Index.filter ui.bwd_index indexed_by (fun (x, _) -> Clause.Bc.(x == clause))



(* True if lit is x=t with x not in t (and therefore lit not oriented), or if 
   lit is x=y with x!=y *)
let is_universal ui lit l r = 
  match l,r with
  | Term.Var _ , Term.Fun _
  | Term.Fun _ , Term.Var _ ->
    (match ui.order.oriented lit with GT | LT -> false | INC | EQ -> true)
  | Term.Var _, Term.Var _ ->
    assert(l != r); true
  | Term.Fun _, Term.Fun _ ->
    false



(* KK: TODO: move to options *)
let fwd_only_orientable_flag = false

(* AC axioms prolific and not necessary for demod *)
(* let fwd_use_aci = false *)
let fwd_use_aci = true

let _ = out_warning (sprintf "%s: demodulationIndex fwd_only_orientable_flag: %B" module_name fwd_only_orientable_flag)





let add_equation ui clause = 
  (* Huge problem!! It's only good to remove A axioms or C axioms if A *and* C are present! Not if only A or only C are there. *)
  match Clause.get_lits clause with
  | [lit] ->
    if ui.use_ground || not @@ Term.is_ground lit then
    begin match Term.Eq.decompose_atom_type lit with
    | Some (etype,l,r) -> 
      if not fwd_use_aci && Clause.is_ac_axiom clause && SMap.mem (Term.get_top_symb l) ui.ac_table.ac then
        false
      else if Term.get_top_symb etype != Symbol.symb_bool_type then ((
        dbg D_trace @@ lazy (
          sprintf "Added unit eq %s (%s)" 
            (Term.to_string lit)
            (match ui.order.oriented lit with GT -> "oriented >" | LT -> "oriented <" | INC -> "unoriented" | EQ -> assert false)
        );

        (* Handle x=t where x not in t *)
        if is_universal ui lit l r then 
          let typ = Term.get_top_symb etype in
          let c = smallest_constant_of_type typ in
          let x = add_var_term (Var.create typ 0) in
          let lit = add_typed_equality_sym typ x c in
          let source = Clause.tstp_source_demodulation ~main:clause ~eqs:[] in  (* TODO *)
          let clause = create_clause source [lit] in
          add_equation_indexed_by ui x 1 lit clause
           
        else
          match ui.order.oriented lit with
          | GT ->
            if not ui.reverse 
            then add_equation_indexed_by ui l 1 lit clause
            else add_equation_indexed_by ui r 2 lit clause
          | LT ->
            if not ui.reverse 
            then add_equation_indexed_by ui r 2 lit clause
            else add_equation_indexed_by ui l 1 lit clause
          | INC ->
            if not fwd_only_orientable_flag then (
              (* Here we check if l=r (normalised) is the same as r=l (normalised). If yes, we just add in one direction. *)
              let vars_l = Term.get_var_set l in
              let vars_r = Term.get_var_set r in
              (* Quick pre-requisites: same number of symbols, number of variables, set of variables. Also here we know they're nonground or they would be orientable. *)
              if Term.get_num_of_symb l = Term.get_num_of_symb r
              && Term.get_num_of_var l = Term.get_num_of_var r
              && VSet.equal vars_l vars_r
              then (
                let[@warning "-8"] [l';r'] = Clause.normalise_term_list term_db_ref [r;l] |> List.map add_term_db in
                dbg D_trace @@ lazy (sprintf "Candidate for symmetric equation: %s = %s , %s = %s (%b %b)" (Term.to_string l) (Term.to_string r) (Term.to_string l') (Term.to_string r') (l'==l) (r'==r));
                dassert (fun () -> List.X.equal ~eq:(==) [l;r] (Clause.normalise_term_list term_db_ref [l;r]));
                add_equation_indexed_by ui l 1 lit clause;
                if not (l' == l && r' == r) then add_equation_indexed_by ui r 2 lit clause;
              ) else (
                (* We can demodulate using l only if all vars in r are in l *)
                let vars_rl = VSet.subset vars_r vars_l in
                let vars_lr = VSet.subset vars_l vars_r in
                if not ui.reverse && vars_rl then add_equation_indexed_by ui l 1 lit clause;
                if not ui.reverse && vars_lr then add_equation_indexed_by ui r 2 lit clause;
                if ui.reverse && not vars_rl then add_equation_indexed_by ui l 1 lit clause;
                if ui.reverse && not vars_lr then add_equation_indexed_by ui r 2 lit clause;
              )
            )
          | EQ -> assert false
       ); true) else false
    | None -> false
    end
    else false
  | _ -> false

let add_bwd_clause ui clause =
  dbg D_trace @@ lazy (sprintf "Add clause to bwd demod: %s" (Clause.to_string clause));

  let terms = ref TSet.empty in

  (* Here, we kept track of the set of terms to index, but also whether or not to check completeness. 
     The reasoning is to account for clauses like ( a=b | f(a)=c ). Scanning the first literal it 
     would appear that a rewrite on 'a' requires a completeness check wrt b, but later on we see that 
     a appears as a subterm, therefore any equation ( a = … ) is smaller than the former clause. 

     But now, since we know that non-unit clauses automatically don't need any check, then if we 
     see a term at the top of a maximal side, we know that it cannot appear a a subterm of the other 
     side (or else it wouldn't be maximal), and also cannot appear on any other literal because 
     there are no more literals. So here, after all, just a set is required. *)
  let[@inline] add_clause_indexed_by' l =
    (* Add a check if l is of eq type *)
    if SSet.mem (Term.get_term_type l) ui.eq_types then (
      dbg D_subtypes @@ lazy (sprintf "%s is eq (%s)" (Term.to_string l) (Symbol.to_string @@ Term.get_term_type l));
      terms @= TSet.add l
    ) else (
      dbg D_subtypes @@ lazy (sprintf "%s not eq (%s)" (Term.to_string l) (Symbol.to_string @@ Term.get_term_type l));
    )
  in
  (* For use at the top-level to avoid indexing variables *)
  let add_clause_indexed_by'' l = 
    if not @@ Term.is_var l then add_clause_indexed_by' l
  in
  (* For use at maximal side of top-level *)
  let add_clause_indexed_by''' l r no_check = 
    if no_check then add_clause_indexed_by'' l else add_clause_indexed_by ui l ~check:(Some r) clause
  in

  let no_check = Clause.demod_no_check clause in

  Clause.get_lits clause |> List.iter (fun lit ->
    let sign, atom = Term.split_sign_lit lit in
    match Term.Eq.decompose_atom_type atom with
    (* If equality literal, index subterms of lhs and rhs *)
    | Some (etype,l,r) -> 
      dassert (fun () -> if Term.get_top_symb etype == Symbol.symb_bool_type || r == SystemDBs.top_term then not @@ SSet.mem (Term.get_term_type l) ui.eq_types else true);
      if true (* Term.get_top_symb etype != Symbol.symb_bool_type && r != SystemDBs.top_term *) then (
        (* add_clause_indexed_by' terms l ~check:(Some r);
        add_clause_indexed_by' terms r ~check:(if Term.is_oriented_eq_lit lit then None else Some l); *)
        match ui.order.oriented lit with
        | GT -> 
          add_clause_indexed_by''' l r no_check;
          add_clause_indexed_by''  r;
        | LT -> 
          add_clause_indexed_by''  l;
          add_clause_indexed_by''' r l no_check;
        | INC -> 
          add_clause_indexed_by''' l r no_check;
          add_clause_indexed_by''' r l no_check;
        | EQ -> assert false
      );
      l |> Term.iter_subterms_preorder_novar (fun x ->
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        add_clause_indexed_by' x
      );
      r |> Term.iter_subterms_preorder_novar (fun x -> 
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        add_clause_indexed_by' x
      )
    (* If nonequality literal, index subterms *)
    | None -> 
      lit |> Term.iter_subterms_preorder_novar (fun x -> add_clause_indexed_by' x);
      (* failwith "add_clause: non-equality literal" *)
  );

  dbg D_trace @@ lazy ("traversal complete, actually adding");
  !terms |> TSet.iter (fun t -> 
    dbg D_trace @@ lazy (sprintf "add %s" (Term.to_string t));
    add_clause_indexed_by ui t ~check:None clause
  )



let elim_equation ui clause =
  match Clause.get_lits clause with
  | [lit] ->
    if ui.use_ground || not @@ Term.is_ground lit then
    begin match Term.Eq.decompose_atom lit with
    | Some (l,r) -> 
      dbg D_trace @@ lazy (
        sprintf "Del unit eq %s (%s)" 
          (Term.to_string lit)
          (match ui.order.oriented lit with GT | LT -> "oriented" | INC -> "unoriented" | EQ -> assert false)
      );

      begin match ui.order.oriented lit with
      | GT ->
        if not ui.reverse 
        then elim_equation_indexed_by ui l clause
        else elim_equation_indexed_by ui r clause
      | LT ->
        if not ui.reverse 
        then elim_equation_indexed_by ui r clause
        else elim_equation_indexed_by ui l clause
      | INC ->
        elim_equation_indexed_by ui l clause;
        elim_equation_indexed_by ui r clause;
      | EQ -> assert false
      end
    | None -> ()
    end
    else ()
  | _ -> ()

(* TODO: check if it's not better to also limit the number of [elim_clause_indexed_by] calls *)
let elim_bwd_clause ui clause =
  dbg D_trace @@ lazy (sprintf "Del clause from bwd demod: %s" (Clause.to_string clause));

  let terms = ref TSet.empty in

  let elim_clause_indexed_by' terms l =
    if SSet.mem (Term.get_term_type l) ui.eq_types then terms @= TSet.add l
  in

  Clause.get_lits clause |> List.iter (fun lit ->
    match Term.Eq.decompose_atom_type @@ Term.get_atom lit with
    (* If equality literal, index subterms of lhs and rhs *)
    | Some (etype,l,r) -> 
      if true (* Term.get_top_symb etype != Symbol.symb_bool_type && r != SystemDBs.top_term *) then (
        elim_clause_indexed_by' terms l;
        elim_clause_indexed_by' terms r;
      );
      l |> Term.iter_preorder_novar (fun x ->
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        elim_clause_indexed_by' terms x
      );
      r |> Term.iter_preorder_novar (fun x -> 
        dbg D_trace @@ lazy (sprintf " (subterm): %s" (Term.to_string x));
        elim_clause_indexed_by' terms x
      )
    (* If nonequality literal, index subterms *)
    | None -> 
      lit |> Term.iter_subterms_preorder_novar (fun x -> elim_clause_indexed_by' terms x);
      (* failwith "add_clause: non-equality literal" *)
  );

  !terms |> TSet.iter (fun t -> 
    dbg D_trace @@ lazy (sprintf "remove %s" (Term.to_string t));
    try
      elim_clause_indexed_by ui t clause
    with Not_found -> 
      dbg D_trace @@ lazy "(not there in the first place)";
      ()
  );
  dbg D_trace @@ lazy (sprintf "end remove")

let elim_clause ui clause =
  elim_equation ui clause;
  elim_bwd_clause ui clause





let get_fwd ui term =
  if not @@ SSet.mem (Term.get_term_type term) ui.eq_types then [] else
  (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  let candidates = Index.generalisations ui.unit_index term in
  (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print candidates)); *)
  candidates

let get_fwd_caching ui term func =
  (* Idea is: if cached, return. Else, pass candidates to func (external 
     callback which implements the desired functionlity) and cache the 
     result *)
  let[@inline] get_demod_result () = 
    (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
    let candidates = Index.generalisations ui.unit_index term in
    (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print candidates)); *)
    let result = func candidates term in
    ui.cache |> Cache.add term result;
    result
  in

  if not @@ SSet.mem (Term.get_term_type term) ui.eq_types then (term, None) else
  match ui.cache |> Cache.find_opt term with
  | Some cached ->
      Statistics.(
      let term' = fst cached in if term != term' then
        bump_int_stat sim_demod_cache_hit_pos
      else
        bump_int_stat sim_demod_cache_hit_neg);
      
      dbg D_cache @@ lazy (sprintf "Demod cache hit: %s" (Term.to_string term));
    
      dbg_env D_cache_consist (fun () -> 
        let demod_res = get_demod_result () in
        let (cached_t',_), (demod_t',_) = cached, demod_res in
        if cached_t' != demod_t' then
          failwith (sprintf "get_fwd_caching: t: %s cached_t': %s demod_t': %s"
                      (Term.to_string term) (Term.to_string cached_t') (Term.to_string demod_t')
                   );
                              );

      cached
  | None ->
      Statistics.(bump_int_stat sim_demod_cache_miss);  
      dbg D_cache @@ lazy (sprintf "Demod cache miss: %s" (Term.to_string term));
      get_demod_result ()

  (* (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  let candidates = Index.get_gen ui.unit_index term in
  (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print candidates)); *)
  let result = func candidates term in
  ui.cache |> Cache.add term result;
  result *)

let get_bwd ui term =
  (* dbg D_trace @@ lazy (sprintf "Unif: lit: %s" (Term.to_string term)); *)
  let candidates = Index.instantiations ui.bwd_index term in
  (* dbg D_trace @@ lazy (sprintf "Candidates: lit: %s, res: %s" (Term.to_string term) (print candidates)); *)
  candidates



let iter_fwd ui term f =
  Index.generalisations_iter ui.unit_index term f

let iter_bwd ui term f =
  Index.instantiations_iter ui.bwd_index term f

let iter_fwd_caching ui term func = 
  let[@inline] get_demod_result () = 
    let candidates = Index.generalisations_iter ui.unit_index term in
    let result = func candidates term in
    ui.cache |> Cache.add term result;
    result
  in

  if not @@ SSet.mem (Term.get_term_type term) ui.eq_types then (term, None) else
  match ui.cache |> Cache.find_opt term with
  | Some cached ->
      
      Statistics.(
      let term' = fst cached in if term != term' then
        (
         bump_int_stat sim_demod_cache_hit_pos;
        (* let result = get_demod_result () in *)
         
        )
      else
        bump_int_stat sim_demod_cache_hit_neg);
      
      dbg D_cache @@ lazy (sprintf "Demod cache hit: %s -> %s" 

                             (Term.to_string term) (let term' = fst cached in if term != term' then Term.to_string term' else "itself")
    );
    
     dbg_env D_cache_consist (fun () -> 
      let demod_res = get_demod_result () in
      let (cached_t',_), (demod_t',_) = cached, demod_res in
      if cached_t' != demod_t' then
        failwith (sprintf "get_fwd_caching: t: %s cached_t': %s demod_t': %s"
          (Term.to_string term) (Term.to_string cached_t') (Term.to_string demod_t')
        );
    ); 

    cached
  | None ->
      Statistics.(bump_int_stat sim_demod_cache_miss);  
    (* dbg D_cache @@ lazy (sprintf "Demod cache miss: %s" (Term.to_string term)); *)
      let result = get_demod_result () in
      
    (* dbg D_cache @@ lazy (sprintf "Demod cache add: %s" (Term.to_string @@ fst result)); *)
      dbg D_cache @@ lazy (sprintf "Demod cache miss: %s -> %s" 
                             (Term.to_string term) (let term' = fst result in
                             if term != term' then Term.to_string term' else "itself")
                          );
    result

