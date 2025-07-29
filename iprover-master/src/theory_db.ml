open Lib
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_trace2
  | D_trie

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_trace2 -> "trace"
  | D_trie -> "trie"

let dbg_groups = [
  D_trace;
   D_trace2; 
   D_trie; 
]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)





(** A record of the theories contained in a set of clauses. Maps theories to instances, even partial ones. *)
module Record = struct
  module TheoryMap = Map.Make(struct 
    type t = Theory.t
    let compare = Ord.lift (fun (x:t) -> x.name) String.compare
  end)

  type t = 
    ((Theory_subst.subst_sym * bit_vec * clause list) list) TheoryMap.t ref

  let create () = 
    ref TheoryMap.empty

  let query x name = 
    let open Option.O in
    (* (!x |> TheoryMap.find_opt {name; axioms=[]}) |? [] *)
    (* match !x |> TheoryMap.find_opt {name; axioms=[]} with *)
    match !x |> TheoryMap.find_first_opt (fun x -> String.compare x.name name >= 0) with
    | Some (theory, l) when String.equal theory.name name -> 
      let complete = (1 lsl List.length theory.axioms) - 1 in
      l |> List.filter_map (fun (subst_sym, bv, clauses) ->   
        if Bit_vec.to_int bv = complete then Some (subst_sym, clauses) else None
      )
    | _ -> []
    (* TODO clean up for efficiency, probably ditch string names in favour of `Polymorphic_variants *)

  let to_list x = 
    TheoryMap.bindings !x |> List.concat_map (fun ((theory:Theory.t), l) -> 
      let complete = (1 lsl List.length theory.axioms) - 1 in
      l |> List.X.filter_map (fun (subst_sym, bv, clauses) -> 
        if Bit_vec.to_int bv = complete then Some (theory, subst_sym, clauses) else None
      )
    )

  let num_of_theories x = TheoryMap.cardinal !x

  let output ?(prefix="") out_channel x = 
    TheoryMap.bindings !x |> List.X.output' ~sep:"" (fun o ((theory:Theory.t), l) -> 
      let width = List.length theory.axioms in
      l |> List.X.output' ~sep:"" (fun o (subst_sym, bv, clauses) -> 
        fprintf o "%s%s %a %a\n" prefix theory.name Theory_subst.output_sym subst_sym (Bit_vec.output ~width) bv;
        clauses |> List.X.output' ~sep:"" (fun o x -> 
          fprintf o "%s  %s\n" prefix (Clause.to_string_tptp x)
        ) o
      ) o
    ) out_channel

  (* let full_theories record : subst_sym list TheoryMap.t = 
    record |> TheoryMap.map (List.filter_map (fun (subst_sym, bv) -> 
      if Bit_vec.to_int bv + 1 = 
    )) *)
end



(** Data structure to store a searchable theory database *)
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
    | Node of ('a t KMap.t) ref
    | Leaf of term * 'a list ref

  let empty_node () = 
    Node (ref KMap.empty)

  let empty_leaf term = 
    Leaf (term, ref [])

  let empty = empty_node

  let iter_all_next f index = 
    match index with
    | Node x -> KMap.iter f !x
    | Leaf _ -> invalid_arg "Trie.next"

  let get_from_leaf index = 
    match index with
    | Leaf (term, list) -> (term, list)
    | Node _ -> invalid_arg "Trie.get_from_leaf"

  let update index term k = 
    let rec loop index terms (k: term -> 'a list ref -> unit) = 
      dbg D_trie @@ lazy (sprintf "update: terms: %s" 
        (List.X.to_string Term.to_string terms)
      );
      match terms with
      | hd::tl ->
        let entry = match index with Node x -> x | Leaf _ -> assert false in
        let key, terms' = 
          match hd with 
          | Term.Fun (sym, args, _) -> Sym sym, (Term.arg_to_list args @ tl)
          | Term.Var (var, _) -> Var var, tl
        in
        let index' = 
          match !entry |> KMap.find_opt key with
          | Some x -> x
          | None -> 
            let new_index = match terms' with _::_ -> empty_node () | [] -> empty_leaf term in
            entry @= KMap.add key new_index;
            new_index
        in
        loop index' terms' k;
      | [] -> 
        (match index with Leaf (term, list) -> k term list | Node _ -> assert false)
    in

    dbg D_trie @@ lazy (sprintf "Trie.update: %s" (Term.to_string term));
    loop index [term] k;
    (* dbg D_trie @@ lazy "Trie.update (result): %a" print_dbg index; *)
    ()

  let rec variants' 
      k 
      subst_var subst_sym 
      subst_var_rev subst_sym_rev 
      index terms 
      = 
    dbg D_trie @@ lazy (sprintf "variants: terms: %s at: %x" (List.X.to_string Term.to_string terms) (addressof ~bits:24 index));
    match terms with
    | Term.Fun (sym, args, _) :: tl -> 
        let sym_special = Symbol.is_special_symb sym in 
        index |> iter_all_next (fun key index' -> 
        match key with 
        | Var _ -> () 
        | Sym sym' ->
            dbg D_trace2 @@ lazy (sprintf "variants: s':%s s:%s ar_eq:%b comp_t:%b sp:%b"
                                    (Symbol.to_string sym')
                                    (Symbol.to_string sym)
                                    (Symbol.get_arity sym == Symbol.get_arity sym')
                                    (Symbol.compatible_basic_types (Symbol.get_val_type_def sym) (Symbol.get_val_type_def sym'))
                                    (sym_special || Symbol.is_special_symb sym')                            
                                 );
            if Symbol.get_arity sym == Symbol.get_arity sym' &&
              Symbol.compatible_basic_types (Symbol.get_val_type_def sym) (Symbol.get_val_type_def sym') then
            (* allow for types to be substituted *)  
              if (sym_special && (not (Symbol.is_type_symb sym))) ||
              ((Symbol.is_special_symb sym') && (not (Symbol.is_type_symb sym'))) then (
                if sym == sym' then
                  let terms' = Term.arg_to_list args @ tl in
                  variants' k subst_var subst_sym subst_var_rev subst_sym_rev index' terms'
               ) else 
                match subst_sym |> SMap.find_opt sym', subst_sym_rev |> SMap.find_opt sym with
                | Some y, Some z when y == sym && z == sym' ->
                    dbg D_trace2 @@ lazy (sprintf "variants: found in subst");   
                    let terms' = Term.arg_to_list args @ tl in                    
                    variants' k subst_var subst_sym subst_var_rev subst_sym_rev index' terms'
                | None, None ->
                    dbg D_trace2 @@ lazy (sprintf "variants: add to subst: s':%s s:%s"
                                            (Symbol.to_string sym')
                                            (Symbol.to_string sym) 
                                         );
                    let subst_sym' = subst_sym |> SMap.add sym' sym in
                    let subst_sym_rev' = subst_sym_rev |> SMap.add sym sym' in
                    let terms' = Term.arg_to_list args @ tl in
                    variants' k subst_var subst_sym' subst_var_rev subst_sym_rev' index' terms'
                | _ -> 
                    ()
                                )

    | Term.Var (var, _) :: tl ->
      index |> iter_all_next (fun key index' -> 
        match key with 
        | Sym _ -> () 
        | Var var' ->
            dbg D_trace2 @@ lazy (sprintf "variants: v':%s v:%s comp_t:%b"
                                    (Var.to_string var')
                                    (Var.to_string var)
                                    (Symbol.compatible_basic_types (Var.get_type var) (Var.get_type var'))
                                 );
          if Symbol.compatible_basic_types (Var.get_type var) (Var.get_type var') then
          match subst_var |> VMap.find_opt var', subst_var_rev |> VMap.find_opt var with
          | Some y, Some z when y == var && z == var' ->
              dbg D_trace2 @@ lazy (sprintf "variants: found in subst");                                         
              let terms' = tl in
              variants' k subst_var subst_sym subst_var_rev subst_sym_rev index' terms'
          | None, None ->
              dbg D_trace2 @@ lazy (sprintf "variants: add to subst: v':%s v:%s"
                                      (Var.to_string var')
                                      (Var.to_string var) 
                                   );                     
              let subst_var' = subst_var |> VMap.add var' var in
              let subst_var_rev' = subst_var_rev |> VMap.add var var' in
              let terms' = tl in
              variants' k subst_var' subst_sym subst_var_rev' subst_sym_rev index' terms'
                
          | x, y -> 
              (
               dbg D_trace2 @@ lazy (sprintf "variants: incomp: x:%s y:%s"
                                       (Option.to_string Var.to_string x)
                                       (Option.to_string Var.to_string y)
                                    );                           
              )
      )

    | [] -> 
      let (term, list) = get_from_leaf index in
      k term (subst_var, subst_sym) list

  (* let variants'_aux k subst_var subst_sym index term = 
    dbg D_trie @@ lazy (sprintf "terms: [%s] at: %x" (Term.to_string term) (addressof ~bits:24 index));
    match term with
    | Term.Fun (sym, args, _) -> 
      index |> iter_all_next (fun key index' -> 
        match key with 
        | Var _ -> () 
        | Sym sym' -> 
          if Symbol.compatible_basic_types (Symbol.get_val_type_def sym) (Symbol.get_val_type_def sym') then
            let subst_sym = subst_var |> SMap.add sym' sym in
            let terms = Term.arg_to_list args in
            variants' k subst_var subst_sym index' terms
      )
    | Term.Var (var, _)  ->
      index |> iter_all_next (fun key index' -> 
        match key with 
        | Sym _ -> () 
        | Var var' -> 
          if Symbol.compatible_basic_types (Var.get_type var) (Var.get_type var') then
            let subst_var = subst_var |> VMap.add var' var in
            let subst_sym = SMap.empty in
            let terms = [] in
            variants' k subst_var subst_sym index' terms
      ) *)

  let variants index term = 
    let l = ref [] in
    let k = fun term subst list -> l @= List.cons (term, subst, !list) (* ) index term *) in
    variants'(*_aux*) k VMap.empty SMap.empty VMap.empty SMap.empty index [term];
    !l

  let variants_iter index term f = 
    let k term subst list = f term subst !list in
    variants'(*_aux*) k VMap.empty SMap.empty VMap.empty SMap.empty index [term]
end



module IntMap = struct 
  include IntMap 
  let find_create k v map = 
    match !map |> find_opt k with
    | Some x -> x
    | None -> let v = v() in map @= add k v; v
end

type trie = (Theory.t * int * Theory.Axiom.t) Trie.t
and length = negatives IntMap.t ref
and negatives = num_symb IntMap.t ref
and num_symb = num_vars IntMap.t ref
and num_vars = trie IntMap.t ref
(* and pos = trie IntMap.t ref *)

type t = 
  (* { axioms: length; theories: Theory.theory list ref } *)
  length

let get_trie ~length ~negatives ~num_symb ~num_vars db = 
  db 
  |> IntMap.find_create length    (fun () -> ref IntMap.empty) 
  |> IntMap.find_create negatives (fun () -> ref IntMap.empty) 
  |> IntMap.find_create num_symb  (fun () -> ref IntMap.empty) 
  |> IntMap.find_create num_vars  (fun () -> Trie.empty ())



let create () = 
  ref IntMap.empty

let add (db: t) (theory: Theory.t) = 
  theory.axioms |> List.iteri (fun i (axioms, subst_sym) -> 
    axioms |> List.iter (fun axiom -> 
      assert (SMap.is_empty subst_sym);  (* TODO: fix *)
      let axiom' = Theory.Axiom.to_clause axiom in
      let lits = Clause.get_lits axiom' in
      let length = Clause.length axiom' in
      let negatives = lits |> List.X.count (Term.is_neg_lit) in
      let lit = List.hd lits in
      let num_symb = Term.get_num_of_symb lit in
      let num_vars = Term.get_num_of_var lit in
      let trie = get_trie ~length ~negatives ~num_symb ~num_vars db in
      Trie.update trie lit (fun _lit l -> 
        dassert (fun () -> _lit == lit);  (* TODO tirar este _term aqui e no perfectDiscrTree *)
        l @= List.cons (theory, i, axiom)
      )
    )
  )

let search (db: t) record clause = 
  dbg D_trace @@ lazy (dbg_env D_trace2 (fun () -> Record.output stdout record); sprintf "search %s" (Clause.to_string_tptp clause));
  let lits = Clause.get_lits clause in
  let length = Clause.length clause in
  let negatives = lits |> List.X.count (Term.is_neg_lit) in
  (* Search the first literal in the trie *)
  let[@warning "-8"] hd::tl = lits in
  let num_symb = Term.get_num_of_symb hd in
  let num_vars = Term.get_num_of_var hd in
  let trie = get_trie ~length ~negatives ~num_symb ~num_vars db in
  let candidates = Trie.variants_iter trie hd in
  candidates (fun term subst list -> 
    list |> List.iter (fun ((theory:Theory.t), i, axiom) -> 
      dbg D_trace @@ lazy (sprintf "candidate theory %s %d" (theory.name) (i));
      (* For the remaining literals in the axiom, try to match them *)
      (* let axiom, _subst_sym = List.nth theory.axioms i in
      assert (SMap.is_empty _subst_sym);  (* TODO: fix *) *)
      let lits_axiom = Clause.get_lits (Theory.Axiom.to_clause axiom) in
      let[@warning "-8"] hd_axiom::tl_axiom = lits_axiom in
      let rec loop subst a b = 
        match a, b with
        | [], [] -> Some subst
        | hd_a::tl_a, hd_b::tl_b -> 
          begin match Theory_subst.renaming subst hd_a hd_b with
          | Some subst' -> loop subst' tl_a tl_b
          | None -> None
          end
        | _ -> assert false
      in
      match loop subst tl tl_axiom with
      | None -> ()
      (* [clause] is theory axiom number [i] of [theory], instantiated with [subst_sym] *)
      | Some (subst_var, subst_sym) -> 
        dbg D_trace @@ lazy (sprintf "candidate successful with %s %s" (Theory_subst.to_string_var subst_var) (Theory_subst.to_string_sym subst_sym));
        (* Is this theory already in the record? *)
        record @= Record.TheoryMap.update theory (function  (* TODO as substs na instance e na theory, e.g. para lattices *)
          (* If yes, check if already exists with a compatible subst_sym *)
          | Some instances -> 
            let rec loop instances = 
              match instances with
              (* If yes, add this new axiom to that instance *)
              | ((subst_sym', bv', clauses) as hd)::tl -> 
                let contained s1 s2 = s1 |> SMap.for_all (fun k v -> match s2 |> SMap.find_opt k with Some v' -> v == v' | None -> false) in
                if bv' |> Bit_vec.get i then (if contained subst_sym subst_sym' then instances else hd :: loop tl) else
                begin match Theory_subst.merge_sym subst_sym subst_sym' with
                | Some subst_sym'' -> 
                  let bv'' = bv' |> Bit_vec.set true i in
                  (subst_sym'', bv'', clause::clauses) :: loop tl
                | None -> hd :: loop tl
                end
              (* If not, add a new instance *)
              | [] -> [(subst_sym, Bit_vec.(false_vec |> set true i), [clause])]
            in
            Some (loop instances)
          (* If not, add it *)
          | None -> 
            Some [(subst_sym, Bit_vec.(false_vec |> set true i), [clause])]
        )
    )
  )

let search_many db record clauses = 
  List.iter (db record) clauses

let global_record_ref = ref (Record.create())

let get_global_record () = !global_record_ref
    
let reset_global_record () = global_record_ref := Record.create()
  




(********************)
(* Builtin database *)
(********************)

  (* Theories: list of clauses + mapping of symbols in the clause to general symbols in the theory (if necessary to bind them, can be empty) *)

(* let i =  Symbol.symb_default_type *)

(* KK: using Symbol.symb_type_types in place of i so types can be substituted *)
(* KK: TODO: 
   use placeholder types so we can have different theories crossing over different types (vector spaces, modules, etc.)
   but need some redesign for this (?) *)
    
(* order of how placeholders are created using mk_symb is important and is used in theory_orderings.ml *)
(* KK: rework ? *)
    
let i =  Symbol.symb_type_types 
  
let o = Symbol.symb_bool_type 

let mk_symb stype = SymbolDB.create_new_placeholder symbol_db_ref stype 

  (* let f = Symbol.create_from_str_type_property ~is_sig:false "$$iProver_placeholder_iii" (Symbol.create_stype [i;i] i) in *)
let f = add_fun_term @@ mk_symb (Symbol.create_stype [i;i] i) 
let g = add_fun_term @@ mk_symb (Symbol.create_stype [i] i) 
let c = add_fun_term (mk_symb (Symbol.create_stype [] i)) [] 

let x = add_var_term @@ Var.create i 0 
let y = add_var_term @@ Var.create i 1 
let z = add_var_term @@ Var.create i 2 

let (=~) = add_typed_equality_sym i 
let (=~!) = add_typed_disequality_sym i 
let (~~) = add_neg_atom 
let cl = create_clause ~normalise_eqs:true (Clause.tstp_source_tmp) %> Theory.Axiom.of_clause 

  (* Axioms: clauses with placeholder symbols *)
let assoc  = [ cl [ f[x; f[y; z]] =~ f[f[x; y]; z] ]         ] 
let commut = [ cl [ f[x; y] =~ f[y; x] ]                     ] 
let unit   = [ cl [ f[c; x] =~ x ] ; cl [ f[x; c] =~ x ]     ] 
let inv    = [ cl [ f[g[x]; x] =~ c] ; cl [ f[x; g[x]] =~ c] ] 
let idemp  = [ cl [ f[x;x] =~ x ]                            ] 
let _invol  = [ cl [ g[g[x]] =~ x ]                          ]  (* not used yet *)
  (* Welldef is variadic, leave that for now *)
  (* let welldef_rel = cl [ f [x; g [x]] =~ c ; x =~ y ] in
  let total_rel   = cl [ f [x; g [x]] =~ c ; x =~ y ] in *)


  (* let unit2 = [ cl [ f2[c; x] =~ c ] ; cl [ f2[x; c] =~ c ]      ] in *)
let f2 = add_fun_term @@ mk_symb (Symbol.create_stype [i;i] i)
let g2 = add_fun_term @@ mk_symb (Symbol.create_stype [i] i) 
let c2 = add_fun_term (mk_symb (Symbol.create_stype [] i)) [] 

let assoc2  = [ cl [ f2[x; f2[y; z]] =~ f2[f2[x; y]; z] ]     ] 
let commut2 = [ cl [ f2[x; y] =~ f2[y; x] ]                   ] 
let unit2   = [ cl [ f2[c2; x] =~ x ] ; cl [ f2[x; c2] =~ x ] ] 
let inv2    = [ cl [ x =~ c ; f2[g2[x]; x] =~ c2] ; cl [ x =~ c ; f2[x; g2[x]] =~ c2] ] 

let distr_l = [ cl [ f2[x; f[y; z]] =~ f[f2[x; y]; f2[x; z]] ] ] 
let distr_r = [ cl [ f2[f[y; z]; x] =~ f[f2[y; x]; f2[z; x]] ] ] 
let distr = distr_l @ distr_r 

  (* Lattice-like *)

  let distr_l2 = [ cl [ f[x; f2[y; z]] =~ f2[f[x; y]; f[x; z]] ] ] 
  let distr_r2 = [ cl [ f[f2[y; z]; x] =~ f2[f[y; x]; f[z; x]] ] ] 
  let distr2 = distr_l2 @ distr_r2 
  let distrcyc  = [ cl [ f[f2[x;y]; f[f2[x;z]; f2[y;z]]] =~ f2[f[x;y]; f2[f[x;z]; f[y;z]]] ] ]   (* Missing all permutations *)
  let distrcanc = [ cl [ f[x;y] =~! f[x;z] ; f2[x;y] =~! f2[x;z] ; y =~ z ] ] 
  let distrall  = distrcyc @ distrcanc @ distr @ distr2 
  (* If commutative, distr_l* ⇔ distr_r*. Any lattice that satisfies distr satisfies distr2, and vice-versa. distrcyc implies distr and distt2. distrcanc implies distr and distt2. *)
  (* let compl   = [ cl [ f[x; g[x]] =~ c2 ] ; cl [ f[g[x]; x] =~ c2 ] ] in
  let compl2  = [ cl [ f2[x; g[x]] =~ c ] ; cl [ f2[g[x]; x] =~ c ] ] in
  let compl12 = [ cl [ f2[x; g[x]] =~ c ] ; cl [ f2[g[x]; x] =~ c ] ] in *)
  let ucompl  = [ cl [ f[x; g[x]] =~ c2 ] ; cl [ f[g[x]; x] =~ c2 ] ] 
  let ucompl2 = [ cl [ f2[x; g[x]] =~ c ] ; cl [ f2[g[x]; x] =~ c ] ] 
  let absorb12 = [ cl [ f[x; f2[x; y]] =~ x ] ; cl [ f[x; f2[y; x]] =~ x ] ; cl [ f[f2[x; y]; x] =~ x ] ; cl [ f[f2[y; x]; x] =~ x ] ] 
  let absorb21 = [ cl [ f2[x; f[x; y]] =~ x ] ; cl [ f2[x; f[y; x]] =~ x ] ; cl [ f2[f[x; y]; x] =~ x ] ; cl [ f2[f[y; x]; x] =~ x ] ] 

    
let none = SMap.empty 

let add_group_like db =
    (* Group-like *)
  
  add db {name = "ac"; axioms = [(assoc, none); (commut, none)]};
  (* add db {name = "aci"; axioms = [(assoc, none); (commut, none); (unit, none)]}; *)
  add db {name = "group"; axioms = [(assoc, none); (unit, none); (inv, none)]};
  add db {name = "monoid"; axioms = [(assoc, none); (unit, none)]};
  add db {name = "semigroup"; axioms = [(assoc, none)]};
  add db {name = "commgroup"; axioms = [(assoc, none); (unit, none); (inv, none); (commut, none)]};
  
  add db {name = "unit"; axioms = [(unit, none)]};
  add db {name = "idemp"; axioms = [(idemp, none)]};
  add db {name = "aci"; axioms = [(assoc, none); (commut, none); (idemp, none)]};
  db


let add_ring_like db =
  (* Ring-like *)


  add db {name = "ring"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (assoc2, none); (unit2, none); 
    (distr_l, none); (distr_r, none); 
  ]};
  add db {name = "rng"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (assoc2, none); (*unit2, none*) 
    (distr_l, none); (distr_r, none); 
  ]};
  add db {name = "semiring"; axioms = [
    (assoc, none); (commut, none); (unit, none); (*inv, none*) 
    (assoc2, none); (unit2, none); 
    (distr_l, none); (distr_r, none); 
  ]};
  add db {name = "semirng"; axioms = [
    (assoc, none); (commut, none); (unit, none); (*inv, none*) 
    (assoc2, none); (*unit2, none*) 
    (distr_l, none); (distr_r, none); 
  ]};
  add db {name = "nonassocring"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (*assoc2, none*) (unit2, none); 
    (distr_l, none); (distr_r, none); 
  ]};
  add db {name = "nonassocrng"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (*assoc2, none*) (*unit2, none*) 
    (distr_l, none); (distr_r, none); 
  ]};
  add db {name = "commring"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (assoc2, none); (unit2, none); (commut2, none); 
    (distr, none); 
  ]};
  add db {name = "commrng"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (assoc2, none); (*unit2, none*) (commut2, none); 
    (distr, none); 
  ]};
  add db {name = "field"; axioms = [
    (assoc, none); (commut, none); (unit, none); (inv, none); 
    (assoc2, none); (commut2, none); (unit2, none); (inv2, none); 
    (distr, none); 
  ]};
  db


let add_lattice_like db =

  add db {name = "semilattice"; axioms = [
    (assoc, none); (commut, none); (idemp, none)
  ]};
  add db {name = "boundsemilattice"; axioms = [
    (assoc, none); (commut, none); (idemp, none); (unit, none); 
  ]};
  add db {name = "lattice"; axioms = [
    (assoc, none); (commut, none); 
    (assoc2, none); (commut2, none); 
    (absorb12, none); (absorb21, none); 
  ]};
  add db {name = "boundlattice"; axioms = [
    (assoc, none); (commut, none); (unit, none);
    (assoc2, none); (commut2, none); (unit2, none);
    (absorb12, none); (absorb21, none); 
  ]};
  (* add db {name = "compllattice"; axioms = [
    (assoc, none); (commut, none); (unit, none);
    (assoc2, none); (commut2, none); (unit2, none);
    (absorb12, none); (absorb21, none); 
  ]}; *)
  add db {name = "ucompllattice"; axioms = [
    (assoc, none); (commut, none); (*unit, none*) (ucompl, none); 
    (assoc2, none); (commut2, none); (*unit2, none*) (ucompl2, none); 
    (absorb12, none); (absorb21, none); 
  ]};
  add db {name = "distlattice"; axioms = [
    (assoc, none); (commut, none); 
    (assoc2, none); (commut2, none); 
    (absorb12, none); (absorb21, none); 
    (distrall, none);
  ]};
  (* TODO *)
  (* ortholattice *)
  (* quasilattice *)
  (* LATx and MVAx axioms in general *)
  (* weakassociative lattice *)
  add db {name = "boolalg"; axioms = [
    (commut, none); (unit, none); (ucompl, none); 
    (commut2, none); (unit2, none); (ucompl2, none); 
    (distrall, none); 
    (* Note: Absorbption and associativity are implied by the rest *)
  ]};

  let f3 = add_fun_term @@ mk_symb (Symbol.create_stype [i;i] i) in
  let g3 = add_fun_term @@ mk_symb (Symbol.create_stype [i] i) in
  let c3 = add_fun_term (mk_symb (Symbol.create_stype [] i)) [] in
  let assoc3 = [ cl [ f3[x; f3[y; z]] =~ f3[f3[x; y]; z] ]     ] in
  let unit3  = [ cl [ f3[c3; x] =~ x ] ; cl [ f3[x; c3] =~ x ] ] in
  let inv3   = [ cl [ f3[g3[x]; x] =~ c3] ; cl [ f3[x; g3[x]] =~ c3] ] in
  let mono13 = [ cl [ f3[x; f[y; z]] =~ f[f3[x; y]; f3[x; z]] ] ] in
  let mono31 = [ cl [ f3[f[y; z]; x] =~ f[f3[y; x]; f3[z; x]] ] ] in
  let mono23 = [ cl [ f3[x; f2[y; z]] =~ f2[f3[x; y]; f3[x; z]] ] ] in
  let mono32 = [ cl [ f3[f2[y; z]; x] =~ f2[f3[y; x]; f3[z; x]] ] ] in
  add db {name = "latticeordgroup"; axioms = [
    (assoc3, none); (unit3, none); (inv3, none); 
    (assoc, none); (commut, none);
    (assoc2, none); (commut2, none); 
    (absorb12, none); (absorb21, none); 
    (mono13, none); (mono31, none); (mono23, none); (mono32, none); 
  ]};
  db  

let add_relations db =
  (* Relations *)
  let r = add_fun_term (mk_symb (Symbol.create_stype [i;i] o)) in
  let refl     = [ cl [ r[x;x] ]                           ] in
  let irrefl   = [ cl [ ~~(r[x;x]) ]                       ] in
  let trans    = [ cl [ ~~(r[x;y]) ; ~~(r[y;z]) ; r[x;z] ] ] in
  let symm     = [ cl [ ~~(r[x;y]) ; r[y;z] ]              ] in
  let asymm    = [ cl [ ~~(r[x;y]) ; ~~(r[y;z]) ]          ] in
  let antisymm = [ cl [ ~~(r[x;y]) ; ~~(r[y;x]) ; x =~ y ] ] in
  let total    = [ cl [ r[x;y] ; r[y;x] ]                  ] in
  let stotal   = [ cl [ r[x;y] ; r[y;x] ; x =~ y ]         ] in
  add db {name = "equivrel"; axioms = [(refl, none); (symm, none); (trans, none)]};
  add db {name = "partialord_strict"; axioms = [(irrefl, none); (asymm, none); (trans, none)]};
  add db {name = "partialord_nonstrict"; axioms = [(refl, none); (antisymm, none); (trans, none)]};
  add db {name = "totalord_strict"; axioms = [(irrefl, none); (asymm, none); (trans, none); (stotal, none)]};
  add db {name = "totalord_nonstrict"; axioms = [(refl, none); (antisymm, none); (trans, none); (total, none)]};
  db
  
let create_builtin () = 
  let db = create () in

  let db = add_relations db in 
  let db = add_group_like db in
  let db = add_ring_like db in
  let db = add_lattice_like db in 
  
 (* repeated due to a compilation issue on macOS *)  
(*  let db = add_relations db in
  let db = add_relations db in
  let db = add_ring_like db in
  let db = add_relations db in
  let db = add_relations db in
  let db = add_ring_like db in
  *)
  (* -- *)
  db

    
let builtin = (create_builtin[@inlined never]) ()
 
(*let builtin = create_builtin ()*)
    
(* Temporary! *)
let print_report ?(trunc=false) ~heading clauses = 
  let theory_record = Record.create () in
  List.iter (search builtin theory_record) clauses;
  let fname = !Options.global_options.problem_files |> List.X.get_singleton |> Filename.basename in
  let f = (if trunc then open_out else open_out_gen [Open_append; Open_wronly] 0o664) ("THEORY_PREFIX_2/"^fname) in
  output_string f ("% "^heading^":\n");
  theory_record |> Record.output ~prefix:"% " f;
  output_string f "% End.\n";
  close_out f;
