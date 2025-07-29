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
    


(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)





module Term_map = struct
  type key = { 
    regular: Z3.Expr.expr option; 
    prop: Z3.Expr.expr option; 
    quant: Z3.Expr.expr option; 
  }

  (* type map = key TMap.t *)

  let empty = 
    { regular=None; prop=None; quant=None }
end

type symb_or_var = Symb of symbol | Var of var

type state = {
  ctx: Z3.context;

  mutable prop_term_id: int;
  mutable term_map: Term_map.key TMap.t;  (* TODO: investigate performance impact when this potentially has 100,000s of terms *)

  mutable sort_map: Z3.Sort.sort SMap.t;

  mutable var_id: int;
  mutable var_map: Z3.Symbol.symbol VMap.t;

  mutable funcdecl_map: Z3.FuncDecl.func_decl SMap.t;
  mutable functheory_map: (Z3.Expr.expr list -> Z3.Expr.expr) SMap.t;  (* TODO: split according to arity? *)

  mutable rev_symbol_map: symb_or_var IntMap.t;

  (* mutable numbering_max: int;
  mutable numbering_map: Z3.Symbol.symbol IntMap.t;  (* TODO change to lookup array? *) *)

  fast_tactic: Z3.Tactic.tactic;
}

type term = Z3.Expr.expr

type clause = Z3.Expr.expr

module Ip = Logic_interface



(* Theories *)
let add_bool_sorts ctx smap = 
  smap 
  |> SMap.add Symbol.symb_bool_type (Z3.Boolean.mk_sort ctx)
   (* KK for now symb_false_fun  are treated as usual functions; otherwise literals can be simplified into vars *)
(*  |> SMap.add Symbol.symb_bool_fun_type (Z3.Boolean.mk_sort ctx) *)

let add_arithmetic_sorts ctx smap = 
  smap 
  |> SMap.add Symbol.symb_real_type (Z3.Arithmetic.Real.mk_sort    ctx)
  |> SMap.add Symbol.symb_rat_type  (Z3.Arithmetic.Real.mk_sort    ctx)
  |> SMap.add Symbol.symb_int_type  (Z3.Arithmetic.Integer.mk_sort ctx)

let add_bool_funcs ctx smap = 
  let[@warning "-8"] z3_true  [] = Z3.Boolean.mk_true ctx in
  let[@warning "-8"] z3_false [] = Z3.Boolean.mk_false ctx in
  smap
(* KK check if true/false can/how be function constants in Z3 *)
(* KK for now symb_false_fun  are treated as usual functions; otherwise literals can be simplified into vars *)
    (*
  |> SMap.add Symbol.symb_true_fun  z3_true
  |> SMap.add Symbol.symb_false_fun z3_false
     *)
    
      (* KK added as $true /$false can occur as top predicates *)
  |> SMap.add Symbol.symb_true  z3_true
  |> SMap.add Symbol.symb_false  z3_false
(* let add_arithmetic_funcdecls ctx smap fmap = 
  let int_sort = smap |> SMap.find Symbol.symb_int_type in
  let real_sort = smap |> SMap.find Symbol.symb_real_type in
  let[@inline] add symb typ_args typ_val = 
    SMap.add symb (Z3.FuncDecl.mk_func_decl ctx (Z3.Symbol.mk_int ctx (Symbol.get_fast_key symb)) typ_args typ_val)
  in
  fmap
  |> add Symbol.symb_abs_int       [int_sort] int_sort
  |> add Symbol.symb_ceiling_real  [real_sort] real_sort
  |> add Symbol.symb_truncate_real [real_sort] real_sort
  |> add Symbol.symb_round_real    [real_sort] real_sort *)

let add_arithmetic_funcs ctx (*fdmap*) fmap = 
  (* TODO: convert iprover $add(x,$add(y,z)) to z3 (+ x y z) *)
  let[@warning "-8"] z3_add = Z3.Arithmetic.mk_add ctx in
  let[@warning "-8"] z3_sub = Z3.Arithmetic.mk_sub ctx in
  let[@warning "-8"] z3_mul = Z3.Arithmetic.mk_mul ctx in
  let[@warning "-8"] z3_div [x;y] = Z3.Arithmetic.mk_div ctx x y in
  (* A bit confusing, smtlib (mod …) is translated by vampire to remainder_e(…). But 
     semantics is mk_mod, not mk_rem, however this is not explicit in the documentation 
     so I'm not 100% sure!. *)
  let[@warning "-8"] z3_div_e [x;y] = Z3.Arithmetic.mk_div ctx x y in
  let[@warning "-8"] z3_rem_e [x;y] = Z3.Arithmetic.Integer.mk_mod ctx x y in  
  (* let[@warning "-8"] z3_abs = Z3.Expr.mk_app ctx (fdmap |> SMap.find Symbol.symb_abs_int) in *)
  let[@warning "-8"] z3_uminus [x] = Z3.Arithmetic.mk_unary_minus ctx x in
  let[@warning "-8"] z3_lt [x;y] = Z3.Arithmetic.mk_lt ctx x y in
  let[@warning "-8"] z3_le [x;y] = Z3.Arithmetic.mk_le ctx x y in
  let[@warning "-8"] z3_gt [x;y] = Z3.Arithmetic.mk_gt ctx x y in
  let[@warning "-8"] z3_ge [x;y] = Z3.Arithmetic.mk_ge ctx x y in
  let[@warning "-8"] z3_floor [x] = Z3.Arithmetic.(Integer.mk_int2real ctx (Real.mk_real2int ctx x)) in
  let[@warning "-8"] z3_ceil [x] = z3_uminus [z3_floor [z3_uminus [x]]] in
  (* let[@warning "-8"] z3_ceil = Z3.Expr.mk_app ctx (fdmap |> SMap.find Symbol.symb_ceiling_real) in *)
  (* let[@warning "-8"] z3_trunc = Z3.Expr.mk_app ctx (fdmap |> SMap.find Symbol.symb_truncate_real) in *)
  (* let[@warning "-8"] z3_round = Z3.Expr.mk_app ctx (fdmap |> SMap.find Symbol.symb_round_real) in *)
  let[@warning "-8"] z3_to_real_int [x] = Z3.Arithmetic.Integer.mk_int2real ctx x in
  let[@warning "-8"] z3_to_real_rat [x] = x in
  let[@warning "-8"] z3_to_int [x] = Z3.Arithmetic.Real.mk_real2int ctx x in
  let[@warning "-8"] z3_to_rat_int [x] = Z3.Arithmetic.Integer.mk_int2real ctx x in
  let[@warning "-8"] z3_to_rat_real [x] = x in (* As per tptp, this is undefined when x is not rational, so it can just be the identity *)
  (*
  let[@warning "-8"] z3_is_int [x] = Z3.Arithmetic.Real.mk_is_integer ctx x in
  let[@warning "-8"] z3_is_rat [x] = Z3.Boolean.mk_val ctx (Z3.Arithmetic.is_rat_numeral x) in
  *)
  let n_0 = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
  let n_05 = Z3.Arithmetic.Real.mk_numeral_nd ctx 1 2 in
  (* let[@warning "-8"] z3_abs [x] = Z3.(
    Boolean.mk_ite ctx 
      (Arithmetic.mk_lt ctx x (n_0))
      (Arithmetic.mk_unary_minus ctx x) (x)
  ) in *)
  let[@warning "-8"] z3_trunc [x] = Z3.(
    Boolean.mk_ite ctx 
      (Arithmetic.mk_lt ctx x (n_0))
      (z3_ceil [x]) (z3_floor [x]) 
  ) in
  let[@warning "-8"] z3_round [x] = Z3.(
    Boolean.mk_ite ctx 
      (* (Arithmetic.mk_lt ctx (Arithmetic.mk_sub ctx [x ; Arithmetic.Real.mk_real2int ctx x]) (n_05)) *)
      (Arithmetic.mk_lt ctx x (Arithmetic.mk_add ctx [Arithmetic.Real.mk_real2int ctx x ; n_05]))
      (* (Arithmetic.Real.mk_real2int ctx x) (Arithmetic.mk_add ctx [Arithmetic.Real.mk_real2int ctx x ; Arithmetic.Integer.mk_numeral_i ctx 1]) *)
      (z3_floor [x]) (z3_ceil [x]) 
  ) in

  let[@warning "-8"] id [x] = x in
  let[@warning "-8"] const k _ = k in

  (* KK is_int_real is replaced with floor(x) = x *)
  let [@warning "-8"] z3_is_int_real [x] =
    dbg D_trace @@ lazy (sprintf "z3_is_int_real");
    Z3.(
    Boolean.mk_eq ctx (z3_floor [x]) x
                  )
  in      
  fmap 
  |> SMap.add Symbol.symb_sum_real        z3_add
  |> SMap.add Symbol.symb_difference_real z3_sub
  |> SMap.add Symbol.symb_product_real    z3_mul
  |> SMap.add Symbol.symb_quotient_real   z3_div
  |> SMap.add Symbol.symb_uminus_real     z3_uminus
  |> SMap.add Symbol.symb_less_real       z3_lt
  |> SMap.add Symbol.symb_lesseq_real     z3_le
  |> SMap.add Symbol.symb_greater_real    z3_gt
  |> SMap.add Symbol.symb_greatereq_real  z3_ge
  (* |> SMap.add Symbol.symb_is_int_real     z3_is_int *)
      (* |> SMap.add Symbol.symb_is_rat_real     z3_is_rat *)
 (* KK new *)     
  |> SMap.add Symbol.symb_is_int_real     z3_is_int_real
  (* Symbol.symb_is_rat_real is currently treated as uninterpreted; so not included here *)    
  |> SMap.add Symbol.symb_to_int_real     z3_to_int
  |> SMap.add Symbol.symb_to_rat_real     z3_to_rat_real
  |> SMap.add Symbol.symb_to_real_real    id
  |> SMap.add Symbol.symb_floor_real      z3_floor
  |> SMap.add Symbol.symb_ceiling_real    z3_ceil
  |> SMap.add Symbol.symb_truncate_real   z3_trunc
  |> SMap.add Symbol.symb_round_real      z3_round
  
  |> SMap.add Symbol.symb_sum_rat         z3_add
  |> SMap.add Symbol.symb_difference_rat  z3_sub
  |> SMap.add Symbol.symb_product_rat     z3_mul
  |> SMap.add Symbol.symb_quotient_rat    z3_div
  |> SMap.add Symbol.symb_uminus_rat      z3_uminus
  |> SMap.add Symbol.symb_less_rat        z3_lt
  |> SMap.add Symbol.symb_lesseq_rat      z3_le
  |> SMap.add Symbol.symb_greater_rat     z3_gt
  |> SMap.add Symbol.symb_greatereq_rat   z3_ge
  (* |> SMap.add Symbol.symb_is_int_rat      z3_is_int *)
  |> SMap.add Symbol.symb_is_rat_rat      (const (Z3.Boolean.mk_true ctx))
  |> SMap.add Symbol.symb_to_int_rat      z3_to_int
  |> SMap.add Symbol.symb_to_rat_rat      id
  |> SMap.add Symbol.symb_to_real_rat     z3_to_real_rat
  |> SMap.add Symbol.symb_floor_rat       z3_floor
  |> SMap.add Symbol.symb_ceiling_rat     z3_ceil
  |> SMap.add Symbol.symb_truncate_rat    z3_trunc
  |> SMap.add Symbol.symb_round_rat       z3_round
  
  |> SMap.add Symbol.symb_sum_int         z3_add
  |> SMap.add Symbol.symb_difference_int  z3_sub
  |> SMap.add Symbol.symb_product_int     z3_mul
  |> SMap.add Symbol.symb_quotient_e_int  z3_div_e
  |> SMap.add Symbol.symb_remainder_e_int z3_rem_e
  (* |> SMap.add Symbol.symb_abs_int         z3_abs *)
  |> SMap.add Symbol.symb_uminus_int      z3_uminus
  |> SMap.add Symbol.symb_less_int        z3_lt
  |> SMap.add Symbol.symb_lesseq_int      z3_le
  |> SMap.add Symbol.symb_greater_int     z3_gt
  |> SMap.add Symbol.symb_greatereq_int   z3_ge
  |> SMap.add Symbol.symb_is_int_int      (const (Z3.Boolean.mk_true ctx))
  |> SMap.add Symbol.symb_is_rat_int      (const (Z3.Boolean.mk_true ctx))
  |> SMap.add Symbol.symb_to_int_int      id
  |> SMap.add Symbol.symb_to_rat_int      z3_to_rat_int
  |> SMap.add Symbol.symb_to_real_int     z3_to_real_int
  |> SMap.add Symbol.symb_floor_int       id
  |> SMap.add Symbol.symb_ceiling_int     id
  |> SMap.add Symbol.symb_truncate_int    id
  |> SMap.add Symbol.symb_round_int       id

(* let add_arithmetic_defs state solver = 
  let ctx = state.ctx in
  let x = Z3.Expr.mk_const_s state.ctx "__arith_defs_qint"  (state.sort_map |> SMap.find Symbol.symb_int_type) in
  let y = Z3.Expr.mk_const_s state.ctx "__arith_defs_qreal" (state.sort_map |> SMap.find Symbol.symb_real_type) in
  let[@warning "-8"] z3_abs = Z3.(
    Boolean.mk_ite ctx 
      (Arithmetic.mk_lt ctx x (Arithmetic.Integer.mk_numeral_i ctx 0))
      (Arithmetic.mk_unary_minus ctx x) (x)
  ) in
  let[@warning "-8"] z3_ceil = Z3.(
    Boolean.mk_ite ctx 
      (Arithmetic.Real.mk_is_integer ctx y) 
      (y) (Arithmetic.mk_add ctx [Arithmetic.Real.mk_real2int ctx y ; Arithmetic.Integer.mk_numeral_i ctx 1])
  ) in
  let[@warning "-8"] z3_trunc = Z3.(
    Boolean.mk_ite ctx 
      (Arithmetic.mk_lt ctx y (Arithmetic.Integer.mk_numeral_i ctx 0))
      (z3_ceil) (Arithmetic.Real.mk_real2int ctx y) 
  ) in
  let[@warning "-8"] z3_round = Z3.(
    Boolean.mk_ite ctx 
      (Arithmetic.mk_lt ctx (Arithmetic.mk_sub ctx [y ; Arithmetic.Real.mk_real2int ctx y]) (Arithmetic.Real.mk_numeral_nd ctx 1 2))
      (Arithmetic.Real.mk_real2int ctx y) (Arithmetic.mk_add ctx [Arithmetic.Real.mk_real2int ctx y ; Arithmetic.Integer.mk_numeral_i ctx 1])
  ) in
  let[@inline] add var a b = 
    let body = Z3.Boolean.mk_iff ctx a b in
    Z3.Quantifier.mk_forall_const ctx [var] body None [] [] None None
    |> Z3.Quantifier.expr_of_quantifier
  in
  Z3.Solver.add solver [
    add x (z3_abs)   ((state.functheory_map |> SMap.find Symbol.symb_abs_int) [x]);
    add y (z3_ceil)  ((state.functheory_map |> SMap.find Symbol.symb_ceiling_real) [y]);
    add y (z3_trunc) ((state.functheory_map |> SMap.find Symbol.symb_truncate_real) [y]);
    add y (z3_round) ((state.functheory_map |> SMap.find Symbol.symb_round_real) [y]);
  ] *)



(*  Symbols in Z3 are indexed by ints. We use the following ranges: 
      lower    | upper       | 
      0        | 1 ×2^28 - 1 | symbols fast_key 
      1  ×2^28 | 2 ×2^28 - 1 | variables (no relation to var fast key)
      3  ×2^28 | 4 ×2^28 - 1 | tags
      4  ×2^28 | 5 ×2^28 - 1 | unsat core tags
      10 ×2^28 | ——          | atoms to propositional atoms
*)
let range_symb_l = 0
let range_symb_u = 1 lsl 28
let range_var_l = 1 lsl 28
let range_var_u = 2 lsl 28
(* let range_intprop_l = 2 lsl 28
let range_intprop_u = 3 lsl 28 *)
let range_tag_l = 3 lsl 28
let range_tag_u = 4 lsl 28
let range_uctag_l = 4 lsl 28
let range_uctag_u = 5 lsl 28
let range_prop_l = 10 lsl 28
let range_prop_u = Int.max_int



type options = {
  interpreted_arithmetic: bool;
}

let make_state opts = 
  let ctx = Z3.mk_context [] in

  (* Database of interpreted terms/sorts must already be baked into the initial state *)
  let sort_map = 
    SMap.empty 
    |> add_bool_sorts ctx
    |> if opts.interpreted_arithmetic then add_arithmetic_sorts ctx else Fun.id
  in
  
  let funcdecl_map =
    SMap.empty
    (* |> if opts.interpreted_arithmetic then add_arithmetic_funcdecls ctx sort_map else Fun.id *)
    (* |> SMap.add symb_typed_equality (Z3.) *)
  in

  let functheory_map = 
    SMap.empty 
    |> add_bool_funcs ctx
    |> if opts.interpreted_arithmetic then add_arithmetic_funcs ctx else Fun.id
  in

  {
    ctx;

    prop_term_id = range_prop_l;
    term_map = TMap.empty;

    sort_map = sort_map;

    var_id = range_var_l;
    var_map = VMap.empty;

    funcdecl_map;
    functheory_map;

    rev_symbol_map = IntMap.empty;

    (* numbering_max = 0;
    numbering_map = IntMap.empty; *)

    (*
    fast_tactic = 
      Z3.Tactic.mk_tactic ctx 
        "solve-eqs";
        (* "ctx-simplify"; *)
        (* "ctx-solver-simplify"; *)
        (* "smt"; *)
        (* "sat-preprocess"; *)
    *)
    fast_tactic = 
      let (|>) a b = Z3.Tactic.and_then ctx a b [] in
      let t x = Z3.Tactic.mk_tactic ctx x in
      t "simplify" |> t "ctx-simplify" |> t "ctx-solver-simplify" |> t "sat-preprocess"
  }





(* Plain version *)
let var_to_z3symbol state var =
  (* Map variables to z3 symbols in range [1*2^28, 2*2^28[ *)
  let n = state.var_id in
  
  dbg_env D_trace
    (fun () ->
      if (n >= range_var_u) then
        dbg D_trace @@ lazy (sprintf "var_to_z3symbol: v:%s range_var_l:%i range_var_u:%i state.var_id:%i n:%i " (Var.to_string var) range_var_l range_var_u state.var_id n)
      else()
    );
  assert (n < range_var_u);
  state.var_id <- succ state.var_id;
(* Z3.Symbol.mk_int state.ctx n *)
(* KK *)
  Z3.Symbol.mk_string state.ctx (sprintf "v_%i" n) 
  |> tap (fun x -> 
      dbg D_trace @@ lazy (sprintf "New entry: var %s is SMT %s" (Var.to_string var) (Z3.Symbol.to_string x))
         )

(* Caching version *)
let var_to_z3symbol state var =
  match VMap.find_opt var state.var_map with
  | Some x -> x
  | None -> 
    let result = var_to_z3symbol state var in
    state.var_map <- VMap.add var result state.var_map;
    state.rev_symbol_map <- IntMap.add (state.var_id - 1) (Var var) state.rev_symbol_map;
    result



let type_to_z3sort' state typ =
  
  (* Map symbols to z3 symbols in range [0*2^28, 1*2^28[ *)
  let n = Symbol.get_fast_key typ + range_symb_l in
  assert (n < range_symb_u);
  (* let symb' = Z3.Symbol.mk_int state.ctx n in *)
  (* let symb' = Z3.Symbol.mk_string state.ctx (string_of_int n) in *)
  (* KK *)
  let symb' = Z3.Symbol.mk_string state.ctx (sprintf "s_%i" n) in
  dbg D_trace @@ lazy (sprintf "New entry: type symb %s is SMT %s" (Symbol.to_string typ) (Z3.Symbol.to_string symb'));
  Z3.Sort.mk_uninterpreted state.ctx symb'

let type_to_z3sort state typ =
  match SMap.find_opt typ state.sort_map with
  | Some x -> x
  | None ->
    let result = type_to_z3sort' state typ in
    state.sort_map <- SMap.add typ result state.sort_map;
    result
    (* |> tap (fun x -> state.sort_map <- SMap.add typ x state.sort_map) *)



let func_to_z3funcdecl' state symb =
  (* Map symbols to z3 symbols in range [0*2^28, 1*2^28[ *)
  let n = Symbol.get_fast_key symb + range_symb_l in
  assert (n < range_symb_u);
(*  let symb' = Z3.Symbol.mk_int state.ctx n in  *)
  (* KK *)
  let symb' = Z3.Symbol.mk_string state.ctx (sprintf "f_%i" n) in
  
  (* dbg D_trace @@ lazy (sprintf "New entry: symb %s is SMT %s" (Symbol.to_string symb) (Z3.Symbol.to_string symb')); *)

  let args_typ, value_typ = Symbol.get_stype_args_val_def symb in
  let args_typ' = List.map (type_to_z3sort state) args_typ in
  let value_typ' = type_to_z3sort state value_typ in

  dbg D_trace @@ lazy (sprintf "New entry: func_decl %s : %s > %s is SMT %s : %s > %s" 
    (Symbol.to_string symb)      
    (List.X.to_string ~first:"" ~last:"" ~sep:" * " Symbol.to_string args_typ) 
    (Symbol.to_string value_typ)

    (Z3.Symbol.to_string symb')
    (List.X.to_string ~first:"" ~last:"" ~sep:" * " Z3.Sort.to_string args_typ')
    (Z3.Sort.to_string value_typ')
  );

  Z3.FuncDecl.mk_func_decl state.ctx symb' args_typ' value_typ'

let func_to_z3funcdecl state symb =
  match SMap.find_opt symb state.funcdecl_map with
  | Some x -> x
  | None ->
    let result = func_to_z3funcdecl' state symb in
    state.funcdecl_map <- SMap.add symb result state.funcdecl_map;
    state.rev_symbol_map <- IntMap.add (Symbol.get_fast_key symb) (Symb symb) state.rev_symbol_map;
    result


      
let rec term_to_z3expr' state t =
  match t with 
  | Term.Fun (symb, args, _) ->
      (* Handle equality predicate *)
      if symb == Symbol.symb_typed_equality then (
      let (typ, l,r) = Term.get_3_args args in
      let l' = term_to_z3expr state l in
      let r' = term_to_z3expr state r in
      Z3.Boolean.mk_eq state.ctx l' r'
        (* Handle negation *)
       ) else if symb == Symbol.symb_neg then (
      let x = Term.get_1_args args in
      let x' = term_to_z3expr state x in
      Z3.Boolean.mk_not state.ctx x'
        ) else if (symb == Symbol.symb_false_fun || symb == Symbol.symb_true_fun) then (
          (* KK for now symb_false_fun  are treated as usual functions; otherwise literals can be simplified into vars *)
          let fun_decl = (func_to_z3funcdecl state symb) in
          Z3.Expr.mk_app state.ctx fun_decl []
         ) else (* if (symb == Symbol.symb_is_int_real) then (
          ) else *) begin match Symbol.get_property symb with

            (* Handle theory function *)
            (* Symbol.symb_is_rat_real is currently treated as uninterpreted *)         
          | Theory when symb != Symbol.symb_is_rat_real ->
              (try
                let func' = SMap.find symb state.functheory_map in
                let args' = List.map (term_to_z3expr state) (Term.arg_to_list args) in
                (* TODO dassert correct sort *)
                func' args'
              with Not_found ->
                failwith (sprintf "term_to_z3expr': Theory symb is not defined: %s t: %s" (Symbol.to_string symb) (Term.to_string t)) 
              )    
                (* Handle numeral *)
          | Num_int _ -> 
              Z3.Arithmetic.Integer.mk_numeral_s state.ctx (Symbol.get_name symb)
             (* | Num_rat (_, n, d) -> 
                let n = Z3.Arithmetic.Real.mk_numeral_s state.ctx n in 
                let d = Z3.Arithmetic.Real.mk_numeral_s state.ctx d in 
                Z3.Arithmetic.mk_div state.ctx n d *)
          | Num_rat _ -> 
              Z3.Arithmetic.Real.mk_numeral_s state.ctx (Symbol.get_name symb)
          | Num_real _ -> 
              Z3.Arithmetic.Real.mk_numeral_s state.ctx (Symbol.get_name symb)
                (* Handle regular predicate/function *)
          | _ ->
              let args' = List.map (term_to_z3expr state) (Term.arg_to_list args) in
              let func_decl' = func_to_z3funcdecl state symb in
              Z3.Expr.mk_app state.ctx func_decl' args'
          end
  | Term.Var (var, _) ->
      let sort' = type_to_z3sort state (Var.get_type var) in
      let var' = var_to_z3symbol state var in
      Z3.Expr.mk_const state.ctx var' sort'

and term_to_z3expr state t =
  match TMap.find_opt t state.term_map with
  | Some Term_map.{regular=Some x} -> 
    x
  (* | Some (None  , y) -> *)
  | Some (Term_map.{regular=None} as value) -> 
    let result = term_to_z3expr' state t in
    (* let value = (Some result, y) in *)
    let value = {value with regular = Some result} in
    state.term_map <- TMap.add t value state.term_map;
    result
  | None -> 
    let result = term_to_z3expr' state t in
    let value = {Term_map.empty with regular = Some result} in
    state.term_map <- TMap.add t value state.term_map;
    result



let lit_to_smt = term_to_z3expr

let clause_to_smt state clause =
  let lits = Clause.get_lits clause in
  match lits with 
  | [lit] ->
    term_to_z3expr state lit
  | lits ->
    Z3.Boolean.mk_or state.ctx (List.map (term_to_z3expr state) lits)



let clause_to_smt_ground state clause =
  let clause' = Prop_solver_exchange.ground_abstr_clause clause in
  clause_to_smt state clause'



let term_to_z3expr_prop' state t = 
(*  let n = state.prop_term_id in *)
  (* KK *)
  let n = range_prop_u + state.prop_term_id in 
  assert (n < range_prop_u);
  state.prop_term_id <- succ state.prop_term_id;
  Z3.Symbol.mk_int state.ctx n 
  |> Z3.Boolean.mk_const state.ctx

let term_to_z3expr_prop state t = 
  dbg D_trace @@ lazy "term_to_z3expr_prop";
  let sign, atom = Term.split_sign_lit t in
  let prop_atom = 
    match TMap.find_opt t state.term_map with
    | Some Term_map.{prop=Some x} -> 
      x
    | Some (Term_map.{prop=None} as value) ->
      let result = term_to_z3expr_prop' state t in
      let value = {value with prop = Some result} in
      state.term_map <- TMap.add t value state.term_map;
      result
    | None -> 
      let result = term_to_z3expr_prop' state t in
      let value = {Term_map.empty with prop = Some result} in
      state.term_map <- TMap.add t value state.term_map;
      result
  in
  (
  if sign then
    prop_atom
  else
    Z3.Boolean.mk_not state.ctx prop_atom
  ) |> tap (fun _ -> dbg_env D_trace (fun () ->
    dbg D_trace @@ lazy "term_map:"; 
    state.term_map |> TMap.iter (fun key Term_map.{regular; prop; quant} ->
      dbg D_trace @@ lazy (sprintf "%s -> (%s , %s, %s)" 
        (Term.to_string key) 
        (Option.to_string Z3.Expr.to_string regular)
        (Option.to_string Z3.Expr.to_string prop)
        (Option.to_string Z3.Expr.to_string quant)
      )
    )
  ))


  
let lit_to_smt_prop = term_to_z3expr_prop

let clause_to_smt_prop state clause =
  let lits = Clause.get_lits clause in
  match lits with
  | [lit] -> 
    term_to_z3expr_prop state lit
  | lits -> 
    Z3.Boolean.mk_or state.ctx (List.map (term_to_z3expr_prop state) lits)



(*
let int_to_smt_prop state sign n = 
  dbg D_trace @@ lazy (sprintf "int_to_smt_prop: %c%d" (if sign then '+' else '-') (n));
  let z3_atom = 
    (* let n = state.prop_term_id + (2 lsl 28) in *)
    (* state.prop_term_id <- succ state.prop_term_id; *)
    let n = n + range_intprop_l in
    assert (n < range_intprop_u);
    Z3.Symbol.mk_int state.ctx n 
    |> Z3.Boolean.mk_const state.ctx
  in
  (if sign then
    z3_atom
  else
    Z3.Boolean.mk_not state.ctx z3_atom
  ) |> tap (fun x -> dbg D_trace @@ lazy (sprintf "becomes %s" (Z3.Expr.to_string x)))
*)





let rec term_to_z3expr_quant' state t =
  match t with 
  | Term.Fun (symb, args, _) ->
    (* Handle equality predicate *)
    if symb == Symbol.symb_typed_equality then (
      let (typ, l,r) = Term.get_3_args args in
      let l' = term_to_z3expr_quant state l in
      let r' = term_to_z3expr_quant state r in
      Z3.Boolean.mk_eq state.ctx l' r'
    (* Handle negation *)
    ) else if symb == Symbol.symb_neg then (
      let x = Term.get_1_args args in
      let x' = term_to_z3expr state x in
      Z3.Boolean.mk_not state.ctx x'
    (* Handle regular predicate/ *)
    ) else (
      let args' = List.map (term_to_z3expr_quant state) (Term.arg_to_list args) in
      let func_decl' = func_to_z3funcdecl state symb in
      Z3.Expr.mk_app state.ctx func_decl' args'
    )
  | Term.Var (var, _) ->
    failwith "unimplemented"

and term_to_z3expr_quant state t =
  match TMap.find_opt t state.term_map with
  | Some Term_map.{quant=Some x} -> 
    x
  | Some (Term_map.{quant=None} as value) -> 
    let result = term_to_z3expr_quant' state t in
    let value = {value with quant = Some result} in
    state.term_map <- TMap.add t value state.term_map;
    result
  | None -> 
    let result = term_to_z3expr_quant' state t in
    let value = {Term_map.empty with quant = Some result} in
    state.term_map <- TMap.add t value state.term_map;
    result

let lit_to_smt_quant = term_to_z3expr_quant

let clause_to_smt_quant state clause =
  let lits = Clause.get_lits clause in
  match lits with
  | [lit] ->
    term_to_z3expr_quant state lit
  | lits ->
    Z3.Boolean.mk_or state.ctx (List.map (term_to_z3expr_quant state) lits)





exception Return

(* Aux functions: all rational numbers are mapped by Z3 to the same "real" sort, so here we must coerce *)
let real_to_rat term =
  match term with
  | Term.Fun (symb, _, _) -> 
    dbg D_trace @@ lazy (sprintf "real_to_rat %s" (Term.to_string term));
    begin match Symbol.get_property symb with
    | Symbol.Num_real q -> 
      dassert (fun () -> Symbol.get_val_type_def symb == Symbol.symb_real_type);
        let name = Symbol.get_name symb in
        (* Todo: add  n/1; all rat have /; all reals are n1.m1/n2.m2 *)
        let name = if String.ends_with ~suffix:".0" name then String.sub name 0 (String.length name - 2) else name in
      dassert (fun () -> not @@ String.contains name '/');
      (* let name = if String.contains name '/' then name else name^"/1" in *)
        let symb = create_symbol_property name (Symbol.create_stype [] Symbol.symb_rat_type) (Symbol.Num_rat (q)) in
        add_fun_term symb [] |> tap (fun x -> dbg D_trace @@ lazy (sprintf "into %s" (Term.to_string x)))
      | _ -> dassert (fun () -> Symbol.get_val_type_def symb != Symbol.symb_real_type); term
    end
  | Term.Var _ -> term

let rat_to_real term =
  match term with
  | Term.Fun (symb, _, _) -> 
    dbg D_trace @@ lazy (sprintf "rat_to_real %s" (Term.to_string term));
    begin match Symbol.get_property symb with
    | Symbol.Num_rat q -> 
        dassert (fun () -> Symbol.get_val_type_def symb == Symbol.symb_rat_type);
        let nat_name_to_real n = if String.contains n '.' then n else n^".0" in
        let name = Symbol.get_name symb in
        let name = 
          match name |> String.split_on_char '/' with
      (* KK: that's wrong cast: fixed next line *)
(*        | [n;d] -> string_of_float (float_of_string n /. float_of_string d) *)
          | [n;d] -> sprintf "%s/%s" (nat_name_to_real n) (nat_name_to_real d) 
          | [n] -> nat_name_to_real n 
        | _ -> assert false
      in
      dassert (fun () -> String.contains name '.');
      let symb = create_symbol_property name (Symbol.create_stype [] Symbol.symb_real_type) (Symbol.Num_real (q)) in
      add_fun_term symb [] |> tap (fun x -> dbg D_trace @@ lazy (sprintf "into %s" (Term.to_string x)))
    | _ -> dassert (fun () -> Symbol.get_val_type_def symb != Symbol.symb_rat_type); term
    end
  | Term.Var _ -> term

let[@warning "-8"] choose_type args' = 
  args'
  |> List.filter (function Term.Fun(s,_,_) -> (match Symbol.get_property s with Symbol.Num_int _ | Symbol.Num_rat _ | Symbol.Num_real _ -> false | _ -> true) | Term.Var _ -> true)
  |> function
    | hd::_ -> (match hd with Term.Fun(s,_,_) -> Symbol.get_val_type_def s | Var(x,_) -> Var.get_type x)
    | [] -> Symbol.symb_real_type

let[@warning "-8"] dassert_types args args' =
  dbg D_trace @@ lazy (sprintf "dassert_types: %s" (List.X.to_string Symbol.to_string (List.map (function Term.Fun(s,_,_) -> Symbol.get_val_type_def s | Var (x,_) -> Var.get_type x) args')));
  dassert (fun () -> 
    (let hd::tl = List.map Z3.Expr.get_sort args in
    List.for_all (Z3.Sort.equal hd) tl) &&
    (let hd::tl = List.map (function Term.Fun(s,_,_) -> Symbol.get_val_type_def s | Var (x,_) -> Var.get_type x) args'
    in List.for_all (fun x -> x == hd) tl)
  )

let rec term_of_z3expr state term = 
  let[@inline] [@warning "-8"] typed args args' fi fa fr = 
    let typ = choose_type args' in
    dassert (fun () -> Z3.Arithmetic.is_int (List.hd args) != Z3.Arithmetic.is_real (List.hd args));
    dassert (fun () -> Z3.Sort.equal (Z3.Expr.get_sort (List.hd args)) (state.sort_map |> SMap.find typ));
    if typ == Symbol.symb_int_type then (
      dassert (fun () -> Z3.Arithmetic.is_int (List.hd args));
      dassert_types args args';
      add_fun_term fi args'
    ) else if typ == Symbol.symb_rat_type then (
      dassert (fun () -> Z3.Arithmetic.is_real (List.hd args));
      dassert_types args (List.map real_to_rat args');
      add_fun_term fa (List.map real_to_rat args')
    ) else if typ == Symbol.symb_real_type then (
      dassert (fun () -> Z3.Arithmetic.is_real (List.hd args));
      dassert_types args (List.map rat_to_real args');
      add_fun_term fr (List.map rat_to_real args')
    ) else assert false
  in
  let[@inline] typed_variadic args args' fi fa fr = 
    let typ = choose_type args' in
    dassert (fun () -> Z3.Arithmetic.is_int (List.hd args) != Z3.Arithmetic.is_real (List.hd args));
    dassert (fun () -> Z3.Sort.equal (Z3.Expr.get_sort (List.hd args)) (state.sort_map |> SMap.find typ));
    if typ == Symbol.symb_int_type then (
      dassert (fun () -> Z3.Arithmetic.is_int (List.hd args));
      dassert_types args args';
      AC.mk_term fi args'
    ) else if typ == Symbol.symb_rat_type then (
      dassert (fun () -> Z3.Arithmetic.is_real (List.hd args));
      dassert_types args (List.map real_to_rat args');
      AC.mk_term fa (List.map real_to_rat args')
    ) else if typ == Symbol.symb_real_type then (
      dassert (fun () -> Z3.Arithmetic.is_real (List.hd args));
      dassert_types args (List.map rat_to_real args');
      AC.mk_term fr (List.map rat_to_real args')
    ) else assert false
  in

  dbg D_trace @@ lazy (sprintf "term_of_z3expr: %s" (Z3.Expr.to_string term));
  let args = Z3.Expr.get_args term in
  let args' = List.map (term_of_z3expr state) args in

  let result = 

  if Z3.Boolean.is_true term then
    term_true (* add_fun_term Symbol.symb_true_fun [] *)
  else if Z3.Boolean.is_false term then
    term_false (* add_fun_term Symbol.symb_false_fun [] *)

  else if Z3.Arithmetic.is_add term then
    typed_variadic args args' Symbol.symb_sum_int Symbol.symb_sum_rat Symbol.symb_sum_real
  else if Z3.Arithmetic.is_sub term then
    typed_variadic args args' Symbol.symb_difference_int Symbol.symb_difference_rat Symbol.symb_difference_real
  else if Z3.Arithmetic.is_mul term then
    typed_variadic args args' Symbol.symb_product_int Symbol.symb_product_rat Symbol.symb_product_real
  else if Z3.Arithmetic.is_div term then
    (dassert (fun () -> Z3.Arithmetic.is_real term); add_fun_term Symbol.symb_quotient_real args')
  else if Z3.Arithmetic.is_uminus term then
    typed args args' Symbol.symb_uminus_int Symbol.symb_uminus_rat Symbol.symb_uminus_real
  else if Z3.Arithmetic.is_lt term then
    typed args args' Symbol.symb_less_int Symbol.symb_less_rat Symbol.symb_less_real
  (* These three should not occur in normalised terms? *)
  else if Z3.Arithmetic.is_le term then
    typed args args' Symbol.symb_lesseq_int Symbol.symb_lesseq_rat Symbol.symb_lesseq_real
  else if Z3.Arithmetic.is_gt term then
    typed args args' Symbol.symb_greater_int Symbol.symb_greater_rat Symbol.symb_greater_real
  else if Z3.Arithmetic.is_ge term then
    typed args args' Symbol.symb_greatereq_int Symbol.symb_greatereq_rat Symbol.symb_greatereq_real
  else if Z3.Arithmetic.is_int2real term then
    add_fun_term Symbol.symb_to_real_int args'
  else if Z3.Arithmetic.is_real2int term then
    add_fun_term Symbol.symb_to_int_real args'
(*      
  else if Z3.Arithmetic.is_real_is_int term then
    add_fun_term Symbol.symb_is_int_real args'
*)
  else if Z3.Arithmetic.is_int_numeral term then
    let name = Z3.Arithmetic.Integer.numeral_to_string term in
    let symb = create_symbol_property name (Symbol.create_stype [] Symbol.symb_int_type) (Symbol.Num_int (Z.of_string name)) in
    add_fun_term symb []
  else if Z3.Arithmetic.is_rat_numeral term then 
    let name = Z3.Arithmetic.Real.numeral_to_string term in
    if String.contains name '/' then 
      (* Rational *)
      let symb = create_symbol_property name (Symbol.create_stype [] Symbol.symb_rat_type) (Symbol.Num_rat (Q.of_string name)) in
      add_fun_term symb []
    else 
      (* Real *or* rational *)
      let name = if String.contains name '.' then name else name^".0" in
      let symb = create_symbol_property name (Symbol.create_stype [] Symbol.symb_real_type) (Symbol.Num_real (Q.of_string name)) in
      add_fun_term symb []

  else (
    dbg D_trace @@ lazy (sprintf "%d" (term |> Z3.Expr.get_func_decl |> Z3.FuncDecl.get_decl_kind |> Z3enums.int_of_decl_kind));
    dbg D_trace @@ lazy (sprintf "%s" (term |> Z3.Expr.get_func_decl |> Z3.FuncDecl.to_string));
    dbg D_trace @@ lazy (sprintf "%s" (term |> Z3.Expr.get_func_decl |> Z3.FuncDecl.get_name |> Z3.Symbol.to_string));
    let symb = term |> Z3.Expr.get_func_decl |> Z3.FuncDecl.get_name |> Z3.Symbol.get_int in
    (* if symb == Symbol.(get_fast_key symb_ceiling_real) then
      add_fun_term Symbol.symb_ceiling_real args'
    else if symb == Symbol.(get_fast_key symb_truncate_real) then
      add_fun_term Symbol.symb_truncate_real args'
    else if symb == Symbol.(get_fast_key symb_round_real) then
      add_fun_term Symbol.symb_round_real args'
    else if symb == Symbol.(get_fast_key symb_abs_int) then
      add_fun_term Symbol.symb_abs_int args'
    else *)
      (* let symb' = IntMap.find symb state.rev_symbol_map in *)
      begin match IntMap.find symb state.rev_symbol_map with
      | Symb symb' -> add_fun_term symb' args'
      | Var var' -> add_var_term var'
      end
  )

  in dbg D_trace @@ lazy (sprintf "term_of_z3expr: %s to %s" (Z3.Expr.to_string term) (Term.to_string result)); result

let rec lit_of_z3expr state lit = 
  let[@inline] typed args args' fi fa fr = 
    let typ = choose_type args' in
    dassert (fun () -> Z3.Arithmetic.is_int (List.hd args) != Z3.Arithmetic.is_real (List.hd args));
    (*dbg D_trace @@ lazy (sprintf "cuona %s %s" (Z3.Sort.to_string @@ Z3.Expr.get_sort (List.hd args)) (Z3.Sort.to_string @@ SMap.find typ state.sort_map));*)
    dbg D_trace @@ lazy (sprintf "cuona %s %s" (Z3.Sort.to_string @@ Z3.Expr.get_sort (List.hd args)) (Symbol.to_string typ));
    dassert (fun () -> Z3.Sort.equal (Z3.Expr.get_sort (List.hd args)) (state.sort_map |> SMap.find typ));
    if typ == Symbol.symb_int_type then (
      dassert (fun () -> Z3.Arithmetic.is_int (List.hd args));
      dassert_types args args';
      add_fun_term fi args'
    ) else if typ == Symbol.symb_rat_type then (
      dassert (fun () -> Z3.Arithmetic.is_real (List.hd args));
      dassert_types args (List.map real_to_rat args');
      add_fun_term fa (List.map real_to_rat args')
    ) else if typ == Symbol.symb_real_type then (
      dassert (fun () -> Z3.Arithmetic.is_real (List.hd args));
      dassert_types args (List.map rat_to_real args');
      add_fun_term fr (List.map rat_to_real args')
    ) else assert false
  in

  dbg D_trace @@ lazy (sprintf "lit_of_z3expr: %s" (Z3.Expr.to_string lit));
  let args = Z3.Expr.get_args lit in
  if Z3.Boolean.is_not lit then
    let[@warning "-8"] [x] = args in
    add_compl_lit (lit_of_z3expr state x)
  else if Z3.Boolean.is_eq lit then
    let[@warning "-8"] [x;y] = args in
    let x' = term_of_z3expr state x in
    let y' = term_of_z3expr state y in
    let typ_x' = Term.get_term_type x' in
    let typ_y' = Term.get_term_type y' in
    if typ_x' == typ_y' then
      add_typed_equality_sym typ_x' x' y'
    else 
      let x' = rat_to_real x' in
      let y' = rat_to_real y' in
      let typ_x' = Term.get_term_type x' in
      let typ_y' = Term.get_term_type y' in
      if typ_x' == typ_y' then
        add_typed_equality_sym typ_x' x' y'
      else 
        let x' = real_to_rat x' in
        let y' = real_to_rat y' in
        let typ_x' = Term.get_term_type x' in
        let typ_y' = Term.get_term_type y' in
        let _ = dassert (fun () -> typ_x' == typ_y') in
        add_typed_equality_sym typ_x' x' y'
  else if Z3.Arithmetic.is_lt lit then
    let args' = List.map (term_of_z3expr state) args in
    typed args args' Symbol.symb_less_int Symbol.symb_less_rat Symbol.symb_less_real
  (* These three should not occur in normalised terms? *)
  else if Z3.Arithmetic.is_le lit then
    let args' = List.map (term_of_z3expr state) args in
    typed args args' Symbol.symb_lesseq_int Symbol.symb_lesseq_rat Symbol.symb_lesseq_real
  else if Z3.Arithmetic.is_gt lit then
    let args' = List.map (term_of_z3expr state) args in
    typed args args' Symbol.symb_greater_int Symbol.symb_greater_rat Symbol.symb_greater_real
  else if Z3.Arithmetic.is_ge lit then
    let args' = List.map (term_of_z3expr state) args in
    typed args args' Symbol.symb_greatereq_int Symbol.symb_greatereq_rat Symbol.symb_greatereq_real

  else
    term_of_z3expr state lit

let lit_of_z3expr state lit = 
  let lit' = lit_of_z3expr state lit in
  if Term.get_top_symb lit' == Symbol.symb_true_fun then
    raise_notrace Return
  else if Term.get_top_symb lit' == Symbol.symb_false_fun then
    None
  else
    (*  *)
    Some lit'

let clause_of_z3expr state clause = 
  dbg D_trace @@ lazy (sprintf "clause_of_z3expr: %s" (Z3.Expr.to_string clause));
  dbg_env D_trace (fun () -> 
    dbg D_trace @@ lazy "rev_map";
    state.rev_symbol_map |> IntMap.iter (fun k v -> 
      dbg D_trace @@ lazy (sprintf "  %d %s" k (match v with Symb s -> Symbol.to_string s | Var v -> Var.to_string v));
    )
  );
  if Z3.Boolean.is_or clause then
    Z3.Expr.get_args clause
    |> List.X.filter_map (lit_of_z3expr state)
  else
    match lit_of_z3expr state clause with Some x -> [x] | None -> []

let term_of_smt = term_of_z3expr

let lit_of_smt state lit = 
  match lit_of_z3expr state lit with
  | Some _ as lit' -> lit'
  | None -> Some term_false
  | exception Return -> Some term_true
  | exception Z3.Error s -> dassert (fun () -> String.equal s "invalid argument"); None

let clause_of_smt state clause = 
  try Some (clause_of_z3expr state clause)
  with Return -> None





let term_to_unit_clause state x = 
  x

let term_list_to_clause state l =
  dbg D_trace @@ lazy (sprintf "formula_list_to_formula: id=%d" (Obj.magic state : int));
  dbg_env D_trace (fun () ->
    l |> List.iteri (fun i x ->
      (* eprintf "%d: %s\n" (i) (Z3.Sort.to_string (Z3.Expr.get_sort x)) *)
      dbg D_trace @@ lazy (sprintf "[%d] %s: %s id=%d" (i) (Z3.Expr.to_string x) (Z3.Sort.to_string (Z3.Expr.get_sort x)) (Obj.magic x : int))
    )
  );
  match l with
  | [] -> invalid_arg "formula_list_to_formula: empty list"
  | [x] -> x
  | _::_::_ -> 
      let [@warning "-26"] rec check l' =
        match l' with
        | a::b::tl -> 
            dbg D_trace @@ lazy (sprintf "checking %s = %s" (Z3.Sort.to_string @@ Z3.Expr.get_sort a) (Z3.Sort.to_string @@ Z3.Expr.get_sort b));
            (* assert (Z3.Expr.get_sort a != Z3.Expr.get_sort b); check (b::tl) *)
            assert (Z3.Sort.equal (Z3.Expr.get_sort a) (Z3.Expr.get_sort b)); check (b::tl)
        | _ -> ()
      in
    dbg D_trace @@ lazy "fltf: foo";
    (* [%dbg D_trace "foo"] *)
    (* check l;  (* l has all same type *)
    l |> List.iter (fun x -> assert (Z3.Boolean.is_bool x));  (* and that type is clearly bool *) *)
    dbg D_trace @@ lazy "fltf: bar";
    Z3.Boolean.mk_or state.ctx (l |> tap (fun x -> dbg D_trace @@ lazy (sprintf "len=%d" (List.length x))))
    (* then why "Sort mismatch at argument #1 for function (declare-fun or (Bool Bool) Bool) supplied sort is Bool"?? *)
    |> tap (fun _ -> dbg D_trace @@ lazy "fltf: qux")

let tagged_formula state id clause =
  let id_term = 
    let n = id + range_tag_l in
    assert (n < range_tag_u);
    Z3.Symbol.mk_int state.ctx n
    |> Z3.Boolean.mk_const state.ctx
  in
  Z3.Boolean.mk_or state.ctx [id_term; clause]





type problem = Z3.Solver.solver

let make_problem state =
  (* Z3.Solver.mk_simple_solver state.ctx *)
  let solver = Z3.Solver.mk_solver state.ctx None in
  (* add_arithmetic_defs state solver; *)
  solver





let add solver clause =
  dbg D_trace @@ lazy (sprintf "add: %s" (Z3.Expr.to_string clause));
  (* try *)
  Z3.Solver.add solver [clause]
  (* with Z3.Error _ -> dbg D_trace @@ lazy "ERROR!" *)

let add_many solver clauses =
  dbg_env D_trace (fun () ->
    clauses |> List.iter (fun x ->
      dbg D_trace @@ lazy (sprintf "add_many: %s" (Z3.Expr.to_string x));
    )
  );
  Z3.Solver.add solver clauses

let push solver = 
  Z3.Solver.push solver

let pop solver = 
  Z3.Solver.pop solver 1

(* pp_ *)

let clear solver = 
  Z3.Solver.reset solver





type result = Sat | Unsat | Unknown
(* INVARIANT: Cannot have fields (`of stuff`) *)

let z3status_to_result x =
  match x with
  | Z3.Solver.SATISFIABLE   -> Sat
  | Z3.Solver.UNKNOWN       -> Unknown
  | Z3.Solver.UNSATISFIABLE -> Unsat

let check_assumptions solver assumptions =
 Statistics.(time smt_solver_time) @@ fun () -> 

  (* dbg D_trace @@ lazy (sprintf "assumptions = %a\n" (List.X.output ~sep:"\n" Z3.Expr.to_string) assumptions); *)
  dbg D_trace @@ lazy (sprintf "clauses = %s" (List.X.to_string ~sep:"\n" Z3.Expr.to_string (Z3.Solver.get_assertions solver)));
  dbg D_trace @@ lazy (sprintf "assumptions = %s" (List.X.to_string ~sep:"\n" Z3.Expr.to_string assumptions));
  Statistics.(bump_int_stat smt_solver_calls);
  let status = Z3.Solver.check solver assumptions in
  dbg D_trace @@ lazy (sprintf "result = %s" (Z3.Solver.string_of_status status));
  z3status_to_result status

let check solver =
  check_assumptions solver []

(* Fast check *)
(* Method 1: try to simplify assumptions to tautology *)
(* let fast_check_assumptions solver assumptions = 
  dbg D_trace @@ lazy (sprintf "fast_check");
  dbg D_trace @@ lazy (sprintf "clauses = %s" (List.X.to_string ~sep:"\n" Z3.Expr.to_string (Z3.Solver.get_assertions solver)));
  dbg D_trace @@ lazy (sprintf "assumptions = %s" (List.X.to_string ~sep:"\n" Z3.Expr.to_string assumptions));

  Statistics.(bump_int_stat smt_fast_solver_calls);
  let start_time = Unix.gettimeofday() in
  let result = 
    List.for_all (fun c ->
      dbg D_trace @@ lazy (sprintf "c  %s" (Z3.Expr.to_string c));
      let c' = Z3.Expr.simplify c None in
      dbg D_trace @@ lazy (sprintf "c' %s" (Z3.Expr.to_string c'));
      Z3.Boolean.is_false c'
    ) assumptions
  in
  let end_time = Unix.gettimeofday() in
  Statistics.(add_float_stat (end_time -. start_time) smt_fast_solver_time);
  match result with
  | true -> Unsat
  | false -> Unknown *)





type model = Z3.Model.model

let model solver = 
  None



module Uc = struct
  type problem = {
    prob: Z3.Solver.solver; 
    mutable next_number: int;
    ctx: Z3.context;
  }

  type tag = Z3.Expr.expr

  let tag_equal = Z3.Expr.equal

  let make_problem state = {
    prob = make_problem state;
    next_number = range_uctag_l;
    ctx = state.ctx;
  }

  let next_numbering (solver:problem) =
    let n = solver.next_number in
    solver.next_number <- succ solver.next_number;
    Z3.Symbol.mk_int solver.ctx n
    |> Z3.Boolean.mk_const solver.ctx

  let add (solver:problem) clause : tag =
    let tag = next_numbering solver in
    Z3.Solver.assert_and_track solver.prob clause tag;
    tag

  let add_many (solver:problem) clauses : tag list =
    (* clauses |> List.map (fun c ->
      let tag = next_numbering solver in
      Z3.Solver.assert_and_track solver.prob c tag;
      tag
    ) *)
    let tags = List.map (fun _ -> next_numbering solver) clauses in
    Z3.Solver.assert_and_track_l solver.prob clauses tags;
    tags

  let check solver = check solver.prob

  let check_assumptions solver assumptions = check_assumptions solver.prob assumptions

  let push solver = push solver.prob

  let pop solver = pop solver.prob

  let clear solver = clear solver.prob

  let unsat_core solver =
    Z3.Solver.get_unsat_core solver.prob
end



let make_problem_fast state = 
  (* Z3.Solver.mk_solver state.ctx None *)
  let solver = Z3.Solver.mk_solver_t state.ctx state.fast_tactic in
  (* add_arithmetic_defs state solver; *)
  solver

(* module Fast = struct
  type _problem = problem
  type problem = _problem

  let make_problem = make_problem_fast

  let add = add

  let add_many = add_many

  let check = check

  let check_assumptions = check_assumptions

  let push = push

  let pop = pop

  let clear = clear
end *)



let eval_term model x =
  match
  Z3.Model.eval model x false (* ??? what is the last parameter, docs don't say. UPDATE: it's "completion, default false" *)
  with
  | Some x -> x
  | None -> failwith "SMTSolver_Z3.eval: cannot evaluate"

let eval_term_bool model x =
  let r = eval_term model x in
  match Z3.Boolean.get_bool_value r with
  | Z3enums.L_FALSE -> Some false
  | Z3enums.L_TRUE  -> Some true
  | Z3enums.L_UNDEF -> None

let eval_clause = eval_term

let eval_clause_bool = eval_term_bool



let simplify_term (*state*) x =
  Z3.Expr.simplify x None

let simplify_clause = simplify_term





module Term = struct
  type t = term

  let equal a b = 
    Z3.Expr.equal a b

  let compare a b = 
    Z3.Expr.compare a b

  let hash x = 
    failwith "unimplemented" [@@alert unimplemented "unimplemented"]
end

module Clause = struct
  type t = clause

  let equal a b = 
    Z3.Expr.equal a b

  let compare a b = 
    Z3.Expr.compare a b

  let hash x = 
    failwith "unimplemented" [@@alert unimplemented "unimplemented"]
end
