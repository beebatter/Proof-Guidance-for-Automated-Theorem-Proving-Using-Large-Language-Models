(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Logic_interface
  
module LI = Logic_interface
module AD = Ar_abstract_domain
module C = Ar_common
    
module SKE = struct
  include Symbol.Key
  let to_string s = Symbol.to_string s
end

(* We use a bimap as the data structure to store the abstract signature *)
module BM = Bimap.Make(SKE)(SKE)

type info_abstr =
  { opts : Options.options;
    id : string;
  }

module SigGrup = struct
  (* Includes the type t and functions: add, mem, ... *)
  include BM

  type csymb = BM.key
  type asymb = BM.value
  type info = info_abstr

  let abstraction_type = Options.Abstr_ref_sig

  let init _ = BM.empty

  let cstsymb _ symb = symb

  let abstract_symbol info symbol =
    let stype = Symbol.get_type symbol in
    let (arg_ts,val_t) = Symbol.get_stype_args_val_def symbol in
(*    let sid2str s = string_of_int (Symbol.get_fast_key s) in *)
    let sid2str s = string_of_int (SymbolDB.get_basic_type_id !symbol_db_ref s) in 
    let sid_strs = List.map sid2str (val_t :: arg_ts) in
    let name = String.concat "_" (info.id :: sid_strs) in
    LI.create_symbol name stype

  let is_functor symbol term =
    Symbol.(is_pred symbol || is_fun symbol) && not (Term.is_const_term term)

  let is_sk_const_spl symbol term =
    Term.is_skolem_const term || Term.is_skolem_lit term ||
    (Symbol.(is_constant symbol && get_property symbol = Undef_Prop)) ||
    Symbol.(get_property symbol = Definition)

  let term_eligible test = function
    | Term.Fun (symbol,_,_) as term -> test symbol term
    | _ -> false

  let is_eligible (info:info) term =
    match info.opts.abstr_ref_sig_restrict with
    | Funpre -> term_eligible is_functor term
    | Skc -> term_eligible is_sk_const_spl term

  let rec replace_symbols details = function
    | Term.Fun (symb,args,_) ->
      let args_lst = Term.arg_to_list args in
      let (nsymb,nargs) =
        match BM.find symb details with
        | None ->
          let nargs = List.map (replace_symbols details) args_lst in
          (symb, nargs)
        | Some asymb ->
          let nargs = List.map (replace_symbols details) args_lst in
          (asymb, nargs)
      in
      LI.add_fun_term nsymb nargs
    | term -> term

  let refine_abstract_sig details asymbs =
    let rem accum asymb = BM.remove_by_value asymb accum in
    (* 'details' is only the abstract signature in this case *)
    List.fold_left rem details asymbs
end

module SG = Ar_abstract_signature.Make(SigGrup)

type details = SigGrup.t

let empty_details = BM.empty

let abstract opts clauses =
  Ar_dbg.dbg D_trace (lazy("\n\t* In Signature grouping abstraction"));
  let info = {opts = opts; id = "arSG" ^ string_of_int (C.get_id_ars ())} in
  let (details,adom) = SG.apply_abstr_sig info clauses in
  (* 'details' is only the abstract signature in this case *)
  Ar_dbg.dbg D_SGA (lazy(BM.to_string `Both details));
  Ar_dbg.dbg D_SGA (lazy(AD.to_string `V1 adom));
  (adom, details)

let refine adom details uc_ids clauses =
  Ar_dbg.dbg D_trace (lazy("\n\t* In Signature grouping refinement"));
  let asymbs = SG.get_abstr_symbs_in_uc adom uc_ids in
  let (details',adom') = SG.apply_refinement details asymbs clauses in
  Ar_dbg.dbg D_SGA (lazy(BM.to_string `Both details'));
  Ar_dbg.dbg D_SGA (lazy(AD.to_string `V1 adom'));
  (adom', details')
