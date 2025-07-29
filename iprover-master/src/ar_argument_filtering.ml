(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain
module C = Ar_common

(* filters elements of list corresponding to b (true/false) positions in bsig *)
let bs_filter_list b bsig lst =
  assert(List.((length bsig = length lst)));
  let f accu current_b elt = if b = current_b then elt :: accu else accu in
  List.rev (List.fold_left2 f [] bsig lst)

module ArgFun = struct
  let elg_symb (opts:Options.options) symb =
    if opts.abstr_ref_af_restrict_to_split_sk then
      Symbol.((get_property symb) = Definition || is_skolem symb)
    else
      true

  let filter_args_by_bs bsig arg_lst = bs_filter_list true bsig arg_lst

  let create_astype symb bsig =
    let (arg_ts,val_t) = Symbol.get_stype_args_val_def symb in
    let aarg_typs = bs_filter_list true bsig arg_ts in
    LI.create_stype aarg_typs val_t
end

module AA = Ar_abstract_arguments.Make(ArgFun)

type info_abstr =
  { opts : Options.options;
    id : string;
    symb2bs : bool list LI.SMap.t;
  }

type detls_abstr =
  { asig : AA.t;
    symb2bs : bool list LI.SMap.t;
    id : string;
  }

module ArgFil = struct
  type t = detls_abstr
  type csymb = AA.csymb
  type asymb = AA.asymb
  type info = info_abstr

  let abstraction_type = Options.Abstr_ref_arg_filter

  let init (info:info) = {asig = AA.empty; symb2bs = info.symb2bs; id = info.id}

  let add csymb asymb dtls = {dtls with asig = AA.add csymb asymb dtls.asig}

  let mem csymb dtls = AA.mem csymb dtls.asig

  let cstsymb (info:info) symb = AA.cstsymb info.symb2bs symb

  let abstract_symbol (info:info) csymb =
    AA.abstract_symbol info.symb2bs info.id csymb

  let is_eligible (info:info) term = AA.is_eligible info.symb2bs info.opts term

  let replace_symbols (detls:t) term =
    let create_abstr_args_f args abs = bs_filter_list true abs args in
    AA.replace_symbols create_abstr_args_f detls.asig term

  let refine_abstract_sig detls asymbs =
    let asig = AA.refine_abstract_sig detls.id detls.asig asymbs in
    {detls with asig = asig}
end

module AF = Ar_abstract_signature.Make(ArgFil)

type details = AF.details

let empty_details = {asig = AA.empty; symb2bs = LI.SMap.empty; id = ""}

let abstract opts clauses =
  Ar_dbg.dbg D_trace (lazy("\n\t* In Argument filtering abstraction"));
  let symb2bs = LI.BCSet.fold (AA.fill_symb2bsig opts) clauses LI.SMap.empty in
  let info =
    { opts = opts;
      id = "arAF" ^ string_of_int (C.get_id_ars ());
      symb2bs = symb2bs;
    }
  in
  let (details,adom) = AF.apply_abstr_sig info clauses in
  Ar_dbg.dbg D_AFA (lazy(AA.to_string `Fwd details.asig));
  Ar_dbg.dbg D_AFA (lazy(AD.to_string `V1 adom));
  (adom, details)

let refine adom details uc_ids clauses =
  Ar_dbg.dbg D_trace (lazy("\n\t* In Argument filtering refinement"));
  let asymbs_uc = AF.get_abstr_symbs_in_uc adom uc_ids in
  let nasymbs = List.map (fun s -> (s, [])) asymbs_uc in
  let (details',adom') = AF.apply_refinement details nasymbs clauses in
  Ar_dbg.dbg D_AFA (lazy(AA.to_string `Fwd details'.asig));
  Ar_dbg.dbg D_AFA (lazy(AD.to_string `V1 adom'));
  (adom', details')
