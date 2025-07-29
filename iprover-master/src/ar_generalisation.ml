(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Lib 

module LI = Logic_interface
module AD = Ar_abstract_domain
module C = Ar_common



module ArgFun = struct
  let elg_symb _ _ = true

  let filter_args_by_bs _ arg_lst = arg_lst

  let create_astype symb _ = Symbol.get_type symb
end

module AA = Ar_abstract_arguments.Make(ArgFun)

type venv =
  | Empty
  | Env of Var.fresh_vars_env

type info_abstr =
  { opts : Options.options;
    id : string;
    symb2bs : bool list LI.SMap.t;
    var_env: venv;
  }

type detls_abstr =
  { asig : AA.t;
    symb2bs : bool list LI.SMap.t;
    id : string;
    var_env: venv;
  }

module Gen = struct
  type t = detls_abstr
  type csymb = AA.csymb
  type asymb = AA.asymb
  type info = info_abstr

  let abstraction_type = Options.Abstr_ref_gen

  let init (info:info) =
    { asig = AA.empty;
      symb2bs = info.symb2bs;
      id = info.id;
      var_env = info.var_env;
    }

  let add csymb asymb dtls = {dtls with asig = AA.add csymb asymb dtls.asig}

  let mem csymb dtls = AA.mem csymb dtls.asig

  let cstsymb (info:info) symb = AA.cstsymb info.symb2bs symb

  let abstract_symbol (info:info) csymb =
    AA.abstract_symbol info.symb2bs info.id csymb

  let is_eligible (info:info) term = AA.is_eligible info.symb2bs info.opts term

  let replace_symbols (detls:t) term =
    let change_term_to_var t = function
      | true -> t
      | false ->
        match detls.var_env with
        | Env ve ->
          Term.get_term_type t |> Var.get_next_fresh_var ve |> LI.add_var_term
        | Empty -> failwith ("ar_generalisation.ml:replace_symbols: " ^
                             "No var env; this should not happen")
    in
    let create_abstr_args_f = List.map2 change_term_to_var in
    AA.replace_symbols create_abstr_args_f detls.asig term

  let refine_abstract_sig detls asymbs =
    let asig = AA.refine_abstract_sig detls.id detls.asig asymbs in
    {detls with asig = asig}
end

module G = Ar_abstract_signature.Make(Gen)

type details = G.details

let empty_details =
  { asig = AA.empty;
    symb2bs = LI.SMap.empty;
    id = "";
    var_env = Empty;
  }

let create_vars_env clauses =
  let clause_vars clause vset =
    LI.VSet.union (Clause.get_var_set clause) vset
  in
  let vars = LI.BCSet.fold clause_vars clauses LI.VSet.empty in
  Env (Var.init_fresh_vars_env_away (LI.VSet.elements vars))

let abstract opts clauses =
  Ar_dbg.dbg D_trace (lazy("\n\t* In generalisation abstraction"));
  let symb2bs = LI.(BCSet.fold (AA.fill_symb2bsig opts) clauses SMap.empty) in
  let info =
    { opts = opts;
      id = "arG" ^ string_of_int (C.get_id_ars ());
      symb2bs = symb2bs;
      var_env = create_vars_env clauses;
    }
  in
  let (details,adom) = G.apply_abstr_sig info clauses in
  Ar_dbg.dbg D_GA (lazy(AA.to_string `Both details.asig));
  Ar_dbg.dbg D_GA (lazy(AD.to_string `V1 adom));
  (adom, details)

let refine adom details uc_ids clauses =
  Ar_dbg.dbg D_trace (lazy("\n\t* In generalisation refinement"));
  let asymbs_uc = G.get_abstr_symbs_in_uc adom uc_ids in
  let nasymbs = List.map (fun s -> (s, [])) asymbs_uc in
  let (details',adom') = G.apply_refinement details nasymbs clauses in
  Ar_dbg.dbg D_GA (lazy(AA.to_string `Both details'.asig));
  Ar_dbg.dbg D_GA (lazy(AD.to_string `V1 adom'));
  (adom', details')
