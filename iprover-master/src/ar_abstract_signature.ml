(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain

module type AbstrSig = sig
  type t
  type csymb
  type asymb
  type info
  val abstraction_type: Options.abstr_ref_type
  val abstract_symbol: info -> csymb -> asymb
  val is_eligible: info -> Term.term -> bool
  val replace_symbols: t -> Term.term -> Term.term
  val refine_abstract_sig: t -> asymb list -> t
  val cstsymb: info -> Symbol.symbol -> csymb
  val init: info -> t
  val add: csymb -> asymb -> t -> t
  val mem: csymb -> t -> bool
end

module type S = sig
  type t
  type details
  type info
  type asymb
  val apply_abstr_sig: info -> LI.BCSet.t -> t
  val apply_refinement: details -> asymb list -> LI.BCSet.t -> t
  val get_abstr_symbs_in_uc: AD.t -> LI.TSet.t -> LI.SSet.elt list
end

module Make(AS: AbstrSig) = struct
  type details = AS.t
  type t = details * AD.t
  type info = AS.info
  type asymb = AS.asymb

  let incr_abstr_sig info detls = function
    | Term.Fun(symb,_,_) as trm when AS.is_eligible info trm &&
                                     not (AS.(mem (cstsymb info symb) detls)) &&
                                     not (LI.is_special_symb symb) ->
      let abstr_symb = AS.(abstract_symbol info (cstsymb info symb)) in
      AS.(add (cstsymb info symb) abstr_symb detls)
    | _ -> detls

  let abstr_sig_atom info details lit =
    let atom = Term.get_atom lit in
    Term.fold_subterms (incr_abstr_sig info) details atom

  let create_partial_abstr_sig info details clause =
    Clause.fold (abstr_sig_atom info) details clause

  let replace_symbs_in_asig details lits =
    let abstr_lit lit =
      LI.add_term_db (Term.apply_to_atom (AS.replace_symbols details) lit)
    in
    List.map abstr_lit lits

  let fill_abstract_domain details clause abstr_dom =
    let lits = Clause.get_literals clause in
    let abstr_lits = replace_symbs_in_asig details lits in
    AD.ov_add AS.abstraction_type clause lits abstr_lits abstr_dom

  let abstract_clause info clause (details,abstr_dom) =
    let details' = create_partial_abstr_sig info details clause in
    let abstr_dom' = fill_abstract_domain details' clause abstr_dom in
    (details',abstr_dom')

  let apply_abstr_sig info clauses =
    let accum = (AS.init info, AD.(empty Over)) in
    LI.BCSet.fold (abstract_clause info) clauses accum

  let apply_refinement details asymbs clauses =
    let details' = AS.refine_abstract_sig details asymbs in
    let accum = AD.(empty Over) in
    let abstr_dom =
      LI.BCSet.fold (fill_abstract_domain details') clauses accum
    in
    (details',abstr_dom)

  let get_abstr_symbs_in_uc adom uc_ids =
    let join id acc =
      AD.get_all_symbols_by_abstr_id id adom
      |> LI.SSet.union acc
    in
    LI.TSet.fold join uc_ids LI.SSet.empty |> LI.SSet.elements
end
