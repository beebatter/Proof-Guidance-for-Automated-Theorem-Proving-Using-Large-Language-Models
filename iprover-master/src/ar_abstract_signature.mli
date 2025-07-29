(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain

(** This module implements the abstraction of the concrete domain's signature.
    Currently, the over-approximating abstractions that abstracts the signature
    are signature grouping, argument filtering and generalisation. *)

module type AbstrSig = sig
  type t
  (** The type of the abstract signature. *)

  type csymb
  (** The type of the concrete symbols *)

  type asymb
  (** The type of the abstract symbols. *)

  type info
  (** The type of the passed info to the abstraction. *)

  val abstraction_type: Options.abstr_ref_type
  (** Sets the type of the atomic abstraction. Currently, it can be one of the
      following: argument filtering, signature grouping and generalisation. *)

  val abstract_symbol: info -> csymb -> asymb
  (** [abstract_symbol i cs as] abstracts a concrete symbol [cs] yielding
      the abstract symbols [as]. The parameter [i] can contain useful
      information such as options to restrict the signature abstraction. *)

  val is_eligible: info -> Term.term -> bool
  (** [is_eligible i trm] provides a function to allow the abstraction of
      certain symbols in a term. *)

  val replace_symbols: t -> Term.term -> Term.term
  (** [replace_symbols t trm] defines the flow to replace symbols in a term
      [trm] based on the abstract signature [t]. *)

  val refine_abstract_sig: t -> asymb list -> t
  (** [refine_abstract_sig t asl] refines the abstract signature [t] based on
      a list of abstract symbols [asl]. *)

  val cstsymb: info -> Symbol.symbol -> csymb
  (** This functions casts a symbol into a proper concrete symbol in the context
      of the current atomic abstraction. *)

  val init: info -> t
  (** Initialise the abstract signature. *)

  val add: csymb -> asymb -> t -> t
  (** [add cs as t] adds a concrete symbol [cs] and its corresponding abstract
      symbol [as] to the abstract signature [t]. *)

  val mem: csymb -> t -> bool
  (** [mem cs t] checks whether a concrete symbol [cs] has an abstract symbol
      in the abstract signature [t], i.e., check whether [cs] has been
      abstracted. *)
end
(** Input signature of the functor !{Ar_abstract_signature.Make}. *)

module type S = sig
  type t
  (** The type of the abstraction based on abstract signature. *)

  type details
  (** The type of the details of the abstraction. *)

  type info
  (** The type of the passed info to the abstraction. *)

  type asymb
  (** The type of the abstract symbols. *)

  val apply_abstr_sig: info -> LI.BCSet.t -> t
  (** [apply_abstr_sig i cls] applies the abstraction to the set of clauses
      [cls]. *)

  val apply_refinement: details -> asymb list -> LI.BCSet.t -> t
  (** [apply_refinement d asl cls] refines the details of the abstraction [d]
      based on the list of abstract symbols and then applies the refined
      abstraction to the set of clauses [cls]. *)

  val get_abstr_symbs_in_uc: AD.t -> LI.TSet.t -> Symbol.t list
  (** [get_abstr_symbs_in_uc ad trms] returns a list with the symbols that are
      in the given set of terms. *)
end
(** Output signature of the functor !{Ar_abstract_signature.Make}. *)

module Make(AS: AbstrSig) : S with type info = AS.info
                               and type asymb = AS.asymb
                               and type details = AS.t
                               and type t = AS.t * AD.t
(** Functor building an abstraction based on abstract signature. *)
