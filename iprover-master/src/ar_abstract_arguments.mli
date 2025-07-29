(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface

(** This module implements an specialisation of the signature abstraction, which
    focuses on abstracting some/all of the parameters of the predicates and
    functions. Currently, the abstractions argument filtering and generalisation
    use the provided functor of this module. *)

module type ArgFun = sig
  val elg_symb: Options.options -> Symbol.symbol -> bool
  (** [elg_symb o s] checks whether a symbol [s] is eligible to be abstracted.
      The passed options [o] can give more information if it is needed. *)

  val filter_args_by_bs: bool list -> 'a list -> 'a list
  (** [filter_args_by_bs bl l] defines which elements of [l] have to be filtered
      depending on [bl], which is a list of booleans. *)

  val create_astype: Symbol.symbol -> bool list -> Symbol.stype
  (** [create_astype s bl] creates a proper stype for an abstract symbol [s] and
      its arguments based on [bl], which is a list of booleans. *)
end
(** Input signature of the functor !{Ar_abstract_arguments.Make}. *)

module type S = sig
  type t
  (** The type of the abstraction based on abstract arguments. *)

  type csymb
  (** The type of the concrete symbols. *)

  type asymb
  (** The type of the abstract symbols. *)

  val fill_symb2bsig: Options.options -> Clause.clause -> bool list LI.SMap.t ->
    bool list LI.SMap.t
  (** [fill_symb2bsig o c m] helps on filling a map from symbols to boolean
      signature, which is a list of booleans. The boolean signature provides
      which parameters will be abstracted. *)

  val empty: t
  (** The empty structure of abstract arguments. *)

  val add: csymb -> asymb -> t -> t
  (** [add cs as t] adds a bind between a concrete symbol [cs] and its
      abstraction [as] on the abstraction [t]. *)

  val mem: csymb -> t -> bool
  (** [mem cs t] checks whether a concrete symbol [cs] is member of the
      abstraction [t]. *)

  val to_string: [`Both | `Bwd | `Fwd] -> t -> string
  (** Gives a string representation of the abstraction [t]. *)

  val cstsymb: bool list LI.SMap.t -> Symbol.symbol -> csymb
  (** Casts a symbol to make a proper concrete symbol in this context. *)

  val abstract_symbol: bool list LI.SMap.t -> string -> csymb -> asymb
  (** [abstract_symbol bl s cs] abstracts the concrete symbol [cs] based on the
      boolean list [bl] to abstract its parameters. The string [s] is used as
      a prefix to differentiate the created abstract symbols. *)

  val is_eligible: 'a LI.SMap.t -> Options.options -> Term.term -> bool
  (** Provides a function to determine whether a term is eligible to be
      abstracted. *)

  val replace_symbols: (Term.term list -> bool list -> Term.term list) ->
    t -> Term.term -> Term.term
  (** [replace_symbols f t trm] performs the replacement of concrete symbols in
      the term [trm] to abstract symbols based on the abstraction [t] and
      the function [f]. *)

  val refine_abstract_sig: string -> t -> asymb list -> t
  (** [refine_abstract_sig s t al] refines the abstraction [t] based on the list
      of abstract symbols [al]. The string [s] is used as a prefix to
      differentiate the refined abstract symbols whether they remain abstract. *)
end
(** Output signature of the functor !{Ar_abstract_arguments.Make}. *)

module Make(AF: ArgFun) : S with type csymb = Symbol.symbol * bool list
                             and type asymb = Symbol.symbol * bool list
(** Functor building an abstraction based on abstract arguments. *)
