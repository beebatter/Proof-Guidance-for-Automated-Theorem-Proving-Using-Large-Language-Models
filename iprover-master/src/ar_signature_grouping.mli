(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain

(** This module implements the signature grouping abstraction, which is an
    over-approximating abstraction. *)

type details
(** The type of the details of the signature grouping abstraction. *)

val abstract: Options.options -> LI.BCSet.t -> AD.t * details
(** [abstract o cs] abstracts a set of clauses [cs] using the signature grouping
    abstraction. *)

val refine: AD.t -> details -> LI.TSet.t -> LI.BCSet.t -> AD.t * details
(** [refine ad d ts cs] refines the passed abstract domain [ad] based on the
    switch predicates [ts] used as identifiers of the abstract clauses. The
    details [d] gives the context to the refinement process. Additionally, the
    refinement of the abstraction can be applied to the set of clauses [cs].
    This is useful in process as the refinement until SAT. *)

val empty_details: details
(** The empty details of the signature grouping abstraction. *)
