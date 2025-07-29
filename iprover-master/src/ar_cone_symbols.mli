(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface
module AD = Ar_abstract_domain

(** This module implements the cone symbols abstraction, which is an
    under-approximating abstraction. *)

type details
(** The type of the details of the cone symbols abstraction. *)

val abstract: Options.options -> LI.BCSet.t -> AD.t * details
(** [abstract o cs] abstracts a set of clauses [cs] using the cone symbols
    abstraction. *)

val refine: AD.t -> details -> AD.t * details
(** [refine ad d] refines the abstract domain [ad] based on the details [d]. *)

val empty_details: details
(** The empty details of the cone symbols abstraction. *)

val should_try_sat: unit -> bool
(** It is an auxiliary function to give more time when trying to determine the
    satisfiability of a set of clauses. *)
