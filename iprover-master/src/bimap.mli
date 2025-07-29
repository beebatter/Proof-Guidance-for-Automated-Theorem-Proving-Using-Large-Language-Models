(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module implements a bidirectional map (bimap), based on two maps. One
    of them maps elements forward and the other one backward. *)

module type OrdPrinType = sig
  include Map.OrderedType
  (** Same Ordered type as the map structure. *)

  val to_string: t -> string
  (** Convert a proper representation of the Ordered types. *)
end
(** Input signature of the functor !{Bimap.Make}. *)

module type S = sig
  type t
  (** The type of the bimap. *)

  type key
  (** The type of the bimap keys. *)

  type value
  (** The type of the bimap values. *)

  val empty: t
  (** The empty bimap. *)

  val add: key -> value -> t -> t
  (** [add k v bm] adds a key [k] and its value [v] to the bimap [bm]. *)

  val mem: key -> t -> bool
  (** [mem k bm] test whether a key [k] is a member of the bimap [bm]. *)

  val find: key -> t -> value option
  (** [find k bm] searches for a key [k] in the bimap [bm] and returns an option
      value, [Some v] if found or [None] otherwise. *)

  val find_by_value: value -> t -> key list option
  (** [find_by_value v bm] searches for a given value [v] in the bimap [bm]. If
      the value is found it returns an option list with the key(s) that have
      associated that value ([Some k list]). Otherwise it returns [None]. *)

  val remove_by_key: key -> t -> t
  (** [remove_by_key k bm] removes the given key [k] and its associated value
      from the bimap [bm]. *)

  val remove_by_value: value -> t -> t
  (** [remove_by_value v bm] finds the keys associated to the value [v] and
      removes those keys from the bimap [bm]. *)

  val to_string: [`Fwd | `Bwd | `Both] -> t -> string
  (** Returns a string representation of the bimap. [`Fwd] provides a string
      representation of the form key -> value, [`Bwd] gives a string that
      represents the relations ship value -> key and [`Both] returns a string
      representing both ways key <-> value *)
end
(** Output signature of the functor !{Bimap.Make}. *)

module Make(D: OrdPrinType)(R: OrdPrinType) : S with type key = D.t
                                                 and type value = R.t
(** Functor building an implementation of the bimap structure given two totally
    ordered types. *)
