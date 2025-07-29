open Logic_interface

(* Low-level trie with jump lists *)
(* module Trie : sig
  type key = Sym of symbol | Var of var

  val key_to_string_dbg : key -> string

  (* module KMap : (module type Map.S with key := key end) *)

  type 'a t

  val print_dbg : out_channel -> 'a t -> unit

  val create : unit -> 'a t

  val is_empty : 'a t -> bool

  (* val add : 'a t -> term -> 'a -> 'a t *)
  val add : 'a t -> term -> ('a t -> 'a t) -> 'a t

  val next : 'a t -> key -> 'a t option

  val skip : 'a t -> 'a t TMap.t

  val get_from_leaf : 'a t -> 'a
end *)



(** Perfect discrimination trie that indexes ['a] by [term]s. Each leaf 
    contains stores a list of ['a] associated with that term. Imperative. *)
type 'a t

(** Empty tree *)
val create : unit -> 'a t

val clear : 'a t -> unit

(** [update index term k] navigates to the leaf indexed by [term] in [index], 
    which contains the [term] and an ['a list ref] of values indexed by that 
    term. It runs the function [k term list_ref]. All operations can be 
    implemented in terms of this. *)
(* val update : 'a t -> term -> ('a ref option -> 'a ref option) -> 'a t *)
(* val update : 'a t -> term -> ('a option -> 'a option) -> 'a t *)
val update : 'a t -> term -> (term -> 'a list ref -> unit) -> unit



(** Add ['a] indexed by [term]. *)
val add : 'a t -> term -> 'a -> unit

(** From the ['a list] associated with [term], delete those for which the 
    function yields true. *)
val filter : 'a t -> term -> ('a -> bool) -> unit



(** Returns list *)
val instantiations : 'a t -> term -> (term * Subst.subst * 'a list) list

val generalisations : 'a t -> term -> (term * Subst.subst * 'a list) list

(** [instantiations_iter index term f] is equivalent to [instantiations index term |> List.iter f] *)
val instantiations_iter : 'a t -> term -> (term -> Subst.subst -> 'a list -> unit) -> unit

val generalisations_iter : 'a t -> term -> (term -> Subst.subst -> 'a list -> unit) -> unit
