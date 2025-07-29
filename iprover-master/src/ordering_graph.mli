open Lib
(* open Logic_interface *)



(** This module provides a data structure that stores a partial ordering 
    between its elements. It stores orders between elements and also infers 
    as much as possible, e.g.
      if a=b and b=c, infers a=c
      if a>b and b>c and c>d, infers a>c, a>d, b>d

    Use cases include:
      Caching expensive orderings among a fixed or small set of elements, as 
      well as inferring implied 
      Representing an ordering among variables.
      Detecting if such an ordering is contradictory. *)

(** This module is parametrised on a map of elements *)
module Make (M : Map.S) : sig
  type elt = M.key

  (** Caches some partial ordering among a finite and fixed set of terms. *)
  type t

  (** Creates an OrderingGraph.t among the given terms (initially all 
      relations unknown). *)
  val make : elt list -> t

  (** Returns a copy that can be independently updated. *)
  val copy : t -> t

  (** Checks if an element is in the table (but still might have all 
      relations set to none/INC). *)
  (* val mem : t -> elt -> bool *)

  (** Apply function to all elements in the table. *)
  val iter_elts : t -> (elt -> unit) -> unit

  (** Apply function to all relations in the table (incl INC but excl none). *)
  val iter_relations : t -> (elt -> elt -> PartialOrd.t -> unit) -> unit



  (** Get the value of the ordering cached in the graph, or None if it isn't 
      cached. *)
  val get : t -> elt -> elt -> PartialOrd.t option

  (** Set the value of the ordering cached in the graph, but does nothing and 
      returns [false] if this binding would make the ordering inconsistent. *)
  type res = Eq | Ne | Invalid
  val set : t -> elt -> elt -> PartialOrd.t -> res

  (** As [get], but assumes that the result exists (elements are both in the 
      graph) and that result isn't None. *)
  val get_unsafe : t -> elt -> elt -> PartialOrd.t

  (** As [set], but: assumes that the elements are both in the graph, and 
      does not check consistency, so may put t into an invalid state. *)
  val set_unsafe : t -> elt -> elt -> PartialOrd.t -> unit

  (** If [order] is an ordering, [wrap g order] is the same ordering but 
      where results are cached into g. *)
  val wrap :
    t ->
    (elt -> elt -> PartialOrd.t) ->
    (elt -> elt -> PartialOrd.t)



  (** Number of distinct nodes, even unrelated, in this graph. *)
  val size : t -> int

  (** Checks if all nodes in the graph are related. *)
  val is_total : t -> bool



  (* val update' : t -> int -> int -> (PartialOrd.t -> PartialOrd.t) -> unit *)

  (** Add an element to the graph, which is comapred with every other possible 
      element using a given ordering. *)
  val add_with : t -> (elt -> elt -> PartialOrd.t) -> elt -> unit

  (** As [List.iter (add graph order) list], but more efficient. *)
  val add_with_many : t -> (elt -> elt -> PartialOrd.t) -> elt list -> unit

  (** Add a list of linearly ordered terms, non-strictly ascending (≤). 
      Invariant: the graph is empty (asserted in debug mode). *)
  val add_sorted : t -> elt list -> unit



  (** Debug printout *)
  val to_string_dbg : (elt -> string) -> t -> string
end
