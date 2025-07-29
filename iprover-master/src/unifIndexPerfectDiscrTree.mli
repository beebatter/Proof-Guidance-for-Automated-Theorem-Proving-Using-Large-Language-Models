open Logic_interface



(** Perfect discrimination tree that indexes ['a] by [term]s. In the leaves, 
    it stores the list of ['a] associated with that key, as well as the key 
    itself. *)
type 'a t

val create : unit -> 'a t

val clear : 'a t -> unit

val mem : 'a t -> lit -> bool

(** Indexed by [lit], adds ['a] to the list of elements associated with that lit. *)
val add_to_lit : 'a t -> lit -> 'a -> unit

(** Indexed by [lit], removes from the list of elements associated with that lit,
    those elements which [f elem] returns true. *)
val filter_from_lit : 'a t -> lit -> ('a -> bool) -> unit

(** Returns list *)
val get_gen : 
  'a t -> lit -> 
  ((lit * 'a list) * Subst.subst) list

val get_inst : 
  'a t -> lit -> 
  ((lit * 'a list) * Subst.subst) list

(** [iter_gen index term f] is equivalent to [get_gen index term |> List.iter f],
    (almost, apart from currying) *)
val iter_gen : 
  'a t -> lit -> 
  (lit -> 'a list -> Subst.subst -> unit) -> unit

val iter_inst : 
  'a t -> lit -> 
  (lit -> 'a list -> Subst.subst -> unit) -> unit
