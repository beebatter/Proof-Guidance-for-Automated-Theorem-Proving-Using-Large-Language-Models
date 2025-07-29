open Lib
open Logic_interface



(** Returns axioms to order all numerical symbols which currently exist in 
    symboldb, for ints, rats and reals. *)
val order_axioms : unit -> clause list * clause list * clause list
