open Logic_interface

(** Superposition state *)
type state

(** Creates an empty state possibly with some starting clauses in the passive *)
(* val create_state : ?input_clauses:(clause list) -> unit -> state *)
val create_state : unit -> state
(** Adds clauses to passive *)
val add_clauses_passive : state -> clause list -> unit 



(** Thrown on saturation *)
exception Empty_passive

(** Executes the main superposition loop until unsatisfiability or saturation *)
val main_loop : state -> unit



(** Equality transformation *)
(** Converts an arbitrary literal to an equality literal, via:
    P(...)  =>  P(...) = symb_top
 *)
val lit_to_eq : lit -> lit

(** Converts a list of clauses to equality *)
val clauselist_to_eq : clause list -> clause list
