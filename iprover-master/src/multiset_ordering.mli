open Lib

(* open Logic_interface *)

type term = Term.term

(** Lift a partial ordering to multisets (represented as lists) *)
(** Currently only works on terms *)
val ord : (term -> term -> PartialOrd.t) -> term list -> term list -> PartialOrd.t
