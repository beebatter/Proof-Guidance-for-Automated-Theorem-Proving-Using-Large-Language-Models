open Lib
open Logic_interface



(** Given a record of detected theories, suggests some (or no) orderings. *)
val get_ordering : ?permut:int -> ?symb_ordering_default:(symbol -> symbol -> int) -> Theory_db.Record.t -> Orderings.t list
