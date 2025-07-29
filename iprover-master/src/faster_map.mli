(** Batteries version: "imperative" *)
val map_mut : ('a -> 'b) -> 'a list -> 'b list

(** Unrolled version. *)
val plain_unrolled_map_5 : ('a -> 'b) -> 'a list -> 'b list

(** "Community" version: unrolling + tail recursive. REVERSES SIDE-EFFECT-ORDER (right-to-left)! *)
val faster_map : ('a -> 'b) -> 'a list -> 'b list