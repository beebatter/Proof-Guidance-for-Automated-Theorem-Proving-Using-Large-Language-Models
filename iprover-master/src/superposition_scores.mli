open Lib
open Logic_interface



type elt = {
  mutable simplified: int;
  mutable generated: int;
}

type t

(** Creates an empty score table *)
val empty : ?size:int -> unit -> t


(** Add a zero entry *)
val add_entry : t -> clause -> unit

(** Increment simplified/generated, implicitly adding if not already in table *)
val incr_simplified : t -> clause -> unit
val incr_generated : t -> clause -> unit
val incr_simplified_by : t -> clause -> int -> unit
val incr_generated_by : t -> clause -> int -> unit

(** Get the scores for a clause *)
val get : t -> clause -> elt option



(** Iter and fold through the bindings in the table *)
val iter : (clause -> elt -> unit) -> t -> unit

val fold : (clause -> elt -> 'a -> 'a) -> t -> 'a -> 'a



(** For debug purposes *)
val score_to_string : elt -> string

(** Creates clause cmp function based on scores: larger scores are larger in 
    the ordering *)
val score_type_to_cl_cmp : Options.sup_score_type -> t -> (clause -> clause -> int)

(** Creates a score_fun based on the options *)
val score_type_to_fun : Options.sup_score_type -> (elt -> int)



(** Selects top clauses from score map according to [score_fun elt]. Examples 
   of score_fun: 
     elt.sim
     elt.sim - elt.gen
     (elt.sim + 1) / (elt.gen + 1)
   Only clauses with score greater than [1 - frac_top_score] of the highest score 
   will be selected. At most [max_num_cl] highest-scoring clauses will be returned. 
   If [worst] is true, then this will be reversed: only the worst [max_num_cl] 
   clauses with score less than [frac_top_score] of the highest score will be returned. *)
val select_top_clauses : 
  ?worst:bool -> 
  frac_top_score:float -> 
  max_num_cl:int -> 
  score_fun:(elt -> int) -> 
  t -> 
  clause list

