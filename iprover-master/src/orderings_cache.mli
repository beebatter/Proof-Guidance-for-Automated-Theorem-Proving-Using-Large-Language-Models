open Lib



type is_oriented = LeftGreater | RightGreater | NotOriented

type ord_id = int

val is_oriented' : 
  (Term.term -> Term.term -> PartialOrd.t) -> ord_id ->
  Term.term -> PartialOrd.t

val is_oriented : 
  (Term.term -> Term.term -> PartialOrd.t) -> ord_id ->
  Term.term -> is_oriented

val is_any_oriented : 
  (Term.term -> Term.term -> PartialOrd.t) -> ord_id ->
  Term.term -> bool
