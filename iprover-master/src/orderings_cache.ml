open Lib

type term = Term.term



type ord_id = int

type is_oriented = LeftGreater | RightGreater | NotOriented

let is_oriented' ord ord_id lit = 
  dassert (fun () -> Term.Eq.is_eq lit);
  let atom = Term.get_atom lit in
  match atom with
  | Term.Fun (_, args, info) ->
    (* let open PartialOrd in  *)
    begin try 
      Term.get_orderings_cache_info ord_id info 
    with Not_found ->
      let (_,l,r) = Term.get_3_args args in
      let result = ord l r in
      Term.set_orderings_cache_info ord_id result info;
      result
    end
  | Term.Var _ -> failwith "Term.Eq.is_oriented: Not an equality literal"

let is_oriented ord ord_id lit = 
  let open PartialOrd in
  match is_oriented' ord ord_id lit with
  | GT -> 
    LeftGreater
  | LT -> 
    RightGreater
  | INC | EQ -> 
    NotOriented

let is_any_oriented ord ord_id lit =
  let open PartialOrd in
  match is_oriented' ord ord_id lit with
  | GT
  | LT ->
    true
  | INC | EQ -> 
    false
