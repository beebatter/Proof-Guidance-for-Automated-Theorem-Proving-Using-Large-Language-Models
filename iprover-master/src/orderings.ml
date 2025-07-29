open Lib

type term = Term.term
type atom = Term.atom
type lit  = Term.lit

type var = Var.var
type symbol = Symbol.symbol

let uid_ref = ref 0
let get_next_uid () = 
  let r = !uid_ref in
  incr uid_ref;
  r

(********** DEPRECATED IMPLEMENTATION
module VarOrder = struct
  (* Represent a partial order viar a transitive reduction. *)
  (* [graph] represents one connected component, [t] represents the relation. *)
  type graph = 
    | End
    | Gt of var list * graph list  (* All variables in the list are =, and > than any variable below them in the graph *)
  type t = 
    graph list

  let empty = 
    []

  let none = (Obj.magic (-1) : PartialOrd.t)

  (** Helper function *)
  let[@inline] fold_none f orders = 
    List.fold_left (fun acc order -> if acc != none then acc else f order) none orders
    
  (** Find the value of comparing x and y *)
  let rec query (order: graph) x y : PartialOrd.t = 
    let rec query' order y = 
      match order with
      | End -> INC
      | Gt (vars, children) -> 
        let y_mem = vars |> List.X.mem ~eq:Var.equal y in
        match y_mem with
        | true -> GT
        | false -> children |> fold_none (fun ord -> query' ord y)
    in
    match order with
    | End -> none
    | Gt (vars, children) -> 
      (* Passing terms is more efficient (==) than passing vars (destructure, then ==, then ==) *)
      let x_mem = vars |> List.X.mem ~eq:Var.equal x in
      let y_mem = vars |> List.X.mem ~eq:Var.equal y in
      match x_mem, y_mem with
      | true, true -> EQ
      | true, false -> children |> fold_none (fun ord -> query' ord y)
      | false, true -> children |> fold_none (fun ord -> query' ord x)
      | false, false -> children |> fold_none (fun ord -> query ord x y)

  let query (order: t) x y : PartialOrd.t = 
    let result = order |> fold_none (fun ord -> query ord x y) in
    if result == none then INC else result
    (* order |> List.fold_left (fun acc _ -> acc) INC *)

  let find (order: graph) x = 
    match order with
    | End -> None
    | Gt (vars, children) -> 
      if vars |> List.X.mem ~eq:Var.equal x then
        Some order
      else

  let find order x = 
    (* Option.bind *)
    order |> fold

  (** Attempt to add x>y to order. Returns [Some order'] or [None] if it is contradictory *)
  let add_gt (order: t) x y : t option = 
    (* Locate x and y in the tree *)

  (** Attempt to add x=y to order. Returns [Some order'] or [None] if it is contradictory *)
  let rec add_eq (order: graph) x y = 
    match order with
    | End -> Some order
    | Gt (vars, children) -> 
      let x_mem = vars |> List.X.mem ~eq:Var.equal x in
      let y_mem = vars |> List.X.mem ~eq:Var.equal y in
      match x_mem, y_mem with
      | true, true -> 
        

  let add_eq (order: t) x y : t option = 
    unimplemented()

  let to_string_dbg order = 
    List.X.to_string ~first:"" ~last:"" ~sep:"\n" (
      List.X.to_string ~first:"" ~last:"" ~sep:"" (
        Pair.to_string Var.to_string (function EQ -> " = " | LT -> " < ")
      ) 
    ) 
    order
end
**********)

module VarOrder = struct
  (* TODO: change this interface to use mutation rather than a functional interface *)
  module Graph = Ordering_graph.Make(Var.VMap)

  type t = Graph.t

  let empty = 
    Graph.make []

  let add_gt g x y = 
    let g' = Graph.copy g in
    let okay = Graph.set g' x y GT in
    match okay with Graph.Eq -> Some g | Graph.Ne -> Some g' | Graph.Invalid -> None

  let add_eq g x y = 
    let g' = Graph.copy g in
    let okay = Graph.set g' x y EQ in
    match okay with Graph.Eq -> Some g | Graph.Ne -> Some g' | Graph.Invalid -> None

  let query g x y = 
    Option.O.(Graph.get g x y |? INC)

  (* let mem g x = 
    Graph.mem g x *)

  let iter_vars g f = 
    Graph.iter_elts g f

  let iter_relations g f = 
    Graph.iter_relations g f

  let is_total g n = 
    Graph.size g = n && Graph.is_total g

  let to_string_dbg = 
    Graph.to_string_dbg Var.to_string
end

type t = {
  uid: int;
  terms: term -> term -> PartialOrd.t;
  atoms: atom -> atom -> PartialOrd.t;
  lits: lit -> lit -> PartialOrd.t;
  oriented: lit -> PartialOrd.t;

  terms_var:    VarOrder.t -> term -> term -> PartialOrd.t;
  terms_var_gt: VarOrder.t -> term -> term -> VarOrder.t option;
}

type terms = term -> term -> PartialOrd.t
type atoms = atom -> atom -> PartialOrd.t
type lits = lit -> lit -> PartialOrd.t
type oriented = lit -> PartialOrd.t

let empty_terms_var    : VarOrder.t -> term -> term -> PartialOrd.t      = fun _ _ _ -> undefined()
let empty_terms_var_gt : VarOrder.t -> term -> term -> VarOrder.t option = fun _ _ _ -> undefined()


let make ~terms ?(terms_var=empty_terms_var) ?(terms_var_gt=empty_terms_var_gt) ~atoms ~lits ~oriented () = {
  uid = get_next_uid ();
  terms; atoms; lits; oriented; terms_var; terms_var_gt;
}
