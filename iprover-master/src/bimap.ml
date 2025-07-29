(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Lib

module type OrdPrinType = sig
  include Map.OrderedType
  val to_string: t -> string
end

module type S = sig
  type t
  type key
  type value
  val empty: t
  val add: key -> value -> t -> t
  val mem: key -> t -> bool
  val find: key -> t -> value option
  val find_by_value: value -> t -> key list option
  val remove_by_key: key -> t -> t
  val remove_by_value: value -> t -> t
  val to_string: [`Fwd | `Bwd | `Both] -> t -> string
end

module Make(D: OrdPrinType)(R: OrdPrinType) = struct
  module Dom2ran = Map.Make(D)
  module Ran2dom = Map.Make(R)
  module Keys = Set.Make(D)

  type key = D.t

  type value = R.t

  type dmap = {fwd:value Dom2ran.t; bwd:Keys.t Ran2dom.t}

  type t =
    | Empty
    | Bmap of dmap

  let empty = Empty

  let add_to_domain x y = function
    | Empty ->
      let fwd' = Dom2ran.singleton x y in
      Bmap {fwd = fwd'; bwd = Ran2dom.empty}
    | Bmap {fwd;bwd} ->
      let fwd' = Dom2ran.add x y fwd in
      let bwd' =
        match Dom2ran.find_opt x fwd with
        | None -> bwd
        | Some y' ->
          match Ran2dom.find_opt y' bwd with
          | None -> bwd
          | Some ks ->
            let ks' = Keys.remove x ks in
            if Keys.is_empty ks' then
              Ran2dom.remove y' bwd
            else
              Ran2dom.add y' ks' bwd
      in
      Bmap {fwd = fwd'; bwd = bwd'}

  let add_to_range y x = function
    | Empty -> Empty
    | Bmap {fwd;bwd} ->
      match Ran2dom.find_opt y bwd with
      | None ->
        let bwd' = Ran2dom.add y (Keys.singleton x) bwd in
        Bmap {fwd = fwd; bwd = bwd'}
      | Some ks ->
        let ks' = Keys.add x ks in
        let bwd' = Ran2dom.add y ks' bwd in
        Bmap{fwd = fwd; bwd = bwd'}

  let add x y bm =
    bm |> add_to_domain x y |> add_to_range y x

  let mem x = function
    | Empty -> false
    | Bmap bm -> Dom2ran.mem x bm.fwd

  let find k = function
    | Bmap bm -> Dom2ran.find_opt k bm.fwd
    | Empty -> None

  let find_by_value v = function
    | Bmap bm -> begin
      match Ran2dom.find_opt v bm.bwd with
      | Some ks -> Some (Keys.elements ks)
      | None -> None
    end
    | Empty -> None

  let remove_by_key k = function
    | Empty -> Empty
    | Bmap bm as m ->
      match find k m with
      | None -> m
      | Some v ->
        match Ran2dom.find_opt v bm.bwd with
        | None -> m
        | Some ks ->
          let fwd' = Dom2ran.remove k bm.fwd in
          let ks' = Keys.remove k ks in
          let bwd' =
            if Keys.is_empty ks' then
              Ran2dom.remove v bm.bwd
            else
              Ran2dom.add v ks' bm.bwd
          in
          Bmap {fwd = fwd'; bwd = bwd'}

  let remove_by_value v = function
    | Empty -> Empty
    | Bmap bm as m ->
      match find_by_value v m with
      | None -> m
      | Some ks ->
        let bwd' = Ran2dom.remove v bm.bwd in
        let rem a b = Dom2ran.remove b a in
        let fwd' = List.fold_left rem bm.fwd ks in
        Bmap {fwd = fwd'; bwd = bwd'}

  let dom2str m =
    let dom2str' k v a =
      let bs = D.to_string k ^ " -> " ^ R.to_string v in
      a ^ "\n" ^ bs
    in
    Dom2ran.fold dom2str' m ""

  let ran2str m =
    let ran2str' k v a =
      let ks = Keys.fold (fun e a -> a ^ (D.to_string e) ^ "; ") v "" in
      let bs = R.to_string k ^ " <- " ^ ks in
      a ^ "\n" ^ bs
    in
    Ran2dom.fold ran2str' m ""

  let to_string dir = function
    | Empty -> "{ }"
    | Bmap bm ->
      match dir with
      | `Fwd -> "\n" ^ dom2str bm.fwd
      | `Bwd -> "\n" ^ ran2str bm.bwd
      | `Both -> "\n" ^ dom2str bm.fwd ^ "\n" ^ ran2str bm.bwd
end
