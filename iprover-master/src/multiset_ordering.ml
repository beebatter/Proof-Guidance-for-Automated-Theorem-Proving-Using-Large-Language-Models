open Lib
(* open Logic_interface *)



(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
  D_trace;
]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)

type term = Term.term

let remove_equal ord x y =
  let rec remove1 z l =
    match l with
    | y::ys ->
      if ord y z == PartialOrd.EQ then
        ys
      else
        y :: remove1 z ys
    | [] -> raise Not_found
  in

  (* We don't pre-sort, that probably will not be worth it in small multisets *)
  (* let rec f x y to_scan =
    match to_scan with
    | hd::tl ->
      try
        let y' = remove hd y in
        f 
      with Not_found ->

  in
  f x y x *)
  (* List.fold_left (fun (x',y') elem ->

  ) ([],[]) x *)

  (* This approach requires only 1 pass through x *)
  let rec f x y x_to_scan =
    (* Loop through the elements in x *)
    match x_to_scan with
    | hd::tl ->
      (* If hd (element of x) also in y, remove it *)
      begin try
        let y' = remove1 hd y in
        f (x) (y') tl
      (* If not in y, keep same y, and also cons hd into x *)
      with Not_found ->
        f (hd::x) (y) tl 
      end
    | [] -> 
      (x,y)
  in
  f [] y x



(* There is only hope for this being fast if we cache the results inside  *)
(* Also, due to the fact we have to use this functorial interface, this is currently restricted to terms. Not ideal but will do for now. *)

(* Generic cache *)
module Cache(M : Map.OrderedType) = struct
  (* Map for pairs of M.t *)
  module PairMap = Map.Make(struct
    type t = M.t * M.t
    let compare (x:t) (y:t) =
      pair_compare_lex M.compare M.compare x y
  end)

  type t = PartialOrd.t PairMap.t ref

  let empty () = ref PairMap.empty

  let query (cache:t) ord x y =
    (* If [(x,y) |-> result] in cache, return it, else compute and store it. *)
    (* This cache is not size-limited, use with care. *)
    try
      PairMap.find (x,y) !cache
    with Not_found ->
      (* dbg D_trace @@ lazy "Cache miss"; *)
      let result = ord x y in
      cache := PairMap.add (x,y) result !cache;
      cache := PairMap.add (y,x) (PartialOrd.reverse result) !cache;
      result
end

module CacheTerms = Cache(Term.TKey)

module CacheInt = Cache(struct
  type t = int
  let compare = Int.compare
end)

(* Cache interface that doesn't actually cache anything *)
module NoCache = struct
  type t = unit
  let empty _ = ()
  let query cache ord x y =
    ord x y
end

module Cache' = CacheTerms

(* module Cache' = NoCache *)

let multiset_ordering ord x y =
  let open PartialOrd in

  let cache = Cache'.empty () in

  let rec inner ord x y = 
    dbg D_trace @@ lazy (sprintf "x : %s" (List.X.to_string Term.to_string x));
    dbg D_trace @@ lazy (sprintf "y : %s" (List.X.to_string Term.to_string y));
    (* For each element in x, try to remove as many smaller elements in y as possible *)
    match x,y with
    (* y cleared *)
    | _, [] -> true
    (* x cleared, but not y *)
    | [], _::_ -> false
    (* x and y still have values *)
    | hd::tl, y -> 
      let y' = List.filter (fun elem ->
        not (Cache'.query cache ord elem hd == LT)
      ) y
      in
      inner ord tl y'
  in

  let x,y = remove_equal (Cache'.query cache ord) x y in
  dbg D_trace @@ lazy (sprintf "After remove_equal: %s" (List.X.to_string Term.to_string x));
  dbg D_trace @@ lazy (sprintf "vs                : %s" (List.X.to_string Term.to_string y));
  match x,y with
  | []   , []   -> EQ
  | _::_ , []   -> GT
  | []   , _::_ -> LT
  | _::_ , _::_ -> 
    if inner ord x y then
      GT
    else if inner ord y x then
      LT
    else
      INC



let ord = multiset_ordering
