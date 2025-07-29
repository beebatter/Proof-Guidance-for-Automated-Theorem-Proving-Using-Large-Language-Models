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
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module Make (M : Map.S) = struct
  type elt = M.key

  (* This builds a dag that represents the partial ordering among a number of 
     terms. This ensures worst case is n*(n-1)/2 comparisons. It also avoids 
     comparisons when it can: for example if a>b>c, and adds a d<c, then also
     adds d<b, d<a. *)
  type t = {
    mutable table: PartialOrd.t array;
    mutable nodes: int M.t;
    mutable size: int;
    (* offsets: int array; *)
  }

  let none = (Obj.magic (-1) : PartialOrd.t)

  let _ = dassert (fun () -> not @@ List.memq none PartialOrd.[EQ; GT; LT; INC])



  (* Order in which the relations are stored in the table:
       x y idx 
       0 1 0
       0 2 1
       1 2 2
       0 3 3
       1 3 4
       2 3 5
       0 4 6
       … … …
       x y y(y-1)/2 + x *)

  (* let node_add g *)

  let make terms = 
    let i = ref 0 in
    let nodes = 
      terms
      |> List.fold_left (fun acc x -> 
        (* dbg D_trace @@ lazy (sprintf "add %s" ()); *)
        (* let acc' = M.add x !i acc in
        if acc' != acc then incr i; *)
        if M.mem x acc then (
          acc
        ) else (
          let acc' = M.add x !i acc in
          incr i;
          acc'
        )
      ) M.empty
    in
    let size = !i in
    let table = Array.make ((size - 1) * size / 2) none in
    (* let j = ref 0 in
    let offsets = 
      Array.init size (fun idx -> 
        let x = !j in
        i := !i - 1;
        j := !j + !i;
        x
      )
    in *)
    {table; nodes; size; (* offsets *)}

  let copy g = 
    {g with table = Array.copy g.table}

  let size g = 
    M.cardinal g.nodes

  let is_total g = 
    g.table |> Array.for_all (fun x -> x != none && x != INC)

  (* Element -> Index of element *)
  let idx_of_elem g x = 
    g.nodes |> M.find x

  (* Element -> (Index of element) option *)
  let idx_of_elem' g x = 
    g.nodes |> M.find_opt x

  (* Element -> Index of element, but add if not existing *)
  let idx_of_elem'' g x = 
    match g.nodes |> M.find_opt x with
    | Some x_idx -> x_idx
    | None -> 
      let x_idx = g.size in
      (* (s-1)s/2 - (s-2)(s-1)/2 = (s² -s -s² +2s +s -2)/2 = (2s-2)/2 = s *)
      g.nodes <- M.add x x_idx g.nodes;
      g.size <- g.size + 1;
      g.table <- Array.init (Array.length g.table + g.size) (fun i -> 
        if i < Array.length g.table then Array.unsafe_get g.table i else none
      );
      (* let table' = Array.make (Array.length g.table + g.size) none; *)
      (* Array.blit g.table 0 table' 0  *)
      x_idx

  (* Index of two elements -> Index of their relation in table *)
  let idx_of g x_idx y_idx = 
    assert (x_idx < y_idx);  (* TODO change to dassert *)
    (* g.offsets.(x_idx) + (y_idx - (x_idx + 1)) *)
    let idx = y_idx*(y_idx-1)/2 + x_idx in
    dassert (fun () -> 0 <= idx && idx < Array.length g.table);
    idx



  let get' g x_idx y_idx = 
    if x_idx < y_idx then
      let r = g.table.(idx_of g x_idx y_idx) in
      (* if r == none then none else *)
      r
    else if x_idx > y_idx then
      let r = g.table.(idx_of g y_idx x_idx) in
      if r == none then none else
      PartialOrd.reverse r
    else
      EQ

  let get g x y = 
    let open Option.O in
    let* x_idx = idx_of_elem' g x in
    let* y_idx = idx_of_elem' g y in
    let r = get' g x_idx y_idx in
    if r == none then None else Some r

  let get_unsafe g x y = 
    let x_idx = idx_of_elem g x in
    let y_idx = idx_of_elem g y in
    let r = get' g x_idx y_idx in
    dassert (fun () -> r != none);  (* TODO change to dassert *)
    r

  let set' g x_idx y_idx v = 
    if x_idx < y_idx then
      g.table.(idx_of g x_idx y_idx) <- v
    else if x_idx > y_idx then
      g.table.(idx_of g y_idx x_idx) <- PartialOrd.reverse v
    else
      ()

  (* Given that x_idx ≟ y_idx is result, set all inferred orderings by transitivity *)
  let set_inferred' g x_idx y_idx result = 
    (* let set_eq g a_idx b_idx = 
      if let r = g.table.(idx_of g z_idx x_idx) in r == gt || r == EQ then
        g.table.(idx_of g z_idx y_idx) <- gt; *)
    let (=::) = List.X.(=::) in

    (* Helper function (GT and LT) *)
    let[@inline] loop g x_idx y_idx gt lt = 
      let above = ref [] in
      let below = ref [] in
      let sort = x_idx < y_idx in
      let idx_1, idx_2 = if sort then x_idx, y_idx else y_idx, x_idx in
      (* z<x z<y *)
      for z_idx = 0 to idx_1 - 1 do
        if let r = g.table.(idx_of g z_idx x_idx) in r == gt || r == EQ then
          (g.table.(idx_of g z_idx y_idx) <- gt; z_idx =:: above)
        else if let r = g.table.(idx_of g z_idx y_idx) in r == lt || r == EQ then
          (g.table.(idx_of g z_idx x_idx) <- lt; z_idx =:: below)
      done;
      if sort then
        (* x<z<y *)
        for z_idx = idx_1 + 1 to idx_2 - 1 do        
          if let r = g.table.(idx_of g x_idx z_idx) in r == lt || r == EQ then
            (g.table.(idx_of g z_idx y_idx) <- gt; z_idx =:: above)
          else if let r = g.table.(idx_of g z_idx y_idx) in r == lt || r == EQ then
            (g.table.(idx_of g x_idx z_idx) <- gt; z_idx =:: below)
        done
      else
        (* y<z<x *)
        for z_idx = idx_1 + 1 to idx_2 - 1 do        
          if let r = g.table.(idx_of g z_idx x_idx) in r == gt || r == EQ then
            (g.table.(idx_of g y_idx z_idx) <- lt; z_idx =:: above)
          else if let r = g.table.(idx_of g y_idx z_idx) in r == gt || r == EQ then
            (g.table.(idx_of g z_idx x_idx) <- lt; z_idx =:: below)
        done;
      (* x<z y<z *)
      for z_idx = idx_2 + 1 to g.size - 1 do
        if let r = g.table.(idx_of g x_idx z_idx) in r == lt || r == EQ then
          (g.table.(idx_of g y_idx z_idx) <- lt; z_idx =:: above)
        else if let r = g.table.(idx_of g y_idx z_idx) in r == gt || r == EQ then
          (g.table.(idx_of g x_idx z_idx) <- gt; z_idx =:: below)
      done;
      !above |> List.iter (fun x ->
        !below |> List.iter (fun y -> 
          set' g x y gt
        )
      )
    in

    (* Helper function (EQ) *)
    let[@inline] loop_eq g x_idx y_idx = 
      let above = ref [] in
      let below = ref [] in
      let sort = x_idx < y_idx in
      let idx_1, idx_2 = if sort then x_idx, y_idx else y_idx, x_idx in
      (* z<x z<y *)
      for z_idx = 0 to idx_1 - 1 do
        let r = g.table.(idx_of g z_idx x_idx) in if r != none && r != INC then  (* TODO wouldn't INC be propagated too? *)
          (g.table.(idx_of g z_idx y_idx) <- r; (z_idx,r) =:: above)
        else let r = g.table.(idx_of g z_idx y_idx) in if r != none && r != INC then 
          (g.table.(idx_of g z_idx x_idx) <- r; (z_idx,r) =:: below)
      done;
      if sort then
        (* x<z<y *)
        for z_idx = idx_1 + 1 to idx_2 - 1 do        
          let r = g.table.(idx_of g x_idx z_idx) in if r != none && r != INC then
            (g.table.(idx_of g z_idx y_idx) <- PartialOrd.reverse r; (z_idx,PartialOrd.reverse r) =:: above)
          else let r = g.table.(idx_of g z_idx y_idx) in if r != none && r != INC then
            (g.table.(idx_of g x_idx z_idx) <- PartialOrd.reverse r; (z_idx,r) =:: below)
        done
      else
        (* y<z<x *)
        for z_idx = idx_1 + 1 to idx_2 - 1 do        
          let r = g.table.(idx_of g z_idx x_idx) in if r != none && r != INC then
            (g.table.(idx_of g y_idx z_idx) <- PartialOrd.reverse r; (z_idx,r) =:: above)
          else let r = g.table.(idx_of g y_idx z_idx) in if r != none && r != INC then
            (g.table.(idx_of g z_idx x_idx) <- PartialOrd.reverse r; (z_idx,PartialOrd.reverse r) =:: below)
        done;
      (* x<z y<z *)
      for z_idx = idx_2 + 1 to g.size - 1 do
        let r = g.table.(idx_of g x_idx z_idx) in if r != none && r != INC then
          (g.table.(idx_of g y_idx z_idx) <- r; (z_idx,PartialOrd.reverse r) =:: above)
        else let r = g.table.(idx_of g y_idx z_idx) in if r != none && r != INC then
          (g.table.(idx_of g x_idx z_idx) <- r; (z_idx,PartialOrd.reverse r) =:: below)
      done;
      if List.X.is_nonempty !below then  (* Short-circuit *)
      !above |> List.iter (fun (x,rx) -> 
        match rx with
        | EQ -> 
          !below |> List.iter (fun (y,ry) -> 
            set' g y x ry
          )
        | GT -> 
          !below |> List.iter (fun (y,ry) -> 
            match ry with
            | GT -> ()
            | EQ | LT -> set' g x y GT
            | _ -> assert false 
          )
        | LT -> 
          !below |> List.iter (fun (y,ry) -> 
            match ry with
            | GT | EQ -> set' g x y LT
            | LT -> ()
            | _ -> assert false 
          )
        | _ -> assert false
      )
    in

    match result with
    (* x > y: then ∀z. y ≥ z, also x > z, 
               and ∀z. z ≥ x, also z > y *)
    | GT -> loop g x_idx y_idx GT LT
    (* x < y: then ∀z. y ≤ z, also x < z, 
               and ∀z. z ≤ x, also z < y *)
    | LT -> loop g x_idx y_idx LT GT
    (* x = y: then ∀z. z = x, also z = y
               and ∀z. z = y, also z = x 
               and ∀z. z > x, also z > y
               and ∀z. z > y, also z > x
               and ∀z. z < x, also z < y
               and ∀z. z < y, also z < x *)
    | EQ -> loop_eq g x_idx y_idx
    (* And if INC then nothing can be inferred *)
    | INC -> ()

  let set_unsafe g x y v = 
    let x_idx = idx_of_elem g x in
    let y_idx = idx_of_elem g y in
    set' g x_idx y_idx v

  type res = Eq | Ne | Invalid
  let set g x y v = 
    let x_idx = idx_of_elem'' g x in
    let y_idx = idx_of_elem'' g y in
    let old_v = get' g x_idx y_idx in
    if old_v == none then (
      set' g x_idx y_idx v;
      set_inferred' g x_idx y_idx v;
      Ne
    ) else if old_v == v then (
      Eq
    ) else (
      Invalid
    )

  let mem g x = 
    (* let rec loop g x_idx z_idx max_idx = 
      if z_idx >= max_idx then
        false
      else
        g.table.(idx_of x)
    in
    let x_idx = idx_of_elem g x in
    loop g x_idx (0) (x_idx-1) || loop g x_idx (x_idx+1) (g.size-1) *)
    g.nodes |> M.mem x

  let iter_elts g f = 
    g.nodes |> M.iter (fun x _ -> f x)

  let iter_relations g f = 
    let rec loop g f l = 
      let rec loop' g f x l = 
        match l with
        | y::tl -> 
          let (x_elt, x_idx) = x in
          let (y_elt, y_idx) = y in
          let res = get' g x_idx y_idx in
          if res != none then f x_elt y_elt res;
          loop' g f x tl
        | [] -> ()
      in
      match l with
      | x::tl -> loop' g f x tl; loop g f tl
      | [] -> ()
    in
    loop g f (M.bindings g.nodes)



  let update' g x_idx y_idx f = 
    if x_idx < y_idx then
      let idx_cmp = idx_of g x_idx y_idx in
      let x = g.table.(idx_cmp) in
      let y = f x in
      g.table.(idx_cmp) <- y
    else if x_idx > y_idx then
      let idx_cmp = idx_of g y_idx x_idx in
      let x = PartialOrd.reverse @@ g.table.(idx_cmp) in
      let y = f x in
      g.table.(idx_cmp) <- PartialOrd.reverse y
    else
      ()

  (* Add a list of nodes which are linearly sorted *)
  (* Invariant: graph is empty (current) *)
  let add_sorted g l =
    dassert (fun () -> 
      g.table |> Array.for_all (fun x -> x == none)
    );
    let rec loop g l = 
      match l with
      | [_] -> ()
      | hd::(_::_ as tl) -> 
        tl |> List.iter (fun x -> 
          set' g hd x (if hd == x then EQ else LT)  (* Here was always LT, which is incorrect and results in slightly weaker normalisation in AC *)
        );
        loop g tl
      | [] -> (* assert false; *) ()
    in
    let l_idx = List.map (fun x -> idx_of_elem g x) l in
    loop g l_idx

  let add_with g cmp x =
    (* From list of nodes, compare with every other node *)
    let x_idx = idx_of_elem g x in
    g.nodes |> M.iter (fun y y_idx ->
      update' g x_idx y_idx (fun res -> 
        if res != none then res else (
          let result = cmp x y in
          set_inferred' g x_idx y_idx result;
          result
        )
      )
    )

  let add_with_many g cmp l = 
    List.iter (add_with g cmp) l



  let wrap g f x y = 
    let x_idx = idx_of_elem g x in
    let y_idx = idx_of_elem g y in
    if x_idx < y_idx then
      let idx_cmp = idx_of g x_idx y_idx in
      let r = g.table.(idx_cmp) in
      if r != none then r else (
        let r' = f x y in
        g.table.(idx_cmp) <- r';
        set_inferred' g x_idx y_idx r';
        r'
      )
    else if x_idx > y_idx then
      let idx_cmp = idx_of g y_idx x_idx in
      let r = g.table.(idx_cmp) in
      if r != none then PartialOrd.reverse r else (
        let r' = f y x in
        g.table.(idx_cmp) <- r';
        set_inferred' g y_idx x_idx r';
        PartialOrd.reverse r'
      )
    else
      EQ



  (* For debugging only *)
  (* let to_string_dbg to_string_elt g = 
    let buf = Buffer.create 1024 in
    let bindings = M.bindings g.nodes |> List.sort (fun (_,x) (_,y) -> compare x y) in
    bindings |> List.iter (fun (x, x_idx) -> 
      Buffer.add_string buf (sprintf "%d: %s  " x_idx (to_string_elt x))
    );
    bindings |> List.iter (fun (_, x_idx) -> 
      bindings |> List.iter (fun (_, y_idx) -> 
        let r = get' g x_idx y_idx in
        Buffer.add_string buf (sprintf "\n%d %d : %s %s" x_idx y_idx 
          (if r == none then "— " else PartialOrd.to_string r)
          (if x_idx < y_idx then sprintf "(%d)" (idx_of g x_idx y_idx) else "") 
        )
      )
    );
    Buffer.contents buf *)

  let to_string_dbg to_string_elt g = 
    let buf = Buffer.create 1024 in
    let bindings = 
      M.bindings g.nodes 
      |> List.sort (fun (_,x) (_,y) -> compare x y)
      |> List.map (fun (elt, idx) -> (to_string_elt elt, idx))
    in
    let max_label = bindings |> List.map fst |> List.map String.length |> List.X.max Int.compare |> Int.max 2 in

    Buffer.add_string buf (sprintf "\n%*s" max_label "");
    bindings |> List.iter (fun (y, _) -> 
      Buffer.add_string buf (sprintf " %*s" max_label y)
    );
    bindings |> List.iter (fun (x, x_idx) -> 
      Buffer.add_string buf (sprintf "\n%*s" max_label x);
      bindings |> List.iter (fun (y, y_idx) -> 
        let r = get' g x_idx y_idx in
        Buffer.add_string buf (sprintf " %*s" max_label (if r == none then "— " else PartialOrd.to_string r))
      )
    );
    Buffer.contents buf
  let to_string_dbg to_string_elt g = 
    if g.size = 0 then "" else to_string_dbg to_string_elt g
end
