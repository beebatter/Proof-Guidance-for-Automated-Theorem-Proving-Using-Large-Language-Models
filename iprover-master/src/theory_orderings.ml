open Lib
open Logic_interface

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



(* Aux function *)
let permutation_num n l = 
  dbg D_trace @@ lazy (sprintf "permutation %d %s" n (List.X.to_string Symbol.to_string l));
  let m = List.length l in
  let l' = Array.make m (None) in
  let rec loop acc i m n = 
    if i = m+1 then
      acc
    else
      let n' = n / i in
      let d = n mod i in
      let acc' = d :: acc in
      loop acc' (i+1) m n'
  in
  let[@warning "-8"] first::digits = loop [] 1 (m+1) n in  (* +1 for the position of "rest" *)
  List.combine l digits |> List.iter (fun (elt, i) -> 
    let rec loop i j = 
      if l'.(j) |> Option.is_some then
        loop i (j+1)
      else if i = 0 then 
        j
      else
        loop (i-1) (j+1)
    in
    l'.(loop i 0) <- Some elt
  );
  let l' = l' |> Array.to_seq |> Seq.map Option.get |> List.of_seq in
  let rec loop head tail i = 
    if i = 0 then List.rev head, tail else let hd,tl = List.X.hd_tl tail in loop (hd::head) tl (i-1)
  in
  loop [] l' first
  |> tap (fun (a,b) -> dbg D_trace @@ lazy (sprintf "permutation = %s %s" (List.X.to_string Symbol.to_string a) (List.X.to_string Symbol.to_string b)))

(* First implementation: try to find these theories *)
let get_ordering ?permut ?symb_ordering_default record = 
  dbg D_trace @@ lazy "get_ordering";
  let[@inline] query x = Theory_db.Record.query record x in
  let[@inline] get subst_sym i =
   (*
      let j = Symbol.bit_offset_placeholder_syms + i in 
      subst_sym |> SMap.find_first (fun x -> Symbol.get_fast_key x >= j) |> snd
    *)
    try
      subst_sym |> SMap.find_first (fun x ->
        try
          (SymbolDB.get_placeholder_id !symbol_db_ref x) >= i
        with Not_found ->        
          false
                                   ) |> snd
    with Not_found -> failwith (sprintf "get_ordering: Not_found:get_placeholder_id >= %d" i)
  in
  let basic_symb_ordering = Option.O.(symb_ordering_default |? Symbol.cmp_invfreq) in

  match query "latticeordgroup" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found latticeordgroup: %s" (Theory_subst.to_string_sym subst_sym));
    (* inv < 1 < glb < lub < _ < × *)
    let f = get subst_sym in
    let symb_ordering = symbol_cmp_custom [f 7; f 8; f 0; f 3] basic_symb_ordering [f 6] in
    [LPO.make ~symb_ordering]
  | [] -> 

  match query "field" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found field: %s" (Theory_subst.to_string_sym subst_sym));
    (* 0 < 1 < + < _ < × < - < 1/x *)
    let f = get subst_sym in
    let symb_ordering = symbol_cmp_custom [f 2; f 5; f 0] basic_symb_ordering [f 3; f 1; f 4] in
    [LPO.make ~symb_ordering]
  | [] -> 
  match query "nonassocring" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found nonassocring: %s" (Theory_subst.to_string_sym subst_sym));
    (* 0 < 1 < + < _ < × < - *)
    (* - < × < _ < 0 < 1 < + *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 2; f 5; f 0] basic_symb_ordering [f 3; f 1] in
    let symb_ordering2 = symbol_cmp_custom [f 1; f 3] basic_symb_ordering [f 2; f 5; f 0] in
    [LPO.make ~symb_ordering:symb_ordering1 ; LPO.make ~symb_ordering:symb_ordering2]
  | [] -> 
  match query "nonassocrng" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found nonassocrng: %s" (Theory_subst.to_string_sym subst_sym));
    (* 0 < + < _ < × < - *)
    (* - < × < _ < 0 < + *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 2; f 0] basic_symb_ordering [f 3; f 1] in
    let symb_ordering2 = symbol_cmp_custom [f 1; f 3] basic_symb_ordering [f 2; f 0] in
    [LPO.make ~symb_ordering:symb_ordering1 ; LPO.make ~symb_ordering:symb_ordering2]
  | [] -> 
  match query "semiring" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found semiring: %s" (Theory_subst.to_string_sym subst_sym));
    (* + < × < 0 < 1 < _ *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 0; f 3; f 2; f 5] basic_symb_ordering [] in
    let symb_ordering2 = symbol_cmp_custom [f 3] basic_symb_ordering [f 2; f 5; f 0] in
    [LPO.make ~symb_ordering:symb_ordering1 ; LPO.make ~symb_ordering:symb_ordering2]
  | [] -> 
  match query "semirng" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found semirng: %s" (Theory_subst.to_string_sym subst_sym));
    (* + < × < 0 < _ *)
    (* × < _ < 0 < + *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 0; f 3; f 2] basic_symb_ordering [] in
    let symb_ordering2 = symbol_cmp_custom [f 3] basic_symb_ordering [f 2; f 0] in
    [LPO.make ~symb_ordering:symb_ordering1 ; LPO.make ~symb_ordering:symb_ordering2]
  | [] -> 

  match query "boolalg" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found boolalg: %s" (Theory_subst.to_string_sym subst_sym));
    (* 0,1 < ∨,∧ < ¬ < _ *)
    let f = get subst_sym in
    let a = List.sort basic_symb_ordering [f 2; f 5] in
    let b = List.sort basic_symb_ordering [f 0; f 3] in
    let c = List.sort basic_symb_ordering [f 1] in
    let symb_ordering = symbol_cmp_custom (a@b@c) basic_symb_ordering [] in
    [LPO.make ~symb_ordering]
  | [] -> 
  match query "boundlattice" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found boundlattice: %s" (Theory_subst.to_string_sym subst_sym));
    (* LPO ∧ < ∨ < 1 < 0 < _ *)
    (* KBO 0 < _ < 1 < ∧ < ∨ *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 3; f 0; f 5; f 2] basic_symb_ordering [] in
    let symb_ordering2 = symbol_cmp_custom [f 2] basic_symb_ordering [f 5; f 3; f 0] in
    let weight = Term.get_num_of_symb in
    [LPO.make ~symb_ordering:symb_ordering1 ; KBO.make ~with_var:true ~inc_criteria:false ~weight ~symb_ordering:symb_ordering2 ()]
  | [] ->
  match query "ucompllattice" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found ucompllattice: %s" (Theory_subst.to_string_sym subst_sym));
    (* KBO ∧ < ∨ < _ < ¬ *)
    (* LPO _ < ∨ < ∧ < ¬ *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 3; f 0] basic_symb_ordering [f 1] in
    let wsym = (let inv = f 1 in fun s -> if s == inv then 0 else 1) in
    let wvar = 1 in
    let symb_ordering2 = symbol_cmp_custom [] basic_symb_ordering [f 0; f 3; f 1] in
    [KBO.make_sw ~with_var:true ~wsym ~wvar ~symb_ordering:symb_ordering1 (); LPO.make ~symb_ordering:symb_ordering2]
  | [] -> 
  match query "distlattice" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found distlattice: %s" (Theory_subst.to_string_sym subst_sym));
    (* LPO with ∨<∧ and ∧<∨ *)
    let f = get subst_sym in
    let symb_ordering1 = symbol_cmp_custom [f 0; f 3] basic_symb_ordering [] in
    let symb_ordering2 = symbol_cmp_custom [] basic_symb_ordering [f 3; f 0] in
    (* let symb_ordering = basic_symb_ordering in *)
    (* [LPO.make ~symb_ordering] *)
    [LPO.make ~symb_ordering:symb_ordering1 ; LPO.make ~symb_ordering:symb_ordering2]
  | [] -> 
  (* match query "lattice" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found lattice: %s" (Theory_subst.to_string_sym subst_sym));
    (* ∨ < ∧ < rest *)
    let f = get subst_sym in
    let symb_ordering = symbol_cmp_custom [f 0; f 3] basic_symb_ordering [] in
    Some (LPO.make ~symb_ordering)
  | [] -> *)

  match query "group" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found group: %s" (Theory_subst.to_string_sym subst_sym));
    (* 0 < ⋅ < _ < ^-1, with weight of ^-1 being 0 *)
    let f = get subst_sym in
    let symb_ordering = symbol_cmp_custom [f 2; f 0] basic_symb_ordering [f 1] in
    let wsym = (let inv = f 1 in fun s -> if s == inv then 0 else 1) in
    let wvar = 1 in
    [KBO.make_sw ~with_var:true ~wsym ~wvar ~symb_ordering ()]
  | [] -> 

  (* match query "unit" with
  | (subst_sym, _)::_ -> 
    dbg D_trace @@ lazy (sprintf "found unit: %s" (Theory_subst.to_string_sym subst_sym));
    (* 0 < rest *)
    let f = get subst_sym in
    let symb_ordering = symbol_cmp_custom [f 2] basic_symb_ordering [] in
    let weight = Term.get_num_of_symb in
    Some (KBO.make ~with_var:true ~weight ~symb_ordering)
  | [] ->  *)

    dbg D_trace @@ lazy "none found";
    []
