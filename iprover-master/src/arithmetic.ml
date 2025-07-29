open Lib
open Logic_interface


(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr =
  | D_trace 

let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
 D_trace
]

let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)

(*--------------------------*)

(* converts float string n.m  to Some(Q.t) or None if string is not of this form  *)

let real_str_to_rat real_str = 
  dbg D_trace @@ lazy (sprintf "real_str_to_rat: %s" real_str);
(*
  let rexp =  Str.regexp "\\." in
  let rat_str_list = Str.split rexp real_str in 
  match rat_str_list with 
  | [before_dot;after_dot] ->       
    let base = Z.of_int 10 in
    let maintissa = Z.of_string (String.concat "" rat_str_list) in
    let non_neg_exponent = String.length after_dot in
    let pos_power = Z.pow base non_neg_exponent in 
    Some (Q.make maintissa pos_power)
  | _ | exception Invalid_argument _ -> None
*)
  try
    Some(Q.of_string real_str) (* also recognises E*)
  with
  | Invalid_argument _ -> None

      

  
let order_axioms () = 
  let symbs_int  = ref [] in
  (* let symbs_rat  = ref [] in *)
  let symbs_real = ref [] in
  !Logic_interface.symbol_db_ref |> SymbolDB.iter (fun x ->
    dbg D_trace @@ lazy (sprintf "order_axioms: s:%s" (Symbol.to_string x));
    match Symbol.get_property x with
    | Symbol.Num_int n -> 
      dbg D_trace @@ lazy (sprintf "order_axioms: Num_int: %s n: %s" (Symbol.to_string x) (Z.to_string n));
      symbs_int := (x, n) :: !symbs_int

    (* rationals are not supported n/0 is possible and orderign on it is not well defined *)
    (* | Symbol.Num_rat n -> 
      symbs_rat := (x, n) :: !symbs_rat *)                          
    | Symbol.Num_real n ->      
(*      begin match real_str_to_rat (Symbol.get_name x) with  *)

(*      | Some n ->  *)
        dbg D_trace @@ lazy (sprintf "order_axioms: Num_real: %s n: %s" (Symbol.to_string x) (Q.to_string n));
        symbs_real := (x, n) :: !symbs_real
(*      | None -> () 
      end *)
    | _ -> ()
  );
  
  (* assert that comparisions in the list are strict (they should be since different symbols should be converted to diff numbers) *)

  let asset_not_euqal res = 
    if res = 0 then 
      failwith "arithmetic.ml: different number symbols converted to the same number" 
    else res 
  in
  let symbs_int_sorted = List.sort (fun (_,x) (_,y) ->  asset_not_euqal (Z.compare x y)) !symbs_int in
  (* let symbs_rat = List.sort (fun (_,x) (_,y) -> Q.compare x y) !symbs_rat in *)
  let symbs_real_sorted = List.sort (fun (_,x) (_,y) ->  asset_not_euqal (Q.compare x y)) !symbs_real in

  dbg D_trace @@ lazy (sprintf "order_axioms: symbs_int_sorted: ln: %i" (List.length symbs_int_sorted));

  let mk_clause term = 
    let source = Clause.(tstp_source_theory_axiom TSTP_arith) in
    create_clause ~normalise_eqs:true source [term]
  in   
  let rec make_axioms acc f l = 
    match l with
    | [] | [_] -> acc
    | a::(b::_ as tl) -> 
      let at = add_fun_term (fst a) [] in
      let bt = add_fun_term (fst b) [] in      
      let new_cl = mk_clause (add_fun_term f [at;bt]) in
      dbg D_trace @@ lazy (sprintf "order_axioms: %s" (Clause.to_string new_cl));        
      let new_acc = new_cl::acc in
      make_axioms new_acc f tl
  in
  let axioms_int  = make_axioms [] Symbol.symb_less_int symbs_int_sorted in
  (* let axioms_rat  = make_axioms Symbol.symb_less_rat symbs_rat in *)
  let axioms_real = make_axioms [] Symbol.symb_less_real symbs_real_sorted in
  axioms_int, (* axioms_rat *) [], axioms_real

