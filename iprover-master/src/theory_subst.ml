open Lib
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_trace2

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_trace2 -> "trace"

let dbg_groups = [
  D_trace;
  (* D_trace2; *)
]
    


(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



type t = subst_var * subst_sym
and subst_var = var VMap.t
and subst_sym = symbol SMap.t  (* TODO enforce injectivity! *)

let to_string_var x = 
  VMap.bindings x |> List.X.to_string' ~sep:" " (fun (a,b) -> 
    sprintf "%s/%s" (Var.to_string a) (Var.to_string b)
  )

let to_string_sym x = 
  SMap.bindings x |> List.X.to_string' ~sep:" " (fun (a,b) -> 
    sprintf "%s/%s" (Symbol.to_string a) (Symbol.to_string b)
  )

let output_var o x = 
  VMap.bindings x |> List.X.output' ~sep:" " (fun o (a,b) -> 
    fprintf o "%s/%s" (Var.to_string a) (Var.to_string b)
  ) o

let output_sym o x = 
  SMap.bindings x |> List.X.output' ~sep:" " (fun o (a,b) -> 
    fprintf o "%s/%s" (Symbol.to_string a) (Symbol.to_string b)
  ) o



exception Renaming_sym_fail

(** Checks if a and b are equal modulo variable and symbol renamings, under 
    some extension of var [subst_var] and symbol [subst_sym]. *)
let rec renaming subst_var subst_sym subst_var_rev subst_sym_rev a b = 
  if Term.get_num_of_symb a <> Term.get_num_of_symb b
  || Term.get_num_of_var a <> Term.get_num_of_var b
  then
    raise_notrace Renaming_sym_fail
  else 
    match a, b with
    | Term.Fun (sym_a, args_a, _), Term.Fun (sym_b, args_b, _) -> 
      let[@inline] recurse subst_var subst_sym subst_var_rev subst_sym_rev = 
        List.fold_left2 (fun (subst_var, subst_sym, subst_var_rev, subst_sym_rev) a b -> 
          renaming subst_var subst_sym subst_var_rev subst_sym_rev a b
        ) (subst_var, subst_sym, subst_var_rev, subst_sym_rev) (Term.arg_to_list args_a) (Term.arg_to_list args_b)
      in
      (* Special symbols (=, +, not, etc) cannot be renamed *)
      if Symbol.is_special_symb sym_a || Symbol.is_special_symb sym_b then
        if sym_a == sym_b then 
          recurse subst_var subst_sym subst_var_rev subst_sym_rev
        else 
          raise_notrace Renaming_sym_fail
      else 
        begin match subst_sym |> SMap.find_opt sym_a, subst_sym_rev |> SMap.find_opt sym_b with
        | None, None -> 
          let subst_sym' = subst_sym |> SMap.add sym_a sym_b in
          let subst_sym_rev' = subst_sym_rev |> SMap.add sym_b sym_a in
          recurse subst_var subst_sym' subst_var_rev subst_sym_rev'
        | Some sym_b', Some sym_a' when sym_b == sym_b' && sym_a == sym_a' -> 
          recurse subst_var subst_sym subst_var_rev subst_sym_rev
        | _ -> 
          raise_notrace Renaming_sym_fail
        end
    | Term.Var (var_a, _), Term.Var (var_b, _) -> 
      begin match subst_var |> VMap.find_opt var_a, subst_var_rev |> VMap.find_opt var_b with
      | None, None -> 
        let subst_var' = subst_var |> VMap.add var_a var_b in
        let subst_var_rev' = subst_var_rev |> VMap.add var_b var_a in
        subst_var', subst_sym, subst_var_rev', subst_sym_rev
      | Some var_b', Some var_a' when var_b == var_b' && var_a == var_a' -> 
        subst_var, subst_sym, subst_var_rev, subst_sym_rev
      | _ -> 
        raise_notrace Renaming_sym_fail
      end
    | Term.Var _, Term.Fun _ | Term.Fun _, Term.Var _ -> 
      raise_notrace Renaming_sym_fail

let renaming (subst_var, subst_sym) a b = 
  dbg D_trace2 @@ lazy (sprintf "renaming: %s %s" (Term.to_string a) (Term.to_string b));
  try 
    let subst_var_rev = subst_var |> VMap.to_seq |> Seq.map (fun (x,y) -> (y,x)) |> VMap.of_seq in
    let subst_sym_rev = subst_sym |> SMap.to_seq |> Seq.map (fun (x,y) -> (y,x)) |> SMap.of_seq in
    let (subst_var, subst_sym, _, _) = renaming subst_var subst_sym subst_var_rev subst_sym_rev a b in
    dbg D_trace2 @@ lazy (sprintf "renaming: ok");
    Some (subst_var, subst_sym)
  with Renaming_sym_fail -> 
    dbg D_trace2 @@ lazy (sprintf "renaming: ok");
    None

(** Merges two [subst_sym], if they're not conflicting. *)
let merge_sym a b = 
  dbg D_trace @@ lazy (sprintf "merge_sym: %s %s" (to_string_sym a) (to_string_sym b));
  try
    let a_rev = a |> SMap.to_seq |> Seq.map (fun (x,y) -> (y,x)) |> SMap.of_seq in
    let b_rev = b |> SMap.to_seq |> Seq.map (fun (x,y) -> (y,x)) |> SMap.of_seq in
    let merged = 
      ignore @@ SMap.union (fun key x y -> 
        if x != y then raise_notrace Renaming_sym_fail else Some x
      ) a_rev b_rev;
      SMap.union (fun key x y -> 
        if x != y then raise_notrace Renaming_sym_fail else Some x
      ) a b
      (* SMap.union (fun key x y -> 
        dbg D_trace @@ lazy (sprintf "%s %s" (Symbol.to_string x) (Symbol.to_string y));
        if x != y || a_rev |> SMap.find y != x || b_rev |> SMap.find x != y then raise_notrace Renaming_sym_fail else Some x
      ) a b *)
    in
    dbg D_trace @@ lazy (sprintf "merge_sym: ok");
    Some merged
  with Renaming_sym_fail -> 
    dbg D_trace @@ lazy (sprintf "merge_sym: fail");
    None
