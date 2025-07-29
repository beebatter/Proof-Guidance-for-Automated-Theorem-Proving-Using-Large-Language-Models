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
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)



let is_well_definedness_axiom clause = 
  if Clause.length clause <> 3 then None else
  let pos, neg = List.partition (Term.is_pos_lit) (Clause.get_lits clause) in
  match pos, neg with
  | [pos], [neg1; neg2] -> 
    begin match Term.Eq.decompose_atom_type pos with
    | Some (eq_type, Term.Var(x, _), Term.Var(y, _)) when Var.O.(x != y) ->
      begin match Term.get_atom neg1, Term.get_atom neg2 with
      | Term.Fun(sym1, args1, _), Term.Fun(sym2, args2, _) when sym1 == sym2 && sym1 != Symbol.symb_typed_equality ->
        let rec loop pos v_set i args1 args2 = 
          match args1, args2 with
          | (Term.Var(x1, _) as hd1) :: tl1, (Term.Var(x2, _) as hd2) :: tl2 ->
            if hd1 == hd2 then
              let v_set' = v_set |> VSet.add x1 in
              if v_set' != v_set then
                loop pos v_set' (i+1) tl1 tl2
              else 
                None
            else
              if pos = (-1) then
                let pos' = i in
                let v_set' = v_set |> VSet.add x1 in
                let v_set'' = v_set' |> VSet.add x2 in
                if v_set != v_set' && v_set' != v_set'' then
                  loop pos' v_set'' (i+1) tl1 tl2
                else
                  None
              else
                None
          | [], [] -> 
            if pos <> -1 then
              Some (sym1, pos, eq_type)
            else
              None
          | _ -> None
        in
        loop (-1) VSet.empty 0 (Term.arg_to_list args1) (Term.arg_to_list args2)

        (* begin match args1, args2 with
        | [Term.Var(a1, _); Term.Var(b1, _); Term.Var(c1, _)] 
        , [Term.Var(a2, _); Term.Var(b2, _); Term.Var(c2, _)] ->
          if Var.O.(a1 <> b1) && Var.O.(b1 <> c1) && Var.O.(c1 <> a1)
          && Var.O.(a2 <> b2) && Var.O.(b2 <> c2) && Var.O.(c2 <> a2)
          then
            if Var.O.(a1 = a2) && Var.O.(b1 = b2) && (Var.O.(c1 = x) && Var.O.(c2 = y) || Var.O.(c1 = y) && Var.O.(c2 = x)) then
              Some (sym1, 2, eq_type)
            else if Var.O.(a1 = a2) && Var.O.(c1 = c2) && (Var.O.(b1 = x) && Var.O.(b2 = y) || Var.O.(b1 = y) && Var.O.(b2 = x)) then
              Some (sym1, 1, eq_type)
            else if Var.O.(b1 = b2) && Var.O.(c1 = c2) && (Var.O.(a1 = x) && Var.O.(a2 = y) || Var.O.(a1 = y) && Var.O.(a2 = x)) then
              Some (sym1, 0, eq_type)
            else
              None
          else 
            None
        | _ -> None
        end *)
      | _ -> None
      end
    | _ -> None
    end
  | _ -> None
      
let is_well_definedness_axiom clause = 
  dbg D_trace @@ lazy (sprintf "is_well_definedness_axiom: %s" (Clause.to_string_tptp clause));
  let result = is_well_definedness_axiom clause in
  dbg_env D_trace (fun () -> 
    match result with Some (sym, n, _) -> dbg D_trace @@ lazy (sprintf "%s well_defined on argument %d" (Symbol.to_string sym) n) | None -> ()
  );
  result

let is_totality_axiom clause = 
  match Clause.get_lits clause with
  | [lit] -> 
    begin match lit with
    | Term.Fun (sym, args, _) -> 
      if Symbol.get_arity sym > 1 then
        let rec loop vars vars_set f i args = 
          match args with
          | hd :: tl -> 
            begin match hd with
            | Term.Var (x, _) -> 
              let vars' = hd :: vars in
              let vars_set' = vars_set |> VSet.add x in
              if vars_set' != vars_set then
                loop vars' vars_set' f (i+1) tl
              else
                None
            | Term.Fun (sym_f, args_f, _) -> 
              if Symbol.get_arity sym_f = Symbol.get_arity sym - 1 then
                match f with
                | None -> loop vars vars_set (Some (sym_f, Term.arg_to_list args_f, i)) (i+1) tl
                | Some _ -> None
              else
                None
            end
          | [] -> 
            begin match f with
            | Some (sym_f, args_f, pos_f) -> 
              if List.X.equal ~eq:(==) (List.rev vars) args_f then
                Some (sym, pos_f, sym_f)
              else
                None
            | None -> None
            end
        in
        loop [] VSet.empty None 0 (Term.arg_to_list args)
      else 
        None

      (* begin match Term.arg_to_list args with
      | [Term.Var (x, _); Term.Var (y, _); Term.Fun (f, args_f, _)] -> 
        begin match Term.arg_to_list args_f with
        | [Term.Var (x', _); Term.Var (y', _)] when Var.O.(x = x' && y = y') ->
          Some (sym, 2, f)
        | _ -> None
        end
      | [Term.Var (x, _); Term.Fun (f, args_f, _); Term.Var (y, _);] -> 
        begin match Term.arg_to_list args_f with
        | [Term.Var (x', _); Term.Var (y', _)] when Var.O.(x = x' && y = y') ->
          Some (sym, 1, f)
        | _ -> None
        end
      | [Term.Fun (f, args_f, _); Term.Var (x, _); Term.Var (y, _)] -> 
        begin match Term.arg_to_list args_f with
        | [Term.Var (x', _); Term.Var (y', _)] when Var.O.(x = x' && y = y') ->
          Some (sym, 0, f)
        | _ -> None
        end
      | _ -> None
      end *)
    | Term.Var _ -> None
    end
  | _ -> None

let is_totality_axiom clause = 
  dbg D_trace @@ lazy (sprintf "is_totality_axiom: %s" (Clause.to_string_tptp clause));
  let result = is_totality_axiom clause in
  dbg_env D_trace (fun () -> 
    match result with Some (sym, n, f_sym) -> dbg D_trace @@ lazy (sprintf "%s total on argument %d to %s" (Symbol.to_string sym) n (Symbol.to_string f_sym)) | None -> ()
  );
  result


(* let new_symbol sym n = 
  let typ = Symbol.get_type sym in
  let Some((typ_val, [typ_a; typ_b; typ_c])) = Symbol.split_stype typ in
  dassert (fun () -> typ_val == Symbol.symb_bool_type);
  let typ'_val, typ'_args = 
    match n with
    | 0 -> typ_a, [typ_b; typ_c]
    | 1 -> typ_b, [typ_a; typ_c]
    | 2 -> typ_c, [typ_a; typ_b]
    | _ -> assert false
  in 
  let name' = Symbol.add_iprover_pref ("welldef_" ^ Symbol.get_name sym) in
  let typ' = Symbol.create_stype typ'_args typ'_val in
  let sym' = create_symbol name' typ' in
  sym' *)

let process_clause_set clauses = 
  dbg D_trace @@ lazy "process_clause_set";
  let map_wd = 
    clauses |> List.fold_left (fun map c -> 
      match is_well_definedness_axiom c with
      | None -> map
      | Some (sym, n, eq_type) -> map |> SMap.add sym (n, eq_type)
    ) SMap.empty
  in
  let map_tot = 
    clauses |> List.fold_left (fun map c -> 
      match is_totality_axiom c with
      | None -> map
      | Some (sym, n, f_sym) -> map |> SMap.add sym (n, f_sym)
    ) SMap.empty
  in
  let map = 
    SMap.merge (fun sym wd tot ->
      match wd, tot with
      | Some (n, eq_type), Some (n', f_sym) when n = n' -> 
        Some (n, eq_type, f_sym)
      | _ -> None
    ) map_wd map_tot
  in

  clauses |> List.map (fun clause ->
    let lits = Clause.get_lits clause in
    let any_changed = ref false in
    let lits' = 
      lits |> List.map (fun lit ->
        let sign, atom = Term.split_sign_lit lit in
        match atom with
        | Term.Fun(symb, args, _) -> 
          begin match map |> SMap.find_opt symb with
          | Some (n, eq_type, symb') -> 
            any_changed := true;
            let rhs, args' = 
              let rec loop i a b = 
                if i = n then
                  a, b
                else
                  let[@warning "-8"] hd::tl = b in
                  loop (i+1) (hd::a) tl
              in
              let[@warning "-8"] a, (rhs::b) = loop 0 [] (Term.arg_to_list args) in
              rhs, List.rev_append a b

              (* match n with
              | 0 -> a, [b;c]
              | 1 -> b, [a;c]
              | 2 -> c, [a;b]
              | _ -> assert false *)
            in
            add_lit_eq sign eq_type (add_fun_term symb' args') rhs
          | None -> lit
          end
        | Term.Var _ -> assert false
      )
    in
    if !any_changed then (
      let source = Clause.tstp_source_well_definedness clause in
      let clause' = create_clause ~normalise_eqs:true source lits' in
      let clause'' = Inference_rules.unflatten_clause ~normalise_eqs:true clause' in
      dbg D_trace @@ lazy (sprintf "Well definedness from %s" (Clause.to_string_tptp clause));
      dbg D_trace @@ lazy (sprintf "                   to %s" (Clause.to_string_tptp clause'));
      dbg D_trace @@ lazy (sprintf "                   to %s" (Clause.to_string_tptp clause''));
      Statistics.(bump_int_stat prep_well_definedness);
      clause'
    ) else
      clause
  )

