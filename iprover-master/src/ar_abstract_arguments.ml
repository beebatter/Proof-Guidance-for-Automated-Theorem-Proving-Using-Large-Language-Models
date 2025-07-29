(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

module LI = Logic_interface

module type ArgFun = sig
  val elg_symb: Options.options -> Symbol.symbol -> bool
  val filter_args_by_bs: bool list -> 'a list -> 'a list
  val create_astype: Symbol.symbol -> bool list -> Symbol.stype
end

module type S = sig
  type t
  type csymb
  type asymb
  val fill_symb2bsig: Options.options -> Clause.clause -> bool list LI.SMap.t ->
    bool list LI.SMap.t
  val empty: t
  val add: csymb -> asymb -> t -> t
  val mem: csymb -> t -> bool
  val to_string: [`Both | `Bwd | `Fwd] -> t -> string
  val cstsymb: bool list LI.SMap.t -> Symbol.symbol -> csymb
  val abstract_symbol: bool list LI.SMap.t -> string -> csymb -> asymb
  val is_eligible: 'a LI.SMap.t -> Options.options -> Term.term -> bool
  val replace_symbols: (Term.term list -> bool list -> Term.term list) ->
    t -> Term.term -> Term.term
  val refine_abstract_sig: string -> t -> asymb list -> t
end

module SBS = struct
  type t = Symbol.symbol * bool list

  let compare (s1,_) (s2,_) =
    Symbol.compare_fast_key s1 s2

  let bool_list_to_str bl =
    let bl_str = String.concat "," (List.map string_of_bool bl) in
    String.concat "" ["["; bl_str; "]"]

  let to_string (s,bs) = Symbol.to_string s ^ " : " ^ bool_list_to_str bs
end

module BM = Bimap.Make(SBS)(SBS)

module Make(AF: ArgFun) = struct
  include BM

  type csymb = key
  type asymb = value

  (* non-variable filter sig *)
  let non_var_bsig term_lst = List.map (fun t -> not (Term.is_var t)) term_lst

  let rec fill_symb2bsig_map info map = function
    | Term.Fun _ as t when Term.is_ground t -> map
    | Fun (symb,args,_) when symb != Symbol.symb_neg && AF.elg_symb info symb ->
      let args_lst = Term.arg_to_list args in
      let novar_bs = non_var_bsig args_lst in
      let new_args = AF.filter_args_by_bs novar_bs args_lst in
      let nmap =
        if List.exists not novar_bs then
          match LI.SMap.find_opt symb map with
          | Some bs -> LI.SMap.add symb (List.map2 (&&) novar_bs bs) map
          | None -> LI.SMap.add symb novar_bs map
        else
          map
      in
      List.fold_left (fill_symb2bsig_map info) nmap new_args
    | Fun (symb,args,_) ->
      let args_lst = Term.arg_to_list args in
      List.fold_left (fill_symb2bsig_map info) map args_lst
    | t -> map

  let fill_symb2bsig info clause map =
    List.fold_left (fill_symb2bsig_map info) map (Clause.get_literals clause)

  let cstsymb symb2bs symb =
    match LI.SMap.find_opt symb symb2bs with
    | Some bs -> (symb, bs)
    | None -> (symb, [])

  let bs_filter_positions b bsig =
    let f (cnt,res) e = if (e = b) then (cnt+1, cnt :: res) else (cnt+1, res) in
    let (_, res) = (List.fold_left f (1, []) bsig) in
    List.rev res

  let create_asymb pfx symb bsig =
    let positions = bs_filter_positions true bsig in
    let symb_str = Symbol.to_string symb in
    let positions_str = "0" :: (List.map string_of_int positions) in
    let name = String.concat "_" (pfx :: symb_str :: positions_str) in
    let astype = AF.create_astype symb bsig in
    LI.create_symbol name astype

  let abstract_symbol symb2bs id ((symb,bs) as csymb) =
    match LI.SMap.find_opt symb symb2bs with
    | Some bs -> (create_asymb id symb bs,bs)
    | None -> csymb

  let is_eligible symb2bs opts = function
    | Term.Fun (symb,_,_) when 
        (LI.SMap.mem symb symb2bs) &&  (not (LI.is_special_symb symb))
      -> AF.elg_symb opts symb
    | _ -> false

  let rec replace_symbols ca_f asig = function
    | Term.Fun (symb,args,_) ->
      let args_lst = Term.arg_to_list args in
      let (nsymb,nargs) =
        match BM.find (symb,[]) asig with
        | Some (asymb,abs) ->
          let aargs = ca_f args_lst abs in
          let nargs = List.map (replace_symbols ca_f asig) aargs in
          (asymb, nargs)
        | None ->
          let nargs = List.map (replace_symbols ca_f asig) args_lst in
          (symb, nargs)
      in
      LI.add_fun_term nsymb nargs
    | term -> term

  let flip_first_false bsig =
    let rec f flag accu = function
      | [] -> accu
      | hd::tl when flag && not hd -> f false (true :: accu) tl
      | hd::tl -> f flag (hd :: accu) tl
    in
    List.rev (f true [] bsig)

  let ref_asymb id asig asymb =
    match BM.find_by_value asymb asig with
    | Some ((s,bs) as csymb :: tl) when tl = [] ->
      let nbsig = flip_first_false bs in
      if List.for_all (fun e -> e) nbsig then
        BM.remove_by_key csymb asig
      else
        let ncsmb = (s, nbsig) in
        let nasmb = (create_asymb id s nbsig, nbsig) in
        BM.add ncsmb nasmb asig
    | None -> asig
    | _ -> failwith ("ar_abstract_arguments.ml:ref_asymb: " ^
                     "More than one val in asig; this should not happen")

  let refine_abstract_sig id asig asymbs =
    List.fold_left (ref_asymb id) asig asymbs
end
