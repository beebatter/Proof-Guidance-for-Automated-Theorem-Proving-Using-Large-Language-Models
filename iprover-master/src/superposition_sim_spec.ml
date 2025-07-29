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

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)




module Simplify = Simplify_new



(* First step: turn [Options.SupSimplificationSetup] into [Simplify] modules.
   - Figure out indices required by rules
   - Optionally remove demod, or demod in immed simplifications *)

type spec' = {
  indices_passive' : (module Simplify.Index) list;
  indices_active'  : (module Simplify.Index) list;
  indices_immed'   : (module Simplify.Index) list;
  indices_input'   : (module Simplify.Index) list;

  full_triv' : (module Simplify.TrivRule) list;
  full_fw' : (module Simplify.FwRule) list;
  full_bw' : (module Simplify.BwRule) list;
  
  immed_triv' : (module Simplify.TrivRule) list;
  immed_fw_immed' : (module Simplify.FwRule) list;
  immed_bw_immed' : (module Simplify.BwRule) list;
  immed_fw_main' : (module Simplify.FwRule) list;
  immed_bw_main' : (module Simplify.BwRule) list;

  input_triv' : (module Simplify.TrivRule) list;
  input_fw' : (module Simplify.FwRule) list;
  input_bw' : (module Simplify.BwRule) list;
}



let indices_of_fw_rule x = 
  let open Options.SupSimplificationSetup in
  match x with
  | FwSubsumption 
  | FwSubsumptionRes -> [NonunitSubsumptionIndex]
  | FwUnitSubsumption -> [UnitSubsumptionIndex]
  | FwDemod
  | FwDemodLoopTriv -> [FwDemodIndex]
  | FwLightNorm -> [LightNormIndexNoreduce]
  | FwDemodLightNormLoopTriv -> [FwDemodIndex; LightNormIndexNoreduce]
  | ACJoinability
  | ACNormalisation -> []
  | SMTSubs -> [SMTIncrIndex]
  | FwACDemod -> [FwACDemodIndex]
  | FwGroundJoinability -> [FwDemodIndex]
  | FwConnectedness -> [FwDemodIndex; (* RevDemodIndex *)]

let indices_of_bw_rule x = 
  let open Options.SupSimplificationSetup in
  match x with
  | BwSubsumption 
  | BwSubsumptionRes -> [NonunitSubsumptionIndex]
  | BwUnitSubsumption -> [UnitSubsumptionIndex]
  | BwDemod -> [BwDemodIndex]
  | BwACDemod -> [BwACDemodIndex]

let indices_of_rules fw bw = 
  List.append
    (List.concat_map indices_of_fw_rule fw)
    (List.concat_map indices_of_bw_rule bw)

let add_smt_check_interval interval rules = 
  let open Options.SupSimplificationSetup in
  if interval < 0 then
    rules
  else
    SMTSetIndex :: rules

let add_gjoin_interval interval rules = 
  let open Options.SupSimplificationSetup in
  if interval <= 0 then
    rules
  else
    BwGjoinIndex :: rules

let add_triv l =
  let open Simplify.FirstClass in
  match l with
  | [] -> [trivRules]
  | _::_ -> trivRules :: (l @ [trivRules])

let add_ss l = 
  let open Simplify.FirstClass in
  subsetSubsumptionIndex :: l

let add_bwgjoin i l = 
  let open Simplify.FirstClass in
  if i <= 0 then l else l @ [bwGroundJoinability]



(* Make spec' from options *)
let mk_spec' (opts: Options.SupSimplificationSetup.spec) : spec' = 
  let uniq = List.X.sort_uniq (Ord.lift Obj.magic compare) in

  let indices_passive, indices_active = 
    List.append 
      (indices_of_rules opts.full_fw       opts.full_bw)
      (indices_of_rules opts.immed_fw_main opts.immed_bw_main)
    |> add_smt_check_interval opts.sup_smt_interval
    |> add_gjoin_interval opts.sup_bw_gjoin_interval
    |> uniq
    |> List.partition (fun x -> List.memq x opts.indices_passive)
  in
  let indices_immed = indices_of_rules opts.immed_fw_immed opts.immed_bw_immed |> uniq in
  let indices_input = indices_of_rules opts.input_fw       opts.input_bw       |> uniq in

  Simplify.FirstClass.{
    indices_passive' = add_ss @@ List.map module_of_index indices_passive;
    indices_active'  =           List.map module_of_index indices_active;
    indices_immed'   = add_ss @@ List.map module_of_index indices_immed;
    indices_input'   = add_ss @@ List.map module_of_index indices_input;

    full_triv' = add_triv @@ List.map module_of_trivRule opts.full_triv;
    full_fw' = List.map module_of_fwRule opts.full_fw;
    full_bw' = List.map module_of_bwRule opts.full_bw;
    
    immed_triv' = add_triv @@ List.map module_of_trivRule opts.immed_triv;
    immed_fw_immed' = List.map module_of_fwRule opts.immed_fw_immed;
    immed_bw_immed' = List.map module_of_bwRule opts.immed_bw_immed;
    immed_fw_main' = List.map module_of_fwRule opts.immed_fw_main;
    immed_bw_main' = List.map module_of_bwRule opts.immed_bw_main |> add_bwgjoin opts.sup_bw_gjoin_interval;

    input_triv' = add_triv @@ List.map module_of_trivRule opts.input_triv;
    input_fw' = List.map module_of_fwRule opts.input_fw;
    input_bw' = List.map module_of_bwRule opts.input_bw;
  }



(* Remove FwDemod/BwDemod/LightNorm from a [spec'] *)
let remove_demod x =
  let open Simplify.FirstClass in
  {
    indices_passive' = remove_demod_index x.indices_passive';
    indices_active'  = remove_demod_index x.indices_active';
    indices_immed'   = remove_demod_index x.indices_immed';
    indices_input'   = remove_demod_index x.indices_input';

    full_triv' = x.full_triv';
    full_fw' = remove_demod_fw x.full_fw';
    full_bw' = remove_demod_bw x.full_bw';
    
    immed_triv' = x.immed_triv';
    immed_fw_immed' = remove_demod_fw x.immed_fw_immed';
    immed_bw_immed' = remove_demod_bw x.immed_bw_immed';
    immed_fw_main' = remove_demod_fw x.immed_fw_main';
    immed_bw_main' = remove_demod_bw x.immed_bw_main';

    input_triv' = x.input_triv';
    input_fw' = remove_demod_fw x.input_fw';
    input_bw' = remove_demod_bw x.input_bw';
  }

(* Remove Subsumption/SubsetSubsumption from a [spec'] *)
(* let remove_subs x =
  let open Simplify.FirstClass in
  {
    indices_passive' = remove_subs_index x.indices_passive';
    indices_active'  = remove_subs_index x.indices_active';
    indices_immed'   = remove_subs_index x.indices_immed';
    indices_input'   = remove_subs_index x.indices_input';

    full_triv' = x.full_triv';
    full_fw' = remove_subs_fw x.full_fw';
    full_bw' = remove_subs_bw x.full_bw';
    
    immed_triv' = x.immed_triv';
    immed_fw_main' = remove_subs_fw x.immed_fw_main';
    immed_fw_immed' = remove_subs_fw x.immed_fw_immed';
    immed_bw_main' = remove_subs_bw x.immed_bw_main';
    immed_bw_immed' = remove_subs_bw x.immed_bw_immed';

    input_triv' = x.input_triv';
    input_fw' = remove_subs_fw x.input_fw';
    input_bw' = remove_subs_bw x.input_bw';
  } *)

(* Remove FwDemod/BwDemod/LightNorm from a [spec'] *)
let remove_ac x =
  let open Simplify.FirstClass in
  {
    indices_passive' = remove_ac_index x.indices_passive';
    indices_active'  = remove_ac_index x.indices_active';
    indices_immed'   = remove_ac_index x.indices_immed';
    indices_input'   = remove_ac_index x.indices_input';

    full_triv' = x.full_triv';
    full_fw' = remove_ac_fw x.full_fw';
    full_bw' = remove_ac_bw x.full_bw';
    
    immed_triv' = x.immed_triv';
    immed_fw_immed' = remove_ac_fw x.immed_fw_immed';
    immed_bw_immed' = remove_ac_bw x.immed_bw_immed';
    immed_fw_main' = remove_ac_fw x.immed_fw_main';
    immed_bw_main' = remove_ac_bw x.immed_bw_main';

    input_triv' = x.input_triv';
    input_fw' = remove_ac_fw x.input_fw';
    input_bw' = remove_ac_bw x.input_bw';
  }

(* As [remove_demod] but only ffrom immed, and immed_bw_main *)
let remove_immeddemod x =
  let open Simplify.FirstClass in
  {
    indices_passive' = x.indices_passive';
    indices_active'  = x.indices_active';
    indices_immed'   = remove_demod_index x.indices_immed';
    indices_input'   = x.indices_input';

    full_triv' = x.full_triv';
    full_fw' = x.full_fw';
    full_bw' = x.full_bw';
    
    immed_triv' = x.immed_triv';
    immed_fw_immed' = remove_demod_fw x.immed_fw_immed';
    immed_bw_immed' = remove_demod_bw x.immed_bw_immed';
    immed_fw_main' = x.immed_fw_main';
    immed_bw_main' = remove_demod_bw x.immed_bw_main';

    input_triv' = x.input_triv';
    input_fw' = x.input_fw';
    input_bw' = x.input_bw';
  }



(* Second step: turn [Simplify] modules into functions.
   - indices_* will add the clause to those indices
   - full/immed/input_* will perform those simplifications in sequence, and 
     until fixpoint if options say so *)

type spec = {
  indices_passive : (clause -> unit);
  indices_active  : (clause -> unit);
  indices_immed   : Simplify.set -> (clause -> unit);
  indices_input   : (clause -> unit);

  full_triv : (clause -> Simplify.fw_result);
  full_fw : (clause -> Simplify.fw_result);
  full_bw : (clause -> Simplify.bw_result);
  
  immed_triv : (clause -> Simplify.fw_result);
  immed_fw_immed : Simplify.set -> (clause -> Simplify.fw_result);
  immed_bw_immed : Simplify.set -> (clause -> Simplify.bw_result);  
  immed_fw_main  : (* Simplify.set -> *) (clause -> Simplify.fw_result);
  immed_bw_main  : (* Simplify.set -> *) (clause -> Simplify.bw_result);  

  input_triv : (clause -> Simplify.fw_result);
  input_fw : (clause -> Simplify.fw_result);
  input_bw : (clause -> Simplify.bw_result);
}

let mk_spec_inner (x:spec') ~sim_state ~imsim_state (opts:Options.SupSimplificationSetup.spec) : spec =
  let module CacheSim = Options.SupSimplificationSetup.CacheSim in
  
  let index_module_to_func state l = 
    List.map (fun (module M : Simplify.Index) -> M.index) l
    |> Simplify.add_to_indices state
  in

  let triv_module_to_func (* ~cache *) l =
    (* let module M = Options.SupSimplificationSetup.CacheSim in *)
    (* match cache with *)
    List.map (fun (module M : Simplify.TrivRule) -> M.simplify) l
    |> Simplify.Fw_result.fold
  in
  
  let fw_module_to_func ~fixpoint ~cache state l =
    if List.X.is_empty l then
      Simplify.Fw_result.return
    (* No fixpoint: transform into functions with map and fold, handling cache as requested *)
    else if not fixpoint then
      begin match cache with
      | CacheSim.None -> 
        List.map (fun (module M : Simplify.FwRule) -> M.simplify state) l
        |> Simplify.Fw_result.fold
        |> Simplify.Fw_result.elim_sim state
      | CacheSim.Once -> 
        List.map (fun (module M : Simplify.FwRule) -> M.simplify state) l
        |> Simplify.Fw_result.fold
        |> Simplify.Fw_result.cache_sim state
      | CacheSim.All -> 
        List.map (fun (module M : Simplify.FwRule) -> Simplify.Fw_result.cache_sim state (M.simplify state)) l
        |> Simplify.Fw_result.fold
      end
    (* Fixpoint: store in array of functions, handling cache as requested, and build a function 
       that loops through that array until fixpoint has been reached *)
    else 
      let assert_local_fixpoint = true in
      let assert_global_fixpoint = true in
      
      let eq_simp x s = match s with Simplify.Simplified x' -> x == x' | Simplify.Eliminated _ -> false in

      let eq_simp_dbg x s = match s with
        Simplify.Simplified x' ->
          (if x!=x' then
            dbg D_trace @@ lazy (sprintf "eq_simp_dbg: x: %s x': %s"
                                   (Clause.to_string x) (Clause.to_string x'))
          );
          x == x'
      |  Simplify.Eliminated x' ->
          dbg D_trace @@ lazy (sprintf "eq_simp_dbg: Simplify.Eliminated: x:%s x':%s"  (Clause.to_string x) (Clause.clause_list_to_string x'));
          false
      in
      
      (* let fp opt f = if opt then Simplify.Fw_result.fix_point f else f in *)
      let sim_fun_list : (clause -> Simplify.fw_result) list =
        let p = fun (module M : Simplify.FwRule) -> Simplify.Fw_result.cache_sim state (M.simplify state) in
        List.map p l
      in
        
      let app_sim_res f res =
        match res with
        | Simplify.Eliminated _ -> res
        | Simplify.Simplified x -> f x
      in
      let wrap_fun_res f =
        (fun res -> app_sim_res f res)
      in

      let sim_res_fun_list = List.map wrap_fun_res sim_fun_list in
      
      let join_res old_res new_res =
        match (old_res, new_res) with
        | (Simplify.Eliminated _ ,_) -> old_res
        | (_, Simplify.Eliminated _) -> new_res
        | (Simplify.Simplified old_x, Simplify.Simplified new_x) -> 
            if old_x == new_x then old_res
            else
              (
               dbg D_trace @@ lazy (sprintf "sim fp single: join_res: old:%s new:%s"  (Clause.to_string old_x) (Clause.to_string new_x));
               new_res)
                 
      in
                
      let rec sim_fp_single f res =
        let new_res = join_res res (f res) in
        if new_res == res then res
        else
          sim_fp_single f new_res
      in
      
      let sim_res_fun_list_fp = List.map (fun f -> sim_fp_single f) sim_res_fun_list in
          
      let sim_fp_result c_res = sim_fp_single (fold_left_fun_list sim_res_fun_list_fp) c_res in
      
      if cache == CacheSim.Once then
        Simplify.Fw_result.cache_sim state (fun c -> sim_fp_result (Simplify.Simplified c))
      else if cache == CacheSim.None then
        Simplify.Fw_result.elim_sim state (fun c -> sim_fp_result (Simplify.Simplified c))
      else
        fun c -> sim_fp_result (Simplify.Simplified c)
            
(*
      let array_f : (clause -> Simplify.fw_result) array = 
        if cache == CacheSim.All then
          let p[@inline] = fun (module M : Simplify.FwRule) -> Simplify.Fw_result.cache_sim state (M.simplify state) in
          l |> List.to_seq |> Seq.map p |> Array.of_seq
        else
          let p[@inline] = fun (module M : Simplify.FwRule) -> M.simplify state in
          l |> List.to_seq |> Seq.map p |> Array.of_seq
      in
      let array_results : clause array = 
        Array.make (Array.length array_f) (Obj.magic 0)
      in
      (* let array_dirty : bool array = 
        let p[@inline] = fun m -> not @@ List.memq m Simplify.FirstClass.[fwSubsumption; fwSubsumptionRes; fwUnitSubs; fwSubsumptionNonStrict] in
        l |> List.to_seq |> Seq.map p |> Array.of_seq
      in *)
      let n = Array.length array_f in

      let rec loop_2 (*dirty*) i x =
        dbg D_trace @@ lazy (sprintf "loop_2: n:%i i: %i x: %s"
                               n i (Clause.to_string x));
    
        (* printf "loop_2 %b %d %s\n" dirty i (Clause.to_string_tptp x); *)
        if i = n then loop_2 (*dirty*) 0 x else
        if (* not dirty || *) Clause.Bc.(x == array_results.(i)) then (
          if assert_global_fixpoint then dassert (fun () -> 
            array_f |> Array.for_all (fun f -> eq_simp x (f x));
          );
          Simplify.Simplified x
        ) else (
          let x' = array_f.(i) x in
          match x' with
          | Simplify.Eliminated _ -> 
            x'
          | Simplify.Simplified x' -> 
            if assert_local_fixpoint && x' != x then dassert (fun () -> eq_simp_dbg x' (array_f.(i) x'));
            array_results.(i) <- x';
            (* let dirty' = dirty || (x' != x (*&& array_dirty.(i)*)) in *)
            loop_2 (*dirty'*) (i+1) x'
        )
      in

      let rec loop_1 (*dirty*) i x =
        dbg D_trace @@ lazy (sprintf "loop_1: n:%i i: %i x: %s"
                              n i (Clause.to_string x));
        (* printf "loop_1 %b %d %s\n" dirty i (Clause.to_string_tptp x); *)
        if i = n then loop_2 (*dirty*) 0 x else
        let x' = array_f.(i) x in
        match x' with
        | Simplify.Eliminated _ -> 
          x'
        | Simplify.Simplified x' -> 
            if assert_local_fixpoint && x' != x then
              (
               dbg D_trace @@ lazy (sprintf "loop_1: x: %s x': %s"
                                      (Clause.to_string x) (Clause.to_string x'));
(*                 dassert (fun () ->
                   dbg D_trace @@ lazy (sprintf "loop_1: x: %s x': %s"
                                          (Clause.to_string x) (Clause.to_string x'));
                   eq_simp_dbg x' (array_f.(i) x'));
 *)             
               array_results.(i) <- x';
               loop_1 (*dirty'*) i x'
              )
            else
          (* let dirty' = dirty || (x' != x (*&& array_dirty.(i)*)) in *)
              (
               loop_1 (*dirty'*) (i+1) x')
      in

      if cache == CacheSim.Once then
        Simplify.Fw_result.cache_sim state (fun c -> loop_1 (*false*) 0 c)
      else if cache == CacheSim.None then
        Simplify.Fw_result.elim_sim state (fun c -> loop_1 (*false*) 0 c)
      else
        fun c -> loop_1 (*false*) 0 c
*)
  in
  
  let bw_module_to_func state l =
    List.map (fun (module M : Simplify.BwRule) -> M.simplify state) l
    |> Simplify.Bw_result.fold
  in
  
  {
    indices_passive = index_module_to_func sim_state x.indices_passive';
    indices_active  = index_module_to_func sim_state x.indices_active';
    indices_immed   = (fun s -> index_module_to_func s x.indices_immed');
    indices_input   = index_module_to_func imsim_state x.indices_input';

    full_triv = triv_module_to_func x.full_triv';
    full_fw = fw_module_to_func sim_state x.full_fw' ~fixpoint:opts.full_fixpoint ~cache:opts.cache_sim;
    full_bw = bw_module_to_func sim_state x.full_bw';
    
    immed_triv = triv_module_to_func x.immed_triv';
    immed_fw_immed = (fun s -> fw_module_to_func s x.immed_fw_immed' ~fixpoint:opts.immed_fixpoint ~cache:opts.cache_sim);
    immed_bw_immed = (fun s -> bw_module_to_func s x.immed_bw_immed');
    immed_fw_main  = fw_module_to_func sim_state x.immed_fw_main' ~fixpoint:opts.main_fixpoint ~cache:opts.cache_sim;
    immed_bw_main  = bw_module_to_func sim_state x.immed_bw_main';

    input_triv = triv_module_to_func x.input_triv';
    input_fw = fw_module_to_func imsim_state x.input_fw' ~fixpoint:opts.input_fixpoint ~cache:CacheSim.None;
    input_bw = bw_module_to_func imsim_state x.input_bw';
  }



let mk_spec ~demod_flag (* ~subs_flag *) ~ac_flag ~sim_state ~imsim_state x =
  let spec' = 
    mk_spec' x
    |> (if demod_flag then Fun.id else remove_demod)
    |> (if ac_flag then Fun.id else remove_ac)
  in
  mk_spec_inner spec' ~sim_state ~imsim_state x
  (* match demod_flag, subs_flag with
  | true, true ->
    mk_spec_inner (mk_spec' x) ~sim_state ~imsim_state
  | false, true ->
    mk_spec_inner (remove_demod @@ mk_spec' x) ~sim_state ~imsim_state
  | true, false ->
    mk_spec_inner (remove_subs @@ mk_spec' x) ~sim_state ~imsim_state
  | false, false -> 
    assert false *)

let mk_spec_noimmeddemod ~sim_state ~imsim_state x = 
  let spec' = remove_immeddemod (mk_spec' x) in
  mk_spec_inner spec' ~sim_state ~imsim_state x

(* Disable immed inter-simplification *)
let disable_immed spec = { spec with
  indices_immed = (Simplify_new.SubsetSubsumptionIndex.add);
  (* immed_triv = []; *)  (* Triv should be kept *)
  immed_fw_immed = (fun _ -> Simplify.Fw_result.return);
  (* immed_bw_immed = (fun _ -> []); *)  (* And actually bw should be kept also, due to given clause simplification *)
}



(* --- *)


 (* unit is needed due to optional and only labelled args *)
let mk_order ?(with_var=true) ~ordering ~symb_ordering ~term_weight ?theory_record () = 
  let symb_ordering = 
    match symb_ordering with
    | Options.Ordering.Symb.Invfreq         -> Symbol.cmp_invfreq
    | Options.Ordering.Symb.InvfreqArity    -> Symbol.cmp_invfreq_arity
    | Options.Ordering.Symb.InvfreqInvArity -> Symbol.cmp_invfreq_invarity
    | Options.Ordering.Symb.Arity       -> Symbol.cmp_symb_arity
    | Options.Ordering.Symb.ArityRev    -> Symbol.cmp_symb_arity_rev
    | Options.Ordering.Symb.ArityRandom -> Symbol.cmp_symb_arity_random
    | Options.Ordering.Symb.Random -> Symbol.cmp_symb_random
    | Options.Ordering.Symb.Custom l ->
        symbol_cmp_custom (List.map find_symbol' l) (Symbol.cmp_invfreq) []  (* TODO change options to specify this properly *)
          
  in
  match ordering with
  | Options.Ordering.Func.Theory -> 
    let open Option.O in
    let ords = Theory_orderings.get_ordering ~symb_ordering_default:symb_ordering (Option.get theory_record) in 
    begin match ords with
    | [] -> KBO.make ~with_var:true ~inc_criteria:false ~weight:Term.get_num_of_symb ~symb_ordering ()
    | hd::_ -> hd
    end
  (* HACK! Fix later! *)
  | Options.Ordering.Func.TheoryN n -> 
    let open Option.O in
    let ords = Theory_orderings.get_ordering ~symb_ordering_default:symb_ordering (Option.get theory_record) in 
    begin match ords with
    | [] -> KBO.make ~with_var:true ~inc_criteria:false ~weight:Term.get_num_of_symb ~symb_ordering ()
    | [hd] -> hd
    | a::b::_ -> if n = 1 then b else a
    (* Theory_orderings.get_ordering ~permut:n (Option.get theory_record) |? 
      let weight = Term.get_num_of_symb in
      KBO.make ~with_var:true ~inc_criteria:false ~weight ~symb_ordering *)
    end
  | Options.Ordering.Func.KBO | Options.Ordering.Func.KBOalt -> 
    begin match term_weight with
    | Options.Ordering.Weight.Default 
    | Options.Ordering.Weight.Custom {wvar=_;wsym=[]} -> 
      let inc_criteria = ordering == Options.Ordering.Func.KBOalt in
      let weight = Term.get_num_of_symb in
      KBO.make ~with_var:true ~inc_criteria ~weight ~symb_ordering ()
    | Options.Ordering.Weight.Custom {wvar;wsym} -> 
      let wsym = 
        (* TODO validate weight function wrt. symbol ordering *)
        let map = 
          List.fold_left (fun map (s,w) ->
            SMap.add (find_symbol' s) w map
          ) SMap.empty wsym
        in
        fun x ->          
          Option.O.(SMap.find_opt x map |? wvar)         
      in
      KBO.make_sw ~with_var:true ~wvar ~wsym ~symb_ordering ()
    end
  | Options.Ordering.Func.LPO -> 
    LPO.make ~symb_ordering
