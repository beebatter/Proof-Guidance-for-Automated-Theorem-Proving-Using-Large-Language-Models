open Lib
open Logic_interface



(*----- debug modifiable part-----*)

let dbg_flag = false
    
type dbg_gr = 
  | D_context
  | D_contextsize
  | D_triv
  | D_subs
  | D_sub_subs
  | D_fw_subs
  | D_bw_subs
  | D_fw_subs_res
  | D_bw_subs_res
  | D_fw_sub_subs
  | D_bw_sub_subs
  | D_unit_subs
  (* | D_hybrid_subs *)
  | D_fw_unit_subs
  | D_bw_unit_subs
  (* | D_fw_hybrid_subs
  | D_bw_hybrid_subs *)
  | D_demod
  | D_demod2
  | D_lightnorm
  | D_joinability
  | D_joinability2
  | D_acnorm
  | D_smt
  | D_fw_ac_demod
  | D_fw_ac_demod2
  | D_bw_ac_demod
  | D_cache_sim
  | D_fw_gjoin
  | D_fw_gjoin2
  | D_fw_gjoin3
  | D_fw_connect
  | D_fw_connect2
  | D_bw_gjoin

let dbg_gr_to_str = function 
  | D_context -> "context"
  | D_contextsize -> "contextsize"
  | D_triv -> "triv"
  | D_subs -> "subs"
  | D_sub_subs -> "sub_subs"
  | D_fw_subs -> "fw_subs"
  | D_bw_subs -> "bw_subs"
  | D_fw_subs_res -> "fw_subs_res"
  | D_bw_subs_res -> "bw_subs_res"
  | D_fw_sub_subs -> "fw_sub_subs"
  | D_bw_sub_subs -> "bw_sub_subs"
  | D_unit_subs -> "unit_subs"
  (* | D_hybrid_subs -> "hybrid_subs" *)
  | D_fw_unit_subs -> "fw_unit_subs"
  | D_bw_unit_subs -> "bw_unit_subs"
  (* | D_fw_hybrid_subs -> "fw_hybrid_subs"
  | D_bw_hybrid_subs -> "bw_hybrid_subs" *)
  | D_demod -> "demod"
  | D_demod2 -> "demod"
  | D_lightnorm -> "lightnorm"
  | D_joinability -> "joinability"
  | D_joinability2 -> "joinability"
  | D_acnorm -> "acnorm"
  | D_smt -> "smt"
  | D_fw_ac_demod -> "fw_ac_demod"
  | D_fw_ac_demod2 -> "fw_ac_demod"
  | D_bw_ac_demod -> "bw_ac_demod"
  | D_cache_sim -> "cache_sim"
  | D_fw_gjoin 
  | D_fw_gjoin2 
  | D_fw_gjoin3 -> "fw_gjoin"
  | D_fw_connect 
  | D_fw_connect2 -> "fw_connect"
  | D_bw_gjoin -> "bw_gjoin"

let dbg_groups = [

 (*` D_context; *)
(*
  (* D_contextsize; *)
  D_triv;
  D_subs;

  D_sub_subs;
  D_fw_subs;
  D_bw_subs;
  D_fw_subs_res;
  D_bw_subs_res;
  D_fw_sub_subs;
  D_bw_sub_subs;
  D_unit_subs;
  (* D_hybrid_subs; *)
  D_fw_unit_subs;
  D_bw_unit_subs;
  (* D_fw_hybrid_subs; *)
  (* D_bw_hybrid_subs; *)
 *)
  (*
  D_demod;
  D_demod2;

  
  D_lightnorm;
  
  D_joinability;
  D_joinability2;
  D_acnorm;
*)
  D_smt; 
(*
  D_fw_ac_demod;
  D_bw_ac_demod;
  D_fw_ac_demod2;

  D_cache_sim;
 *)
  (*
  D_fw_gjoin;  
  D_fw_gjoin2;  
  D_fw_gjoin3;
*)
    (*
  D_fw_connect;  
  D_fw_connect2;  
  D_bw_gjoin;  
*)
]
    
let module_name = "simplify_new"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
  
(*----- debug -----*)

(* Small helper function *)
let dbg_inference d_label text ~from ~into ~via = 
  let n = String.length text in
  dbg d_label @@ lazy (sprintf "%-*s %s" n text (Clause.to_string_tptp from));
  dbg d_label @@ lazy (sprintf "%-*s %s" n "to" (match into with Some into -> Clause.to_string_tptp into | None -> "tautology"));
  dbg_env d_label (fun () ->
    match via with
    | [] -> ()
    | [x] -> 
      dbg d_label @@ lazy (sprintf "%-*s %s" n "via" (Clause.to_string_tptp x));
    | hd::tl -> 
      dbg d_label @@ lazy (sprintf "%-*s %s" n "via" (Clause.to_string_tptp hd));
      tl |> List.iter (fun x -> 
        dbg d_label @@ lazy (sprintf "%-*s %s" n "" (Clause.to_string_tptp x));
      )
  );


(* ------- *)
(* Indices *)
(* ------- *)

(* Modules *)
module FwDemodIndex_M = DemodulationIndexPerfect
module BwDemodIndex_M = DemodulationIndexPerfect
module RevDemodIndex_M = DemodulationIndexPerfect
module SubsumptionIndexTop_M = SubsumptionIndex
module SubsumptionIndex_M = SubsumptionIndexTop_M.SCFVIndexNoTrack
module SubsetSubsumptionIndex_M = SubsetSubsume_new
module LightNormIndex_M = LightNormalisationIndex
(* module HybridSubsumptionIndex_M = HybridSubsumptionIndex *)
module UnitSubsumptionIndex_M = UnitSubsumptionIndex
module ACDemodIndex_M = ACDemodulationIndex

(* Index tag *)
module Index_tag = struct
  (* Types of index *)
  type t = 
    | FwDemod
    | BwDemod
    (* | Subs *)
    | SubsetSubs
    | LightNorm
    | LightNormNoreduce
    (* | ProbProps *)
    | SMTIncr
    | SMTSet
    | UnitSubs
    | NonunitSubs
    (* | HybridSubs *)
    | FwACDemod
    | BwACDemod
    | RevDemod
    | BwGjoin

  let to_string = function
    | FwDemod -> "FwDemod"
    | BwDemod -> "BwDemod"
    (* | Subs -> "Subs" *)
    | SubsetSubs -> "SubsetSubs"
    | LightNorm -> "LightNorm"
    | LightNormNoreduce -> "LightNormNoreduce"
    (* | ProbProps -> "ProbProps" *)
    | SMTIncr -> "SMTIncr"
    | SMTSet -> "SMTSet"
    | UnitSubs -> "UnitSubs"
    | NonunitSubs -> "NonunitSubs"
    (* | HybridSubs -> "HybridSubs" *)
    | FwACDemod -> "FwACDemod"
    | BwACDemod -> "BwACDemod"
    | RevDemod -> "RevDemod"
    | BwGjoin -> "BwGjoin"

  let to_int = function
    | FwDemod -> 1
    | BwDemod -> 2
    (* | Subs -> 3 *)
    | SubsetSubs -> 3
    | LightNorm -> 4
    | LightNormNoreduce -> 5
    (* | ProbProps -> 6 *)
    | SMTIncr -> 6
    | SMTSet -> 7
    | UnitSubs -> 8
    | NonunitSubs -> 9
    (* | HybridSubs -> 10 *)
    | FwACDemod -> 10
    | BwACDemod -> 11
    | RevDemod -> 12
    | BwGjoin -> 13

  let of_int = function
    | 1 -> FwDemod
    | 2 -> BwDemod
    (* | 3 -> Subs *)
    | 3 -> SubsetSubs
    | 4 -> LightNorm
    | 5 -> LightNormNoreduce
    (* | 6 -> ProbProps *)
    | 6 -> SMTIncr
    | 7 -> SMTSet
    | 8 -> UnitSubs
    | 9 -> NonunitSubs
    (* | 10 -> HybridSubs *)
    | 10 -> FwACDemod
    | 11 -> BwACDemod
    | 12 -> RevDemod
    | 13 -> BwGjoin
    | x -> invalid_arg (sprintf "Simplify.Index_tag.of_int: %d is not in range" x)

  let list = [
    FwDemod;
    BwDemod;
    (* Subs; *)
    SubsetSubs;
    LightNorm;
    LightNormNoreduce;
    (* ProbProps; *)
    SMTIncr;
    SMTSet;
    UnitSubs;
    NonunitSubs;
    (* HybridSubs; *)
    FwACDemod;
    BwACDemod;
    RevDemod;
    BwGjoin;
  ]

  let iter f = 
    f 1 FwDemod;
    f 2 BwDemod;
    (* Subs; *)
    f 3 SubsetSubs;
    f 4 LightNorm;
    f 5 LightNormNoreduce;
    (* ProbProps; *)
    f 6 SMTIncr;
    f 7 SMTSet;
    f 8 UnitSubs;
    f 9 NonunitSubs;
    (* HybridSubs; *)
    f 10 FwACDemod;
    f 11 BwACDemod;
    f 12 RevDemod;
    f 13 BwGjoin;
    ()

  let () = dassert (fun () ->
    list |> List.iter (fun x -> dassert (fun () -> x == (x |> to_int |> of_int)));
    iter (fun i x -> dassert (fun () -> i == to_int x && of_int i == x));
    true
  )
end

(* (* Forward declaration of Set is needed here (just types though) *)
module type Set = sig
  module Context : sig
    type elt = {
      clause: clause;
      param: Bit_vec.bit_vec;
    }

    type t = elt BCHtbl.t
  end

  type t = {
    (* Indices *)
    fw_demod_index: FwDemodIndex_M.t;
    bw_demod_index: BwDemodIndex_M.t;
    subsumption_index: SubsumptionIndex_M.index;
    subset_subsumption_index: SubsetSubsumptionIndex_M.index;

    (* Context *)
    context: Context.t;
  }
end *)

(* module Index *)



(* ------------------ *)
(* Simplification set *)
(* ------------------ *)

(* Options *)
type context_data_structure = ContextMap | ContextTbl

type options = {
  order: ordering;
  demod_completeness_check: Options.Demod_check.t;
  eq_types: Symbol.sym_set;
  demod_use_ground: bool;
  cache_sim: Options.SupSimplificationSetup.CacheSim.t;
  ac_symbols: AC.Table.t (* ref *);
  smt_check_interval: int;
  bw_gjoin_interval: int;
  subs_bck_mult: int;
  context_data_structure: context_data_structure;
}

module Set = struct
  module Context = struct
    type elt = {
      clause: clause;
      param: Bit_vec.bit_vec;
    }

    module Map = struct
      (* Flexible, but slightly inefficient? One extra boxing = one extra pattern matching and pointer chasing on every access. *)
      type t = 
        | Map of (elt BCMap.t) ref
        | Tbl of elt BCHtbl.t

      let empty tag = 
        match tag with
        | ContextMap -> Map (ref BCMap.empty)
        | ContextTbl -> Tbl (BCHtbl.create 50021)

      let clear map = 
        match map with
        | Map m -> m := BCMap.empty
        | Tbl m -> BCHtbl.clear m

      let add map x y = 
        match map with
        | Map m -> m @= BCMap.add x y
        | Tbl m -> BCHtbl.add m x y

      let remove map x = 
        match map with
        | Map m -> m @= BCMap.remove x
        | Tbl m -> BCHtbl.remove m x

      let update map x f = 
        match map with
        | Map m -> m @= BCMap.update x f
        | Tbl m -> 
          let y = BCHtbl.find_opt m x in
          begin match y with
          | None -> 
            let y' = f y in
            begin match y' with
            | None -> 
              ()
            | Some z' -> 
              BCHtbl.add m x z'
            end
          | Some z -> 
            let y' = f y in
            begin match y' with
            | None -> 
              BCHtbl.remove m x
            | Some z' -> 
              if z == z' then
                ()
              else
                BCHtbl.replace m x z'
            end
          end

      let mem map x = 
        match map with
        | Map m -> BCMap.mem x !m
        | Tbl m -> BCHtbl.mem m x

      let find map x = 
        match map with
        | Map m -> BCMap.find x !m
        | Tbl m -> BCHtbl.find m x

      let find_opt map x = 
        match map with
        | Map m -> BCMap.find_opt x !m
        | Tbl m -> BCHtbl.find_opt m x

      let iter f map = 
        match map with
        | Map m -> BCMap.iter f !m
        | Tbl m -> BCHtbl.iter f m

      let fold f map init = 
        match map with
        | Map m -> BCMap.fold f !m init
        | Tbl m -> BCHtbl.fold f m init

      let size map = 
        match map with
        | Map m -> BCMap.cardinal !m
        | Tbl m -> BCHtbl.length m
    end

    type t = {
      map: Map.t;
      mutable size: int;
      mutable dead: int;
    }

    (* More informative exceptions, for pattern matching *)
    exception Is_mem
    exception Is_dead
    exception Not_mem

    let empty tag = {
      map = Map.empty tag; (* 5323 *) (* 16127 *)
      size = 0;
      dead = 0;
    }

    let clear context = 
      Map.clear context.map;
      context.size <- 0;
      context.dead <- 0;
      ()

    let incr_size context = context.size <- context.size + 1
    let incr_dead context = context.dead <- context.dead + 1

    (* exception Clause_in_index *)

    (* Set the [index] flag on [clause] in context, adding to context if it wasn't there at all. Raises Failure if clause already in index. *)
    let add context index clause = 
      (* [%dbg D_context (sprintf "added %s to %s" (Clause.to_string_tptp clause) (Index_tag.to_string index))]; *)
      let index_n = Index_tag.to_int index in
      let bv_old = 
        match Map.find_opt context.map clause with
        | Some x ->
            if Bit_vec.get index_n x.param then (
              dbg D_context @@ lazy (sprintf "Simplify.Set.add: already in index %s" (Index_tag.to_string index));

              (*KK: shared clauses may be added to SMT sovler from outside, so we need to skip this check in this case *)
              (*
              if Stdlib.(not (index = Index_tag.SMTSet)) then 
                raise Is_mem
              else
                x.param
               *)
              raise Is_mem
              
           )
          else if Bit_vec.get 0 x.param then (
            dbg D_context @@ lazy "Simplify.Set.add: already dead";
            raise Is_dead
          )
          else
            x.param
        | None ->
          Bit_vec.false_vec
      in
      let elt_new = {
        clause = clause; 
        param = Bit_vec.set true index_n bv_old;
      } 
      in
      Map.add context.map clause elt_new;
      incr_size context

    (* Add a new "dead" clause. This is for e.g. demodulation intermediate steps. *)
    let add_dead context clause = 
      Map.update context.map clause (fun x ->
        match x with
        | Some _ ->
          dbg D_context @@ lazy (sprintf "Simplify.Set.add_dead: already in context");
          raise Is_mem
        | None ->
          let elt = {
            clause = clause; 
            param = Bit_vec.false_vec |> Bit_vec.set true 0;
          } 
          in
          Some elt 
      );
      incr_size context;
      incr_dead context

    (* Set the "dead" flag on [clause] in context. Raises Failure if already dead or not in context. *)
    let set_dead context clause = 
      Map.update context.map clause (fun y ->
        let elt_old = 
          match y with
          | Some x when Bit_vec.get 0 x.param ->
            dbg D_context @@ lazy "Simplify.Set.set_dead: already dead";
            raise Is_dead
          | Some x ->
            x
          | None ->
            dbg D_context @@ lazy "Simplify.Set.set_dead: not in context";
            raise Not_mem
        in
        let elt_new = {elt_old with 
          param = Bit_vec.set true 0 elt_old.param;
        } 
        in
        Some elt_new
      );
      incr_dead context

    (* let set_dead_and_remove_indices context clause =
      assert false [@@ppwarning "unimplemented"] *)

    (* Check if [index] flag in [context] is set. [true] if yes, [false] if unset or not in context. *)
    let mem context index clause =
      try
        (Map.find context.map clause)
        .param
        |> Bit_vec.get (Index_tag.to_int index)
      with Not_found ->
        false

    (* Check if clause is in context (in any index, that is). *)
    let mem_any context clause =
      Map.mem context.map clause

    (* Check if "dead" flag in [context] is set. [true] if yes, [false] if unset or not in context. *)
    let is_dead context clause =
      try
        (Map.find context.map clause)
        .param
        |> Bit_vec.get 0
      with Not_found ->
        false

    type status = Live | Dead | NotMem
    let status context clause = 
      try
        (Map.find context.map clause)
        .param
        |> Bit_vec.get 0
        |> function true -> Dead | false -> Live
      with Not_found ->
        NotMem


    let list_all context : clause list =
      Map.fold (fun _ {clause} acc ->
        clause :: acc
      ) context.map []

    let list_dead context : clause list =
      Map.fold (fun _ {param;clause} acc ->
        if Bit_vec.get 0 param then
          clause :: acc
        else
          acc
      ) context.map []

    let list_nondead context : clause list =
      Map.fold (fun _ {param;clause} acc ->
        if Bit_vec.get 0 param then
          acc
        else
          clause :: acc
      ) context.map []

    let size context = 
      let res = context.size in
      dassert (fun () -> 
        let res_dbg = Map.size context.map in
        dbg D_contextsize @@ lazy (sprintf "Simplify_new.Set.size: res=%d res_dbg=%d" res res_dbg);
        res = res_dbg || res-1 = res_dbg  (* This is because [add_to_indices] may raise due to SMT unsatisfiability, and therefore context.size will be out of sync with the map *)
      );
      res

    let size_dead context = 
      let res = context.dead in
      dassert (fun () -> 
        let res_dbg = 
          Map.fold (fun _ {param;_} acc ->
            if Bit_vec.get 0 param then
              acc + 1
            else
              acc
          ) context.map 0
        in
        dbg D_contextsize @@ lazy (sprintf "Simplify_new.Set.size_dead: res=%d res_dbg=%d" res res_dbg);
        res = res_dbg
      );
      res

    let size_nondead context = 
      let res = context.size - context.dead in
      dassert (fun () -> 
        let res_dbg = 
          Map.fold (fun _ {param;_} acc ->
            if Bit_vec.get 0 param then
              acc
            else
              acc + 1
          ) context.map 0
        in
        dbg D_contextsize @@ lazy (sprintf "Simplify_new.Set.size_nondead: res=%d res_dbg=%d" res res_dbg);
        res = res_dbg || res-1 = res_dbg  (* This is because [add_to_indices] may raise due to SMT unsatisfiability, and therefore context.size will be out of sync with the map *)
      );
      res


    (* Iterative interface *)
    let iter (f: clause -> bool -> unit) context = 
      context.map |> Map.iter (fun _ {param;clause} ->
        let is_dead = Bit_vec.get 0 param in
        f clause is_dead
      )


    (* Unset the [index] flag on [clause] in context. Raises Failure if clause already not in index. *)
    let remove context index clause = 
      let index_n = Index_tag.to_int index in
      Map.update context.map clause (fun y ->
        let bv_old = 
          match y with
          | Some x when not @@ Bit_vec.get index_n x.param ->
            dbg D_context @@ lazy (sprintf "Simplify.Set.remove: already not in index %s" (Index_tag.to_string index));
            raise Not_mem
          | Some x ->
            x.param
          | None ->
            dbg D_context @@ lazy (sprintf "Simplify.Set.remove: already not in context %s" (Index_tag.to_string index));
            raise Not_mem
        in
        let elt_new = {
          clause = clause;
          param = Bit_vec.set false index_n bv_old;
        } 
        in
        Some elt_new
      )

    (* let remove_full context clause =
      context.map <- BCMap.remove clause context.map *)

    (* List of indices where clause is in *)
    let indices context clause : Index_tag.t list =
      try
        let bv = (Map.find context.map clause).param in
        Index_tag.list |> List.filter (fun x ->
          Bit_vec.get (Index_tag.to_int x) bv
        )
      with Not_found -> 
        []
  end



  (* Disable for release builds! *)
  let debug_index_flag = false

  (* For efficiency reasons this has to be stored here, and therefore it has 
     to be declared at this point *)
  exception Return_demod_func of (term * (clause * Subst.subst) option)  (* TODO: should not return subst, not needed *)
  let demod_func_nocheck ~order = fun candidates s ->
    try 
      candidates (fun _term subst lst ->
        lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
          (* if Clause.Bc.(clause == eq_clause) then () else *)
          let l,r = Term.Eq.decompose_atom_exn eq_lit in
          let l,r = Term.Eq.regularize_pos pos l r in
          dassert (fun () -> _term == l);
          dbg D_demod @@ lazy (sprintf "func: l: %s r: %s s: %s" (Term.to_string l) (Term.to_string r) (Term.to_string s));
          let result = Inference_rules.demodulation_bare_nomatch ~order eq_lit l r s subst in
          begin match result with
          | Some y -> 
            dbg D_demod @@ lazy (sprintf "Hit %s to %s" (Term.to_string s) (Term.to_string y));
            (* dbg D_demod @@ lazy (sprintf "via %s" (Clause.to_string_tptp eq_clause)); *)
            (* Need also to return lhs of original equation for encompassment ordering *)
            dassert (fun () -> s == subst_apply subst l);
            dassert (fun () -> y == subst_apply subst r);
            raise_notrace @@ Return_demod_func (y, Some (eq_clause, subst))
          | None -> ()
          end
        )
      );
      (s, None)
    with Return_demod_func x -> x

  type t = {
    unique_id: int;

    order: ordering;

    (* Indices *)
    fw_demod_index: FwDemodIndex_M.t;
    bw_demod_index: BwDemodIndex_M.t;
    rev_demod_index: RevDemodIndex_M.t;
    subsumption_index: SubsumptionIndex_M.index;
    subset_subsumption_index: SubsetSubsumptionIndex_M.index;
    lightnorm_index: LightNormIndex_M.t;
    ac_symbols: AC.Table.t (* ref *);
    smt_incremental: SMT_incremental.set;
    smt_set: SMTSolver.problem;
    mutable smt_set_count: int;
    (* hybrid_subs_index: HybridSubsumptionIndex_M.t; *)
    unit_subs_index: UnitSubsumptionIndex_M.t;
    ac_demod_index: ACDemodIndex_M.t;
    bw_gjoin_index: BCSet.t ref;
    mutable bw_gjoin_count: int;
    mutable bw_gjoin_step: int;

    (* Debug indices *)
    fw_demod_index_debug: DemodulationIndex.t;

    (* Funcs *)
    fw_demod_func_nocheck: ((term -> Subst.subst -> (int * lit * clause) list -> unit) -> unit) -> term -> term * (clause * Subst.subst) option;
    (* fw_demod_func: (term * Subst.subst * (int * lit * clause) list) list -> term -> term * (clause * Subst.subst) option; *)

    (* Context and options *)
    context: Context.t;
    options: options;
  }

  exception Is_mem  = Context.Is_mem
  exception Is_dead = Context.Is_dead
  exception Not_mem = Context.Not_mem

  let next_unique_id = ref 0

  let create (options:options) = let order = options.order in {
    unique_id = (let x = !next_unique_id in incr next_unique_id; x);
    order;
    fw_demod_index = FwDemodIndex_M.create ~order ~eq_types:options.eq_types ~use_ground:options.demod_use_ground ~ac_table:options.ac_symbols ();
    bw_demod_index = BwDemodIndex_M.create ~order ~eq_types:options.eq_types ~use_ground:options.demod_use_ground ~ac_table:options.ac_symbols ();
    rev_demod_index = RevDemodIndex_M.create ~order ~eq_types:options.eq_types ~use_ground:options.demod_use_ground ~ac_table:options.ac_symbols ~reverse:true ();
    fw_demod_func_nocheck = demod_func_nocheck ~order;
    subsumption_index = SubsumptionIndex_M.create();
    subset_subsumption_index = SubsetSubsumptionIndex_M.create();
    lightnorm_index = LightNormIndex_M.create ~order ~eq_types:options.eq_types ();
    ac_symbols = options.ac_symbols;
    smt_incremental = SMT_incremental.make_set !GlobalSMT.state;
    smt_set = SMTSolver.make_problem !GlobalSMT.state;
    smt_set_count = 0;
    (* hybrid_subs_index = HybridSubsumptionIndex_M.create(); *)
    unit_subs_index = UnitSubsumptionIndex_M.create();
    ac_demod_index = ACDemodIndex_M.create ~ac_symbols:options.ac_symbols ~order ~subs_bck_mult:options.subs_bck_mult ~complete:(options.demod_completeness_check != Options.Demod_check.Off) ();
    bw_gjoin_index = ref BCSet.empty;
    bw_gjoin_count = options.bw_gjoin_interval;
    bw_gjoin_step = options.bw_gjoin_interval;

    fw_demod_index_debug = DemodulationIndex.create ~order ~eq_types:options.eq_types ~use_ground:options.demod_use_ground ();

    context = Context.empty options.context_data_structure;
    options;
  }

  let clear x = 
    dbg D_context @@ lazy (sprintf "[id %d] clear" (x.unique_id));
    Context.clear x.context;
    FwDemodIndex_M.clear x.fw_demod_index;
    BwDemodIndex_M.clear x.bw_demod_index;
    (* RevDemodIndex_M.clear x.rev_demod_index; *)
    LightNormIndex_M.clear x.lightnorm_index;
    if debug_index_flag then DemodulationIndex.clear x.fw_demod_index_debug;
    (* HybridSubsumptionIndex_M.clear x.hybrid_subs_index; *)
    UnitSubsumptionIndex_M.clear x.unit_subs_index;
    x.bw_gjoin_index := BCSet.empty;
    x.bw_gjoin_count <- x.options.bw_gjoin_interval;
    x.bw_gjoin_step <- x.options.bw_gjoin_interval;
    { 
      x with
      subsumption_index = SubsumptionIndex_M.create();
      subset_subsumption_index = SubsetSubsumptionIndex_M.create();
    }

  let add state index clause =
    dbg D_context @@ lazy (sprintf "[id %d] add %s to %s" (state.unique_id) (Clause.to_string_tptp clause) (Index_tag.to_string index));
    (* state.context <- *) Context.add state.context index clause

  let remove state index clause =
    dbg D_context @@ lazy (sprintf "[id %d] remove %s" (state.unique_id) (Clause.to_string_tptp clause));
    (* state.context <- *) Context.remove state.context index clause

  (* let remove_full state clause =
    dbg D_context @@ lazy (sprintf "[id %d] remove_full %s" (state.unique_id) (Clause.to_string_tptp clause));
    (* state.context <- *) Context.remove_full state.context clause *)

  let add_dead state clause = 
    dbg D_context @@ lazy (sprintf "[id %d] add %s as dead" (state.unique_id) (Clause.to_string_tptp clause));
    (* state.context <- *) Context.add_dead state.context clause

  let set_dead state clause = 
    dbg D_context @@ lazy (sprintf "[id %d] set_dead %s" (state.unique_id) (Clause.to_string_tptp clause));
    (* state.context <- *) Context.set_dead state.context clause

(*
  let add_dead state clause = 
    state.context <- Context.add_dead state.context clause
*)
  (* let set_dead_and_remove state clause =
    set_dead *)

  let is_dead state clause =
    let res = Context.is_dead state.context clause in
    dbg D_context @@ lazy (sprintf "[id %d] is_dead %s = %B" (state.unique_id) (Clause.to_string_tptp clause) (res));
    res

  type status = Context.status = Live | Dead | NotMem
  let status state clause =
    Context.status state.context clause

  let list_dead state =
    Context.list_dead state.context 

  let list_all state =
    Context.list_all state.context 

  let list_nondead state = 
    Context.list_nondead state.context

  let list_nondead state = 
    Context.list_nondead state.context

  let size state = 
    Context.size state.context

  let size_dead state = 
    Context.size_dead state.context

  let size_nondead state = 
    Context.size_nondead state.context

  let iter f state = 
    Context.iter f state.context

  let mem state index clause =
    let res = Context.mem state.context index clause in
    dbg D_context @@ lazy (sprintf "[id %d] mem %s = %B" (state.unique_id) (Clause.to_string_tptp clause) (res));
    res

  let mem_any state clause = 
    let res = Context.mem_any state.context clause in
    dbg D_context @@ lazy (sprintf "[id %d] mem_any %s = %B" (state.unique_id) (Clause.to_string_tptp clause) (res));
    res

  let indices state clause = 
    Context.indices state.context clause

  let unique_id state = 
    state.unique_id
end

type set = Set.t



(* ------- *)
(* Indexes *)
(* ------- *)

(* Unified interface for indices *)
module type Index = sig
  (* Type of index that this rule reads/writes to *)
  val index : Index_tag.t
  (* Adds clause to index in set *)
  val add : Set.t -> clause -> unit
  (* Removes clause from index in set *)
  val remove : Set.t -> clause -> unit
  (* (* Checks if clause is in set *)
  val mem : Set.t -> clause -> bool *)
end



(* Exported modules handle adding to context *)
module MakeIndex (Index: Index) : sig include Index val add' : set -> clause -> unit val remove' : set -> clause -> unit end = struct
  let index = Index.index

  let add' = Index.add

  let remove' = Index.remove

  let add state clause = 
    (* undefined () *)
    (* dbg D_triv @@ lazy "warning: add"; *)
    Index.add state clause;
    Set.add state index clause

  let remove state clause = 
    (* undefined () *)
    (* dbg D_triv @@ lazy "warning: remove"; *)
    Index.remove state clause;
    Set.remove state index clause
end

module NonunitSubsIndex = MakeIndex(struct
  let index = Index_tag.NonunitSubs

  (* Add to / remove from fv index *)
  let add_fv state clause = 
    (* let feature_list = (get_feature_list clause) in *)
    (* let com_feature_list = compress_feature_list feature_list in *)
    dbg D_subs @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    SubsumptionIndex_M.add_clause Set.(state.subsumption_index) clause

  let remove_fv state clause =
    dbg D_subs @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    (* try with Not_found -> () *)
    SubsumptionIndex_M.remove_clause Set.(state.subsumption_index) clause

  let add state clause = 
    if not @@ Clause.is_unit clause then add_fv state clause

  let remove state clause = 
    if not @@ Clause.is_unit clause then remove_fv state clause
end)

module UnitSubsIndex = MakeIndex(struct
  let index = Index_tag.UnitSubs

  let add state clause = 
    dbg D_unit_subs @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    UnitSubsumptionIndex_M.add_clause Set.(state.unit_subs_index) clause

  let remove state clause =
    dbg D_unit_subs @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    UnitSubsumptionIndex_M.remove_clause Set.(state.unit_subs_index) clause
end)

(* Add to unit index, and if nonunit add also to fv *)
(* module SubsumptionIndex = struct
  let index = unit  (* Depends on two indices... *)

  let add state clause = 
    if Clause.is_unit clause then (
      UnitSubsIndex.add state clause;
    ) else (
      UnitSubsIndex.add state clause;
      FVSubsumptionIndex.add state clause;
    )

  let remove state clause = 
    if Clause.is_unit clause then (
      UnitSubsIndex.remove state clause;
    ) else (
      UnitSubsIndex.remove state clause;
      FVSubsumptionIndex.remove state clause;
    )
end *)



module SubsetSubsumptionIndex = MakeIndex(struct
  let index = Index_tag.SubsetSubs

  let add state clause = 
    dbg D_sub_subs @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    dbg_env D_sub_subs (fun () ->
      try 
        let by_clause = SubsetSubsumptionIndex_M.is_subsumed Set.(state.subset_subsumption_index) clause in
        dbg D_fw_sub_subs @@ lazy (sprintf "add subsumed: %s by: %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp by_clause));
      with Not_found -> ()
    );
    dbg_env D_sub_subs (fun () ->
      try
        let subsumed_clauses = 
          SubsetSubsumptionIndex_M.find_subsumed Set.(state.subset_subsumption_index) clause
          |> List.X.remove_all ~eq:Clause.Bc.equal clause 
        in
        subsumed_clauses |> List.iter (fun x -> 
          dbg D_bw_sub_subs @@ lazy (sprintf "add subsumed: %s by: %s" (Clause.to_string_tptp x) (Clause.to_string_tptp clause));
        )
      with SubsetSubsumptionIndex_M.No_subsumed -> ()
    );
    SubsetSubsumptionIndex_M.add_clause Set.(state.subset_subsumption_index) clause

  let remove state clause =
    dbg D_sub_subs @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    SubsetSubsumptionIndex_M.remove Set.(state.subset_subsumption_index) clause
end)



module FwDemodIndex = MakeIndex(struct
  let index = Index_tag.FwDemod

  (** Add to list of unit equations *)
  let add state clause =
    dbg D_demod @@ lazy (sprintf "fw add: %s" (Clause.to_string_tptp clause));
    let added = FwDemodIndex_M.add_equation Set.(state.fw_demod_index) clause in
    if added then state.bw_gjoin_count <- state.bw_gjoin_count - 1;
    if Set.debug_index_flag then DemodulationIndex.add_equation Set.(state.fw_demod_index_debug) clause

  let remove state clause =
    dbg D_demod @@ lazy (sprintf "fw remove: %s" (Clause.to_string_tptp clause));
    try 
      FwDemodIndex_M.elim_equation Set.(state.fw_demod_index) clause;
      if Set.debug_index_flag then DemodulationIndex.elim_equation Set.(state.fw_demod_index_debug) clause
    with Not_found -> ()
end)

module BwDemodIndex = MakeIndex(struct
  let index = Index_tag.BwDemod

  (** Add to list of bwd clauses *)
  let add state clause =
    dbg D_demod @@ lazy (sprintf "bw add: %s" (Clause.to_string_tptp clause));
    BwDemodIndex_M.add_bwd_clause Set.(state.bw_demod_index) clause

  let remove state clause =
    dbg D_demod @@ lazy (sprintf "bw remove: %s" (Clause.to_string_tptp clause));
    try 
      BwDemodIndex_M.elim_bwd_clause Set.(state.bw_demod_index) clause;
    with Not_found -> ()
end)

module RevDemodIndex = MakeIndex(struct
  let index = Index_tag.RevDemod

  (** Add to list of unit equations *)
  let add state clause =
    dbg D_demod @@ lazy (sprintf "rev add: %s" (Clause.to_string_tptp clause));
    (* RevDemodIndex_M.add_equation Set.(state.rev_demod_index) clause *) assert false

  let remove state clause =
    dbg D_demod @@ lazy (sprintf "rev remove: %s" (Clause.to_string_tptp clause));
    try 
      (* RevDemodIndex_M.elim_equation Set.(state.rev_demod_index) clause *) assert false
    with Not_found -> ()
end)

module BwGjoinIndex = MakeIndex(struct
  let index = Index_tag.BwGjoin

  let add state clause =
    dbg D_bw_gjoin @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    Set.(state.bw_gjoin_index) @= BCSet.add clause

  let remove state clause =
    dbg D_bw_gjoin @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    Set.(state.bw_gjoin_index) @= BCSet.remove clause
end)

module LightNormIndex = MakeIndex(struct
  let index = Index_tag.LightNorm

  let add state clause =
    dbg D_lightnorm @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    LightNormIndex_M.add_and_update Set.(state.lightnorm_index) clause

  let remove state clause =
    dbg D_lightnorm @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    try LightNormIndex_M.remove Set.(state.lightnorm_index) clause with Not_found -> ()
end)

module LightNormIndexNoreduce = MakeIndex(struct
  (* include LightNormIndex *)
  let index = Index_tag.LightNormNoreduce

  let add state clause =
    dbg D_lightnorm @@ lazy (sprintf "add: %s (noreduce)" (Clause.to_string_tptp clause));
    LightNormIndex_M.add_bare Set.(state.lightnorm_index) clause

  let remove state clause =
    dbg D_lightnorm @@ lazy (sprintf "remove: %s (noreduce)" (Clause.to_string_tptp clause));
    try LightNormIndex_M.remove Set.(state.lightnorm_index) clause with Not_found -> ()
end)

module SMTIncrIndex = MakeIndex(struct
  let index = Index_tag.SMTIncr

  let add state clause =
    try 
      dbg D_smt @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
      let unsat = SMT_incremental.add Set.(state.smt_incremental) clause in
      if unsat then (
        (* Currently, when we find SMT contradiction, we just dump the non-dead clauses and let the UC solver find an unsat core. This may be refined in the future *)
        raise @@ Unsatisfiable_gr_smt_na (Set.list_nondead state)
       )
    with
    | Set.Is_mem | Set.Is_dead -> ()
   
  let remove state clause =
    dbg D_smt @@ lazy (sprintf "[UNIMPLEMENTED] remove: %s" (Clause.to_string_tptp clause));
    ()
end)

module SMTSetIndex = MakeIndex(struct
  let index = Index_tag.SMTSet

  let add state clause =
(*    try *)
      dbg D_smt @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
      SMTSolver.add Set.(state.smt_set) (SMTSolver.clause_to_smt !GlobalSMT.state clause);
      state.smt_set_count <- state.smt_set_count + 1;
      dassert (fun () -> state.options.smt_check_interval >= 0);
      if state.smt_set_count > state.options.smt_check_interval then (
        dbg D_smt @@ lazy (sprintf "smt_interval_check");
        begin match SMTSolver.check state.smt_set with
        | SMTSolver.Unsat -> raise @@ Unsatisfiable_gr_smt_na (Set.list_all state);
        | SMTSolver.Sat | SMTSolver.Unknown -> ()
        end;
        state.smt_set_count <- state.smt_set_count - state.options.smt_check_interval;
       )
          (*
    with
         (*KK: shared clauses may be added to SMT sovler from outside, so we need to skip this check in this case *)
    | Set.Is_mem | Set.Is_dead -> ()
          *)
  let remove state clause =
    dbg D_smt @@ lazy (sprintf "[UNIMPLEMENTED] remove: %s" (Clause.to_string_tptp clause));
    ()
end)

    (* KK *)
    
    (* add clause to SMT outside from the indexes;
       this is used when we add shared clauses for outside;
       we can not add to indexes as this would lead to incompleteness 
     *)
    
let smt_add_outside_clause state clause =
  Set.(state.smt_set_count <- state.smt_set_count + 1);
  SMTSolver.add Set.(state.smt_set) (SMTSolver.clause_to_smt !GlobalSMT.state clause)
  
    
(* module HybridSubsIndex = struct
  let index = Index_tag.HybridSubs

  let add state clause = 
    dbg D_hybrid_subs @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    HybridSubsumptionIndex_M.add_clause Set.(state.hybrid_subs_index) clause

  let remove state clause =
    dbg D_hybrid_subs @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    HybridSubsumptionIndex_M.remove_clause Set.(state.hybrid_subs_index) clause
end *)

module FwACDemodIndex = MakeIndex(struct
  let index = Index_tag.FwACDemod

  let add (state: Set.t) clause =
    dbg D_fw_ac_demod @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    ACDemodIndex_M.add_equation Set.(state.ac_demod_index) clause

  let remove state clause =
    dbg D_fw_ac_demod @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    ACDemodIndex_M.elim_equation Set.(state.ac_demod_index) clause
end)

module BwACDemodIndex = MakeIndex(struct
  let index = Index_tag.BwACDemod

  let add state clause =
    dbg D_bw_ac_demod @@ lazy (sprintf "add: %s" (Clause.to_string_tptp clause));
    ACDemodIndex_M.add_bwd_clause Set.(state.ac_demod_index) clause

  let remove state clause =
    dbg D_bw_ac_demod @@ lazy (sprintf "remove: %s" (Clause.to_string_tptp clause));
    ACDemodIndex_M.elim_bwd_clause Set.(state.ac_demod_index) clause
end)



(* Due to the difficulty in making mutually recursive modules, the "set_dead_and_remove" function has to be kind of outside *)
let set_dead_and_remove (state:Set.t) (clause:clause) : unit =
  dbg D_context @@ lazy (sprintf "[id %d] set_dead_and_remove: %s" (state.unique_id) (Clause.to_string_tptp clause));

  let context = state.context in
  Set.Context.Map.update context.map clause (fun (value: Set.Context.elt option) ->
    let elt_old = 
      match value with
      | Some x when Bit_vec.get 0 x.param ->
        dbg D_context @@ lazy "Simplify.Set.set_dead_and_remove: already dead";
        raise Set.Is_dead
      | Some x ->
        x
      | None ->
        dbg D_context @@ lazy "Simplify.Set.set_dead_and_remove: not in context";
        raise Set.Not_mem
    in

    Index_tag.iter (fun i x ->
      if Bit_vec.get i elt_old.param then (
        match x with
        | FwDemod -> FwDemodIndex.remove' state clause
        | BwDemod -> BwDemodIndex.remove' state clause
        | RevDemod -> RevDemodIndex.remove' state clause
        (* | Subs -> assert false (* FVSubsumptionIndex.remove' state clause *) *)
        | SubsetSubs -> SubsetSubsumptionIndex.remove' state clause
        | LightNorm -> LightNormIndex.remove' state clause
        | LightNormNoreduce -> LightNormIndexNoreduce.remove' state clause
        (* | ProbProps -> () *)
        | SMTIncr -> SMTIncrIndex.remove' state clause
        | SMTSet -> SMTSetIndex.remove' state clause
        | UnitSubs -> UnitSubsIndex.remove' state clause
        | NonunitSubs -> NonunitSubsIndex.remove' state clause
        (* | HybridSubs -> HybridSubsIndex.remove' state clause *)
        | FwACDemod -> FwACDemodIndex.remove' state clause
        | BwACDemod -> BwACDemodIndex.remove' state clause
        | BwGjoin -> BwGjoinIndex.remove' state clause
      )
    );

    let elt_new = {elt_old with 
      param = Bit_vec.false_vec |> Bit_vec.set true 0;
    } 
    in
    Some elt_new
  );
  Set.Context.incr_dead context

(* As above, but if not in index then set dead, and if already dead ignore *)
let force_dead_and_remove (state: Set.t) clause = 
  dbg D_context @@ lazy (sprintf "[id %d] force_dead_and_remove: %s" (state.unique_id) (Clause.to_string_tptp clause));

  let context = state.context in
  Set.Context.Map.update context.map clause (fun (value: Set.Context.elt option) ->
    match value with
    (* Already dead *)
    | Some elt_old when Bit_vec.get 0 elt_old.param ->
      value

    (* Already non-dead *)
    | Some elt_old ->
      Set.Context.incr_dead context;
      Index_tag.iter (fun i x ->
        if Bit_vec.get i elt_old.param then (
          match x with
          | FwDemod -> FwDemodIndex.remove' state clause
          | BwDemod -> BwDemodIndex.remove' state clause
          | RevDemod -> RevDemodIndex.remove' state clause
          (* | Subs -> assert false (* FVSubsumptionIndex.remove' state clause *) *)
          | SubsetSubs -> SubsetSubsumptionIndex.remove' state clause
          | LightNorm -> LightNormIndex.remove' state clause
          | LightNormNoreduce -> LightNormIndexNoreduce.remove' state clause
          (* | ProbProps -> () *)
          | SMTIncr -> SMTIncrIndex.remove' state clause
          | SMTSet -> SMTSetIndex.remove' state clause
          | UnitSubs -> UnitSubsIndex.remove' state clause
          | NonunitSubs -> NonunitSubsIndex.remove' state clause
          (* | HybridSubs -> HybridSubsIndex.remove' state clause *)
          | FwACDemod -> FwACDemodIndex.remove' state clause
          | BwACDemod -> BwACDemodIndex.remove' state clause
          | BwGjoin -> BwGjoinIndex.remove' state clause
        )
      );

      let elt_new = {elt_old with 
        param = Bit_vec.false_vec |> Bit_vec.set true 0;
      } 
      in
      Some elt_new

    (* Not in context *)
    | None ->
      Set.Context.incr_size context;
      Set.Context.incr_dead context;
      let elt_new = Set.Context.{
        clause = clause;
        param = Bit_vec.false_vec |> Bit_vec.set true 0;
      }
      in
      Some elt_new
  )

(* Alternative version, only those indices where it can happen that A is redundant wrt B ∧ B is redundant wrt A *)
let minimal_dead_and_remove (state: Set.t) clause = 
  dbg D_context @@ lazy (sprintf "[id %d] minimal_dead_and_remove: %s" (state.unique_id) (Clause.to_string_tptp clause));

  let context = state.context in
  Set.Context.Map.update context.map clause (fun (value: Set.Context.elt option) ->
    match value with
    (* Already dead *)
    (* | Some elt_old when Bit_vec.get 0 elt_old.param -> 
      value *)

    (* Already non-dead *)
    | Some elt_old ->
      if not (Bit_vec.get 0 elt_old.param) then Set.Context.incr_dead context;
      let param' = ref elt_old.param in
      Index_tag.iter (fun i x ->
        if Bit_vec.get i elt_old.param then (
          match x with
          | SubsetSubs  -> param' @= Bit_vec.set false i; SubsetSubsumptionIndex.remove' state clause
          (* | SMTIncr     -> param' @= Bit_vec.set false i; SMTIncrIndex.remove' state clause *)
          (* | SMTSet      -> param' @= Bit_vec.set false i; SMTSetIndex.remove' state clause *)
          | UnitSubs    -> param' @= Bit_vec.set false i; UnitSubsIndex.remove' state clause
          | NonunitSubs -> param' @= Bit_vec.set false i; NonunitSubsIndex.remove' state clause
          | BwDemod     -> param' @= Bit_vec.set false i; BwDemodIndex.remove' state clause
          | BwACDemod   -> param' @= Bit_vec.set false i; BwACDemodIndex.remove' state clause
          | _ -> ()
        )
      );
      let elt_new = {elt_old with param = !param' |> Bit_vec.set true 0} in
      Some elt_new

    (* Not in context *)
    | None -> None
  )

let add_to_indices (state:Set.t) indices clause =
  dbg D_context @@ lazy (sprintf "[id %d] add_to_indices: %s" (state.unique_id) (Clause.to_string_tptp clause));

  let context = state.context in
  Set.Context.Map.update context.map clause (fun (value:Set.Context.elt option) ->
    let elt_old = 
      match value with
      | Some x ->
        if Bit_vec.get 0 x.param then (
          dbg D_context @@ lazy "Simplify.add_to_indices: already dead";
          raise Set.Is_dead
        ) else (
          x
        )
      | None ->
        Set.Context.incr_size context;
        {param=Bit_vec.false_vec; clause}
    in

    let param_new = indices |> List.fold_left (fun param index ->
      let open Index_tag in
      (* TODO make dassert *)

      (*KK: shared clauses may be added to SMT sovler from outside, so we need to skip this check in this case *)
      if (Bit_vec.get (to_int index) param) (* &&  Stdlib.(not (index = Index_tag.SMTSet) ) *) then (
        (* Allow attempt to add to lightnorm twice, by just ignoring *)
        (* if index = LightNorm then (
          param
        ) else ( *)
        dbg D_context @@ lazy (sprintf "Simplify.add_to_indices: already in index %s" (to_string index));
        raise Set.Is_mem
        (* ) *)
      ) else (
        dbg D_context @@ lazy (sprintf "Adding to %s" (to_string index));
        begin match index with
        | FwDemod -> FwDemodIndex.add' state clause
        | BwDemod -> BwDemodIndex.add' state clause
        | RevDemod -> RevDemodIndex.add' state clause
        (* | Subs -> assert false (* FVSubsumptionIndex.add' state clause *) *)
        | SubsetSubs -> SubsetSubsumptionIndex.add' state clause
        | LightNorm -> LightNormIndex.add' state clause 
        | LightNormNoreduce -> LightNormIndexNoreduce.add' state clause 
        (* | ProbProps -> () *)
        | SMTIncr -> SMTIncrIndex.add' state clause
        | SMTSet -> SMTSetIndex.add' state clause
        | UnitSubs -> UnitSubsIndex.add' state clause
        | NonunitSubs -> NonunitSubsIndex.add' state clause
        (* | HybridSubs -> HybridSubsIndex.add' state clause *)
        | FwACDemod -> FwACDemodIndex.add' state clause
        | BwACDemod -> BwACDemodIndex.add' state clause
        | BwGjoin -> BwGjoinIndex.add' state clause
        end;
        Bit_vec.set true (to_int index) param
      )
    ) elt_old.param
    in

    let elt_new = {elt_old with param = param_new} in
    Some elt_new
  )



(* ---------- *)
(* Interfaces *)
(* ---------- *)

(* Results of simplifications *)

type fw_result =
  (* | Same  (* TODO Is this good to have? *) *)
  | Simplified of clause
  | Eliminated of clause list

module Fw_result = struct
  type t = fw_result

  (* Monadic bind. Useful to succinctly chain simplifications: in [simp1 x >>= simp2]
     if [simp1] returns [Simplified c], the c is fed to [simp2], if it returns 
     [Eliminated parents], the whole expression returns [Eliminated parents] 
     without trying [simp2]. *)
  let (>>=) x f = 
    match x with
    | Simplified c -> f c
    | Eliminated _ -> x

  (* Monadic return, for completeness *)
  let return x = Simplified x

  let rec fold l x =
    match l with
    | [] -> Simplified x
    | [hd] -> hd x
    | hd::tl -> 
      let result = hd x in
      begin match result with
      | Simplified x' -> 
        fold tl x'
      | Eliminated _ -> 
        result
      end

  let to_string_tptp = function
    | Simplified c -> Clause.to_string_tptp c
    | Eliminated _ -> "eliminated"

  (* let same a b =
    match a,b with
    | Simplified x, Simplified y -> x == y
    | _, _ -> false *)

  (* let equal ~eq a b =
    match a,b with
    | Simplified x, Simplified y -> eq x y
    | Eliminated x, Eliminated y -> BCSet.equal (BCSet.of_list x) (BCSet.of_list y)
    | Simplified _, Eliminated _ -> false
    | Eliminated _, Simplified _ -> false *)

  module O = struct 
    (* let (==) = 
      equal ~eq:(==)

    let (!=) a b = 
      not (a == b) *)

    let (>>=) = (>>=)

    let ( let* ) = (>>=)
  end



  (* Run until fixpoint *)
  (* Crude version *)
  let rec fix_point f x = 
    let result = f x in
    match result with
    | Simplified x' -> 
      if x' != x then fix_point f x' else result
    | Eliminated _ -> 
      result



  (** Wrapper function to help cache repeated simplifications *)
  let cache_sim (index: set) (f: clause -> t) = fun c ->
    dbg D_cache_sim @@ lazy (sprintf "cache_sim [id %d]" (Set.unique_id index));
    (* Apply simplification *)
    let c' = f c in
    (* dbg D_trace @@ lazy (sprintf "cache_sim: c: %s c': %s " (Clause.to_string_tptp c) (Clause.to_string_tptp c')); *)
    begin match c' with
    (* If clause was deleted, then we can cache it as dead to avoid repeated 
       work in simplificaiton. If it was already in the index, then it was 
       already dead. *)
    | Eliminated parents when not (List.X.mem ~eq:Clause.equal_bc c parents)-> 
      dbg D_cache_sim @@ lazy (sprintf "cache_sim: E %s : cached as dead (was %s)" 
        (Clause.to_string_tptp c) Set.(match status index c with Live -> "live" | Dead -> "dead" | NotMem -> "not mem")
      );
      force_dead_and_remove index c

    (* If clause clause was simplified from c to c', then we can cache c as 
       dead, so that if we come across c again we save the work of doing the 
       same simplification steps again to get c' and find that it is already 
       in the index. However, if c was already in the index  *)
    | Simplified c' when Clause.Bc.(c' != c) -> 
      dbg D_cache_sim @@ lazy (sprintf "cache_sim: S %s to %s: cached as dead (was %s)" 
        (Clause.to_string_tptp c) (Clause.to_string_tptp c') Set.(match status index c with Live -> "live" | Dead -> "dead" | NotMem -> "not mem")
      );
      force_dead_and_remove index c;

    (* Clause remained the same. Nothing to cache. *)
    | _ -> 
      dbg_env D_cache_sim (fun () -> 
        match c' with
        | Eliminated _ -> dbg D_cache_sim @@ lazy (sprintf "cache_sim: I (eliminated by itself)")
        | Simplified _ -> dbg D_cache_sim @@ lazy (sprintf "cache_sim: I (no simplification)")
      );
    end;
    c'

  (** Wrapper function to help cache repeated simplifications *)
  let elim_sim (index: set) (f: clause -> t) = fun c ->
    dbg D_cache_sim @@ lazy (sprintf "elim_sim [id %d]" (Set.unique_id index));
    (* Apply simplification *)
    let c' = f c in
    begin match c' with
    (* If clause was deleted, then we can cache it as dead to avoid repeated 
       work in simplificaiton. If it was already in the index, then it was 
       already dead. *)
    | Eliminated parents when not (List.X.mem ~eq:Clause.equal_bc c parents) -> 
      dbg D_cache_sim @@ lazy (sprintf "elim_sim: E %s" (Clause.to_string_tptp c));
      minimal_dead_and_remove index c

    (* Clause remained the same. Nothing to cache. *)
    | _ -> 
      dbg_env D_cache_sim (fun () -> 
        match c' with
        | Eliminated _ -> dbg D_cache_sim @@ lazy (sprintf "elim_sim: I (eliminated by itself)")
        | Simplified c' -> 
          if Clause.Bc.(c' != c) then 
            dbg D_cache_sim @@ lazy (sprintf "elim_sim: S %s to %s" (Clause.to_string_tptp c) (Clause.to_string_tptp c'))
          else
            dbg D_cache_sim @@ lazy (sprintf "elim_sim: I (no simplification)")
      );
    end;
    c'
end



(* More fine grained control: clause [from] is to be replaced by [add]. If 
   we have a big list of added clauses and removed clauses we don't know
   which corresponds to which. *)
type bw_result_elt = 
  | BwSimplified of {from: clause; into: clause}
  | BwEliminated of clause  (* TODO: put parents here as well *)

type bw_result = bw_result_elt list

module Bw_result = struct
  type elt = bw_result_elt

  type t = bw_result

  let empty = []

  let (@) = (@)

  (* let rec fold l x =
    let rec f acc l x =
      match l with
      | [] -> acc
      | hd::tl -> 
        let result : t = hd x in
        let acc' : t = result @ acc in
        f acc' tl x
    in 
    match l with
    | [] -> {add=[]; remove=[]}
    | hd::tl -> 
      let result = hd x in
      f result tl x *)

  (* let map fs x = 
    List.map (fun f -> f x) fs *)

  let fold funcs clause = 
    List.concat_map (fun f -> f clause) funcs

  (* let to_string_tptp {add; remove} =
    sprintf "{\n  add" *)

  module O = struct
    let (@) = (@)
  end

  let handle_elt ~remove ~add x = 
    match x with
    | BwSimplified {from; into} -> 
      remove from; add into
    | BwEliminated c -> 
      remove c

  let handle ~remove ~add xs = 
    List.iter (handle_elt ~remove ~add) xs
end



(* type fwbw_result = {
  fw_result: fw_result;
  bw_result: bw_result;
} *)



(* Trivial rule, with no index *)
module type TrivRule = sig
  val simplify : clause -> fw_result
end

(* Fw simplification rule *)
module type FwRule = sig
  (* include Index *)
  (* val index : Index_tag.t *)
  val simplify : Set.t -> clause -> fw_result
end

(* Bw simplification rule *)
module type BwRule = sig
  (* include Index *)
  (* val index : Index_tag.t *)
  val simplify : Set.t -> clause -> bw_result
end

(* Fw and bw simplification rule *)
(* module type FwBwRule = sig
  include Rule
  val simplify_fw : Set.t -> clause -> fw_result
  val simplify_bw : Set.t -> clause -> bw_result
end *)

(* The "special" case of subset subsumption. Fw and bw simplification rule
   that triggers automatically on adding. *)
(* module type AutoFwBwRule = sig
  val add_and_simplify : set -> clause -> (fw_result * bw_result)
  val remove : set -> clause -> unit  (* TODO Also may need to do bw simp? *)
end *)



(* -------------------- *)
(* Simplification Rules *)
(* -------------------- *)

(* module EqResolution : TrivRule = struct
  let simplify clause =
    let clause' = Inference_rules.unflatten_clause clause in
    if clause' == clause then (
      Simplified clause'
    ) else (
      (* incr_int_stat 1 res_num_eq_res_simplified; *)
      Statistics.(bump_int_stat sim_eq_res);
      dbg D_triv @@ lazy (sprintf "EqResolution: %s" (Clause.to_string_tptp clause));
      Simplified clause'
    )
end *)
 
module EqResolutionSimp : TrivRule = struct
  let simplify clause =
    Statistics.(time sim_time_eq_res_simp) @@ fun () ->

    let clause' = Inference_rules.equality_resolution_simp clause in
    if clause' != clause then (
      dassert (fun () -> Clause.Bc.(clause' != clause));
      (* incr_int_stat 1 res_num_eq_res_simplified; *)
      Statistics.(bump_int_stat sim_eq_res_simp);
      dbg D_triv @@ lazy (sprintf "EqResolutionSimp: %s" (Clause.to_string_tptp clause));
    );
    Simplified clause'
end



module PropSubs : TrivRule = struct
  let simplify clause =
    let clause' = Prop_solver_exchange.prop_subsumption clause in
    if clause' != clause then (
      (* incr_int_stat 1 res_num_eq_res_simplified; *)
      dbg D_triv @@ lazy (sprintf "PropSubs: %s new: %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp clause'));
      dassert (fun () -> Clause.length clause' < Clause.length clause);
     );
    Simplified clause'
end

module SMTSubs : FwRule = struct
  include SMTIncrIndex

  let simplify state clause =
    Statistics.(time sim_time_smt_subs) @@ fun () ->

    let clause' = SMT_incremental.global_smt_subsumption Set.(state.smt_incremental) clause in
    if clause' != clause then (
      dbg D_smt @@ lazy (sprintf "SMTSubs: %s" (Clause.to_string_tptp clause));
      dbg D_smt @@ lazy (sprintf "to:      %s" (Clause.to_string_tptp clause'));
      Statistics.(bump_int_stat sim_smt_subsumption);
    );
    Simplified clause'
  let simplify state clause =
    dbg D_smt @@ lazy "smt_subsumption";
    simplify state clause
end

(* module SMTSet : FwRule = struct
  include SMTSetIndex

  let simplify state clause = 
    if Set.(state.c)
end *)



module TautologyElim : TrivRule = struct
  let simplify clause = 
    Statistics.(time sim_time_tautology_del) @@ fun () ->

    if not @@ Inference_rules.is_tautology clause then (
      Simplified clause
    ) else (
      (* incr_int_stat 1 res_tautology_del; *)
      Statistics.(bump_int_stat sim_tautology_del);
      dbg D_triv @@ lazy (sprintf "TautologyElim: %s" (Clause.to_string_tptp clause));
      Eliminated []
    )
end

(* Cannot use with axiomtic equality! Only in preprocessing before eq axioms are added. *)
module EqTautologyElim : TrivRule = struct
  let simplify clause = 
    Statistics.(time sim_time_eq_tautology_del) @@ fun () ->

    if not @@ Inference_rules.is_eq_tautology clause then (
      Simplified clause
    ) else (
      (* incr_int_stat 1 res_tautology_del; *)
      Statistics.(bump_int_stat sim_eq_tautology_del);
      dbg D_triv @@ lazy (sprintf "EqTautologyElim: %s" (Clause.to_string_tptp clause));
      Eliminated []
    )
end

(* Pseudo-rule encapsulates all "cheap" triv simplifications. *)
module TrivRules : TrivRule = struct
  let simplify clause =
    let open Fw_result in
    TautologyElim.simplify clause
    >>= EqTautologyElim.simplify
    >>= EqResolutionSimp.simplify 
end

module Unflattening : TrivRule = struct
  let simplify clause = 
    let clause' = Inference_rules.unflatten_clause ~normalise_eqs:true clause in
    Simplified clause'
end

(* Check if in set *)
(* module CheckMem : FwRule = struct
  let simplify state clause =
    if Set.mem_any state clause 
    (* || Simplify.forward_subset_subsume sim_state clause != clause *)
    then (
      dbg D_trace @@ lazy "[existing] or subsumed";
      Simplify.Eliminated
    ) else (
      Simplify.Simplified clause
    )
end *)

module SMTSimplify : TrivRule = struct
  let sim_term t =
    dbg D_smt @@ lazy (sprintf "sim_term %s" (Term.to_string t));
    let t_smt = SMTSolver.lit_to_smt !GlobalSMT.state t in
    let t_smt' = SMTSolver.simplify_term t_smt in
    if SMTSolver.Term.equal t_smt t_smt' then 
      t
    else 
      Option.O.(SMTSolver.lit_of_smt !GlobalSMT.state t_smt' |? t)
  
  let sim_subterms lit = 
    dbg D_smt @@ lazy "true, sim_subterms";
    let sign, atom = Term.split_sign_lit lit in
    match atom with
    | Term.Fun (sym, args, _) ->
      let args = Term.arg_to_list args in
      let args' = List.map sim_term args in
      if List.X.equal ~eq:(==) args args' then
        lit
      else
        add_lit (sign) (add_fun_term sym args')
    | Term.Var _ -> failwith "simplify_new.ml: sim_subterms: Term.Var"
    
  let[@warning "-8"] sim_lit lit = 
    let lt_i = Symbol.symb_less_int in
    let lt_a = Symbol.symb_less_rat in
    let lt_r = Symbol.symb_less_real in

    let lit' = sim_term lit in
    if lit' != lit then dbg D_smt @@ lazy (sprintf "%s to %s" (Term.to_string lit) (Term.to_string lit'));
    (* if lit' == term_false then lit' else 
    let lit'' = if lit' == term_true then sim_subterms lit else lit' in
    if lit'' != lit' then dbg D_smt @@ lazy (sprintf "%s to %s" (Term.to_string lit') (Term.to_string lit'')); *)
    if lit' == term_false || lit' == term_true then lit' else 
    let lit'' = lit' in
    let sign, atom = Term.split_sign_lit lit'' in
    match atom with
    | Term.Fun (sym, args, _) ->
      if sym == Symbol.symb_less_real then
        lit''
      else if sym == Symbol.symb_lesseq_real then
        let [a;b] = Term.arg_to_list args in
        add_lit (not sign) (add_fun_term lt_r [b;a])
      else if sym == Symbol.symb_greater_real then
        let [a;b] = Term.arg_to_list args in
        add_lit (sign) (add_fun_term lt_r [b;a])
      else if sym == Symbol.symb_greatereq_real then
        add_lit (not sign) (add_fun_term_args lt_r args)
      
      else if sym == Symbol.symb_less_rat then
        lit''
      else if sym == Symbol.symb_lesseq_rat then
        let [a;b] = Term.arg_to_list args in
        add_lit (not sign) (add_fun_term lt_a [b;a])
      else if sym == Symbol.symb_greater_rat then
        let [a;b] = Term.arg_to_list args in
        add_lit (sign) (add_fun_term lt_a [b;a])
      else if sym == Symbol.symb_greatereq_rat then
        add_lit (not sign) (add_fun_term_args lt_a args)
      
      else if sym == Symbol.symb_less_int then
        lit''
      else if sym == Symbol.symb_lesseq_int then
        let [a;b] = Term.arg_to_list args in
        add_lit (not sign) (add_fun_term lt_i [b;a])
      else if sym == Symbol.symb_greater_int then
        let [a;b] = Term.arg_to_list args in
        add_lit (sign) (add_fun_term lt_i [b;a])
      else if sym == Symbol.symb_greatereq_int then
        add_lit (not sign) (add_fun_term_args lt_i args)
      
      else
        lit''
    | Term.Var _ -> assert false
    

  let simplify_v1 clause = 
    dbg D_smt @@ lazy (sprintf "smt_simplify: %s" (Clause.to_string_tptp clause));
    if not !Parser_types.has_ext_theory_names then (
      dbg D_smt @@ lazy (sprintf "no theory in input");
      Simplified clause 
    ) else if Clause.is_ext_axiom clause then (
      dbg D_smt @@ lazy (sprintf "is axiom");
      Simplified clause 
    ) else (
      let clause_smt = SMTSolver.clause_to_smt(* _quant *) !GlobalSMT.state clause in
      let clause_smt' = SMTSolver.simplify_clause clause_smt in
      if SMTSolver.Clause.equal clause_smt clause_smt' then
        Simplified clause
      else
        (* let lits = SMTSolver.clause_of_smt !GlobalSMT.state clause_smt' in *)
        match SMTSolver.clause_of_smt !GlobalSMT.state clause_smt' with
        | Some lits -> 
          let source = Clause.tstp_source_smt_theory_normalisation clause in
          let clause' = create_clause ~normalise_eqs:true source lits in
          dbg D_smt @@ lazy (sprintf "SMT simplified %s" (Clause.to_string_tptp clause));
          if Clause.Bc.(clause' != clause) then (
            Statistics.(bump_int_stat sim_smt_simplified);
            dbg D_smt @@ lazy (sprintf "to             %s" (Clause.to_string_tptp clause'));
            Simplified clause'
          ) else (
            dbg D_smt @@ lazy (sprintf "to             same");
            Simplified clause
          )
        | None -> 
          Statistics.(bump_int_stat sim_smt_simplified);
          dbg D_smt @@ lazy (sprintf "SMT simplified %s" (Clause.to_string_tptp clause));
          dbg D_smt @@ lazy (sprintf "to             tautology");
          Eliminated [clause]
    )

  exception Return
  let simplify_v2 clause = 
    dbg D_smt @@ lazy (sprintf "smt_simplify: %s" (Clause.to_string_tptp clause));
    if not !Parser_types.has_ext_theory_names then (
      dbg D_smt @@ lazy (sprintf "no theory in input");
      Simplified clause 
    ) else (
      let any_changed = ref false in
      let lits = 
        Clause.get_lits clause |> List.filter_map (fun lit -> 
          let lit_smt = SMTSolver.lit_to_smt !GlobalSMT.state lit in
          let lit_smt' = SMTSolver.simplify_term lit_smt in
          if SMTSolver.Term.equal lit_smt lit_smt' then (
            Some lit
          ) else (
            (* dbg D_smt @@ lazy (sprintf "SMT: %s to %s"); *)
            let lit' = Option.O.(SMTSolver.lit_of_smt !GlobalSMT.state lit_smt' |? lit) in
            dbg D_smt @@ lazy (sprintf "%s to %s" (Term.to_string lit) (Term.to_string lit'));
            if lit' == term_true then 
              (* raise_notrace Return *)
              Some lit
            else if lit' == term_false then
              (any_changed:= true; None)
            else
              if Term.get_num_of_symb lit > Term.get_num_of_symb lit' then 
                (any_changed := true; Some lit')
              else 
                Some lit
          )
        )
      in
      if !any_changed then (
        let source = Clause.tstp_source_smt_theory_normalisation clause in
        let clause' = create_clause ~normalise_eqs:true source lits in
        dassert (fun () -> Clause.Bc.(clause' != clause));
        Statistics.(bump_int_stat sim_smt_simplified);
        dbg D_smt @@ lazy (sprintf "SMT simplified %s" (Clause.to_string_tptp clause));
        dbg D_smt @@ lazy (sprintf "to             %s" (Clause.to_string_tptp clause'));
        Simplified clause'
      ) else (
        Simplified clause
      )
    )

  let simplify_v3 ?(complete=true) clause = 
    dbg D_smt @@ lazy (sprintf "smt_simplify: %s" (Clause.to_string_tptp clause));
    if not !Parser_types.has_ext_theory_names then (
      dbg D_smt @@ lazy (sprintf "no theory in input");
      Simplified clause 
    ) else (
      let any_changed = ref false in
      let lits = 
        Clause.get_lits clause |> List.filter_map (fun lit -> 
          let lit' = sim_lit lit in
          if lit' != lit then (
            (* dbg D_smt @@ lazy (sprintf "SMT: %s to %s"); *)
            (* dbg D_smt @@ lazy (sprintf "%s to %s" (Term.to_string lit) (Term.to_string lit')); *)
            if lit' == term_true then 
              (* raise_notrace Return *) Some lit
            else if lit' == term_false then
              (any_changed := true; None)
            else
              if not complete || Term.get_num_of_symb lit > Term.get_num_of_symb lit' then 
                (any_changed := true; Some lit')
              else 
                Some lit
          ) else (
            Some lit
          )
        )
      in
      if !any_changed then (
        let source = Clause.tstp_source_smt_theory_normalisation clause in
        let clause' = create_clause ~normalise_eqs:true source lits in
        dassert (fun () -> Clause.Bc.(clause' != clause));
        Statistics.(bump_int_stat sim_smt_simplified);
        dbg D_smt @@ lazy (sprintf "SMT simplified %s" (Clause.to_string_tptp clause));
        dbg D_smt @@ lazy (sprintf "to             %s" (Clause.to_string_tptp clause'));
        Simplified clause'
      ) else (
        Simplified clause
      )
    )

  let simplify clause = simplify_v3 clause
end





(* -- Subsumption -- *)

module FwSubsumptionPrecond(M : sig val pre_cond : cl_in:clause -> cl_by:clause -> bool end) : FwRule = struct
  (* include NonunitSubsIndex *)

  let forward_subs state clause = 
    Statistics.(time sim_time_fw_subs) @@ fun () ->

    (* do not need light simplifications since light backward *)
    (* assert (state.sim_opt.sim_use_sub_index); *)
    match
      SubsumptionIndex_M.is_subsumed ~subs_bck_mult:Set.(state.options.subs_bck_mult)
        ~pre_cond:M.pre_cond Set.(state.subsumption_index) clause
    with
    | Some (by_cl, _subst) -> 
      Statistics.(bump_int_stat sim_fw_subsumed);
      (* if not (Clause.equal_bc by_cl clause) then set_dead_and_remove state clause; *)
      dbg D_fw_subs @@ lazy (sprintf "%s by %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp by_cl));
      Eliminated [by_cl]
    | None -> 
      Simplified clause

  let simplify state clause = 
    dbg D_fw_subs @@ lazy "fw_subsumption";
    forward_subs state clause

  (* Let's assume that clauses in nonunitsubsindex are also in unitsubsindex. So if this clause is 
     unit we don't even bother checking bw simplification, since we would have already checked it
     with the unit index, more efficiently. Ditto also for the BwSubs, FwSubsRes, BwSubsRes. *)
  let simplify state clause = 
    if Clause.is_unit clause then Simplified clause else simplify state clause
end

module FwSubsumptionNonStrict = FwSubsumptionPrecond(struct 
  let pre_cond_true_fun = BasicSubsumptionIndex.pre_cond_true_fun
  let pre_cond = pre_cond_true_fun 
end)

module FwSubsumption = FwSubsumptionPrecond(struct 
  (* let pre_cond_subs_strict ~cl_in ~cl_by = Clause.length cl_by < Clause.length cl_in  *)
  let pre_cond_subs_strict ~cl_in ~cl_by = cl_by != cl_in 
  let pre_cond = pre_cond_subs_strict 
end)

module BwSubsumption : BwRule = struct
  include NonunitSubsIndex

  let backward_subs_full state clause =
    Statistics.(time sim_time_bw_subs) @@ fun () ->

    (* assert(state.sim_opt.sim_use_sub_index); *)
    (*let was_in_index = SubsumptionIndex_M.in_subs_index Set.(state.subsumption_index) clause in*)
    let was_in_index = Set.mem state NonunitSubsIndex.index clause in
    (* TODO check if this is not unnecessary *)

    let b_subsumed_list =
      SubsumptionIndex_M.find_subsumed ~subs_bck_mult:Set.(state.options.subs_bck_mult) Set.(state.subsumption_index) clause
      |> ListExtra.removeq_all clause  (* TODO should be Clause.Bc.(==) instead? *)
    in
    dassert (fun () -> List.for_all (fun x -> Clause.Bc.(x != clause)) b_subsumed_list);

    (* Set.add state index clause; *)   (* TODO ver *)
    (* Add back the clause itself *)
    if was_in_index then SubsumptionIndex_M.add_clause state.subsumption_index clause;
 
    b_subsumed_list |> List.iter (fun subsumed ->
      Statistics.(bump_int_stat sim_bw_subsumed);
      dbg D_bw_subs @@ lazy (sprintf "%s by %s with Clause.Bc.equal=%B"  (* TODO check *)
        (Clause.to_string_tptp subsumed) (Clause.to_string_tptp clause) (Clause.equal_bc subsumed clause)
      );
      (* Clause already removed by [SubsumptionIndex_M.find_subsumed], mark so in context param *)
      Set.remove state Index_tag.NonunitSubs subsumed;
      set_dead_and_remove state subsumed; 
    );

    b_subsumed_list

  let backward_subs_by_length state length clause =
    if Clause.length clause <= length then
      backward_subs_full state clause
    else 
      []

  let simplify state clause = 
    dbg D_fw_subs @@ lazy "bw_subsumption";
    let remove = backward_subs_full state clause in 
    List.map (fun x -> BwEliminated x) remove

  let simplify state clause = 
    if Clause.is_unit clause then [] else simplify state clause
end

module FwSubsumptionRes : FwRule = struct
  include NonunitSubsIndex

  (* Can raise Unsatisfiable, Eliminated *)
  let forward_subs_res state clause =
    Statistics.(time sim_time_fw_subs_res) @@ fun () ->

    (* let lits = get_lits clause in *)
    let new_lits, parents = SubsumptionIndex_M.fw_subsres ~subs_bck_mult:Set.(state.options.subs_bck_mult)  Set.(state.subsumption_index) clause in
    if List.X.is_nonempty parents then (
      let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause parents in
      let clause' = create_clause ~normalise_eqs:true tstp_source new_lits in
      assert (Clause.Bc.(clause' != clause));
      dbg D_fw_subs_res @@ lazy (sprintf "%s to %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp clause'));
      dbg_env D_fw_subs_res (fun () -> 
        parents |> List.iter (fun x -> dbg D_fw_subs_res @@ lazy (sprintf "by %s" (Clause.to_string_tptp x)))
      );
      Statistics.(incr_int_stat (List.length parents) sim_fw_subsumption_res);
      (* forward; should not assign dead without returing subsumed to the caller *)
      (* set_dead_and_remove state clause; *)
      clause'
    ) else (
      clause
    )

  let simplify state clause =
    dbg D_fw_subs @@ lazy "fw_subsumption_res";
    try
      Simplified (forward_subs_res state clause)
    with Lib.Eliminated ->
      assert false

  let simplify state clause = 
    if Clause.is_unit clause then Simplified clause else simplify state clause
end

module BwSubsumptionRes : BwRule = struct
  include NonunitSubsIndex

  let simplify state clause = 
    dbg D_fw_subs @@ lazy "bw_subsumption_res";
    (* let remove, add = backward_subs_res state clause in *)
    let remove, add = SubsumptionIndex_M.bw_subsres ~subs_bck_mult:Set.(state.options.subs_bck_mult) Set.(state.subsumption_index) clause in
    List.map2 (fun from into_lits -> 
      let tstp_source = Clause.tstp_source_backward_subsumption_resolution from [clause] in
      let into = create_clause ~normalise_eqs:true tstp_source into_lits in
      dbg D_bw_subs_res @@ lazy (sprintf "%s to %s" (Clause.to_string_tptp from) (Clause.to_string_tptp into));
      dbg D_bw_subs_res @@ lazy (sprintf "by %s" (Clause.to_string_tptp clause));
      Statistics.(bump_int_stat sim_bw_subsumption_res);
      (* ALREADY REMOVED FROM SUBS! *)
      (* set_dead_and_remove state from; *)
      (*dassert (fun () -> SubsumptionIndex_M.in_subs_index state.subsumption_index from == false);*)
      (* dassert (fun () -> Set.mem state NonunitSubsIndex.index clause); *)
      (* TODO assert from not in SubsumptionIndex_M index? *)
      Set.remove state Index_tag.NonunitSubs from;
      set_dead_and_remove state from;
      BwSimplified {from; into}
    ) remove add

  let simplify state clause = 
    if Clause.is_unit clause then [] else simplify state clause
end



(****************)
(* Hybrid index *)
(****************)

(* module FwHybridSubs : FwRule = struct
  let index = Index_tag.HybridSubs

  let simplify state clause = 
    dbg D_fw_hybrid_subs @@ lazy "fw_hybrid_subsumption";
    match HybridSubsumptionIndex_M.is_subsumed  ~subs_bck_mult:Set.(state.options.subs_bck_mult) Set.(state.hybrid_subs_index) clause with
    | Some x -> 
      if x != clause then (
        dbg D_fw_hybrid_subs @@ lazy (sprintf "%s to %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp x));
        dbg_env D_fw_hybrid_subs (fun () ->
          Clause.get_parents x |> List.iter (fun c -> if c != clause then dbg D_fw_hybrid_subs @@ lazy (sprintf "by %s" (Clause.to_string_tptp c)))
        )
      );
      Simplified x
    | None -> 
      dbg D_fw_hybrid_subs @@ lazy (sprintf "subsumed %s" (Clause.to_string_tptp clause));
      Eliminated
end

module BwHybridSubs : BwRule = struct
  let index = Index_tag.HybridSubs

  let simplify state clause = 
    dbg D_bw_hybrid_subs @@ lazy "bw_hybrid_subsumption";
    let subs, subs_res = HybridSubsumptionIndex_M.subsumes  ~subs_bck_mult:Set.(state.options.subs_bck_mult) Set.(state.hybrid_subs_index) clause in
    let subs' = subs |> List.map (fun x -> 
      dbg D_bw_hybrid_subs @@ lazy (sprintf "subsumed %s" (Clause.to_string_tptp x));
      Set.remove state Index_tag.HybridSubs x;
      set_dead_and_remove state x;
      BwEliminated x
    ) in
    let subs_res' = subs_res |> List.map (fun (from, into) -> 
      dbg D_bw_hybrid_subs @@ lazy (sprintf "%s to %s" (Clause.to_string_tptp from) (Clause.to_string_tptp into));
      dbg D_bw_hybrid_subs @@ lazy (sprintf "by %s" (Clause.to_string_tptp clause));
      (* ALREADY REMOVED FROM SUBS! *)
      (* set_dead_and_remove state from; *)
      (* dassert (fun () -> SubsumptionIndex_M.in_subs_index state.hybrid_subs_index. from == false); *)
      Set.remove state Index_tag.HybridSubs from;
      set_dead_and_remove state from;
      BwSimplified {from; into}
    ) in
    subs' @ subs_res'
end *)

module FwUnitSubs : FwRule = struct
  let simplify state clause = 
    Statistics.(time sim_time_fw_unit_subs) @@ fun () ->
     
    dbg D_fw_unit_subs @@ lazy "fw_unit_subsumption";
    match UnitSubsumptionIndex_M.is_subsumed Set.(state.unit_subs_index) clause with
    | UnitSubsumptionIndex_M.Nothing ->
      Simplified clause
    | UnitSubsumptionIndex_M.Subs by_clause ->
      dbg D_fw_unit_subs @@ lazy (sprintf "subsumed %s" (Clause.to_string_tptp clause));
      Eliminated [by_clause]
    | UnitSubsumptionIndex_M.SubsRes (parents, lits) ->
      let tstp_source = Clause.tstp_source_forward_subsumption_resolution clause parents in
      let clause' = create_clause ~normalise_eqs:true tstp_source lits in
      Statistics.(bump_int_stat sim_fw_unit_subs);
      dassert (fun () -> Clause.Bc.(clause' != clause));
      dbg D_fw_unit_subs @@ lazy (sprintf "%s to %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp clause'));
      dbg_env D_fw_unit_subs (fun () ->
        parents |> List.iter (fun c -> if c != clause then dbg D_fw_unit_subs @@ lazy (sprintf "by %s" (Clause.to_string_tptp c)))
      );
      Simplified clause'
end

module BwUnitSubs : BwRule = struct
  let simplify state clause = 
    Statistics.(time sim_time_bw_unit_subs) @@ fun () ->
 
    dbg D_bw_unit_subs @@ lazy "bw_unit_subsumption";
    match Clause.get_lits clause with
    | [lit] ->
      let subs, subs_res = UnitSubsumptionIndex_M.subsumes Set.(state.unit_subs_index) lit in
      dassert (fun () -> List.for_all (fun c -> c != clause) subs);
      let subs = subs |> List.map (fun c -> 
        dbg D_bw_unit_subs @@ lazy (sprintf "subsumed %s" (Clause.to_string_tptp c));
        Statistics.(bump_int_stat sim_bw_unit_subs);
        Set.remove state Index_tag.UnitSubs c;
        set_dead_and_remove state c;
        BwEliminated c
      ) in
      let subs_res = subs_res |> List.map (fun (from, into_lits) ->
        let tstp_source = Clause.tstp_source_backward_subsumption_resolution from [clause] in
        let into = create_clause ~normalise_eqs:true tstp_source into_lits in
        dbg D_bw_unit_subs @@ lazy (sprintf "%s to %s" (Clause.to_string_tptp from) (Clause.to_string_tptp into));
        dbg D_bw_unit_subs @@ lazy (sprintf "by %s" (Clause.to_string_tptp clause));
        (* ALREADY REMOVED FROM SUBS! *)
        (* set_dead_and_remove state from; *)
        (* dassert (fun () -> SubsumptionIndex_M.in_subs_index state.unit_subs_index. from == false); *)
        Statistics.(bump_int_stat sim_bw_unit_subs);
        Set.remove state Index_tag.UnitSubs from;
        set_dead_and_remove state from;
        BwSimplified {from; into}
      ) in
      subs @ subs_res
    | _ -> 
      dbg D_bw_unit_subs @@ lazy "not a unit clause";
      []
end

(* module FwFullSubsAndRes : FwRule = struct
  let index = unit

  let simplify state clause = 
    FwUnitSubs.simplify state clause
    >>= FwSubsumption.simplify state
    >>= FwSubsumptionRes.simplify state
end

module BwFullSubsAndRes : BwRule = struct
  let index = unit

  let simplify state clause = 
    BwUnitSubs.simplify state clause
    >>= BwSubsumption.simplify state
    >>= BwSubsumptionRes.simplify state
end *)





(* ------------------ *)
(* Subset Subsumption *)
(* ------------------ *)

module FwSubsetSubsumption : FwRule = struct
  let simplify state clause = 
    Statistics.(time sim_time_fw_subset_subs) @@ fun () ->

    try
      let by_clause = SubsetSubsumptionIndex_M.is_subsumed Set.(state.subset_subsumption_index) clause in
      dbg D_fw_sub_subs @@ lazy (sprintf "subsumed: %s by: %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp by_clause));
      Statistics.(bump_int_stat sim_fw_subset_subsumed);
      Eliminated [by_clause]
    with Not_found -> 
      Simplified clause
  let simplify state clause =
    dbg D_fw_sub_subs @@ lazy "fw_subset_subsumption";
    simplify state clause
end

module BwSubsetSubsumption : BwRule = struct
  let simplify state main_clause =
    Statistics.(time sim_time_bw_subset_subs) @@ fun () ->
    try
      let subsumed_clauses = 
        SubsetSubsumptionIndex_M.find_subsumed Set.(state.subset_subsumption_index) main_clause
        |> List.X.remove_all ~eq:Clause.Bc.equal main_clause
      in
      if List.X.is_nonempty subsumed_clauses then (
        Statistics.(incr_int_stat (List.length subsumed_clauses) sim_bw_subset_subsumed);
        subsumed_clauses |> List.iter (fun x -> 
          dbg D_bw_sub_subs @@ lazy (sprintf "subsumed: %s by: %s" (Clause.to_string_tptp x) (Clause.to_string_tptp main_clause));
          set_dead_and_remove state x
        )
      ) else (
        ()
      );
      subsumed_clauses
    with SubsetSubsumptionIndex_M.No_subsumed -> 
      []
  let simplify state clause =
    dbg D_bw_sub_subs @@ lazy "bw_subset_subsumption";
    let remove = simplify state clause in
    List.map (fun x -> BwEliminated x) remove
end



(* ------------ *)
(* Demodulation *)
(* ------------ *)

(** Alternative version of [Clause.demod_no_check] *)
module Completeness = struct
  type t = 
    | L of term  (* Greater lhs, compare vs this rhs *)
    | R of term  (* Greater rhs, compare vs this lhs *)
    | LR of term * term  (* Both maximal both need to be checked *)
    | None  (* Neither needs to be checked *)

  let tag_to_string = function
    | L _ -> "L " | R _ -> " R" | LR _ -> "LR" | None -> "  "

  let to_string = function
    | L r0 -> sprintf "L  %s" (Term.to_string r0) 
    | R l0 -> sprintf " R %s" (Term.to_string l0) 
    | LR (l0,r0) -> sprintf "LR %s %s" (Term.to_string l0) (Term.to_string r0) 
    | None -> "No"
end

let rec demod_no_check' (state: Set.t) clause = 
  (* If unit *)
  match Clause.get_lits clause with
  | [lit] ->
    (* If positive eq *)
    begin match Term.Eq.decompose_atom lit with
    | Some (l,r) -> 
      (* If non-predicate *)
      if r != SystemDBs.top_term then
        (* Have to check completeness *)
        let completeness lit l r = 
          Some (state.order.oriented lit, l, r)
        in
        (* If already comitted to state, cannot look to earlier parents *)
        if Set.mem_any state clause then
          completeness lit l r
        (* Otherwise, look into earlier rewrite parents *)
        else
          (* KK 2024 !!!fix: incomplete: replaced by ompleteness lit l r *)
          (*
          match Clause.get_tstp_source clause with
          | TSTP_inference_record (tstp_inference_rule, main_parents) ->        
            begin match tstp_inference_rule with
            | Demodulation
            | LightNormalisation
            | TheoryNormalisation 
            | SMTTheoryNormalisation 
            | ACDemodulation
            | GroundJoinability ->                
              let clause' = List.hd main_parents in
              demod_no_check' state clause'


            | _ ->
                      
                 completeness lit l r
            end

            | _ ->
            *)
            completeness lit l r
      else
        None
    | None -> 
      None
    end
  | _ -> 
    None

let demod_no_check (state: Set.t) clause = 
  (* If incomplete (demod check disabled in options) *)
  dbg D_demod @@ lazy (sprintf "demod_no_check: %s" (Options.Demod_check.to_string state.options.demod_completeness_check));
  if state.options.demod_completeness_check == Options.Demod_check.Off then Completeness.None else
  (* If unit *)
  match Clause.get_lits clause with
  | [lit] ->
    (* If positive eq *)
    begin match Term.Eq.decompose_atom lit with
    | Some (l,r) -> 
      (* If non-predicate *)
      if r != SystemDBs.top_term then
        (* Have to check completeness *)
        let completeness lit l r = 
          match state.order.oriented lit with
          | GT -> Completeness.L r
          | LT -> Completeness.R l
          | INC -> Completeness.LR (l,r)
          | EQ -> dassert (fun () -> false); Completeness.None
        in
        (* If simplifications are cached, we cannot look up *)
        if state.options.cache_sim != Options.SupSimplificationSetup.CacheSim.None then
          completeness lit l r
        (* If already comitted to state, cannot look to earlier parents *)
        else if Set.mem_any state clause then
          completeness lit l r
        (* Otherwise, look into earlier rewrite parents *)
        else
          match Clause.get_tstp_source clause with
          | TSTP_inference_record (tstp_inference_rule, main_parents) ->        
            begin match tstp_inference_rule with
            | Demodulation
            | LightNormalisation
            | TheoryNormalisation 
            | SMTTheoryNormalisation 
            | ACDemodulation
            | GroundJoinability -> 
                (* Get earliest possible rewrite parent, then adjust *)
                (* KK 2024 !!!fix: incomplete: replaced by ompleteness lit l r *)
              let clause' = List.hd main_parents in
              let (===) = Unif.is_renaming in
              let (=!=) = neg2 Unif.is_renaming in
              let completeness' = 
                match demod_no_check' state clause' with
                | None -> Completeness.None
                | Some (GT,l0,r0) -> 
                  dassert (fun () -> l0 != r0 && l != r);
                  if l0 == l then Completeness.L r0 else if l0 == r then Completeness.R r0 else if l0 === r then Completeness.R l else Completeness.None
                | Some (LT,l0,r0) -> 
                  dassert (fun () -> l0 != r0 && l != r);
                  if r0 == l then Completeness.L l0 else if r0 == r then Completeness.R l0 else if r0 === l then Completeness.L r else if r0 === r then Completeness.R l else Completeness.None
                | Some (INC,l0,r0) -> 
                  dassert (fun () -> l0 != r0 && l != r);
                  if l0 === r0 then completeness lit l r else 
                  if l0 == l then
                    if r0 == r then
                      Completeness.LR (l0,r0) (* assert false *)
                    else
                      Completeness.L r0
                  else if l0 == r then
                    (dassert (fun () -> r0 =!= l);
                    Completeness.R r0)
                  else if l0 === r then
                    (dassert (fun () -> r0 =!= l);
                    Completeness.R l)
                  else 
                    if r0 == l then
                      Completeness.L l0
                    else if r0 == r then
                      Completeness.R l0
                    else if r0 === l then
                      Completeness.L r
                    else if r0 === r then
                      Completeness.R l
                    else
                      Completeness.None
                | Some (EQ,_,_) -> assert false
              in
              (* If completeness constraint was improved by looking to earlier parents, log that *)
              (* TODO: add statistic for this *)
              dbg_env D_demod (fun () -> 
                let completeness = completeness lit l r in
                if Poly.(completeness <> completeness') then (
                  dbg D_demod @@ lazy (sprintf "completeness  %s" (Completeness.to_string completeness));
                  dbg D_demod @@ lazy (sprintf "completeness' %s" (Completeness.to_string completeness'));
                )(*  else (
                  dbg D_demod @@ lazy (sprintf "completesame %s" (Completeness.to_string completeness));
                ) *)
                              );
                (* KK: 2024: commented*)
                (* completeness' *)                
                completeness lit l r (* KK added*)
            | _ -> 
              completeness lit l r
            end
          | _ -> 
            completeness lit l r
      else
        Completeness.None
    | None -> 
      Completeness.None
    end
  | _ -> 
    Completeness.None



module FwDemod = struct
  module Check = struct
    let strict = demod_no_check
    let lazy_ state clause = lazy (demod_no_check state clause)
    let l0 lazy_ = lazy begin
      let strict = Lazy.force_val lazy_ in
      match strict with 
      | Completeness.R l0 | Completeness.LR (l0,_) -> Some l0
      | _ -> None
    end
    let r0 lazy_ = lazy begin
      let strict = Lazy.force_val lazy_ in
      match strict with 
      | Completeness.L r0 | Completeness.LR (_,r0) -> Some r0
      | _ -> None
    end
  end

  (* include FwDemodIndex *)

  (* Memoisation now handled in index itself *)
  (* module State = struct
    type t = {
      mutable terms: term TMap.t;
      mutable parents: BCSet.t;
    }
  end *)

  module State = struct
    type t = { mutable parents: BCSet.t }

    let empty() = { parents = BCSet.empty }

    let add x state = 
      match x with
      | None -> ()
      | Some (x, _) -> state.parents <- BCSet.add x state.parents
  end

  (* TODO: how to handle cases where some top term appears as subterm 
     elsewhere in the clause? E.g.: 
       a=b | f(a) = c
     `a` can actually be demodulated. Not clear how to do this efficiently. 
     Scan all subterms? *)

  (* Solution: if a top term doesn't pass the completeness test, still allow it to be tried again *)

  (* TODO: Also an issue: if we assume equations in the index aren't 
     necessarily reduced, then we should recurse into demodulated subterms *)

  (** The function that tries to demodulate term [s] from clause [clause] via
      equations in [index]. *)
  (* Moved to set *)
  (* let func = undefined() *)

  (* As func but tests instead of stopping on the first rewrite, tries all and chooses the smallest one? *)
  (* let func_all candidares s = undefined() *)

  (* let func_nocheck = func (* (fun _ _ _ -> true) *) *)



  (* Settings for traversal: *)
  let demodulate_term_before_args = true
  let demodulate_term_after_args  = true
  let recurse_into_demodulated_subterms = true
  let assert_we_get_fixpoint = false

  let demod_func_check ~order clause dmc s = 
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        if Clause.Bc.(clause == eq_clause) then () else (
          let l,r = Term.Eq.decompose_atom_exn eq_lit in
          let l,r = Term.Eq.regularize_pos pos l r in
          dassert (fun () -> _term == l);
          dbg D_demod @@ lazy (sprintf "func: l: %s r: %s s: %s" (Term.to_string l) (Term.to_string r) (Term.to_string s));
          let result = Inference_rules.demodulation_bare_nomatch ~order eq_lit l r s subst in
          begin match result with
          | Some s' -> 
            dbg D_demod @@ lazy (sprintf "Hit %s to %s" (Term.to_string s) (Term.to_string s'));
            (* dbg D_demod @@ lazy (sprintf "via %s" (Clause.to_string_tptp eq_clause)); *)
            dassert (fun () -> s  == subst_apply subst l);
            dassert (fun () -> s' == subst_apply subst r);
            if dmc s' subst then
              raise_notrace @@ Set.Return_demod_func (s', Some (eq_clause, subst))
            else
              ()
          | None -> ()
          end
        )
      )

  (** Demodulate top (cache, no recurse into subterms) *)
  let demodulate_top (* check *) state ~order index clause dmc s = 
    dbg D_demod @@ lazy (sprintf "demodulate_top: %s" (Term.to_string s));
    match s with
    | Term.Fun _ -> 
      (* State.get state (func index clause) s *)
      let s', data = 
        try FwDemodIndex_M.iter_fwd index s (demod_func_check ~order clause dmc s); s, None   (* TODO all of this needs cleaning up when there's time *)
        with Set.Return_demod_func x -> x
      in
      begin match data with
      | Some (parent_clause, _subst) ->
        dassert (fun () -> parent_clause != clause);
        dassert (fun () -> s != s');
        State.add data state;
        s'
      | _ -> 
        s
      end
    | Term.Var _ -> s

  (** Demodulate top (no cache, no recurse into subterms) *)
  let demodulate_top_nocheck (* check *) state index clause func s = 
    dbg D_demod @@ lazy (sprintf "demodulate_top: %s" (Term.to_string s));
    match s with
    | Term.Fun _ -> 
      (* State.get state (func index clause) s *)
      let s', data = FwDemodIndex_M.iter_fwd_caching index s func in
      begin match data with
      | Some (parent_clause, _subst) ->
        dassert (fun () -> parent_clause != clause);
        dassert (fun () -> s != s');
        State.add data state;
        s'
      | _ -> 
        s
      end
    | Term.Var _ -> s

  (** Demodulate recursively (no ordering checks) *)
  let rec demodulate_subterms state index (* clause *) func_nocheck s = 
    match s with
    | Term.Fun (sym, args, _) ->
      dbg D_demod @@ lazy (sprintf "demodulate_subterms: %s" (Term.to_string s));

      (* First, demodulate term itself *)
      let s' = 
        (* let s' = State.get state (func index clause) s in *)
        if demodulate_term_before_args then (
          let s', data = FwDemodIndex_M.iter_fwd_caching index s func_nocheck in
          State.add data state;
          s'
        ) else (
          s
        )
      in

      (* If successful, try again until fixpoint *)
      if s' != s then (
        (* dbg D_demod @@ lazy (sprintf "demod_sub: %s becomes %s" (Term.to_string s) (Term.to_string s')); *)
        if recurse_into_demodulated_subterms then
          demodulate_subterms state index func_nocheck s'
        else
          s'
      ) 

      (* Then demodulate all args *)
      else (
        (* let args = Term.arg_to_list (if s == s' then args else Term.get_args s') in *)
        let args = dassert (fun () -> s == s'); Term.arg_to_list args in
        let args' = List.map (demodulate_subterms state index func_nocheck) args in

        (* If same, then we're done *)
        if List.for_all2 (==) args args' then
          s'

        (* If changed, then we also try to re-demodulate the term itself *)
        else
          let s'' = add_term_db @@ Term.create_fun_term sym args' in
          (* State.get state (func index clause) s'' *)
          let s''' = 
            if demodulate_term_after_args then (
              let s''', data = FwDemodIndex_M.iter_fwd_caching index s'' func_nocheck in
              State.add data state;
              s'''
            ) else (
              s''
            )
          in

          (* And again, if successful, try again until fixpoint *)
          if recurse_into_demodulated_subterms && s''' != s'' then 
            demodulate_subterms state index func_nocheck s'''
          else
            s'''
      )
    | Term.Var _ -> s

  (** Demodulate args recursively (no ordering checks) *)
  let demodulate_args state index (* clause *) func_nocheck s =
    match s with
    | Term.Fun (sym, args, _) ->
      dbg D_demod @@ lazy (sprintf "demodulate_args: %s" (Term.to_string s));
      let args = Term.arg_to_list args in
      let args' = List.map (demodulate_subterms state index func_nocheck) args in
      if List.for_all2 (==) args args' then
        s
      else
        add_term_db @@ Term.create_fun_term sym args'
    | Term.Var _ -> s


  (* Controls whether to record encompassment statistics or not *)
  let record_statistics = false
  let _ = if record_statistics then (out_warning "simplify_new:can be expensive:record_statistics=true")

  let demod_completeness_check ~(order:ordering) check_type typ clause l l' r subst =
    (* Old version *)
    (* let is_renaming lsigma l = 
      lsigma == l || (try ignore @@ Unif.matches lsigma l; true with Unif.Matching_failed -> false)
    in *)
    
    let[@inline] check_renaming subst = 
      not (Subst.is_renaming subst)
    in
    let[@inline] check_order_fast ~(order:ordering) r l' = 
      order.terms r l' == GT  (* TODO: This can actually go in the orderings_cache *)
    in
    let[@inline] check_order_full ~(order:ordering) typ l l' clause = 
      (* TODO: here it is useless building of term *)
      let eq_term' = add_lit_eq true typ l l' in
      List.exists (fun x -> order.lits x eq_term' == GT) (Clause.get_lits clause)
    in
    
    let open Options.Demod_check in
    match check_type with
    | Off -> 
      true

    | FullOld ->
      dbg D_demod @@ lazy "really checking demod order (full)";
      dbg D_demod2 @@ lazy (sprintf "Rules: ( %s = %s ) < %s" (Term.to_string l) (Term.to_string l') (Clause.to_string_tptp clause));
      check_order_full ~order typ l l' clause
    | FastOld ->
      dbg D_demod @@ lazy "really checking demod order (fast)";
      (* dbg D_demod2 @@ lazy (sprintf "Rules: %s cannot be a renaming of %s" (Term.to_string l) (Term.to_string orig_l)); *)
      dbg D_demod2 @@ lazy (sprintf "Rules: %s < %s" (Term.to_string l') (Term.to_string r));
      check_order_fast ~order r l'
    
    | Full ->
      dbg D_demod @@ lazy "really checking demod order (full + renaming)";
      (* dbg D_demod2 @@ lazy (sprintf "Rules: %s cannot be a renaming of %s" (Term.to_string l) (Term.to_string orig_l)); *)
      dbg D_demod2 @@ lazy (sprintf "Rules: %s cannot be a renaming" (Subst.to_string subst));
      dbg D_demod2 @@ lazy (sprintf "or     ( %s = %s ) < %s" (Term.to_string l) (Term.to_string l') (Clause.to_string_tptp clause));
      let result = 
        if check_renaming subst then (
          if record_statistics && not (check_order_full ~order typ l l' clause) then Statistics.(bump_int_stat sim_encompassment_demod);
          true
        ) else (
          check_order_full ~order typ l l' clause
        )
      in
      dbg_env D_demod (fun () ->
        let a = check_renaming subst in
        let b = check_order_full ~order typ l l' clause in
        dbg D_demod @@ lazy (sprintf "demod order result: %B %B" a b)
      );
      result
    | Fast ->
      dbg D_demod @@ lazy "really checking demod order (fast + renaming)";
      (* dbg D_demod2 @@ lazy (sprintf "Rules: %s cannot be a renaming of %s" (Term.to_string l) (Term.to_string orig_l)); *)
      dbg D_demod2 @@ lazy (sprintf "Rules: %s cannot be a renaming" (Subst.to_string subst));
      dbg D_demod2 @@ lazy (sprintf "or     %s < %s" (Term.to_string l') (Term.to_string r));
      let result = 
        if check_renaming subst then (
          if record_statistics && not (check_order_fast ~order r l') then Statistics.(bump_int_stat sim_encompassment_demod);
          true
        ) else (
          check_order_fast ~order r l'
        )
      in
      dbg_env D_demod (fun () ->
        let a = check_renaming subst in
        let b = check_order_fast ~order r l' in
        dbg D_demod @@ lazy (sprintf "demod order result: %B %B" a b)
      );
      result

  exception Return of clause list

  (** Demodulate clause via equations in index. Returns [Some demodulated_clause] if possible, or [None] if no demodulation can be done *)
  let demodulate_forward ~order ~demod_completeness_check_type ~func set index clause =
    Statistics.(time sim_time_fw_demod) @@ fun () ->

    dbg D_demod @@ lazy "demodulate_forward";

    (* Idea: 
       - Keep a map of subterms -> normal_forms
       - First check if any non-equality or negative literal in the clause: 
         if yes, [no_check] is true and we don't need to check ordering for 
         completeness
         is false, since equalities will always be smaller
       - For all literals in clause, starting at the top, first check if the 
         term appears in the map. 
         - if yes, that's its normal form
         - if not, then demodulate
           - if at a subterm position, no need to do ordering_check
           - if at the top, *first* try to match, then only if matching 
             succeeds do we need to check ordering 
         - if demodulation was successful, return
         - otherwise, recurse into subterms *)

    (* Demodulation ordering check
       - Clause being rewritten is non-unit: no check, else
       - Literal is non-eq: no check, else
       - Literal is negative: no check, else
       - At this point we need ordering check, but only at the top. 
         If we use eq a=b to rewrite l=r to l'=r, where l = a\sigma and l' = b\sigma, then
         - If l<r: no check
         - If l matches a, that is, that l\theta = a, that is, l and a are variants: no check
         - Else, check that l'<r. *)

    let state = State.empty() in

    (* No check needed at all if: non-unit, negative literal, predicate literal. *)
    (* let no_check = Clause.demod_no_check clause in *)
    let no_check = lazy (demod_no_check set clause) in
    dbg D_demod @@ lazy (sprintf "no_check = %s" (Completeness.to_string @@ Lazy.force no_check));

    let lits = Clause.get_lits clause in
    let lits' = 
      lits |> List.map (fun lit ->
        match Term.Eq.decompose_lit_type lit with
        | Some (sign, typ,l,r) ->
          let[@inline] if_eq_return sign typ l r = if sign then raise_notrace (Return (BCSet.elements state.parents)) else add_lit_eq sign typ l r in

          (* Rather than check is_oriented first, just demodulate first without 
             checks, then manually check if needed (and only at the top). *)
          (* Actually, it's a headache to do this and not really worth it: it's only
             one ordering check, and it's probably cached, and it's only on unit 
             positive equations, and it's only a difference if there are 0 candidates
             for any side. TODO change in the future? Not sure if worth it. *)
          let l', r' = 
            (* Ordering checks *)
            (* TODO let's hope all these closures don't slow things too much, needs profiling with perf and possible manual tweaking *)
            (* let dmc[@inline] = demod_completeness_check ~order demod_completeness_check_type typ clause in *)
            let[@inline] dmc a b c d = match Lazy.force_val c with Some c -> demod_completeness_check ~order demod_completeness_check_type typ clause a b c d | None -> true in
            demodulate_top state ~order index clause (fun l' subst -> let r0 = Check.r0 no_check in dmc l l' r0 subst) l,
            demodulate_top state ~order index clause (fun r' subst -> let l0 = Check.l0 no_check in dmc r r' l0 subst) r
            (* match no_check with
            | Completeness.None -> 
              demodulate_top_nocheck state index clause func l,
              demodulate_top_nocheck state index clause func r
            | Completeness.L r0 -> 
              demodulate_top state ~order index clause (fun l' subst -> dmc l l' r0 subst) l,
              demodulate_top_nocheck state index clause func r
            | Completeness.R l0 -> 
              demodulate_top_nocheck state index clause func l,
              demodulate_top state ~order index clause (fun r' subst -> dmc r r' l0 subst) r
            | Completeness.LR (l0,r0) -> 
              demodulate_top state ~order index clause (fun l' subst -> dmc l l' r0 subst) l,
              demodulate_top state ~order index clause (fun r' subst -> dmc r r' l0 subst) r *)
          in

          let l' = 
            if recurse_into_demodulated_subterms && l' != l then
              fix_point (demodulate_top_nocheck state index clause func) l'
            else
              l'
          in
          let r' = 
            if recurse_into_demodulated_subterms && r' != r then
              fix_point (demodulate_top_nocheck state index clause func) r'
            else
              r'
          in

          if l' == r' then if_eq_return sign typ l' r' else

          let l'' = demodulate_args state index func l' in
          let r'' = demodulate_args state index func r' in

          if l'' == r'' then if_eq_return sign typ l'' r'' else

          let l''' = 
            if l'' == l' then
              l''
            else if recurse_into_demodulated_subterms then
              fix_point (demodulate_subterms state index func) l''
            else
              demodulate_top_nocheck state index clause func l''
          in
          let r''' = 
            if r'' == r' then
              r''
            else if recurse_into_demodulated_subterms then
              fix_point (demodulate_subterms state index func) r''
            else
              demodulate_top_nocheck state index clause func r''
          in

          if l''' == l && r''' == r then
            lit
          else if l''' == r''' then
            if_eq_return sign typ l''' r'''
          else
            add_lit_eq sign typ l''' r'''

        (* Non-equality literal *)
        | None ->
          demodulate_args state index func lit
      )
    in

    if List.for_all2 (==) lits lits' then (
      clause
    ) else (
      (* Only here do we actually build the clause *)
      Statistics.(bump_int_stat sim_fw_demodulated);  (* Just counts total number of demodulated clauses, not comparable to previous versions *)
      let parents = BCSet.elements state.parents in
      let source = Clause.tstp_source_demodulation ~main:clause ~eqs:parents in
      let conclusion = create_clause ~normalise_eqs:true source lits' in
      dbg_env D_demod (fun () -> dbg_inference D_demod 
        "Forward demodulated" ~from:clause ~into:(Some conclusion) ~via:parents
      );
      conclusion
    )

  (*-----------KK: demodulate_forward_new -------*)
  (*
  type cl_demod_state = 
      {(* dres: stores result of demodulation of subterms *)
       (* dres: s -> Some ((r',[l=r])) if subterm s demod to r'; l=r is used for proof *)
       (*       s -> None if there is no matching l in demod index *)
       (*       s is not in; then we have not tried to demod s *)
             
       mutable dres : (term * clause list) TMap.t; 

  (* TODO: new_ueqs that were generated during e.g. shortening demod paths *)
  (* TODO: whether record demod instances in light norm *)
       mutable dueq_new : clause list;
     }


  (*-------*)        
  let demodulate_forward_new ~demod_completeness_check_type index (clause:clause) : unit =

    dbg D_demod @@ lazy "demodulate_forward";

  (*-- demod top down --*)

    let demod_term_update ~check_fun cl_ds s = (* is is a term but below literal *)
      try 
        match TMap.find s cl_ds.dres with 
        | Some _ -> cl_ds (* already there *)
        | None   -> cl_ds (* alredy tried *)            
      with 
        Not_found ->        
          
          let candidates = 
            DemodulationIndex.get_fwd_candidates index s |>
              (* sort small ueq first *)
            List.sort (fun (_,(_,_,ueq_1)) (_,(_,_,ueq_2)) -> Clause.cmp_num_symb ueq_1 ueq_2)             
          in
          candidates |> List.iter (fun (_, lst) ->
            (* dbg D_demod @@ lazy (sprintf "length lst %d" (List.length lst)); *)
            lst |> List.iter (fun (pos, ueq_lit, ueq_clause) ->
              if not @@ Clause.equal_bc clause ueq_clause then (
                match Term.decompose_eq_atom ueq_lit with
                | [_;l;r] ->
                    let l,r = Term.Eq.regularize_pos pos l r in
                    
                    Inference_rules.demodulation
                      ~demod_completeness_check_type ~to_check eq_clause eq_lit l r s clause 
                |_ -> assert fail 
               )
                             )
                  )            
    in ()
  *)
  (*----------*)

  let exists_some_candidate_dbg state clause = 
    let some_nonempty = ref false in
    clause |> Clause.iter (fun lit -> 
      lit |> Term.iter_preorder_novar (fun t -> 
        (* if List.X.is_nonempty @@ DemodulationIndex.get_fwd_candidates Set.(state.fw_demod_index_debug) t then
          some_nonempty := true *)
        DemodulationIndex.get_fwd_candidates Set.(state.fw_demod_index_debug) t 
        |> List.iter (fun (term, _lst) ->
          if (try ignore @@ Unif.matches term t; true with Unif.Matching_failed -> false) then (
            dbg D_demod2 @@ lazy (sprintf "Rewrite %s with %s confirmed" (Term.to_string t) (Term.to_string term));
            some_nonempty := true
          )
        )
      )
    );
    if not !some_nonempty then dbg D_demod2 @@ lazy (sprintf "FAILED TO REWRITE %s" (Clause.to_string_tptp clause));
    !some_nonempty

  let assert_some_candidate_dbg state clause clause' = 
    if Set.debug_index_flag then (
      assert (clause' == clause || exists_some_candidate_dbg state clause)
    )

  let simplify state clause =
    try
    (* assert (state.sim_opt.sim_use_demod); *)
    (* match Set.(demodulate_forward state.fw_demod_index) clause with
    | Some new_clause -> new_clause
    | None -> clause *)
    let demod_completeness_check_type = Set.(state.options.demod_completeness_check) in
    let clause' = demodulate_forward ~order:state.order ~demod_completeness_check_type ~func:state.fw_demod_func_nocheck state Set.(state.fw_demod_index) clause in
    assert_some_candidate_dbg state clause clause';
    (* Simplified clause' *)
    if Clause.Bc.(clause' != clause) then
      (* Important! Here pattern match to ensure that if retention test tells us that clause is eliminated, parents are still recorded *)
      match TrivRules.simplify clause' with
      | Simplified _ as result -> 
        if assert_we_get_fixpoint then dassert (fun () -> 
          let clause'' = demodulate_forward ~order:state.order ~demod_completeness_check_type ~func:state.fw_demod_func_nocheck state Set.(state.fw_demod_index) clause' in
          clause'' == clause'
        );
        result
      | Eliminated parents -> 
        Eliminated (parents @ Clause.get_demod_eqs clause')
    else
      Simplified clause
    with Return parents -> 
      dbg_inference D_demod "Forward demodulated" ~from:clause ~into:None ~via:parents;
      Eliminated parents
end

module FwDemodLoop (RetentionTest : TrivRule) : FwRule = struct

  include FwDemodIndex
  let make_intermediate_dead state clause =
    dbg D_demod @@ lazy (sprintf "FwDemodLoop:make_intermediate_dead %s" (Clause.to_string_tptp clause));
    try Set.add_dead state clause
    with Set.Is_mem -> try set_dead_and_remove state clause
    with Set.Is_dead -> ()
  let make_intermediate_dead state clause = ()

  let rec loop ~check ~parents_acc state clause =
    dbg D_demod @@ lazy "FwDemodLoop:loop";
    let demod_completeness_check_type = Set.(state.options.demod_completeness_check) in
    if check && Set.mem_any state clause then (
      Eliminated parents_acc
    ) else (
      let open Fw_result.O in
      try
      let clause' = FwDemod.demodulate_forward ~order:state.order ~demod_completeness_check_type ~func:state.fw_demod_func_nocheck state Set.(state.fw_demod_index) clause in
      FwDemod.assert_some_candidate_dbg state clause clause';
      if Clause.Bc.(clause' == clause) then (
        Simplified clause
      ) else (
        (* Clause was demodulated, let's apply retention tests and go for another round *)
        make_intermediate_dead state clause;
        match RetentionTest.simplify clause' with
        | Simplified clause'' ->
          if Clause.Bc.(clause'' != clause') then make_intermediate_dead state clause';
          let parents_acc = Clause.get_demod_eqs clause' @ parents_acc in
          loop (* ~demod_completeness_check *) ~check:true ~parents_acc state clause''
        | Eliminated parents -> 
          Eliminated (parents @ parents_acc)
      )
      with FwDemod.Return parents -> 
        dbg_inference D_demod "Forward demodulated" ~from:clause ~into:None ~via:parents;
        Eliminated parents
    )

  let simplify (* ~demod_completeness_check *) state clause =
    loop (* ~demod_completeness_check *) ~check:false ~parents_acc:[] state clause
end

module FwDemodLoopTriv = FwDemodLoop(TrivRules)

module BwDemod : BwRule = struct
  include BwDemodIndex

  (** Demodulate clauses in index via equality clause eq. Returns list of demodulated clauses *)
  let simplify state eq_clause =
    Statistics.(time sim_time_bw_demod) @@ fun () ->

    (* assert (state.sim_opt.sim_use_demod); *)

    let demod_completeness_check_type = Set.(state.options.demod_completeness_check) in
    let use_nonoriented_for_bw_demod =  true  in
    
    let demodulate_via old_ref new_ref eq_lit l r =
      dbg D_demod @@ lazy (sprintf "demodulate_backward: l: %s r:%s lit:%s" (Term.to_string l) (Term.to_string r) (Term.to_string eq_lit)) ;
      let candidates = BwDemodIndex_M.iter_bwd Set.(state.bw_demod_index) l in
      candidates (fun s subst lst ->
        (* dbg D_demod @@ lazy (sprintf "length lst %d" (List.length lst)); *)
        lst |> List.iter (fun (clause, do_check) ->
          (* TODO: If same clause demodulated in several ways, what should do? *)
          (* if clause != eq_clause *)
          if Clause.Bc.(clause != eq_clause)
          && not (List.memq clause !old_ref) then (
            (* let conclusion = clause |> fix_point (fun x ->
              dbg D_demod @@ lazy (sprintf "iter x=%s" (Clause.to_string_tptp x));
              try
                Inference_rules.demodulation eq_clause eq_lit l r s x; x
              with Inference_rules.Return x' ->
                x'
            )
            in *)

            (* Old interface: just ordering check *)
            (*
            begin try
              Inference_rules.demodulation ~demod_completeness_check_type ~do_check eq_clause eq_lit l r s clause;
              ()
            with Inference_rules.Return conclusion -> 
              Statistics.(bump_int_stat sim_bw_demodulated);
              dbg D_demod @@ lazy (sprintf "Backward demodulated %s" (Clause.to_string_tptp clause));
              dbg D_demod @@ lazy (sprintf "to                   %s" (Clause.to_string_tptp conclusion));
              (* Get list of indices *)
              (* let indices = Set.indices state clause in *)
              (* Remove old clause from sim_state *)
              set_dead_and_remove state clause;
              (* Add new clause to sim_state *)
              (* ignore @@ sim_add_clause state conclusion; *)
              (* add_to_indices state indices conclusion; *)
              (* Cons new clause onto new_ref *)
              ListExtra.cons_ref conclusion new_ref;
              ListExtra.cons_ref clause old_ref;
            end
            *)

            let result = Inference_rules.demodulation_bare_nomatch ~order:state.order eq_lit l r s subst in
            match result with
            | None -> ()
            | Some s' -> 
              dbg D_demod @@ lazy (sprintf "Bw %s to %s" (Term.to_string s) (Term.to_string s'));
              (* Demodulated clause[s]
                 To          clause[s']
                 Via         l = r *)
              let checked = 
                match do_check with
                | None -> true
                | Some t -> 
                  FwDemod.demod_completeness_check 
                    ~order:state.order demod_completeness_check_type 
                    (Term.Eq.eq_type eq_lit) clause s s' t subst
              in
              if checked then (
                let source = Clause.tstp_source_demodulation ~main:clause ~eqs:[eq_clause] in
                let lits' = 
                  Clause.get_lits clause
                  |> List.map (Term.replace s s')
                in
                let conclusion = create_clause ~normalise_eqs:true source lits' in
                (* TODO also TrivialRules? *)

                Statistics.(bump_int_stat sim_bw_demodulated);
                dbg_env D_demod (fun () -> dbg_inference D_demod 
                  "Backward demodulated" ~from:clause ~into:(Some conclusion) ~via:[eq_clause]
                );
                set_dead_and_remove state clause;
                ListExtra.cons_ref conclusion new_ref;
                ListExtra.cons_ref clause old_ref;
              )
          )
        )
      )
    in

    (* Has to be unit positive equality *)
    match Clause.get_lits eq_clause with
    | [eq_lit] ->
      begin match Term.Eq.decompose_atom eq_lit with
      | Some (l,r) -> 
          let old_ref = ref [] in
          let new_ref = ref [] in
          dbg D_demod @@ lazy (sprintf "state.order.oriented eq_lit: %s :%s" (Term.to_string eq_lit) (PartialOrd.to_string (state.order.oriented eq_lit)));
          begin match state.order.oriented eq_lit with
          | GT ->
            demodulate_via old_ref new_ref eq_lit l r;
          | LT ->
            demodulate_via old_ref new_ref eq_lit r l;
          | INC ->
            (* Could also choose not to try via unoriented equations *)
            if use_nonoriented_for_bw_demod then (
              if Term.var_subset r l then demodulate_via old_ref new_ref eq_lit l r;
              if Term.var_subset l r then demodulate_via old_ref new_ref eq_lit r l;
            )
          | EQ -> invalid_arg "Uncaught s=s"
          end;

          List.map2 (fun from into -> BwSimplified {from; into}) !old_ref !new_ref

      | None -> []
      end
    | _ -> []
end

(* module Demod : FwBwRule = struct
  let add state clause = 
    FwDemod.add state clause;
    BwDemod.add state clause

  let remove state clause = 
    FwDemod.remove state clause;
    BwDemod.remove state clause

  let fw_simplify = FwDemod.simplify 

  let bw_simplify = BwDemod.simplify 
end *)

module FwLightNorm : FwRule = struct
  include LightNormIndex

  let simplify state clause = 
    Statistics.(time sim_time_light_norm) @@ fun () ->

    let any_changed = ref false in
    let equalities = ref BCSet.empty in
    let no_check : LightNormIndex_M.completeness Lazy.t = lazy (demod_no_check state clause |> Obj.magic) in
    let lits = 
      Clause.get_lits clause
      |> List.map (fun x -> 
        let x', new_equalities = LightNormIndex_M.normalise Set.(state.lightnorm_index) ~no_check x in
        if x != x' then (
          any_changed := true;
          equalities @= BCSet.union new_equalities;
        );
        (* TODO: early return if s=s *)
        x'
      )
    in
    if !any_changed then (
      dassert (fun () -> not @@ BCSet.is_empty !equalities);
      dassert (fun () -> not @@ BCSet.mem clause !equalities);
      let parents = BCSet.elements !equalities in
      let tstp_source = Clause.tstp_source_lightnorm ~main:clause ~eqs:parents in
      let conclusion = create_clause ~normalise_eqs:true tstp_source lits in
      dbg_env D_lightnorm (fun () -> dbg_inference D_lightnorm 
        "Light normalised" ~from:clause ~into:(Some conclusion) ~via:parents
      );
      Statistics.(bump_int_stat sim_light_normalised);

      (* Important! Here pattern match to ensure that if retention test tells us that clause is eliminated, parents are still recorded *)
      match TrivRules.simplify conclusion with
      | Simplified _ as result -> result
      | Eliminated parents -> Eliminated (parents @ Clause.get_demod_eqs conclusion)
    ) else (
      Simplified clause
    )

  let simplify state clause = 
    let fixpoint = true in
    (* Due to renaming, here we also need to re *)
    if fixpoint then
      Fw_result.fix_point (simplify state) clause
    else
      simplify state clause
end

module FwDemodLightNormLoop (RetentionTest : TrivRule) : FwRule = struct
  let make_intermediate_dead state clause =
    dbg D_demod @@ lazy (sprintf "FwDemodLightNormLoop:make_intermediate_dead %s" (Clause.to_string_tptp clause));
    try Set.add_dead state clause
    with Set.Is_mem -> try set_dead_and_remove state clause
    with Set.Is_dead -> ()
  let make_intermediate_dead state clause = ()

  let make_intermediate_dead_if_different state old_clause clause =
    if clause != old_clause then (
      dbg D_demod @@ lazy (sprintf "FwDemodLightNormLoop:make_intermediate_dead %s" (Clause.to_string_tptp clause));
      make_intermediate_dead state old_clause
    )

  let check_existing_and_do_retention state parents clause =
    if Set.mem_any state clause then (
      dbg D_demod @@ lazy "Avoided repeated demod/lightnorm";
      Eliminated parents
    ) else (
      RetentionTest.simplify clause
    )
  (* let check_existing_and_do_retention state clause = Simplified clause *)

  let check_existing_and_do_retention_if_different state parents old_clause clause =
    if clause != old_clause then
      check_existing_and_do_retention state parents clause
    else
      Simplified clause

  let rec simplify ~first ~parents_acc state clause =
    let open Fw_result.O in

    (* The original clause, don't check if it is repeated, otherwise do *)
    if not first && Set.mem_any state clause then (
      dbg D_demod @@ lazy "Avoided repeated demod/lightnorm";
      Eliminated parents_acc
    ) 

    else (
      (* Light normalise *)
      let* clause' = FwLightNorm.simplify state clause in
      (* TODO if not first also we're done? because now also no need for iterating until fixpoint on demodulation *)
      (* dassert to check that *)
      if Clause.Bc.(clause' != clause) then make_intermediate_dead state clause;
      let parents_acc = if clause' != clause then Clause.get_demod_eqs clause' @ parents_acc else parents_acc in

      (* Moved to FwLightNorm *)
      (* check_existing_and_do_retention_if_different state parents_acc clause clause' >>= fun clause'_ret ->
      if Clause.Bc.(clause'_ret != clause') then make_intermediate_dead state clause'; *)
      
      (* Demodulate *)
      let* clause'' = FwDemod.simplify state clause' in
      FwDemod.assert_some_candidate_dbg state clause' clause'';
      let parents_acc = if clause'' != clause' then Clause.get_demod_eqs clause'' @ parents_acc else parents_acc in

      (* If no demodulation, we are done *)
      if Clause.Bc.(clause'' == clause') then (
        Simplified clause'
      ) 

      (* If clause was demodulated, apply retention tests and go for another round *)
      else (
        (* Also, store the previous as dead (because it has been demodulated to this) *)
        make_intermediate_dead state clause';

        (* Moved to FwDemod *)
        (* (* RetentionTest.simplify *) check_existing_and_do_retention state parents_acc clause'' >>= fun clause''_ret ->
        if Clause.Bc.(clause''_ret != clause'') then make_intermediate_dead state clause''; *)

        simplify ~first:false ~parents_acc state clause''
      )
    )
  let simplify state clause =
    simplify ~first:true ~parents_acc:[] state clause
end

module FwDemodLightNormLoopTriv = FwDemodLightNormLoop(TrivRules)



(* module BwLightNorm : BwRule = struct
  let index = Index_tag.LightNorm

  let simplify state clause = 
    let open Fw_result.O in
    let context = Set.(state.context) in
    let new_ref = ref [] in
    let old_ref = ref [] in
    context |> BCMap.iter (fun clause param ->
      (* let clause' = FwLightNorm.simplify clause in *)
      match FwLightNorm.simplify state clause with
      | Simplified clause' ->
        if Clause.Bc.(clause' != clause) then (
          (* [clause] bw removed, [clause'] bw added *)
          set_dead_and_remove state clause;
          ListExtra.cons_ref clause  old_ref;
          ListExtra.cons_ref clause' new_ref;
        )
      | Eliminated -> ()
    );
    {add=(!new_ref); remove=(!old_ref)}
end



module Orient : TrivRule = struct
  let simplify clause =
    let changed = ref false in
    let new_lits = Clause.get_lits clause |> List.map (fun lit ->
      try 
        let sign, (typ,l,r) = Term.Eq.decompose_lit_type lit in
        if Term.get_fast_key l < Term.get_fast_key r then (
          lit
        ) else (
          changed := true;
          add_lit_eq sign typ r l
        )
      with Invalid_argument _ ->
        lit
    ) in
    if !changed then (
      let tstp_source = Clause.TSTP_inference_record (Clause.EqFlip, [clause]) in
      let clause' = create_clause tstp_source new_lits in
      Statistics.(bump_int_stat sim_light_normalised);
      clause'
    ) else (
      clause
    )
  let simplify clause = 
    Simplified (simplify clause)
end *)



module ACJoinability : FwRule = struct
  exception Tautology_exn

  let simplify (state: Set.t) clause =
    Statistics.(time sim_time_joinable) @@ fun () ->

    dbg D_joinability @@ lazy "ac_joinability";

    dbg_env D_joinability2 (fun () ->
      if Clause.is_ac_axiom clause then (
        dbg D_joinability2 @@ lazy (sprintf "X Is axiom: %s" (Clause.to_string_tptp clause));
      );
      if not (AC.Table.has_ac state.ac_symbols) then (
        dbg D_joinability2 @@ lazy (sprintf "X No-ac symbols");
      );
    );
    
    if not (AC.Table.has_ac state.ac_symbols) 
    || Clause.is_ac_axiom clause
    then
      Simplified clause
    else try
      let ops = state.ac_symbols.ac in
      let some_deleted = ref false in
      let lits' = 
        Clause.get_lits clause
        |> List.filter (fun lit ->
          match Term.Eq.decompose_lit lit with
          | Some (sign, l,r) -> 
            (* let l' = AC.normalise_ac ops l in
            let r' = AC.normalise_ac ops r in
            dbg D_joinability2 @@ lazy (sprintf "  orig: %s %s" (Term.to_string l ) (Term.to_string r ));
            dbg D_joinability2 @@ lazy (sprintf "  norm: %s %s" (Term.to_string l') (Term.to_string r'));
            if l' == r' then ( *)
            if AC.equal_mod_ac ops l r then (
              if sign then (
                dbg D_joinability @@ lazy (sprintf "%s ↓ %s (pos)" (Term.to_string l) (Term.to_string r));
                raise_notrace Tautology_exn
              ) else (
                dbg D_joinability @@ lazy (sprintf "%s ↓ %s (neg)" (Term.to_string l) (Term.to_string r));
                some_deleted:= true; false
              )
            ) else (
              true
            )
          | None -> 
            true
        )
      in
      if !some_deleted then (
        dbg D_joinability @@ lazy (sprintf "Joinable neg: %s" (Clause.to_string_tptp clause));
        let source = Clause.tstp_source_theory_normalisation ~main:clause ~axioms:state.ac_symbols.axiom_list in
        let clause' = create_clause ~normalise_eqs:true source lits' in
        Statistics.(incr_int_stat (Clause.length clause - Clause.length clause') sim_ac_joinable_simp);
        Simplified clause'
      ) else (
        Simplified clause
      )

    with Tautology_exn -> 
      Statistics.(bump_int_stat sim_ac_joinable_taut);
      dbg D_joinability @@ lazy (sprintf "Joinable pos: %s" (Clause.to_string_tptp clause));
      Eliminated state.ac_symbols.axiom_list
end


module ACNormalisation : FwRule = struct
  exception Tautology_exn

  let simplify (state: Set.t) clause =
    Statistics.(time sim_time_ac_norm) @@ fun () ->

    dbg D_acnorm @@ lazy "ac_normalisation";

    let order_terms = state.order.terms in
    let order_uid = state.order.uid in
    let ops = state.ac_symbols.ac in
    let[@inline] normalise_term x = AC.normalise_ac_complete ~order_terms ~order_uid ops x in

    (* Don't touch axioms themselves *)
    if SMap.is_empty ops
    || Clause.is_ac_axiom clause 
    then (
      (* dbg D_acnorm @@ lazy (sprintf "foo %d" (SMap.cardinal ops)); *)
      Simplified clause
    ) else (
      try
        let any_change = ref false in
        let lits' = 
          Clause.get_lits clause
          |> List.X.filter_map (fun lit ->
            match Term.Eq.decompose_lit_type lit with
            | Some (sign, typ, l,r) ->
              let l' = normalise_term l in
              let r' = normalise_term r in

              (* If unchanged *)
              if l == l' && r == r' then (
                if AC.equal_mod_ac ops l r then 
                  if sign then raise_notrace Tautology_exn else None
                else
                  Some lit
              (* If modified *)
              ) else (
                any_change := true;
                (* Check if we now have a tautology s=s or contradiction s!=s *)
                if l' == r' then (
                  if sign then raise_notrace Tautology_exn else None
                (* Else we really have to construct a new eq term *)
                ) else (
                  (* But we will also check joinability first (which AC normalisation doesn't fully cover since it must be complete) *)
                  (* TODO fully split this, it doesn't need to be coupled *)
                  dbg D_acnorm @@ lazy (sprintf "checking if %s %s joinable" (Term.to_string l') (Term.to_string r'));
                  if AC.equal_mod_ac ops l' r' then (
                    dbg D_acnorm @@ lazy (sprintf "%s %s normalised to %s %s but joinable" 
                      (Term.to_string l) (Term.to_string r) (Term.to_string l') (Term.to_string r'));
                    if sign then raise_notrace Tautology_exn else None
                  ) else (
                    Statistics.(bump_int_stat sim_ac_normalised);  (* Other cases are sim_ac_joinable *)
                    Some (add_lit_eq sign typ l' r')
                  )
                )
              )
            | None ->
              let lit' = normalise_term lit in
              if lit == lit' then (
                Some lit
              ) else (
                any_change := true;
                Some lit'  (* already added to termDB *)
              )
          )
        in
        if !any_change then (
          (* Currently puts all AC axioms as parents. We can refine to only put as parents the specific
             ones who were used to rewrite, but this is not necessary in general. *)
          let parents = state.ac_symbols.axiom_list in
          let tstp_source = Clause.tstp_source_theory_normalisation ~main:clause ~axioms:parents in
          let clause' = create_clause ~normalise_eqs:true tstp_source lits' in
          dbg_env D_acnorm (fun () -> dbg_inference D_acnorm 
            "AC normalised" ~from:clause ~into:(Some clause') ~via:[]
          );
          let deleted_lits = Clause.length clause - Clause.length clause' in
          Statistics.(incr_int_stat deleted_lits sim_ac_joinable_simp);
          Simplified clause'
        ) else (
          Simplified clause
        )
      with Tautology_exn -> 
        dbg_env D_acnorm (fun () -> dbg_inference D_acnorm 
          "AC normalised" ~from:clause ~into:None ~via:[]
        );
        Statistics.(bump_int_stat sim_ac_joinable_taut);
        let parents = state.ac_symbols.axiom_list in
        Eliminated parents
    )
end



module FwACDemod = struct
  let index = FwACDemodIndex.index

  type check_type = 
    | Off
    | Fast of term
    | Full of clause

  let completeness_check 
      ~order ~subs_bck_mult ~check_type 
      subst t_subterms t_subterms' lhs_subterms
      t t' (* lhs rhs *) eq_clause
  = 
    let[@inline] check_renaming subst t_subterms' = 
      not (Subst.is_renaming subst) 
      (* || (match t_subterms' with [] | [Term.Var _] -> false | _ -> true) *)
      || not (t_subterms' |> List.for_all Term.is_var)
    in
    let[@inline] check_subterm ~subs_bck_mult t_subterms t_subterms' lhs_subterms = 
      (
        (* match t_subterms' with hd::tl when hd == List.hd t_subterms -> true | _ -> false *)
        let x = List.hd t_subterms in
        (* t_subterms' |> List.exists (fun y -> try Subst.is_renaming (Unif.matches x y) with Unif.Matching_failed -> false) *)
        t_subterms' |> List.exists (fun y -> x == y)
      ) || (
        try ignore @@ Unif.subsumes_lits ~subs_bck_mult lhs_subterms (List.tl t_subterms); true
        with Unif.Subsumption_failed -> false
      )
    in
    let[@inline] check_order_fast ~(order:ordering) r t' = 
      order.terms r t' == GT
    in
    let[@inline] check_order_full ~(order:ordering) clause eq_clause = 
      let eq_term = Clause.get_lits eq_clause |> List.X.get_singleton in
      List.exists (fun x -> order.lits x eq_term == GT) (Clause.get_lits clause)
    in
    (* TODO: implement all options, currently ignores "old" *)
    (* let open Options.Demod_check in *)
    match check_type with
    | Off -> 
      true
    | Fast r -> 
      dbg D_fw_ac_demod @@ lazy "  really checking acdemod order (fast + renaming)";
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  Rules: %s cannot be a renaming" (Subst.to_string subst));
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     %s cannot have only variables" (List.X.to_string Term.to_string t_subterms'));
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     one of the former has to be = %s" (Term.to_string (List.hd t_subterms)));
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     even without the former it is still subsumed");  (* TODO improve here *)
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     %s < %s" (Term.to_string t') (Term.to_string r));
      check_renaming subst t_subterms' || check_subterm ~subs_bck_mult t_subterms t_subterms' lhs_subterms 
      || check_order_fast ~order r t'
    | Full clause -> 
      dbg D_fw_ac_demod @@ lazy "  really checking acdemod order (full + renaming)";
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  Rules: %s cannot be a renaming" (Subst.to_string subst));
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     %s cannot have only variables" (List.X.to_string Term.to_string t_subterms'));
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     one of the former has to be = %s" (Term.to_string (List.hd t_subterms)));
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     even without the former it is still subsumed");
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  or     %s < %s" (Clause.to_string_tptp eq_clause) (Clause.to_string_tptp clause));
      check_renaming subst t_subterms' || check_subterm ~subs_bck_mult t_subterms t_subterms' lhs_subterms 
      || check_order_full ~order clause eq_clause

  (* Rewrites t to t' via lhs=rhs, such that lhsθ ⊆ t and t' is t with lhsθ replaced by rhsθ *)
  let rewrite_term ~check_type state parents t = 
    match ACDemodIndex_M.get_fwd_special Set.(state.ac_demod_index) t with
    | Some (t', clause) -> 
      dbg D_fw_ac_demod @@ lazy (sprintf "  %s -> %s (special)" (Term.to_string t) (Term.to_string t'));
      parents := clause :: !parents;
      t'
    | None -> 
    match ACDemodIndex_M.get_fwd Set.(state.ac_demod_index) t with
    | Some (lhs, rhs, subst, eq_clause) -> 
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  t %s" (Term.to_string t));
      let t_subterms = AC.ac_subterms t in
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  t_subterms %s" (List.X.to_string Term.to_string t_subterms));
      let lhs_subterms = AC.ac_subterms lhs in
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  lhs_subterms %s" (List.X.to_string Term.to_string lhs_subterms));
      (* Assert completeness *)
      (* TODO: if we want to be able to not put ac terms into normal demod index, we have to treat completeness properly here *)
      (* dassert (fun () -> List.compare_lengths lhs_subterms t_subterms == Ord.lt || not (Subst.is_renaming subst)); *)
      (* if List.compare_lengths lhs_subterms t_subterms <> Ord.lt && Subst.is_renaming subst then t else ( *)
      let t_subterms' = 
        lhs_subterms 
        |> List.fold_left (fun acc x ->
          let x' = subst_apply subst x in
          let acc' = List.X.removeq x' acc in
          acc'
        ) t_subterms
      in
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  t_subterms' %s" (List.X.to_string Term.to_string t_subterms'));
      let t_subterms'' = t_subterms' @ [subst_apply subst rhs] in
      dbg D_fw_ac_demod2 @@ lazy (sprintf "  t_subterms'' %s" (List.X.to_string Term.to_string t_subterms''));
      let t' = AC.mk_term (Term.get_top_symb t) t_subterms'' in
      if check_type == Off
      || completeness_check 
          ~order:state.order ~subs_bck_mult:state.options.subs_bck_mult ~check_type 
          subst t_subterms t_subterms' lhs_subterms t t' eq_clause 
      then (
        dbg D_fw_ac_demod @@ lazy (sprintf "  %s -> %s" (Term.to_string t) (Term.to_string t'));
        parents := eq_clause :: !parents;
        t'
      ) else (
        (* try let subst = Unif.subsumes_lits ~subs_bck_mult lhs_subterms (List.tl t_subterms) *)
        dbg D_fw_ac_demod @@ lazy (sprintf "  %s ⌿> %s" (Term.to_string t) (Term.to_string t'));
        t
      )
    | None -> 
      t
      
  let rec demod_term ~check_type (state: Set.t) parents t = 
    match t with
    | Term.Fun (sym, args, _) ->
      if state.ac_symbols.ac |> SMap.mem sym then (
        let subterms = AC.ac_subterms t in
        dbg D_fw_ac_demod @@ lazy (sprintf "demod_term: %s = %s" (Term.to_string t) (List.X.to_string Term.to_string subterms));
        let subterms' = List.map (demod_term ~check_type:Off state parents) subterms in
        dbg D_fw_ac_demod @@ lazy (sprintf "to %s" (List.X.to_string Term.to_string subterms'));
        let t' = 
          if List.X.equal ~eq:(==) subterms subterms' then
            t
          else
            AC.mk_term sym subterms'
        in
        let t'' = rewrite_term ~check_type state parents t' in
        dbg D_fw_ac_demod @@ lazy (sprintf "and to %s" (Term.to_string t''));
        t''
      ) 
      else (
        dbg D_fw_ac_demod @@ lazy (sprintf "demod_term: %s = nonac" (Term.to_string t));
        let args = Term.arg_to_list args in
        let args' = List.map (demod_term ~check_type:Off state parents) args in
        if List.X.equal ~eq:(==) args args' then
          t
        else
          add_fun_term sym args'
      )
    | Term.Var _ -> t

  (* let demod_args (state: Set.t) parents t = 
    match t with
    | Term.Fun (sym, args, _) ->
      begin match ACDemodIndex_M.get_fwd_special Set.(state.ac_demod_index) t with
      | Some (t', clause) -> 
        dbg D_fw_ac_demod2 @@ lazy (sprintf "%s to %s (special)" (Term.to_string t) (Term.to_string t'));
        parents := clause :: !parents;
        t'
      | None -> 
        let args = Term.arg_to_list args in
        let args' = List.map (demod_term state parents) args in
        if List.X.equal ~eq:(==) args args' then
          t
        else
          add_fun_term sym args'
      end
    | Term.Var _ -> t *)

  let demod_top (state: Set.t) parents t t' clause =
    let check_type = 
      (* if no_check then Off else *)
      let module O = Options.Demod_check in
      match state.options.demod_completeness_check with
      | O.Off -> Off
      | O.Fast | O.FastOld -> Fast t'
      | O.Full | O.FullOld -> Full clause
    in 
    demod_term ~check_type state parents t

  let demod_subterm state parents t = 
    demod_term ~check_type:Off state parents t

  exception Return
  let simplify (state: Set.t) clause = 
    Statistics.(time sim_time_fw_ac_demod) @@ fun () ->
    dbg D_fw_ac_demod @@ lazy "fw_acdemod";
    if not (AC.Table.has_ac state.ac_symbols) 
    || ACDemodulationIndex.is_special_axiom clause 
    then
      Simplified clause
    else (
      let parents = ref [] in
      try
        let no_check = demod_no_check state clause in
        let lits = Clause.get_lits clause in
        (* let lits' = List.map (demod_term state parents) lits in *)
        let lits' = 
          lits |> List.filter_map (fun lit -> 
            match Term.Eq.decompose_lit_type lit with
            | Some (sign, etype, l, r) ->
              let l', r' = 
                match no_check with
                | Completeness.L r0 -> 
                  demod_top state parents l r0 clause, demod_subterm state parents r
                | Completeness.R l0 -> 
                  demod_subterm state parents l, demod_top state parents r l0 clause
                | Completeness.LR (l0,r0) -> 
                  demod_top state parents l r0 clause, demod_top state parents r l0 clause
                | Completeness.None -> 
                  demod_subterm state parents l, demod_subterm state parents r
              in
              if l' == r' then
                if sign then raise_notrace Return else None
              else if l != l' || r != r' then
                Some (add_lit_eq sign etype l' r')
              else 
                Some lit
            | None -> 
              Some (demod_subterm state parents lit)
          )
        in
        if List.X.is_empty !parents then
          Simplified clause
        else (
          Statistics.(bump_int_stat sim_fw_ac_demod);
          let source = Clause.tstp_source_ac_demodulation ~main:clause ~axioms:state.ac_symbols.axiom_list ~eqs:!parents in
          let clause' = create_clause ~normalise_eqs:true source lits' in
          dbg D_fw_ac_demod @@ lazy (sprintf "Fw ACDemod %s" (Clause.to_string_tptp clause));
          dbg D_fw_ac_demod @@ lazy (sprintf "to         %s" (Clause.to_string_tptp clause'));
          dbg_env D_fw_ac_demod (fun () -> 
            !parents |> List.iter (fun x ->
              dbg D_fw_ac_demod @@ lazy (sprintf "via        %s" (Clause.to_string_tptp x));
            )
          );
          Simplified clause'
        )
      with Return -> 
        dbg D_fw_ac_demod @@ lazy (sprintf "Fw ACDemod %s" (Clause.to_string_tptp clause));
        dbg D_fw_ac_demod @@ lazy (sprintf "to         tautology");
        dbg_env D_fw_ac_demod (fun () -> 
          !parents |> List.iter (fun x ->
            dbg D_fw_ac_demod @@ lazy (sprintf "via        %s" (Clause.to_string_tptp x));
          )
        );
        Eliminated !parents
    )

  let simplify state clause = 
    let fixpoint = true in
    if fixpoint then 
      Fw_result.fix_point (simplify state) clause
    else
      simplify state clause
end

module BwACDemod = struct
  let index = Index_tag.BwACDemod
  
  let demod (state: Set.t) eq_clause l r = 
    dbg D_bw_ac_demod @@ lazy (sprintf "Trying bw acdemod with %s -> %s" (Term.to_string l) (Term.to_string r));
    let candidates = ACDemodIndex_M.get_bwd state.ac_demod_index l in
    let results = ref [] in
    candidates |> List.iter (fun (t, subst, lst) ->
      lst |> List.iter (fun (clause, do_check) -> 
        dbg D_bw_ac_demod @@ lazy (sprintf "candidate: %s" (Clause.to_string_tptp clause));
        (* TODO: If same clause demodulated in several ways, what should do? *)
        (* if clause == eq_clause  *)
        if Clause.Bc.(clause == eq_clause) 
        || !results |> List.exists (function BwSimplified {from; _} when from == clause -> true | _ -> false)
        then () else (
          let t_subterms = AC.ac_subterms t in
          let l_subterms = AC.ac_subterms l in
          let t_subterms' = 
            l_subterms |> List.fold_left (fun acc x ->
              let x' = subst_apply subst x in
              let acc' = List.X.removeq x' acc in
              acc'
            ) t_subterms
          in
          let t_subterms'' = t_subterms' @ [subst_apply subst r] in
          let t' = AC.mk_term (Term.get_top_symb t) t_subterms'' in
          let check_type = 
            (* if no_check then FwACDemod.Off else *)
            match do_check with 
            | None -> FwACDemod.Off
            | Some r -> 
              let module O = Options.Demod_check in
              begin match state.options.demod_completeness_check with
              | O.Off -> FwACDemod.Off
              | O.Fast | O.FastOld -> FwACDemod.Fast r
              | O.Full | O.FullOld -> FwACDemod.Full clause
              end
          in
          if check_type == FwACDemod.Off 
          || FwACDemod.completeness_check 
              ~order:state.order ~subs_bck_mult:state.options.subs_bck_mult ~check_type 
              subst t_subterms t_subterms' l_subterms t t' eq_clause 
          then (
            let source = Clause.tstp_source_ac_demodulation ~main:clause ~axioms:state.ac_symbols.axiom_list ~eqs:[eq_clause] in
            let lits' = Clause.get_lits clause |> List.map (Term.replace t t') in
            let clause' = create_clause ~normalise_eqs:true source lits' in
            Statistics.(bump_int_stat sim_bw_ac_demod);
            dbg D_bw_ac_demod @@ lazy (sprintf "Bw ACDemod %s" (Clause.to_string_tptp clause));
            dbg D_bw_ac_demod @@ lazy (sprintf "to         %s" (Clause.to_string_tptp clause'));
            dbg D_bw_ac_demod @@ lazy (sprintf "via        %s" (Clause.to_string_tptp eq_clause));
            let result = BwSimplified {from=clause; into=clause'} in
            List.X.cons_ref result results
          ) else (
            dbg D_bw_ac_demod @@ lazy (sprintf "Bw acdemod completeness failed");
          )
        )
      )
    );
    !results

  let simplify (state: Set.t) eq_clause = 
    Statistics.(time sim_time_bw_ac_demod) @@ fun () ->
    match Clause.get_lits eq_clause with
    | [eq_lit] -> 
      begin match Term.Eq.decompose_atom eq_lit with
      | Some (l,r) -> 
        let condition l r = 
          SMap.mem (Term.get_top_symb l) state.ac_symbols.ac
          && Term.get_num_of_symb l > Term.get_num_of_symb r  (* TODO: replace by actual weight *)
        in
        begin match state.order.oriented eq_lit with
        | GT when condition l r ->
          demod state eq_clause l r
        | LT when condition r l ->
          demod state eq_clause r l
        | EQ -> assert false
        | _ -> []
        end
      | None -> []
      end
    | _ -> []
end



module FwGroundJoinability = struct
  (** VarOrder: graph representation *)
  module VarOrder = Orderings.VarOrder

  (** VarOrderReduced: transitive reduction (list of edges) representation
      Used in for [add_order_covered] *)
  module VarOrderReduced = struct
    type ord = L_GT | L_EQ
    type link = var * ord * var
    type t = link list

    let link_to_string (x,ord,y) = 
      sprintf "%s %c %s" 
        (Var.to_string x) (match ord with L_GT -> '>' | L_EQ -> '=') (Var.to_string y)

    let to_string l = 
      List.sort (fun (x,_,_) (y,_,_) -> Var.compare x y) l |> 
      List.X.to_string ~first:"" ~last:"" ~sep:", " link_to_string

    module LinkSet = Stdlib.Set.Make(struct
      type t = link
      let compare (x1,y1,z1) (x2,y2,z2) = 
        match y1, y2 with
        | L_EQ, L_EQ -> 
          let x1, z1 = if Var.compare x1 z1 == Ord.gt then z1, x1 else x1, z1 in
          let x2, z2 = if Var.compare x2 z2 == Ord.gt then z2, x2 else x2, z2 in
          let r1 = Var.compare x1 x2 in
          if r1 <> Ord.eq then r1 else
          let r2 = Var.compare z1 z2 in
          r2
        | L_GT, L_GT -> 
          let r1 = Var.compare x1 x2 in
          if r1 <> Ord.eq then r1 else
          let r2 = Var.compare z1 z2 in
          r2
        | L_EQ, L_GT -> Ord.lt
        | L_GT, L_EQ -> Ord.gt
      (* let compare x y = lex_combination3 () *)
    end)

    let transitive_reduction (var_order: VarOrder.t) : t = 
      (* TODO dassert correctness *)
      let[@warning "-26"] tiebreak v u = if Var.compare v u <= 0 then v,u else u,v in  (* KK: not used *)
      let vars = 
        let ref = ref [] in
        VarOrder.iter_vars var_order (fun x -> List.X.cons_ref x ref);
        dassert (fun () -> List.X.equal ~eq:(==) (List.sort Var.compare !ref) (List.rev !ref));
        List.rev !ref
      in
      let rec iter_pairs f l = 
        let rec iter_pairs' f x l = 
          match l with
          | hd::tl -> f x hd; iter_pairs' f x tl
          | [] -> ()
        in
        match l with
        | hd::tl -> iter_pairs' f hd tl; iter_pairs f tl
        | [] -> ()
      in
      let[@warning "-26"] rec iter_triples f l =  (* not used yet *)
        let rec iter_triples' f x l = 
          let rec iter_triples'' f x y l = 
            match l with
            | hd::tl -> f x y hd; iter_triples'' f x y tl
            | [] -> ()
          in
          match l with
          | hd::tl -> iter_triples'' f x hd tl; iter_triples' f x tl
          | [] -> ()
        in
        match l with
        | hd::tl -> iter_triples' f hd tl; iter_triples f tl
        | [] -> ()
      in
      (* TODO: needs much optimisation *)
      let links = ref LinkSet.empty in
      (* dbg D_fw_gjoin3 @@ lazy (sprintf "transitive_reduction: %s" (to_string @@ LinkSet.elements !links)); *)
      vars |> iter_pairs (fun v u ->
        dassert (fun () -> Var.O.(v <> u) && true);
        match VarOrder.query var_order v u with
        | GT -> links @= LinkSet.add (v,L_GT,u)
        | LT -> links @= LinkSet.add (u,L_GT,v)
        | EQ -> links @= LinkSet.add (v,L_EQ,u)
        | INC -> ()          
      );
      vars |> iter_pairs (fun v u ->
        dassert (fun () -> Var.compare v u == Ord.lt);
        dbg D_fw_gjoin3 @@ lazy (sprintf "%5s %5s         : %s" (Var.to_string v) (Var.to_string u) (to_string @@ LinkSet.elements !links));
        match VarOrder.query var_order v u with
        | GT -> 
          VarOrder.iter_vars var_order (fun w -> 
            if u == w || v == w then () else
            match VarOrder.query var_order u w with
            | GT -> links @= LinkSet.remove (v,L_GT,w)
            | EQ -> if Var.compare u w == Ord.lt then links @= LinkSet.remove (v,L_GT,w)
            | _ -> ()
          )
        | LT -> 
          VarOrder.iter_vars var_order (fun w -> 
            if v == w || u == w then () else
            match VarOrder.query var_order v w with
            | GT -> links @= LinkSet.remove (u,L_GT,w)
            | EQ -> if Var.compare v w == Ord.lt then links @= LinkSet.remove (u,L_GT,w)
            | _ -> ()
          )
        | EQ -> 
          VarOrder.iter_vars var_order (fun w -> 
            if u == w || v == w then () else
            (* match VarOrder.query var_order u w with
            | EQ -> if Var.compare u w == Ord.lt then links @= LinkSet.remove (v,L_EQ,w)
            | GT -> if Var.compare u w == Ord.lt then links @= LinkSet.remove (v,L_GT,w)
            | _ -> () *)
            match VarOrder.query var_order u w with
            | EQ -> if Var.compare u w == Ord.lt then links @= LinkSet.remove (v,L_EQ,w)
            | GT -> links @= LinkSet.remove (v,L_GT,w)
            | LT -> links @= LinkSet.remove (w,L_GT,u)
            | _ -> ()
          )
        | INC -> ()          
      );
      dbg D_fw_gjoin3 @@ lazy (sprintf "transitive_reduction: %s" (to_string @@ LinkSet.elements !links));
      LinkSet.elements !links

    let to_string_red = to_string %% transitive_reduction

    let transitive_expansion (x: t) : VarOrder.t = 
      (* TODO: for efficiency, unsafely assume VarOrder.t = Orderings_graph.t *)
      dbg D_fw_gjoin3 @@ lazy (sprintf "transitive_expansion: %s" (to_string x));
      let r = 
      x |> List.fold_left (fun var_order (l,ord,r) -> 
        dbg D_fw_gjoin3 @@ lazy (sprintf "transitive_expansion: %s %s" (link_to_string (l,ord,r)) (VarOrder.to_string_dbg var_order));
        match ord with
        | L_GT -> VarOrder.add_gt var_order l r |> Option.get
        | L_EQ -> VarOrder.add_eq var_order l r |> Option.get
      ) VarOrder.empty
      in
      dbg D_fw_gjoin3 @@ lazy (sprintf "transitive_expansion: %s" (VarOrder.to_string_dbg r));
      dbg D_fw_gjoin3 @@ lazy (sprintf "transitive_expansion: %s" (to_string_red r));
      (* dassert (fun () -> LinkSet.equal (LinkSet.of_list x) (LinkSet.of_list @@ transitive_reduction r));  (* Not necessarily true *) *)
      r
  end

  (** Stores parents for proof *)
  module Parents = struct
    type t = {
      mutable parents: BCSet.t;
      mutable parents_ac: SSet.t;
    }

    let empty () = {
      parents = BCSet.empty;
      parents_ac = SSet.empty;
    }

    let add state clause = 
      state.parents <- state.parents |> BCSet.add clause

    let add_ac state sym = 
      state.parents_ac <- state.parents_ac |> SSet.add sym

    let has_parents state = 
      not (BCSet.is_empty state.parents) || not (SSet.is_empty state.parents_ac)

    let merge a b =
      a.parents    <- BCSet.union a.parents b.parents;
      a.parents_ac <- SSet.union a.parents_ac b.parents_ac

    let get state (set: Set.t) = 
      let parents = BCSet.elements state.parents in
      let parents_ac = set.ac_symbols.ac |> SMap.bindings |> List.filter (fun (x,_) -> SSet.mem x state.parents_ac) |> List.concat_map (fun (_,(x,y)) -> [x;y]) in
      parents_ac @ parents 
  end

  (** The state of one attempt of proving ground joinability *)
  module State = struct
    (* module VarOrdersTrie = struct end *)

    type t = {
      (* var_orders: (term * term * VarOrder.t * Completeness.t) Stack.t; *)
      var_orders_gt: (term * term * VarOrder.t * Completeness.t) Stack.t;
      var_orders_eq: (term * term * VarOrder.t * Completeness.t) Stack.t;
      parents: Parents.t;
      vars: int;
    }

    let make l r complete vars () = {
      (* var_orders = (Stack.create () |> tap (Stack.push (l, r, VarOrder.empty, complete))); *)
      var_orders_gt = (Stack.create () |> tap (Stack.push (l, r, VarOrder.empty, complete)));
      var_orders_eq = (Stack.create ());
      parents = Parents.empty ();
      vars = vars;
    }

    let add_order_covered state var_order l r complete var_order' = 
      (* The idear is: 
           let comm = var_order' ∩ var_order
           let diff = var_order' \ var_order  
         then we need to check all orders
           { comm ∪ compl(x) | x ∈ diff }
         examples:
           var_order var_order' result (i.e. orders to check)
           ()        x<y        x=y
                                x>y
           x<y       x<y y<z    x<y y=z
                                x<y y>z
           ()        x<y y<z    x>y
                                x=y
                                y=z
                                y>z *)
      dbg D_fw_gjoin @@ lazy (sprintf "add_order_covered: var_order = %s ; var_order' = %s" 
        (VarOrderReduced.to_string_red var_order) (VarOrderReduced.to_string_red var_order')
      );
      (* dbg D_fw_gjoin @@ lazy (sprintf "add_order_covered:\nvar_order\n%s\nvar_order'\n%s" 
        (VarOrder.to_string_dbg var_order) (VarOrder.to_string_dbg var_order')
      ); *)
      let module R = VarOrderReduced in
      (* dassert (fun () -> 
        var_order_reduced |> List.for_all (fun (l,ord,r) -> match ord with R.L_GT -> VarOrder.query var_order' l r == GT )
      ) *)
      ignore (R.transitive_reduction var_order' |> List.fold_left (fun var_order_acc (x,ord,y) -> 
        (* TODO: is it possible that there are duplicate orders here? investigate closely *)
        let[@inline] get x y = VarOrder.query var_order_acc x y in
        let[@inline] gt x y = VarOrder.add_gt var_order_acc x y |> Option.get in
        let[@inline] eq x y = VarOrder.add_eq var_order_acc x y |> Option.get in
        match ord with
        | R.L_GT -> 
          if get x y != GT then (
            Stack.push (l, r, eq x y, complete) state.var_orders_eq; 
            Stack.push (l, r, gt y x, complete) state.var_orders_gt;
            dbg D_fw_gjoin @@ lazy (sprintf "              add: %s"  (R.to_string_red @@ eq x y));
            dbg D_fw_gjoin @@ lazy (sprintf "              add: %s"  (R.to_string_red @@ gt y x));
            gt x y
          ) else var_order_acc
        | R.L_EQ -> 
          if get x y != EQ then (
            Stack.push (l, r, gt x y, complete) state.var_orders_gt; 
            Stack.push (l, r, gt y x, complete) state.var_orders_gt;
            dbg D_fw_gjoin @@ lazy (sprintf "              add: %s"  (R.to_string_red @@ gt x y));
            dbg D_fw_gjoin @@ lazy (sprintf "              add: %s"  (R.to_string_red @@ gt y x));
            eq x y
          ) else var_order_acc
        (* dbg D_fw_gjoin @@ lazy (sprintf "              add: %s"  (VarOrder.to_string_dbg @@ List.nth result 0));
        dbg D_fw_gjoin @@ lazy (sprintf "              add: %s"  (VarOrder.to_string_dbg @@ List.nth result 1)); *)
      ) var_order)

    let get_order_not_covered state = 
      (* TODO: intuitively, orders with EQ are more likely to be found joinable. therefore, should we prefer to leave those for last? key: if the term is joinable, we will have to check all of them anyway, and if it's not then we want to realise it as soon as possible *)
      match Stack.pop state.var_orders_gt with
      | x -> Some x
      | exception Stack.Empty -> 
      match Stack.pop state.var_orders_eq with
      | x -> Some x
      | exception Stack.Empty -> None

    let stack_iter f state = 
      Stack.iter f state.var_orders_gt;
      Stack.iter f state.var_orders_eq
  end



  (* * Core functions * *)

  (* Settings: *)
  let use_noninc = true
  let use_ac = true
  let use_lightnorm = true
  let exhaust_term_before_args = true
  let exhaust_term_after_args  = true
  let recurse_into_exhausted_subterms = true
  let extend_term_before_args = true && false
  let assert_we_get_fixpoint = false
  let exhaust_before_join_all = false

  exception Return_gjoin_extend of (term * VarOrder.t * clause (* * subst *))
  exception Return_gjoin_exhaust of (term * clause (* * subst *))
  type ('a,'b) either = Left of 'a | Right of 'b  (* TODO remove when migrate to ocaml 4.12 *)

  (* TODO: move these two to set as we have for demod *)

  (** Core function that tries to force lhs>rhs under some extension of order_var *)
  let func_extend (order: Orderings.t) order_var s = 
    dbg D_fw_gjoin2 @@ lazy (sprintf "  func_extend %s" (Term.to_string s));
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        if use_noninc || order.oriented eq_lit == INC then (
          let l,r = Term.Eq.decompose_atom_exn eq_lit in
          let l,r = Term.Eq.regularize_pos pos l r in
          dbg D_fw_gjoin2 @@ lazy (sprintf "    trying %s = %s" (Term.to_string l) (Term.to_string r));
          dassert (fun () -> _term == l);
          dassert (fun () -> s == subst_apply subst l);
          let s' = subst_apply subst r in
          match order.terms_var_gt order_var s s' with
          | Some order_var' -> 
            dbg D_fw_gjoin @@ lazy (sprintf "    %s > %s via ( %s = %s ) under%s" 
              (Term.to_string s) (Term.to_string s') (Term.to_string l) (Term.to_string r) (VarOrder.to_string_dbg order_var') 
            );
            raise_notrace @@ Return_gjoin_extend (s', order_var', eq_clause)
          | None -> ()
        )
      )

  (** Core function that uses order_var but does not try to extend *)
  let func_exhaust (order: Orderings.t) order_var s = 
    dbg D_fw_gjoin2 @@ lazy (sprintf "  func_exhaust %s" (Term.to_string s));
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        if use_noninc || order.oriented eq_lit == INC then (
          let l,r = Term.Eq.decompose_atom_exn eq_lit in
          let l,r = Term.Eq.regularize_pos pos l r in
          dbg D_fw_gjoin2 @@ lazy (sprintf "    trying %s = %s" (Term.to_string l) (Term.to_string r));
          dassert (fun () -> _term == l);
          dassert (fun () -> s == subst_apply subst l);
          let s' = subst_apply subst r in
          match order.terms_var order_var s s' with
          | GT -> 
            dbg D_fw_gjoin @@ lazy (sprintf "    %s > %s via ( %s = %s ) under%s" 
              (Term.to_string s) (Term.to_string s') (Term.to_string l) (Term.to_string r) (VarOrder.to_string_dbg order_var) 
            );
            raise_notrace @@ Return_gjoin_exhaust (s', eq_clause)
          | _ -> ()
        )
      )

  (** As [func_extend], but for use at the top level. *)
  let func_top_extend (order: Orderings.t) order_var clause s k_completeness = 
    dbg D_fw_gjoin2 @@ lazy (sprintf "  func_top_extend %s" (Term.to_string s));
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        (* dbg D_fw_gjoin2 @@ lazy (sprintf "  cec %s %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp eq_clause)); *)
        if Clause.Bc.(clause == eq_clause) then () else
        if use_noninc || order.oriented eq_lit == INC then (
          let l,r = Term.Eq.decompose_atom_exn eq_lit in
          let l,r = Term.Eq.regularize_pos pos l r in
          dbg D_fw_gjoin2 @@ lazy (sprintf "    trying %s = %s" (Term.to_string l) (Term.to_string r));
          dassert (fun () -> _term == l);
          dassert (fun () -> s == subst_apply subst l);
          let s' = subst_apply subst r in
          match order.terms_var_gt order_var s s' with
          | Some order_var' -> 
            dbg D_fw_gjoin @@ lazy (sprintf "    %s > %s via ( %s = %s ) under%s" 
              (Term.to_string s) (Term.to_string s') (Term.to_string l) (Term.to_string r) (VarOrder.to_string_dbg order_var') 
            );
            (* output_list @= List.cons (s', order_var', eq_clause, subst) *)
            begin match k_completeness s' order_var' eq_clause subst with
            | true -> raise_notrace @@ Return_gjoin_extend (s', order_var', eq_clause)
            | false -> ()
            end
          | None -> ()
        )
      )

  (** As [func_exhaust], but for use at the top level. *)
  let func_top_exhaust (order: Orderings.t) order_var clause s k_completeness = 
    dbg D_fw_gjoin2 @@ lazy (sprintf "  func_top_exhaust %s" (Term.to_string s));
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        (* dbg D_fw_gjoin2 @@ lazy (sprintf "  cec %s %s" (Clause.to_string_tptp clause) (Clause.to_string_tptp eq_clause)); *)
        if Clause.Bc.(clause == eq_clause) then () else
        if use_noninc || order.oriented eq_lit == INC then (
          let l,r = Term.Eq.decompose_atom_exn eq_lit in
          let l,r = Term.Eq.regularize_pos pos l r in
          dbg D_fw_gjoin2 @@ lazy (sprintf "    trying %s = %s" (Term.to_string l) (Term.to_string r));
          dassert (fun () -> _term == l);
          dassert (fun () -> s == subst_apply subst l);
          let s' = subst_apply subst r in
          match order.terms_var order_var s s' with
          | GT -> 
            dbg D_fw_gjoin @@ lazy (sprintf "    %s > %s via ( %s = %s ) under%s" 
              (Term.to_string s) (Term.to_string s') (Term.to_string l) (Term.to_string r) (VarOrder.to_string_dbg order_var) 
            );
            (* output_list @= List.cons (s', eq_clause, subst) *)
            begin match k_completeness s' eq_clause subst with
            | true -> raise_notrace @@ Return_gjoin_exhaust (s', eq_clause)
            | false -> ()
            end
          | _ -> ()
        )
      )

  let func_ac_extend (state: Set.t) order_var s = 
    (* Checks if any two ac-subterms of s can be swapped by an extension of order_var. *)
    (* TODO: optimise *)
    (* let order_var' = ref (VarOrder.empty) in
    let s' = 
      AC.normalise_ac_complete 
        ~order_terms:(fun s t -> match state.order.terms_var_gt order_var s t with | Some o -> order_var' := o; GT) ~order_uid:state.order.uid 
        state.ac_symbols.ac s 
    in *)
    let args = AC.ac_subterms s in
    let args = Array.of_list args in
    let n = Array.length args in
    let rec loop_i (* args *) i n = 
      let rec loop_j (* args *) i j n = 
        if j = n then
          loop_i (i+1) n
        else
          match state.order.terms_var_gt order_var args.(i) args.(j) with
          | Some order_var' -> 
            let tmp = args.(i) in
            args.(i) <- args.(j);
            args.(j) <- tmp;
            AC.mk_term (Term.get_top_symb s) (Array.to_list args), order_var'
          | None -> 
            loop_j i (j+1) n
      in
      if i = n then
        s, order_var
      else
        loop_j i (i+1) n
    in
    loop_i 0 n

  let func_ac_exhaust (state: Set.t) order_var s = 
    let s' = 
      AC.normalise_ac_complete 
        ~order_terms:(state.order.terms_var order_var) ~order_uid:(-1)(*state.order.uid*) 
        state.ac_symbols.ac s 
    in
    s'




  (* * Exhaustive traversal * *)

  (** Demodulate top (no ordering checks, no recurse into subterms) *)
  let exhaust_top (state: Set.t) parents order_var clause s k_completeness = 
    let[@inline] rewrite (state: Set.t) parents order_var s sym = 
      (* Is AC *)
      let s' = 
        if use_ac && state.ac_symbols.ac |> SMap.mem sym then (
          let s' = func_ac_exhaust state order_var s in
          if s' != s then (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: %s" (Term.to_string s'));
            Parents.add_ac parents sym;
          ) else (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: same");
          );
          s'  (* AC rewrites always complete *)
        ) else (
          s
        )
      in
      let s' = 
        if use_lightnorm && not state.options.demod_use_ground then
          let s'', parents' = LightNormIndex_M.normalise_term Set.(state.lightnorm_index) s' in
          if s'' != s' 
          && BCSet.for_all (Clause.Bc.nequal clause) parents' 
          && (s' != s || k_completeness s'' (**)clause(**) (Subst.create ())) then (
            dbg D_fw_gjoin @@ lazy (sprintf "  ln into: %s" (Term.to_string s''));
            parents' |> BCSet.iter (fun c -> dbg D_fw_gjoin @@ lazy (sprintf "    ln aaaaaa: %s" (Clause.to_string_tptp c)));
            parents.parents <- BCSet.union parents' parents.parents; s''
          ) else (
            dbg D_fw_gjoin @@ lazy (sprintf "  ln into: same");
            s'
          )
        else
          s'
      in
      (* Not AC *)
      try 
        (* begin match FwDemodIndex_M.iter_fwd(*_caching*) index s (func state order_var) with
        | Some (s', order_var', parent_clause, subst) -> *)
        let k_completeness_none s' eq_clause subst = true in
        if s' == s then
          FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func_top_exhaust state.order order_var clause s' k_completeness)
        else
          FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func_top_exhaust state.order order_var clause s' k_completeness_none);
        dbg D_fw_gjoin @@ lazy (sprintf "  into: same");
        s'
      with Return_gjoin_exhaust (s'', parent_clause) -> 
        dbg D_fw_gjoin @@ lazy (sprintf "  into: %s" (Term.to_string s''));
        dassert (fun () -> s' != s'');
        Parents.add parents parent_clause;
        s''
    in

    match s with
    | Term.Fun (sym, _, _) -> 
      dbg D_fw_gjoin @@ lazy (sprintf "  exhaust_top: %s" (Term.to_string s));
      rewrite state parents order_var s sym
    | Term.Var _ -> 
      s

  (** Demodulate recursively (no ordering checks) *)
  let rec exhaust_subterms (state: Set.t) parents order_var sym' s = 
    let[@inline] rewrite (state: Set.t) parents order_var sym' s sym = 
      (* Is AC *)
      let s' = 
        if use_ac && sym' != sym && state.ac_symbols.ac |> SMap.mem sym then (
          let s' = func_ac_exhaust state order_var s in
          if s' != s then (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: %s" (Term.to_string s'));
            Parents.add_ac parents sym;
          ) else (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: same");
          );
          s'
        ) else (
          s
        )
      in
      let s' = 
        if use_lightnorm && not state.options.demod_use_ground then 
          let s'', parents' = LightNormIndex_M.normalise_term Set.(state.lightnorm_index) s' in
          if s'' != s' then (
            (* dassert (fun () -> BCSet.for_all (Clause.Bc.nequal clause) parents'); *)
            dbg D_fw_gjoin @@ lazy (sprintf "  ln into: %s" (Term.to_string s'')); 
            parents' |> BCSet.iter (fun c -> dbg D_fw_gjoin @@ lazy (sprintf "    ln aaaaaa: %s" (Clause.to_string_tptp c)));
            parents.parents <- BCSet.union parents' parents.parents; s''
          ) else s'
        else 
          s'
      in
      (* Not AC *)
      try 
        (* begin match FwDemodIndex_M.iter_fwd(*_caching*) index s (func state order_var) with
        | Some (s', order_var', parent_clause, subst) -> *)
        FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func_exhaust state.order order_var s');
        dbg D_fw_gjoin @@ lazy (sprintf "  into: same");
        s'
      with Return_gjoin_exhaust (s'', parent_clause) -> 
        dbg D_fw_gjoin @@ lazy (sprintf "  into: %s" (Term.to_string s''));
        dassert (fun () -> s' != s'');
        Parents.add parents parent_clause;
        s''
    in
    
    match s with
    | Term.Fun (sym, args, _) ->
      dbg D_fw_gjoin @@ lazy (sprintf "  exhaust_subterms: %s" (Term.to_string s));
      (* First, demodulate term itself *)
      let s' = 
        if exhaust_term_before_args then 
          rewrite state parents order_var sym' s sym
        else 
          s
      in
      (* If successful, try again until fixpoint *)
      if s' != s then (
        if recurse_into_exhausted_subterms then
          exhaust_subterms state parents order_var sym s'
        else
          s'
      ) 
      (* Then demodulate all args *)
      else (
        let args = dassert (fun () -> s == s'); Term.arg_to_list args in
        let args' = List.map (exhaust_subterms state parents order_var sym) args in
        (* If same, then we're done *)
        if List.for_all2 (==) args args' then
          s'
        (* If changed, then we also try to re-demodulate the term itself *)
        else
          let s'' = add_fun_term sym args' in
          let s''' = 
            if exhaust_term_after_args then 
              rewrite state parents order_var sym' s'' sym
            else 
              s''
          in
          (* And again, if successful, try again until fixpoint *)
          if recurse_into_exhausted_subterms && s''' != s'' then 
            exhaust_subterms state parents order_var sym s'''
          else
            s'''
      )
    | Term.Var _ -> s
  let exhaust_subterms (state: Set.t) parents order_var s = 
    exhaust_subterms state parents order_var (Obj.magic 0) s

  (** Demodulate args recursively (no ordering checks) *)
  let exhaust_args (state: Set.t) parents order_var s =
    match s with
    | Term.Fun (sym, args, _) ->
      dbg D_fw_gjoin @@ lazy (sprintf "  exhaust_args: %s" (Term.to_string s));
      let args = Term.arg_to_list args in
      let args' = List.map (exhaust_subterms state parents order_var) args in
      if List.for_all2 (==) args args' then
        s
      else
        add_fun_term sym args'
    | Term.Var _ -> s



  (* * Once traversal * *)

  let extend_top (state: Set.t) parents order_var clause s k_completeness = 
    let[@inline] rewrite (state: Set.t) parents order_var s sym = 
      (* Is AC *)
      let (s', order_var') as pair = 
        if use_ac && state.ac_symbols.ac |> SMap.mem sym then (
          let s', order_var' = func_ac_extend state order_var s in
          if s' != s then (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: %s" (Term.to_string s'));
            Parents.add_ac parents sym;
          ) else (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: same");
            dassert (fun () -> order_var' == order_var);
          );
          s', order_var'
        ) else (
          s, order_var
        )
      in
      if s' != s then s', order_var'
      (* Not AC *)
      else try
        (* begin match FwDemodIndex_M.iter_fwd(*_caching*) index s (func state order_var) with
        | Some (s', order_var', parent_clause, subst) -> *)
        FwDemodIndex_M.iter_fwd state.fw_demod_index s (func_top_extend state.order order_var clause s k_completeness);
        dbg D_fw_gjoin @@ lazy (sprintf "  into: same");
        s, order_var
      with Return_gjoin_extend (s', order_var', parent_clause) -> 
        dbg D_fw_gjoin @@ lazy (sprintf "  into: %s" (Term.to_string s'));
        dassert (fun () -> s != s');
        dassert (fun () -> parents.parents |> BCSet.mem parent_clause);
        s', order_var'
    in

    match s with
    | Term.Fun (sym, _, _) -> 
      dbg D_fw_gjoin @@ lazy (sprintf "  extend_top: %s" (Term.to_string s));
      rewrite state parents order_var s sym
    | Term.Var _ -> 
      s, order_var

  let rec loop_subterms (state: Set.t) parents order_var sym' args = 
    let rec loop (state: Set.t) parents order_var sym' original_args acc args = 
      match args with
      | [] -> original_args, order_var
      | hd::tl -> 
        let hd', order_var' = extend_subterms state parents order_var sym' hd in
        if hd' != hd then (
          dassert (fun () -> true || order_var != order_var');
          (List.rev acc @ hd' :: tl), order_var'
        ) else (
          dassert (fun () -> order_var == order_var');
          loop state parents order_var sym' original_args (hd::acc) tl
        )
    in
    loop state parents order_var sym' args [] args

  and extend_subterms (state: Set.t) parents order_var sym' s = 
    let rewrite (state: Set.t) parents order_var sym' s sym = 
      (* Is AC *)
      let (s', order_var') as pair = 
        if use_ac && sym' != sym && state.ac_symbols.ac |> SMap.mem sym then (
          let (s', order_var') as pair = func_ac_extend state order_var s in
          if s' != s then (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: %s" (Term.to_string s'));
            Parents.add_ac parents sym;
          ) else (
            dbg D_fw_gjoin @@ lazy (sprintf "  ac into: same");
          );
          pair
        ) else (
          s, order_var
        )
      in
      if s' != s then pair
      (* Not AC *)
      else try
        (* match FwDemodIndex_M.iter_fwd(*_caching*) index s (func state order_var) with
        | Some (s', order_var', parent_clause, subst) -> *)
        FwDemodIndex_M.iter_fwd state.fw_demod_index s (func_extend state.order order_var s);
        dbg D_fw_gjoin @@ lazy (sprintf "  into: same");
        s, order_var
      with Return_gjoin_extend (s', order_var', parent_clause) -> 
        dbg D_fw_gjoin @@ lazy (sprintf "  into: %s" (Term.to_string s'));
        dassert (fun () -> s != s');
        Parents.add parents parent_clause;
        s', order_var'
    in

    match s with
    | Term.Fun (sym, args, _) -> 
      dbg D_fw_gjoin @@ lazy (sprintf "  extend_subterms: %s" (Term.to_string s));
      (* Top-down *)
      if extend_term_before_args then (
        let s', order_var' = rewrite state parents order_var sym' s sym in
        if s' != s then (
          dassert (fun () -> true || order_var != order_var');
          s', order_var'
        ) else (
          let args = Term.arg_to_list args in
          let args', order_var' = loop_subterms state parents order_var sym args in
          if args' != args then (
            dassert (fun () -> not @@ List.X.equal ~eq:(==) args args');
            dassert (fun () -> true || order_var != order_var');
            let s' = add_fun_term sym args' in
            s', order_var'
          ) else (
            dassert (fun () -> order_var == order_var');
            s, order_var
          )
        )
      ) 
      (* Bottom-up *)
      else (
        let args = Term.arg_to_list args in
        let args', order_var' = loop_subterms state parents order_var sym args in
        if args' != args then (
          dassert (fun () -> not @@ List.X.equal ~eq:(==) args args');
          dassert (fun () -> true || order_var != order_var');
          let s' = add_fun_term sym args' in
          s', order_var'
        ) else (
          dassert (fun () -> order_var == order_var');
          rewrite state parents order_var sym' s sym
        )
      )
    | Term.Var _ -> 
      s, order_var

  and extend_args (state: Set.t) parents order_var s = 
    match s with
    | Term.Fun (sym, args, _) -> 
      dbg D_fw_gjoin @@ lazy (sprintf "  extend_args: %s" (Term.to_string s));
      let args = Term.arg_to_list args in
      let args', order_var' = loop_subterms state parents order_var (Obj.magic 0) args in
      if args' != args then (
        dassert (fun () -> not @@ List.X.equal ~eq:(==) args args');
        dassert (fun () -> true || order_var != order_var');
        add_fun_term sym args', order_var'
      ) else (
        dassert (fun () -> order_var == order_var');
        s, order_var
      )
    | Term.Var _ -> 
      s, order_var



  let completeness_check ~order_terms l' r subst =
    (* match subst with Some subst -> *)
    dbg D_fw_gjoin @@ lazy "  really checking gjoin order (fast + renaming)";
    dbg D_fw_gjoin2 @@ lazy (sprintf "  Rules: %s cannot be a renaming" (Subst.to_string subst));
    dbg D_fw_gjoin2 @@ lazy (sprintf "  or     %s < %s" (Term.to_string l') (Term.to_string r));
    dbg D_fw_gjoin2 @@ lazy (sprintf "  result: %B %B" (not (Subst.is_renaming subst)) (order_terms r l' == GT));
    not (Subst.is_renaming subst)
    || order_terms r l' == GT



  let norm_eq order_var s = 
    let map = ref VMap.empty in
    VarOrder.iter_relations order_var (fun x y r -> 
      if r == EQ then 
        let x,y = if Var.compare x y == Ord.lt then x,y else y,x in
        map @= VMap.update y (function Some x' -> if Var.compare x x' == Ord.lt then Some x else Some x' | None -> Some x)
    );
    let rec loop map s = 
      match s with
      | Term.Var (var, _) -> 
        (match map |> VMap.find_opt var with Some var' -> add_var_term var' | None -> s)
      | Term.Fun (sym, args, _) -> 
        let args = Term.arg_to_list args in
        let args' = List.map (loop map) args in
        if List.X.equal ~eq:(==) args args' then
          s
        else
          add_fun_term sym args'

    in
    if VMap.is_empty !map then s else loop !map s



  (** Attempts to rewrite either [l] and [r] via some equation in [index] 
      under an ordering which is an extension of [order_var]. This either 
      succeeds, returning [Some (l', r', order_var')], or fails, returning 
      [None]. Writes parent clauses to [parents]. *)
  let join_extend (state: Set.t) clause parents complete order_var l r = 
    dbg D_fw_gjoin @@ lazy (sprintf "join_extend: %s %s | %s" (Term.to_string l) (Term.to_string r) (VarOrderReduced.to_string_red order_var));
    dassert (fun () -> l != r);
    dassert (fun () -> state.order.terms_var order_var l r != EQ);
    (* TODO randomize l/r attempt order? otherwise we are biasing this towards rewriting l *)
    let[@inline] do_check is_inc l r0 = 
      let extend_top_check state parents order_var clause is_inc l r0 = 
        extend_top state parents order_var clause l (fun l' order_var' eq_clause subst -> 
          dassert (fun () -> l' != l);
          if (is_inc && order_var' != order_var && state.order.terms_var order_var' l r0 == LT)  (* < or ≤ ? *)
          || (completeness_check ~order_terms:(state.order.terms_var order_var') l' r0 subst) then 
            (Parents.add parents eq_clause; true) 
          else 
            false
        )
      in
      let (l', _order_var') as res = extend_args state parents order_var l in
      if l' != l then
        res
      else 
        extend_top_check state parents order_var clause is_inc l r0
    in
    let[@inline] do_nocheck r = 
      (* extend_subterms state parents order_var (Obj.magic 0) l; *)
      let extend_top_nocheck state parents order_var clause l = 
        extend_top state parents order_var clause l (fun l' order_var' eq_clause subst -> 
          dassert (fun () -> l' != l);
          Parents.add parents eq_clause; true
        )
      in
      let (r', order_var') as res = extend_args state parents order_var r in
      if r' != r then
        res
      else 
        extend_top_nocheck state parents order_var clause r
    in

    let l', order_var', complete' = 
      match complete with 
      | Completeness.L r0      -> let l', order_var' = do_check false l r0 in l', order_var', (if l' != l then Completeness.None else complete)
      | Completeness.LR (_,r0) -> let l', order_var' = do_check true  l r0 in l', order_var', (if l' != l then Completeness.R r0 else complete)
      | Completeness.R _ | Completeness.None -> let l', order_var' = do_nocheck l in l', order_var', complete
    in
    match l' != l with
    | true ->
      dbg D_fw_gjoin @@ lazy (sprintf "into_extend: l' = %s | %s" (Term.to_string l') (VarOrderReduced.to_string_red order_var'));
      dassert (fun () -> order_var' != order_var);
      Some (l', r, order_var', complete')
    | false ->
      dassert (fun () -> order_var' == order_var);

    let r', order_var', complete' = 
      match complete with 
      | Completeness.R l0      -> let r', order_var' = do_check false r l0 in r', order_var', (if r' != r then Completeness.None else complete)
      | Completeness.LR (l0,_) -> let r', order_var' = do_check true  r l0 in r', order_var', (if r' != r then Completeness.L l0 else complete)
      | Completeness.L _ | Completeness.None -> let r', order_var' = do_nocheck r in r', order_var', complete
    in
    match r' != r with
    | true -> 
      dbg D_fw_gjoin @@ lazy (sprintf "into_extend: r' = %s | %s" (Term.to_string r') (VarOrderReduced.to_string_red order_var'));
      dassert (fun () -> order_var' != order_var);
      Some (l, r', order_var', complete')
    | false -> 
      dbg D_fw_gjoin @@ lazy (sprintf "into_extend: cannot extend");
      dassert (fun () -> order_var' == order_var);
      None

  (** Attempts to rewrite [l] and [r] via equations in [index] under an 
      order on variables [order_var]. Returns the rewritten terms and whether 
      or not they are equal modulo [order_var]. *)
  let join_exhaustively (state: Set.t) clause parents complete order_var l r = 
    dbg D_fw_gjoin @@ lazy (sprintf "join_exhaustively: %s %s" (Term.to_string l) (Term.to_string r));
    let cmp = state.order.terms_var order_var l r in
    if cmp == EQ then (
      dbg D_fw_gjoin @@ lazy (sprintf "into_exhaustively: EQ");
      true, l, r, complete
    ) else (
      let l = norm_eq order_var l in
      let r = norm_eq order_var r in
      let complete = 
        match complete with
        | Completeness.None | Completeness.L _ | Completeness.R _ -> complete
        | Completeness.LR (l0,r0) -> 
          (* dassert (fun () -> (l == l0) && (r == r0)); *)
          begin match cmp with 
          | GT -> dbg D_fw_gjoin @@ lazy "updated LR to L"; Completeness.L r0 
          | LT -> dbg D_fw_gjoin @@ lazy "updated LR to R"; Completeness.R l0 
          | INC -> complete 
          | EQ -> assert false
          end
      in
      let l', r', complete' = 
        (* if complete then *)
        (* match complete with
        | Some lit ->  *)
        let dmc[@inline] = completeness_check ~order_terms:(state.order.terms_var order_var) in
        let[@inline] exhaust_top_check state parents order_var clause l r0 = 
          exhaust_top state parents order_var clause l (fun l' eq_clause subst -> 
            dassert (fun () -> l' != l);
            if dmc l' r0 subst then true else false
          )
        in
        let[@inline] exhaust_top_nocheck state parents order_var clause l = 
          exhaust_top state parents order_var clause l (fun l' eq_clause subst -> 
            dassert (fun () -> l' != l);
            true
          )
        in
        let[@inline] do_check l r0 = 
          let l' = exhaust_top_check state parents order_var clause l r0 in
          let l' = 
            if recurse_into_exhausted_subterms && l' != l then
              fix_point (exhaust_top_nocheck state parents order_var clause) l'
            else 
              l'
          in
          let l'' = exhaust_args state parents order_var l' in
          let l''' = 
            if l'' != l' then 
              if recurse_into_exhausted_subterms then 
                (* fix_point (exhaust_subterms state parents order_var) l'' *)
                fix_point (exhaust_args state parents order_var %% exhaust_top_nocheck state parents order_var clause) l''
              else
                exhaust_top_nocheck state parents order_var clause l''
            else
              l''
          in
          l'''
        in
        let[@inline] do_nocheck r = 
          (* exhaust_subterms state parents order_var r *)
          if recurse_into_exhausted_subterms then 
            fix_point (exhaust_args state parents order_var %% exhaust_top_nocheck state parents order_var clause) r
          else
            (exhaust_args state parents order_var %% exhaust_top_nocheck state parents order_var clause) r
        in
        match complete with
        | Completeness.L r0 -> 
          let l' = do_check l r0 in
          let r' = do_nocheck r in
          l', r', if l' != l then Completeness.None else complete
        | Completeness.R l0 -> 
          let l' = do_nocheck l in
          let r' = do_check r l0 in
          l', r', if r' != r then Completeness.None else complete
        | Completeness.LR (l0,r0) -> 
          let l' = do_check l r0 in
          let r' = do_check r l0 in
          let complete' = 
            match l' != l, r' != r with 
            | false, false -> complete
            | false, true -> Completeness.L r0
            | true, false -> Completeness.R l0
            | true, true -> Completeness.None
          in
          l', r', complete'
        | Completeness.None -> 
          let l' = do_nocheck l in
          let r' = do_nocheck r in
          l', r', Completeness.None
      in
      if l' != l || r' != r then (
        let equal = state.order.terms_var order_var l' r' == EQ in
        dbg D_fw_gjoin @@ lazy (sprintf "into_exhaustively: %s %s %s" (Term.to_string l') (Term.to_string r') (if equal then "EQ" else ""));
        equal, l', r', complete'
      ) else (
        dbg D_fw_gjoin @@ lazy (sprintf "into_exhaustively: same");
        false, l, r, complete
      )
    )

  (** The main loop of the algorithm. Returns [Some parents] if [l] and [r] are 
      ground joinable wrt. the clauses in [set.fw_demod_index], or [None] if not. *)
  let join_all (set: Set.t) clause complete l r = 
    (* Start with an empty variable ordering *)
    let vars = Term.(add_var_set (get_var_set l) r) |> VSet.cardinal in
    let state = State.make l r complete vars () in
    (* Loop as follows *)
    let rec loop_outer (set: Set.t) (state: State.t) (* l0 r0 *) = 
      dbg D_fw_gjoin @@ lazy (sprintf "loop_outer" (*Term.to_string l0) (Term.to_string r0*));
      dbg_env D_fw_gjoin (fun () -> 
        state |> State.stack_iter (fun (l,r,x,c) -> 
          dbg D_fw_gjoin @@ lazy (sprintf "stack (%s) %s | %s = %s" (Completeness.tag_to_string c) (VarOrderReduced.to_string_red x) (Term.to_string l) (Term.to_string r))
          (* dbg D_fw_gjoin @@ lazy (sprintf "stack (%s) %s | %s = %s" (Completeness.tag_to_string c) (VarOrder.to_string_dbg x) (Term.to_string l) (Term.to_string r)) *)
        )
      );
      (* Try to find order not covered *)
      match State.get_order_not_covered state with
      (* No such order, we are done *)
      | None -> Some state.parents
      (* Some such order, try to show joinability in this order, or at least an extension thereof *)
      | Some (l, r, order_var, complete) -> 
        (* Try to reduce l and r as much as possible, with order_var *)
        let equal, l', r', complete' = join_exhaustively set clause state.parents complete order_var l r in
        (* Either they are joinable, and we have this ordering covered and go look into the next one *)
        if equal then (
          (* State.add_order_covered state order_var order_var complete'; *)
          loop_outer set state
        (* Or not, and we must try to extend *)
        ) else (
          (* By looping in this way *)
          let rec loop_inner (set: Set.t) (state: State.t) order_var0 l0 r0 complete0 order_var l r complete = 
            (* Search for a rewrite that can be done by extending that order_var *)
            match join_extend set clause state.parents complete order_var l r with
            (* No such rewrite, we fail to show joinability for this order_var, and hence we fail to show ground joinability *)
            | None -> None
            (* Some such rewrite. Try now to exhaustively rewrite with this order. *)
            | Some (l', r', order_var', complete') -> 
              dbg D_fw_gjoin @@ lazy (sprintf "rewritable under ordering %s" (VarOrderReduced.to_string_red order_var'));
              (* dbg D_fw_gjoin @@ lazy (sprintf "rewritable under ordering %s" (VarOrder.to_string_dbg order_var')); *)
              (* And again try to reduce l' and r' as much as possible, with order_var' *)
              let equal, l'', r'', complete'' = join_exhaustively set clause state.parents complete' order_var' l' r' in
              (* Either they are joinable, and we have this ordering covered and go look into the next one *)
              if equal then (
                State.add_order_covered state order_var0 l0 r0 complete0 order_var';
                loop_outer set state
              (* Or not, and we must try to extend *)
              ) else (
                if VarOrder.is_total order_var' state.vars then
                  None
                else
                  loop_inner set state 
                    order_var0 l0  r0  complete0 
                    order_var' l'' r'' complete''
              )
          in
          loop_inner set state 
            order_var l' r' complete' 
            order_var l' r' complete'
        )
    in
    loop_outer set state



  exception Tautology_exn of Parents.t

  let simplify (state: Set.t) clause = 
    Statistics.(time sim_time_fw_gjoin) @@ fun () ->
    if Clause.is_ac_axiom clause then Simplified clause else
    try
      let parents = Parents.empty () in
      (* let complete = 
        if state.options.demod_completeness_check == Options.Demod_check.Off then Completeness.None else
        match Clause.get_lits clause with 
        | [lit] (* when Term.Eq.is_pos_eq x *) -> 
          begin match Term.Eq.decompose_atom lit with
          | Some (l,r) -> 
            if r == SystemDBs.top_term then Completeness.None else
            begin match state.order.oriented lit with
            | GT -> Completeness.L r
            | LT -> Completeness.R l
            | INC -> Completeness.LR (l,r)
            | EQ -> assert false
            end
          | None -> Completeness.None
          end
        | _ -> Completeness.None
      in
      dassert (fun () -> not (Clause.demod_no_check clause) || complete == Completeness.None); *)
      let complete = demod_no_check state clause in
      let lits' = 
        (* TODO: look at positive literals first? Also a pertinent question for AC joinability *)
        Clause.get_lits clause |> List.filter (fun lit ->
          match Term.Eq.decompose_lit lit with
          | None -> true
          | Some (sign, l, r) -> 
            (* We assume that the term is already fully rewritten via fw demod as much 
               as possible, so we look among unoriented equations only. *)
            if Term.is_ground l && Term.is_ground r then
              true
            else (
              dbg D_fw_gjoin @@ lazy (sprintf "join_all: %s" (Term.to_string lit));
              match join_all state clause complete l r with
              | Some parents' -> 
                dbg D_fw_gjoin @@ lazy (sprintf "join_all: success");
                if sign then (
                  raise_notrace (Tautology_exn parents') 
                ) else (
                  Parents.merge parents parents';
                  false
                )
              | None -> 
                dbg D_fw_gjoin @@ lazy (sprintf "join_all: failure");
                true
            )
        )
      in
      if Parents.has_parents parents then (
        let parents = Parents.get parents state in
        let tstp_source = Clause.tstp_source_ground_joinability ~main:clause ~eqs:parents in
        let clause' = create_clause ~normalise_eqs:true tstp_source lits' in
        dbg_env D_fw_gjoin (fun () -> dbg_inference D_fw_gjoin 
          "Ground joinability" ~from:clause ~into:(Some clause') ~via:parents
        );
        dassert (fun () -> not @@ List.X.mem ~eq:Clause.equal_bc clause parents);
        (* Statistics.(incr_int_stat (Clause.length clause - Clause.length clause') sim_ground_joinable); *)
        Statistics.(bump_int_stat sim_ground_joinable);
        Simplified clause'
      ) else (
        Simplified clause
      )
    with Tautology_exn parents -> 
      let parents = Parents.get parents state in
      dbg_env D_fw_gjoin (fun () -> dbg_inference D_fw_gjoin 
        "Ground joinability" ~from:clause ~into:None ~via:parents
      );
      dassert (fun () -> not @@ List.X.mem ~eq:Clause.equal_bc clause parents);
      Statistics.(bump_int_stat sim_ground_joinable);
      Eliminated parents
end



module BwGroundJoinability : BwRule = struct
  let index = Index_tag.BwGjoin
  
  let simplify (state: Set.t) _clause = 
    dassert (fun () -> state.options.bw_gjoin_interval > 0);
    (* state.bw_gjoin_count <- state.bw_gjoin_count - 1; *)
    dbg D_bw_gjoin @@ lazy (sprintf "count = %d" state.bw_gjoin_count);
    if state.bw_gjoin_count <= 0 then (
      dbg D_bw_gjoin @@ lazy (sprintf "%d equations added: re-doing ground joinability on %d clauses" (state.bw_gjoin_step - state.bw_gjoin_count) (BCSet.cardinal !(state.bw_gjoin_index)));
      let result = 
        BCSet.fold (fun from acc -> 
          match FwGroundJoinability.simplify state from with
          | Simplified into when Clause.Bc.(into != from) -> 
            (* Statistics.(incr_int_stat (-1) sim_ground_joinable); *)
            Statistics.(bump_int_stat sim_bw_ground_joinable);
            BwSimplified {from; into} :: acc
          | _ -> acc
        ) !(state.bw_gjoin_index) []
      in
      state.bw_gjoin_step <- state.bw_gjoin_step + state.options.bw_gjoin_interval;
      state.bw_gjoin_count <- state.bw_gjoin_step;
      result
    ) else (
      []
    )
end



module FwConnectedness = struct
  module VarOrder = Orderings.VarOrder

  module Parents = FwGroundJoinability.Parents



  (* Either s<u, or s=u with C non-positive-unit or subst not a renaming *)
  let completeness order_terms s ?subst us = 
    us |> List.exists (fun (u, non_positive_unit) -> 
      match order_terms s u with
      | LT -> true
      | EQ -> non_positive_unit || (match subst with Some subst -> not (Subst.is_renaming subst) | None -> false)
      | GT | INC -> false
    )

  type check_completeness = Always | Top | Never
  let upd cc = match cc with Always -> Always | Top -> Always | Never -> Never

  (* * Core functions * *)

  (* Settings: *)
  let use_ac = true
  let rewrite_term_before_args = true
  let rewrite_term_after_args  = true
  let recurse_into_rewritten_subterms = true
  let assert_we_get_fixpoint = false

  exception Return_connect of (term * clause (* * subst *))

  (** Core function that uses order_var but does not try to extend *)
  let func (order: Orderings.t) order_var clause us s = 
    dbg D_fw_connect2 @@ lazy (sprintf "  func %s" (Term.to_string s));
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        if Clause.Bc.(clause == eq_clause) then () else (
        let l,r = Term.Eq.decompose_atom_exn eq_lit in
        let l,r = Term.Eq.regularize_pos pos l r in
        dbg D_fw_connect2 @@ lazy (sprintf "    trying %s = %s" (Term.to_string l) (Term.to_string r));
        dassert (fun () -> _term == l);
        dassert (fun () -> s == subst_apply subst l);
        let s' = subst_apply subst r in
        match order.terms_var order_var s s' with
        | GT -> 
          if completeness order.terms s  ~subst us 
          && completeness order.terms s' ~subst us 
          then (
            dbg D_fw_connect @@ lazy (sprintf "    %s to %s" (Term.to_string s) (Term.to_string s'));
            raise_notrace @@ Return_connect (s', eq_clause)
          )
        | _ -> ()
      ))

  (** As [func], but doesn't check completeness. *)
  let func_nc (order: Orderings.t) order_var clause s = 
    dbg D_fw_connect2 @@ lazy (sprintf "  func %s" (Term.to_string s));
    fun _term subst lst ->
      lst |> List.iter (fun (pos, eq_lit, eq_clause) ->
        if Clause.Bc.(clause == eq_clause) then () else (
        let l,r = Term.Eq.decompose_atom_exn eq_lit in
        let l,r = Term.Eq.regularize_pos pos l r in
        dbg D_fw_connect2 @@ lazy (sprintf "    trying %s = %s" (Term.to_string l) (Term.to_string r));
        dassert (fun () -> _term == l);
        dassert (fun () -> s == subst_apply subst l);
        let s' = subst_apply subst r in
        match order.terms_var order_var s s' with
        | GT -> 
          dbg D_fw_connect @@ lazy (sprintf "    %s to %s" (Term.to_string s) (Term.to_string s'));
          raise_notrace @@ Return_connect (s', eq_clause)
        | _ -> ()
      ))

  let func_ac (state: Set.t) order_var us s = 
    dbg D_fw_connect2 @@ lazy (sprintf "  func_ac %s" (Term.to_string s));
    let s' = 
      AC.normalise_ac_complete 
        ~order_terms:(state.order.terms_var order_var) ~order_uid:(-1)(*state.order.uid*) 
        state.ac_symbols.ac s 
    in
    if s' != s 
    && completeness state.order.terms s  us 
    && completeness state.order.terms s' us 
    then
      s'
    else
      s

  let func_ac_nc (state: Set.t) order_var s = 
    dbg D_fw_connect2 @@ lazy (sprintf "  func_ac_nc %s" (Term.to_string s));
    let s' = 
      AC.normalise_ac_complete 
        ~order_terms:(state.order.terms_var order_var) ~order_uid:(-1)(*state.order.uid*) 
        state.ac_symbols.ac s 
    in
    s'



  (* * Traversal * *)

  (** Rewrite top (no recurse into subterms) *)
  let rewrite_top (state: Set.t) parents order_var clause cc us s = 
    let[@inline] rewrite (state: Set.t) parents order_var clause cc us s sym = 
      (* Is AC *)
      let s' = 
        if use_ac && state.ac_symbols.ac |> SMap.mem sym then (
          let s' = match cc with Never -> func_ac(*_nc*) state order_var us s | Top | Always -> func_ac state order_var us s in
          if s' != s then (
            dbg D_fw_connect @@ lazy (sprintf "  ac into: %s" (Term.to_string s'));
            Parents.add_ac parents sym;
          ) else (
            dbg D_fw_connect @@ lazy (sprintf "  ac into: same");
          );
          s'  (* AC rewrites always complete *)
        ) else (
          s
        )
      in
      (* Not AC *)
      try 
        begin match (if s' != s then upd cc else cc) with
        | Always      -> FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func    state.order order_var clause us s');
        | Top | Never -> FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func_nc state.order order_var clause    s');
        end;
        dbg D_fw_connect @@ lazy (sprintf "  into: same");
        s'
      with Return_connect (s'', parent_clause) -> 
        dbg D_fw_connect @@ lazy (sprintf "  into: %s" (Term.to_string s''));
        dassert (fun () -> s' != s'');
        dassert (fun () -> parents.parents |> BCSet.mem parent_clause);
        s''
    in

    match s with
    | Term.Fun (sym, _, _) -> 
      dbg D_fw_connect @@ lazy (sprintf "  rewrite_top: %s" (Term.to_string s));
      rewrite state parents order_var clause cc us s sym
    | Term.Var _ -> 
      s

  (** Rewrite recursively *)
  let rec rewrite_subterms (state: Set.t) parents order_var clause cc us sym' s = 
    let[@inline] rewrite (state: Set.t) parents order_var clause cc us sym' s sym = 
      (* Is AC *)
      let s' = 
        if use_ac && sym' != sym && state.ac_symbols.ac |> SMap.mem sym then (
          let s' = match cc with Top | Never -> func_ac(*_nc*) state order_var us s | Always -> func_ac state order_var us s in
          if s' != s then (
            dbg D_fw_connect @@ lazy (sprintf "  ac into: %s" (Term.to_string s'));
            Parents.add_ac parents sym;
          ) else (
            dbg D_fw_connect @@ lazy (sprintf "  ac into: same");
          );
          s'
        ) else (
          s
        )
      in
      (* Not AC *)
      try 
        begin match (if s' != s then upd cc else cc) with
        | Always      -> FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func    state.order order_var clause us s');
        | Top | Never -> FwDemodIndex_M.iter_fwd state.fw_demod_index s' (func_nc state.order order_var clause    s');
        end;
        dbg D_fw_connect @@ lazy (sprintf "  into: same");
        s'
      with Return_connect (s'', parent_clause) -> 
        dbg D_fw_connect @@ lazy (sprintf "  into: %s" (Term.to_string s''));
        dassert (fun () -> s' != s'');
        Parents.add parents parent_clause;
        s''
    in
    
    match s with
    | Term.Fun (sym, args, _) ->
      dbg D_fw_connect @@ lazy (sprintf "  rewrite_subterms: %s" (Term.to_string s));
      (* First, rewrite term itself *)
      let s' = 
        if rewrite_term_before_args then 
          rewrite state parents order_var clause cc us sym' s sym
        else 
          s
      in
      (* If successful, try again until fixpoint *)
      if s' != s then (
        if recurse_into_rewritten_subterms then
          rewrite_subterms state parents order_var clause (upd cc) us sym s'
        else
          s'
      ) 
      (* Then rewrite all args *)
      else (
        let args = dassert (fun () -> s == s'); Term.arg_to_list args in
        let args' = List.map (rewrite_subterms state parents order_var clause cc us sym) args in
        (* If same, then we're done *)
        if List.for_all2 (==) args args' then
          s'
        (* If changed, then we also try to rewrite the term itself *)
        else
          let s'' = add_fun_term sym args' in
          dassert (fun () -> s'' != s');
          let s''' = 
            if rewrite_term_after_args then 
              rewrite state parents order_var clause (upd cc) us sym' s'' sym
            else 
              s''
          in
          (* And again, if successful, try again until fixpoint *)
          if recurse_into_rewritten_subterms && s''' != s'' then 
            rewrite_subterms state parents order_var clause (upd cc) us sym s'''
          else
            s'''
      )
    | Term.Var _ -> s
  let rewrite_subterms (state: Set.t) parents order_var clause cc us s = 
    rewrite_subterms state parents order_var clause cc us (Obj.magic 0) s

  (** Rewrite args recursively *)
  let rewrite_args (state: Set.t) parents order_var clause cc us s =
    match s with
    | Term.Fun (sym, args, _) ->
      dbg D_fw_connect @@ lazy (sprintf "  rewrite_args: %s" (Term.to_string s));
      let args = Term.arg_to_list args in
      let args' = List.map (rewrite_subterms state parents order_var clause cc us) args in
      if List.for_all2 (==) args args' then
        s
      else
        add_fun_term sym args'
    | Term.Var _ -> s

  let rewrite set parents order_var clause cc us s = 
    rewrite_subterms set parents order_var clause cc us s

  let connect_under (set: Set.t) order_var clause us l r = 
    dbg D_fw_connect @@ lazy (sprintf "connect_under: %s" (VarOrder.to_string_dbg order_var));
    let parents = Parents.empty () in
    let cc = if set.options.demod_completeness_check == Options.Demod_check.Off then Never else Always in  (* TODO *)
    let l' = rewrite set parents order_var clause cc us l in
    let r' = rewrite set parents order_var clause cc us r in
    if l' == r' then Some parents else None

  let connect (set: Set.t) clause us l r = 
    let vars = Term.add_var_set (Term.get_var_set l) r |> VSet.elements in
    if List.compare_length_with vars 2 = Ord.lt then (*  *)
      None
    else
      let var_orders = 
        let rec loop order1 order2 vars = 
          match vars with 
          | [] | [_] -> [order1; order2]
          | a::(b::_ as tl) -> 
            let order1' = VarOrder.add_gt order1 b a |> Option.get in
            let order2' = VarOrder.add_gt order2 a b |> Option.get in
            loop order1' order2' tl
        in
        loop VarOrder.empty VarOrder.empty vars
      in
      var_orders |> List.X.find_map_opt (fun order_var -> 
        connect_under set order_var clause us l r
      )

  let rec get_superposition_parents clause = 
    match Clause.get_tstp_source clause with
    | TSTP_inference_record (tstp_inference_rule, main_parents) ->        
      begin match tstp_inference_rule with
      | Demodulation
      | LightNormalisation
      | TheoryNormalisation 
      | SMTTheoryNormalisation 
      | ACDemodulation
      | GroundJoinability -> 
        let clause' = List.hd main_parents in
        get_superposition_parents clause'
      | Superposition 
      | Equality_resolution  
      | Equality_factoring -> 
        main_parents
      | _ -> []
      end
    | _-> []

  exception Tautology_exn of Parents.t

  let simplify (state: Set.t) clause = 
    Statistics.(time sim_time_fw_connected) @@ fun () ->
    if Clause.is_ac_axiom clause then Simplified clause else
    (* Find superposition parents *)
    let parents = get_superposition_parents clause in
    (* Find maximal terms *)
    let terms = 
      (clause :: parents) |> List.concat_map (fun c -> 
        let npu = not (Clause.is_pos_unit_eq c) in
        Clause.get_lits c |> List.concat_map (fun lit -> 
          match Term.Eq.decompose_lit lit with Some (_,l,r) -> [l,npu;r,npu] | None -> [lit,npu]
        )
      )
    in
    let us = 
      terms 
      |> List.sort_uniq (fun (x,_) (y,_) -> Term.compare_fast_key x y)
      |> List.X.max_elements_partial_ord (fun (x,_) (y,_) -> state.order.terms x y) 
    in
    dbg D_fw_connect @@ lazy (sprintf "clause:  %s" (Clause.to_string_tptp clause));
    dbg D_fw_connect @@ lazy (sprintf "parents: %s" (List.X.to_string Clause.to_string_tptp parents));
    dbg D_fw_connect @@ lazy (sprintf "us:      %s" (List.X.to_string Term.to_string @@ List.map fst us));
    (* For every literal, call connect *)
    try
      let parents = Parents.empty () in
      let lits' = 
        Clause.get_lits clause |> List.filter (fun lit ->
          match Term.Eq.decompose_lit lit with
          | None -> true
          | Some (sign, l, r) -> 
            dbg D_fw_connect @@ lazy (sprintf "connect: %s" (Term.to_string lit));
            match connect state clause us l r with
            | Some parents' -> 
              dbg D_fw_connect @@ lazy (sprintf "connect: success");
              if sign then (
                raise_notrace (Tautology_exn parents') 
              ) else (
                Parents.merge parents parents';
                false
              )
            | None -> 
              dbg D_fw_connect @@ lazy (sprintf "connect: failure");
              true
        )
      in
      if Parents.has_parents parents then (
        let parents = Parents.get parents state in
        let tstp_source = Clause.tstp_source_connectedness ~main:clause ~eqs:parents in
        let clause' = create_clause ~normalise_eqs:true tstp_source lits' in
        dbg_env D_fw_connect (fun () -> dbg_inference D_fw_connect 
          "Connectedness" ~from:clause ~into:(Some clause') ~via:parents
        );
        dassert (fun () -> not @@ List.X.mem ~eq:Clause.equal_bc clause parents);
        Statistics.(bump_int_stat sim_connectedness);
        Simplified clause'
      ) else (
        Simplified clause
      )
    with Tautology_exn parents -> 
      let parents = Parents.get parents state in
      dbg_env D_fw_connect (fun () -> dbg_inference D_fw_connect 
        "Connectedness" ~from:clause ~into:None ~via:parents
      );
      dassert (fun () -> not @@ List.X.mem ~eq:Clause.equal_bc clause parents);
      Statistics.(bump_int_stat sim_connectedness);
      Eliminated parents
end



module FirstClass = struct
  type index = (module Index)
  type trivRule = (module TrivRule)
  type fwRule = (module FwRule)
  type bwRule = (module BwRule)
  (* type autoFwBwRule = (module AutoFwBwRule) *)

  (* let subsumptionIndex =
    (module SubsumptionIndex : Index) *)
  let subsetSubsumptionIndex =
    (module SubsetSubsumptionIndex : Index)
  let fwDemodIndex =
    (module FwDemodIndex : Index)
  let bwDemodIndex =
    (module BwDemodIndex : Index)
  let lightNormIndex =
    (module LightNormIndex : Index)
  let lightNormIndexNoreduce =
    (module LightNormIndexNoreduce : Index)
  let smtIncrIndex =
    (module SMTIncrIndex : Index)
  let smtSetIndex =
    (module SMTSetIndex : Index)
  let unitSubsIndex =
    (module UnitSubsIndex : Index)
  let nonunitSubsIndex =
    (module NonunitSubsIndex : Index)
  (* let hybridSubsIndex =
    (module HybridSubsIndex : Index) *)
  let fwACDemodIndex =
    (module FwACDemodIndex : Index)
  let bwACDemodIndex =
    (module BwACDemodIndex : Index)
  let revDemodIndex =
    (module RevDemodIndex : Index)
  let bwGjoinIndex =
    (module BwGjoinIndex : Index)

  let eqResolutionSimp =
    (module EqResolutionSimp : TrivRule)
  let tautologyElim =
    (module TautologyElim : TrivRule)
  let eqTautologyElim =
    (module EqTautologyElim : TrivRule)
  let propSubs =
    (module PropSubs : TrivRule)

  let trivRules =
    (module TrivRules : TrivRule)

  let unflattening =
    (module Unflattening : TrivRule)

  let smtSimplify =
    (module SMTSimplify : TrivRule)

  (* let subsetSubsumption =
    (module SubsetSubsumption : AutoFwBwRule) *)
  let fwSubsetSubsumption =
    (module FwSubsetSubsumption : FwRule)
  let bwSubsetSubsumption =
    (module BwSubsetSubsumption : BwRule)

  (* let fwSubsumptionPrecond f =
    let arg = 
    (* (cl_in fwSubsumptionPrecond ::clause -> cl_by:clause -> bool) -> (module FwRule) *)
    module (FwSubsumptionPrecond(module )) *)
  let fwSubsumption =
    (module FwSubsumption : FwRule)
  let fwSubsumptionNonStrict =
    (module FwSubsumptionNonStrict : FwRule)
  let bwSubsumption =
    (module BwSubsumption : BwRule)

  let fwSubsumptionRes =
    (module FwSubsumptionRes : FwRule)
  let bwSubsumptionRes =
    (module BwSubsumptionRes : BwRule)

  let fwUnitSubs =
    (module FwUnitSubs : FwRule)
  let bwUnitSubs =
    (module BwUnitSubs : BwRule)

  (* let fwHybridSubs =
    (module FwHybridSubs : FwRule)
  let bwHybridSubs =
    (module BwHybridSubs : BwRule) *)

  let fwDemod =
    (module FwDemod : FwRule)
  (* let fwDemodLoop =
    (module FwDemodLoop : TrivRule) -> (module FwRule) *)
  let fwDemodLoopTriv =
    (module FwDemodLoopTriv : FwRule)
  let bwDemod =
    (module BwDemod : BwRule)

  let fwLightNorm =
    (module FwLightNorm : FwRule)
  let fwDemodLightNormLoopTriv =
    (module FwDemodLightNormLoopTriv : FwRule)

  let acJoinability =
    (module ACJoinability : FwRule)
  let acNormalisation =
    (module ACNormalisation : FwRule)
  let fwACDemod =
    (module FwACDemod : FwRule)
  let bwACDemod =
    (module BwACDemod : BwRule)
  let fwGroundJoinability =
    (module FwGroundJoinability : FwRule)
  let bwGroundJoinability =
    (module BwGroundJoinability : BwRule)
  let fwConnectedness =
    (module FwConnectedness : FwRule)

  let smtSubs =
    (module SMTSubs : FwRule)



  open Options.SupSimplificationSetup 
  let module_of_index = function
    (* | SubsumptionIndex -> subsumptionIndex *)
    (* | SubsetSubsumptionIndex -> subsetSubsumptionIndex *)
    | FwDemodIndex -> fwDemodIndex
    | BwDemodIndex -> bwDemodIndex
    | LightNormIndexReduce -> lightNormIndex
    | LightNormIndexNoreduce -> lightNormIndexNoreduce
    | SMTIncrIndex -> smtIncrIndex
    | SMTSetIndex -> smtSetIndex
    | UnitSubsumptionIndex -> unitSubsIndex
    | NonunitSubsumptionIndex -> nonunitSubsIndex
    | FwACDemodIndex -> fwACDemodIndex
    | BwACDemodIndex -> bwACDemodIndex
    | RevDemodIndex -> revDemodIndex
    | BwGjoinIndex -> bwGjoinIndex
    (* | HybridSubsumptionIndex -> hybridSubsIndex *)

  let module_of_trivRule = function
    (* | EqResolutionSimp -> eqResolutionSimp
    | TautologyElim -> tautologyElim
    | EqTautologyElim -> eqTautologyElim
    | TrivRules -> trivRules *)
    | PropSubs -> propSubs
    | Unflattening -> unflattening
    | SMTSimplify -> smtSimplify

  let module_of_fwRule = function
    (* | FwSubsetSubsumption -> fwSubsetSubsumption *)
    | FwSubsumption -> fwSubsumption
    (* | FwSubsumptionNonStrict -> fwSubsumptionNonStrict *)
    | FwSubsumptionRes -> fwSubsumptionRes
    | FwUnitSubsumption -> fwUnitSubs
    (* | FwHybridSubsumption -> fwHybridSubs *)
    | FwDemod -> fwDemod
    | FwDemodLoopTriv -> fwDemodLoopTriv
    | FwLightNorm -> fwLightNorm
    | FwDemodLightNormLoopTriv -> fwDemodLightNormLoopTriv
    | ACJoinability -> acJoinability
    | ACNormalisation -> acNormalisation
    | SMTSubs -> smtSubs
    | FwACDemod -> fwACDemod
    | FwGroundJoinability -> fwGroundJoinability
    | FwConnectedness -> fwConnectedness

  let module_of_bwRule = function
    (* | BwSubsetSubsumption -> bwSubsetSubsumption *)
    | BwSubsumption -> bwSubsumption
    | BwSubsumptionRes -> bwSubsumptionRes
    | BwUnitSubsumption -> bwUnitSubs
    (* | BwHybridSubsumption -> bwHybridSubs *)
    | BwDemod -> bwDemod
    | BwACDemod -> bwACDemod



  (* Helper functions *)
  let remove_demod_index = 
    List.filter (fun m  -> not @@ List.memq m [fwDemodIndex; bwDemodIndex; lightNormIndex; lightNormIndexNoreduce; fwACDemodIndex; bwACDemodIndex; revDemodIndex])

  let remove_demod_fw = 
    List.filter (fun m -> not @@ List.memq m [fwDemod; fwDemodLoopTriv; fwLightNorm; fwDemodLightNormLoopTriv; fwACDemod; fwGroundJoinability; fwConnectedness])

  let remove_demod_bw = 
    List.filter (fun m -> not @@ List.memq m [bwDemod; bwACDemod; bwGroundJoinability])

  let remove_ac_index = 
    List.filter (fun m  -> not @@ List.memq m [fwACDemodIndex; bwACDemodIndex])

  let remove_ac_fw = 
    List.filter (fun m -> not @@ List.memq m [acJoinability; acNormalisation; fwACDemod])

  let remove_ac_bw = 
    List.filter (fun m -> not @@ List.memq m [bwACDemod])

  (* Deprecated, this is already automatic now *)
  (* let remove_subs_index = 
    List.filter (fun (module M : Index)  -> not @@ List.memq M.index Index_tag.[Subs; SubsetSubs])

  let remove_subs_fw = 
    List.filter (fun (module M : FwRule) -> not @@ List.memq M.index Index_tag.[Subs; SubsetSubs])

  let remove_subs_bw = 
    List.filter (fun (module M : BwRule) -> not @@ List.memq M.index Index_tag.[Subs; SubsetSubs]) *)
end
