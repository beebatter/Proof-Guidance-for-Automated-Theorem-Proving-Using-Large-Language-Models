open Lib
open Logic_interface



(** {1 Indice tags} *)

(** Index tag *)
module Index_tag : sig
  type t = 
    | FwDemod
    | BwDemod
    (* | Subs *)
    | SubsetSubs
    | LightNorm
    | LightNormNoreduce
    | SMTIncr
    | SMTSet
    | UnitSubs
    | NonunitSubs
    (* | HybridSubs *)
    | FwACDemod
    | BwACDemod
    | RevDemod
    | BwGjoin

  (** Convert to string *)
  val to_string : t -> string

  (** Convert to distinct int (useful for bitvector) *)
  val to_int : t -> int

  (** Inverse of [to_int] *)
  val of_int : int -> t
end



(** {1 Set} *)

type context_data_structure = ContextMap | ContextTbl

(** Options *)
type options = {
  (** Term ordering *)
  order: ordering;
  (** Type of demodulation check *)
  demod_completeness_check: Options.Demod_check.t;
  (** Set of equality types *)
  eq_types: Symbol.sym_set;
  (** Whether to rewrite ground terms with demodulation or leave it to light normalisation *)
  demod_use_ground: bool;
  (** When sims are cached *)
  cache_sim: Options.SupSimplificationSetup.CacheSim.t;
  (** Set of AC symbols *)
  ac_symbols: AC.Table.t (* ref *);

  (** Check smt consistency every <n> clauses *)
  smt_check_interval: int;
  
  bw_gjoin_interval: int;

  (** limit on clause to clause subsumptions as multiple of the subsumee; negative is unlimited *)
  subs_bck_mult: int;

  (** Which data structure to use to store the context. *)
  context_data_structure: context_data_structure;
}

(** Simplification set. This represents the simplification state, which
    encapsulates all indices as well as the context. It can be queried via 
    the functions below. It is taken as an argument by the add/remove/simplify
    operations of any simplification rule, and may be modified by them (except
    when forward simplifying). *)
module Set : sig
  type t

  (** Signals clause was already in the set/index *)
  exception Is_mem
  (** Signals clause was already dead *)
  exception Is_dead
  (** Signals clause was not in the set/index *)
  exception Not_mem

  (** Create an empty set. *)
  val create : options -> t

  (** Get a unique id for the set, for debugging purposes. *)
  val unique_id : t -> int

  (** Clear an existing superposition set and all its indices. *)
  val clear : t -> t

  (** Check if a clause is dead in the context. *)
  val is_dead : t -> clause -> bool

  (** Add a clause (to no actual indices) as dead. *)
  val add_dead : t -> clause -> unit

  type status = Live | Dead | NotMem
  (** Checks if clause is live, dead, or not in set. *)
  val status : t -> clause -> status

  (** List clauses in the context. *)
  val list_all : t -> clause list

  (** List all dead clauses in the context. *)
  val list_dead : t -> clause list

  (** List all nondead clauses in the context. *)
  val list_nondead : t -> clause list

  (** Number of clauses in the context. *)
  val size : t -> int

  (** Number of dead clauses in the context. *)
  val size_dead : t -> int

  (** Number of nondead clauses in the context. *)
  val size_nondead : t -> int

  (** Apply the function [f clause is_dead] to all clauses in the set. *)
  val iter : (clause -> bool -> unit) -> t -> unit

  (** Check if a clause is in an index (specified by an [Index_tag.t]). *)
  val mem : t -> Index_tag.t -> clause -> bool

  (** Check if a clause is in the context (in any index). *)
  val mem_any : t -> clause -> bool
end

(** Convenience shortcut *)
type set = Set.t

(** Adds to set and indices.
    Raises [Is_mem] if already in index. Raises [Is_dead] if clause is dead 
    in set. *)
val add_to_indices : set -> Index_tag.t list -> clause -> unit

(** Sets dead and removes from all indices. 
    Raises [Is_dead] if clause is already dead. Raises [Non_mem] if clause is 
    not in index. *)
val set_dead_and_remove : set -> clause -> unit

(** As [set_dead_and_remove], but forces the clause to be dead in the index 
    even if it is not in the index or already dead. *)
val force_dead_and_remove : set -> clause -> unit


val smt_add_outside_clause : Set.t -> clause -> unit


(** {1 Unified interface for indices} *)

(** Unified interface for indices that supports adding/removing *)
module type Index = sig
  (** Type of index that this rule reads/writes to *)
  val index : Index_tag.t
  (** Adds clause to index in set *)
  val add : set -> clause -> unit
  (** Removes clause from index in set *)
  val remove : set -> clause -> unit
  (* (** Checks if clause is in set *)
  val mem : Set.t -> clause -> bool *)
end



(** {1 Result interfaces} *)

(** Result of a forward simplification *)
type fw_result = 
  | Simplified of clause
  (** Clause was simplified to [clause], or it was not simplified, in which 
      case it is guaranteed that input is physically equal to output. *)

  | Eliminated of clause list
  (** Clause was found to be redundant, wrt. the clause list. *)

(** Monadic interface, also print function for debugging *)
module Fw_result : sig
  type t = fw_result

  (** Monadic bind. Useful to succinctly chain simplifications: in [simp1 x >>= simp2]
      if [simp1] returns [Simplified c], the c is fed to [simp2], if it returns 
      [Eliminated parents], the whole expression returns [Eliminated parents] 
      without trying [simp2]. *)
  val (>>=) : t -> (clause -> t) -> t

  (** Monadic return *)
  val return : clause -> t

  (** Monadic fold *)
  val fold : (clause -> t) list -> clause -> t

  (** Run until fixpoint *)
  val fix_point : (clause -> t) -> clause -> t

  (** [Clause.to_string_tptp] if [Simplified], "Eliminated" if [Eliminated]. *)
  val to_string_tptp : t -> string

  (* val same : t -> t -> bool *)

  (** Compare results using [eq] to compare inner values inside [Simplified]. *)
  (* val equal : eq:(clause -> clause -> bool) -> t -> t -> bool *)

  module O : sig
    (** Compare results using [==] to compare inner values inside [Simplified]. *)
    (* val (==) : t -> t -> bool *)

    (* val (!=) : t -> t -> bool *)

    val (>>=) : t -> (clause -> t) -> t

    (** New syntax in >=4.08. [let* x = m in ...] is syntactic sugar for 
        [m >>= fun x -> ...] *)
    val ( let* ) : t -> (clause -> t) -> t
  end

  (** Wraps a simplification a→b, such that if a is simplified to smaller b 
      by clauses smaller than a, then a is made dead in the set. *)
  val cache_sim : set -> (clause -> t) -> (clause -> t)

  (** Minimal wrapper: removes eliminated clauses from the set. *)
  val elim_sim : set -> (clause -> t) -> (clause -> t)
end

(** More fine grained control: clause [from] is to be replaced by [into]. If 
    we have a big list of added clauses and removed clauses we don't know
    which corresponds to which. *)
type bw_result_elt = 
  | BwSimplified of {from: clause; into: clause}
  | BwEliminated of clause

type bw_result = bw_result_elt list

module Bw_result : sig
  type elt = bw_result_elt

  type t = bw_result

  val empty : t

  (** [x @ y] is [{add=x.add@y.add; remove=x.remove@y.remove}] *)
  (* val (@) : bw_result -> bw_result -> bw_result *)

  (* [fold x l] is equivalent to [l |> List.map (fun f -> f x) |> List.fold] *)
  (** [fold x l] applies the functions in [l] to the clause [x] and collects the results in one [bw_result] *)
  (* val fold : (clause -> t) list -> clause -> t *)

  (** [map f_list clause] = [List.map (fun f -> f clause) f_list]. *)
  (* val map : (clause -> t) list -> clause -> t list *)

  (** Given a list of bw simplifications, applies each of them to [clause] 
      and collects the result in one [bw_result]. *)
  val fold : (clause -> t) list -> clause -> t

  (* module O : sig
    val (@) : bw_result -> bw_result -> bw_result
  end *)

  (** Applies [add] to all clauses to be added, and [remove] to all clauses to be removed. *)
  val handle : remove:(clause -> unit) -> add:(clause -> unit) -> t -> unit

  (** As [handle] but handles only one single [elt]. *)
  val handle_elt : remove:(clause -> unit) -> add:(clause -> unit) -> elt -> unit
end



(** {1 Rule interfaces} *)

(** "Trivial" rule, meaning no context is needed. *)
module type TrivRule = sig
  (** Simplifies clause. No need for set. *)
  val simplify : clause -> fw_result
end

(** Fw simplification rule *)
module type FwRule = sig
  (* module Index : Index *)
  (* include Index *)
  (* val index : Index_tag.t *)

  (** Simplifies clause via rule wrt clauses in set. *)
  val simplify : set -> clause -> fw_result
end

(** Bw simplification rule *)
module type BwRule = sig
  (* module Index : Index *)
  (* include Index *)
  (* val index : Index_tag.t *)

  (** Simplifies clause via rule wrt clauses in set. *)
  val simplify : set -> clause -> bw_result
end

(** The "special" case of subset subsumption. Fw and bw simplification rule
    that triggers automatically on add. *)
(* module type AutoFwBwRule = sig
  val add_and_simplify : set -> clause -> (fw_result * bw_result)
  val remove : set -> clause -> unit  (* TODO Also may need to do bw simp? *)
end *)



(** {1 Indices} *)

(* module SubsumptionIndex : Index *)
module UnitSubsIndex : Index
module NonunitSubsIndex : Index
module SubsetSubsumptionIndex : Index
module FwDemodIndex : Index
module BwDemodIndex : Index
module LightNormIndex : Index
module LightNormIndexNoreduce : Index
module SMTSetIndex : Index
module SMTIncrIndex : Index
    
module FwACDemodIndex : Index
module BwACDemodIndex : Index



(** {1 Simplification rules} *)

(** Note: in inference rules, a '═' underline means deleted premise, '─' means regular inference *)



(** {2 Trivial rules} (independent of state) *)

(* module EqResolution : TrivRule  *)

(** Syntactic Equality Resolution:

      t≠t ∨ C
      ═══════
         C
  *)
module EqResolutionSimp : TrivRule 

(** Tautology Elimination:

      p ∨ ¬p ∨ C
      ══════════

  *)
module TautologyElim : TrivRule 

(** Eq. Tautology Elimination:

      t=t ∨ C
      ═══════

    Note: cannot use with axiomtic equality! Only in preprocessing before eq 
    axioms are added. *)
module EqTautologyElim : TrivRule 

(** Pseudo-rule encapsulates all "cheap" triv simplifications. This is 
    equivalent to [fun clause -> TautologyElim.simplify clause >>= 
    EqTautologyElim.simplify >>= EqResolutionSimp.simplify] *)
module TrivRules : TrivRule 

(** Unflattening:

      x≠t ∨ C
      ═══════
         Cθ

    where x does not appear in t and θ is x->t. 

    Note: cannot use with axiomtic equality! Only in preprocessing before eq 
    axioms are added. *)
module Unflattening : TrivRule

(** Propositional subsumption (not truly a trivrule, depends on global state) *)
module PropSubs : TrivRule

(** Theory simplification via SMT solver call *)
module SMTSimplify : TrivRule



(** {2 Subset subsumption} *)

(* module SubsetSubsumption : AutoFwBwRule *)

(** Subset Subsumption:

      C ∨ D   C
      ═════────

  *)

module FwSubsetSubsumption : FwRule
module BwSubsetSubsumption : BwRule


(** {2 Subsumption} *)

(** Subsumption with precondition:

      Cθ ∨ D   C
      ══════────

    where [cl_in(Cθ ∨ D) = true] and [cl_by(C) = true].
  *)
module FwSubsumptionPrecond : functor 
  (M : sig val pre_cond : cl_in:clause -> cl_by:clause -> bool end) -> FwRule

(** As [FwSubsumptionPrecond(pre_cond ~cl_in ~cl_by = true)] *)
module FwSubsumption : FwRule

(** As [FwSubsumptionPrecond(pre_cond ~cl_in ~cl_by = length cl_by < length cl_in)] *)
module FwSubsumptionNonStrict : FwRule

(** Backward subsumption, no precondition. *)
module BwSubsumption : BwRule



(** {2 Subsumption resolution} *)

(** Subsumption resolution:

      A ∨ C   B̅ ∨ D
      ────────═════
            D

    where, for some θ, (A∨C)θ ⊆ B∨D *)

module FwSubsumptionRes : FwRule

module BwSubsumptionRes : BwRule



(** {2 Demodulation} *)

(** Demodulation:

      l=r   C[lθ]
      ──────═════
         C[rθ]

    where 
      lθ ≻ rθ
      lθ=rθ ≺ C[lθ] *)

module FwDemod : FwRule

(** As [FwDemod], but if clause was simplified, apply [RetentionTest.simplify] and demodulate again *)
module FwDemodLoop (RetentionTest : TrivRule) : FwRule

(** [FwDemodLoop(TrivRules)] *)
module FwDemodLoopTriv : FwRule

module BwDemod : BwRule


(** {2 Light Normalisation} *)

(** Light Normalisation:

      l=r   C[l]
      ──────════
         C[r]

    where 
      l ≻ r
      l=r ≺ C[l] *)

module FwLightNorm : FwRule

(** As [FwLightNorm], but first try light normalisation. If clause was simplified, apply [RetentionTest.simplify] and go again. *)
module FwDemodLightNormLoop (RetentionTest : TrivRule) : FwRule

(** [FwDemodLightNormLoop(TrivRules)] *)
module FwDemodLightNormLoopTriv : FwRule



(* module Orient : TrivRule *)

module ACJoinability : FwRule
module ACNormalisation : FwRule
module FwACDemod : FwRule
(* module BwACDemod : BwRule *)


(** Global SMT subsumption. When adding to index, clauses are abstracted by 
    mapping distinct variables to distinct fresh constants. Then, simplifying
    with this rule checks and removes ground implied literals from the clause. 

    This rule uses the implementation in [SMT_incremental]. *)
module SMTSubs : FwRule




(** {1 First-class modules} *)
module FirstClass : sig
  type index = (module Index)
  type trivRule = (module TrivRule)
  type fwRule = (module FwRule)
  type bwRule = (module BwRule)
  (* type autoFwBwRule = (module AutoFwBwRule) *)

  (* val subsumptionIndex : index *)
  val subsetSubsumptionIndex : index
  val fwDemodIndex : index
  val bwDemodIndex : index
  val lightNormIndex : index
  val lightNormIndexNoreduce : index
  val smtIncrIndex : index
  val smtSetIndex : index
  val unitSubsIndex : index
  (* val hybridSubsIndex : index *)
  val fwACDemodIndex : index
  val bwACDemodIndex : index
  val revDemodIndex : index
  val bwGjoinIndex : index

  val eqResolutionSimp : trivRule
  val tautologyElim : trivRule
  val eqTautologyElim : trivRule
  val propSubs : trivRule
  val trivRules : trivRule
  val unflattening : trivRule
  val smtSimplify : trivRule

  (* val subsetSubsumption : autoFwBwRule *)
  val fwSubsetSubsumption : fwRule
  val bwSubsetSubsumption : bwRule

  (* val fwSubsumptionPrecond : (cl_in:clause -> cl_by:clause -> bool) -> fwRule *)
  val fwSubsumption : fwRule
  val fwSubsumptionNonStrict : fwRule
  val bwSubsumption : bwRule

  val fwSubsumptionRes : fwRule
  val bwSubsumptionRes : bwRule

  val fwUnitSubs : fwRule
  val bwUnitSubs : bwRule

  (* val fwHybridSubs : fwRule
  val bwHybridSubs : bwRule *)

  val fwDemod : fwRule
  val fwDemodLoopTriv : fwRule
  (* val fwDemodLoop : trivRule -> fwRule *)
  val bwDemod : bwRule

  val fwLightNorm : fwRule
  val fwDemodLightNormLoopTriv : fwRule

  val acJoinability : fwRule
  val acNormalisation : fwRule
  val fwACDemod : fwRule
  (* val bwACDemod : bwRule *)
  val fwGroundJoinability : fwRule
  val fwConnectedness : fwRule
  val bwGroundJoinability : bwRule

  val smtSubs : fwRule



  (** First-class modules from options *)
  val module_of_index    : Options.SupSimplificationSetup.index -> index
  val module_of_trivRule : Options.SupSimplificationSetup.trivRule -> trivRule
  val module_of_fwRule   : Options.SupSimplificationSetup.fwRule -> fwRule
  val module_of_bwRule   : Options.SupSimplificationSetup.bwRule -> bwRule



  (** Helper functions *)
  val remove_demod_index : index list -> index list 
  val remove_demod_fw : fwRule list -> fwRule list 
  val remove_demod_bw : bwRule list -> bwRule list 

  val remove_ac_index : index list -> index list 
  val remove_ac_fw : fwRule list -> fwRule list 
  val remove_ac_bw : bwRule list -> bwRule list 

  (* val remove_subs_index : index list -> index list 
  val remove_subs_fw : fwRule list -> fwRule list 
  val remove_subs_bw : bwRule list -> bwRule list  *)
end
