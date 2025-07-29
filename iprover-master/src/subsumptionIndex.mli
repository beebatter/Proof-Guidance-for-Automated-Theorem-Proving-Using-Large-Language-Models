open Lib
open Logic_interface



(** Concrete implementation of a subsumption index for clauses, with fixed 
    compressed features as defined in [BasicSubsumptionIndex.DefaultFeatureCom]. *)
module SCFVIndex : sig
  (** See documentation in [BasicSubsumptionIndex.Index]. *)
  include BasicSubsumptionIndex.Index with type elt := clause  (* Can also be = *)

  (* val get_feature_list : clause -> feature list *)

  (** Subsumption-resolution **)

  (** Check if a clause can be fw simplified via subsumption resolution. If 
      yes, returns lit list after subsumption resolution, and list of parents. *)
  val fw_subsres : 
    subs_bck_mult:int -> index -> clause -> lits * clause list

  (** For a clause, return all clauses in the index which can be simplified 
      by it via subsumption resolution. Returns a list of clauses which get 
      simplified to the corresponding lit list *)
  val bw_subsres : 
    subs_bck_mult:int -> index -> clause -> clause list * lits list
end

module SCFVIndexNoTrack : sig
  include BasicSubsumptionIndex.Index with type elt := clause

  val fw_subsres : 
    subs_bck_mult:int -> index -> clause -> lits * clause list

  val bw_subsres : 
    subs_bck_mult:int -> index -> clause -> clause list * lits list
end

(** Also exported here *)
(*include module type of SCFVIndex*)
