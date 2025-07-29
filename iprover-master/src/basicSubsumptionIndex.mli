(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2016 Konstantin Korovin and The University of Manchester. 
   This file is part of iProver - a theorem prover for first-order logic.

   iProver is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or 
   (at your option) any later version.
   iProver is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
   See the GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with iProver.  If not, see <http://www.gnu.org/licenses/>.         *)
(*----------------------------------------------------------------------[C]-*)



(* index is based on feature index of S. Schulz  *)

open Lib
open Logic_interface



(** Output interface of a subsumption index, which indexes objects of type [elt] 
    with feature of type [feature]. *)
module type Index = sig
  type elt

  type feature

  type index

  val create : unit -> index

  (** We assume that feature list is of the same length for 
      every clause, and if C subsumes D then feature list of D 
      is greater or equal than feature list of C. 
      We also assume that the the clause is in clause db. *)
  val add_clause : index -> elt -> unit

  val remove_clause : index -> elt -> unit

  (* val in_subs_index : index -> elt -> bool *)

  val is_empty : index -> bool



  (** Subsumption **)

  (** Check if the clause is subsumed and return [Some (d, unif)] if it is 
      and [None] otherwise. *)
  val is_subsumed : 
    ?pre_cond:(cl_in:elt -> cl_by:elt -> bool) -> 
    subs_bck_mult:int -> index -> elt -> (elt * Subst.subst) option

  (** As [is_subsumed ~pre_cond:(fun ~cl_in ~cl_by -> cl_by != cl_in]. *)
  val is_subsumed_strict : 
    subs_bck_mult:int -> index -> elt -> (elt * Subst.subst) option

  (** Returns list of clauses in the index subsumed by the clause, and 
      removes them from the index. Empty list if no such clauses. *)
  val find_subsumed : 
    subs_bck_mult:int -> index -> elt -> elt list

  (* As [find_subsumed] but also gives subsitution: (subsumed, subst) list *)
  val find_subsumed_subst : 
    subs_bck_mult:int -> index -> elt -> (elt * Subst.subst) list

  (* val remove_subsumed : clause -> index -> index *)
end

(** Output interface of a subsumption index, which indexes objects ['a] by 
    objects  of type [elt], with feature of type [feature]. *)
module type IndexData = sig
  type elt

  type feature

  type 'a index

  val create : unit -> 'a index

  (* Add data ['a] indexed by [elt]. *)
  val add : 'a index -> elt -> 'a -> unit

  (* For ['a] indexed by [elt], filter out those for which the predicate is true. *)
  val filter : 'a index -> elt -> ('a -> bool) -> unit

  (* val is_empty : 'a index -> bool *)



  val is_subsumed : 
    subs_bck_mult:int -> 'a index -> elt -> (elt * Subst.subst * 'a list) option

  val is_subsumed_strict : 
    subs_bck_mult:int -> 'a index -> elt -> (elt * Subst.subst * 'a list) option

  val find_subsumed :
    subs_bck_mult:int -> 'a index -> elt -> (elt * Subst.subst * 'a list) list
end

(* Default pre_cond, returns [true] for all inputs. *)
val pre_cond_true_fun : cl_in:'a -> cl_by:'a -> bool



(** General abstract "clause" *)
module type Elt = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val subsumes : subs_bck_mult:int -> t -> t -> subst

  val to_string : t -> string
  val get_lits : t -> lit list
end

(** Features *)
module type Feature = sig
  type t

  type elt

  val compare : t -> t -> int
  val get_feature_list : elt -> t list 
end

(** Compressed features *)
module type FeatureCom = sig
  type t

  type elt

  val compare_pos : t -> t -> int
  val compare_val : t -> t -> int
  val get_feature_list : elt -> t list 

  (** for debug *)
  val to_string : t -> string
end

module type Options = sig
  (** Whether or not to track clauses added to the index in a BCSet. In
      resolution, we rely on this, but in Simplify_new this is wasteful 
      because it's already tracked. *)
  val track_clauses : bool
end





(** From a concrete implementation of [Elt], and from a [Feature] for elt, 
    make an [Index]. *)
(* module Make (Feature:Feature) : (Index with type feature=Feature.t) *)

(** From a concrete implementation of [Elt], and from a [FeatureCom] for elt, 
    make an [Index]. *)
module MakeCom (Elt : Elt) (FeatureCom : FeatureCom with type elt=Elt.t) (Options : Options) : (Index with type elt=Elt.t and type feature=FeatureCom.t)
(*Index with type elt := Elt.t and type feature = FeatureCom.t*)

(** From a concrete implementation of [Elt], and from a [FeatureCom] for elt, 
    make an [Index]. *)
module MakeComData (Elt : Elt) (FeatureCom : FeatureCom with type elt=Elt.t) : (IndexData with type elt=Elt.t and type feature=FeatureCom.t)



(** A "default" implementation of [FeatureCom]. *)
module DefaultFeatureCom : (FeatureCom with type elt = lit list)
