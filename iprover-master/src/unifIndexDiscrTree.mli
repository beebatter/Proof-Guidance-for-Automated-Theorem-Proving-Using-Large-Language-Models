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



open Logic_interface



(** Index from lit to 'a *)
type 'a t

(** Creates an empty index *)
val create : unit -> 'a t

(** Clears the index. [clear old] functionally identical to 
    [let old = create()] *)
val clear : 'a t -> unit

val add_elem_to_lit : 'a t -> lit -> 'a -> unit

val elim_elem_from_lit : 'a t -> lit -> 'a -> unit

val elim_filter_from_lit : 'a t -> lit -> ('a -> bool) -> unit

val eliminate_lit : 'a t -> lit -> 'a list

(** Returns unification candidates. s and t are unifiable if exists θ such that
    sθ = tθ. *)
val get_unif_candidates :
  'a t -> lit -> (lit * 'a list) list

(** Returns generalisation candidates. t is a generalisation of s if exists θ such that
    s = tθ. *)
val get_gen_candidates :
  'a t -> lit -> (lit * 'a list) list

(** Returns instantiation candidates. t is an instance of s if exists θ such that
    sθ = t. *)
val get_inst_candidates :
  'a t -> lit -> (lit * 'a list) list

(** Returns all terms which are a renaming of the given term. *)
val get_variants :
  'a t -> lit -> (lit * 'a list) list

(** Checks if any element in index is indexed by lit *)
val mem : 'a t -> lit -> bool



(** Alternative interface *)

(** As [List.iter (get_unif_candidates ...)] *)
val get_unif_candidates_iter :
  'a t -> lit -> (lit -> 'a list -> unit) -> unit

(** As [List.iter (get_gen_candidates ...)] *)
val get_gen_candidates_iter :
  'a t -> lit -> (lit -> 'a list -> unit) -> unit

(** As [List.iter (get_inst_candidates ...)] *)
val get_inst_candidates_iter :
  'a t -> lit -> (lit -> 'a list -> unit) -> unit
