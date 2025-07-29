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

type t 

val create : order:ordering -> eq_types:Symbol.sym_set -> unit -> t

val clear : t -> unit

(** Add clause indexed by lit *)
val add_clause_with_sel :
  t -> lit -> clause -> unit

(** Eliminate clause indexed by lit. Raises [Not_found] if it doesn't exist in the index *)
val elim_clause_with_sel :
  t -> lit -> clause -> unit

(* val add_clause_with_sel_subterms :
  t -> lit -> clause -> unit

val add_clause_with_sel_terms :
  t -> lit -> clause -> unit

val elim_clause_with_sel_subterms :
  t -> lit -> clause -> unit

val elim_clause_with_sel_terms :
  t -> lit -> clause -> unit *)

(* val eliminate_lit : t -> lit -> clause list *)

(* val get_unif_candidates_term :
  t -> term -> (term * (int * lit * clause) list) list *)

(** Candidates for backward_superposition via a clause l=r *)
val backward_superposition_candidates :
  t -> term -> (term * (int * lit * clause) list) list

(** Candidates for forward superposition via a clause l[s]=r *)
val forward_superposition_candidates :
  t -> term -> (term * (int * lit * clause) list) list

(* val get_unif_candidates :
  t -> term -> (term * (int * lit * clause) list) list *)

(* val in_unif_index : t -> term -> bool *)

(* filters lits with minimal combined cl_measure of unif candidates in the index *)
(* cl_measure : cl -> int *)
(* val filter_lits_min_unif_cand : t -> (clause -> int) -> term list -> term list *)

(* val get_measure_unif_cand_lits : t -> (clause -> int) -> term list -> int *)



(** Alternative interface *)

(** As [List.iter (backward_superposition_candidates ...)] *)
val backward_superposition_candidates_iter :
  t -> term -> (term -> (int * lit * clause) list -> unit) -> unit

(** As [List.iter (forward_superposition_candidates ...)] *)
val forward_superposition_candidates_iter :
  t -> term -> (term -> (int * lit * clause) list -> unit) -> unit
