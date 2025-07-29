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

val create : 
  order:ordering -> 
  eq_types:Symbol.sym_set -> 
  use_ground:bool -> 
  ac_table:AC.Table.t -> 
  ?reverse:bool -> 
  unit -> t

val clear : t -> unit

val add_equation :
  t -> clause -> bool

val add_bwd_clause :
  t -> clause -> unit

val elim_equation :
  t -> clause -> unit

val elim_bwd_clause :
  t -> clause -> unit

(* val elim_clause_with_sel :
  t -> lit -> lit -> clause -> unit *)

(* val eliminate_lit : t -> lit -> clause list *)



(** List interface *)

val get_fwd :
  t -> term -> 
    ((term * Subst.subst * (int * lit * clause) list)) list

val get_fwd_caching :
  t -> term -> (
    (term * Subst.subst * (int * lit * clause) list) list -> 
    term -> 
    term * (clause * Subst.subst) option
  ) -> 
  term * (clause * Subst.subst) option
    
    (* (((term * (int * lit * clause) list) * Subst.subst) list -> term -> term * clause option) ->  *)

val get_bwd :
  t -> term -> 
    ((term * Subst.subst * (clause * lit option) list)) list



(** Iter interface *)

val iter_fwd : 
  t -> term -> 
    ( (term -> Subst.subst -> (int * lit * clause) list -> unit) -> unit )

(** Given an index, term, and a function that given an iterator through the 
    list of candidates (term, subst, (pos, eq_lit, eq_clause) list), and the 
    query term, returns the demodulated term and optionally some parent clause
    and subst. *)
val iter_fwd_caching : 
  t -> term -> (
    ( (term -> Subst.subst -> (int * lit * clause) list -> unit) -> unit ) -> 
    term -> 
    term * (clause * Subst.subst) option
  ) -> 
  term * (clause * Subst.subst) option
    (* ( (term -> Subst.subst -> (int * lit * clause) list -> unit) -> term -> term * clause option ) ->  *)

val iter_bwd : 
  t -> term -> 
    (term -> Subst.subst -> (clause * lit option) list -> unit) -> unit
