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




open Lib

type term = Term.term
type atom = Term.atom
type lit  = Term.lit

type var = Var.var
type symbol = Symbol.symbol



(** Represents an ordering on variables *)
module VarOrder : sig
  type t
  val empty : t
  val add_gt : t -> var -> var -> t option
  val add_eq : t -> var -> var -> t option
  val query : t -> var -> var -> PartialOrd.t
  val is_total : t -> int -> bool
  (* val mem : t -> var -> bool *)
  val iter_vars : t -> (var -> unit) -> unit
  val iter_relations : t -> (var -> var -> PartialOrd.t -> unit) -> unit
  val to_string_dbg : t -> string
end

(** An ordering is comprised of a function that compares terms, one that 
    compares atoms, and one that compares literals. Also a specialised 
    function that, given a literal l=r or l!=r, returns [terms l r] and 
    caches the result. 
    
    Furthermore, two variants of [terms] are provided, that extend the 
    ordering with a given variable ordering, and another that tries to return
    GT under some extension of the given ordering. *)
type t = {
  uid: int;  (* Uniquely identifies the ordering *)
  terms: term -> term -> PartialOrd.t;
  atoms: atom -> atom -> PartialOrd.t;
  lits: lit -> lit -> PartialOrd.t;
  oriented: lit -> PartialOrd.t;

  terms_var    : VarOrder.t -> term -> term -> PartialOrd.t;
  terms_var_gt : VarOrder.t -> term -> term -> VarOrder.t option;
}

(** Make an ordering, with a unique id *)

val make : 
  terms:(term -> term -> PartialOrd.t) -> 
  ?terms_var:(VarOrder.t -> term -> term -> PartialOrd.t) -> 
  ?terms_var_gt:(VarOrder.t -> term -> term -> VarOrder.t option) -> 
  atoms:(atom -> atom -> PartialOrd.t) -> 
  lits:(lit -> lit -> PartialOrd.t) -> 
  oriented:(lit -> PartialOrd.t) ->
  unit ->
  t


(** Convenience type aliases *)
type terms = term -> term -> PartialOrd.t
type atoms = atom -> atom -> PartialOrd.t
type lits = lit -> lit -> PartialOrd.t
type oriented = lit -> PartialOrd.t

(** A monotonically increasing unique id for orderings *)
val get_next_uid : unit -> int



(* Given a term ordering, generate a whole host of convenience functions *)
(* module Make (S: PartialOrdered with type t := term) : sig *)
(* module Make (S: sig val order : term -> term -> PartialOrd.t end) : sig
  val kbo_terms : term -> term -> PartialOrd.t
  val kbo_atoms : atom -> atom -> PartialOrd.t
  val kbo_lits  : lit  -> lit  -> PartialOrd.t

  module Terms : sig
    val (>)   : term -> term -> bool
    val (>=)  : term -> term -> bool
    val (>=!) : term -> term -> bool
    val (<)   : term -> term -> bool
    val (<=)  : term -> term -> bool
    val (<=!) : term -> term -> bool
  end

  module Atoms : sig
    val (>)   : atom -> atom -> bool
    val (>=)  : atom -> atom -> bool
    val (>=!) : atom -> atom -> bool
    val (<)   : atom -> atom -> bool
    val (<=)  : atom -> atom -> bool
    val (<=!) : atom -> atom -> bool
  end

  module Lits : sig
    val (>)   : lit -> lit -> bool
    val (>=)  : lit -> lit -> bool
    val (>=!) : lit -> lit -> bool
    val (<)   : lit -> lit -> bool
    val (<=)  : lit -> lit -> bool
    val (<=!) : lit -> lit -> bool
  end
end *)
