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

type symbol = Symbol.symbol
type var  = Var.t



(** Knuth-Bendix ordering on terms, parametrised by: 
    - weight function, that assigns an integer weight to each term
    - symbol precedence, total order on symbols 
    You can call directly, but most likely you want to partially apply. *)
(* val terms : 
  weight:(term -> int) ->
  symb_ordering:(symbol -> symbol -> Ord.t) ->
  term -> term -> PartialOrd.t *)

(** Specialised version that computes (and caches) the result of [terms l r]
    in a literal l=r / l!=r, given a comparison function for terms. *)
(* val oriented : 
  (term -> term -> PartialOrd.t) -> 
  lit -> PartialOrd.t *)

(** Given a kbo comparison function for terms, returns one for atoms *)
(* val atoms : 
  (term -> term -> PartialOrd.t) ->
  atom -> atom -> PartialOrd.t *)

(** Given a kbo comparison function for terms, returns one for literals *)
(* val lits : 
  (term -> term -> PartialOrd.t) ->
  lit -> lit -> PartialOrd.t *)



(** If [with_var = false], has empty [terms_var] and [terms_var_gt]. If 
    [inc_criteria = true], try niche optimisation to attempt to orient INCs *)
val make : 
  ?with_var:bool -> 
  ?inc_criteria:bool -> 
  weight:(term -> int) ->
  symb_ordering:(symbol -> symbol -> Ord.t) ->
   unit ->  (* unit is needed due to optional and only labelled args *)
  Orderings.t

(** As [make], but weight function is [symbol->int] rather than [term->int]. *)
val make_sw : 
  ?with_var:bool -> 
  wvar:(int) -> wsym:(symbol -> int) ->
  symb_ordering:(symbol -> symbol -> Ord.t) ->
  unit ->  (* unit is needed due to optional and only labelled args *)
  Orderings.t

(*
(** This is a version of [(KBO.make ~weight ~symb_ordering).terms] which also takes a variable 
    ordering to compare variable occurrences. *)
val kbo_terms_var : 
  weight:(term -> int) ->
  symb_ordering:(symbol -> symbol -> Ord.t) ->
  VarOrder.t -> term -> term -> PartialOrd.t

(** This is a version of [(KBO.make ~weight ~symb_ordering).terms] which attempts to make t>s
    under the given variable ordering or an extension thereof. If this is possible, returns [Some
    order_var'], where order_var' is an ordering on variables which makes s>t and is an extension 
    of the given one. Otherwise returns [None] *)
val kbo_terms_var_gt : 
  weight:(term -> int) ->
  symb_ordering:(symbol -> symbol -> Ord.t) ->
  VarOrder.t -> term -> term -> VarOrder.t option
*)
