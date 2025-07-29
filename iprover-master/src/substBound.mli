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

type bound = int

(* type bound_subst *)

type subst       = Subst.subst
type bound_term  = Term.bound_term
type var         = Var.var
type bound_var   = Var.bound_var
type term        = Term.term
type symbol      = Symbol.symbol
module VMap      = Var.VMap
type term_db_ref = TermDB.termDB ref

(* exception Subst_bound_var_already_def *)
(* creates the empty subst. (identity) *)


module SubstKey :
  sig
    type t = bound_var
    val compare : Var.bound_var -> Var.bound_var -> int
  end

module SubstM : Map.S with type key = bound_var

type bound_subst = bound_term SubstM.t

(*include SubstM *)

(* include module type of SubstM *)

include module type of struct include SubstM end 

(* for the difference between: *)
(* "include module type of SubstM"  and *)
(* "include module type of struct include SubstM end" *)
(* see: https://stackoverflow.com/questions/37300584/how-to-get-a-module-type-from-an-interface *)

(* all standard Map functions are included by above *)
(*
val mem    : bound_var -> bound_subst -> bool 
val add    : bound_var -> bound_term -> bound_subst -> bound_subst
val remove : bound_var -> bound_subst -> bound_subst 
val find   : bound_var -> bound_subst -> bound_term
val map    : (bound_term -> bound_term) -> bound_subst -> bound_subst 
val fold   : (bound_var -> bound_term -> 'a ->'a)-> bound_subst -> 'a -> 'a
val iter   : (bound_var -> bound_term -> unit) ->  bound_subst -> unit
val is_empty :  bound_subst -> bool
*)


val create : unit -> bound_subst

(*----------- applications of bounded substitutions --------------*)

(*--- preconditions: *)
(* 1. there are no cycles in the substiutions              *)
(* 2. all terms are in term_db (including terms in subst)! *)

(*--- general: *)

(* 1. subst. application is an exhaustive replacement of bound variables until "bounded normal form" or a constant is reached *)
(*    the bounded normal forms are consitently replaced by fresh variables *)

(* 2. primed versions of application functions take the renaming environment as one of the arguments and update it *)
(*    so it is safe to continue to apply the primed application functions to several terms in sequence and the renaming will be consistent *)



(*--- renaming environment ---*)

type renaming_env =
	{
	 (* map from types to next un-used variable of this type *)
	 mutable ren_max_used_vars : Var.fresh_vars_env;
	 (* map from bvars -> var terms *)
	 mutable ren_map : (var Var.BMap.t);
	 (*	mutable ren_term_db_ref : TermDB.termDB ref;*)
       }

val init_renaming_env :  unit -> renaming_env 
val get_next_unused_var: renaming_env -> symbol -> var 
val find_renaming : renaming_env -> bound_var -> var 
val in_renaming : renaming_env -> bound_var -> bool

(* returns mapping: v-> u iff (bound,v) -> u is in ren_env  *)
val project_renaming : renaming_env -> int -> Var.renaming



(* module Replacement = struct
  type t = Term.position * Term.term
  let create pos term = (pos,term)
end *)

(* type replacement = Term.position * term

type bound_replacement = Term.position * bound_term *)

(* type bound_replacement = bound_term Term.BTMap.t *)
type bound_replacement = (bound_term * bound_term)

(*--- non-primed application ---*)

(* if you work in a context of several terms use apply_bsubst_btlist_norm_subst or apply_bsubst_btlist_norm_subst' *)
val apply_bsubst_bterm  : ?replacement:bound_replacement -> term_db_ref -> bound_subst -> bound_term -> term

val apply_bsubst_btlist : ?replacement:bound_replacement -> term_db_ref -> bound_subst -> bound_term list -> term list 

(* apply_bsubst_btlist_norm_subst: *)
(* as apply_bsubst_btlist but also returns substitution corresponding to the result *)

val apply_bsubst_btlist_norm_subst :  
    ?replacement:bound_replacement -> term_db_ref -> bound_subst -> bound -> bound_term list -> (term list) * subst


(*--- primed applications ----*) 

val apply_bsubst_bterm' : 
    term_db_ref -> renaming_env -> bound_subst -> bound_term->term
val apply_bsubst_bterm'_replacement : 
    replacement:bound_replacement -> term_db_ref -> renaming_env -> bound_subst -> bound_term->term

val apply_bsubst_btlist' : 
    term_db_ref -> renaming_env -> bound_subst -> bound_term list -> term list 
val apply_bsubst_btlist'_replacement : 
    replacement:bound_replacement -> term_db_ref -> renaming_env -> bound_subst -> bound_term list -> term list 

(*
val apply_bsubst_btlist_norm_subst' :  
    term_db_ref -> renaming_env -> bound_subst -> bound -> bound_term list -> (term list) * subst
*)	

(*----- sipliting substituions to proper/non-proper ---------*)

(* normalise variable until bound_fun_term or nf bound_var_term *)
(* note: vars in fun_term are not normalised                    *)
val find_vnorm : bound_var -> bound_subst -> bound_term

val is_proper_instantiator :  bound_subst -> bound -> bool

(* applies find_vnorm to all right vars *)
(* note: as in find_vnorm               *)
val right_vnorm_bsubst : bound_subst -> bound_subst 

(* returns (proper,non_proper) subst after right_vnorm_bsubst *)
val split_proper_inst : bound_subst -> bound_subst * bound_subst

(* returns (bsubst st. left (bv,var) bv = b , remainder) subst after right_vnorm_bsubst *)
val split_left_bound : bound_subst -> int -> bound_subst * bound_subst


(*----- to_strings -------*)

val to_stream      : 'a string_stream -> bound_subst -> unit
val out            :  bound_subst -> unit
val to_string : bound_subst -> string 
