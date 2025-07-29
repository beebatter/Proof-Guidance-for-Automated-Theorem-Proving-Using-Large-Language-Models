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


module SSet = Symbol.Set
module TSet = Term.TSet
module SMap = Symbol.Map

type symbol = Symbol.symbol

(*------- systemDBs ---------*)

val symbol_db_ref : SymbolDB.symbolDB ref
val term_db_ref : Clause.term_db ref


(*
(*--- special symbols set ---*)

val special_symbols_set : SSet.t
val is_special_symbol : symbol -> bool
*)

(*--- special terms ---*)

val bot_term : Term.term
val top_term : Term.term 

val true_term  : Term.term
val false_term : Term.term

val true_fun_term  : Term.term
val false_fun_term : Term.term

(*--- domains ---*)

val bool_fun_dom  : TSet.t

type type_to_domain = TSet.t SMap.t
type type_to_domain_ref = type_to_domain ref


val type_to_domain : type_to_domain_ref

(* after subtyping domain terms can be retyped or even domain type split into several *)
(* so we retype type_to_domain based on "new" types of the domain elements  *)
(* TODO: rework affects of subtyping *)
val retype_type_to_domain : unit -> unit

val type_to_domain_to_string : unit -> string
