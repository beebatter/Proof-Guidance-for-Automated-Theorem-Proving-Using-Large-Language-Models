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

type prob_props =
    {
     mutable clauses : int;
     mutable conjectures : int;
     mutable epr : int;
     mutable horn : int;
     mutable ground : int;
     mutable unary : int;
     mutable binary : int;
     mutable lits : int;
     mutable lits_eq : int;

     (*------ finite domains -----*)     
     mutable fd_pure         : int;
     mutable fd_pseudo       : int;
     mutable fd_cond         : int;
     mutable fd_pseudo_cond : int;

     (*---- Theories -------*)     
     mutable ac_symbols : AC.Table.t;
     mutable theories : Theory_db.Record.t;

     (*---- Misc ----*)
     mutable has_is_int_rat: bool;
   }


(* val input_problem_props : problem_props ref *)


val get_prob_props : clause list -> prob_props

val prob_props_to_statistics : prob_props -> unit 

val prob_props_to_string : prob_props -> string

(*--------- derived properties -------*)

val is_epr        : prob_props -> bool
val is_horn       : prob_props -> bool 
val is_ground     : prob_props -> bool 
val has_eq        : prob_props -> bool 
val is_unary      : prob_props -> bool 
val is_binary     : prob_props -> bool 
val is_pure_eq    : prob_props -> bool 
val is_unit_eq    : prob_props -> bool 
val has_ac        : prob_props -> bool 

(*--------- clause list properties computed directly -------*)
val has_equality : clause list -> bool

val change_prolific_symb_input : clause list -> unit



(** Signature map: symb -> cnt *)

type sig_cnt_map = int SMap.t

val get_sym_cnt_cl_list : clause list  ->  sig_cnt_map

val out_sig_cnt_map :  sig_cnt_map -> unit 
val out_sig_cnt     : clause list -> unit 

(** Signature trigger map: *)
(** trigger -> in how many clauses this symb is a trigger  *)
type trigger_cnt_map = int SMap.t

(* we need to compute first sig_cnt_map; float is the tolerance *)
val trigger_cnt_map : elig_sk_split:bool -> sig_cnt_map -> float -> clause list -> trigger_cnt_map

val out_trigger_cnt_map : trigger_cnt_map -> unit 

val out_trigger_cnt     : elig_sk_split:bool -> sig_cnt_map -> float -> clause list -> unit 

(** Out both symbol cnt and triggers so not to recompute symbol cnt *)
val out_sig_trig_cnts : Options.options ->  clause list -> unit 


(*------------ transform clauses into abstract clauses by -----------------------*)
(*------------ collapsing symbols of the same arity (or other properties) -------*)

val get_abstr_clause_list : clause list -> clause list 

val abstr_and_out : str_add:string -> clause list -> unit 

val abstr_and_out_conj : clause list -> unit  (* split conjectures and axioms *)
