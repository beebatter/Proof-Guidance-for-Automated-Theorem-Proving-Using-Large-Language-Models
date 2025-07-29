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
open Options

(* val preprocess : (* is_eq_problem:bool -> *) clause list -> clause list *)

type prep_state 

(* get preprocessing options from the current schedule *)
val glb_sched_to_prep_options : Problem_properties.prob_props -> options

(* solver assumptions are added automatically as side atoms *)
(* call after solver assumptions are assinged  *)
val prep_create_state : prep_opts:options -> clause_list:(clause list) -> extra_side_atoms:(term list) (* -> side_clauses:(clause list) *) -> prep_state

val preprocess_sim : before_eq_axioms:bool -> prep_state -> unit (* -> prep_state*)

val preprocess_trans : prep_state -> unit (* -> prep_state*)

val prep_get_clauses : prep_state -> clause list

val prep_get_inst_pre_model : prep_state -> Instantiation_env.inst_pre_model


val set_reduce : clause list -> clause list 

(** Inter-simplify clause set *)

(** How to use:
    - Make a [Superposition_sim_spec.spec] with functions in that model
    - Call [superposition_sim_only (...) spec clause_list] *)

(* val mk_spec : Options.SupSimplificationSetup.spec -> (Simplify_new.set -> sup_sim_spec) *)

val superposition_sim_only : 
  Superposition_sim_spec.spec -> 
  Simplify_new.set -> 
  clause list -> clause list





(** Normalises AC by: sorting AC terms by fast_key and making right-associative. *)
val normalise_ac : order:Orderings.t -> Problem_properties.prob_props -> clause list -> clause list

(** Normalises AC by: sorting AC terms by fast_key and making right-associative. *)
(* val smt_implied_axioms : Problem_properties.prob_props -> clause list -> clause list *)



(** Check if options are to exit after preprocessing. *)
val maybe_print_and_exit : prep_state -> unit

