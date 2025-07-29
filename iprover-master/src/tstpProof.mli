(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2022 Konstantin Korovin and The University of Manchester. 
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

(** Proofs include clausification formulas (Ext_formula) by external clausifier *)
type proof_formula =  
  | Int_clause of clause 
  | Ext_formula of int * string * (int list) (* (id, formula_str, parent_ids) *)

val proof_formula_to_string : proof_formula -> string

(** Clauses to and from proof_formulas *)
val clause_to_pf : clause -> proof_formula
val clauses_to_pf : clause list -> proof_formula list

(* raises Not_found if proof_formula  is not Int_clause *)
val pf_to_clause : proof_formula -> clause

module PFSet :  Set.S with type elt = proof_formula

module DGS : Graph.Sig.P with type V.t = proof_formula

(** Main component of a proof is the proof_graph: edges are from premesis to conclusions *)
type proof_graph = DGS.t

val out_proof_dot : string -> proof_graph -> unit

(** DGS_TOPO.fold/iter can be used to traverse proof_graph in the topological order
https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Topological/Make_stable/index.html *)

module DGS_TOPO :
    sig
      val fold : (proof_formula -> 'a -> 'a) -> DGS.t -> 'a -> 'a
      val iter : (proof_formula -> unit) -> DGS.t -> unit
    end
    

(** Main proof object *)

type proof 

val get_proof : ?with_clausification:bool -> ?ignore_parents:(proof_formula -> bool) -> proof_formula list -> proof

val pp_tstp_proof : Format.formatter -> proof -> unit

val get_proof_graph : proof -> proof_graph

(** Get the roots of the proof; these are the same as formulas in get_proof  *)
val get_roots : proof -> proof_formula list 

(** Output a clause and its source with justification for global subsumption *)
val pp_clause_with_source_gs : ?clausify_proof:bool -> Format.formatter -> Clause.clause -> unit

(** List all formulas in the proof graph in topological order *)

val pf_graph_to_list : proof_graph -> proof_formula list 
val proof_to_list : proof -> proof_formula list
val proof_to_list_cls : proof -> clause list 

(** Get proof leaves *)
val get_leaves : proof -> proof_formula list 

(** Get proof leaves upto clausification *)
val get_leaves_cls : proof -> clause list

(** Reduce proof graph keeping filtered nodes, result is an implication graph modulo removed nodes 
    reduction starts with nodes (proof_formula list) (can be different from root nodes of the proof_graph). 
    If filter = (fun x -> true) then reduced graph is the subgraph induced by the start nodes closed upwards. 
*)

val reduce_proof_graph : filter: (proof_formula -> bool) -> proof_graph:proof_graph -> proof_formula list -> proof_graph

