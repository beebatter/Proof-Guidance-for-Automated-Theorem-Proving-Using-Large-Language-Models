open Lib
open Logic_interface

(*----- !!! for now use bin_hyper_res (the same but binary definitions computationally cheaper then all of them)-------*)
(*----- !!! TODO: refactor changes from bin_hyper_res.ml where used axioms def axioms are fixed !!! *)

(* 1) normalises binary definitions (l1<->l2) defs in def_map;  and binary clauses in clause_list *)
(* 2) applies normalisation to all clauses *)
(* 3) reduce binary implication garph (based on binary clauses from clause_list) 
     a) using 1) to obtain an implication graph on normal forms then 
     b) apply transitive reduction/completion to normal form graph and replace binary clauses with the reduced graph clauses *)
(* 4) adds normalisation axioms for 1) *)

val bin_def_apply_def_map: Def_discovery.def_map -> clause list -> clause list

(* as above but def_map is computed *)
val bin_def_apply: clause list -> clause list
