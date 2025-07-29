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
open Instantiation_env
open Options

module CD = Clause.Clause_Dism
type clause_dism = CD.t

(*
val num_of_dismatch_blockings    :  int ref 
val num_of_non_proper_inst       :  int ref 
val num_of_duplicates            :  int ref
*)


(* assume literals are ordered as by default *)
val is_tautology : clause -> bool 
val is_eq_tautology : clause -> bool

(*-----------------------------------------------------------------*)
(*-- unflatting: x!=t\/ C[x,y] -> C[t,s] where x not in t ---------*)
(*-- 1) collect diequalities {x_i!=t_i}; unify {x_1 = t_1;..;x_n=t_n} and apply mgu to the remainder of the clause *)
(*--- if 1) fails due non-unification then collect disequality of vaiables and unify them and apply one substitution --*)
(*--- the ramaining disequalitites are applied one by one *)
(*-----------------------------------------------------------------*)

val unflatten_clause : ?normalise_eqs:bool -> clause -> clause
val unflatten : clause list -> clause list

val bmc1_merge_next_func : clause -> clause

(* strict_subset_subsume by_clause to_clause *)
(* we assume that to_subs_clause has defined length *)
(* and by_clause does not, but all lits a are in    *)

val strict_subset_subsume  : clause -> clause -> bool

(* resolution, factoring, instantiation_nrom can raise  Unif.Unification_failed *)
(* resolution can raise Main_subsumed_by*)

exception Main_subsumed_by of clause
val resolution : fwd_subs_res:bool->   clause -> literal ->
                      clause list -> literal -> clause list 


val resolution_dismatch : 
    dismatching_flag:bool  -> 
      inst_model_flag:bool -> 
        forward_subs_resolution_flag:bool -> 
          backward_subs_resolution_flag:bool -> 
            clause_dism -> literal ->
              clause_dism list -> literal -> clause list 


val subs_resolution : fwd_subs_res:bool ->  clause -> literal ->
                      clause list -> literal -> clause list 

val factoring     : clause -> literal -> literal -> clause

(*
val instantiation : term_db ref -> clause -> literal -> literal ->
                      clause list -> literal -> clause list 
*)

val equality_resolution_simp: clause -> clause

val instantiation_norm : 
    dismatching_flag:bool ->
      inst_model_flag:bool ->
        inst_restr_to_given:bool ->
          is_redundant_fun:(clause -> bool) -> clause_dism -> literal ->
            clause_dism list -> literal -> clause list

(* val instantiation_norm : context -> (clause * inst_cp)  -> literal ->
  bool -> (clause * inst_cp) list -> literal -> clause list 
*)


(*------------- domain instantiation used in QBF/ also try finite domains ------------*)

(*
type dom_inst_param = 
    Dom_inst_single | 
    Dom_inst_chain
 *)

type inst_domain_result = 
  | Main_dom_inst of clause list (* main is redunandant in the presence of these clauses *)
  | Side_dom_inst of clause list (* side is redunandant in the presence of these clauses *)

(* let instantiation_domain_single : type_to_domain (c1,c1_param) l1 c_c_param_list2 l2 = *)
val instantiation_domain_single: 
    qbf_dom_inst_type -> (* defined in options.mli *)
    SystemDBs.type_to_domain -> 
      clause  -> literal ->
        clause list -> literal -> inst_domain_result


(* for element of domain pre-instantiates clause all variables mapped to this element *)
(* used in QBF *)
val dom_pre_inst: SystemDBs.type_to_domain -> clause -> clause list



(* Superposition *)

(** A call to [equality_superposition pos_left pos_right s left_lit right_lit
    left_clause right_clause] checks if the following superposition inference 
    can be done:

      l=r ∨ C    tl[s]≐tr ∨ D
      -----------------------
        (tl[r]≐tr ∨ C ∨ D)θ

    where
      θ = mgu(l,s)
      trθ ⋡ tlθ
      s not a variable
    and 
      left_lit is l=r
      right_lit is tl=tr
      left_clause is l=r ∨ C
      right_clause is tl=tr ∨ D
      pos_left tells if l=r is to be taken as flipped
      pos_right tells if tl=tr is to be taken as flipped
    
    If yes, then it returns [Some conclusion], otherwise it returns [None] *)
val equality_superposition : 
  order:ordering -> 
  int -> int -> 
  term -> 
  lit -> lit -> 
  clause -> clause -> 
  clause option

(** A call to [equality_resolution lit clause] checks if the following equality
    resolution inference can be done:

      l≠r ∨ C
      -------
        Cθ

    where
      θ = mgu(l,r)
    and 
      lit is l≠r
      clause is l≠r ∨ C
    
    If yes, then it returns [Some conclusion], otherwise it returns [None] *)
val equality_resolution : lit -> clause -> clause option

(** A call to [equality_factoring pos1 pos2 lit1 lit2 clause] checks if the 
    following equality factoring inference can be done:

       l=r ∨ l'=r' ∨ C
      -----------------
      (l=r ∨ r≠r' ∨ C)θ

    where
      θ = mgu(l,l')
      rθ ⋡ lθ
      r'θ ⋡ rθ
    and 
      lit1 is l=r
      lit2 is l'=r'
      clause is l=r ∨ l'=r' ∨ C
      pos1 tells if l=r is to be taken as flipped
      pos2 tells if tl=tr is to be taken as flipped
    
    If yes, then it returns [Some conclusion], otherwise it returns [None] *)
val equality_factoring : 
  order:ordering -> 
  int -> int -> 
  lit -> lit -> 
  clause -> 
  clause option

(* Demodulation *)

(** Exception to mimic early return *)
exception Return of clause

(** A call to [demodulation ~order ~check_type ~do_check eq_clause eq_lit l r s clause] checks
    if the following inference can be done:
    
      l=r   C[lθ]
      -----------
         C[rθ]

    where
      lθ ≻ rθ
    and
      eq_lit is the term l=r / r=l
      eq_clause is the unit clause { eq_lit }
      s is lθ
      clause is C

    If yes, raises [Return conclusion], if not returns [()] *)
(* Currently deprecated, for use with nonperfect indexing
val demodulation : 
  order:ordering -> 
  demod_completeness_check_type:Options.Demod_check.t -> 
  do_check:term option -> 
  clause -> lit -> 
  term -> term -> term -> 
  clause -> 
  unit
*)

(** Demodulation core, no completeness checks or clause creation. Simply try 
    to use an oriented instance of l=r to rewrite s, and if yes return [Some 
    s'], otherwise return [None]. *)
val demodulation_bare : 
  order:ordering -> 
  lit -> 
  term -> term -> term -> 
  term option

(** Same but subst is given (for use when indexing already returns a substitution). *)
val demodulation_bare_nomatch : 
  order:ordering -> 
  lit -> 
  term -> term -> term -> 
  Subst.subst -> 
  term option
