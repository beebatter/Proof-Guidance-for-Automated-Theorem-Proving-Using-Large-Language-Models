open Lib
open Logic_interface



(** {2 Create state} *)

(** All functions take a parameter of type [state]. *)
type state

(** Options to initialise a state. *)
type options = {
  interpreted_arithmetic: bool;
}

(** Initialise an empty [state] with given [options]. *)
val make_state : options -> state



(** {2 Create terms and clauses} *)

(** Represents a term/formula that can be asserted on an SMT problem instance. *)
type term

type clause

(** Shortcut to refer to iprover terms/clauses *)
module Ip = Logic_interface

(** Misc functions for [term], packed into an interface *)
module Term : sig
  type t = term 
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end

(** Misc functions for [clause], packed into an interface *)
module Clause : sig
  type t = clause 
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end



(** {3 Convert structurally, with quantified variables -> fresh constants} *)

(** Converts an iProver literal into an (abstract) [SMTSolver.term] that 
    can be added to the solver (as a unit clause) or packed into a clause via
    [term_list_to_clause]. *)
val lit_to_smt : state -> Ip.term -> term

(** As [lit_to_smt], but on clauses. *)
val clause_to_smt : state -> Ip.clause -> clause



(** {3 Convert structurally, with quantified variables -> same constant} *)

(** As [lit_to_smt], . *)
(* val lit_to_smt_ground : state -> Ip.term -> term *)

(** As [lit_to_smt_ground], but on clauses. *)
val clause_to_smt_ground : state -> Ip.clause -> clause



(** {3 Convert structurally, with quantified variables -> quantified variables} *)

(** As [lit_to_smt], but quantified variables are still quantified rather 
    than constants. *)
val lit_to_smt_quant : state -> Ip.term -> term

(** As [lit_to_smt_quant], but on clauses. *)
val clause_to_smt_quant : state -> Ip.clause -> clause



(** {3 Convert to propositional symbol} *)

(** Converts an iProver literal into a propositional [SMTSolver.formula]. Two
    different atoms (wrt (==)) will give different propositional variables, 
    and p and -p will give opposite variables. *)
val lit_to_smt_prop : state -> Ip.term -> term

(** As [lit_to_smt_prop], but on clauses. *)
val clause_to_smt_prop : state -> Ip.clause -> clause

(** Manually create a prop formula from a polarity and an int. *)
(* val int_to_smt_prop : state -> bool -> int -> term *)



(** {3 Reverse conversions, from smt terms/clauses back to iprover terms/clauses} *)
val term_of_smt : state -> term -> Ip.term
val lit_of_smt : state -> term -> Ip.term option  (** None if cannot convert *)
val clause_of_smt : state -> clause -> Ip.lits option  (** None if $true *)



(** {3 Misc} *)

(** Manually create a unit clause. *)
val term_to_unit_clause : state -> term -> clause

(** Manually create a clause from a list of terms. *)
val term_list_to_clause : state -> term list -> clause

(** Add a tag to a clause. A tag is a unique id, and the tagged clause C is 
    C \/ id. id can then be asserted true to "deactivate" the clause. *)
val tagged_formula : state -> int -> clause -> clause



(** {2 Create problem} *)

(** A problem is an object derived from state, to which we can add Ip.clauses and check satisfiability. *)
type problem

(** Create a problem in a state *)
val make_problem : state -> problem



(** {2 Add formulas to problem} *)

(** Add an assertion to the problem. *)
val add : problem -> clause -> unit

(** Add several assertions at once. *)
val add_many : problem -> clause list -> unit

(** Create a backtracking point. *)
val push : problem -> unit

(** Reset to previous backtracking point. *)
val pop : problem -> unit

(** Clear all clauses asserted. *)
val clear : problem -> unit



(** {2 Check satisfiability} *)

(** Type of results of calls to the solver. *)
type result = Sat | Unsat | Unknown

(** Check consistency of asserted clauses. *)
val check : problem -> result

(** Check consistency of asserted clauses plus the given assumptions. *)
val check_assumptions : problem -> clause list -> result

(** Quick incomplete check. *)
(* val fast_check : problem -> result *)

(** Quick incomplete check. *)
(* val fast_check_assumptions : problem -> clause list -> result *)

(** Abstract type of an SMT model. *)
type model 

(** If result was [Sat], get a model. *)
val model : problem -> model option



(** {2 Fast incomplete solver} *)

(** Create a problem in a state, where [check]/[check_assumption] calls are done with a fast (possibly incomplete) algorithm. *)
val make_problem_fast : state -> problem

(* module Fast : sig
  type problem

  val make_problem : state -> problem

  val add : problem -> clause -> unit

  val add_many : problem -> clause list -> unit 

  val push : problem -> unit

  val pop : problem -> unit

  val clear : problem -> unit

  val check : problem -> result

  val check_assumptions : problem -> clause list -> result
end *)



(** {2 Alternative interface: produce unsat cores} *)

module Uc : sig
  type problem

  (** Object that marks a formula *)
  type tag

  val tag_equal : tag -> tag -> bool

  val make_problem : state -> problem

  (** As [add], but will return a tag for that formula. *)
  val add : problem -> clause -> tag

  (** As [add_many], but will return tags for those formulas. *)
  val add_many : problem -> clause list -> tag list

  val push : problem -> unit

  val pop : problem -> unit

  val clear : problem -> unit



  val check : problem -> result

  val check_assumptions : problem -> clause list -> result

  (** Minimally inconsistent subset of formulas. Will return a list of tags 
      associated with those formulas. *)
  val unsat_core : problem -> tag list
end



(** {2 Query model} *)

(** Value of a term in model. *)
val eval_term : model -> term -> term

(** Truth value of term in model, if it is a true/false value, otherwise [None]. *)
val eval_term_bool : model -> term -> bool option

(** As [eval] , but for clauses. *)
val eval_clause : model -> clause -> clause

(** As [eval_bool], but for clauses. *)
val eval_clause_bool : model -> clause -> bool option



(** {2 Extra functions} *)

(** Equivalent but simplified version of [term]. *)
val simplify_term : (* state -> *) term -> term

(** Equivalent but simplified version of [clause]. *)
val simplify_clause : (* state -> *) clause -> clause
