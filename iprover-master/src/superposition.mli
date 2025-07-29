open Options
open Logic_interface





(** Superposition loop
    ==================

    Instructions: Create options, then create an empty state with those 
    options. Then add clauses. Then, you can run a step at a time (atomically). 
    Between steps, you may add more clauses. *)



(** Options object, inherits superposition options from global [Options.options], 
    and also other stuff that superposition needs, such as the set of eq types, 
    problem properties, etc.  *)
type options

type sim_flag = NoSubs | NoDemod | AllSims

(** From global options and other required parameters, make superposition options. 
    hook_passive: is an outside function that is applied before adding to passive; if it returns None then the clause is discarded *)
val make_options : 
  ?hook_passive:(clause -> clause option) -> 
  Options.options -> 
  sim_flag:sim_flag -> 
  eq_types:Symbol.sym_set -> 
  (* demod_use_ground:bool -> *)
  prob_props:Problem_properties.prob_props -> 
  options



(** Represents the current proof search state. *)
type state

(** Create an empty state, optionally with some input_clauses (same as [create] 
    then [add_clauses]). *)
val create : (* ?input_clauses:(clause list) -> *) ?scores_frozen:(Superposition_scores.t option) -> options -> state

(** Adds clauses to state. These are held in an "incoming" queue, and are 
    processed when [step] is next called. *)
val add_clauses : state -> clause list -> unit 

(** As [add_clauses], but aditionally performs simplifications according to 
    --sup_input_* options. *)
val add_input_clauses : state -> clause list -> unit 



(** Executes one given clause iteration of the main superposition loop. Either:
    - Advances the state by one iteration of the loop (mutating the state)
    - Raises [Sup_satisfiable m], meaning the set is saturated (with model [m])
    - Raises [Empty_clause c], meaning a contradiction has been found (with 
      proof reconstructable from parents of [c]) *)
val step : state -> unit



(** A model is a saturated set of clauses, here represented by the map from 
    clauses to selected literals, used internally by the module. *)
type sup_model = term list BCMap.t

exception Sup_satisfiable of sup_model

(** Re-Assigns hook_passive function to state; *)
val assign_hook_passive : (clause -> clause option) -> state -> unit 

(** Get number of clauses; TODO: fix Statistis when there are multiple sup. componnets *)
val get_num_of_clauses : state  -> int 

(** Get scores from state *)
val get_scores : state -> Superposition_scores.t

(** Get extra clauses, such as ac axioms, etc *)
val get_extra_clauses : state -> clause list

(** Add extra clause just to SMT solver *)
val smt_add_clause : state -> clause -> unit

