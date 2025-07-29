open Lib 
open Clause 

(** Clauses shared between components inst/res/sup *)
val get_all_shared_clauses : unit -> BCSet.t
val add_shared_clause : clause -> unit 
val add_shared_clause_list : clause list -> unit 
val add_shared_clause_set : BCSet.t -> unit 
val rm_shared_clause : clause -> unit

(** Clauses shared for submission to SMT  which is done in sup. component *)
    
val smt_get_shared_clauses : unit -> BCSet.t
val smt_add_shared_clause : clause -> unit 
val smt_add_shared_clause_list : clause list -> unit 
val smt_add_shared_clause_set : BCSet.t -> unit 
val smt_rm_shared_clause : clause -> unit
val smt_empty_shared_clauses : unit -> unit
