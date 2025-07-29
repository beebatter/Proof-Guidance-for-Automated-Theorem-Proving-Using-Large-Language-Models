open Lib
open Clause

let shared_clauses = ref BCSet.empty 
let get_all_shared_clauses () = !shared_clauses

let add_shared_clause clause = 
  shared_clauses := BCSet.add clause !shared_clauses

let add_shared_clause_list clause_list =
  List.iter add_shared_clause clause_list

let add_shared_clause_set clause_set =
  shared_clauses := BCSet.union clause_set !shared_clauses

let rm_shared_clause clause = 
  shared_clauses := BCSet.remove clause !shared_clauses

(*-------*)
let smt_shared_clauses = ref BCSet.empty 
let smt_get_shared_clauses () = !smt_shared_clauses

let smt_add_shared_clause clause =
  smt_shared_clauses := BCSet.add clause !smt_shared_clauses

let smt_add_shared_clause_list clause_list =
  List.iter add_shared_clause clause_list

let smt_add_shared_clause_set clause_set =
  smt_shared_clauses := BCSet.union clause_set !smt_shared_clauses

let smt_rm_shared_clause clause = 
  smt_shared_clauses := BCSet.remove clause !smt_shared_clauses

let smt_empty_shared_clauses () =
  smt_shared_clauses := BCSet.empty 
