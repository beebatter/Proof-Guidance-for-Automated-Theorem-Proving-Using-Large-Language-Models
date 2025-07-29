(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(*----- debug modifiable part-----*)
let dbg_flag = false

type dbg_gr =
  | D_trace
  | D_SGA
  | D_AFA
  | D_GA
  | D_SBA
  | D_CSA
  | D_over
  | D_under
  | D_atp

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_SGA -> "signature grouping abstraction"
  | D_AFA -> "argument filtering abstraction"
  | D_GA -> "generalisation abstraction"
  | D_SBA -> "subsumption-based abstraction"
  | D_CSA -> "cone symbols abstraction"
  | D_over -> "over approximation"
  | D_under -> "under approximation"
  | D_atp -> "ATP"

let dbg_groups =
  [
    D_trace;
     D_SGA; 
     D_AFA; 
     D_GA; 
     D_SBA; 
     D_CSA; 
     D_over; 
     D_under; 
     D_atp; 
  ]

let module_name = "ar_dbg"

(*----- debug fixed part --------*)
let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f
