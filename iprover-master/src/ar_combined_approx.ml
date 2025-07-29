(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

let solve (opts:Options.options) timelmt clauses =
  Ar_dbg.dbg D_trace (lazy (Printf.sprintf "ar_combined_approx:solve:timelmt: %s" (Lib.param_to_string string_of_float timelmt)));
  let over_solv = Ar_over_approx.get_solver opts.abstr_ref in
  let comb_solv = Ar_under_approx.get_solver over_solv opts.abstr_ref_under in
  ignore(Ar_atp.check_answer (comb_solv opts timelmt clauses))
