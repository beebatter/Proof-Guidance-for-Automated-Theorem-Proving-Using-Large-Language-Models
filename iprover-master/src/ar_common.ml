(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

open Lib
open Logic_interface

exception AR_time_out

type 'a state =
  { orgcls : BCSet.t;
    orgcls_lst : clause list;
    abstr_dom : Ar_abstract_domain.t;
    details : 'a;
    opts : Options.options;
    start_time : float;
    time_limit : float Lib.param;
  }

let id_ars = ref 0

let get_id_ars () =
  let id = !id_ars in
  id_ars := !id_ars + 1;
  id

let create_state approx orig_cls details opts time_limit =
  { orgcls = BCSet.of_list orig_cls;
    orgcls_lst = orig_cls;
    abstr_dom = Ar_abstract_domain.empty approx;
    details = details;
    opts = opts;
    start_time = Unix.gettimeofday ();
    time_limit = time_limit;
  }

let check_time state = 
  match state.time_limit with
  | Undef -> ()
  | Def time_limit ->
      if Float.O.(Unix.gettimeofday () -. state.start_time > time_limit)
      then raise AR_time_out
      else ()

let update_abstract_assets details adom state =
  check_time state;
  {state with abstr_dom = adom; details = details}

let update_time_limit state ntimelmt = {state with time_limit = ntimelmt}

let time_limit_diff_intrvl tl1 tl2 = function
  | Lib.Undef -> Lib.Undef
  | Lib.Def gt ->
    match tl1 with
    | Lib.Undef -> Lib.Undef
    | Lib.Def t1 ->
      match tl2 with
      | Lib.Undef -> Lib.Undef
      | Lib.Def t2 -> Lib.Def (gt -. (t1 -. t2))

