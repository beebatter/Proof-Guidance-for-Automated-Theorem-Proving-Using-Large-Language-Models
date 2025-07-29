(*----------------------------------------------------------------------(C)-*)
(*   This file is part of iProver - a theorem prover for first-order logic. *)
(*   see the LICENSE  file for the license                                  *)

(** This module contains types and functions used in different modules of
    abstraction-refinement. *)

open Logic_interface

exception AR_time_out

type 'a state = private
  { orgcls : BCSet.t;
    orgcls_lst : clause list;
    abstr_dom : Ar_abstract_domain.t;
    details : 'a;
    opts : Options.options;
    start_time : float;
    time_limit : float Lib.param;
  }
(** Type of the state in the over/under approximation procedures. *)

val get_id_ars: unit -> int
(** Returns an integer. Each time this functions is called the value of the ID
    increments by one. *)

val create_state: Ar_abstract_domain.approx -> clause list -> 'a ->
  Options.options -> float Lib.param -> 'a state
(** [create_state a cls dtls opts tl] creates a under/over approximation state
    depending on the passed argument [a] which can be [Under] or [Over]. The
    argument [cls] is a list of the concrete clauses (original clauses) to be
    abstracted. On the other hand, the parameter dtls contains the details of
    the abstraction to be applied during the over/under approximation process.
    The two last parameters opts and tl pass to the state the options given by
    iProver and the time limit for the procedure. *)

val update_abstract_assets: 'a -> Ar_abstract_domain.t -> 'b state -> 'a state
(** [update_abstract_assets dtls adom s] updates the details of the abstraction
    and the abstract domain in the given state [s]. The details and the abstract
    domain are updated with [dtls] and [adom] respectively.
    Checks time_out and can -- raise AR_time_out  --
 *)

val update_time_limit: 'a state -> float Lib.param -> 'a state
(** [update_time_limit s tl] updates the time limit of the state [s] with the
    parameter [tl]. *)

val time_limit_diff_intrvl: float Lib.param -> float Lib.param ->
  float Lib.param -> float Lib.param
(** [time_limit_diff_intrvl t1 t2 tlm] Calculates the remaining time limit after
    subtracting the difference between [t1] and [t2], i.e., tlm - (t1 - t2). *)

