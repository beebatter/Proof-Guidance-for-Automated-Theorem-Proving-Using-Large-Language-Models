open Lib
open Logic_interface



(* The final spec contains the final functions which are called in the simplification loop. *)
type spec = {
  indices_passive : (clause -> unit);
  indices_active  : (clause -> unit);
  indices_immed   : Simplify_new.set -> (clause -> unit);
  indices_input   : (clause -> unit);

  full_triv : (clause -> Simplify_new.fw_result);
  full_fw : (clause -> Simplify_new.fw_result);
  full_bw : (clause -> Simplify_new.bw_result);
  
  immed_triv : (clause -> Simplify_new.fw_result);
  immed_fw_immed : Simplify_new.set -> (clause -> Simplify_new.fw_result);
  immed_bw_immed : Simplify_new.set -> (clause -> Simplify_new.bw_result);
  immed_fw_main  : (* Simplify_new.set -> *) (clause -> Simplify_new.fw_result);
  immed_bw_main  : (* Simplify_new.set -> *) (clause -> Simplify_new.bw_result);

  input_triv : (clause -> Simplify_new.fw_result);
  input_fw : (clause -> Simplify_new.fw_result);
  input_bw : (clause -> Simplify_new.bw_result);
}

(* Transforms an [Options.SupSimplificationSetup.spec] (user command-line options) 
   into a [Superposition_sim_spec.spec] (actual simplification functions which are
   used in superposition). *)
val mk_spec : 
  demod_flag:bool ->  (* If true remove fw and bw demod. *)
  (* subs_flag:bool ->  *)
  ac_flag:bool ->  (* If true remove ac rules. *)
  sim_state:Simplify_new.set -> 
  imsim_state:Simplify_new.set -> 
  Options.SupSimplificationSetup.spec -> spec

(* As [mk_spec] but removes immediate demod. This is useful as an optimisation
   when there are no equalities in the immediate set. *)
val mk_spec_noimmeddemod : 
  sim_state:Simplify_new.set -> 
  imsim_state:Simplify_new.set -> 
  Options.SupSimplificationSetup.spec -> spec

(* As [mk_spec] but removes immediate simplifications. This is useful as an 
   optimisation when there is only 1 clause in the immediate set. *)
val disable_immed : spec -> spec

(* --- *)

(** From the specification in an [Options.t], generate an [Orderings.t]. *)
val mk_order : 
  ?with_var:bool -> 
  ordering:Options.Ordering.Func.t -> 
  symb_ordering:Options.Ordering.Symb.t -> 
  term_weight:Options.Ordering.Weight.t -> 
  ?theory_record:Theory_db.Record.t -> 
  unit ->  (* unit is needed due to optional and only labelled args *)
  Orderings.t
