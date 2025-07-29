open Lib
open Logic_interface



(** Injective substitutions of vars to vars and symbols to symbols. *)
type t = (subst_var * subst_sym)
and subst_var = var VMap.t
and subst_sym = symbol SMap.t

val to_string_var : subst_var -> string
val to_string_sym : subst_sym -> string
val output_var : out_channel -> subst_var -> unit
val output_sym : out_channel -> subst_sym -> unit

(** Checks if a and b can be equal modulo renaming of variables and symbols, 
    under some extension of [(subst_var, subst_sym)]. If yes, returns 
    [Some (subst_var', subst_sym')], if not, returns [None]. *)
val renaming : t -> term -> term -> t option

(** Merges two subst_sym, if not conflicting *)
val merge_sym : subst_sym -> subst_sym -> subst_sym option
