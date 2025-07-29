open Lib
open Logic_interface



(** A theory represents a template of a set of axioms that can be matched
    against clauses and clause sets. *)



(** This represents a prototype of theory axiom. *)
module Axiom : sig
  type t

  (* val to_string : t -> string
  val output : out_channel -> t -> unit *)

  val to_clause : t -> clause
  val of_clause : clause -> t
end

(** This represents a prototype of a theory, with several axioms, each with a 
    subst_sym (which may be empty). *)
type t = {
  name: string;
  axioms: (Axiom.t list * Theory_subst.subst_sym) list;
}
