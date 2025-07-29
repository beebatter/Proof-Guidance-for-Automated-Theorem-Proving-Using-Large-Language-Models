open Lib
open Logic_interface

(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_trace2

let dbg_gr_to_str = function 
  | D_trace -> "trace"
  | D_trace2 -> "trace"

let dbg_groups = [
  D_trace;
  (* D_trace2; *)
]
    


(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)





(** A theory is a mapping from a list of symbols to a set of clauses. For instance
      (f)     -> [ f(X,Y)=f(Y,X) ; f(X,f(Y,Z))=f(f(X,Y),Z) ]
      (f,1,i) -> [ f(X,1)=X ; f(X,i(X))=1 ; f(X,f(Y,Z))=f(f(X,Y),Z) ] 
    etc. *)



(** This represents a prototype of theory axiom. *)
module Axiom = struct
  type t = clause

  let to_clause = Fun.id
  let of_clause = Fun.id

  let to_string = Clause.to_string_tptp
  let output out x = output_string out (to_string x)
end



(** This represents a prototype of a theory, with several axioms, each with a 
    subst_sym (which may be empty). *)
type t = {
  name: string;
  axioms: (Axiom.t list * Theory_subst.subst_sym) list;
}
