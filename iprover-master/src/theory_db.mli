open Lib
open Logic_interface



(** A database of theories, against which a clause or a set of clauses can be 
    searched to produce a [Record]. *)
type t

(** Create an empty database *)
val create : unit -> t

(** Add a theory to the database *)
val add : t -> Theory.t -> unit

(** A record of the theories contained in a set of clauses. Maps theories to 
    instances, even partial ones. *)
module Record : sig
  type t

  val create : unit -> t

  (** Returns list of complete occurrences of theory with that name *)
  val query : t -> string -> (Theory_subst.subst_sym * clause list) list

  (** Returns list of all complete occurrences of theories *)
  val to_list : t -> (Theory.t * Theory_subst.subst_sym * clause list) list

 (** the number of theories *)
  val num_of_theories: t -> int

  (** For debug purposes *)
  val output : ?prefix:string -> out_channel -> t -> unit
end

(** Search a clause in a database, and add the result to the record. *)
val search : t -> Record.t -> clause -> unit

(** Global theory record *)

val get_global_record : unit -> Record.t
    
val reset_global_record : unit -> unit

(** The builtin theory database *)
val builtin : t



(** Print report of theories found in [builtin] to hardcoded path (TEMPORARY HACK) *)
val print_report : ?trunc:bool -> heading:string -> clause list -> unit
