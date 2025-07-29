open Lib
open Logic_interface



(** Check if clause is an associativity axiom for some [symb]. If yes, returns 
    [Some symb], else returns [None]. *)
val is_assoc_axiom : clause -> symbol option

(** Check if clause is a commutativity axiom for some [symb]. If yes, returns 
    [Some symb], else returns [None]. *)
val is_commut_axiom : clause -> symbol option

(** If [parents] is 
      
      f(X,Y) = f(Y,X)
      f(X,f(Y,Z)) = f(f(X,Y),Z)
    
    then [extra_ac_axioms ~parents f] is 

      f(X,f(Y,Z)) = f(Y,f(X,Z))
      f(X,f(Y,Z)) = f(Y,f(Z,X))
      f(X,f(Y,Z)) = f(Z,f(Y,X))
    *)
val extra_ac_axioms : parents:clause list -> symbol -> clause list



(** The set of A, C, and AC operators *)
module Table : sig
  type t = {
    ac: (clause * clause) SMap.t;
    assoc: clause SMap.t;
    commut: clause SMap.t;
    axiom_list: clause list;
  }

  (** Empty set *)
  val empty : t

  (** Add many clauses to the set *)
  val populate : t -> clause list -> t

  (** Add one clause to the set, also returns (symbol, a_axiom, c_axiom) if 
      this makes the symbol AC. *)
  val add : t -> clause -> t * (symbol * clause * clause) option

  (** Whether there is at least one ac symbol. *)
  val has_ac : t -> bool
end



(** [mk_term f [x; ...; z]] = f(x, f(..., z)). Adds to term_db. *)
val mk_term : symbol -> term list -> term


(** Set of AC operators, passed as argument to functions *)
type ac_operator_set = (clause*clause) SMap.t

(*
(** Normalise a term wrt. AC, using an arbitrary (but consistent) total 
    extension of [order_terms]. *)
val normalise_ac_incomplete : order_terms:Orderings.terms -> ac_operator_set -> term -> term
*)

(** Normalise a term wrt. AC, but only reorder elements such that the resulting 
    term is smaller (wrt. [order_terms]).  *)
val normalise_ac_complete : order_terms:Orderings.terms -> order_uid:int -> ac_operator_set -> term -> term

(** Normalise a term wrt. AC, using only fast_key. *)
val normalise_ac_fastkey : ac_operator_set -> term -> term



(** Semantics of [equal_mod_ac ops t s] equivalent to [normalise_ac ops t == 
    normalise_ac ops s], but faster. *)
val equal_mod_ac : ac_operator_set -> term -> term -> bool

(** Collect subterms of AC operator, e.g.: f(f(a,b),c) -> [a;b;c]. 
    @precondition: [term] is an AC term. *)
val ac_subterms : term -> term list

