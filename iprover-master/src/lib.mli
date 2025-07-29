(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2016 Konstantin Korovin and The University of Manchester. 
   This file is part of iProver - a theorem prover for first-order logic.

   iProver is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or 
   (at your option) any later version.
   iProver is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
   See the GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with iProver.  If not, see <http://www.gnu.org/licenses/>.         *)
(*----------------------------------------------------------------------[C]-*)




(** Comparisons *)

(** Unqualified comparison operators are now monomorphic over [int]. *)
val (=)  : int -> int -> bool
val (<>) : int -> int -> bool
val (<)  : int -> int -> bool
val (>)  : int -> int -> bool
val (<=) : int -> int -> bool
val (>=) : int -> int -> bool

val compare : int -> int -> int
val min     : int -> int -> int
val max     : int -> int -> int

(** The old polymorphic comparison functions are available inside the 
    [Poly] module. *)
module Poly : sig
  val (=)  : 'a -> 'a -> bool
  val (<>) : 'a -> 'a -> bool
  val (<)  : 'a -> 'a -> bool
  val (>)  : 'a -> 'a -> bool
  val (<=) : 'a -> 'a -> bool
  val (>=) : 'a -> 'a -> bool

  val compare : 'a -> 'a -> int
  val min     : 'a -> 'a -> 'a
  val max     : 'a -> 'a -> 'a
end





(** Basic types, with added functionality. *)

(** Infix operators, if present, are put in a module [O], for convenience 
    when opening locally (as in [String.O.(x = "abc")]. *)

module Int : sig
  include module type of Int
  (** As the unqualified versions, but also exported here for consistency *)
  module O : sig
    val (=)  : int -> int -> bool
    val (<>) : int -> int -> bool
    val (<)  : int -> int -> bool
    val (>)  : int -> int -> bool
    val (<=) : int -> int -> bool
    val (>=) : int -> int -> bool
  end
  val min : int -> int -> int
  val max : int -> int -> int
end

module Bool : sig
  include module type of Bool
  module O : sig
    val (=)  : bool -> bool -> bool
    val (<>) : bool -> bool -> bool
    val (<)  : bool -> bool -> bool
    val (>)  : bool -> bool -> bool
    val (<=) : bool -> bool -> bool
    val (>=) : bool -> bool -> bool
  end
end

module Float : sig
  include module type of Float
  module O : sig
    val (=)  : float -> float -> bool
    val (<>) : float -> float -> bool
    val (<)  : float -> float -> bool
    val (>)  : float -> float -> bool
    val (<=) : float -> float -> bool
    val (>=) : float -> float -> bool
  end
end

module String : sig
  include module type of String
  module X : sig
    val is_empty : string -> bool
    val is_nonempty : string -> bool
  end

  module O : sig
    val (=)  : string -> string -> bool
    val (<>) : string -> string -> bool
  end
end

module Char : sig
  include module type of Char

  module O : sig
    val (=)  : char -> char -> bool
    val (<>) : char -> char -> bool
  end
end





(** Depending on the value of [fast_raise_flag], this can be aliased to 
    [raise_notrace]. *)
val raise : exn -> 'a
val invalid_arg : string -> 'a
val failwith : string -> 'a

(** Propagates backtrace in exception handlers when called with .. | x -> .. raise_trace x 
    backtrace is propagated only if dassert_global_flag=true otherwise just raise is called
*)

val raise_trace : exn -> 'a  

val undefined : unit -> 'a

val unimplemented : unit -> 'a [@@alert unimp "Unimplemented code."]


(** Returns min/max based on comparison function *)
    
val min_cmp : ('a -> 'a -> int) -> 'a -> 'a -> 'a
val max_cmp : ('a -> 'a -> int) -> 'a -> 'a -> 'a

(* ---------- *)

exception SZS_Unknown

(* unsatisfiable ground possibly with assumptions *)
exception Unsatisfiable_gr 

(* unsatisfiable ground without assumtions; *)
(* solvers should NOT be used after Unsatisfiable_gr_na other than to get proof *)
exception Unsatisfiable_gr_na 

exception Eliminated



(*------------*)
(* init_rnd is called when options are read *)

val init_rnd : int -> unit 
val get_rnd_bits : unit -> int (* generated once; in all called returns the same bits *)
val re_init_rnd_bits: unit -> unit

(* ---------- *)

val answer_mode_ref : bool ref

val sat_incomplete_mode : bool ref
val unsat_incomplete_mode : bool ref

val iprover_start_time : float
val iprover_running_time : unit -> float

(*  Lazy debug  *)
(* for usage see predElim.ml *)

val dbg_out_pref : bool -> 'a list -> 'a -> ('a -> string) -> string -> string Lazy.t -> unit

val dbg_flag_msg : bool -> string -> unit
    
val dbg_env_set :  bool -> 'a list -> 'a -> (unit -> unit) -> unit


(** [assert (x())] if debug flag is set, no-op otherwise *)
val dassert : (unit -> bool) -> unit


(* gets path to the iprover executable if defined by /proc/self/exe *)
(* else raises Not_found *)

(** Re-export module List with faster List.map *)
(*
module List : module type of struct include List end
*)


val iprover_exe_name : unit -> string
val iprover_exe_path : unit -> string





(* ---------- *)

(** A ['a printer] is a function that takes an [out_channel] and a value of 
    type ['a] and prints it to stdout. This is very useful to write printers 
    for different types, and to pass as a [%a] argument to [*printf]. *)
type 'a printer =
  out_channel -> 'a -> unit




(* skips: str = "\n"  || str = "" || str="\x00" *)
val channel_first_non_empty_ln : in_channel -> string

(** Comparisons: types and interfaces *)

(** Result of a (total order) comparison *)
module Ord : sig
  type t = int

  val eq : int
  val gt : int
  val lt : int

  (** To "eq", "gt", "lt". *)
  val to_string : t -> string

  (** To "=", ">", "<". *)
  val to_sign : t -> string

  (** Maps [lt] to [gt] and vice-versa. *)
  val reverse : t -> t

  (** Given a function [f] returns a function [f'] such that [f' x y] = [f y x]. *)
  val reverse_f : ('a -> 'a -> t) -> 'a -> 'a -> t

  (** Given a [key] function, that is a function that maps type ['a] to a 
      key of type ['b], and a [cmp] function on the type of keys, [lift key 
      cmp] returns a function that compares values of type ['a] according to 
      their keys. 

      For instance [lift Term.get_fast_key compare] is a function equivalent 
      to [fun x y -> compare (Term.get_fast_key x) (Term.get_fast_key b)]. *)
  val lift : ('a -> 'b) -> ('b -> 'b -> t) -> ('a -> 'a -> t)

  val equal : t -> t -> bool
  module O : sig
    val (=)  : t -> t -> bool
    val (<>) : t -> t -> bool
  end
end



(** Result of a partial order comparison: 
    - ≻ ⇔ [GT]  
    - ≺ ⇔ [LT]
    - = ⇔ [EQ]
    - Otherwise [INC] *)
module PartialOrd : sig
  (** Result of a partial order *)
  type t = 
    | EQ 
    | GT
    | LT
    (* | GE *)
    (* | LE *)
    | INC

  (** To "EQ", "GT", "LT", "INC". *)
  val to_string : t -> string

  (** To "=", ">", "<", "?". *)
  val to_string : t -> string

  (** Converts a comparison result (1, 0, -1) to [GT], [EQ], [LT] respectively. *)
  val of_ord : int -> t

  exception Incomparable

  (** Converts a partial order result to an int comparison. Maps [GT] to 1, 
      [EQ] to 0, [LT] to -1, and raises [Incomparable] on [INC]. *)
  val to_ord : t -> int

  (** Converts a partial order result to an int in [0..3] (not to be confused 
      with [to_cmp]!). *)
  val to_int : t -> int

  (** Inverse of [to_int]. Raises [Invalid_argument] if argument is not in [0..3]. *)
  val of_int : int -> t

  (** Maps [GT] to [LT] and vice-versa, and is the identity for [EQ] and [INC]. *)
  val reverse : t -> t

  (** Given a function [f] returns a function [f'] such that [f' x y] = [f y x]. *)
  val reverse_f : ('a -> 'a -> t) -> 'a -> 'a -> t

  val equal : t -> t -> bool
  
  module O : sig
    val (=)  : t -> t -> bool
    val (<>) : t -> t -> bool
  end
end

(** EQ, GT, LT, INC are re-exported so we can use without [PartialOrd.] *)
type partial_ord = PartialOrd.t = EQ | GT | LT | INC



(** Type that can be (semantically) compared for equality. *)
module type HasEquality = sig
  type t
  val equal : t -> t -> bool
end

(** Type that can be totally ordered. *)
module type Ordered = sig
  type t
  val compare : t -> t -> Ord.t
end

(** Type that can be partially ordered. *)
module type PartialOrdered = sig
  type t
  val partial_compare : t -> t -> PartialOrd.t
end

(** Type that can be hashed. *)
module type Hashable = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end



(** Convenience functor to build infix operators from an [equal] function. *)
module EqMakeInfix (M: HasEquality) : sig
  val (=)  : M.t -> M.t -> bool
  val (<>) : M.t -> M.t -> bool
end

(** Convenience functor to build infix operators from a [compare] function. *)
module OrdMakeInfix (M: Ordered) : sig
  val (>)  : M.t -> M.t -> bool
  val (>=) : M.t -> M.t -> bool
  val (<)  : M.t -> M.t -> bool
  val (<=) : M.t -> M.t -> bool
end

(** Convenience functor to build infix operators from a [partial_compare] function. *)
module PartialOrdMakeInfix (M: PartialOrdered) : sig
  val (>)   : M.t -> M.t -> bool
  val (>=)  : M.t -> M.t -> bool
  val (>=!) : M.t -> M.t -> bool
  val (<)   : M.t -> M.t -> bool
  val (<=)  : M.t -> M.t -> bool
  val (<=!) : M.t -> M.t -> bool
end





(** More important types: option, list, etc. *)

module Option : sig
  type 'a t = 'a option

  exception None_opt

  (** [function Some _ -> true | None -> false] *)
  val is_some : 'a t -> bool
    
  (** [function Some _ -> false | None -> true] *)
  val is_none : 'a t -> bool
    
  (** [function Some x -> x | None -> raise None_opt] *)
  val get : 'a t -> 'a

  (** [get_fun f b = get (f b)] *)
  val get_fun : ('a t -> 'b t) -> 'a t -> 'b 

  (** Monadic bind: If x is Some y, [bind x f] feeds y to f (i.e. returns 
      [f y]), otherwise it propagates the None (without evaluating f at all) *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [return x = Some x] *)
  val return : 'a -> 'a t

  (** [default def = function Some x -> x | None -> def] *)
  val default : 'a -> 'a t -> 'a

  (** In a list of ['a t], keep the values inside [Some] and discard [None]. *)
  val filter_some : 'a t list -> 'a list

  (* val partition_some : ('a t -> 'b t) -> 'a t list -> ('b t list -> 'a t list) *)
  val partition_some : ('a -> 'b t) -> 'a list -> ('b list * 'a list)

  val join : 'a option option -> 'a option

  val lift1 : ('a -> 'b) -> ('a t -> 'b t)

  val lift : ('a -> 'b) -> ('a t -> 'b t)

  val lift2 : ('a1 -> 'a2 -> 'b) -> ('a1 t -> 'a2 t -> 'b t)

  val lift3 : ('a1 -> 'a2 -> 'a3 -> 'b) -> ('a1 t -> 'a2 t -> 'a3 t -> 'b t)

  (** [to_string inner_to_string], where [inner_to_string] converts ['a] to 
      [string], is a function that converts ['a option] to [string] *)
  val to_string : ('a -> string) -> 'a option -> string

  module O : sig
    (** As [bind]. *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t

    (** As [default] with inverted arguments, i.e. value contained left-hand side if Some _, else right-hand side. *)
    val (|?) : 'a t -> 'a -> 'a

    (** If left-hand side is [Some x], return [Some x]; else if right-hand side is [Some y], return [Some y]; else return [None]. *)
    val (<|>) : 'a t -> 'a t -> 'a t
  end
  (* include module type of I *)

  (** Lifts [eq] over ['a] to an equality over ['a option]. *)
  val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (** Orders [None < Some _], and orders [Some x, Some y] via [cmp x y]. *)
  val compare : cmp:('a -> 'a -> Ord.t) -> 'a t -> 'a t -> Ord.t
end

(** Old interface, unscoped. *)

exception None_opt

(* get_some get option value or raises None_opt if the option is None *)
val get_some : 'a option -> 'a
val get_some_fun : ('b -> 'a option) -> ('b -> 'a)
val is_some : 'a option -> bool

val filter_some : 'a option list -> 'a list

(* "split_some apply list" returns (res_list, none_list); *)
(* iterates over list applying apply and collecting results in res_list *)
(* if Some(res); when apply returns None we add the element of list into  non_list *)

val split_some : ('a -> 'b option) -> 'a list -> ('b list * 'a list)



(** Same semantics as [Option], but has the same memory representation as the 
    type itself. This means no boxing, no allocation, and no indirection. 
    Useful for performance-critical parts of the code where the overhead of
    boxing is unaceptable. Using this interface ensure type-safety. *)
module SmallOption (M : sig
  type t
  val none : t  
  val is_none : t -> bool (** For immediate values (int, bool, enums without arguments), can simply be [fun x -> x == none]. *)
end) : sig
  (** The type of [M.t]. *)
  type elt = M.t
  (** The type of [Small_option]s of [M.t]. *)
  type t

  (** Instead of match opt with
                 | Some x -> foo x
                 | None   -> bar

      write      match' opt
                   ~some:(fun x -> foo x)
                   ~none:(fun _ -> bar) 
  *)
  val match' : t -> some:(elt -> 'a) -> none:(unit -> 'a) -> 'a

  (** Construct [some x]. Raises [Invalid_argument] if [x] clashes with the value of [none]. *)
  val some : elt -> t
  (** Equivalent, but disables checks if [dassert] is off. *)
  val some' : elt -> t
  (** Construct [none] *)
  val none : t

  (** Predicates for testing whether a value is [some x] or [none]. *)
  val is_some : t -> bool
  val is_none : t -> bool

  (** If the value is [some y], return [y], otherwise raise [Invalid_argument]. *)
  val get : t -> elt
  (** As [get], but disables checks if [dassert] is off. *)
  val get' : t -> elt
  (** If the value is [some y], return [y], otherwise return [default]. *)
  val value : t -> default:elt -> elt

  (** Monadic interface: [bind x f] is [f y] if [x] is [some y] or [none] if [x] is [none]. *)
  val bind : t -> (elt -> t) -> t

  (** Lifts an equality predicate on [elt]s to an equality predicate on [t]s. *)
  val equal : (elt -> elt -> bool) -> t -> t -> bool
  (** Lifts a comparison predicate on [elt]s to a comparison predicate on [t]s. *)
  val compare : (elt -> elt -> int) -> t -> t -> int

  module O : sig
    (** As [bind]. *)
    val (>>=) : t -> (elt -> t) -> t
    val (let*) : t -> (elt -> t) -> t

    (** As [value]. *)
    val (|?) : t -> elt -> elt
  end
end

module SmallOptionMagic (M : sig type t end) : module type of SmallOption(struct 
  type t = M.t 
  let none : t = (Obj.magic 0) 
  let is_none (x:t) = x == (Obj.magic 0) 
end)

(** An int ≠ -1. Useful for monotonically increasing counters. *)
module IOption : module type of SmallOption(struct 
  type t = int 
  let none = -1 
  let is_none x = x = -1 
end)







(* ----------- *)
(** {1 Lists} **)
(* ----------- *)

(** Extra functions for lists (also available as [List.X]) *)
module ListExtra : sig
  (** [make n el] creates a list of [n] repetitions of [el]. *)
  val make : int -> 'a -> 'a list



  (** [is_empty = function [] -> true | _ -> false ] *)
  val is_empty : 'a list -> bool

  (** [is_nonempty = function [] -> false | _ -> true ] *)
  val is_nonempty : 'a list -> bool

  (** [is_singleton = function [_] -> true | _ -> false ] *)
  val is_singleton : 'a list -> bool

  (** Get the value in a singleton list. *)
  val get_singleton : 'a list -> 'a

  (** Splits list in head/tail, without giving non-exhaustive pattern matching
      warning. Raises at runtime if list is empty. *)
  val hd_tl : 'a list -> 'a * 'a list



  (** [equal eq] lifts an equality predicate on ['a] to an equality predicate
      on ['a list]. *)
  val equal : eq:('a -> 'a -> bool) -> 'a list -> 'a list -> bool

  (** [compare cmp] lifts a comparison on ['a] to a comparison on ['a list], 
      comparing lexicographically. If one list has a bigger length, such that 
      the other is a prefix, then it compares as greater. *)
  val compare : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> int

  (** [mem eq x l] is true iff there is an element [y] in [l] such that [eq x y]. *)
  val mem : eq:('a -> 'a -> bool) -> 'a -> 'a list -> bool

  (** [mem_assoc eq x l] as [mem eq x (List.map (fun (x,_) -> x) l]. *)
  val mem_assoc : eq:('a -> 'a -> bool) -> 'a -> ('a * _) list -> bool



  (** Number of items in list for which the predicate returns true. *)
  val count : ('a -> bool) -> 'a list -> int


  (** [take n l]  take first [n] (with n>0) elements. The order is preserved.  If l has less than n elements then l is returned == to original *)
  val take : int -> 'a list -> 'a list 



  (** As [List.find] but returns index instead of element. *)
  val find_index : ('a -> bool) -> 'a list -> int

  (** Finds the first element [x] in [l] for which [p x = Some y], and returns [y]. 
      @raises: Not_found if no such element. *)
  val find_map : ('a -> 'b option) -> 'a list -> 'b

  (** As [find_map], but returns [Some y] instead of [y] and [None] instead 
      of rasing. *)
  val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option

  (** Maps [f] to list, then filters only those elements that are [Some _].
      Equivalent to [fun f l -> List.map f l |> List.filter Option.is_some |> 
      List.map Option.get], but more efficient of course. *)
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  (** [List.concat % List.map], but more efficient. *)
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list

  (** Splits the given list into a list of chunks of the given size. *)
  val partition_chunks : int -> 'a list -> 'a list list

  (** As [List.for_all2], but returns [false] if lists have different lengths *)
  val for_all2' : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

  (** As [List.exists2], but returns [false] if lists have different lengths *)
  val exists2' : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

  (** [reduce f (hd::tl)] is [fold_left f hd tl], while [reduce f []] raises
      [Invalid_argument]. *)
  val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a



  (** [Ord.lt] if list is in ascending order, [Ord.gt] if list is in 
      descending order, [Ord.eq] otherwise. *)
  val is_sorted : ('a -> 'a -> int) -> 'a list -> int

  (** As [List.sort] but substantially more efficient on already sorted or 
      reverse sorted lists. *)
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list

  (** As [List.sort_uniq] but substantially more efficient on already sorted 
      or reverse sorted lists. *)
  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

  (** Finds the min/max element wrt the given comparison *)
  val min : ('a -> 'a -> int) -> 'a list -> 'a
  val max : ('a -> 'a -> int) -> 'a list -> 'a



  (** Remove one element from a list (if it exists), comparing with [eq]. *)
  val remove : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list

  (** Remove one element from a list (if it exists), comparing with physical equality (==) *)
  val removeq : 'a -> 'a list -> 'a list

  (** Remove one element from a list (if it exists), comparing with eq *)
  (* val remove_by : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list *)

  (** As [remove], but removes all occurences *)
  val remove_all : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list

  (** As [removeq], but removes all occurences *)
  val removeq_all : 'a -> 'a list -> 'a list

  (** As [remove_by], but removes all occurences *)
  (* val remove_by_all : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list *)

  (** Removes _consecutive_ duplicates (wrt [eq]), that is, assumes a sorted list. *)
  val remove_duplicates : eq:('a -> 'a -> bool) -> 'a list -> 'a list

  (** Removes _consecutive_ duplicates (wrt [(==)]), that is, assumes a sorted list. *)
  val removeq_duplicates : 'a list -> 'a list

  (** Removes repeated elemets wrt cmp keeps the first occurence 
      no assumptions if repeated elements are apart; no assumptions that list is ordered 
      the order is preserved *)
  val remove_repeated : cmp:('a -> 'a -> int) -> 'a list -> 'a list

  (** True if there are repeated elements in the list *)
  val check_repeated : cmp:('a -> 'a -> int) -> 'a list -> bool

  (** Removes from the first list. O(nlogn). *)
  val multiset_minus : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list



  (** [cons_some (Some x) l] is [x::l]
      [cons_some (None) l] is [l] *)
  val cons_some : 'a option -> 'a list -> 'a list

  (** [cons_ref x l] is a shorthand for [l := x :: !l] *)
  val cons_ref : 'a -> 'a list ref -> unit

  (** As [cons_ref] *)
  val (=::) : 'a -> 'a list ref -> unit

  (** [cons_some (Some x) l] is [l := x :: !l]
    * [cons_some (None) l] is [()] *)
  val cons_some_ref : 'a option -> 'a list ref -> unit



  (** In a list of options, keeps only those elements that are [Some x]
        Example: [filter_some [Some 1; None; Some 3] = [1;3]] *)
  val filter_some : 'a option list -> 'a list

  (** [partition p l] returns a tuple [(t,f)] where [t] is [List.filter p l] and
      [f] is [List.filter (fun x -> not (p x)) l]. *)
  val partition : ('a -> bool) -> 'a list -> ('a list * 'a list)



  (** As [compare]. *)
  val compare_lex : 
    ('a -> 'a -> int) -> 
    'a list -> 'a list -> int

  (** Returns the list of minimal/maximal elements wrt the given ordering. 
      Skips duplicates wrt to (==). *)
  val min_elements_partial_ord : ('a -> 'a -> PartialOrd.t) -> 'a list -> 'a list
  val max_elements_partial_ord : ('a -> 'a -> PartialOrd.t) -> 'a list -> 'a list

  (** Returns a minimal element in l that is smaller than x (PartialOrd.LT), if no such element in l then x. *)
  val min_below_partial_ord : ('a -> 'a -> PartialOrd.t) -> 'a -> 'a list -> 'a
  val max_above_partial_ord : ('a -> 'a -> PartialOrd.t) -> 'a -> 'a list -> 'a

  (** Returns true if all elements are linearly sorted and comparable, EQ is allowed but not INC *)
  val is_linearly_sorted : ('a -> 'a -> PartialOrd.t) -> 'a list -> bool

  (** Topological sort: smaller first; returns == to input if and only if input is already top. sorted 
      (ie != then at least one proper swap).
      Simple implementation every case N^2 *)      
  val topological_sort_simp :  ('a -> 'a -> PartialOrd.t) -> 'a list -> 'a list 
 
  (** Checks if list is topologically sorted *)
  val is_topologically_sorted_simp : ('a -> 'a -> PartialOrd.t) -> 'a list -> bool
 

  (** Randomly shuffle a list  *)
  val shuffle : 'a list -> 'a list


  (** Given a printer [print_el] for type ['a], [print ~first ~last ~sep] 
      returns a printer for type ['a list].
      Default values:
        ~first:""
        ~last:""
        ~sep:"; " *)
  val output : 
    ?first:string -> ?last:string -> ?sep:string -> 
    (out_channel -> 'a -> unit) -> 
    (out_channel -> 'a list -> unit)

  (** For lists of strings *)
  val string_output : 
    ?first:string -> ?last:string -> ?sep:string -> 
    (out_channel -> string list -> unit)

  (** Similar interface as [output], but returns string. *)
  val to_string : 
    ?first:string -> ?last:string -> ?sep:string -> 
    ('a -> string) -> 
    'a list -> string

  (** Similar interface as [output], but returns string. *)
  val string_to_string : 
    ?first:string -> ?last:string -> ?sep:string -> 
    string list -> string

  (** As [output], but default for [first], [last], [sep] is [""] *)
  val output' : 
    ?first:string -> ?last:string -> ?sep:string -> 
    (out_channel -> 'a -> unit) -> 
    (out_channel -> 'a list -> unit)

  (** As [to_string], but default for [first], [last], [sep] is [""] *)
  val to_string' : 
    ?first:string -> ?last:string -> ?sep:string -> 
    ('a -> string) -> 
    'a list -> string
end

module List : sig
  include module type of List

  (** These two functions silently use polymorphic comparison, so they should 
      not be used. If it's really what is needed, it's better to use 
      [List.X.mem ~eq:Poly.(=)], which makes it explicit. *)
  val mem : 'a -> 'a list -> bool [@@deprecated]
  val mem_assoc : 'a -> ('a * 'b) list -> bool [@@deprecated]

  module X = ListExtra
end

(*-------------------*)
module IListKey : sig
  type t = int list
  val compare : t -> t -> int (* lex, see List.X.compare *) 
end

module ILMap : Map.S with type key = IListKey.t
module ILSet  : Set.S with type elt = IListKey.t
    
(** These non-namespaced functions are kept for backwards compatibility. *)

(** As [ListExtra.is_empty]. *)
val list_is_empty : 'a list -> bool

(** As [ListExtra.is_nonempty]. *)
val list_non_empty : 'a list -> bool

(** As [ListExtra.is_singleton]. *)
val list_is_singleton : 'a list -> bool

val list_skip : 'a -> 'a list -> 'a list

val list_remove_last : 'a list -> ('a * 'a list) option

(* explicitly maps from left to right, 
   since order can matter when use imperative features *)
val list_map_left : ('a -> 'b) -> 'a list -> 'b list

(** Smiliarly but [f] takes index and element (see List.mapi) *)
val list_mapi_left : (int -> 'a -> 'b) -> 'a list -> 'b list

(** Last argument is separator. Optional opening and closing delimiters in [~first] and [~last]. *)
val list_to_string : ?first:string -> ?last:string -> ('a -> string) -> 'a list -> string -> string

(** Shortcut for lists of strings. *)
val list_of_str_to_str : ?first:string -> ?last:string -> string list -> string -> string

val list_findf : ('a -> 'b option) -> 'a list -> 'b option
(*--- used in maps for conting objects; None-> Some(1) --*)





(** Compatibility when ocaml version is <4.14 *)
module Seq : sig
  include module type of Seq
  val take : int -> 'a t -> 'a t
  val (@) : 'a t -> 'a t -> 'a t 
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t 
  val find_map : ('a -> 'b option) -> 'a t -> 'b option
end



(** Resizable array (i.e. C++ vector) *)
module DynArray : sig
  (** These functions are identical to [Array] *)
  type 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val make : int -> 'a -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val copy : 'a t -> 'a t

  (** As make, but reserves some capacity. *)
  val make_capacity : len:int -> cap:int -> 'a -> 'a t

  (** Array with 0 size and 0 capacity. *)
  val empty : unit -> 'a t

  (** Array with 0 size and some capacity. *)
  val empty_capacity : int -> 'a t

  (** Enlarges the array. If attempting to resize to some [n] < [length arr], 
      raises [Invalid_argument "resize to smaller than array length"].  *)
  val resize : 'a t -> int -> unit

  (** Adds a value to the end of the array, enlarging if necessary. *)
  val push_back : 'a t -> 'a -> unit

  (** The "physical" capacity of the underlying array (distinct from the 
      "logical" length of the dynarray. *)
  val capacity : 'a t -> int
end





(** Tuples of two elements *)
module Pair : sig
  type ('a, 'b) t = 'a * 'b

  (** [make a b = (a,b)] *)
  val make : 'a -> 'b -> ('a, 'b) t

  (** As [Pervasives.fst]. *)
  val first : ('a, 'b) t -> 'a
  (** As [Pervasives.snd]. *)
  val second : ('a, 'b) t -> 'b

  (** [map_fst f (a,b) = (f a, b)] *)
  val map_fst : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  (** [map_snd f (a,b) = (a, f b)] *)
  val map_snd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  (** [of_list [a;b] = (a,b)]
      @raises: Invalid_arg if list doesn't have exactly 2 elements. *)
  val of_list : 'a list -> ('a, 'a) t

  (** [to_list (a,b) = [a;b]] *)
  val to_list : ('a, 'a) t -> 'a list


  (** Given a printer for ['a] and a printer for ['b], return a printer for
      ['a * 'b]. *)
  val output : 
    (out_channel -> 'a -> unit) -> 
    (out_channel -> 'b -> unit) -> 
    out_channel -> ('a, 'b) t -> unit

  (** Similar interface as [output] but returns string. *)
  val to_string : 
    ('a -> string) -> 
    ('b -> string) -> 
    ('a, 'b) t -> string

  (** [equal eq1 eq2 (a,b) (c,d)] iff [eq1 a c && eq2 b d] *)
  val equal :
    ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) ->
    ('a, 'b) t -> ('a, 'b) t -> bool

  (** Compare by first element, and if equal, compare by second element. *)
  val compare :
    ('a -> 'a -> Ord.t) ->
    ('b -> 'b -> Ord.t) ->
    ('a, 'b) t -> ('a, 'b) t -> Ord.t

  (** Compare by second element, and if equal, compare by first element. *)
  val compare_rev :
    ('b -> 'b -> Ord.t) ->
    ('a -> 'a -> Ord.t) ->
    ('a, 'b) t -> ('a, 'b) t -> Ord.t
end

exception Not_a_singleton
val get_singleton_from_list : 'a list -> 'a



(** Tuples of three elements. *)
module Triple : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  (** [make a b = (a,b)] *)
  val make : 'a -> 'b -> 'c -> ('a, 'b, 'c) t

  (** Gets first element of the triple *)
  val first : ('a, 'b, 'c) t -> 'a
  (** Gets second element of the triple *)
  val second : ('a, 'b, 'c) t -> 'b
  (** Gets third element of the triple *)
  val third : ('a, 'b, 'c) t -> 'c

  (** [map_fst f (a,b,c) = (f a, b, c)] *)
  val map_fst : ('a -> 'd) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  (** [map_snd f (a,b,c) = (a, f b, c)] *)
  val map_snd : ('b -> 'd) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  (** [map_thd f (a,b,c) = (a, b, f c)] *)
  val map_thd : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t

  (** [of_list [a;b] = (a,b)]
      @raises: Invalid_arg if list doesn't have exactly 2 elements. *)
  val of_list : 'a list -> ('a, 'a, 'a) t

  (** [to_list (a,b) = [a;b]] *)
  val to_list : ('a, 'a, 'a) t -> 'a list


  (** Given a printer for ['a] and a printer for ['b] and a printer for ['c], 
      return a printer for ['a * 'b]. *)
  val output : 
    (out_channel -> 'a -> unit) -> 
    (out_channel -> 'b -> unit) -> 
    (out_channel -> 'c -> unit) -> 
    out_channel -> ('a, 'b, 'c) t -> unit

  (** Similar interface as [output] but returns string. *)
  val to_string : 
    ('a -> string) -> 
    ('b -> string) -> 
    ('c -> string) -> 
    ('a, 'b, 'c) t -> string
end

exception Not_a_triple
val get_triple_from_list  : 'a list -> 'a * 'a * 'a

val get_last_pair_from_triple_list : 'a list -> 'a * 'a

exception Empty_list
val split_list : 'a list -> 'a * ('a list) 





(*---------------*)

val cnt_opt_update : int option -> int option

type 'a param = Def of 'a | Undef 

(* true if param is Def and false if Undef*)
val param_is_def: 'a param -> bool
val param_is_undef: 'a param -> bool

exception Undef_param

val id_fun   : 'a -> 'a
val unit_fun : 'a -> unit 





(*---------------*)

val apply_fun : ('a -> 'b) param -> 'a -> 'b
val apply_fun_if_def : ('a -> unit) param -> 'a -> unit


(* get_param_val gets prameter value or raises Undef is the paramter is Undef *)
val get_param_val : 'a param -> 'a 
val get_param_val_fun : ('b -> 'a param) -> ('b -> 'a) 

(* elements and ref to elem of indexies and all others*)

  type 'a elem = Elem of 'a | Empty_Elem
  type 'a ref_elem = ('a elem) ref










(* does nothing *)
val clear_memory : unit -> unit 

val print_live_memory_usage : unit -> unit
val print_memory_usage : unit -> unit
val print_mem_time_fun : ('a->'b)-> 'a -> 'b

(* print_objsize name t;  prints object size in Megabytes *)

val print_objsize : string -> 'a -> unit

(** Get memory address of value (for debug purposes only! unreliable!) *)
val addressof : ?bits:int -> 'a -> int

(*------- can be used to test memory usage running the same function n times -------*)
(*------- printing memory statistics -----------------------------------------------*)

val mem_test : (unit->unit) -> int -> unit

val string_of_char : char -> string

(* fun is a function unit -> unit, get_time_fun returns time taken by fun  *)
(* truncated by tranc digits after . *)
val get_time_fun : int -> (unit->unit)-> float

(* truncates float to n digits after . *)
val truncate_n : int -> float -> float 

(* outcome of compare fun.*)
val cequal   : int
val cgreater : int
val cless    : int

(** Lifts a key function to a compare function, i.e. [cmp_of_key key x y] = [compare (key x) (key y)] *)
(* val cmp_of_key : ('a -> 'b) -> ('a -> 'a -> int) *)
(** As [cmp_of_key], but specialised for int keys *)
(* val cmp_of_int_key : ('a -> int) -> ('a -> 'a -> int) *)
(** As [cmp_of_key], but specialised for bool keys *)
(* val cmp_of_bool_key : ('a -> bool) -> ('a -> 'a -> int) *)

(* val cmp_bool : bool -> bool -> int *)






(* *)
val param_str_ref : string ref 

val pref_str      : string

(* pref string according to tptp_safe_out option*)
val s_pref_str    : unit -> string 

(* dash_str str:  ------- str ---------*)
val dash_str      : string -> string 

val add_param_str : string -> unit
val add_param_str_front : string -> unit
val param_str_new_line : unit -> unit


(** Function composition: [(f %% g) x] = [f (g x)]. *)
val (%%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

(** Function composition in pipe order: [(f %> g) x] = [f x |> g]. *)
val (%>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)

val compose_sign  : bool -> ('a -> 'a -> int) -> ('a -> 'a -> int)
(* hash sum where the first arg is rest and second is next number*)
val hash_sum : int -> int ->int 

(*let hash_list hash_elem list*)
val hash_list : ('a -> int) -> 'a list -> int

exception Termination_Signal

(*----------------Processes-----------------*)
(* add_child_process pid *)
val add_child_process           : int -> unit 

(* add_child_process_channels (in_channel,out_channel,error_channel) *)
val add_child_process_channels  : 
    (in_channel * out_channel * in_channel) -> unit


(* removes from the list without killing *)
val remove_top_child_process_channels : unit -> unit 


val kill_all_child_processes : unit -> unit

(*----------------End Processes-----------------*)

(** Important: monomorphic compare for int *)
(** Deprecated: use [compare]. *)
(* val int_compare : int -> int -> int *)

(* composes functions *)

val compose_12   : ('a->'b)->('c->'d ->'a) -> 'c->'d -> 'b

(** cmp that returns just 0 *)
val cmp_const_eq : 'a -> 'a -> int

(** Very often we write [some_reference := func !some_reference], instead
    we can write [some_reference @= func]. 
    E.g.     t_map := !t_map |> TMap.add t s
    becomes  t_map @= TMap.add t s *)
val (@=) : 'a ref -> ('a -> 'a) -> unit



(** Used for localization of vars, binding can be applied for vars, terms, 
    clauses. *)
module Bind : sig
  type 'a t = int * 'a

  (** Apply the bind *)
  val bind : int -> 'a -> 'a t

  (** Throw away the bind  *)
  val unbind : 'a t -> 'a

  (** Return the bound *)
  val get_bound : 'a t -> int



  (** Apply a bind to all elements of a list *)
  val bind_list : int -> ('a list) -> ('a t) list

  (** Propagate a binding to a list: [propagate_to_list (b,[l1;...;ln]) = [(b,l1);...;(b,ln)]] *)
  val propagate_to_list :  ('a list) t -> ('a t) list

  (** Apply a function to the bound value *)
  val map : ('a -> 'b) -> 'a t -> 'b t



  (** Given a printer for ['a], return a printer for ['a t]. *)
  val output : 
    (out_channel -> 'a -> unit) -> 
    out_channel -> 'a t -> unit

  (** Similar interface as [output] but returns string. *)
  val to_string : ('a -> string) -> 'a t ->  string



  (** Lifts eq to an equality relation on bound values. If not provided uses [Pervasives.(=)] *)
  val equal : eq:('a -> 'a -> bool) -> ('a t -> 'a t -> bool)

  (** Lifts cmp to a lexicographical, if not provided uses [Pervasives.compare] *)
  val compare : cmp:('a -> 'a -> int) -> ('a t -> 'a t -> int)

  (** As [equal]. *)
  (* val (=) : eq:('a -> 'a -> bool) -> ('a t -> 'a t -> bool) *)

  (* module O : sig
    (** Equality of bounds and physical equality of values. *)
    val (==) : ('a t -> 'a t -> bool)
    val (!=) : ('a t -> 'a t -> bool)
  end *)
end

type 'a bind = 'a Bind.t

val propagate_binding_to_list :  ('a list) bind -> ('a bind) list

val apply_to_bounded : ('a -> 'b) -> 'a bind -> 'b bind

val binded_to_string  : ('a -> string) -> 'a bind ->  string

(* lexicographic comparison of pairs*)
val pair_compare_lex : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a*'b -> 'a*'b -> int)

(* bool operations *)
val bool_plus : bool -> bool -> bool

(* returns 1 if true and 0 if false *)
(* in OCaml true >= false and compare true false = 1 *)
(* val bool_to_int : bool-> int *)

(*-------- folds a function over intervals -------------*)
(*  fold_up_interval f a b init_val *)
(* folds f from a to b inclusive *)
(* f rest i *)

val fold_up_interval   : ('a -> int -> 'a) -> int -> int -> 'a -> 'a
val fold_down_interval : ('a -> int -> 'a) -> int -> int -> 'a -> 'a





(* ------------ *)
(** {1 Output} **)
(* ------------ *)

(** If [tptp_safe_out_ref] is true then "% " is added to all out_str output.
    [tptp_safe_out_ref] by default is false reassigned by tptp_safe_out input option *)
val tptp_safe_out_ref : bool ref

(** Maps "foo" to "% foo" *)
val tptp_safe_str : string -> string

(** [out_str "foo"] prints "% foo" to stdout. *)
val out_str : string -> unit

(* out if debug is on *)
(*val out_str_debug : string -> unit*)

(** [out_err_str "foo"] prints "% foo" to stdout. *)
val out_err_str : string -> unit 

(* out in stderr *)
val out_warning : string -> unit 

(** Given a printer, it returns another printer that is the same except it 
    outputs a "% " at the start. *)
val output_tptp : (out_channel -> 'a -> unit) -> (out_channel -> 'a -> unit)
val output_tptp : 'a printer -> 'a printer


(** gets first non-empty string if exists in the list; returns "" otherwise *)
val get_first_non_empty_str : string list -> string


(** As ListExtra.compare_lex *)
val list_compare_lex : ('a -> 'a -> int) -> 'a list -> 'a list -> int



(** Apply a series of comparisons lexicographically: compare by first element, 
    if equal then compare by the second, etc. If equal by all, then equal. *)
val lex_combination  : ('a -> 'a -> int) list -> 'a -> 'a -> int

(** If comparisons are known, use these rather than the list version (overhead 
    is at least 1/3rd, plus memory allocations, plus missed optimisations). *)
val lex_combination2 : 
  ('a -> 'a -> int) -> ('a -> 'a -> int) -> 
  'a -> 'a -> int
val lex_combination3 : 
  ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a -> 'a -> int) -> 
  'a -> 'a -> int
val lex_combination4 : 
  ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a -> 'a -> int) -> 
  'a -> 'a -> int
val lex_combination5 : 
  ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a -> 'a -> int) -> 
  'a -> 'a -> int
val lex_combination1 : 
  ('a -> 'a -> int) -> 'a -> 'a -> int


(* apply iteratively funs in a list to the result  *)
(* let fold_left_fun_list fun_list x = *)

val fold_left_fun_list : ('a -> 'a) list -> 'a -> 'a
val iter_fun_list : ('a -> unit) list -> 'a -> unit

val fix_point_eq : ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a

val fix_point : ('a -> 'a) -> 'a -> 'a

(* in list_is_max_elem and list_get_max_elements
   we assume that compare as follows: 
   returns cequal if t greater or equal to s and 
   returns cequal+1 if t is strictly greater
   returns cequal-1 if it is not the case
  Note: it is assumed that 
   if t (gr or eq) s and s (gr or eq) t then t==s
*)    

val list_is_max_elem_v :   ('a -> 'a -> int) -> 'a -> 'a list -> bool

val list_get_max_elements_v : ('a -> 'a -> int) -> 'a list -> 'a list

(* for usual orderings *)
val list_is_max_elem :   ('a -> 'a -> int) -> 'a -> 'a list -> bool

(* finds max element in the list if the list is empty raises Not_found*)
val list_find_max_element : ('a -> 'a -> int) -> 'a list -> 'a

val list_find_min_element : ('a -> 'a -> int) -> 'a list -> 'a

val list_find_all_min_elements : ('a -> 'a -> int) -> 'a list -> 'a list
val list_find_all_max_elements : ('a -> 'a -> int) -> 'a list -> 'a list

(* as above but also filter on test *)
val list_find_max_element_p : ('a -> bool) -> ('a -> 'a -> int) -> 'a list -> 'a
val list_find_min_element_p : ('a -> bool) -> ('a -> 'a -> int) -> 'a list -> 'a



val list_find2 : ('a -> 'b -> bool) -> ('a list) -> ('b list) -> ('a *'b) 

val list_return_g_if_f2 : 
    ('a -> 'b -> bool) -> ('a -> 'b -> 'c) -> ('a list) -> ('b list) -> 'c

(* finds first el. a' b' not equal by compare_el, 
  which suppose to return ctrue if equal,
  and returns compare_el 'a 'b 
*)

val list_find_not_equal :  
    ('a -> 'b -> int) -> ('a list) -> ('b list) -> int

(** true if lists have identical elements (==) *)  
val list_identical_elts : ('a list) -> ('a list) -> bool

val list_find_not_identical :
    ('a list) -> ('a list) -> 'a * 'a

val list_exists_not_identical :
    ('a list) -> ('a list) -> bool

(* minimise_list ~keep ~test list  *)
(* returns a minimal substet of the list on which test is true *)
(* keep -- elements that must be kept *)
(* we assume test is monotone -- if test is true on a sub-list then it is true on all lists containing this sub-list *)
(* can raise Not_found if the input list does not satisfy the test *)
(* cmp: compare for priority smaller prioritised for inclusion (larger are eliminated first) *)
val minimise_list : ?cmp:('a -> 'a -> int) -> keep:('a -> bool) -> test:('a list -> bool) -> 'a list -> 'a list

(* returns a list of minimal subsets satisfying test which do not overlap with exception of keep *)
(* if the inital list does not satisfy test then returns [] (does not raise Not_found)*)

val minimise_list_enum : ?cmp:('a -> 'a -> int) -> keep:('a -> bool) -> test:('a list -> bool) -> 'a list -> ('a list) list



(* association lists *)

type ('a, 'b) ass_list = ('a*'b) list

(* appends ass lists: if list1 and list2 have
 elem with (k,v1)  and (k,v2) resp. then new list will have (k,(f !v1 !v2))
 otherwise  appends (k1,v1) and (k2,v2)*)

val append_ass_list : 
    ('b -> 'b -> 'b) -> ('a, 'b) ass_list -> ('a, 'b) ass_list -> ('a, 'b) ass_list

type 'a num_ass_list =  ('a,int) ass_list


(*----------------------------------------------*)
(*------------- reachibility depth -------------*)
(* given a module with an elemet, and reachability relation *)
(* outputs map of rechable elements with the reachability depth *)


module type El =
  sig
    type t 
    val compare : t -> t -> int
  end


module MakeReach : functor
    (El:El) 
    (ReachMap:Map.S with type key=El.t) 
    (ElSet:Set.S with type elt = El.t)     
  ->
  sig
    type reach_map_el = int ReachMap.t
    val compute_reachability_set :
        succ_rel:(ReachMap.key -> ElSet.t) -> ElSet.t -> reach_map_el
    val compute_reachability_list :
        succ_rel:(ReachMap.key -> ElSet.t) -> ElSet.elt list -> reach_map_el
  end

(*
module type ReachRel =
  sig
    type t 
    val succ_rel : t -> t list 
    val compare : t -> t -> int
  end


module MakeReach :
  functor (ReachRel : ReachRel) ->
    sig
      type e = ReachRel.t
      module ReachMap :
          sig   
	    type key = ReachRel.t
	    type 'a t = 'a Map.Make(ReachRel).t
	    val empty : 'a t
	    val is_empty : 'a t -> bool
	    val add : key -> 'a -> 'a t -> 'a t
            val find : key -> 'a t -> 'a
	    val remove : key -> 'a t -> 'a t
	    val mem : key -> 'a t -> bool
	    val iter : (key -> 'a -> unit) -> 'a t -> unit
	    val map : ('a -> 'b) -> 'a t -> 'b t
	    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
	    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
	    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          end
      type reach_map_el = int ReachMap.t
      val compute_reachability : ReachMap.key list -> int ReachMap.t
    end

*)


(*----------- Output Buffers/Channels ----------------------*)

(* string stream can be e.g. a buffer or a channel *)

(** Generic string stream that you write chars/strings to. Examples: stdout, stderr, string buffers. *)
type 'a string_stream = {
  stream : 'a;
  stream_add_char : char   -> unit;
  stream_add_str  : string -> unit;
}

val stdout_stream : out_channel string_stream

val stdout_stream : out_channel string_stream
   
(** [list_to_stream s to_str_el l sep] itreates [to_str_el s el] over all 
    elements in [l] and adds separator [str] in between elem. *)
val list_to_stream : 
  ?first:string -> ?last:string -> 
  'a string_stream ->  
  ('a string_stream -> 'b -> unit) ->
  'b list -> 
  string -> 
  unit

(* "let to_string = to_string_fun_from_to_stream_fun 30 to_stream" *)
(*    creates to_string function from to_stream function with      *)
(*    initial buffer size 30                                       *)


val to_string_fun_from_to_stream_fun :
           int -> (Buffer.t string_stream -> 'a -> 'b) -> 'a -> string
(*
val to_string_fun_from_to_stream_fun :
    int->
    ('a string_stream -> 'b -> unit) ->
    ('b -> string) 
*)

(** Create a buffer of a given size. *)
val create_buffer_stream : int -> Buffer.t string_stream 

(** Get string in buffer. *)
val to_string_buffer_stream :  Buffer.t string_stream -> string




val param_to_string : ('a -> string) -> 'a param -> string

val param_to_stream : 
    ((('a string_stream) -> 'b -> unit )-> 
      ('a string_stream)  -> 'b param -> unit)


(** [formatter_of_filename a n] opens the file [n] and returns a
    formatter writing into the opened file. If [a] is true and the
    file exists it is opened for appending, otherwise it is truncated
    to zero length if it exists. Return the formatter writing to
    stdout if file name is "-".  The [Sys_error] exception is not
    caught here but passed to the calling function. *)
val formatter_of_filename : bool -> string -> Format.formatter

(** [pp_any_array pp sep ppf a] prints the elements of the array [a]
    formatted with the [pp] formatting function separated by the
    string [sep] into the formatter [ppf]. *)
val pp_any_array :
  (Format.formatter -> 'a -> unit) ->
  string -> Format.formatter -> 'a array -> unit

(** [pp_any_list pp sep ppf l] prints the elements of the list [l]
    formatted with the [pp] formatting function separated by the
    string [sep] into the formatter [ppf]. *)
val pp_any_list :
  (Format.formatter -> 'a -> unit) ->
  string -> Format.formatter -> 'a list -> unit

(** [pp_string_list pp sep ppf l] prints the elements of the list of
    strings [l] separated by the string [sep] into the formatter
    [ppf]. *)
val pp_string_list : string -> Format.formatter -> string list -> unit

(** [pp_string_array pp sep ppf a] prints the elements of the array of
    strings [a] separated by the string [sep] into the formatter
    [ppf]. *)
val pp_string_array : string -> Format.formatter -> string array -> unit

(** [pp_int_list pp sep ppf l] prints the elements of the list of
    integers [l] separated by the string [sep] into the formatter
    [ppf]. *)
val pp_int_list : string -> Format.formatter -> int list -> unit

(** [pp_int_array pp sep ppf a] prints the elements of the array of
    integers [a] separated by the string [sep] into the formatter
    [ppf]. *)
val pp_int_array : string -> Format.formatter -> int array -> unit

(** [pp_float_list pp sep ppf l] prints the elements of the list of
    floats [l] separated by the string [sep] into the formatter
    [ppf]. *)
val pp_float_list : string -> Format.formatter -> float list -> unit

(** [pp_float_array pp sep ppf a] prints the elements of the array of
    floats [a] separated by the string [sep] into the formatter
    [ppf]. *)
val pp_float_array : string -> Format.formatter -> float array -> unit

val pp_option : 
  (Format.formatter -> 'a -> unit) -> string -> Format.formatter -> 'a option -> unit

val pp_string_option : string -> Format.formatter -> string option -> unit

val string_of_string_option : string -> string option -> string


(*---------strings-----------*)

(*string filled with n spaces *)
val space_str        :  int -> string 
val space_str_sep    : char -> int -> string 


val to_stream_space : 'a string_stream -> int -> unit
val to_stream_space_sep : char -> 'a string_stream -> int -> unit


(* add spaces to str to reach distance *)
(*if the distance is less than or equal to str then just one space is added*)
(*(used for formatting output) *)
val space_padding_str :  int -> string -> string

val space_padding_str_sep : char -> int -> string -> string

(** These functions are as above, but will write to stream instead of returning a string. *)

val space_padding_stream : int -> 'a string_stream -> string -> unit

val space_padding_stream_sep : char -> int -> 'a string_stream -> string -> unit


(** removes $ from a string; used for sanitising type names when concatinating as suffix to symbols *)
val remove_dollars_str : string -> string


(*--------Named modules----------------------*)

module type NameM = 
  sig
    val name : string
  end

(*----------------reals-----------------*)

(* decimal reals *)
type real = 
    {
     
     real_string : string;

     (* real_fraction Ee exp*)
     real_fraction    : float;
     real_exponent    : int; 
   }

val real_to_string : real -> string


(*--------------Global Time Limits-------------------*)
(* time limit in seconds *)
(* time_limit can be reassigned, there are number of points where it is checked*)


exception Timeout

(*---------Discount time limits can be checked in all related modules-------*)
(* After Timeout using discount can be incomplete (bit still sound) *)

val assign_discount_time_limit :float -> unit 
val assign_discount_start_time : unit -> unit
val unassign_discount_time_limit : unit -> unit

val check_disc_time_limit : unit -> unit

(* removes duplicates and also order list *)

val list_remove_int_duplicates : int list -> int list 




(*------Integer Map/Htbl/Set modules--------------*)

module IntKey :
  sig 
    type t = int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

module IntMap : Map.S with type key = int
module IntHtbl : Hashtbl.S with type key = int
module IntSet  : Set.S with type elt = int

(*----- Pairs of int------ *)

module PairIntKey :
  sig
    type t = int * int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

module PairIntMap : Map.S with type key = (int * int)
module PairIntHtbl : Hashtbl.S with type key = (int * int)
module PairIntSet  : Set.S with type elt = (int * int)


(*-------------Str--------*)

module StrMap : Map.S with type key = string 
module StrSet : Set.S with type elt = string



(** Indexed maps collection of elementes to 0 <= ind_el < size
    indexing is done sequentionally, repeated elements are ignored
    To be consistent with indexes across implementation removal of elements is not included    
 *)
            
module type IndexedSig = sig
  type el
  type t
  val create : unit -> t
  val add : el -> t -> t
  val find_ind : el -> t -> int
  val find_el : int -> t -> el
  val size : t -> int
end
      
module Indexed (Ord : Ordered) : IndexedSig with type el = Ord.t
      
   
(** round robin *)


(* round_robin_spec [(n,f,a),..,] *)

type 'a round_robin_spec = (int * ('a -> unit) * 'a)  list 

(* n iterations over spec *)
val round_robin_n : int -> 'a round_robin_spec  -> unit 

(* indefinite iterations over spec *)
val round_robin_inf : 'a round_robin_spec  -> unit





(** Bringing some common print functions into global scope *)

val printf  : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a

(** Variants which place a "% " at the start, if [tptp_safe_out] is set. *)

val printf_tptp  : ('a, out_channel, unit) format -> 'a
val eprintf_tptp : ('a, out_channel, unit) format -> 'a
val fprintf_tptp : out_channel -> ('a, out_channel, unit) format -> 'a
val sprintf_tptp : (string, unit, string) format -> string



(** [x |> tap f |> y] is [x |> y] with the added side-effect of [f x] *)
val tap : ('a -> unit) -> 'a -> 'a

(* (** ($) as an alias for (@@) *)
val ($) : ('a -> 'b) -> 'a -> 'b *)

(** E.g. [List.filter (neg p)] is easier to write than 
    [List.filter (fun x -> not (p x))]. *)
val neg : ('a -> bool) -> 'a -> bool
val neg2 : ('a -> 'b -> bool) -> 'a -> 'b -> bool
val neg3 : ('a -> 'b -> 'c -> bool) -> 'a -> 'b -> 'c -> bool
