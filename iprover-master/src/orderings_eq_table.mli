open Lib


(** Dummy types, for the node sorts *)
type ord_n
type bin_n
type term_n
type err_n


(** Dummy type, for a tree of sort ['hd] with children of sort ['tl] *)
type ('hd, 'tl) l = L

(** Type of a decision diagram node *)
(** Explicit GADT typing lets the compiler check that the tree is well-typed *)
type _ odd = 
  | BinNode : {neg: 'a odd; pos: 'a odd} -> (bin_n,'a) l odd
  | OrdNode : {eq: 'a odd; gt: 'a odd; lt: 'a odd; inc: 'a odd} -> (ord_n,'a) l odd
  | SkipNode : 'a odd -> ('b,'a) l odd
  | TermNode : PartialOrd.t -> term_n odd
  | ErrNode : 'a odd



(** Well-typed decision diagram *)
type table = (
  bin_n, (
  bin_n, (

  ord_n, (
  ord_n, (

  ord_n, (
  ord_n, (
  ord_n, (
  ord_n, (
    
  term_n )
) l ) l ) l ) l ) l ) l ) l ) l odd

val odd : table



(** Query [BinNode]. [false] takes [neg] branch, [true] takes [pos] branch. *)
val query_bin      : bool -> (bin_n, 'a) l odd -> 'a odd
(** As [query_bin] BUT if node is SkipNode then the lazy value isn't evaluated at all. *)
val query_bin_lazy : (unit -> bool) -> (bin_n, 'a) l odd -> 'a odd

(** Query [OrdNode]. [EQ], [GT], [LT], [INC] take corresponding branch. *)
val query_ord      : PartialOrd.t -> (ord_n, 'a) l odd -> 'a odd
(** As [query_ord] BUT if node is SkipNode then the lazy value isn't evaluated at all. *)
val query_ord_lazy : (unit -> PartialOrd.t) -> (ord_n, 'a) l odd -> 'a odd

(** Get value of [TermNode]. *)
val query_term : term_n odd -> PartialOrd.t

