open Lib



(* -- Datatype -- *)

(* Dummy types, for the node sorts *)
type ord_n
type bin_n
type term_n
type err_n

(* Dummy type, for a tree of sort ['hd] with children of sort ['tl] *)
type ('hd, 'tl) l = L

(* Type of a decision diagram node *)
(* Explicit GADT typing lets the compiler check that the tree is well-typed *)
type _ odd = 
  | BinNode : {neg: 'a odd; pos: 'a odd} -> (bin_n,'a) l odd
  | OrdNode : {eq: 'a odd; gt: 'a odd; lt: 'a odd; inc: 'a odd} -> (ord_n,'a) l odd
  | SkipNode : 'a odd -> ('b,'a) l odd
  | TermNode : PartialOrd.t -> term_n odd
  | ErrNode : 'a odd


(* -- Functions -- *)

(* All of these functions are *explicitly well-typed*, and the compiler checks 
   it for exhaustivity. For instance in [query_bin] if we try to add a match 
   branch for [OrdNode] it complains that it does not match values of type
   [(bin_n, 'a) l odd]. Similarly, if we remove the e.g. branch with [SkipNode], 
   it complains that the match is no longer exhaustive. *)

let query_bin (x: bool) (odd: (bin_n, 'a) l odd) : 'a odd = 
  match odd with
  | SkipNode y -> y
  | BinNode {neg; pos} ->
    begin match x with
    | false -> neg
    | true -> pos
    end
  | ErrNode -> failwith "nontransitive"

let query_bin_lazy (x: unit -> bool) (odd: (bin_n, 'a) l odd) : 'a odd = 
  match odd with
  | SkipNode y -> 
    Statistics.(bump_int_stat comparisons_avoided);
    y
  | BinNode {neg; pos} ->
    begin match x() with
    | false -> neg
    | true -> pos
    end
  | ErrNode -> failwith "nontransitive"


let query_ord (x: PartialOrd.t) (odd: (ord_n, 'a) l odd) : 'a odd = 
  match odd with
  | SkipNode y -> y
  | OrdNode {eq; gt; lt; inc} ->
    let open PartialOrd in
    begin match x with
    | EQ -> eq
    | GT -> gt
    | LT -> lt
    | INC -> inc
    end
  | ErrNode -> failwith "nontransitive"

let query_ord_lazy (x: unit -> PartialOrd.t) (odd: (ord_n, 'a) l odd) : 'a odd = 
  match odd with
  | SkipNode y -> 
    Statistics.(bump_int_stat comparisons_avoided);
    y
  | OrdNode {eq; gt; lt; inc} ->
    let open PartialOrd in
    begin match x() with
    | EQ -> eq
    | GT -> gt
    | LT -> lt
    | INC -> inc
    end
  | ErrNode -> failwith "nontransitive"

let query_term (odd: term_n odd) : PartialOrd.t =
  match odd with
  | TermNode x -> x
  | ErrNode -> failwith "nontransitive"



(* -- Table -- *)

(* Generated with code in [generate_orderings_eq_table.ml]. *)

(* Thanks to the explicit typing introduced above, we can now force the tree 
   to have type [table], defined below as a tree with two "Bin" decision levels, 
   followed, by six "Ord" decision levels, followed by a terminal node with a 
   result. If it is not (e.g. if we try deleting one of the levels in the 
   definition of [type table], or else making it ill-typed in the definition of 
   [let odd], then the code will not compile. *)

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

let odd : table =
  BinNode {
    neg=( BinNode {
      neg=( OrdNode {
        eq=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        gt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( OrdNode {
                eq=( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( SkipNode 
                  ( TermNode INC ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        lt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} )} ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} )} )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        inc=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( OrdNode {
                eq=( ErrNode ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                lt=( ErrNode ); 
                inc=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} )} ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode INC ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} )} )} )} ); 
      pos=( OrdNode {
        eq=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        gt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( SkipNode 
                  ( TermNode GT ) ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( OrdNode {
                eq=( SkipNode 
                  ( TermNode INC ) ); 
                gt=( SkipNode 
                  ( TermNode INC ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        lt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( SkipNode 
            ( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ) ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        inc=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( SkipNode 
                  ( TermNode GT ) ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode INC ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                lt=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} )} )} )} )} ); 
    pos=( BinNode {
      neg=( OrdNode {
        eq=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        gt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( SkipNode 
                  ( TermNode LT ) ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( OrdNode {
                eq=( SkipNode 
                  ( TermNode LT ) ); 
                gt=( SkipNode 
                  ( TermNode INC ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        lt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( SkipNode 
            ( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ) ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        inc=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( SkipNode 
                  ( TermNode INC ) ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode INC ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} )} )} )} ); 
      pos=( OrdNode {
        eq=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        gt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          gt=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) )} ); 
          lt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ) ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( OrdNode {
                eq=( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( SkipNode 
                  ( TermNode INC ) ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        lt=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} )} ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} )} )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} )} ); 
        inc=( OrdNode {
          eq=( OrdNode {
            eq=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ) ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          gt=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( ErrNode ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( SkipNode 
              ( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ) ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( OrdNode {
                eq=( ErrNode ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                lt=( ErrNode ); 
                inc=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} )} ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          lt=( OrdNode {
            eq=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( SkipNode 
                  ( TermNode INC ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( ErrNode ); 
              gt=( ErrNode ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} )} ); 
          inc=( OrdNode {
            eq=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode EQ ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) )} ); 
            gt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode GT ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              inc=( OrdNode {
                eq=( ErrNode ); 
                gt=( SkipNode 
                  ( TermNode GT ) ); 
                lt=( ErrNode ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} ); 
            lt=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              gt=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              lt=( SkipNode 
                ( SkipNode 
                  ( TermNode LT ) ) ); 
              inc=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( ErrNode ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ) )} ); 
            inc=( OrdNode {
              eq=( SkipNode 
                ( SkipNode 
                  ( TermNode INC ) ) ); 
              gt=( SkipNode 
                ( OrdNode {
                  eq=( ErrNode ); 
                  gt=( TermNode GT ); 
                  lt=( ErrNode ); 
                  inc=( TermNode INC )} ) ); 
              lt=( OrdNode {
                eq=( ErrNode ); 
                gt=( ErrNode ); 
                lt=( SkipNode 
                  ( TermNode LT ) ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} ); 
              inc=( OrdNode {
                eq=( OrdNode {
                  eq=( TermNode INC ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                gt=( OrdNode {
                  eq=( TermNode GT ); 
                  gt=( TermNode GT ); 
                  lt=( TermNode INC ); 
                  inc=( TermNode INC )} ); 
                lt=( OrdNode {
                  eq=( TermNode LT ); 
                  gt=( TermNode INC ); 
                  lt=( TermNode LT ); 
                  inc=( TermNode INC )} ); 
                inc=( SkipNode 
                  ( TermNode INC ) )} )} )} )} )} )} )}

(* -- End table -- *)
