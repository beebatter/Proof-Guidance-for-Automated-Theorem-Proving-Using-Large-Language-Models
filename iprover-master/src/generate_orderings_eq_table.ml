open Lib

(** Transforms a table of results into a decision diagram. *)




<<<<<<< HEAD
  let eq  = PartialOrd.to_int EQ  in
  let gt  = PartialOrd.to_int GT  in
  let lt  = PartialOrd.to_int LT  in
  let inc = PartialOrd.to_int INC in
=======
>>>>>>> andrepd/smt-interface

(* ----- *)
(* Table *)
(* ----- *)

let table = 
  let eq  = PartialOrd.to_int EQ  in
  let gt  = PartialOrd.to_int GT  in
  let lt  = PartialOrd.to_int LT  in
  let inc = PartialOrd.to_int INC in

  (** Generated from multiset definition with auxiliary script `equality_orderings.py`. *)
  let tbl : (int array * int) list = [
    (* Let the two literals be
         s ≐ t
         u ≐ v
       where the sign of s≐t is S1 and of u≐v is S2. *)

    (* S1  S2  st   uv   su   tv   sv   tu |  res*)
    ([|1 ; 1 ; gt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; gt ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; eq ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; inc; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; gt ; gt ; eq |], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; gt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; lt ; gt ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; eq ; gt ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; inc; gt ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; lt ; eq ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; lt ; inc; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; eq ; gt ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; gt ; eq ; lt ; gt ; lt |], lt );
    ([|1 ; 1 ; gt ; gt ; eq ; eq ; gt ; lt |], eq );
    ([|1 ; 1 ; gt ; gt ; eq ; inc; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; gt ; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; lt ; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; eq ; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; inc; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; gt ; gt ; inc; inc; gt ; inc|], inc);
    ([|1 ; 1 ; gt ; gt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; inc; inc; lt |], inc);
    ([|1 ; 1 ; gt ; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; eq ; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; inc; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; gt ; eq |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; lt ; eq |], lt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; eq ; gt |], gt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; eq ; lt |], lt );
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; eq ; eq |], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; eq ; inc|], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; inc; gt |], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; inc; lt |], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; inc; eq |], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; lt ; inc; inc|], inc);
    ([|1 ; 1 ; gt ; lt ; gt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; gt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; lt ; eq ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; lt ; inc; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; gt ; lt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; gt ; lt ; inc; lt ; inc; inc|], inc);
    ([|1 ; 1 ; gt ; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; gt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; eq ; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; eq ; gt ; eq ; gt ; eq |], gt );
    ([|1 ; 1 ; gt ; eq ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; eq ; eq ; lt ; eq ; lt |], lt );
    ([|1 ; 1 ; gt ; eq ; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; gt ; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; gt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; inc; gt ; inc; gt ; gt |], gt );
    ([|1 ; 1 ; gt ; inc; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; inc; gt ; inc; gt ; lt |], gt );
    ([|1 ; 1 ; gt ; inc; gt ; inc; gt ; eq |], gt );
    ([|1 ; 1 ; gt ; inc; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; inc; gt ; lt ; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; inc; gt ; eq ; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; inc; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; gt ; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; gt ; inc; gt ; lt ; inc; lt |], inc);
    ([|1 ; 1 ; gt ; inc; gt ; inc; inc; lt |], inc);
    ([|1 ; 1 ; gt ; inc; gt ; inc; inc; eq |], inc);
    ([|1 ; 1 ; gt ; inc; gt ; lt ; inc; inc|], inc);
    ([|1 ; 1 ; gt ; inc; gt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; gt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; inc; lt ; lt ; inc; lt |], lt );
    ([|1 ; 1 ; gt ; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; gt ; inc; eq ; lt ; inc; lt |], lt );
    ([|1 ; 1 ; gt ; inc; eq ; inc; inc; lt |], inc);
    ([|1 ; 1 ; gt ; inc; inc; lt ; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; inc; inc; inc; gt ; lt |], inc);
    ([|1 ; 1 ; gt ; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; gt ; inc; inc; lt ; gt ; inc|], inc);
    ([|1 ; 1 ; gt ; inc; inc; eq ; gt ; inc|], inc);
    ([|1 ; 1 ; gt ; inc; inc; inc; gt ; inc|], inc);
    ([|1 ; 1 ; gt ; inc; inc; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; gt ; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; gt ; inc; inc; lt ; eq ; lt |], lt );
    ([|1 ; 1 ; gt ; inc; inc; lt ; eq ; inc|], inc);
    ([|1 ; 1 ; gt ; inc; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; gt ; inc; inc; inc; inc; lt |], inc);
    ([|1 ; 1 ; gt ; inc; inc; lt ; inc; inc|], inc);
    ([|1 ; 1 ; gt ; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; gt ; eq |], gt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; eq ; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; inc; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; lt ; eq |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; eq ; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; eq ; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; eq ; eq |], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; eq ; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; inc; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; inc; eq |], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; gt ; inc; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; lt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; eq ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; inc; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; gt ; inc; gt ; inc; inc|], inc);
    ([|1 ; 1 ; lt ; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; lt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; gt ; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; lt ; lt ; gt ; eq ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; gt ; inc; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; gt ; gt ; eq ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; gt ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; lt ; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; lt ; lt ; lt ; eq ; lt ; gt |], lt );
    ([|1 ; 1 ; lt ; lt ; lt ; inc; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; lt ; lt ; lt ; lt ; eq |], lt );
    ([|1 ; 1 ; lt ; lt ; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; lt ; lt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; lt ; eq ; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; eq ; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; lt ; lt ; eq ; eq ; lt ; gt |], eq );
    ([|1 ; 1 ; lt ; lt ; eq ; inc; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; inc; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; inc; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; lt ; lt ; inc; eq ; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; inc; inc; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; lt ; lt ; inc; inc; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; lt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; lt ; inc; inc; inc; gt |], inc);
    ([|1 ; 1 ; lt ; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; lt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; eq ; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; eq ; lt ; eq ; lt ; eq |], lt );
    ([|1 ; 1 ; lt ; eq ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; eq ; eq ; gt ; eq ; gt |], gt );
    ([|1 ; 1 ; lt ; eq ; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; lt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; inc; gt ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; lt ; inc; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; inc; lt ; inc; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; inc; lt ; inc; lt ; lt |], lt );
    ([|1 ; 1 ; lt ; inc; lt ; inc; lt ; eq |], lt );
    ([|1 ; 1 ; lt ; inc; lt ; gt ; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; lt ; inc; lt ; eq ; lt ; inc|], lt );
    ([|1 ; 1 ; lt ; inc; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; lt ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; inc; lt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; lt ; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; lt ; inc; lt ; inc; inc; eq |], inc);
    ([|1 ; 1 ; lt ; inc; lt ; gt ; inc; inc|], inc);
    ([|1 ; 1 ; lt ; inc; lt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; lt ; inc; eq ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; inc; eq ; inc; inc; gt |], inc);
    ([|1 ; 1 ; lt ; inc; inc; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; lt ; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; inc; gt ; lt ; gt |], gt );
    ([|1 ; 1 ; lt ; inc; inc; inc; lt ; gt |], inc);
    ([|1 ; 1 ; lt ; inc; inc; gt ; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; lt ; inc; inc; eq ; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; inc; inc; lt ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; inc; gt ; eq ; gt |], gt );
    ([|1 ; 1 ; lt ; inc; inc; gt ; eq ; inc|], inc);
    ([|1 ; 1 ; lt ; inc; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; lt ; inc; inc; inc; inc; gt |], inc);
    ([|1 ; 1 ; lt ; inc; inc; gt ; inc; inc|], inc);
    ([|1 ; 1 ; lt ; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; eq ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; eq ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 1 ; eq ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; eq ; gt ; lt ; eq ; eq ; lt |], lt );
    ([|1 ; 1 ; eq ; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; eq ; gt ; eq ; gt ; gt ; eq |], gt );
    ([|1 ; 1 ; eq ; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; eq ; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; eq ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; eq ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; eq ; lt ; gt ; eq ; eq ; gt |], gt );
    ([|1 ; 1 ; eq ; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; eq ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; eq ; lt ; eq ; lt ; lt ; eq |], lt );
    ([|1 ; 1 ; eq ; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; eq ; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; eq ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; eq ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; eq ; eq ; eq ; eq ; eq ; eq |], eq );
    ([|1 ; 1 ; eq ; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; eq ; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; eq ; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; eq ; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; eq ; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; eq ; inc; eq ; inc; inc; eq |], inc);
    ([|1 ; 1 ; eq ; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; eq ; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; eq ; inc; inc; eq ; eq ; inc|], inc);
    ([|1 ; 1 ; eq ; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; inc; gt ; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 1 ; inc; gt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; inc; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; inc; gt ; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; lt ; inc; gt ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; inc; lt ; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; lt ; inc; eq ; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; inc; eq ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; lt ; gt ; inc; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; lt ; inc; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; eq ; inc; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; inc; gt ; lt ; gt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; gt ; lt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; gt ; eq ; gt ; gt ; inc|], gt );
    ([|1 ; 1 ; inc; gt ; eq ; inc; gt ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; inc; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; inc; gt ; inc; gt ; gt ; lt |], inc);
    ([|1 ; 1 ; inc; gt ; inc; inc; gt ; lt |], inc);
    ([|1 ; 1 ; inc; gt ; inc; gt ; gt ; eq |], gt );
    ([|1 ; 1 ; inc; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; inc; inc; gt ; inc|], inc);
    ([|1 ; 1 ; inc; gt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; inc; gt ; inc; gt ; inc; lt |], inc);
    ([|1 ; 1 ; inc; gt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; inc; gt ; inc; eq ; inc; lt |], inc);
    ([|1 ; 1 ; inc; gt ; inc; inc; inc; lt |], inc);
    ([|1 ; 1 ; inc; gt ; inc; gt ; inc; eq |], inc);
    ([|1 ; 1 ; inc; gt ; inc; gt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; inc; lt ; gt ; inc; gt ; gt |], gt );
    ([|1 ; 1 ; inc; lt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; inc; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; inc; lt ; gt ; inc; lt ; gt |], inc);
    ([|1 ; 1 ; inc; lt ; gt ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; inc; lt ; gt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; lt ; gt ; inc; eq ; gt |], gt );
    ([|1 ; 1 ; inc; lt ; gt ; inc; eq ; inc|], inc);
    ([|1 ; 1 ; inc; lt ; gt ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; inc; lt ; gt ; lt ; inc; gt |], inc);
    ([|1 ; 1 ; inc; lt ; gt ; eq ; inc; gt |], gt );
    ([|1 ; 1 ; inc; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; inc; lt ; gt ; lt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; lt ; gt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; inc; lt ; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; inc; lt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; lt ; eq ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; inc; lt ; eq ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; lt ; inc; lt ; lt ; gt |], lt );
    ([|1 ; 1 ; inc; lt ; inc; inc; lt ; gt |], inc);
    ([|1 ; 1 ; inc; lt ; inc; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; inc; lt ; inc; lt ; lt ; eq |], lt );
    ([|1 ; 1 ; inc; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; inc; lt ; inc; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; lt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; inc; lt ; inc; lt ; inc; gt |], inc);
    ([|1 ; 1 ; inc; lt ; inc; eq ; inc; gt |], inc);
    ([|1 ; 1 ; inc; lt ; inc; inc; inc; gt |], inc);
    ([|1 ; 1 ; inc; lt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; inc; lt ; inc; lt ; inc; eq |], inc);
    ([|1 ; 1 ; inc; lt ; inc; lt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; inc; eq ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; inc; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; inc; eq ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; eq ; eq ; inc; eq ; inc|], inc);
    ([|1 ; 1 ; inc; eq ; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; inc; eq ; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; inc; eq ; inc; eq ; inc; eq |], inc);
    ([|1 ; 1 ; inc; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; inc; inc; gt ; inc; gt ; gt |], gt );
    ([|1 ; 1 ; inc; inc; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 1 ; inc; inc; gt ; inc; gt ; inc|], gt );
    ([|1 ; 1 ; inc; inc; gt ; gt ; inc; gt |], gt );
    ([|1 ; 1 ; inc; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 1 ; inc; inc; gt ; gt ; inc; inc|], gt );
    ([|1 ; 1 ; inc; inc; gt ; lt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; gt ; eq ; inc; inc|], gt );
    ([|1 ; 1 ; inc; inc; gt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; inc; inc; lt ; inc; lt ; lt |], lt );
    ([|1 ; 1 ; inc; inc; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; inc; inc; lt ; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; inc; lt ; lt ; inc; lt |], lt );
    ([|1 ; 1 ; inc; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 1 ; inc; inc; lt ; gt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; lt ; lt ; inc; inc|], lt );
    ([|1 ; 1 ; inc; inc; lt ; eq ; inc; inc|], lt );
    ([|1 ; 1 ; inc; inc; lt ; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; eq ; gt ; inc; inc|], gt );
    ([|1 ; 1 ; inc; inc; eq ; lt ; inc; inc|], lt );
    ([|1 ; 1 ; inc; inc; eq ; eq ; inc; inc|], eq );
    ([|1 ; 1 ; inc; inc; eq ; inc; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; gt ; gt ; gt |], gt );
    ([|1 ; 1 ; inc; inc; inc; inc; gt ; gt |], gt );
    ([|1 ; 1 ; inc; inc; inc; inc; gt ; lt |], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; gt ; eq |], gt );
    ([|1 ; 1 ; inc; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; gt ; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; lt ; gt |], inc);
    ([|1 ; 1 ; inc; inc; inc; lt ; lt ; lt |], lt );
    ([|1 ; 1 ; inc; inc; inc; inc; lt ; lt |], lt );
    ([|1 ; 1 ; inc; inc; inc; inc; lt ; eq |], lt );
    ([|1 ; 1 ; inc; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 1 ; inc; inc; inc; inc; lt ; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; eq ; gt |], gt );
    ([|1 ; 1 ; inc; inc; inc; inc; eq ; lt |], lt );
    ([|1 ; 1 ; inc; inc; inc; inc; eq ; eq |], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; eq ; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; gt ; inc; gt |], gt );
    ([|1 ; 1 ; inc; inc; inc; inc; inc; gt |], inc);
    ([|1 ; 1 ; inc; inc; inc; lt ; inc; lt |], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; inc; lt |], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; inc; eq |], inc);
    ([|1 ; 1 ; inc; inc; inc; gt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; lt ; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; eq ; inc; inc|], inc);
    ([|1 ; 1 ; inc; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; gt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; gt ; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; eq ; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; inc; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; gt ; gt ; eq |], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; gt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; lt ; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; eq ; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; inc; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; lt ; eq ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; lt ; inc; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; eq ; gt ; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; eq ; lt ; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; eq ; eq ; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; eq ; inc; gt ; lt |], lt );
    ([|1 ; 0 ; gt ; gt ; inc; gt ; gt ; lt |], inc);
    ([|1 ; 0 ; gt ; gt ; inc; lt ; gt ; lt |], inc);
    ([|1 ; 0 ; gt ; gt ; inc; eq ; gt ; lt |], inc);
    ([|1 ; 0 ; gt ; gt ; inc; inc; gt ; lt |], inc);
    ([|1 ; 0 ; gt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; gt ; gt ; inc; inc; gt ; inc|], inc);
    ([|1 ; 0 ; gt ; gt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; gt ; gt ; inc; inc; inc; lt |], inc);
    ([|1 ; 0 ; gt ; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; gt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; eq ; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; inc; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; gt ; eq |], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; lt ; eq |], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; eq ; gt |], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; eq ; lt |], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; eq ; eq |], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; eq ; inc|], lt );
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; inc; gt |], inc);
    ([|1 ; 0 ; gt ; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; inc; lt |], inc);
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; inc; eq |], inc);
    ([|1 ; 0 ; gt ; lt ; gt ; lt ; inc; inc|], inc);
    ([|1 ; 0 ; gt ; lt ; gt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; gt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; lt ; eq ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; lt ; inc; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; gt ; lt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; gt ; lt ; inc; lt ; inc; inc|], inc);
    ([|1 ; 0 ; gt ; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; gt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; eq ; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; eq ; gt ; eq ; gt ; eq |], gt );
    ([|1 ; 0 ; gt ; eq ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; eq ; eq ; lt ; eq ; lt |], lt );
    ([|1 ; 0 ; gt ; eq ; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; gt ; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; gt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; inc; gt ; inc; gt ; gt |], gt );
    ([|1 ; 0 ; gt ; inc; gt ; lt ; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; inc; gt ; inc; gt ; lt |], gt );
    ([|1 ; 0 ; gt ; inc; gt ; inc; gt ; eq |], gt );
    ([|1 ; 0 ; gt ; inc; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; inc; gt ; lt ; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; inc; gt ; eq ; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; inc; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; gt ; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; gt ; inc; gt ; lt ; inc; lt |], inc);
    ([|1 ; 0 ; gt ; inc; gt ; inc; inc; lt |], inc);
    ([|1 ; 0 ; gt ; inc; gt ; inc; inc; eq |], inc);
    ([|1 ; 0 ; gt ; inc; gt ; lt ; inc; inc|], inc);
    ([|1 ; 0 ; gt ; inc; gt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; gt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; inc; lt ; lt ; inc; lt |], lt );
    ([|1 ; 0 ; gt ; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; gt ; inc; eq ; lt ; inc; lt |], lt );
    ([|1 ; 0 ; gt ; inc; eq ; inc; inc; lt |], lt );
    ([|1 ; 0 ; gt ; inc; inc; lt ; gt ; lt |], inc);
    ([|1 ; 0 ; gt ; inc; inc; inc; gt ; lt |], inc);
    ([|1 ; 0 ; gt ; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; gt ; inc; inc; lt ; gt ; inc|], inc);
    ([|1 ; 0 ; gt ; inc; inc; eq ; gt ; inc|], inc);
    ([|1 ; 0 ; gt ; inc; inc; inc; gt ; inc|], inc);
    ([|1 ; 0 ; gt ; inc; inc; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; gt ; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; gt ; inc; inc; lt ; eq ; lt |], lt );
    ([|1 ; 0 ; gt ; inc; inc; lt ; eq ; inc|], lt );
    ([|1 ; 0 ; gt ; inc; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; gt ; inc; inc; inc; inc; lt |], inc);
    ([|1 ; 0 ; gt ; inc; inc; lt ; inc; inc|], inc);
    ([|1 ; 0 ; gt ; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; gt ; eq |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; eq ; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; inc; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; lt ; eq |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; eq ; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; eq ; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; eq ; eq |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; eq ; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; inc; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; inc; eq |], lt );
    ([|1 ; 0 ; lt ; gt ; lt ; gt ; inc; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; lt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; eq ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; inc; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; gt ; inc; gt ; inc; inc|], inc);
    ([|1 ; 0 ; lt ; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; lt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; gt ; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; gt ; eq ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; gt ; inc; lt ; gt |], inc);
    ([|1 ; 0 ; lt ; lt ; gt ; gt ; eq ; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; gt ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; lt ; lt ; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; lt ; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; lt ; eq ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; lt ; inc; lt ; gt |], inc);
    ([|1 ; 0 ; lt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; lt ; lt ; lt ; lt ; eq |], lt );
    ([|1 ; 0 ; lt ; lt ; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; lt ; lt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; lt ; eq ; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; eq ; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; eq ; eq ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; eq ; inc; lt ; gt |], inc);
    ([|1 ; 0 ; lt ; lt ; inc; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; inc; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; inc; eq ; lt ; gt |], lt );
    ([|1 ; 0 ; lt ; lt ; inc; inc; lt ; gt |], inc);
    ([|1 ; 0 ; lt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; lt ; lt ; inc; inc; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; lt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; lt ; inc; inc; inc; gt |], inc);
    ([|1 ; 0 ; lt ; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; lt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; eq ; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; eq ; lt ; eq ; lt ; eq |], lt );
    ([|1 ; 0 ; lt ; eq ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; eq ; eq ; gt ; eq ; gt |], gt );
    ([|1 ; 0 ; lt ; eq ; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; lt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; inc; gt ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; lt ; inc; lt ; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; inc; lt ; inc; lt ; gt |], inc);
    ([|1 ; 0 ; lt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; inc; lt ; inc; lt ; lt |], lt );
    ([|1 ; 0 ; lt ; inc; lt ; inc; lt ; eq |], lt );
    ([|1 ; 0 ; lt ; inc; lt ; gt ; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; inc; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; lt ; inc; lt ; eq ; lt ; inc|], lt );
    ([|1 ; 0 ; lt ; inc; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; inc; lt ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; inc; lt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; lt ; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; lt ; inc; lt ; inc; inc; eq |], lt );
    ([|1 ; 0 ; lt ; inc; lt ; gt ; inc; inc|], inc);
    ([|1 ; 0 ; lt ; inc; lt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; lt ; inc; eq ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; inc; eq ; inc; inc; gt |], inc);
    ([|1 ; 0 ; lt ; inc; inc; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; lt ; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; lt ; inc; inc; gt ; lt ; gt |], gt );
    ([|1 ; 0 ; lt ; inc; inc; inc; lt ; gt |], inc);
    ([|1 ; 0 ; lt ; inc; inc; gt ; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; lt ; inc; inc; eq ; lt ; inc|], lt );
    ([|1 ; 0 ; lt ; inc; inc; inc; lt ; inc|], inc);
    ([|1 ; 0 ; lt ; inc; inc; gt ; eq ; gt |], gt );
    ([|1 ; 0 ; lt ; inc; inc; gt ; eq ; inc|], inc);
    ([|1 ; 0 ; lt ; inc; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; lt ; inc; inc; inc; inc; gt |], inc);
    ([|1 ; 0 ; lt ; inc; inc; gt ; inc; inc|], inc);
    ([|1 ; 0 ; lt ; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; eq ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; eq ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 0 ; eq ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; eq ; gt ; lt ; eq ; eq ; lt |], lt );
    ([|1 ; 0 ; eq ; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; eq ; gt ; eq ; gt ; gt ; eq |], lt );
    ([|1 ; 0 ; eq ; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; eq ; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; eq ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; eq ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; eq ; lt ; gt ; eq ; eq ; gt |], lt );
    ([|1 ; 0 ; eq ; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; eq ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; eq ; lt ; eq ; lt ; lt ; eq |], lt );
    ([|1 ; 0 ; eq ; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; eq ; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; eq ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; eq ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; eq ; eq ; eq ; eq ; eq ; eq |], lt );
    ([|1 ; 0 ; eq ; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; eq ; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; eq ; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; eq ; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; eq ; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; eq ; inc; eq ; inc; inc; eq |], lt );
    ([|1 ; 0 ; eq ; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; eq ; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; eq ; inc; inc; eq ; eq ; inc|], lt );
    ([|1 ; 0 ; eq ; inc; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; gt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; inc; gt ; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 0 ; inc; gt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; inc; gt ; lt ; gt ; gt ; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; inc; gt ; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; lt ; inc; gt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; inc; lt ; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; lt ; inc; eq ; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; inc; eq ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; lt ; gt ; inc; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; lt ; inc; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; eq ; inc; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; inc; gt ; lt ; gt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; gt ; lt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; gt ; eq ; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; eq ; inc; gt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; inc; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; inc; gt ; inc; gt ; gt ; lt |], inc);
    ([|1 ; 0 ; inc; gt ; inc; inc; gt ; lt |], inc);
    ([|1 ; 0 ; inc; gt ; inc; gt ; gt ; eq |], inc);
    ([|1 ; 0 ; inc; gt ; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; inc; inc; gt ; inc|], inc);
    ([|1 ; 0 ; inc; gt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; inc; gt ; inc; gt ; inc; lt |], inc);
    ([|1 ; 0 ; inc; gt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; inc; gt ; inc; eq ; inc; lt |], inc);
    ([|1 ; 0 ; inc; gt ; inc; inc; inc; lt |], inc);
    ([|1 ; 0 ; inc; gt ; inc; gt ; inc; eq |], inc);
    ([|1 ; 0 ; inc; gt ; inc; gt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; gt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; lt ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; inc; lt ; gt ; inc; gt ; gt |], gt );
    ([|1 ; 0 ; inc; lt ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; inc; lt ; gt ; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; inc; lt ; gt ; inc; lt ; gt |], inc);
    ([|1 ; 0 ; inc; lt ; gt ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; inc; lt ; gt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; lt ; gt ; inc; eq ; gt |], inc);
    ([|1 ; 0 ; inc; lt ; gt ; inc; eq ; inc|], inc);
    ([|1 ; 0 ; inc; lt ; gt ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; inc; lt ; gt ; lt ; inc; gt |], inc);
    ([|1 ; 0 ; inc; lt ; gt ; eq ; inc; gt |], inc);
    ([|1 ; 0 ; inc; lt ; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; inc; lt ; gt ; lt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; lt ; gt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; lt ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; inc; lt ; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; inc; lt ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; lt ; eq ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; inc; lt ; eq ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; lt ; inc; lt ; lt ; gt |], lt );
    ([|1 ; 0 ; inc; lt ; inc; inc; lt ; gt |], inc);
    ([|1 ; 0 ; inc; lt ; inc; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; inc; lt ; inc; lt ; lt ; eq |], lt );
    ([|1 ; 0 ; inc; lt ; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; inc; lt ; inc; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; lt ; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; inc; lt ; inc; lt ; inc; gt |], inc);
    ([|1 ; 0 ; inc; lt ; inc; eq ; inc; gt |], inc);
    ([|1 ; 0 ; inc; lt ; inc; inc; inc; gt |], inc);
    ([|1 ; 0 ; inc; lt ; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; inc; lt ; inc; lt ; inc; eq |], inc);
    ([|1 ; 0 ; inc; lt ; inc; lt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; lt ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; eq ; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; inc; eq ; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; inc; eq ; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; inc; eq ; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; eq ; eq ; inc; eq ; inc|], inc);
    ([|1 ; 0 ; inc; eq ; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; inc; eq ; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; inc; eq ; inc; eq ; inc; eq |], inc);
    ([|1 ; 0 ; inc; eq ; inc; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; gt ; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; inc; inc; gt ; inc; gt ; gt |], gt );
    ([|1 ; 0 ; inc; inc; gt ; gt ; gt ; inc|], gt );
    ([|1 ; 0 ; inc; inc; gt ; inc; gt ; inc|], gt );
    ([|1 ; 0 ; inc; inc; gt ; gt ; inc; gt |], gt );
    ([|1 ; 0 ; inc; inc; gt ; inc; inc; gt |], inc);
    ([|1 ; 0 ; inc; inc; gt ; gt ; inc; inc|], gt );
    ([|1 ; 0 ; inc; inc; gt ; lt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; gt ; eq ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; gt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; lt ; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; inc; inc; lt ; inc; lt ; lt |], lt );
    ([|1 ; 0 ; inc; inc; lt ; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; inc; inc; lt ; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; inc; lt ; lt ; inc; lt |], lt );
    ([|1 ; 0 ; inc; inc; lt ; inc; inc; lt |], lt );
    ([|1 ; 0 ; inc; inc; lt ; gt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; lt ; lt ; inc; inc|], lt );
    ([|1 ; 0 ; inc; inc; lt ; eq ; inc; inc|], lt );
    ([|1 ; 0 ; inc; inc; lt ; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; eq ; gt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; eq ; lt ; inc; inc|], lt );
    ([|1 ; 0 ; inc; inc; eq ; eq ; inc; inc|], lt );
    ([|1 ; 0 ; inc; inc; eq ; inc; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; gt ; gt ; gt |], gt );
    ([|1 ; 0 ; inc; inc; inc; inc; gt ; gt |], gt );
    ([|1 ; 0 ; inc; inc; inc; inc; gt ; lt |], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; gt ; eq |], inc);
    ([|1 ; 0 ; inc; inc; inc; gt ; gt ; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; gt ; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; lt ; gt |], inc);
    ([|1 ; 0 ; inc; inc; inc; lt ; lt ; lt |], lt );
    ([|1 ; 0 ; inc; inc; inc; inc; lt ; lt |], lt );
    ([|1 ; 0 ; inc; inc; inc; inc; lt ; eq |], lt );
    ([|1 ; 0 ; inc; inc; inc; lt ; lt ; inc|], lt );
    ([|1 ; 0 ; inc; inc; inc; inc; lt ; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; eq ; gt |], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; eq ; lt |], lt );
    ([|1 ; 0 ; inc; inc; inc; inc; eq ; eq |], lt );
    ([|1 ; 0 ; inc; inc; inc; inc; eq ; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; gt ; inc; gt |], gt );
    ([|1 ; 0 ; inc; inc; inc; inc; inc; gt |], inc);
    ([|1 ; 0 ; inc; inc; inc; lt ; inc; lt |], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; inc; lt |], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; inc; eq |], inc);
    ([|1 ; 0 ; inc; inc; inc; gt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; lt ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; eq ; inc; inc|], inc);
    ([|1 ; 0 ; inc; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; gt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; gt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; eq ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; inc; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; gt ; gt ; eq |], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; gt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; lt ; gt ; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; eq ; gt ; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; inc; gt ; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; lt ; eq ; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; lt ; inc; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; gt ; gt ; eq ; gt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; eq ; lt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; eq ; eq ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; eq ; inc; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; gt ; inc; gt ; gt ; lt |], inc);
    ([|0 ; 1 ; gt ; gt ; inc; lt ; gt ; lt |], inc);
    ([|0 ; 1 ; gt ; gt ; inc; eq ; gt ; lt |], inc);
    ([|0 ; 1 ; gt ; gt ; inc; inc; gt ; lt |], inc);
    ([|0 ; 1 ; gt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; gt ; gt ; inc; inc; gt ; inc|], inc);
    ([|0 ; 1 ; gt ; gt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; gt ; inc; inc; inc; lt |], inc);
    ([|0 ; 1 ; gt ; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; gt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; eq ; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; inc; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; gt ; eq |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; lt ; eq |], lt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; eq ; gt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; eq ; lt |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; eq ; eq |], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; eq ; inc|], gt );
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; inc; gt |], inc);
    ([|0 ; 1 ; gt ; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; inc; eq |], inc);
    ([|0 ; 1 ; gt ; lt ; gt ; lt ; inc; inc|], inc);
    ([|0 ; 1 ; gt ; lt ; gt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; gt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; lt ; eq ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; lt ; inc; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; gt ; lt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; lt ; inc; lt ; inc; inc|], inc);
    ([|0 ; 1 ; gt ; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; gt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; eq ; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; eq ; gt ; eq ; gt ; eq |], gt );
    ([|0 ; 1 ; gt ; eq ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; eq ; eq ; lt ; eq ; lt |], gt );
    ([|0 ; 1 ; gt ; eq ; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; gt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; inc; gt ; inc; gt ; gt |], gt );
    ([|0 ; 1 ; gt ; inc; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; inc; gt ; inc; gt ; lt |], gt );
    ([|0 ; 1 ; gt ; inc; gt ; inc; gt ; eq |], gt );
    ([|0 ; 1 ; gt ; inc; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; inc; gt ; lt ; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; inc; gt ; eq ; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; inc; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; gt ; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; gt ; inc; gt ; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; inc; gt ; inc; inc; lt |], inc);
    ([|0 ; 1 ; gt ; inc; gt ; inc; inc; eq |], inc);
    ([|0 ; 1 ; gt ; inc; gt ; lt ; inc; inc|], inc);
    ([|0 ; 1 ; gt ; inc; gt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; gt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; inc; lt ; lt ; inc; lt |], lt );
    ([|0 ; 1 ; gt ; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; gt ; inc; eq ; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; inc; eq ; inc; inc; lt |], inc);
    ([|0 ; 1 ; gt ; inc; inc; lt ; gt ; lt |], inc);
    ([|0 ; 1 ; gt ; inc; inc; inc; gt ; lt |], inc);
    ([|0 ; 1 ; gt ; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; gt ; inc; inc; lt ; gt ; inc|], inc);
    ([|0 ; 1 ; gt ; inc; inc; eq ; gt ; inc|], inc);
    ([|0 ; 1 ; gt ; inc; inc; inc; gt ; inc|], inc);
    ([|0 ; 1 ; gt ; inc; inc; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; gt ; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; gt ; inc; inc; lt ; eq ; lt |], inc);
    ([|0 ; 1 ; gt ; inc; inc; lt ; eq ; inc|], inc);
    ([|0 ; 1 ; gt ; inc; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; gt ; inc; inc; inc; inc; lt |], inc);
    ([|0 ; 1 ; gt ; inc; inc; lt ; inc; inc|], inc);
    ([|0 ; 1 ; gt ; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; gt ; eq |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; eq ; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; inc; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; lt ; eq |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; eq ; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; eq ; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; eq ; eq |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; eq ; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; inc; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; inc; eq |], gt );
    ([|0 ; 1 ; lt ; gt ; lt ; gt ; inc; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; lt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; eq ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; inc; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; gt ; inc; gt ; inc; inc|], inc);
    ([|0 ; 1 ; lt ; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; lt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; gt ; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; lt ; lt ; gt ; eq ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; gt ; inc; lt ; gt |], inc);
    ([|0 ; 1 ; lt ; lt ; gt ; gt ; eq ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; gt ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; lt ; lt ; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; lt ; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; lt ; lt ; lt ; eq ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; lt ; inc; lt ; gt |], inc);
    ([|0 ; 1 ; lt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; lt ; lt ; lt ; lt ; eq |], lt );
    ([|0 ; 1 ; lt ; lt ; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; lt ; lt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; lt ; eq ; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; eq ; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; lt ; lt ; eq ; eq ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; eq ; inc; lt ; gt |], inc);
    ([|0 ; 1 ; lt ; lt ; inc; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; inc; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; lt ; lt ; inc; eq ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; inc; inc; lt ; gt |], inc);
    ([|0 ; 1 ; lt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; lt ; lt ; inc; inc; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; lt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; lt ; inc; inc; inc; gt |], inc);
    ([|0 ; 1 ; lt ; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; lt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; eq ; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; eq ; lt ; eq ; lt ; eq |], gt );
    ([|0 ; 1 ; lt ; eq ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; eq ; eq ; gt ; eq ; gt |], gt );
    ([|0 ; 1 ; lt ; eq ; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; lt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; inc; gt ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; lt ; inc; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; inc; lt ; inc; lt ; gt |], inc);
    ([|0 ; 1 ; lt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; inc; lt ; inc; lt ; lt |], lt );
    ([|0 ; 1 ; lt ; inc; lt ; inc; lt ; eq |], inc);
    ([|0 ; 1 ; lt ; inc; lt ; gt ; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; lt ; inc; lt ; eq ; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; lt ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; inc; lt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; lt ; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; lt ; inc; lt ; inc; inc; eq |], inc);
    ([|0 ; 1 ; lt ; inc; lt ; gt ; inc; inc|], inc);
    ([|0 ; 1 ; lt ; inc; lt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; lt ; inc; eq ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; inc; eq ; inc; inc; gt |], inc);
    ([|0 ; 1 ; lt ; inc; inc; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; lt ; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; inc; gt ; lt ; gt |], gt );
    ([|0 ; 1 ; lt ; inc; inc; inc; lt ; gt |], inc);
    ([|0 ; 1 ; lt ; inc; inc; gt ; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; lt ; inc; inc; eq ; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; inc; inc; lt ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; inc; gt ; eq ; gt |], gt );
    ([|0 ; 1 ; lt ; inc; inc; gt ; eq ; inc|], inc);
    ([|0 ; 1 ; lt ; inc; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; lt ; inc; inc; inc; inc; gt |], inc);
    ([|0 ; 1 ; lt ; inc; inc; gt ; inc; inc|], inc);
    ([|0 ; 1 ; lt ; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; eq ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; eq ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 1 ; eq ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; eq ; gt ; lt ; eq ; eq ; lt |], lt );
    ([|0 ; 1 ; eq ; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; eq ; gt ; eq ; gt ; gt ; eq |], gt );
    ([|0 ; 1 ; eq ; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; eq ; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; eq ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; eq ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; eq ; lt ; gt ; eq ; eq ; gt |], gt );
    ([|0 ; 1 ; eq ; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; eq ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; eq ; lt ; eq ; lt ; lt ; eq |], lt );
    ([|0 ; 1 ; eq ; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; eq ; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; eq ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; eq ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; eq ; eq ; eq ; eq ; eq ; eq |], gt );
    ([|0 ; 1 ; eq ; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; eq ; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; eq ; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; eq ; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; eq ; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; eq ; inc; eq ; inc; inc; eq |], inc);
    ([|0 ; 1 ; eq ; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; eq ; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; eq ; inc; inc; eq ; eq ; inc|], inc);
    ([|0 ; 1 ; eq ; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; inc; gt ; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 1 ; inc; gt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; inc; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; inc; gt ; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; inc; gt ; lt ; inc; gt ; inc|], inc);
    ([|0 ; 1 ; inc; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; inc; lt ; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; gt ; lt ; inc; eq ; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; inc; eq ; inc|], inc);
    ([|0 ; 1 ; inc; gt ; lt ; gt ; inc; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; lt ; inc; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; eq ; inc; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; inc; gt ; lt ; gt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; gt ; lt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; gt ; eq ; gt ; gt ; inc|], gt );
    ([|0 ; 1 ; inc; gt ; eq ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; inc; gt ; inc; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; inc; gt ; inc; gt ; gt ; lt |], inc);
    ([|0 ; 1 ; inc; gt ; inc; inc; gt ; lt |], inc);
    ([|0 ; 1 ; inc; gt ; inc; gt ; gt ; eq |], gt );
    ([|0 ; 1 ; inc; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; inc; gt ; inc; inc; gt ; inc|], inc);
    ([|0 ; 1 ; inc; gt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; inc; gt ; inc; gt ; inc; lt |], inc);
    ([|0 ; 1 ; inc; gt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; inc; gt ; inc; eq ; inc; lt |], inc);
    ([|0 ; 1 ; inc; gt ; inc; inc; inc; lt |], inc);
    ([|0 ; 1 ; inc; gt ; inc; gt ; inc; eq |], gt );
    ([|0 ; 1 ; inc; gt ; inc; gt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; inc; lt ; gt ; inc; gt ; gt |], gt );
    ([|0 ; 1 ; inc; lt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; inc; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; inc; lt ; gt ; inc; lt ; gt |], inc);
    ([|0 ; 1 ; inc; lt ; gt ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; inc; lt ; gt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; lt ; gt ; inc; eq ; gt |], gt );
    ([|0 ; 1 ; inc; lt ; gt ; inc; eq ; inc|], gt );
    ([|0 ; 1 ; inc; lt ; gt ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; inc; lt ; gt ; lt ; inc; gt |], inc);
    ([|0 ; 1 ; inc; lt ; gt ; eq ; inc; gt |], gt );
    ([|0 ; 1 ; inc; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; inc; lt ; gt ; lt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; lt ; gt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; inc; lt ; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; inc; lt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; lt ; eq ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; inc; lt ; eq ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; lt ; inc; lt ; lt ; gt |], lt );
    ([|0 ; 1 ; inc; lt ; inc; inc; lt ; gt |], inc);
    ([|0 ; 1 ; inc; lt ; inc; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; inc; lt ; inc; lt ; lt ; eq |], lt );
    ([|0 ; 1 ; inc; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; inc; lt ; inc; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; lt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; inc; lt ; inc; lt ; inc; gt |], inc);
    ([|0 ; 1 ; inc; lt ; inc; eq ; inc; gt |], gt );
    ([|0 ; 1 ; inc; lt ; inc; inc; inc; gt |], inc);
    ([|0 ; 1 ; inc; lt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; inc; lt ; inc; lt ; inc; eq |], inc);
    ([|0 ; 1 ; inc; lt ; inc; lt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; inc; eq ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; inc; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; inc; eq ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; eq ; eq ; inc; eq ; inc|], gt );
    ([|0 ; 1 ; inc; eq ; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; inc; eq ; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; inc; eq ; inc; eq ; inc; eq |], gt );
    ([|0 ; 1 ; inc; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; inc; inc; gt ; inc; gt ; gt |], gt );
    ([|0 ; 1 ; inc; inc; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 1 ; inc; inc; gt ; inc; gt ; inc|], gt );
    ([|0 ; 1 ; inc; inc; gt ; gt ; inc; gt |], gt );
    ([|0 ; 1 ; inc; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 1 ; inc; inc; gt ; gt ; inc; inc|], gt );
    ([|0 ; 1 ; inc; inc; gt ; lt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; gt ; eq ; inc; inc|], gt );
    ([|0 ; 1 ; inc; inc; gt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; inc; inc; lt ; inc; lt ; lt |], lt );
    ([|0 ; 1 ; inc; inc; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; inc; inc; lt ; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; inc; lt ; lt ; inc; lt |], lt );
    ([|0 ; 1 ; inc; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 1 ; inc; inc; lt ; gt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; lt ; lt ; inc; inc|], lt );
    ([|0 ; 1 ; inc; inc; lt ; eq ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; lt ; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; eq ; gt ; inc; inc|], gt );
    ([|0 ; 1 ; inc; inc; eq ; lt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; eq ; eq ; inc; inc|], gt );
    ([|0 ; 1 ; inc; inc; eq ; inc; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; gt ; gt ; gt |], gt );
    ([|0 ; 1 ; inc; inc; inc; inc; gt ; gt |], gt );
    ([|0 ; 1 ; inc; inc; inc; inc; gt ; lt |], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; gt ; eq |], gt );
    ([|0 ; 1 ; inc; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; gt ; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; lt ; gt |], inc);
    ([|0 ; 1 ; inc; inc; inc; lt ; lt ; lt |], lt );
    ([|0 ; 1 ; inc; inc; inc; inc; lt ; lt |], lt );
    ([|0 ; 1 ; inc; inc; inc; inc; lt ; eq |], inc);
    ([|0 ; 1 ; inc; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 1 ; inc; inc; inc; inc; lt ; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; eq ; gt |], gt );
    ([|0 ; 1 ; inc; inc; inc; inc; eq ; lt |], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; eq ; eq |], gt );
    ([|0 ; 1 ; inc; inc; inc; inc; eq ; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; gt ; inc; gt |], gt );
    ([|0 ; 1 ; inc; inc; inc; inc; inc; gt |], inc);
    ([|0 ; 1 ; inc; inc; inc; lt ; inc; lt |], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; inc; lt |], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; inc; eq |], inc);
    ([|0 ; 1 ; inc; inc; inc; gt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; lt ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; eq ; inc; inc|], inc);
    ([|0 ; 1 ; inc; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; gt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; gt ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; eq ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; inc; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; gt ; gt ; eq |], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; gt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; lt ; gt ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; eq ; gt ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; inc; gt ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; lt ; eq ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; lt ; inc; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; eq ; gt ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; gt ; eq ; lt ; gt ; lt |], lt );
    ([|0 ; 0 ; gt ; gt ; eq ; eq ; gt ; lt |], eq );
    ([|0 ; 0 ; gt ; gt ; eq ; inc; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; gt ; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; lt ; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; eq ; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; inc; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; gt ; gt ; inc; inc; gt ; inc|], inc);
    ([|0 ; 0 ; gt ; gt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; inc; inc; lt |], inc);
    ([|0 ; 0 ; gt ; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; eq ; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; inc; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; gt ; eq |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; lt ; eq |], lt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; eq ; gt |], gt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; eq ; lt |], lt );
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; eq ; eq |], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; eq ; inc|], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; inc; gt |], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; inc; lt |], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; inc; eq |], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; lt ; inc; inc|], inc);
    ([|0 ; 0 ; gt ; lt ; gt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; gt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; lt ; eq ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; lt ; inc; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; gt ; lt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; gt ; lt ; inc; lt ; inc; inc|], inc);
    ([|0 ; 0 ; gt ; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; gt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; eq ; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; eq ; gt ; eq ; gt ; eq |], gt );
    ([|0 ; 0 ; gt ; eq ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; eq ; eq ; lt ; eq ; lt |], lt );
    ([|0 ; 0 ; gt ; eq ; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; gt ; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; gt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; inc; gt ; inc; gt ; gt |], gt );
    ([|0 ; 0 ; gt ; inc; gt ; lt ; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; inc; gt ; inc; gt ; lt |], gt );
    ([|0 ; 0 ; gt ; inc; gt ; inc; gt ; eq |], gt );
    ([|0 ; 0 ; gt ; inc; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; inc; gt ; lt ; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; inc; gt ; eq ; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; inc; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; gt ; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; gt ; inc; gt ; lt ; inc; lt |], inc);
    ([|0 ; 0 ; gt ; inc; gt ; inc; inc; lt |], inc);
    ([|0 ; 0 ; gt ; inc; gt ; inc; inc; eq |], inc);
    ([|0 ; 0 ; gt ; inc; gt ; lt ; inc; inc|], inc);
    ([|0 ; 0 ; gt ; inc; gt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; gt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; inc; lt ; lt ; inc; lt |], lt );
    ([|0 ; 0 ; gt ; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; gt ; inc; eq ; lt ; inc; lt |], lt );
    ([|0 ; 0 ; gt ; inc; eq ; inc; inc; lt |], inc);
    ([|0 ; 0 ; gt ; inc; inc; lt ; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; inc; inc; inc; gt ; lt |], inc);
    ([|0 ; 0 ; gt ; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; gt ; inc; inc; lt ; gt ; inc|], inc);
    ([|0 ; 0 ; gt ; inc; inc; eq ; gt ; inc|], inc);
    ([|0 ; 0 ; gt ; inc; inc; inc; gt ; inc|], inc);
    ([|0 ; 0 ; gt ; inc; inc; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; gt ; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; gt ; inc; inc; lt ; eq ; lt |], lt );
    ([|0 ; 0 ; gt ; inc; inc; lt ; eq ; inc|], inc);
    ([|0 ; 0 ; gt ; inc; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; gt ; inc; inc; inc; inc; lt |], inc);
    ([|0 ; 0 ; gt ; inc; inc; lt ; inc; inc|], inc);
    ([|0 ; 0 ; gt ; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; gt ; eq |], gt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; eq ; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; inc; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; lt ; eq |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; eq ; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; eq ; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; eq ; eq |], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; eq ; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; inc; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; inc; eq |], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; gt ; inc; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; lt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; eq ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; inc; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; gt ; inc; gt ; inc; inc|], inc);
    ([|0 ; 0 ; lt ; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; lt ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; gt ; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; lt ; lt ; gt ; eq ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; gt ; inc; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; gt ; gt ; eq ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; gt ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; lt ; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; lt ; lt ; lt ; eq ; lt ; gt |], lt );
    ([|0 ; 0 ; lt ; lt ; lt ; inc; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; lt ; lt ; lt ; lt ; eq |], lt );
    ([|0 ; 0 ; lt ; lt ; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; lt ; lt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; lt ; eq ; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; eq ; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; lt ; lt ; eq ; eq ; lt ; gt |], eq );
    ([|0 ; 0 ; lt ; lt ; eq ; inc; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; inc; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; inc; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; lt ; lt ; inc; eq ; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; inc; inc; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; lt ; lt ; inc; inc; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; lt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; lt ; inc; inc; inc; gt |], inc);
    ([|0 ; 0 ; lt ; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; lt ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; eq ; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; eq ; lt ; eq ; lt ; eq |], lt );
    ([|0 ; 0 ; lt ; eq ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; eq ; eq ; gt ; eq ; gt |], gt );
    ([|0 ; 0 ; lt ; eq ; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; lt ; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; inc; gt ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; lt ; inc; lt ; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; inc; lt ; inc; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; inc; lt ; inc; lt ; lt |], lt );
    ([|0 ; 0 ; lt ; inc; lt ; inc; lt ; eq |], lt );
    ([|0 ; 0 ; lt ; inc; lt ; gt ; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; lt ; inc; lt ; eq ; lt ; inc|], lt );
    ([|0 ; 0 ; lt ; inc; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; lt ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; inc; lt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; lt ; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; lt ; inc; lt ; inc; inc; eq |], inc);
    ([|0 ; 0 ; lt ; inc; lt ; gt ; inc; inc|], inc);
    ([|0 ; 0 ; lt ; inc; lt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; lt ; inc; eq ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; inc; eq ; inc; inc; gt |], inc);
    ([|0 ; 0 ; lt ; inc; inc; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; lt ; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; inc; gt ; lt ; gt |], gt );
    ([|0 ; 0 ; lt ; inc; inc; inc; lt ; gt |], inc);
    ([|0 ; 0 ; lt ; inc; inc; gt ; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; lt ; inc; inc; eq ; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; inc; inc; lt ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; inc; gt ; eq ; gt |], gt );
    ([|0 ; 0 ; lt ; inc; inc; gt ; eq ; inc|], inc);
    ([|0 ; 0 ; lt ; inc; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; lt ; inc; inc; inc; inc; gt |], inc);
    ([|0 ; 0 ; lt ; inc; inc; gt ; inc; inc|], inc);
    ([|0 ; 0 ; lt ; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; eq ; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; eq ; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 0 ; eq ; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; eq ; gt ; lt ; eq ; eq ; lt |], lt );
    ([|0 ; 0 ; eq ; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; eq ; gt ; eq ; gt ; gt ; eq |], gt );
    ([|0 ; 0 ; eq ; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; eq ; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; eq ; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; eq ; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; eq ; lt ; gt ; eq ; eq ; gt |], gt );
    ([|0 ; 0 ; eq ; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; eq ; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; eq ; lt ; eq ; lt ; lt ; eq |], lt );
    ([|0 ; 0 ; eq ; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; eq ; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; eq ; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; eq ; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; eq ; eq ; eq ; eq ; eq ; eq |], eq );
    ([|0 ; 0 ; eq ; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; eq ; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; eq ; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; eq ; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; eq ; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; eq ; inc; eq ; inc; inc; eq |], inc);
    ([|0 ; 0 ; eq ; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; eq ; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; eq ; inc; inc; eq ; eq ; inc|], inc);
    ([|0 ; 0 ; eq ; inc; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; gt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; inc; gt ; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 0 ; inc; gt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; inc; gt ; lt ; gt ; gt ; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; inc; gt ; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; lt ; inc; gt ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; inc; lt ; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; lt ; inc; eq ; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; inc; eq ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; lt ; gt ; inc; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; lt ; inc; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; eq ; inc; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; inc; gt ; lt ; gt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; gt ; lt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; gt ; eq ; gt ; gt ; inc|], gt );
    ([|0 ; 0 ; inc; gt ; eq ; inc; gt ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; inc; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; inc; gt ; inc; gt ; gt ; lt |], inc);
    ([|0 ; 0 ; inc; gt ; inc; inc; gt ; lt |], inc);
    ([|0 ; 0 ; inc; gt ; inc; gt ; gt ; eq |], gt );
    ([|0 ; 0 ; inc; gt ; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; inc; inc; gt ; inc|], inc);
    ([|0 ; 0 ; inc; gt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; inc; gt ; inc; gt ; inc; lt |], inc);
    ([|0 ; 0 ; inc; gt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; inc; gt ; inc; eq ; inc; lt |], inc);
    ([|0 ; 0 ; inc; gt ; inc; inc; inc; lt |], inc);
    ([|0 ; 0 ; inc; gt ; inc; gt ; inc; eq |], inc);
    ([|0 ; 0 ; inc; gt ; inc; gt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; gt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; lt ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; inc; lt ; gt ; inc; gt ; gt |], gt );
    ([|0 ; 0 ; inc; lt ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; inc; lt ; gt ; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; inc; lt ; gt ; inc; lt ; gt |], inc);
    ([|0 ; 0 ; inc; lt ; gt ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; inc; lt ; gt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; lt ; gt ; inc; eq ; gt |], gt );
    ([|0 ; 0 ; inc; lt ; gt ; inc; eq ; inc|], inc);
    ([|0 ; 0 ; inc; lt ; gt ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; inc; lt ; gt ; lt ; inc; gt |], inc);
    ([|0 ; 0 ; inc; lt ; gt ; eq ; inc; gt |], gt );
    ([|0 ; 0 ; inc; lt ; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; inc; lt ; gt ; lt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; lt ; gt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; lt ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; inc; lt ; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; inc; lt ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; lt ; eq ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; inc; lt ; eq ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; lt ; inc; lt ; lt ; gt |], lt );
    ([|0 ; 0 ; inc; lt ; inc; inc; lt ; gt |], inc);
    ([|0 ; 0 ; inc; lt ; inc; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; inc; lt ; inc; lt ; lt ; eq |], lt );
    ([|0 ; 0 ; inc; lt ; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; inc; lt ; inc; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; lt ; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; inc; lt ; inc; lt ; inc; gt |], inc);
    ([|0 ; 0 ; inc; lt ; inc; eq ; inc; gt |], inc);
    ([|0 ; 0 ; inc; lt ; inc; inc; inc; gt |], inc);
    ([|0 ; 0 ; inc; lt ; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; inc; lt ; inc; lt ; inc; eq |], inc);
    ([|0 ; 0 ; inc; lt ; inc; lt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; lt ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; eq ; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; inc; eq ; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; inc; eq ; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; inc; eq ; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; eq ; eq ; inc; eq ; inc|], inc);
    ([|0 ; 0 ; inc; eq ; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; inc; eq ; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; inc; eq ; inc; eq ; inc; eq |], inc);
    ([|0 ; 0 ; inc; eq ; inc; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; gt ; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; inc; inc; gt ; inc; gt ; gt |], gt );
    ([|0 ; 0 ; inc; inc; gt ; gt ; gt ; inc|], gt );
    ([|0 ; 0 ; inc; inc; gt ; inc; gt ; inc|], gt );
    ([|0 ; 0 ; inc; inc; gt ; gt ; inc; gt |], gt );
    ([|0 ; 0 ; inc; inc; gt ; inc; inc; gt |], inc);
    ([|0 ; 0 ; inc; inc; gt ; gt ; inc; inc|], gt );
    ([|0 ; 0 ; inc; inc; gt ; lt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; gt ; eq ; inc; inc|], gt );
    ([|0 ; 0 ; inc; inc; gt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; lt ; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; inc; inc; lt ; inc; lt ; lt |], lt );
    ([|0 ; 0 ; inc; inc; lt ; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; inc; inc; lt ; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; inc; lt ; lt ; inc; lt |], lt );
    ([|0 ; 0 ; inc; inc; lt ; inc; inc; lt |], lt );
    ([|0 ; 0 ; inc; inc; lt ; gt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; lt ; lt ; inc; inc|], lt );
    ([|0 ; 0 ; inc; inc; lt ; eq ; inc; inc|], lt );
    ([|0 ; 0 ; inc; inc; lt ; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; eq ; gt ; inc; inc|], gt );
    ([|0 ; 0 ; inc; inc; eq ; lt ; inc; inc|], lt );
    ([|0 ; 0 ; inc; inc; eq ; eq ; inc; inc|], eq );
    ([|0 ; 0 ; inc; inc; eq ; inc; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; gt ; gt ; gt |], gt );
    ([|0 ; 0 ; inc; inc; inc; inc; gt ; gt |], gt );
    ([|0 ; 0 ; inc; inc; inc; inc; gt ; lt |], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; gt ; eq |], gt );
    ([|0 ; 0 ; inc; inc; inc; gt ; gt ; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; gt ; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; lt ; gt |], inc);
    ([|0 ; 0 ; inc; inc; inc; lt ; lt ; lt |], lt );
    ([|0 ; 0 ; inc; inc; inc; inc; lt ; lt |], lt );
    ([|0 ; 0 ; inc; inc; inc; inc; lt ; eq |], lt );
    ([|0 ; 0 ; inc; inc; inc; lt ; lt ; inc|], lt );
    ([|0 ; 0 ; inc; inc; inc; inc; lt ; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; eq ; gt |], gt );
    ([|0 ; 0 ; inc; inc; inc; inc; eq ; lt |], lt );
    ([|0 ; 0 ; inc; inc; inc; inc; eq ; eq |], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; eq ; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; gt ; inc; gt |], gt );
    ([|0 ; 0 ; inc; inc; inc; inc; inc; gt |], inc);
    ([|0 ; 0 ; inc; inc; inc; lt ; inc; lt |], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; inc; lt |], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; inc; eq |], inc);
    ([|0 ; 0 ; inc; inc; inc; gt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; lt ; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; eq ; inc; inc|], inc);
    ([|0 ; 0 ; inc; inc; inc; inc; inc; inc|], inc);
  ]
  in

  tbl





(* ------------ *)
(* Permutations *)
(* ------------ *)

(** Ordering matters when building the decision diagram, (i.e. checking su, 
    tv, sv, tu, or maybe checking sv, tu, su, tv). We wish to be able to test 
    any permutation. *)
let permutate_rows permutation tbl =
  tbl |> List.map (fun (x,y) -> 
    let [|a; b; c; d; x1; x2; x3; x4|] = x in
    let [p1; p2; p3; p4] = permutation in

    let x1' = List.nth [x1;x2;x3;x4] (p1-1) in
    let x2' = List.nth [x1;x2;x3;x4] (p2-1) in
    let x3' = List.nth [x1;x2;x3;x4] (p3-1) in
    let x4' = List.nth [x1;x2;x3;x4] (p4-1) in

    let x' = [|a; b; c; d; x1'; x2'; x3'; x4'|] in

    (x',y)
  )

let list_table_to_array_table tbl =
  let open Bigarray in

  let arr_tbl = Genarray.create int8_signed c_layout [|2;2;4;4;4;4;4;4|] in
  Genarray.fill arr_tbl (-1);
  let set = Genarray.set arr_tbl in

  List.iter (fun (x,y) -> set x y) tbl;
  arr_tbl

let permutation =
  let input = Sys.argv in
  if Array.length input = 5 then (
    let p1 = input.(1) in
    let p2 = input.(2) in
    let p3 = input.(3) in
    let p4 = input.(4) in
    [p1;p2;p3;p4] |> List.map int_of_string
  ) else (
    eprintf "Usage: %s <permutation>\n" input.(0);
    exit 1
  )

let table = 
  table
  |> permutate_rows permutation
  |> list_table_to_array_table



(* ----------------------- *)
(* Ordered decision digram *)
(* ----------------------- *)

type odd = 
  | BinNode of {neg: odd; pos: odd}
  | OrdNode of {eq: odd; gt: odd; lt: odd; inc: odd}
  | SkipNode of odd
  | TermNode of term_result
and term_result = Ok of PartialOrd.t | Intransitive

let indent n = 
  String.make n ' '

let rec odd_to_string indent' x =
  let odd_to_string y = odd_to_string (indent'+2) y in
  let indent_str = indent (indent'+2) in
  match x with
  | BinNode {neg; pos} ->
    sprintf "BinNode {\n%sneg=( %s ); \n%spos=( %s )}" 
      (indent_str) (odd_to_string neg) 
      (indent_str) (odd_to_string pos)
  | OrdNode {eq; gt; lt; inc} ->
    sprintf "OrdNode {\n%seq=( %s ); \n%sgt=( %s ); \n%slt=( %s ); \n%sinc=( %s )}" 
      (indent_str) (odd_to_string eq) 
      (indent_str) (odd_to_string gt) 
      (indent_str) (odd_to_string lt) 
      (indent_str) (odd_to_string inc)
  | SkipNode x ->
    sprintf "SkipNode \n%s( %s )" (indent_str) (odd_to_string x)
  | TermNode Ok x ->
<<<<<<< HEAD
    sprintf "TermNode (Ok %s)" (PartialOrd.to_string x)
=======
    sprintf "TermNode %s" (PartialOrd.to_string x)
>>>>>>> andrepd/smt-interface
  | TermNode Intransitive ->
    sprintf "ErrNode"

let odd_to_string x = 
  odd_to_string 0 x ^ "\n"

(* If xx and yy are equal modulo (∀z. Intransitive = z), return [Some result], else return [None]. *)
(* We assert ∀z. Intransitive = z because we assume this branch is never going to be taken, so it can be collapsed into any other branch. *)
let rec odd_equal xx yy : odd option =
  let open Option.I in
  match xx,yy with
  | TermNode x, TermNode y ->
    begin match x,y with
    | Ok a, Ok b ->
      if a = b then
        Some (TermNode (Ok a))
      else 
        None

    | Ok a, Intransitive 
    | Intransitive, Ok a ->
      Some (TermNode (Ok a))

    | Intransitive, Intransitive ->
      Some (TermNode Intransitive)
    end

  | BinNode x, BinNode y ->
    odd_equal x.neg y.neg >>= fun neg' ->
    odd_equal x.pos y.pos >>= fun pos' ->
    Some (BinNode {neg=neg'; pos=pos'})

  | OrdNode x, OrdNode y ->
    odd_equal x.eq  y.eq  >>= fun eq' ->
    odd_equal x.gt  y.gt  >>= fun gt' ->
    odd_equal x.lt  y.lt  >>= fun lt' ->
    odd_equal x.inc y.inc >>= fun inc' ->
    Some (OrdNode {eq=eq'; gt=gt'; lt=lt'; inc=inc'})

  (* Intransitive collapses with any *)
  | TermNode Intransitive, x
  | x, TermNode Intransitive ->
    Some x

  | SkipNode x, SkipNode y ->
    odd_equal x y >>= fun res ->
    Some (SkipNode res)

  | SkipNode _, _ 
  | _, SkipNode _ ->
    None

  | OrdNode _, BinNode _ 
  | BinNode _, OrdNode _ 
  | OrdNode _, TermNode _
  | TermNode _, OrdNode _
  | BinNode _, TermNode _
  | TermNode _, BinNode _  ->
  (* | _ -> *)
    (* print_endline @@ sprintf "comparing %s and %s" (odd_to_string xx) (odd_to_string yy); *)
    (* assert false *)
    None



(* Initial odd, from table *)
let odd_of_table table = 
  let rec f shape vals = 
    match shape with
    | 2::tl ->  (* BinNode *)
      let neg = f tl (0::vals) in
      let pos = f tl (1::vals) in
      BinNode {neg; pos}

    | 4::tl ->  (* OrdNode *)
      let open PartialOrd in
      let eq  = f tl (to_int EQ :: vals) in
      let gt  = f tl (to_int GT :: vals) in
      let lt  = f tl (to_int LT :: vals) in
      let inc = f tl (to_int INC:: vals) in
      OrdNode {eq; gt; lt; inc}

    | _::tl -> 
      assert false

    | [] ->
      (* let [tv; tu; sv; su; uv; st; sign2; sign1] = vals in *)
      let indices = vals |> List.rev |> Array.of_list in 
      (* Array.iter (fun i -> print_int i; print_string ";") indices; print_newline(); *)
      try
        let res = Bigarray.(Genarray.get table indices) (* |> tap print_int *) |> PartialOrd.of_int in
        (* print_endline (PartialOrd.to_string res); *)
        TermNode (Ok res)
      with Invalid_argument _ ->
        (* print_endline ("none"); *)
        TermNode (Intransitive)
  in
  let shape = [2;2;4;4;4;4;4;4] in
  f shape []

let odd = odd_of_table table



(* let () =
  print_endline "========";
  print_endline "Original";
  print_endline "========";
  print_newline();
  print_endline @@ odd_to_string odd;
  print_newline();
  () *)



(* If [p x y] for all consecutive pairs of elements (including last and first) then true else false *)
(* let rec (consecutive_equal: ('a -> 'a -> bool) -> 'a list -> bool) p l = *)
let rec consecutive_equal (p: 'a -> 'a -> bool) (l: 'a list) : bool =
  match l with
  | x1::(x2::_ as rest) ->
    p x1 x2 && consecutive_equal p rest
  | _::_ ->
    true
  | [] ->
    assert false
let consecutive_equal p l =
  match l with
  | [] -> 
    true
  | _::_ ->
    consecutive_equal p l && (
      let first = List.hd l in
      let last  = List.hd @@ List.rev l in 
      p first last
    )

let rec odd_reduce x =
  let open Option.I in  (* Option monadic operators, very useful here *)

  (* Helper function, collapses intransitive nodes *)
  let coalesce_intransitive x =
    match x with
    | SkipNode (TermNode Intransitive) -> TermNode Intransitive
    | _ -> x
  in

  match x with
  | BinNode {neg; pos} ->
    let neg' = odd_reduce neg in
    let pos' = odd_reduce pos in
    begin match odd_equal neg' pos' with
    | Some res ->
      SkipNode res |> coalesce_intransitive
    | None ->
      BinNode {neg=neg'; pos=pos'}
    end

  | OrdNode {eq; gt; lt; inc} ->
    let eq'  = odd_reduce eq in
    let gt'  = odd_reduce gt in
    let lt'  = odd_reduce lt in
    let inc' = odd_reduce inc in
    begin match
      eq' |>
      odd_equal gt' >>=
      odd_equal lt' >>=
      odd_equal inc' >>=
      odd_equal eq'
    with
    | Some res -> 
      SkipNode res |> coalesce_intransitive
    | _ ->
      OrdNode {eq=eq'; gt=gt'; lt=lt'; inc=inc'}
    end

  | SkipNode y -> 
    let y' = odd_reduce y in
    begin match y' with
    | TermNode Intransitive ->
      y'
    | _ ->
      SkipNode y'
    end

  | TermNode _ -> x

let odd = odd_reduce odd



let () =
  print_endline "=======";
  print_endline "Reduced";
  print_endline "=======";
  print_newline();
  print_endline @@ odd_to_string odd;
  print_newline();
  ()
