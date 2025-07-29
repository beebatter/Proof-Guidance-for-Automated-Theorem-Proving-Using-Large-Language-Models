(**
  Feature index-style cache:
  Stores and updates arbtrary data based on list of features.

  Features can be any OrderedType

  Main operation: is get_geq geting greater or equal component-wise subindex

  This is used for the demodulation cache see src/demodulationIndexPerfect.ml
  (can be also used for subsumption but currently verctorIndex is used for that).

   Inv:
   Two features in the index can be of different length but can not be exact prefix of another:
   [f_1,..,f_{i},f_{i+1},..,f_n] and
   [f_1,..,f_{i}]
   are not allowed.
   If such features are needed then these can be encoded using special token End in the Feature  OrderedType 
   where End is smaller than any other element of the Feature type.
   Then we can add End as the last feature in the sequence
   
   [f_1,..,f_{i},f_{i+1},..,f_n, End] and
   [f_1,..,f_{i},End]
   And we will preserve the invariant as End != f_{i+1}.
   
 *)

open Lib

module Make : functor (Feat : Map.OrderedType) ->
  sig
    type feat_list = Feat.t list

(** 'a in the index can be e.g. set or map, or any other data  *)          
    type 'a index

    val empty: 'a index
        
(** Finds the element of the indexed by the feature list
   can raise Not_found *)
          
    val find : feat_list -> 'a index -> 'a

(** app_leaf applies f to the leaf element indexed by the feature list
    if festure path does not exist in the index then it is created and (f None) is stored
    if (f x) = None then  this path is removed from the index

 *)   
    val app_leaf :
        ('a option -> 'a option) ->
          feat_list -> 'a index -> 'a index option

(** get_less gets an new index consisting of all instances where at least one feature is strictly less then
    the correcponding feature in feat_list

    This is used for the demodulation cache see src/demodulationIndexPerfect.ml
    (can be also used for subsumption but currently verctorIndex is used for that).

 *)
    val get_less : feat_list -> 'a index -> 'a index

    val iter : ('a -> unit) -> 'a index -> unit
  end
