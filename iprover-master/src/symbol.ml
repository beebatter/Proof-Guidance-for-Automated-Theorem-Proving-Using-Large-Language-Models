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

open Lib
open Options


(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace

let dbg_gr_to_str = function 
  | D_trace -> "trace"

let dbg_groups = [
  D_trace;
]
    

(*----- debug fixed part --------*)

let module_name = __MODULE__

let () = Lib.dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



type arity = int param
type key = string


type sproperty =
  |Theory
  |Num_real of Q.t 
(* |Num_int (* of int *)
  |Num_rat of string * string (*int * int*) *)
  |Num_int of Z.t
  |Num_rat of Q.t
  |Definition  (* INVARIANT: Cannot have fields (`of stuff`) *) (* iProver introduced defintions *)
  |Smallest
  |Flat
  |FunPred        (* use only in translation Non-Eq -> Eq, see preprocess *)
  |DomainConstant (* use only in finite_models*)
  |DomainPred     (* use only in finite_models*)  (* INVARIANT: Cannot have fields (`of stuff`) *)
  |DefPred        (* use only in finite_models*)
  |Type
  |Quantifier
  |Connective
  |Placeholder
  |Undef_Prop  (* INVARIANT: Cannot have fields (`of stuff`) *)
  |FDLess of int
  |FDRange of int * int


let to_string_sproperty = function
  |Theory           -> "theory"
  |Num_real _       -> "num_real"
  |Num_int _        -> "num_int"
  |Num_rat _        -> "num_rat"
  |Definition       -> "definition"
  |Smallest         -> "smallest"
  |Flat             -> "flat"
  |FunPred          -> "funpred"  (* in translation Non-Eq -> Eq, see preprocess *)
  |DomainConstant   -> "domain constant"  (*in finite models *)
  |DomainPred       -> "domain predicate" (*in finite models *)
  |DefPred          -> "defpred"
  |Type             -> "type"
  |Connective       -> "connective"
  |Quantifier       -> "quantifier"
  |Placeholder      -> "placeholder"
  |Undef_Prop       -> "undefined property"
  |FDLess i         -> "less_"^(string_of_int i) (*finte domain less_i (x)*)
  |FDRange (i, j)   -> "range_"^(string_of_int i)^"_"^(string_of_int j)
(* finite domain range_i_j (x)*)
(*
  |BitVector i -> "bv_"^(string_of_int i)
  |Memory (i,j) -> "memory_"^(string_of_int i)^"_"^(string_of_int j)
 *)

(*  |Type stype       -> ("type_"^(to_string_stype stype)) *)


let to_stream_sproperty s sproperty =
  s.stream_add_str (to_string_sproperty sproperty)


(* fast key can be used only when symb. are already put in symbolDB*)
type fast_key = IOption.t


type symbol_bool_param = int

(* conjecture symbol but not the common theory symbols such as =,+,-*)
let is_conj_symb                   = 0
let large_ax_considered_gr         = 1
let large_ax_considered_non_gr     = 2

let is_bound_constant              = 3 (* used in BMC *)

(* signature symbol is used to distinguish fun/preds from other symbols like connectives types etc.*)
let signature_symb                 = 4

let is_input                       = 5

(* is not in a clause that got removed during proprocessed etc. *)
let is_essential_input             = 6

(* defined predicates are predicates connected with father_of/son_of attributes *)
(* we compute reachibility relation/and priorities based on these predicates *)
let is_defined_symb_input           =7


let is_clock = 8
let is_less  = 9
let is_range = 10


let is_special = 11


(* use SymbolDB.get_num_of_sym_groups to get the actual number of groups*)
(* groups a numbered from 0 to max_num_of_sym_groups-1 *)
let max_num_of_sym_groups = 100



type symbol = {
  mutable fast_key: fast_key;
  mutable db_id: fast_key;
  mutable bool_param: Bit_vec.bit_vec;
  name: string;
  mutable arity: arity;
  mutable stype: stype; (*(symbol list) param;*)
  mutable sproperty: sproperty;
  mutable group: int param;
  (* it is useful to partition all symbols to groups e.g. to represent
     signature information in terms
     inv: group number should be less or equal to the Bit_vec.max_pos *)
  (*     mutable is_input : bool param;*)
  (* chnaged to bool_param *)
  (* is_input -- the symbol is in the input of the problem *)
  mutable is_skolem: bool;
  (* the symbol is a skolem function *)
  (*    mutable hash    : int param;*)
  (* hash is assigned when added to symbol db *)

  (* number of occurences of this symbol into *)
  mutable num_input_occur: int;

  (* circuit depth *)
  mutable defined_depth: int param;
  mutable flattening: symbol param
}

and stype = (symbol list) param (* Def([value_type;arg_type1;..;arg_typen]) *)

type t = symbol

let set_bool_param value param symb =
  symb.bool_param <- Bit_vec.set value param symb.bool_param

let get_name s = s.name

let get_bool_param param symb =
  Bit_vec.get param symb.bool_param



let is_signature_symb symb    = get_bool_param signature_symb symb
let set_signature_symb b symb = set_bool_param b signature_symb symb

let is_special_symb symb    = get_bool_param is_special symb
let set_is_special_symb b symb = set_bool_param b is_special symb


(*
  let is_signature_symb symb  = symb.is_signature
  let set_signature_symb b symb = symb.is_signature_symb <- b
 *)

let inherit_bool_param param from_s to_s =
  set_bool_param
    (get_bool_param param from_s) param to_s

let inherit_bool_param_all from_s to_s =
  to_s.bool_param <- from_s.bool_param

(* value of the type is the first in the stype list *)
let create_stype args value =
  Def(value::args)

(* assume stypes are never empty: they have alawys value type *)
(* let create_empty_stype () = Def([]) *)


let split_stype stype = 
  match stype with 
  |Undef     -> None
  |Def((val_type::arg_types)) -> Some((val_type, arg_types))
  |Def([])   -> failwith "split_stype: types should have at least value type"




let get_stype_args_val symb =
  match symb.stype with
  |Def(value::args) -> Def((args,value))
  |_-> Undef

let get_stype_args_val_def symb =
  match symb.stype with
  |Def(value::args) -> (args,value)
  |_-> failwith "get_stype_args_val_def: arg_types should be defined"

let get_val_type_def symb =
  (* let _arg_types, val_type = get_stype_args_val_def sym in
  val_type *)
  match symb.stype with
  | Def (value::_args) -> value
  | _-> failwith (sprintf "get_val_type_def:%s :symb.stype should be defined" symb.name)

let dassert_stype_prop stype = 
  dassert (fun () -> 
    match stype with 
    | Def stype -> 
      stype |> List.for_all (fun t -> 
        if Poly.(t.sproperty <> Type) then      
          failwith (sprintf "symbol.ml: type symbol: %s with property %s which is different from required Type property and would break invariants" 
            t.name (to_string_sproperty t.sproperty)
          )
        else
          true
      )
    | Undef -> true
  )

let assign_stype s stype =
  dassert_stype_prop stype;
  s.stype <- stype

(*------------*)

let iprover_pref = "$$iProver_"

let add_iprover_pref str = iprover_pref^str

let remove_iprover_pref str = 
  let iprover_str_length = String.length iprover_pref in
  try 
    let to_test = (String.sub str 0 iprover_str_length) in
    if String.O.(to_test = iprover_pref) 
    then
      let tail_length = (String.length str) - iprover_str_length in
      let tail_str = String.sub str iprover_str_length tail_length in
      tail_str
    else str  
  with 
    Invalid_argument _ -> str

(*---------special symbols----------*)

(* epmty symbol is just used as a template *)

let empty_symb_fun() = {
  fast_key   = IOption.none;
  db_id      = IOption.none;
  name       = "";
  stype      = Undef;
  arity      = Undef;
  sproperty  = Undef_Prop;
  group      = Undef;
  (*   is_input   = Undef;*)
  is_skolem  = false;
  bool_param = Bit_vec.false_vec;
  num_input_occur = 0;
  defined_depth   = Undef;
  flattening = Undef
  (*   hash = Undef;*)
}

let empty_symb = empty_symb_fun()

(* signature symbols*)
let empty_sig_symb =
  let sig_symb = empty_symb_fun() in
  set_signature_symb true sig_symb;
  sig_symb

(*
  let create_less_symb (stype:stype) i    =
  {empty_sig_symb with
  sproperty = FDLess i;
  name = "$"^(to_string_sproperty (FDLess i));
  arity         = Def(1);
  is_skolem     = Def(false);
  stype         = stype;
  }

 *)



(*--------------Types--------------*)


(*--------------------------------------------------*)

(* $tType is the type for all types *)
(* variables of this type unifiable with any other type *)
(* it is a supertype (other types are incomparable) *)
(* defined before symb_type_template *)

let symb_type_types = {
  empty_sig_symb with
  name      = "$tType";
  arity     = Def(0);
  sproperty = Type;     
                     }
(* assign $tType to have $tType *)
let () =  symb_type_types.stype <- Def([symb_type_types])
    
let is_basic_stype stype = 
  match stype with 
  |Def([val_type]) -> val_type == symb_type_types
  |_ -> false

(* $$tBot is a subtype of any type; bot symb is of this type  *)
(* $$tBot does not have a type *)
let symb_bot_type = {
  empty_symb with
  name      = "$$tBot";
  arity     = Def(0);
  sproperty = Type;
}


(* can be as constants in typed equality *)
let symb_type_template = {
  empty_sig_symb with
  arity     = Def(0);
  stype     = create_stype [] symb_type_types;
  sproperty = Type;
}



let symb_bool_type = {
  symb_type_template with
  name = "$o";
}

let symb_default_type = {
  symb_type_template with
  name = "$i";
}

let symb_real_type = {
  symb_type_template with
  name = "$real";
}

let symb_int_type = {
  symb_type_template with
  name = "$int";
}

let symb_rat_type = {
  symb_type_template with
  name = "$rat";
}



let symb_term_algebra_type = {
  symb_type_template with
  name = "$$term_algebra_type";
}

(* used in function translations like args of preds in QBF translation *)
(* domain of $$bool_fun: {$$true_fun, $$false_fun}*)
let symb_bool_fun_type = {
  symb_type_template with
(*  name = "$$bool_fun"; *)
  name = "$bool"; (* vampire outputs bool_fun as  $bool*)
}


(*---------types used in verification -----*)
(* since these symbols are  not defined in TPTP we use $$, to be consistent with current Zurab output $$ is omitted *)

(* old Intel benchmarks requre "state_type" in places of "$$state_type" *)

let symb_ver_state_type = {
  symb_type_template with
  name = "$$state_type";
  (*   name      = "state_type"; *)
}

let symb_ver_address_type = {
  symb_type_template with
  name = "$$address_type";
  (*   name      = "address_type"; *)
}

let symb_ver_bit_index_type = {
  symb_type_template with
  name = "$$bitindex_type";
  (*   name      = "bitindex_type"; *)
}

let symb_ver_relation_index_type = {
  symb_type_template with
  name = "$$reli_type";
}

let create_type_symb_from_str
    (*~is_sig:true since can occur in typed equalities*)
    ?is_sig:(is_sig=true) symb_name =
  let symb = {
    symb_type_template with
    name = symb_name;
  }
  in
  set_signature_symb is_sig symb;
  symb

let defined_types = [
  symb_default_type;
  symb_bool_type;
  symb_real_type;
  symb_int_type;
  symb_rat_type;
  symb_bool_fun_type; 
  symb_term_algebra_type;
  symb_ver_state_type;
  symb_ver_address_type;
  symb_ver_bit_index_type;
  symb_ver_relation_index_type;
  symb_bot_type;
  symb_type_types;
]

let is_defined_type symb =
  List.memq symb defined_types


(*-------------Connectives-----------------------*)
(* possible clash if someone defines symbols '~' as e.g. function symbol *)

let symb_connective_template = {
  empty_symb with
  sproperty = Connective;
}

let symb_unary_connective_template = {
  symb_connective_template with
  arity = Def(1);
  stype = create_stype [symb_bool_type] symb_bool_type;
}

let symb_binary_connective_template = {
  symb_connective_template with
  arity = Def(2);
  stype = create_stype [symb_bool_type;symb_bool_type] symb_bool_type;
}

let symb_neg = {
  symb_unary_connective_template with
  name = "~";
}

(*
  let symb_and =
  {symb_binary_connective_template with
  name      = "&";
  }

  let symb_or =
  {symb_binary_connective_template with
  name      = "|";

  }

  let symb_impl =
  {symb_binary_connective_template with
  name      = "->";
  }
 *)

(*-----------quantifiers------------*)

(* at the moment quantifiers with undef type *)
(* (generally should be polymorphic: (n-> bool) -> (n-1 -> bool) *)

(*
  let symb_quantifier_template =
  { empty_symb with
  arity         = Def(1);
  is_skolem     = Def(false);
  sproperty     = Quantifier;
  }

  let symb_forall =
  { symb_quantifier_template with
  name      = "!";
  }

  let symb_exists =
  {symb_quantifier_template  with
  name      = "?";
  }
 *)

(*--------theory symbols--------------*)

let theory_symbol_template =
  {
    empty_sig_symb with
    sproperty = Theory;
  }


(* theory symbols*)
let symb_true =
  {theory_symbol_template with
   name      = "$true";
   arity     = Def(0);
   stype     = create_stype [] symb_bool_type;
  }

let symb_false =
  {theory_symbol_template with
   name      = "$false";
   arity     = Def(0);
   stype     = create_stype [] symb_bool_type;
  }


(* see bool_fun *)
let symb_true_fun =
  {theory_symbol_template with
(*   name      = "$$true_fun"; *)
   name      = "$$true";  (* vampire outputs $$true/$$false for functional true/false*)
   arity     = Def(0);
   stype     = create_stype [] symb_bool_fun_type;
  }

let symb_false_fun =
  {theory_symbol_template with
(*   name      = "$$false_fun"; *)
   name      = "$$false";
   arity     = Def(0);
   stype     = create_stype [] symb_bool_fun_type;
  }



(* only typed equality remains; for untyped use $i type or $tType for the top type *)
(*
  let symb_equality =
  {theory_symbol_template with
  name      = "=";
  arity     = Def(2);
  stype     = create_stype [symb_type_types;symb_type_types] symb_bool_type;
  }
 *)

(* typed equality:  $equality_sorted(type_name,t_1,t2) *)
let symb_typed_equality =
  {theory_symbol_template with
   name      = "$$equality_sorted";
   arity     = Def(3);
   stype     = create_stype [symb_type_types;symb_type_types;symb_type_types] symb_bool_type;
  }

(* sometimes it is usefull use a separate predicate for t!=s *)
let symb_dis_equality = 
  {theory_symbol_template with
   name      = "$$dis_equality_sorted";
   arity     = Def(3);
   stype     = create_stype [symb_type_types;symb_type_types;symb_type_types] symb_bool_type;
  }  

(*------- symbols for verification -------*)
let symb_ver_next_state =
  {
    theory_symbol_template with
    name      = "$$nextState";
    arity     = Def(3);
    stype     = create_stype [symb_ver_relation_index_type;symb_ver_state_type;symb_ver_state_type] symb_bool_type;
    (*   stype     = create_stype [symb_type_types;symb_type_types] symb_bool_type;*)
  }

let symb_ver_reachable_state =
  {
    theory_symbol_template with
    name      = "$$reachableState";
    arity     = Def(1);
    (*   stype     = create_stype [symb_type_types] symb_bool_type;*)
    stype     = create_stype [symb_ver_state_type] symb_bool_type;
  }

(* tsar: add symbol $$init *)
let symb_ver_init =
  {
    theory_symbol_template with
    name      = "$$init";
    arity     = Def(2);
    stype     = create_stype [symb_ver_relation_index_type;symb_ver_state_type] symb_bool_type;
    (* stype     = create_stype [symb_ver_state_type] symb_bool_type; *)
  }

(* tsar: add symbol $$property *)
let symb_ver_property =
  {
    theory_symbol_template with
    name      = "$$property";
    arity     = Def(1);
    stype     = create_stype [symb_ver_state_type] symb_bool_type;
  }

(* tsar: add symbol $$equiv_state *)
let symb_ver_equal =
  {
    theory_symbol_template with
    name      = "$$equiv_state";
    arity     = Def(2);
    stype     = create_stype [symb_ver_state_type;symb_ver_state_type] symb_bool_type;
  }

(* tsar: add symbol $$eligible *)
let symb_ver_eligible =
  {
    theory_symbol_template with
    name      = "$$eligibleDeadlock";
    arity     = Def(1);
    stype     = create_stype [symb_ver_state_type] symb_bool_type;
  }

(* tsar: add symbol $$deadlock *)
let symb_ver_deadlock =
  {
    theory_symbol_template with
    name      = "$$deadlock";
    arity     = Def(1);
    stype     = create_stype [symb_ver_state_type] symb_bool_type;
  }


(* ------------ arithmetic operations -------  *)
(* currently prefixed with type real/int *)
(* do not change names: in parser_types.ml prefix is added in a pattern; names needed to match for correct associations *)

let symb_sum_real =
  {theory_symbol_template with
   name      = "$sum_real";
   arity     = Def(2);
   stype     = create_stype [symb_real_type;symb_real_type] symb_real_type;
 }

let symb_difference_real =
  {theory_symbol_template with
   name       = "$difference_real";
   arity      = Def(2);
   stype      = create_stype [symb_real_type;symb_real_type] symb_real_type;
 }

let symb_product_real =
  {theory_symbol_template with
   name      = "$product_real";
   arity     = Def(2);
   stype     = create_stype [symb_real_type;symb_real_type] symb_real_type;
 }

let symb_quotient_real =
  {theory_symbol_template with
   name       = "$quotient_real";
   arity      = Def(2);
   stype      = create_stype [symb_real_type;symb_real_type] symb_real_type;
 }
    
let symb_uminus_real =
  {theory_symbol_template with
   name      = "$uminus_real";
   arity     = Def(1);
   stype     = create_stype [symb_real_type] symb_real_type;
 }

 

let symb_less_real =
  {theory_symbol_template with
   name       = "$less_real";
   arity      = Def(2);
   stype      = create_stype [symb_real_type;symb_real_type] symb_bool_type;
 }

let symb_lesseq_real =
  {theory_symbol_template with
   name       = "$lesseq_real";
   arity      = Def(2);
   stype      = create_stype [symb_real_type;symb_real_type] symb_bool_type;
 }

let symb_greater_real =
  {theory_symbol_template with
   name       = "$greater_real";
   arity      = Def(2);
   stype      = create_stype [symb_real_type;symb_real_type] symb_bool_type;
 }

let symb_greatereq_real =
  {theory_symbol_template with
   name       = "$greatereq_real";
   arity      = Def(2);
   stype      = create_stype [symb_real_type;symb_real_type] symb_bool_type;
 }

let symb_to_int_real =
  {theory_symbol_template with
   name       = "$to_int_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_int_type;
 }

let symb_to_rat_real =
  {theory_symbol_template with
   name       = "$to_rat_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_rat_type;
 }

let symb_to_real_real =
  {theory_symbol_template with
   name       = "$to_real_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_real_type;
 }

let symb_is_int_real =
  {theory_symbol_template with
   name       = "$is_int_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_bool_type;
 }

let symb_is_rat_real =
  {theory_symbol_template with
   name       = "$is_rat_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_bool_type;
 }


let symb_floor_real =
  {theory_symbol_template with
   name       = "$floor_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_real_type;
 }

let symb_ceiling_real =
  {theory_symbol_template with
   name       = "$ceiling_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_real_type;
 }

let symb_truncate_real =
  {theory_symbol_template with
   name       = "$truncate_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_real_type;
 }

let symb_round_real =
  {theory_symbol_template with
   name       = "$round_real";
   arity      = Def(1);
   stype      = create_stype [symb_real_type] symb_real_type;
 }



(* ----- rat ----- *)
let symb_sum_rat =
  {theory_symbol_template with
   name      = "$sum_rat";
   arity     = Def(2);
   stype     = create_stype [symb_rat_type;symb_rat_type] symb_rat_type;
 }

let symb_difference_rat =
  {theory_symbol_template with
   name       = "$difference_rat";
   arity      = Def(2);
   stype      = create_stype [symb_rat_type;symb_rat_type] symb_rat_type;
 }

let symb_product_rat =
  {theory_symbol_template with
   name      = "$product_rat";
   arity     = Def(2);
   stype     = create_stype [symb_rat_type;symb_rat_type] symb_rat_type;
 }

let symb_quotient_rat =
  {theory_symbol_template with
   name       = "$quotient_rat";
   arity      = Def(2);
   stype      = create_stype [symb_rat_type;symb_rat_type] symb_rat_type;
 }
    
let symb_uminus_rat =
  {theory_symbol_template with
   name      = "$uminus_rat";
   arity     = Def(1);
   stype     = create_stype [symb_rat_type] symb_rat_type;
 }

 

let symb_less_rat =
  {theory_symbol_template with
   name       = "$less_rat";
   arity      = Def(2);
   stype      = create_stype [symb_rat_type;symb_rat_type] symb_bool_type;
 }

let symb_lesseq_rat =
  {theory_symbol_template with
   name       = "$lesseq_rat";
   arity      = Def(2);
   stype      = create_stype [symb_rat_type;symb_rat_type] symb_bool_type;
 }

let symb_greater_rat =
  {theory_symbol_template with
   name       = "$greater_rat";
   arity      = Def(2);
   stype      = create_stype [symb_rat_type;symb_rat_type] symb_bool_type;
 }

let symb_greatereq_rat =
  {theory_symbol_template with
   name       = "$greatereq_rat";
   arity      = Def(2);
   stype      = create_stype [symb_rat_type;symb_rat_type] symb_bool_type;
 }

let symb_to_int_rat =
  {theory_symbol_template with
   name       = "$to_int_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_int_type;
 }

let symb_to_rat_rat =
  {theory_symbol_template with
   name       = "$to_rat_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_rat_type;
 }

let symb_to_real_rat =
  {theory_symbol_template with
   name       = "$to_real_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_real_type;
 }

let symb_is_int_rat =
  {theory_symbol_template with
   name       = "$is_int_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_bool_type;
 }

let symb_is_rat_rat =
  {theory_symbol_template with
   name       = "$is_rat_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_bool_type;
 }


let symb_floor_rat =
  {theory_symbol_template with
   name       = "$floor_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_rat_type;
 }

let symb_ceiling_rat =
  {theory_symbol_template with
   name       = "$ceiling_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_rat_type;
 }

let symb_truncate_rat =
  {theory_symbol_template with
   name       = "$truncate_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_rat_type;
 }

let symb_round_rat =
  {theory_symbol_template with
   name       = "$round_rat";
   arity      = Def(1);
   stype      = create_stype [symb_rat_type] symb_rat_type;
 }



(*-------- int ------*)

let symb_sum_int =
  {theory_symbol_template with
   name      = "$sum_int";
   arity     = Def(2);
   stype     = create_stype [symb_int_type;symb_int_type] symb_int_type;
 }
    
let symb_difference_int =
  {theory_symbol_template with
   name       = "$difference_int";
   arity      = Def(2);
   stype      = create_stype [symb_int_type;symb_int_type] symb_int_type;
 }

let symb_product_int =
  {theory_symbol_template with
   name      = "$product_int";
   arity     = Def(2);
   stype     = create_stype [symb_int_type;symb_int_type] symb_int_type;
 }

let symb_quotient_e_int =
  {theory_symbol_template with
   name      = "$quotient_e_int";
   arity     = Def(2);
   stype     = create_stype [symb_int_type;symb_int_type] symb_int_type;
 }

let symb_remainder_e_int =
  {theory_symbol_template with
   name      = "$remainder_e_int";
   arity     = Def(2);
   stype     = create_stype [symb_int_type;symb_int_type] symb_int_type;
 }

(* let symb_abs_int =
  {theory_symbol_template with
   name      = "$abs_int";
   arity     = Def(1);
   stype     = create_stype [symb_int_type] symb_int_type;
 } *)

let symb_uminus_int =
  {theory_symbol_template with
   name      = "$uminus_int";
   arity     = Def(1);
   stype     = create_stype [symb_int_type] symb_int_type;
 }
 

let symb_less_int =
  {theory_symbol_template with
   name       = "$less_int";
   arity      = Def(2);
   stype      = create_stype [symb_int_type;symb_int_type] symb_bool_type;
 }

let symb_lesseq_int =
  {theory_symbol_template with
   name       = "$lesseq_int";
   arity      = Def(2);
   stype      = create_stype [symb_int_type;symb_int_type] symb_bool_type;
 }

let symb_greater_int =
  {theory_symbol_template with
   name       = "$greater_int";
   arity      = Def(2);
   stype      = create_stype [symb_int_type;symb_int_type] symb_bool_type;
 }

let symb_greatereq_int =
  {theory_symbol_template with
   name       = "$greatereq_int";
   arity      = Def(2);
   stype      = create_stype [symb_int_type;symb_int_type] symb_bool_type;
 }

let symb_to_int_int =
  {theory_symbol_template with
   name       = "$to_int_int";
   arity      = Def(1);
   stype      = create_stype [symb_int_type] symb_int_type;
 }

let symb_to_rat_int =
  {theory_symbol_template with
   name       = "$to_rat_int";
   arity      = Def(1);
   stype      = create_stype [symb_int_type] symb_rat_type;
 }

let symb_to_real_int =
  {theory_symbol_template with
   name       = "$to_real_int";
   arity      = Def(1);
   stype      = create_stype [symb_int_type] symb_real_type;
 }

let symb_is_int_int =
  {theory_symbol_template with
   name       = "$is_int_int";
   arity      = Def(1);
   stype      = create_stype [symb_int_type] symb_bool_type;
 }

let symb_is_rat_int =
  {theory_symbol_template with
   name       = "$is_rat_int";
   arity      = Def(1);
   stype      = create_stype [symb_int_type] symb_bool_type;
 }


let symb_floor_int =
  {theory_symbol_template with
   name      = "$floor_int";
   arity     = Def(1);
   stype     = create_stype [symb_int_type] symb_int_type;
 }

let symb_ceiling_int =
  {theory_symbol_template with
   name      = "$ceiling_int";
   arity     = Def(1);
   stype     = create_stype [symb_int_type] symb_int_type;
 }

let symb_truncate_int =
  {theory_symbol_template with
   name      = "$truncate_int";
   arity     = Def(1);
   stype     = create_stype [symb_int_type] symb_int_type;
 }

let symb_round_int =
  {theory_symbol_template with
   name      = "$round_int";
   arity     = Def(1);
   stype     = create_stype [symb_int_type] symb_int_type;
 }



(* old *)
(* sometimes equality is defined in terms of some theory equality                      *)
(* for example in model_inst equality can be defined in terms of term algebra equality *)
(* in this case to avoid the clash the defined equality is renamed to iProver_=        *)
(* now this is done via defining a type and typed equality
   let symb_iprover_eq =
   {empty_sig_symb with
   name      = "$$iProver_=";
   arity     = Def(2);
   stype     = create_stype [symb_type_types;symb_type_types] symb_bool_type;
   sproperty = Theory;
   is_skolem = Def(false);
   }
*)

(*---------change later (variadic)-------------*)
let symb_answer =
  {theory_symbol_template with
   name      = "$$answer";
   (*!! Change to variadic/polymorphic symbols, take care since some indexies are using arity function !!*)
   arity     = Undef;
   stype     = Undef; (*create_stype [symb_type_types] symb_bool_type;*)
   sproperty = Theory;

  }


(*
  let symb_iprover_sorted_eq =
  {empty_sig_symb with
  name      = "$$iProver_sorted_equality";
  arity     = Def(3);
  stype     = create_stype [symb_type_types;symb_type_types;symb_type_types] symb_bool_type;
  sproperty = Theory;
  is_skolem = Def(false);
  }
 *)

let symb_distinct =
  {theory_symbol_template with
   name = "$distinct";
   stype = Undef; (* polymorphic *)
   arity = Def(0); (* change in future *)
  }


let symb_bot =
  {theory_symbol_template with
   name      = "$$iProver_bot";
   arity     = Def(0);
   stype     = create_stype [] symb_bot_type;
  }


(*-----is used in replacing non-eq atoms with eq -----*)
(* P(x) by "P(x) = $iProver_top" and \not P(x) by "P(x) \not = iProver_top" *)
let symb_top =
  {empty_sig_symb with
   name      = "$$iProver_top";
   arity     = Def(0);
   sproperty = FunPred;
   stype     = create_stype [] symb_bool_type;
  }


(* don't forget to add new symbols in the list!!!*)
let special_symbols_list =
  defined_types@
  [
    symb_neg;
    (*   symb_and;
         symb_or;
         symb_impl;
         symb_forall;
         symb_exists;
    *)
    symb_true;
    symb_false;
    symb_true_fun;
    symb_false_fun;
    (*  symb_equality;*)
    symb_typed_equality;
    symb_dis_equality;
    symb_ver_next_state;
    symb_ver_reachable_state;
    (* tsar *)
    symb_ver_init;
    symb_ver_property;
    symb_ver_equal;
    symb_ver_eligible;
    symb_ver_deadlock;

(* real *)
   symb_sum_real;
   symb_product_real;
   symb_difference_real;
   symb_uminus_real;
   symb_quotient_real;
   symb_less_real;
   symb_lesseq_real;
   symb_greater_real;
   symb_greatereq_real;
   symb_to_int_real;
   symb_to_rat_real;
   symb_to_real_real;
   symb_is_int_real;
   symb_is_rat_real;
   symb_floor_real;
   symb_ceiling_real;
   symb_truncate_real;
   symb_round_real;
     
(* rat *)
   symb_sum_rat;
   symb_product_rat;
   symb_difference_rat;
   symb_uminus_rat;
   symb_quotient_rat;
   symb_less_rat;
   symb_lesseq_rat;
   symb_greater_rat;
   symb_greatereq_rat;
   symb_to_int_rat;
   symb_to_rat_rat;
   symb_to_real_rat;
   symb_is_int_rat;
   symb_is_rat_rat;
   symb_floor_rat;
   symb_ceiling_rat;
   symb_truncate_rat;
   symb_round_rat;
     
(* int *)
   symb_sum_int;
   symb_difference_int;
   symb_product_int;
   symb_quotient_e_int;
   symb_remainder_e_int;
   (* symb_abs_int; *)
   symb_uminus_int;
   symb_less_int;
   symb_lesseq_int;
   symb_greater_int;
   symb_greatereq_int;
   symb_to_int_int;
   symb_to_rat_int;
   symb_to_real_int;
   symb_is_int_int;
   symb_is_rat_int;
   symb_floor_int;
   symb_ceiling_int;
   symb_truncate_int;
   symb_round_int;
 
    symb_distinct;
    symb_bot;
    (* symb_iprover_eq; *)
    (* symb_iprover_sorted_eq;*)
    symb_top;
    symb_answer
  ]


let is_sk_name str = 
  try
    match (Str.first_chars str 2) with
    |"sK" | "sk"-> true
    | _ -> false
  with
    Invalid_argument _ -> false



(* exception Symbol_fast_key_undef
exception Symbol_fast_key_is_def 
exception Symbol_db_id_is_def *)

exception Arity_undef

(* is_sig is a boolean whether the symbol is in the signature: fun or pred*)
let create_from_str ?is_sig:(is_sig=true) (name:string) (stype:stype) =
  dassert_stype_prop stype;
  if is_sig then
    {
      empty_sig_symb with
      name      =name;
      stype     =stype;
      is_skolem = (is_sk_name name);
    }
  else
    {
      empty_symb with
      name      =name;
      stype     =stype;
      is_skolem = (is_sk_name name);
    }


let get_arity_from stype =
  match stype with
   | Def type_list -> List.length type_list - 1
   | Undef -> 0



let create_from_str_type ?is_sig:(is_sig=true) (str:string) (stype:stype)  =
  dassert_stype_prop stype;
  if is_sig then
    {
      empty_sig_symb with
      name=str;
      stype=stype;
      is_skolem = (is_sk_name str);
      arity= Def(get_arity_from stype);
    }
  else
    {
      empty_symb with
      name=str;
      stype=stype;
      is_skolem = (is_sk_name str);
      arity= Def(get_arity_from stype);
    }

let create_from_str_type_property ?is_sig:(is_sig=true) (str:string) (stype:stype) (prop:sproperty) =
  dassert_stype_prop stype;
  {
    (create_from_str_type ~is_sig str stype) with
    sproperty = prop
  }


let get_num_input_occur s = s.num_input_occur
let incr_num_input_occur s = s.num_input_occur <- s.num_input_occur + 1

let cmp_num_input_occur s1 s2 = Int.compare (get_num_input_occur s1) (get_num_input_occur s2) 

(** t is a subtype of s; currently all types are incomparable except symb_type_types being a supertype of all types *)
let is_subtype t s =
  t == s || s == symb_type_types || t == symb_bot_type

let assign_group s group =
  match s.group with
  |Undef ->
    s.group <- Def(group)
  |_-> failwith "group is already assigned"

let assign_flattening s flat =
  match s.flattening with
  |Undef -> s.flattening <- Def(flat)
  |_-> failwith "flattening is already assigned"

let get_flattening s =
  match s.flattening with
  |Def(flat) -> flat
  | _-> failwith ("flattening is Undefined for "^s.name)

let assign_defined_depth i s =
  s.defined_depth <- Def(i)

let get_defined_depth s =
  s.defined_depth





(*    
let create_placeholder_symb_from_str str = 
  unimplemented()
 *)
    


(*----- fast_key -------*)
    
(* It's convenient to number type symbols separately and sequentially *)

(*    
let bit_offset_basic_types = (1 lsl 24)  (* basic types fast_keys start at 2^24 *)

let types_fast_key = ref bit_offset_basic_types
let get_num_types() = !types_fast_key - bit_offset_basic_types
   
let type_id_arr = DynArray.empty_capacity 32

(* Placeholder symbols, for theory axioms *)
let bit_offset_placeholder_syms = 2 * (1 lsl 24)
    
let placeholder_fast_key = ref bit_offset_placeholder_syms


let create_new_placeholder_symb stype = 
  dassert_stype_prop stype;
  {
   empty_symb with
   name      = sprintf "$$iProver_placeholder_%d" (!placeholder_fast_key - bit_offset_placeholder_syms);
   stype     = stype;
   sproperty = Placeholder;
   arity     = Def(get_arity_from stype);
   is_skolem = false;
 }
 *)
    
(* assigns fast key and a group which is key modulo max_num_of_sym_groups*)
(* assign_fast key is called in SymbolDB *)
(*

  let assign_fast_key (s:symbol) (key:int) = 
  dassert (fun () -> IOption.is_none s.fast_key);  (* raise Symbol_fast_key_is_def *)
  dassert (fun () -> key < bit_offset_basic_types);
  begin match s.sproperty with
  | Type -> 
    let key = IOption.some' !types_fast_key in
    s.fast_key <- key;
    incr types_fast_key;
    DynArray.push_back type_id_arr s;
  | Placeholder ->
    let key = IOption.some' !placeholder_fast_key in
    s.fast_key <- key;
    incr placeholder_fast_key;
  | _ -> 
    let key = IOption.some' key in
    s.fast_key <- key;
  end;
  assign_group s (key mod max_num_of_sym_groups)

let get_basic_type_id s = 
  dassert (fun () -> s.sproperty == Type);
  IOption.get' s.fast_key - bit_offset_basic_types

let get_basic_type_id_rev i = 
  dassert (fun () -> i < get_num_types());
  (* type_id_arr.(i) *)
  DynArray.unsafe_get type_id_arr i

 *)

    
(* KK return assign_fast_key to basic continuous 2025 *)

let assign_fast_key (s:symbol) (key:int) = 
  dassert (fun () -> IOption.is_none s.fast_key);  
  s.fast_key <- IOption.some' key;
  assign_group s (key mod max_num_of_sym_groups)


(*    
let create_from_template template new_name =  
  {
   template with
   name = new_name;
   fast_key   = IOption.none;
   db_id      = IOption.none;
 }
  *)  
(*    
let get_basic_type_id s = 
  dassert (fun () -> s.sproperty == Type);
  IOption.get' s.fast_key - bit_offset_basic_types

let get_basic_type_id_rev i = 
  dassert (fun () -> i < get_num_types());
  (* type_id_arr.(i) *)
  DynArray.unsafe_get type_id_arr i
*)

(* let assign_db_id = function
  (* Raise exception when db_id is already defined *)
  | { db_id = Def _ } ->
    (function _ -> raise Symbol_db_id_is_def)
  (* Set db_id to defined value *)
  | clause ->
    (function db_id -> clause.db_id <- Def(db_id)) *)

let assign_db_id s id = 
  (* Raise exception when db_id is already defined *)
  dassert (fun () -> IOption.is_none s.db_id);  (* Symbol_db_id_is_def *)
  (* Set db_id to defined value *)
  s.db_id <- IOption.some' id


(* checkes compatibility of basic types (for unif, matching): *)
(* for now Symbol.symb_type_types and Symbol.symb_bot_type can unify with any other types *)

let compatible_basic_types t1 t2 = 
  t1 == t2
  || t1 == symb_type_types || t2 == symb_type_types 
  || t1 == symb_bot_type   || t2 == symb_bot_type
    

(*
  exception Symbol_hash_is_def
  let assign_hash (s:symbol) (hash:int) =
  match s.hash with
  |Undef -> s.hash <- Def(hash)
  |_     -> raise Symbol_hash_is_def

 *)


let assign_property prop s =
  s.sproperty <- prop

let get_property s = s.sproperty


let assign_is_input bval s =
  set_bool_param bval is_input s
(*  s.is_input <- Def(bval)*)

let assign_is_essential_input bval s =
  set_bool_param bval is_essential_input s


let assign_is_skolem bval s =
  s.is_skolem <- bval

let assign_type stype s =
  s.stype <- stype

(*
  let assign_type_pred s =
  s.stype <- Pred
 *)


(*exception Is_input_undef*)
let is_input s =
  get_bool_param is_input s

let is_essential_input s =
  get_bool_param is_essential_input s


(*
   match s.is_input with
   |Def(bv) -> bv
   |Undef -> false
(*raise Is_input_undef*)
 *)


let is_flat s =
  match s.sproperty with
  |Flat -> true
  |_ -> false

let is_defpred s =
  match s.sproperty with
  |DefPred -> true
  |_ -> false


let is_deifinition s =
  match s.sproperty with
  |Definition -> true
  |_ -> false
  
let is_smallest s = 
  match s.sproperty with
  | Smallest -> true | _ -> false


let is_skolem s = s.is_skolem

let get_arity (s:symbol)    =
  match s.arity with
  |Def(arity)   -> arity
  |Undef        -> 
      raise  Arity_undef

(* Undef -1*)
let get_arity_def (s:symbol)    =
  match s.arity with
  |Def(arity)   -> arity
  |Undef        -> -1
          
let is_arity_def s =
  match s.arity with
  |Def _ -> true
  |Undef -> false

let assign_arity arity s =
  s.arity <- Def(arity)

let is_pred s =
  if (is_signature_symb s)
  then
    match (get_stype_args_val s) with
    |Def(args,value) ->
      value == symb_bool_type
    |_-> false
  else
    false

let is_fun s =
  (is_signature_symb s) && (not (is_pred s)) (* && (not (s.sproperty = Type)) *)

(* types can occur in typed equality as constants...*)

let is_constant s =
  (match s.arity with Def 0 -> true | _ -> false) && (is_fun s)

let is_definition s =
  s.sproperty == Definition

let is_domain_pred s =
  s.sproperty == DomainPred

let is_undef_prop s =
  s.sproperty == Undef_Prop



let get_type (s:symbol) = s.stype
(*let get_key  (s:symbol) = s.name*)


let get_fast_key (s:symbol) =
  (* dassert (fun () -> fkey <> no_fast_key);  (* Symbol_fast_key_undef *) *)
  IOption.get' s.fast_key

(*
  exception Symbol_hash_undef

  let  get_hash (s:symbol) =
  match s.hash with
  |Def(hash)   -> hash
  |Undef       -> raise  Symbol_hash_undef
 *)

exception Group_undef
let get_group s =
  match s.group with
  |Def(group) -> group
  |Undef      -> raise Group_undef


(*let  compare_key (s1:symbol)(s2:symbol)  = *)
(*(String.compare (get_key s1) (get_key s2))*)

let compare_name s1 s2 = (String.compare s1.name s2.name)


(*let compare_type s1 s2 = (compare s1.stype s2.stype)*)
(* we assume that all type symbols are already in the SymbolDB *)
(* and therefore all fast_keys are defined *)
let compare_type s1 s2 =
  match s1.stype, s2.stype with
  | Def t_list1, Def t_list2 -> 
    list_compare_lex
      (fun t1 t2 -> Int.compare (get_fast_key t1) (get_fast_key t2)) t_list1 t_list2
    (* with Symbol_fast_key_undef -> failwith "symbol.ml compare_type: Symbol_fast_key_undef " *)
  | Def _, Undef -> Ord.gt
  | Undef, Def _ -> Ord.lt
  | Undef, Undef -> Ord.eq

(*let compare_arity s1 s2 = (compare s1.arity s2.arity)*)

let compare_property s1 s2 = 
  Poly.compare s1.sproperty s2.sproperty  (* TODO: ppx_deriving? *)


(* if changed change also create_template_symb !*)
let compare_key =
  lex_combination1 compare_name (* compare_type compare_property *)

(* cannot use compare_type;compare_property since they can change after we put symbol into symbolDB *)

(*! here we neeed to have exactly the same properties as in !*)

(* can be used for checking whether a key is in db or to find a symb with this key *)
let create_template_key_symb name (*stype sproperty*) =
  {empty_symb with
   name  = name;
   (* stype = stype;
      sproperty = sproperty *)
  }

(*
  let  compare_key =
  lex_combination4 compare_name compare_arity compare_type compare_property
 *)

let compare_fast_key (s1:symbol) (s2:symbol) =
  Int.compare (get_fast_key s1) (get_fast_key s2)

let compare = compare_fast_key

let equal = (==)

(*safer but less eff.*)
(*let equal s1 s2 = if (compare s1 s2)==cequal then true else false *)

(*replaced by proper hash*)
let hash  = get_fast_key

(* unsafe compare
   let  compare (s1:t) (s2:t) =
   try (compare_fast_key s1 s2) with
   Fast_key_undef -> (compare_key s1 s2)
*)

let is_type_symb s = s.sproperty == Type

let is_placeholder_symb s = s.sproperty == Placeholder
    
(*-----------verification related test-------------------*)
let is_state_type_symb symb =
  (symb_ver_state_type == symb)

let is_address_type_symb symb =
  (symb_ver_address_type == symb)

let is_bitindex_type_symb symb =
  (symb_ver_bit_index_type == symb)

let is_address_const_symb symb =
  match (get_stype_args_val symb) with
  | Def([], val_type) ->
    (is_address_type_symb val_type)
  | _-> false

let is_a_state_pred_symb symb =
  match (get_stype_args_val symb) with
  | Def([arg_symb], val_type) ->
    ((val_type == symb_bool_type) &&
     (is_state_type_symb arg_symb))
  | _-> false


let is_a_memory_pred_symb symb =
  match (get_stype_args_val symb) with
  | Def([state_type;address_type;bitindex_type], val_type) ->
    ((val_type == symb_bool_type) &&
     (is_state_type_symb state_type) &&
     (is_address_type_symb address_type) &&
     (is_bitindex_type_symb bitindex_type)
    )
  | _-> false

let is_a_bitvec_pred_symb symb =
  match (get_stype_args_val symb) with
  | Def([state_type;bitindex_type], val_type) ->
    ((val_type == symb_bool_type) &&
     (is_state_type_symb state_type) &&
     (is_bitindex_type_symb bitindex_type)
    )
  | _-> false

let is_a_bitvec_unary_pred_symb symb =
  match (get_stype_args_val symb) with
  | Def([state_type], val_type) ->
    ((val_type == symb_bool_type) &&
     (is_state_type_symb state_type)
    )
  | _-> false

(* some skolem symbols are not introduced in iProver but come in input *)

(*
let is_sk_name str = 
  try
    match (Str.first_chars str 2) with
    |"sK" | "sk"-> true
    | _ -> false
  with
    Invalid_argument _ -> false

let is_sk_symb symb = 
  let name = get_name symb in 
  id_sk_name symb
*)

(*------ dirty hack -----*)
let is_vamp_split_sym_name str = 
  try
    match (Str.first_chars str 2) with
    |"sP" -> true
    | _ -> false
  with
    Invalid_argument _ -> false

let is_vamp_split_symbol symb = 
  is_vamp_split_sym_name (get_name symb)


(*----------------------------------------------------------------------------------------*)
(*----- only use Map/Set/Hashtbl after symbol is put into symolDB, after systemDBs.ml ----*)

module Key =
struct
  type t      = symbol
  let equal   = (==)
  let hash    = get_fast_key
  let compare = compare_fast_key
end

module Map = Map.Make(Key)

module Set = Set.Make(Key)

module Hashtbl = Hashtbl.Make(Key)

module Indexed = Indexed(Key)

type sym_set = Set.t


(*-----------------------------------*)

let name_to_tptp str = 
  if (String.length str) < 2
  then 
    str
  else
  if String.O.(String.sub str 0 2 = "$$")
  then
    String.sub str 2 ((String.length str) - 2)
  else
    str

let name_to_tptp_safe str = 
  if !global_options.tptp_safe_out 
  then 
    name_to_tptp str 
  else
    str

let to_stream stream s =
  stream.stream_add_str (name_to_tptp_safe s.name)

let out = to_stream stdout_stream

let to_string (s:symbol) = (name_to_tptp_safe s.name)

let list_to_string sl = list_to_string to_string sl ","



let stype_to_string stype = 
  match split_stype stype with 
  |Some((val_type,arg_types)) -> sprintf "%s -> %s"  (List.X.to_string to_string arg_types) (to_string val_type)
  |None -> "type_undef"
        
        
let pp_symbol ppf { name = n } = Format.fprintf ppf "%s" (name_to_tptp_safe n)

let pp_symbol_tptp ppf { name = n } =
(*
  let n' =
  if (String.length n) < 2 then
  n
  else
  if
  String.sub n 0 2 = "$$"
  then
  String.sub n 2 ((String.length n) - 2)
  else
  n
  in
  Format.fprintf ppf "%s" n'
 *)

  Format.fprintf ppf "%s" (name_to_tptp_safe n)


let rec to_stream_full s symb =
  s.stream_add_str "Name:      ";
  s.stream_add_str symb.name;
  s.stream_add_char '\n';
  s.stream_add_str "\n^^^^^^^^^^\n";
  s.stream_add_str "SType: \n";
  (match symb.stype with
   |Def (type_list) ->
     ( s.stream_add_str "[";
       list_to_stream s to_stream type_list ",";
       (* list_to_stream s to_stream_full  type_list " ";*)

       s.stream_add_str "]";
     )
   |Undef ->
     s.stream_add_str "Undef";
  );
  s.stream_add_str "\n-------------\n";
  (*  s.stream_add_str (to_string_stype symb.stype);*)

  s.stream_add_str "Arity:     ";
  s.stream_add_str (param_to_string string_of_int symb.arity);
  s.stream_add_char '\n';

  s.stream_add_str "SProperty: ";
  s.stream_add_str (to_string_sproperty symb.sproperty);
  s.stream_add_char '\n';

  s.stream_add_str ("is_sig_symb: "^(string_of_bool (is_signature_symb symb))^"\n");
  s.stream_add_str (to_string_sproperty symb.sproperty);
  s.stream_add_char '\n'

let out_full = to_stream_full stdout_stream

let to_string_full =
  to_string_fun_from_to_stream_fun 10 to_stream_full


let lower_first_char_string str = 
  let bytes = Bytes.of_string str in 
  Bytes.set bytes 0 (Char.lowercase_ascii str.[0]);
  Bytes.to_string bytes


let prolog_to_stream s symb =
  let symb_str = to_string symb in
  match symb_str.[0] with
  'a'..'z'  -> s.stream_add_str symb_str
  | 'A'..'Z'  ->
    s.stream_add_str (lower_first_char_string symb_str)

  | '0'..'9'  ->
    s.stream_add_char 'f';
    s.stream_add_str symb_str
  |_ -> failwith "Symbol.to_prolog: unexpected char in symb_name"


let to_prolog symb =
  let symb_str = to_string symb in
  match symb_str.[0] with
  'a'..'z'  -> symb_str
  | 'A'..'Z'  ->
    lower_first_char_string symb_str

  | '0'..'9'  -> "f"^symb_str
  |_ -> failwith "Symbol.to_prolog: unexpected char in symb_name"





(**********************)
(* Symbol precedences *)
(**********************)


let xorshift64 x = 
  let rnd_bits = get_rnd_bits () in
  let x = x + rnd_bits in
  let x = x lxor (x lsl 13) in
  let x = x lxor (x lsr 7) in
  let x = x lxor (x lsl 17) in
  x

let xorshift32 x = 
  let rnd_bits = get_rnd_bits () in
  let x = x * rnd_bits in
  let x = x lxor (x lsl 13) in
  let x = x land 0xFFFFFFFF in
  let x = x lxor (x lsr 17) in
  let x = x lxor (x lsl 15) in
  let x = x land 0xFFFFFFFF in
  x

let randomise = xorshift32

(* Primitives: *)

(* Definitions < rest *)
let cmp_definitions = 
  Ord.reverse_f @@ Ord.lift is_definition Bool.compare
  
let cmp_smallest = 
  Ord.reverse_f @@ Ord.lift is_smallest Bool.compare
  
let cmp_fst = lex_combination2 cmp_definitions cmp_smallest

(* Higher arity > lower arity *)
let cmp_arity = 
  Ord.lift get_arity_def Int.compare

(* Compare by fast_key *)
let cmp_fast_key = 
  compare

(* Random (but consistent) order *)
let cmp_random = 
  Ord.lift (fun s -> randomise (get_fast_key s)) Int.compare

(* Comparisons: *)

(* Arity, then fast_key *)
let cmp_symb_arity = lex_combination3
  cmp_fst cmp_arity cmp_fast_key

(* Arity, then reverse fast_key *)
let cmp_symb_arity_rev = lex_combination3
  cmp_fst cmp_arity (Ord.reverse_f cmp_fast_key)

(* Arity, then random *)
let cmp_symb_arity_random = lex_combination3
  cmp_fst cmp_arity cmp_random

(* Totally random *)
let cmp_symb_random = lex_combination2
  cmp_fst cmp_random

let cmp_freq = lex_combination3 
  cmp_fst cmp_num_input_occur cmp_fast_key

let cmp_invfreq = lex_combination3
  cmp_fst (Ord.reverse_f cmp_num_input_occur) cmp_fast_key

let cmp_invfreq_arity = lex_combination4
  cmp_fst (Ord.reverse_f cmp_num_input_occur) cmp_arity cmp_fast_key

let cmp_invfreq_invarity = lex_combination4
  cmp_fst (Ord.reverse_f cmp_num_input_occur) (Ord.reverse_f cmp_arity) cmp_fast_key


(* let symb_ordering_option_to_func x = 
  match x with
  | Options.Ordering.Symb.Invfreq         -> cmp_invfreq
  | Options.Ordering.Symb.InvfreqArity    -> cmp_invfreq_arity
  | Options.Ordering.Symb.InvfreqInvArity -> cmp_invfreq_invarity
  | Options.Ordering.Symb.Arity       -> cmp_symb_arity
  | Options.Ordering.Symb.ArityRev    -> cmp_symb_arity_rev
  | Options.Ordering.Symb.ArityRandom -> cmp_symb_arity_random
  | Options.Ordering.Symb.Random -> cmp_symb_random
  | Options.Ordering.Symb.Custom l -> 
    let n = - SymbolDB.size !symbol_db_ref - get_num_types () in
    let arr = Array.make n (-1) in
    l |> List.iteri (fun i x -> arr.(Symbol.get_fast_key x) <- i);
    fun x y -> Int.compare (arr.(Symbol.get_fast_key x)) (arr.(Symbol.get_fast_key y))
  
let term_weight_option_to_func x = 
  match x with
  | Options.Ordering.Weight.Default -> Term.get_num_of_symb
  | Options.Ordering.Weight.Custom {wvar;wsym} -> 
    let wsym = 
      let map = 
        List.fold_left (fun map (s,w) ->
          SMap.add (find_symbol s) w map
        ) SMap.empty wsym
      in
      fun x -> map |> SMap.find x
    in
    wvar, wsym
 *)
