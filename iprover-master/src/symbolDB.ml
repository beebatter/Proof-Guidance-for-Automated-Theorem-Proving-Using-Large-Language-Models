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

(*----- debug modifiable part-----*)

let dbg_flag = true

type dbg_gr = 
  | D_trace
      
let dbg_gr_to_str = function 
  | D_trace -> "trace"
        
let dbg_groups = [
  D_trace;
]
    
let module_name = __MODULE__

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)

  
type symbol = Symbol.symbol
type stype  = Symbol.stype
      
module SymKey = 
  struct 
    type t       = symbol
    let compare = Symbol.compare_key
    let assign_fast_key = Symbol.assign_fast_key
    let assign_db_id = Symbol.assign_db_id
 end
    
module SymbolDBM =  AbstAssignDB.Make (SymKey)

module SIndexed = Symbol.Indexed
    
(* module Indexed *)

type symbolDB  = 
    {db                       : SymbolDBM.abstDB;
(*     mutable unused_def_symb_number : int; *)
     mutable unused_fresh_symb_number : int; 
     mutable symb_id_rnd_bound : int;
     mutable types_indexed : SIndexed.t; (* numbering of types *)
     mutable placeholders_indexed : SIndexed.t; (* numbering of placeholders *)
   }

    
let get_name db = SymbolDBM.get_name db.db

let mem sym db = SymbolDBM.mem sym db.db 

let remove symb db     =  { db with db = (SymbolDBM.remove symb db.db)}
let find symb db       = SymbolDBM.find symb db.db
let size db            = SymbolDBM.size db.db
let map f db           = { db with db = (SymbolDBM.map f db.db)}
let fold f db a        = SymbolDBM.fold f db.db a
let iter f db          = SymbolDBM.iter f db.db

let to_list db = fold List.cons db []
  

let add_indexed symb_db symb =
  if (Symbol.is_type_symb symb) then
    (
     dbg D_trace @@ lazy (sprintf "add_indexed:types_indexed: %s" (Symbol.to_string symb));
     symb_db.types_indexed <- SIndexed.add symb symb_db.types_indexed
    )
  else (
    if (Symbol.is_placeholder_symb symb) then
      (
       dbg D_trace @@ lazy (sprintf "add_indexed:placeholders_indexed: %s" (Symbol.to_string symb));
       symb_db.placeholders_indexed <- SIndexed.add symb symb_db.placeholders_indexed
      )
   )
    
let add_ref symb db_ref =
  let sdb_ref   = ref !db_ref.db in
  let added_symb = SymbolDBM.add_ref symb sdb_ref in
  add_indexed !db_ref added_symb;
  db_ref:= {!db_ref with
            db = !sdb_ref};
  added_symb
    
let add symb db  = 
(*  Symbol.assign_hash symb (Random.bits());*)
  let db_ref = ref db in
  let _added_symb = add_ref symb db_ref in
  !db_ref
      
(*
let create_name name = 
  let sdb_ref = ref (SymbolDBM.create_name name) in
(* add all special symbols to db *)
  List.iter 
    (fun symb -> 
      (let added_symb = SymbolDBM.add_ref symb sdb_ref in 
      Symbol.set_is_special_symb true added_symb)) 
    Symbol.special_symbols_list;
  {
   db = !sdb_ref;
   unused_fresh_symb_number=0;
   symb_id_rnd_bound = 10;
   types_indexed = SIndexed.create ();
   placeholders_indexed = SIndexed.create ();
 }
 *)
    
let create_name name = 
  let sdb_ref = ref (SymbolDBM.create_name name) in
  let db = {
    db = !sdb_ref;
    unused_fresh_symb_number=0;
    symb_id_rnd_bound = 10;
    types_indexed = SIndexed.create ();
    placeholders_indexed = SIndexed.create ();
  } in
  let db_ref = ref db in
  
(* add all special symbols to db *)
  List.iter 
    (fun symb -> 
      (let added_symb = add_ref symb db_ref in 
      Symbol.set_is_special_symb true added_symb)) 
    Symbol.special_symbols_list;
  !db_ref
    
let create ()   = 
  create_name "Symbol_DB"
    
(* Create a fresh def symbol and add to the database 

   TODO ?: 
   Follow the TPTP convention for new names, that is, create the
   symbol as sP{n}_iProver_def.
 *)
    
let create_fresh_symb symb_db_ref ?(property=Symbol.Undef_Prop) stype name_pref =
(* Name of symbol conforming to the TPTP convention for new names *)
  let new_symb id = 
    let symb_name = Format.sprintf "%s_%d" name_pref id in
    Symbol.create_from_str_type_property symb_name stype property
  in
  let new_symb_seq = new_symb !symb_db_ref.unused_fresh_symb_number in
  !symb_db_ref.unused_fresh_symb_number <- 
    succ !symb_db_ref.unused_fresh_symb_number;
  let symb_ref = ref new_symb_seq in
  while (mem !symb_ref !symb_db_ref) do    
    let new_id = Random.int !symb_db_ref.symb_id_rnd_bound in
    (if !symb_ref != new_symb_seq then (* rnd not the first time *)
      !symb_db_ref.symb_id_rnd_bound <- 2*(!symb_db_ref.symb_id_rnd_bound)
    );
    symb_ref := new_symb new_id;
    !symb_db_ref.unused_fresh_symb_number <- 
      succ new_id;
  done;
  
  Statistics.incr_int_stat 1 Statistics.num_of_fresh_symb;
  (* Increment counter for def symbols *)
  Symbol.assign_is_essential_input true !symb_ref; 
  (* Add symbol to symbol database *)
  add_ref !symb_ref symb_db_ref 


(* ---- *)        
let create_new_skolem symb_db_ref stype =
 create_fresh_symb symb_db_ref stype "iPr_Sk"
    

(* ---- *)        
let create_new_placeholder symb_db_ref stype =
  create_fresh_symb symb_db_ref ~property:Symbol.Placeholder stype "iPr_plc"
    
(* ---- *)    
let create_new_def_symb symb_db_ref stype = 
  create_fresh_symb symb_db_ref ~property:Symbol.Definition stype "iPr_def"


(* ---- *)
let get_basic_type_id symb_db s = 
(*  dassert (fun () -> Symbol.is_type_symb s); *)
  SIndexed.find_ind s symb_db.types_indexed
    
let get_basic_type_id_rev symb_db id =
(*  dassert (fun () -> id < (SIndexed.size symb_db.types_indexed)); *)
  SIndexed.find_el id symb_db.types_indexed

let get_num_types symb_db = SIndexed.size symb_db.types_indexed
    
(* ---- *)
let get_placeholder_id symb_db s = 
(*  dassert (fun () -> Symbol.is_placeholder_symb s); *)
  SIndexed.find_ind s symb_db.placeholders_indexed
    
let get_placeholder_id_rev symb_db id =
(*  dassert (fun () -> id < (SIndexed.size symb_db.types_indexed)); *)
  SIndexed.find_el id symb_db.placeholders_indexed
    
let get_num_placeholders symb_db = SIndexed.size symb_db.placeholders_indexed
    
(* ---- *)
let get_num_of_sym_groups db = 
  let size_db = size db in
  if Symbol.max_num_of_sym_groups > size_db then 
    size_db 
  else 
    Symbol.max_num_of_sym_groups

(*------------------------------------------*)
let to_stream s symbol_db = 
  SymbolDBM.to_stream s Symbol.to_stream "\n" symbol_db.db

let to_stream_full s symbol_db = 
  SymbolDBM.to_stream s Symbol.to_stream_full "\n" symbol_db.db

let symb_to_stream_full_filter f s symb = 
 if f symb 
 then 
   Symbol.to_stream_full s symb 
 else 
   ()

let to_stream_full_filter f s symbol_db = 
  SymbolDBM.to_stream s (symb_to_stream_full_filter f) "\n" symbol_db.db


let out = to_stream stdout_stream

let out_full = to_stream_full stdout_stream

let out_full_filter f = to_stream_full_filter f stdout_stream

let to_string symbol_db = 
  SymbolDBM.to_string Symbol.to_stream "\n" symbol_db.db



(*debug*)
let get_greatest_key db = SymbolDBM.get_greatest_key db.db
 
