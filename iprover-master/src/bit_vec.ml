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





type bit_vec = int

(* in order better type check bit vec is bool vec*)
(* impl: 0 is false 1 is true*)

let false_vec = 0
let true_vec  = lnot 0
    
let max_pos = 30

let unsafe_set (b: bool) (i: int) (v: bit_vec) =
  if b then 
    (*lsl -- shift 1 by i bits*)
    (1 lsl i) lor v 
  else 
    (lnot (1 lsl i)) land v

let set (b:bool) (i: int) (v: bit_vec) = 
  if i <= max_pos && i >= 0 then 
    unsafe_set b i v
  else
    failwith "bit_vec: trying to access over the range of bit_vec"

let unsafe_get (i: int) (v: bit_vec) =
  (* lsr shift right*)     
  if ((v lsr i) mod 2) = 0 then
    false 
  else
    true

let get (i: int) (v: bit_vec) =
  if i <= max_pos && i >= 0 then
    unsafe_get i v
  else
    failwith "bit_vec: trying to access over the range of bit_vec"

let to_int = Fun.id

let output ?width out_channel x = 
  match width with
  | Some n  -> 
    for i = n-1 downto 0 do
      let v = unsafe_get i x in
      output_char out_channel (Char.chr (Char.code '0' + Bool.to_int v))
    done
  | None -> 
    let printing = ref false in
    for i = max_pos downto 0 do
      let v = unsafe_get i x in
      printing := v;
      if !printing then output_char out_channel (Char.chr (Char.code '0' + Bool.to_int v))
    done
