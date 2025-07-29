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


exception Time_out_real
exception Time_out_virtual

(** sets exceptions for termination sygnals *)

val set_sys_signals : unit -> unit

(**  Sets sytem signals to timeouts 
     if negative then do not set; 
     if 0 raise Time_out_real or Time_out_virtual
*)
val set_time_out : time_out_real:float -> time_out_virtual:float -> unit

(** 
   removes timeouts 
*)
val disable_time_outs : unit -> unit 
