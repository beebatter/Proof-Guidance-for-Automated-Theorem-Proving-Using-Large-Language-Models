(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2017 Konstantin Korovin and The University of Manchester. 
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
open Logic_interface 

type def_merge_opts =
    {
     def_merge_prop_impl  : bool;
     def_merge_mbd        : bool;
     def_merge_tr_red     : bool;
     def_merge_tr_cl      : bool;
   }


val prep_opts_to_def_mege_opts : Options.options -> def_merge_opts

val def_merge : def_merge_opts -> clause list -> clause list
