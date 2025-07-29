(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2021 Konstantin Korovin and The University of Manchester. 
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



let dbg_global_flag = false

let dbg_color_out   = false
let warnings_flag   = false

(* Assert that only gets enabled in debug builds dbg_global_flag=true and (make clean; make debug=true).
   Zero cost otherwise *)
(* use --dbg_backtrace true to record the backtrace for debug; dassert_global_flag=true for full backtrace *)
let dassert_global_flag = true (* dbg_global_flag *)

(* Make comparison operators monomorphic, make polymorphic comparison available through [Poly]. *)

(* These are the usual comparison operators: polymorphic, compare via 
   runtime structural equality *)
module Poly = struct
  let (=)  : 'a -> 'a -> bool = (=)
  let (<>) : 'a -> 'a -> bool = (<>)
  let (<)  : 'a -> 'a -> bool = (<)
  let (>)  : 'a -> 'a -> bool = (>)
  let (<=) : 'a -> 'a -> bool = (<=)
  let (>=) : 'a -> 'a -> bool = (>=)

  let compare : 'a -> 'a -> int  = compare
  let min : 'a -> 'a -> 'a  = min
  let max : 'a -> 'a -> 'a  = max
end

(* The unqualified operators are now monomorphic for ints. *)
let (=)  : int -> int -> bool = (=)
let (<>) : int -> int -> bool = (<>)
let (<)  : int -> int -> bool = (<)
let (>)  : int -> int -> bool = (>)
let (<=) : int -> int -> bool = (<=)
let (>=) : int -> int -> bool = (>=)

let compare : int -> int -> int  = compare
let min : int -> int -> int  = min
let max : int -> int -> int  = max



(* ********************************************** *)
(* Improved implementations of critical functions *)

(* This becomes a no-op *)
let bool_to_int (b: bool) = 
  (Obj.magic b : int)

(* Test *)
let () = 
  if dbg_global_flag then assert (bool_to_int true = 1 && bool_to_int false = 0)

(* 50-70% speedup (i.e. takes 0.6 to 0.65x of the time) *)
(* It's called millions of times *)
let int_compare_fast (a: int) (b: int) = 
  bool_to_int (a>b) - bool_to_int (a<b)

(* We choose this version *)
let compare = int_compare_fast

(* 3.5x speedup (but almost never used in the code) *)
let min a b = if a<=b then a else b
let max a b = if a>=b then a else b
  
(* 100% speedup (2x) *)
(* Also called very often *)
let bool_compare_fast (a: bool) (b: bool) = 
  bool_to_int a - bool_to_int b


(* cmp that returns just 0 *)
let cmp_const_eq c1 c2 = 0


(* Exceptions *)
(* [raise_notrace] is simply a jump to a register address, while [raise] 
   records a stack trace. This is useful for debugging, but otherwise it's 
   unnecessary work. In the case of "control flow exceptions", it's 
   completelely pointless. *)
(* 50% speedup or more, depending on depth *)
(* called millions of times *)
let fast_raise_flag = not dbg_global_flag

let raise = if fast_raise_flag then raise_notrace else raise
let invalid_arg = if fast_raise_flag then (fun x -> raise_notrace (Invalid_argument x)) else invalid_arg
let failwith = if fast_raise_flag then (fun x -> raise_notrace (Failure x)) else failwith
let undefined () = failwith "undefined"
let unimplemented () = failwith "unimplemented"

(* ********************************************** *)



(* For clarity, these are also available through the [Int] module, so that 
   like we write [Bool.compare] or [String.compare] or [Term.compare], we 
   can also write Int.compare if it makes it clearer. *)
module Int = struct
  include Int
  let compare = int_compare_fast
  let min = min
  let max = max
  module O = struct
    let (=)  : int -> int -> bool = (=)
    let (<>) : int -> int -> bool = (<>)
    let (<)  : int -> int -> bool = (<)
    let (>)  : int -> int -> bool = (>)
    let (<=) : int -> int -> bool = (<=)
    let (>=) : int -> int -> bool = (>=)
  end
end



(*  question_answering_mode_ref = true if input has $answer *)

(* unsatisfiable ground possibly with assumptions *)
exception Unsatisfiable_gr 

(* unsatisfiable ground without assumtions; *)
(* solvers should NOT be used after Unsatisfiable_gr_na other than to get proof *)
exception Unsatisfiable_gr_na 
exception SZS_Unknown
exception Eliminated



let answer_mode_ref = ref false

(*-----------------*)
(* when prover is run in incomplete mode. *)
let sat_incomplete_mode   = ref false  
let unsat_incomplete_mode = ref false 

(*-----------------*)

let iprover_start_time = (Unix.gettimeofday ())

let iprover_running_time () =
  (Unix.gettimeofday ()) -. iprover_start_time





(* ------ *)
(* Output *)
(* ------ *)

type 'a printer =
  out_channel -> 'a -> unit

let printf = Printf.printf
let eprintf = Printf.eprintf
let fprintf = Printf.fprintf
let sprintf = Printf.sprintf



let tptp_safe_out_ref = ref false

let tptp_safe_str str = 
  if !tptp_safe_out_ref 
  then
    Printf.sprintf "%% %s" str
  else
    str

let out_str str = 
  if !tptp_safe_out_ref 
  then
    Printf.printf "%% %s\n" str
  else
    print_endline str

let out_err_str str = 
  if !tptp_safe_out_ref 
  then 
    Printf.eprintf "%% %s\n" str
  else
    prerr_endline str
    
(*let out_str_debug s =
  if debug then out_str s else ()*)

let out_stdout f = 
  if !tptp_safe_out_ref then (
    print_string "% "
  );
  f()

let out_stderr f = 
  if !tptp_safe_out_ref then (
    prerr_string "% "
  );
  f()

(* Lifts *)
let output_tptp print_el out x =
  if !tptp_safe_out_ref then (
    output_string out "% "
  );
  print_el out x

let out_warning s = 
  if warnings_flag then out_str (Printf.sprintf "warning: %s" s)



let printf_tptp fmt = 
  if !tptp_safe_out_ref then (
    print_string "% "
  );
  printf fmt

let eprintf_tptp fmt = 
  if !tptp_safe_out_ref then (
    prerr_string "% "
  );
  eprintf fmt

let fprintf_tptp out fmt = 
  if !tptp_safe_out_ref then (
    output_string out "% "
  );
  fprintf out fmt

let sprintf_tptp fmt = 
  if !tptp_safe_out_ref then
    let str : string = sprintf fmt in "% "^str
  else
    sprintf fmt

let addressof ?bits x = 
  match bits with
  | None -> Obj.magic x * 2
  | Some n -> (Obj.magic x * 2) mod (1 lsl n)



let tap f x = f x; x

(*--------- Lazy debug ------------*)
    
let _ = out_warning (sprintf "lib.ml: dbg_global_flag=%s" (string_of_bool dbg_global_flag))

let dbg_getenv_flag = 
  match Sys.getenv_opt "IPROVERDBG" with Some "false" | Some "off" -> false | _ -> true

let is_dbg dbg_flag group_list group = 
  dbg_global_flag 
  && dbg_flag 
  && dbg_getenv_flag
  && List.exists (fun x -> x == group) group_list

let dbg_out_pref dbg_flag group_list group group_to_str pref_str str_lazy = 
  if is_dbg dbg_flag group_list group then
    if dbg_color_out then
      out_str (sprintf "\x1b[1m\x1b[35m%s:\x1b[34m%s:\x1b[0m %s" (pref_str) (group_to_str group) (Lazy.force str_lazy))
    else
      out_str (sprintf "%s:%s: %s" (pref_str) (group_to_str group) (Lazy.force str_lazy))
    (* out_str (sprintf "%s:%s:%d: %s" (pref_str) (group_to_str group) (__LINE__) (Lazy.force str_lazy)) *)
    (* out_str (sprintf "%s:%s:%d: %s" (pref_str) (group_to_str group) (fst @@ __LINE_OF__ str_lazy) (Lazy.force str_lazy)) *)

let dbg_flag_msg dbg_flag module_name = 
  if dbg_flag then 
    out_warning (sprintf "%s: dbg_flag=true" module_name)

(* out_dbg with prefix str *)
(*
let out_dbg_pref group_list group pref_str str_lazy = 
  out_dbg group_list group (lazy (pref_str));
  out_dbg group_list group str_lazy
*)

(*---------------------------*)

let dbg_env_set dbg_flag group_list group f = 
  if (is_dbg dbg_flag group_list group) 
  then
    (
     f();
    )
  else ()

let _ = if dassert_global_flag then out_warning (sprintf "lib.ml: dassert_global_flag=%b" dassert_global_flag)

let[@inline] dassert x = 
  if dassert_global_flag then
    assert (x())
  else
    ()

let raise_trace x = 
  if dassert_global_flag then
    let raw_backtrace = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace x raw_backtrace
  else
    raise x

(* Example of usage: for each module one needs to add *)
(* (from prop_solver_exchange.ml)

(*----- debug modifiable part-----*)

let dbg_flag = true

type dbg_gr = 
  | D_gr_term 
  | D_add_clause

let dbg_gr_to_str = function 
  | D_gr_term -> "gr_term"
  | D_add_clause -> "add_clause"
	
let dbg_groups =
  [
   D_gr_term;
   D_add_clause
 ]
    
let module_name = "prop_solver_ex"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
  
*)

(* Example: dbg_env

(* debug *)
 
  dbg_env D_gr_term 
    (fun () ->

      dbg D_gr_term  
	(lazy ("---------Terms for grounding-----"));
      let f stype gr_term =
	
	let num_of_occ = Symbol.get_num_input_occur (Term.get_top_symb gr_term)
	in
	dbg D_gr_term  
	  (lazy ("Term for grounding type: "
		 ^(Symbol.to_string stype)^" term: "^(Term.to_string gr_term)
		 ^" num of occ: "^(string_of_int num_of_occ)^"\n"));
      in	
      SMap.iter f !gr_by_map;    

    )
 
 *) 







(*--------------iProver version/Header--------------*)

let iprover_name_str = "iProver"

(* version is a list of integers *)
(*let iprover_current_version = [1;0]*)

(* let iprover_current_version = [3;4]  *)

let iprover_current_version = [3;9;3]

let rec iprover_version_to_str v = 
  match v with 
  |[i] -> (string_of_int i)
  |[] -> ""
  |h::rest -> (string_of_int h)^"."^(iprover_version_to_str rest)

let iprover_version_str  = "v"^(iprover_version_to_str iprover_current_version)

(*let iprover_add_info = "(Post CASC-22)"*)
(*let iprover_add_info = "(post CASC-J5 2010)"*)

(* let iprover_add_info = "(CASC-24 2013 r3)" *)

(* let iprover_add_info = "(CASC-J7 2014)"  *)

(*  let iprover_add_info = "(post CASC-27 2019) integrated Z3" *) (* "(post CASC-26 2017)" *)

let iprover_add_info = "(pre CASC 2025/SMT-COMP 2025)"

let pref_str_head = "\n%---------------- " 

let suff_str_head = " ----------------%\n" 
    
let dash_str str = 
  sprintf "%s%s%s" pref_str_head str suff_str_head

let head_str () =  
  sprintf "%s%s %s %s%s"
    pref_str_head
    iprover_name_str
    iprover_version_str
    iprover_add_info
    suff_str_head

let _ = out_str (head_str ())

(*----------------------*)
let param_str_ref = ref ""
let pref_str = "------ "

let s_pref_str () =
  if !tptp_safe_out_ref then
    "% ------ "
  else
    "------ "

(*--------------end iProver version/Header--------------*)

(* gets path to the iprover executable if defined by /proc/self/exe *)
(* else raises Not_found *)


let iprover_exe_name () = 
  let args = Sys.argv in
  args.(0) 
    
let iprover_exe_path () = 
  Filename.dirname (iprover_exe_name ())
    

(*
  let program_path = 
  let cmd_link = "/proc/self/exe" in
  if (Sys.file_exists cmd_link) 
  then
  Filename.dirname (Unix.readlink cmd_link)
  else
  "./"
 *)

(* switch on for debug mode*)
(*let debug = true*)
let debug = true

let solve_num_deb = ref 0 
let solve_pass_empty = ref 0 

let string_of_char = String.make 1

(* truncates float to n digits after . *)
let truncate_n n f = 
  let fl_n =  (10.**(float_of_int n)) in
  (float_of_int (truncate (f*.fl_n)))/.fl_n

let[@inline] (@=) r f = 
  r := f !r



(*---------Memory control---------*)

(* let _ = out_warning (" lib: Gc: changed  space_overhead 50 for qbf ") *)

let mem_control_init () =
  let old_controls = Gc.get () in
  let new_controls = {
    old_controls with
    Gc.minor_heap_size = 64 * 1024 * 1024 * 8 / Sys.word_size; (* 64MB *) 
    (* Gc.minor_heap_size = 16 * 1024 * 1024 * 8 / Sys.word_size; (* 16MB *)  *)
    (* Gc.minor_heap_size = 4 * 1024 * 1024 * 8 / Sys.word_size; (* 4MB *) *)
    Gc.major_heap_increment = 512 * 1024 * 1024 * 8 / Sys.word_size; (* 512MB *) 
    (* Gc.major_heap_increment = 64 * 1024 * 1024 * 8 / Sys.word_size; (* 64MB *)  *)
    (* Gc.major_heap_increment = 8 * 1024 * 1024 * 8 / Sys.word_size; (* 8MB *) *)
    (* Gc.max_overhead = 1000;*)
    Gc.space_overhead = 200;
    (* Gc.space_overhead = 400; *)
    (* Gc.space_overhead = 400; *) (* normal before qbf*)
    (* Gc.space_overhead = 400;*)
  }
  in
  Gc.set new_controls


(*
  let mem_control_init () = 
  let old_controls = Gc.get () in
  let new_controls = {old_controls with Gc.major_heap_increment= 1048576} in
  Gc.set new_controls
 *)

(*    
let _ =  mem_control_init ()
*)
let clear_memory () = ()


(* first mbytes second kbites *)
type mem_size = int * int 

(* get mem in bytes *)
let get_mem_bytes () =
  let stat = Gc.stat () and control = Gc.get () in
  let max_words_total = stat.Gc.heap_words + control.Gc.minor_heap_size in
  let bytes = (max_words_total * ( Sys.word_size / 8) ) in
  bytes

let get_live_mem_bytes () =
  let stat = Gc.stat () and control = Gc.get () in
  let max_words_total = stat.Gc.live_words + control.Gc.minor_heap_size in
  let bytes = (max_words_total * ( Sys.word_size / 8) ) in
  bytes

let bytes_to_mem_size bytes = 
  let kbytes = (bytes / 1024) in
  let mbytes = (kbytes / 1024) in
  (mbytes, (kbytes - mbytes * 1024))

let print_memory_usage () =
  let (mbytes,kbytes) = bytes_to_mem_size (get_mem_bytes ()) in
  Printf.fprintf stderr "Allocated memory:\t%d Mbytes %d kBytes\n"
    mbytes kbytes;
  flush stderr

let print_live_memory_usage () =
  let (mbytes,kbytes) = bytes_to_mem_size (get_live_mem_bytes ()) in
  Printf.fprintf stderr "Allocated live memory:\t%d Mbytes %d kBytes\n"
    mbytes kbytes;
  flush stderr

(* 
   let print_memory_usage () =
   let stat = Gc.stat () and control = Gc.get () in
   (* out_str ("space_overhead="^(string_of_int control.Gc.space_overhead)^"\n");*)
   let max_words_total = stat.Gc.heap_words + control.Gc.minor_heap_size in
   let bytes = (max_words_total * ( Sys.word_size / 8) ) in
   let kbytes = (bytes / 1024) in
   let mbytes = (kbytes / 1024) in
   Printf.fprintf stderr "Allocated memory:\t%d Mbytes %d kBytes\n"
   mbytes (kbytes - mbytes * 1024);
   flush stderr
 *)


(* fun is a function unit -> unit, get_time_fun returns time taken by fun  *)
(* truncated by tranc digits after . *)
let get_time_fun trunc f  =
  let start_time = Unix.gettimeofday () in
  f ();
  let end_time   = Unix.gettimeofday () in 
  truncate_n trunc (end_time -. start_time)

let get_time_arg_res_fun trunc f a =
  let start_time = Unix.gettimeofday () in
  let res = f a in
  let end_time   = Unix.gettimeofday () in 
  let time = truncate_n trunc (end_time -. start_time) in
  (res,time)


(* Gc.full full_major is applied, can be time consuming *)
(* *)
let print_mem_time_fun f a = 
  Gc.full_major ();
  let before_bytes = get_mem_bytes () in
  let (res,time) = get_time_arg_res_fun 3 f a in
  Gc.full_major ();
  let after_bytes = get_mem_bytes () in
  let diff_bytes = after_bytes - before_bytes in
  let (before_mbytes,before_kbytes) = bytes_to_mem_size before_bytes in
  let (after_mbytes,after_kbytes) = bytes_to_mem_size after_bytes in
  let (diff_mbytes,diff_kbytes) = bytes_to_mem_size diff_bytes in
  out_str (s_pref_str ());
  out_str 
    ("Mem before: "
     ^(string_of_int before_mbytes)
     ^ " Mbytes "
     ^(string_of_int before_kbytes) 
     ^" kBytes\n"
     ^"Mem after: "
     ^(string_of_int after_mbytes)
     ^ " Mbytes "
     ^(string_of_int after_kbytes) 
     ^" kBytes\n"
     ^"Mem incr: "	 
     ^(string_of_int diff_mbytes)
     ^ " Mbytes "
     ^(string_of_int diff_kbytes) 
     ^" kBytes\n");
  out_str ("Used Time: "^(string_of_float time));
  res


(*-------can be used to test memory usage running the same function n times-------*)
(*-------printing memory statistics-----------------------------------------------*)

let mem_test fun_to_test n = 
  for i=1 to n do 
    (* Gc.full_major ();
  	print_live_memory_usage (); 
  	print_memory_usage (); *)
    ignore (print_mem_time_fun fun_to_test ()) 
  done
  

(*----------objsize----------*)
let print_objsize name obj =
  let info = Objsize.objsize obj in
  let (mb_without_headers,kb_without_headers) = bytes_to_mem_size (Objsize.size_without_headers info) in
  let (mb_with_headers,kb_with_headers) = bytes_to_mem_size (Objsize.size_with_headers info) in
  out_str 
    ("objsize: "^name^": "^ 
     "without_headers: "^(string_of_int mb_without_headers)^" MB "^
     "with_headers: "^(string_of_int mb_with_headers)^ " MB \n")
    

(*-------------------------*)

exception Termination_Signal




(*----------Global Open Child Processes--------------*)

let child_processes_list_ref = ref []



(* processed opend by Unix.open_process_full, are closed by channels *)
(* (in_channel,out_channel,error_channel) list *)

let child_channels_list_ref = ref []

let add_child_process pid = 
  child_processes_list_ref:= pid::!child_processes_list_ref

let add_child_process_channels chs = 
  child_channels_list_ref:= chs::!child_channels_list_ref

let kill_child_process_channels chs = 
  ignore (Unix.close_process_full chs)

let kill_process_group pid = 
  try                         
    (* Kill processes in process group *)
    Unix.kill (-pid) Sys.sigkill;                             
    ignore(Unix.waitpid [] pid)
  with 
    Unix.Unix_error(Unix.ESRCH, _, _) -> ()

let remove_top_child_process () = 
  match !child_processes_list_ref with 
  |[] -> ()
  |h::tl ->
      kill_process_group h;
      child_processes_list_ref:= tl

let remove_top_child_process_channels () = 
  match !child_channels_list_ref with 
  |[] -> ()
  |h::tl ->      
      child_channels_list_ref:= tl


let kill_all_child_processes () = 
  List.iter kill_process_group !child_processes_list_ref;
  List.iter kill_child_process_channels !child_channels_list_ref





(* ------------ *)
(* Total orders *)
(* ------------ *)

(* TODO benchmark *)
(* let int_compare (x : int) y =
  if x > y then 1
  else if y > x then -1
  else 0 *)

module Ord = struct
  (* outcome of compare fun *)
  type t = int

  let eq =  0
  let gt =  1
  let lt = -1

  let to_string x = 
    match x with
    |  0 -> "eq"
    |  1 -> "gt"
    | -1 -> "lt"
    | _ -> invalid_arg "Ord.to_string"

  let to_sign x = 
    match x with
    |  0 -> "="
    |  1 -> ">"
    | -1 -> "<"
    | _ -> invalid_arg "Ord.to_sign"

  let reverse x = 
    -x

  let lift key cmp_keys =
    fun x y -> cmp_keys (key x) (key y)

  let equal = Int.equal
  module O = struct
    let (=)  = (==)
    let (<>) = (!=)
  end

  let reverse_f f = 
    fun x y -> f y x
end

(* outcome of compare fun *)
let cequal   = Ord.eq
let cgreater = Ord.gt
let cless    = Ord.lt

module type Ordered = Map.OrderedType

let min_cmp cmp a b = if cmp a b == Ord.lt then a else b
let max_cmp cmp a b = if cmp a b == Ord.gt then a else b
      
module OrdMakeInfix (M: Ordered) = struct
  let (>) a b =
    M.compare a b > 0

  let (>=) a b =
    M.compare a b >= 0

  let (<) a b =
    M.compare a b < 0

  let (<=) a b =
    M.compare a b <= 0
end

module type HasEquality = sig 
  type t 
  val equal : t -> t -> bool
end

module EqMakeInfix (M: HasEquality) = struct
  let (=) = M.equal
  let (<>) a b = not (M.equal a b)
end



(* -------------- *)
(* Partial orders *)
(* -------------- *)

module PartialOrd = struct

  (* Result of a partial order *)
  type t = 
    | EQ 
    | GT
    | LT
    (* | GE *)
    (* | LE *)
    | INC

  (* Helper functions *)
  let to_string = function 
    | EQ -> "EQ" 
    | GT -> "GT"
    | LT -> "LT"
    | INC -> "INC"

  let to_sign = function 
    | EQ -> "=" 
    | GT -> ">"
    | LT -> "<"
    | INC -> "?"

  exception Incomparable

  let of_ord = function
    |  1 -> GT
    | -1 -> LT
    |  0 -> EQ
    | _ ->  invalid_arg "PartialOrd.of_ord"

  let to_ord = function
    | EQ -> Ord.eq
    | GT -> Ord.gt
    | LT -> Ord.lt
    | INC -> raise Incomparable (* invalid_arg "PartialOrd.to_cmp: cannot convert INC" *)

  let reverse = function
    | EQ -> EQ
    | GT -> LT
    | LT -> GT
    | INC -> INC

  let to_int = function
    | EQ -> 0
    | GT -> 1
    | LT -> 2
    | INC -> 3

  let of_int = function
    | 0 -> EQ
    | 1 -> GT
    | 2 -> LT
    | 3 -> INC
    | x -> invalid_arg (sprintf "PartialOrd.of_int: invalid int %d" x)

  let equal = (==)
  module O = struct
    let (=)  = (==)
    let (<>) = (!=)
  end

  let reverse_f f = 
    fun x y -> f y x
    
  (* let partial_cmp_to_total partial_cmp tiebreaker x y =
    match partial_cmp x y with
    | EQ -> Ord.eq
    | GT -> Ord.gt
    | LT -> Ord.lt
    | INC -> tiebreaker x y *)
end

type partial_ord = PartialOrd.t = EQ | GT | LT | INC

module type PartialOrdered = sig
  type t
  val partial_compare : t -> t -> PartialOrd.t
end

module PartialOrdMakeInfix (M: PartialOrdered) = struct
  let (>) a b =
    match M.partial_compare a b with
    | GT -> true
    | EQ | LT | INC -> false

  let (>=) a b =
    match M.partial_compare a b with
    | GT | EQ -> true
    | LT | INC -> false

  let (>=!) a b =
    match M.partial_compare a b with
    | LT | INC -> true
    | GT | EQ -> false

  let (<) a b =
    match M.partial_compare a b with
    | LT -> true
    | EQ | GT | INC -> false

  let (<=) a b =
    match M.partial_compare a b with
    | LT | EQ -> true
    | GT | INC -> false

  let (<=!) a b =
    match M.partial_compare a b with
    | GT | INC -> true
    | LT | EQ -> false
end

module type Hashable = Hashtbl.HashedType




(*------option/param--------------*)

module Option = struct
  type 'a t = 'a option

  exception None_opt

  (* let is_some = function 
    | Some _-> true 
    | None -> false *)
  let is_some x =
    x != None
    
  (* let is_none = function 
    | Some _-> false 
    | None -> true *)
  let is_none x =
    x == None
    
  let get = function
    | Some x -> x
    | None -> raise None_opt

  let get_fun f = 
    fun b -> get (f b)

  let bind x f = 
    match x with
    | Some y -> f y
    | None -> None

  let return x = Some x

  let default def x = 
    match x with
    | Some x -> x
    | None -> def

  let filter_some opt_list = List.map get (List.filter is_some opt_list)

  (* split_some apply list returns (res_list, none_list); *)
  (* iterates over list applying apply and collecting results in res_list *)
  (* if Some(res); when apply returns None we add the element of list into  non_list *)
  let partition_some apply list = 
    let f (some_list, non_list) e = 
      match apply e with
      | Some res -> (res::some_list, non_list)
      | None -> (some_list, e::non_list)
    in
    List.fold_left f ([],[]) list

  let join x = 
    match x with 
    | Some (Some x) -> Some(x)
    | Some(None) | None -> None
   
  let lift1 f x1 = 
    match x1 with
    | Some x1 -> Some (f x1)
    | _ -> None

  let lift = lift1

  let lift2 f x1 x2 = 
    match x1, x2 with
    | Some x1, Some x2 -> Some (f x1 x2)
    | _ -> None

  let lift3 f x1 x2 x3 = 
    match x1, x2, x3 with
    | Some x1, Some x2, Some x3 -> Some (f x1 x2 x3)
    | _ -> None

  module O = struct
    (** As [bind]. *)
    let (>>=) = bind
    let (let*) = bind

    (** As [default] with inverted arguments, i.e. value contained left-hand side if Some _, else right-hand side. *)
    let (|?) x def = default def x

    (** If left-hand side is [Some x], return [Some x]; else if right-hand side is [Some y], return [Some y]; else return [None]. *)
    let (<|>) a b = 
      match a with
      | Some _ -> a
      | None -> b
  end

  let to_string inner x =
    match x with
    | Some y -> sprintf "Some (%s)" (inner y)
    | None -> "None"

  (* include O *)

  let equal ~eq a b = 
    match a, b with
    | Some x, Some y -> eq x y
    | None  , None   -> true
    | Some _, None   -> false
    | None  , Some _ -> false

  let compare ~cmp a b = 
    match a, b with
    | Some x, Some y -> cmp x y
    | None  , None   -> Ord.eq
    | Some _, None   -> Ord.gt
    | None  , Some _ -> Ord.lt
end

exception None_opt = Option.None_opt


let is_some = Option.is_some

let get_some = Option.get

let get_some_fun = Option.get_fun

let split_some = Option.partition_some

let filter_some = Option.filter_some


(*--- used in maps for conting objects --*)

let cnt_opt_update opt_cnt = 
  match opt_cnt with 
  |None -> Some(1)
  |Some cnt -> Some(cnt + 1)


(* --- *)

type 'a param = Def of 'a | Undef 

exception Undef_param

let get_param_val p = 
  match p with 
  |Def(p') -> p'
  |Undef  -> raise Undef_param

let get_param_val_fun f = 
  (fun b -> get_param_val (f b))

let param_is_def p = 
  match p with
  | Def _ -> true
  | Undef -> false

let param_is_undef p = 
  match p with
  | Def _ -> false
  | Undef -> true

(*--------------*)

let apply_fun f_d args =
  match f_d with
  | Def (f) -> f args
  | Undef -> failwith "apply_fun: Function is not defined"

let apply_fun_if_def f_d args =
  match f_d with
  | Def (f) -> f args
  | Undef -> ()





(*--------------*)

module SmallOption (M : sig
  type t
  val none : t
  val is_none : t -> bool
end) = struct
  type elt = M.t
  type t = M.t

  let none = 
    M.none

  let some x = 
    if M.is_none x then invalid_arg "Small_option.some" else x

  let get x = 
    if M.is_none x then invalid_arg "Small_option.get" else x

  let some' x = 
    dassert (fun () -> not (M.is_none x)); x

  let get' x = 
    dassert (fun () -> not (M.is_none x)); x

  let is_none = 
    M.is_none

  let is_some = 
    Fun.negate M.is_none

  let[@inline] match' x ~some ~none = 
    match M.is_none x with
    | true  -> none ()
    | false -> some x

  let bind x f = 
    match M.is_none x with
    | true -> x
    | false -> f x

  let value x ~default = 
    if M.is_none x then default else x

  let equal eq = eq

  let compare cmp = cmp 

  module O = struct
    let (>>=) = bind
    let (let*) = bind
    let[@inline] (|?) a b = value a ~default:b
  end
end

module SmallOptionMagic (M : sig type t end) = SmallOption(struct 
  type t = M.t
  let none : t = (Obj.magic 0) 
  let is_none (x:t) = x == (Obj.magic 0) 
end)

module IOption = SmallOption(struct 
  type t = int 
  let none = -1 
  let is_none x = x = -1 
end)





(*--------------*)

module Float = struct
  include Float
  module O = struct
    include EqMakeInfix(Float)
    include OrdMakeInfix(Float)
  end
end





(*--------------*)

(* bool operations *)
let bool_plus x y = ((x&& (not y)) || ((not x)&& y))
(* let bool_plus' (x: bool) (y: bool) : bool = Obj.magic ((Obj.magic x) lxor (Obj.magic y)) *)  (* andrepd: faster *)

(* Includes optimised versions of some functions *)
module Bool = struct
  include Bool

  (* let bool_to_int b = if b then 1 else 0 *)
  let to_int = bool_to_int

  (* let compare (x : bool) y = 
    if x > y then 1
    else if y > x then -1
    else 0 *)
  let compare = bool_compare_fast

  module O = struct
    let (=)  : bool -> bool -> bool = Poly.(=)
    let (<>) : bool -> bool -> bool = Poly.(<>)
    let (<)  : bool -> bool -> bool = Poly.(<)
    let (>)  : bool -> bool -> bool = Poly.(>)
    let (<=) : bool -> bool -> bool = Poly.(<=)
    let (>=) : bool -> bool -> bool = Poly.(>=)
  end
end

let neg f x = 
  not (f x)

let neg2 f x y = 
  not (f x y)

let neg3 f x y z = 
  not (f x y z)





let[@inline] compose_12 g f = 
  fun x y -> g (f x y)

let (%%) f g = 
  fun x -> f (g x)

let (%>) g f = 
  fun x -> f (g x)

let id_fun x = x

let unit_fun x = ()

(* elements and ref to elem of indexies and all others*)

(* let () = Random.init(13) *) (* now initialised when options are read *)

(* Randomisation: *)

let rnd_bits = ref (-1) (* initialised when options are read *)

let get_rnd_bits () = 
  dassert (fun () -> !rnd_bits <> -1);
  !rnd_bits
	
let re_init_rnd_bits () = 
  rnd_bits := Random.bits()
 
let init_rnd seed = 
  Random.init seed; 
  rnd_bits := Random.bits()

let _ = Random.self_init(); re_init_rnd_bits()


(*hash function called djb2*)

let hash_sum rest num = 
  ((rest lsl 5) + rest) + num (* hash * 33 + num *)

(* returns hash of a list; hash_elem is fun elem-> hash*)
let hash_list hash_elem list = 
  List.fold_left (fun rest elem -> hash_sum rest (hash_elem elem)) 0 list 

(** [hash_sum_commutative a b = hash_sum_commutative b a], but if [a != a'] or
    [b != b'] then [hash_sum_commutative a b != hash_sum_commutative a' b']. *)
let hash_sum_commutative a b =
  (* Cantor's pairing function *)
  let s = a+b in
  s*(s+1) / 2 + b

    

type 'a elem = Elem of 'a | Empty_Elem
type 'a ref_elem = ('a elem) ref

exception Empty_list
let split_list l =
  match l with 
  | h::tl -> (h,tl)
  | [] -> raise Empty_list

let add_param_str str = 
  param_str_ref := sprintf "%s%s%s\n" (!param_str_ref) pref_str str

let add_param_str_front str = 
  param_str_ref := sprintf "%s%s\n%s" pref_str str (!param_str_ref)

let param_str_new_line () = 
  param_str_ref := sprintf "%s\n" (!param_str_ref)


(*compose sign with function*)

let compose_sign sign f = 
  if sign then 
    f 
  else 
    (* compose_12 (~-) f *)
    (* Called so often, might as well write manually *)
    fun x y -> f y x





module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let make a b = (a,b)

  let first = Stdlib.fst
  let second = Stdlib.snd

  let map_fst f (a,b) = (f a, b)
  let map_snd f (a,b) = (a, f b)

  let of_list l = 
    match l with
    | [a;b] -> (a,b)
    | _ -> invalid_arg "Pair.of_list: argument must have 2 elements"

  let to_list (a,b) = [a;b]

  let output inner_fst inner_snd out (a,b) =
    fprintf out "(%a, %a)" inner_fst a inner_snd b

  let to_string inner_fst inner_snd (a,b) =
    sprintf "(%s, %s)" (inner_fst a) (inner_snd b)



  let equal eqa eqb (a1,b1) (a2,b2) =
    eqa a1 a2 && eqb b1 b2

  let compare cmpa cmpb (a1,b1) (a2,b2) = 
    let res_cmpa: int = cmpa a1 a2 in
    if res_cmpa = Ord.eq then 
      cmpb b1 b2
    else 
      res_cmpa

  let compare_rev cmpb cmpa (a1,b1) (a2,b2) = 
    let res_cmpb: int = cmpb b1 b2 in
    if res_cmpb = Ord.eq then 
      cmpa a1 a2
    else 
      res_cmpb
end

(* For compatibility *)
exception Not_a_singleton
let get_singleton_from_list = function
  |[a] -> a
  |_ -> raise Not_a_singleton



module Triple = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let make a b c = (a,b,c)

  let first (a,b,c) = a
  let second (a,b,c) = b
  let third (a,b,c) = c

  let last_pair (a,b,c) = (b,c)

  let map_fst f (a,b,c) = (f a, b, c)
  let map_snd f (a,b,c) = (a, f b, c)
  let map_thd f (a,b,c) = (a, b, f c)

  let of_list l = 
    match l with
    | [a;b;c] -> (a,b,c)
    | _ -> invalid_arg "Pair.of_list: argument must have 2 elements"

  let to_list (a,b,c) = [a;b;c]

  let output inner_fst inner_snd inner_thd out (a,b,c) =
    fprintf out "(%a, %a, %a)" inner_fst a inner_snd b inner_thd c

  let to_string inner_fst inner_snd inner_thd (a,b,c) =
    sprintf "(%s, %s, %s)" (inner_fst a) (inner_snd b) (inner_thd c)
end

exception Not_a_triple
let get_triple_from_list = function 
  |[a1;a2;a3] -> (a1,a2,a3)
  |_-> raise Not_a_triple

let get_last_pair_from_triple_list  = function
  |[_;a1;a2] -> (a1,a2)
  |_-> raise Not_a_triple




(* used for localization of vars, binding can be 
   applied for vars, terms, clauses *)

module Bind = struct
  type 'a t = int * 'a

  let bind b a = (b,a)
  let unbind (b,a) = a
  let get_bound (b,a) = b

  let propagate_to_list (b, list) =
    List.map (fun a -> (b,a)) list

  let bind_list b list =
    List.map (fun a -> (b,a)) list

  let map f (b,a) = (b, f a)

  let output inner out (b,a) = 
    fprintf out "(%d,%a)" b inner a 

  let to_string inner (b,a) = 
    sprintf "(%d,%s)" (b) (inner a) 

  let equal ~eq (a1,b1) (a2,b2) =
    a1 = a2 && eq b1 b2

  let compare ~cmp x y =
    Pair.compare compare cmp x y

  (* module O = struct
    let (==) (a1,b1) (a2,b2) = 
      a1==a2 && b1==b2

    let (!=) (a1,b1) (a2,b2) = 
      a1!=a2 || b1!=b2
  end *)
end

(* Include old aliases for backwards-compatibility *)
type 'a bind = 'a Bind.t

let propagate_binding_to_list = Bind.propagate_to_list

let apply_to_bounded = Bind.map

let binded_to_string = Bind.to_string



(* let out_str s = Printf.fprintf stdout " %s \n" s *)
    
(*let out_str_a s = Printf.fprintf stdout " %s \n" s *)



(* lexicographic comparison on pairs*)
let pair_compare_lex = Pair.compare

(* lex combination of all compare functions in compare_fun_list*)
let(*[@inline]*) rec lex_combination_simple compare_fun_list = fun x1 x2 ->
  match compare_fun_list with
  | [] -> cequal
  | h::t ->
    let (res1: int) = h x1 x2 in
    if res1 = cequal then
      lex_combination_simple t x1 x2
    else
      res1

let rec lex_combination_unrolled compare_fun_list x1 x2 = 
  match compare_fun_list with 
  | [] -> 
    cequal 
  | [f1] ->
    f1 x1 x2
  | [f1;f2] ->
    let (res1: int) = f1 x1 x2 in
    if res1 = cequal then
      f2 x1 x2
    else
      res1
  | [f1;f2;f3] ->
    let (res1: int) = f1 x1 x2 in
    if res1 = cequal then
      let (res2: int) = f2 x1 x2 in
      if res2 = cequal then
        f3 x1 x2
      else
        res2
    else
      res1
  | [f1;f2;f3;f4] ->
    let (res1: int) = f1 x1 x2 in
    if res1 = cequal then
      let (res2: int) = f2 x1 x2 in
      if res2 = cequal then
        let (res3: int) = f3 x1 x2 in
        if res3 = cequal then
          f4 x1 x2
        else
          res3
      else
        res2
    else
      res1
  | f1::f2::f3::f4::tl -> 
    let (res1: int) = f1 x1 x2 in
    if res1 = cequal then
      let (res2: int) = f2 x1 x2 in
      if res2 = cequal then
        let (res3: int) = f3 x1 x2 in
        if res3 = cequal then
          let (res4: int) = f4 x1 x2 in
          if res4 = cequal then
            lex_combination_unrolled tl x1 x2
          else
            res4
        else
          res3
      else
        res2
    else
      res1

let lex_combination = lex_combination_simple



(* Fixed-length lex_combinations *)
let lex_combination2 f1 f2 x y =
    let res1 = f1 x y in
    if res1 <> Ord.eq then
      res1
    else
      f2 x y

let lex_combination3 f1 f2 f3 x y =
    let res1 = f1 x y in
    if res1 <> Ord.eq then
      res1
    else
    let res2 = f2 x y in
    if res2 <> Ord.eq then
      res2
    else
      f3 x y

let lex_combination4 f1 f2 f3 f4 x y =
    let res1 = f1 x y in
    if res1 <> Ord.eq then
      res1
    else
    let res2 = f2 x y in
    if res2 <> Ord.eq then
      res2
    else
    let res3 = f3 x y in
    if res3 <> Ord.eq then
      res3
    else
      f4 x y

let lex_combination5 f1 f2 f3 f4 f5 x y =
    let res1 = f1 x y in
    if res1 <> Ord.eq then
      res1
    else
    let res2 = f2 x y in
    if res2 <> Ord.eq then
      res2
    else
    let res3 = f3 x y in
    if res3 <> Ord.eq then
      res3
    else
    let res4 = f4 x y in
    if res4 <> Ord.eq then 
      res4
    else
      f4 x y

let lex_combination1 f1 = f1





(* ----- *)
(* Lists *)
(* ----- *)

(* Extra functions *)
module ListExtra = struct
  let is_empty l = 
    (* match l with
    | [] -> true
    | _ -> false *)
    l == []

  let is_nonempty l = 
    (* match l with
    | [] -> false
    | _ -> true *)
    l != []

  let is_singleton l = 
    match l with
    | [_] -> true
    | _ -> false

  let get_singleton l =
    match l with
    | [x] -> x
    | _ -> invalid_arg "List.X.get_singleton"



  let make n el =
    let rec loop i el acc = 
      if i = 0 then
        acc
      else
        loop (pred i) el (el::acc)
    in 
    loop n el []

  (* Partition list into chunks of specified size. *)  
  let partition_chunks size l = 
    assert (size > 0);
    let rec loop part_counter rest_part rest_ll rest_l =
      match rest_l with 
      | [] -> 
        if is_empty rest_part then 
          List.rev rest_ll 
        else
          let part = List.rev rest_part in           
          List.rev (part::rest_ll)
      | hd::tl -> 
        let new_part = hd::rest_part in
        if part_counter = size - 1 then 
          let new_rest_ll = List.rev new_part :: rest_ll in 
          loop 0 [] new_rest_ll tl
        else
          let new_part_counter = succ part_counter in
          loop new_part_counter new_part rest_ll tl
    in
    loop 0 [] [] l

  let hd_tl l = 
    dassert (fun () -> is_nonempty l);
    let[@warning "-8"] hd::tl = l in
    hd, tl





  let rec remove ~eq x l =
    match l with
    | y::ys ->
      if eq y x then
        ys
      else
        y :: remove ~eq x ys
    | [] -> raise Not_found

  let removeq x l = remove ~eq:(==) x l

  let remove_all ~eq x l =
    List.filter (fun y -> not (eq y x)) l 

  let removeq_all x l =
    List.filter (fun y -> y != x) l 

  let rec remove_duplicates ~eq l = 
    match l with 
    | h1::h2::tl -> 
      if eq h1 h2 then 
        remove_duplicates ~eq (h2::tl) 
      else 
        h1 :: remove_duplicates ~eq (h2::tl)
    | [_] | [] -> l

  let rec removeq_duplicates l = 
    remove_duplicates ~eq:(==) l

  (* removes repeated elemets wrt cmp keeps the first occurence *)
  (* no assumptions if repeated elements are apart; no assumptions that list is ordered *)
  (* uses locally abstract types *)

  let remove_repeated (type a) ~(cmp:a -> a -> int) (list:a list) = 
    let module ESet = Set.Make (struct 
      type t=a
      let compare=cmp 
    end) in
    
    let f (rest,seen_set) el = 
      if ESet.mem el seen_set then 
        (rest, seen_set)
      else
        (el::rest, ESet.add el seen_set)
    in
  let (rev_rm,_) = List.fold_left f ([], ESet.empty) list in
  List.rev rev_rm
    
  (* true if there are repeated elements in the list *)
  let check_repeated ~cmp list = 
    let sorted = List.sort cmp list in    
    let rec f l = 
      match l with 
      | h1::h2::tl -> 
          if (cmp h1 h2) = 0 then 
            true
          else 
            f (h2::tl)
      | [_] | [] -> false
    in
    f sorted

  let multiset_minus ~cmp big smol = 
    let rec loop ~cmp big smol = 
      match big, smol with
      | _, [] | [], _ -> 
        big
      | bh::bt, sh::st -> 
        let c = cmp bh sh in
        if c = 0 then
          loop ~cmp bt st
        else if c > 0 then
          loop ~cmp big st
        else 
          bh :: loop ~cmp bt smol
    in
    loop ~cmp (List.sort cmp big) (List.sort cmp smol)




  let count p l = 
    let f n x = 
      if p x 
      then n+1
      else n
    in
    List.fold_left f 0 l


  (* get first n >0 elements; if l has less than n elements then l *)
  let take n l = 
    assert (n > 0);
    let rec f acc n' l' = 
      if n' = 0 then 
        if is_empty l' then l else List.rev acc 
      else (* n' > 0 *)
        match l' with 
        | h::tl -> 
          f (h::acc) (n' - 1) tl
        | [] -> l 
    in
    f [] n l


  let find_index p l =
    let rec loop idx l =
      match l with 
      | [] -> raise Not_found
      | hd::tl -> 
      if p hd then 
        idx
      else
        loop (succ idx) tl
    in
    loop 0 l



  let partition_rev p l = 
    let rec loop p l acc_true acc_false =
      match l with
      | [] -> 
        (acc_true, acc_false)
      | hd::tl -> 
        if p hd then
          loop p l (hd::acc_true) acc_false
        else
          loop p l acc_true (hd::acc_false)
    in
    loop p l [] []

  let partition p l =
    let (t,f) = partition_rev p l in
    (List.rev t, List.rev l)

  let rec find_map p l = 
    match l with
    | [] -> 
      raise Not_found
    | x::xs -> 
      begin match p x with
      | Some y -> y
      | None -> find_map p xs
      end

  let rec find_map_opt p l = 
    match l with
    | [] -> 
      None
    | x::xs -> 
      let y = p x in
      begin match y with
      | Some _ -> y
      | None -> find_map_opt p xs
      end

  let rec map_first f l = 
    match l with
    | [] -> 
      raise Not_found
    | x::xs -> 
      begin match f x with
      | Some x' -> x' :: xs
      | None -> x :: map_first f xs
      end

  (** [filter_map p l = l |> filter (p %> is_some) |> map (p %> get_some)] *)
  (** [filter_map p l = l |> filter (is_some%p) |> map (get_some%p)] *)
  let rec filter_map p l = 
    match l with
    | [] -> []
    | x::xs -> 
      begin match p x with
      | Some x -> x :: filter_map p xs
      | None -> filter_map p xs
      end

  let concat_map f x = List.concat (List.map f x)

  let rec for_all2' p l1 l2 =
    match l1, l2 with 
    | [], [] -> 
      true
    | h1::t1, h2::t2 ->
      if p h1 h2 then
        for_all2' p t1 t2
      else
        false
    | _::_, [] | [], _::_ -> 
      false

  let rec exists2' p l1 l2 =
    match l1, l2 with 
    | [], [] -> 
      false
    | h1::t1, h2::t2 ->
      if p h1 h2 then
        true
      else
        exists2' p t1 t2
    | _::_, [] | [], _::_ -> 
      false

  let reduce f l =
    match l with
    | hd::tl -> List.fold_left f hd tl
    | [] -> invalid_arg "List.X.reduce: empty list"



  let rec is_sorted cmp l = 
    let rec ascending cmp l = 
      match l with
      | [] | [_] -> Ord.lt
      | a::(b::_ as tl) -> if cmp a b <= Ord.eq then ascending cmp tl else Ord.eq
    in
    let rec descending cmp l = 
      match l with
      | [] | [_] -> Ord.gt
      | a::(b::_ as tl) -> if cmp a b >= Ord.eq then descending cmp tl else Ord.eq
    in
    match l with
    | [] | [_] -> Ord.lt
    | a::(b::_ as tl) ->
      let c = cmp a b in
      if c = Ord.gt then
        descending cmp tl
      else if c = Ord.lt then
        ascending cmp tl
      else
        is_sorted cmp tl

  (* Can be made faster *)
  let sort cmp l = 
    let s = is_sorted cmp l in
    if s = Ord.lt then
      l
    else if s = Ord.gt then
      List.rev l
    else
      List.sort cmp l

  let sort_uniq cmp l = 
    let s = is_sorted cmp l in
    if s = Ord.lt then
      remove_duplicates ~eq:(fun x y -> cmp x y = 0) l
    else if s = Ord.gt then
      remove_duplicates ~eq:(fun x y -> cmp x y = 0) (List.rev l)
    else
      List.sort_uniq cmp l



  let cons_some x l =
    match x with
    | Some y -> y::l
    | None -> l

  let cons_ref x ref_l =
    ref_l := x :: !ref_l

  let (=::) = cons_ref

  let rec filter_some l =
    match l with
    | x::xs -> 
      begin match x with
      | Some y -> y :: filter_some xs
      | None -> filter_some xs
      end
    | [] -> []

  let cons_some_ref x ref_l = 
    match x with 
    | Some x -> ref_l := x :: !ref_l
    | None -> ()



  let equal ~eq x y =
    for_all2' eq x y

  let rec compare_lex cmp l1 l2 =
    match l1,l2 with
    | (hd1::tl1), (hd2::tl2) -> 
      let res = cmp hd1 hd2 in
      if res = Ord.eq then
        compare_lex cmp tl1 tl2
      else 
        res 
    | (_::_), [] -> Ord.gt
    | [], (_::_) -> Ord.lt
    | [], [] -> Ord.eq

  (* For a consistent interface *)
  let compare ~cmp l1 l2 = compare_lex cmp l1 l2

  let rec mem ~eq x l = 
    match l with
    | [] -> false
    | hd::tl -> if eq x hd then true else mem ~eq x tl

  let rec mem_assoc ~eq x l = 
    match l with
    | [] -> false
    | (hd,_)::tl -> if eq x hd then true else mem_assoc ~eq x tl

  

  let min cmp l = 
    let rec loop min cmp l = 
      match l with 
      | [] -> min
      | hd::tl -> 
        let min' = if cmp hd min == Ord.lt then hd else min in
        loop min' cmp tl
    in
    match l with
    | [] -> invalid_arg "List.X.min: empty list"
    | hd::tl -> loop hd cmp tl

  let max cmp l = 
    let rec loop max cmp l = 
      match l with 
      | [] -> max
      | hd::tl -> 
        let max' = if cmp hd max == Ord.gt then hd else max in
        loop max' cmp tl
    in
    match l with
    | [] -> invalid_arg "List.X.max: empty list"
    | hd::tl -> loop hd cmp tl



  (*--------- Lists over partial orders *)
  
  let max_elements_partial_ord cmp l = 
    let rec loop cmp maximals l =
      match l with
      | [] -> maximals
      | x::xs ->
        (* If x is greater than any maximal element in maximals, then that element is not maximal *)
        (* If x is not smaller than any maximal element, then add it to maximals *)
        (* We do it in one pass *)
        let is_maximal = ref true in
        let maximals = 
          maximals |> List.filter (fun y ->
            let module R = PartialOrd in
            match cmp x y with
            | R.GT -> false
            | R.LT -> is_maximal := false; true
            | R.EQ -> 
              (* If it is referentially equal, then also don't add it to maximal elements *)
              if x == y then (is_maximal := false); true
            | R.INC -> true
          ) 
        in
        let maximals = if !is_maximal then x::maximals else maximals in
        loop cmp maximals xs
    in
    loop cmp [] l
  
  let min_elements_partial_ord cmp l = 
    max_elements_partial_ord (PartialOrd.reverse_f cmp) l

  (* returns a minimal element in l that is smaller than x, if no such element in l then x. *)
  let min_below_partial_ord cmp x l =   
    let rec loop min l = 
      match l with 
      | [] -> min 
      | h::tl -> 
        match cmp h min with 
        | LT -> loop h tl
        | _ -> loop min tl
    in
    loop x l

  let max_above_partial_ord cmp x l = 
    min_below_partial_ord (PartialOrd.reverse_f cmp) x l  
        
  let rec is_linearly_sorted cmp l = 
    match l with 
    | [] | [_] -> true
    | x1::x2::tl -> 
      let res = cmp x1 x2 in
      if res == PartialOrd.LT || res == PartialOrd.EQ then 
        is_linearly_sorted cmp tl
      else
        false

  (* simple n^2 version *)    
  let topological_sort_simp cmp l = 
    let is_changed = ref false in 
    let rec loop l = 
      match l with
      | hd::tl ->
        let min = min_below_partial_ord cmp hd tl in 
        if hd == min then  (* there is no el in tl smaller than hd *)
          hd :: loop tl
        else (
          is_changed := true;
          let tl' = hd :: removeq min tl in
          min :: loop tl'
        )
      | [] -> []
    in
    let new_l = loop l in
    if !is_changed then new_l else l
              
  let is_topologically_sorted_simp cmp l =
    let rec loop l = 
      match l with
      | hd::tl ->
        let min = min_below_partial_ord cmp hd tl in 
        if hd == min then  (* there is no el in tl smaller than hd *)
          loop tl 
        else 
          false
      | [] -> true
    in
    loop l



  (*--------------*)
  
  let output ?(first="[") ?(last="]") ?(sep="; ") output_el out l = 
    (* (match first with Some str -> output_string out str | None -> ()); *)
    output_string out first;
    begin match l with
    | [] -> ()
    | [hd] -> output_el out hd
    | hd::tl ->
      output_el out hd;
      tl |> List.iter (fun x -> 
        output_string out sep;
        output_el out x
      )
    end;
    (* (match last with Some str -> output_string out str | None -> ()) *)
    output_string out last

  let string_output ?first ?last ?sep = 
    output ?first ?last ?sep output_string

  let to_string ?(first="[") ?(last="]") ?(sep="; ") to_string_el l = 
    let buf = Buffer.create 256 in
    Buffer.add_string buf first;
    begin match l with
    | [] -> ()
    | [hd] -> Buffer.add_string buf (to_string_el hd)
    | hd::tl ->
      Buffer.add_string buf (to_string_el hd);
      tl |> List.iter (fun x -> 
        Buffer.add_string buf sep;
        Buffer.add_string buf (to_string_el x)
      )
    end;
    Buffer.add_string buf last;
    Buffer.contents buf

  let string_to_string ?first ?last ?sep = 
    to_string ?first ?last ?sep (fun x -> x)

  let output' ?(first="") ?(last="") ?(sep="") output_el out l = 
    output ~first ~last ~sep output_el out l

  let to_string' ?(first="") ?(last="") ?(sep="") to_string_el l = 
    to_string ~first ~last ~sep to_string_el l

  (* let rec shuffle c l = 
    let rec loop pile1 pile2 l =
      match l with
      | hd::tl -> 
        if Random.bool() then
          loop (hd::pile1) pile2 tl
        else
          loop pile1 (hd::pile2) tl
      | [] -> 
        List.rev_append pile1 pile2
    in
    if c = 0 then 
      l 
    else
      let l' = loop [] [] l in
      shuffle (c-1) l' *)

  let shuffle l = 
    match l with
    | [] | [_] -> l
    | [x;y] -> if Random.bool() then l else [y;x]
    | _ -> 
      let arr = Array.of_list l in
      for n = Array.length arr - 1 downto 1 do
        let k = Random.int (n + 1) in
        let temp = arr.(n) in
        arr.(n) <- arr.(k);
        arr.(k) <- temp
      done;
      Array.to_list arr
end

module List = struct
  include List

  (* Silently uses polymorphic compare! *)
  let[@caml.deprecated] mem _ _ = assert false
  let[@caml.deprecated] mem_assoc _ _ = assert false

  module X = ListExtra
  (* !!!KK  Faster_map.faster_map applies f in reverse order! which is not acceptable when functions have side effects! *)
  (* let map = Faster_map.faster_map *)
  let map = Faster_map.plain_unrolled_map_5
end


(*-------------------*)
module IListKey =
  struct
    type t = int list
    let compare = List.X.compare ~cmp:Int.compare
  end

module ILMap = Map.Make (IListKey)
module ILSet = Set.Make (IListKey)
    
(*-------------------*)

    
module Seq = struct
  include Seq

  let rec take n xs =
    if n = 0 then
      empty
    else
      fun () ->
        match xs() with
        | Nil -> Nil
        | Cons (x, xs) -> Cons (x, take (n-1) xs)

  let rec (@) seq1 seq2 () =
    match seq1() with
    | Nil -> seq2()
    | Cons (x, next) -> Cons (x, next @ seq2)

  let rec concat_map f seq () = 
    match seq() with
    | Nil -> Nil
    | Cons (x, next) ->
      (@) (f x) (flat_map f next) ()

  let rec find_map f xs =
    match xs() with
    | Nil -> None
    | Cons (x, xs) ->
      match f x with
      | None -> find_map f xs
      | Some _ as result -> result
end



(** Dynamic array *)
module DynArray = struct
  type 'a t = {
    mutable arr: 'a Array.t;
    mutable size: int;
  }

  let length x = 
    x.size

  let[@inline] unsafe_get x i = Array.unsafe_get x.arr i
  let[@inline] unsafe_set x i y = Array.unsafe_set x.arr i y

  let get x i = 
    if 0 <= i && i < x.size then
      unsafe_get x i
    else
      invalid_arg "index out of bounds"

  let set x i y = 
    if 0 <= i && i < x.size then
      unsafe_set x i y
    else
      invalid_arg "index out of bounds"

  let make n x = 
    { arr = Array.make n x; size = n }

  let init n f = 
    { arr = Array.init n f; size = n }

  let copy x = 
    { arr = Array.copy x.arr; size = x.size }



  let aux arr n =  
    let arr' = Array.make n (Obj.magic 0) in
    for i = 0 to n-1 do Array.unsafe_set arr' i (Array.unsafe_get arr i) done;
    arr'

  let capacity x = 
    Array.length x.arr

  let resize x n = 
    if n >= x.size then 
      x.arr <- aux x.arr n
    else
      invalid_arg "resize to smaller than array length"

  let make_capacity ~len ~cap x = 
    { arr = Array.make cap x; size = len }

  let empty () = 
    { arr = Array.make 0 (Obj.magic 0); size = 0 }

  let empty_capacity c = 
    { arr = Array.make c (Obj.magic 0); size = 0 }

  let push_back x y = 
    dassert (fun () -> 0 <= x.size && x.size <= Array.length x.arr);
    if x.size = Array.length x.arr then (
      x.arr <- aux x.arr (2 * (x.size + 1))
    );
    Array.unsafe_set x.arr x.size y;
    x.size <- x.size + 1;
end



(* Extra functions for String *)
module String = struct
  include String
  module X = struct
    let is_empty x = 
      match x with
      | "" -> true 
      | _ -> false

    let is_nonempty x = 
      match x with
      | "" -> false 
      | _ -> true
  end

  module O = EqMakeInfix(String)
end



module Char = struct
  include Char
  module O = EqMakeInfix(Char)
end





(*-----------------*)
let get_first_non_empty_str str_list = 
  try 
    List.find String.X.is_nonempty str_list
  with 
    Not_found -> ""

(*-----------------*)


(* apply iteratively funs in a list to the result  *)
let fold_left_fun_list fun_list x = 
  let f result l_fun = l_fun result in
  List.fold_left f x fun_list 

let iter_fun_list fun_list x = 
  let apply f = f x in
  List.iter apply fun_list

let fix_point_eq equal f x = 
  let rec fp x' = 
    let new_x = f x' in
    if (equal new_x x') 
    then 
      new_x
    else
      fp new_x
  in
  fp x

let fix_point f x = fix_point_eq (==) f x 

(* bound lists*)

type 'a bound_list = ('a list) bind

(*
  let rec bound_list_fold_left f a (bound_list : bound_list) = 
  
 *)




(*-------- folds a function over intervals -------------*)
(* folds from a to b inclusive *)
(* f rest i *)

let fold_up_interval f a b init_val = 
  let rec g rest i = 
    if i > b then 
      rest 
    else 
      let new_rest = f rest i in 
      g new_rest (succ i)  (* andrepd: faster *)
  in
  g init_val a

let fold_down_interval f a b init_val = 
  let rec g rest i = 
    if i < a then 
      rest 
    else 
      let new_rest = f rest i in 
      g new_rest (pred i)  (* andrepd: faster *)
  in
  g init_val b
    



(*------------------- Lists----------------------*)

(* checks whether list is empty *)
let list_is_empty = ListExtra.is_empty

(* checks whether list is non-empty *)
let list_non_empty = ListExtra.is_nonempty

(* is one elment list *)
let list_is_singleton = ListExtra.is_singleton

(* returns list which starts with the next elem *)
(* assume that elem in l *)
(* careful if there are duplicates*)
let rec list_skip elem l = 
  match l with 
  | h::tl -> 
    if h == elem then 
      tl 
    else 
      list_skip elem tl	
  | [] -> failwith "Lib.list_skip: elem should be in l"


(* explicitly maps from left to right, 
   since order can matter when use imperative features *)

(* let rec list_map_left f l  = 
  match l with
  | h::tl -> 
    let new_h = f h in 
    new_h :: list_map_left f tl
  | [] -> []

let list_mapi_left f l  = 
  let rec aux i f l =
    match l with
    | h::tl -> 
      let new_h = f i h in 
      new_h :: (aux (succ i) f tl)
    | [] -> []
  in
  aux 0 f l *)

(* This contract doesn't change: order is left to right, so we can use the stdlib versions. *)
let list_map_left = List.map
let list_mapi_left = List.mapi



(* stops when f is Some(e) and returns Some(e) otherwise returns None  *)
let rec list_findf f = function 
  |h::tl -> 
      (match (f h) with 
      |Some(e)-> Some(e)
      |None -> list_findf f tl
      )
  |[] -> None



let list_remove_last l = 
  match List.rev l with 
  | hd::tl -> Some (hd, List.rev tl)
  | [] -> None

let list_compare_lex = ListExtra.compare_lex 


(* in list_get_max_elements_v 
   is mainly for non-ground (not exactly) orderings
   we assume that compare as follows: 
   returns cequal if t greater or equal to s and 
   returns cequal+1 if t is strictly greater
   returns cequal-1 if it is not the case
   Note: it is assumed that 
   if t (gr or eq) s and s (gr or eq) t then t==s*)    

let rec list_is_max_elem_v compare elem list = 
  match list with 
  |h::tl -> 
(*      if ((not (h == elem)) & ((compare h elem) >= 0))       
	then false 
	else (list_is_max_elem_v compare elem tl) 
 *)
      if (h == elem) || not ((compare h elem) > 0) 
      then (list_is_max_elem_v compare elem tl)
      else false  
  |[] -> true

let list_get_max_elements_v compare list = 
  let f rest elem = 
    if  list_is_max_elem_v compare elem list
    then elem::rest
    else rest 
  in List.fold_left f [] list

(* for usual orderings *)
let rec list_is_max_elem compare elem list = 
  match list with 
  |h::tl -> 
      if (compare h elem) > 0
      then false 
      else (list_is_max_elem compare elem tl)
  |[] -> true

(*
  let rec list_find_max_element compare list =
  match list with 
  |h::tl -> 
  if tl = [] 
  then h
  else
  let max_rest = list_find_max_element compare tl in
  if (compare max_rest h) > 0 
  then max_rest
  else h
  |[] -> raise Not_found
 *)



let list_find_max_element compare list =
  let rec f max_el rest =     
    match rest with 
    |h::tl -> 
	if ((compare h max_el)>0) then 
	  f h tl 
	else 
	  f max_el tl 
    |[] -> max_el
  in
  match list with 
  |h::tl -> f h tl 
  |[] -> raise Not_found

let list_find_min_element compare list = 
  list_find_max_element (fun a b -> compare b a) list

let list_find_all_min_elements compare list = 
  let incr_sorted_list = List.sort compare list in (* increasing order *)
  let rec get_all_eq_top rest top l = 
    match l with 
    | h::tl -> 	
	if (compare top h) = 0 
	then 
	  get_all_eq_top (h::rest) top tl
	else
	  rest
    |[] -> rest
  in
  let all_min_elts = 
    match incr_sorted_list with 
    | top::tl -> (get_all_eq_top [] top incr_sorted_list)
    | [] -> []
  in
  all_min_elts

let list_find_all_max_elements compare list = 
  list_find_all_min_elements (fun a b -> compare b a) list


let rec list_find_max_element_p test cmp list =
  match list with 
  | h::tl -> 
    if test h 
    then (
      if List.X.is_empty tl 
      then h
      else (
        try 
          let max_rest = list_find_max_element_p test cmp tl in
          if cmp h max_rest > 0 
          then h 
          else max_rest
        with Not_found -> h
      )
    )
    else list_find_max_element_p test cmp tl
  | [] -> raise Not_found

let list_find_min_element_p test cmp list =
  list_find_max_element_p test (fun a b -> cmp b a) list



(* like List.find but for two lists in parallel*)

let rec list_find2 f l1 l2 = 
  match (l1,l2) with
  | ((h1::tl1),(h2::tl2)) -> 
      if f h1 h2  then (h1,h2) 
      else list_find2 f tl1 tl2
  |_ -> raise Not_found

(* like list_find2 only returns (g h1 h2)  *) 

let rec list_return_g_if_f2 f g l1 l2 = 
  match (l1,l2) with
  | ((h1::tl1),(h2::tl2)) -> 
      if f h1 h2  then g h1 h2 
      else list_return_g_if_f2 f g tl1 tl2
  |_ -> raise Not_found

(* *)
let rec list_find_not_equal compare_el l1 l2 = 
  match (l1,l2) with
  | (h1::tl1,h2::tl2) -> 
      let c = compare_el h1 h2 in 
      if  c<>cequal then c 
      else list_find_not_equal compare_el tl1 tl2
  |_ -> raise Not_found


let rec list_find_not_identical l1 l2 = 
  match (l1,l2) with
  | (h1::tl1,h2::tl2) -> 
      if  not (h1==h2) then (h1,h2) 
      else list_find_not_identical tl1 tl2
  |_ -> raise Not_found

        
let rec list_identical_elts l1 l2 = 
  match l1, l2 with
  | h1::t1, h2::t2 -> 
      if h1 == h2 then 
        list_identical_elts t1 t2
      else 
        false      
  | [], [] -> true
  | _ -> false
        

(** Faster than calling [try ignore @@ list_find_not_identical l1 l2; false with Not_found -> true] *)
(** if length of lists are different then true *)
        
let list_exists_not_identical l1 l2 = not (list_identical_elts l1 l2)
  
(*
let rec list_exists_not_identical l1 l2 = 
  match l1, l2 with
  | h1::t1, h2::t2 -> 
    if h1 != h2 then 
      true
    else 
      list_exists_not_identical t1 t2
  | [], [] -> false
  | _ -> true
*)



(* appends ass lists: if list1 and list2 have
   elem with (k,v1)  and (k,v2) resp. then new list will have (k,(f v1 v2))
   otherwise  appends (k1,v1) and (k2,v2)*)

let rec append_ass_list f ass_list_1 ass_list_2  =
  match ass_list_1 with 
  | (k1,v1)::tl1 -> (
    try 
      let v2 = List.assoc k1 ass_list_2 in 
      let new_list_2 = 
        (k1, f v1 v2) :: List.remove_assoc k1 ass_list_2
      in   
      append_ass_list f tl1 new_list_2  
    with
      Not_found -> append_ass_list f tl1 ((k1,v1)::ass_list_2)
  )
  | [] -> ass_list_2

(* number association lists *)

type ('a, 'b) ass_list = ('a*'b) list

type 'a num_ass_list =  ('a, int) ass_list

(* minimise_list ~keep ~test list  *)
(* returns a minimal substet of the list on which test is true *)
(* keep -- elements that must be kept *)
(* we assume test is monotone -- if test is true on a sub-list then it is true on all lists containing this sub-list *)
(* can raise Not_found if the input list does not satisfy the test *)
(* remove from large to small according to cmp if defined *)

(* cmp: compare for priority smaller prioritised for inclusion (larger are eliminated first) *)
let minimise_list ?cmp ~keep ~test list = 
  let sorted_list = 
    match cmp with 
    | Some(cmp_fun) -> 
        (
      (*   out_str "lib.ml:minimise_list"; *)
        List.sort cmp_fun list 
        )
    | None -> list
  in 
  let rec  minimise_list' keep_list rest = 
    match rest with 
    | h::tl -> 
        if (keep h) || (not (test (keep_list@tl)))
        then
          let new_keep = h::keep_list in
          minimise_list' new_keep tl 
        else (* h can be removed *)
          minimise_list' keep_list tl
    | [] -> keep_list 
  in          
  if (not (test sorted_list))
  then 
    raise Not_found 
  else 
    minimise_list' [] sorted_list 

(* returns a list of minimal subsets satisfying test which do not overlap with exception of keep *)

let minimise_list_enum ?cmp ~keep ~test list = 
  let remove_min_list found_list curr_list = 
    List.filter (fun x -> keep x || not (List.mem x found_list)) curr_list   (* not efficient but don't have sets *)
  in
  let rec minimise_list_enum' acc curr_list  = 
    try 
      let new_min_list = minimise_list ?cmp ~keep ~test curr_list in
      let new_curr_list = remove_min_list new_min_list curr_list in
      let new_acc =  new_min_list::acc in
      if List.compare_lengths new_curr_list curr_list = 0 then (* all keep in curr_list *)
        new_acc
      else
        minimise_list_enum' new_acc new_curr_list
    with Not_found -> 
      acc
  in
  minimise_list_enum' [] list

(*
    (match list with 
    |hd::tl -> minimise_list' [] tl 
*)      







      

(*-----------*)
(* dangerous: old lists are changing...
   association lists on ref's

   type 'a 'b ass_list = ('a*('b ref)) list

   let rec append_ass_list f ass_list_1 ass_list_2  = 
   match n_list_1 with 
   |(k1,v1)::tl1 -> 
   (try 
   let v2 = List.assoc k1 n_list_2 in 
   v2 := f !v1 !v2 ;
   append_ass_list f tl1 ass_list_2  
   with
   Not_found -> (k1,v1)::n_list_2
   )
   |[] -> ass_list_2

 *)

(*------------- reachibility depth ----------*)
(* given a module with an elemet, and reachability relation *)
(* outputs map of rechable elements with the reachability depth *)

module type El =
  sig
    type t 
    val compare : t -> t -> int
  end

module MakeReach 
    (El:El) 
    (ReachMap:Map.S with type key=El.t) 
    (ElSet:Set.S with type elt = El.t)     
    = 
  struct
    type reach_map_el = (int ReachMap.t)

	  (* returns a map of el-> int_ref where int is the reachability depth *)
    let rec comp_reach_rec succ_rel current_map current_depth curr_el_set = 
      if (ElSet.is_empty curr_el_set)
      then 
	current_map
      else
	let f el (reach_map, el_set) =
	  if (ReachMap.mem el reach_map)
	  then 
	    (reach_map, el_set) 
	  else 
	    let new_map = (ReachMap.add el current_depth reach_map) in
	    let new_el_set = (ElSet.union (succ_rel el) el_set) in
	    (new_map,new_el_set) 
	in 
	let (new_map, new_el_set) = 
	  ElSet.fold f curr_el_set (current_map, ElSet.empty)  in  
	let new_depth = current_depth+1 in
	comp_reach_rec succ_rel new_map new_depth new_el_set  

    let compute_reachability_set ~succ_rel depth_0_set = 
      let depth = 0 in 
      let (map : reach_map_el) = ReachMap.empty in
      comp_reach_rec succ_rel map depth depth_0_set

    let	compute_reachability_list ~succ_rel depth_0_list =
      let depth_0_set = ElSet.of_list depth_0_list in 
      compute_reachability_set ~succ_rel depth_0_set
  end

(*
module type ReachRel =
  sig
    type t 
    val succ_rel : t -> t  list 
    val compare : t -> t -> int
  end

module MakeReach (ReachRel:ReachRel) = 
  struct
    type e = ReachRel.t
    module ReachMap = Map.Make(ReachRel) 
    type reach_map_el = (int ReachMap.t)

	  (* returns a map of el-> int_ref where int is the reachability depth *)
    let rec comp_reach_rec current_map current_depth el_list = 
      if (el_list = [])
      then 
	current_map
      else
	let f (reach_map, el_list) el =
	  if (ReachMap.mem el reach_map)
	  then 
	    (reach_map, el_list) 
	  else 
	    let new_map = (ReachMap.add el current_depth reach_map) in
	    let new_el_list = (ReachRel.succ_rel el)@el_list in
	    (new_map,new_el_list) 
	in 
	let (new_map,new_el_list) = 
	  List.fold_left f (current_map,[]) el_list in  
	let new_depth = current_depth+1 in
	comp_reach_rec new_map new_depth new_el_list  

    let	compute_reachability depth_0_list =
      let depth = 0 in 
      let (map : reach_map_el) = ReachMap.empty in
      comp_reach_rec map depth depth_0_list
  end

*)


      
      
(*----------- Output Buffers/Channels ----------------------*)

let channel_first_non_empty_ln in_channel = 
  let rec skip () = 
    let str = input_line in_channel in 
    if Stdlib.(str = "\n"  || str = "" || str="\x00")  then
      skip ()
    else 
      str
  in
  skip ()
    
(* string stream can be e.g. a buffer or a channel *)
(* all output should be via streams (for efficiency reasons) *)
(* if strings are needed then to_string       *)
(* should be called only at the most top level *)

type 'a string_stream = {
  stream : 'a;
  stream_add_char : char   -> unit;
  stream_add_str  : string -> unit;
}
      
let create_buffer_stream size = 
  let b = Buffer.create size in
  {
    stream = b;
    stream_add_char = Buffer.add_char b;
    stream_add_str  = Buffer.add_string b;
  }

let to_string_buffer_stream s = 
  Buffer.contents s.stream  

let stdout_stream = {
  stream = stdout;
  stream_add_char = print_char;
  stream_add_str  = print_string;
}

let sterr_stream = {
  stream = stderr;
  stream_add_char = prerr_char;
  stream_add_str  = prerr_string;
}

(* "let to_string = to_string_fun_from_to_stream_fun 30 to_stream" *)
(*    creates to_string function from to_stream function with      *)
(*    initial buffer size 30                                       *)

let to_string_fun_from_to_stream_fun init_buff_size to_stream = 
  let out_fun a =
    let s = create_buffer_stream init_buff_size in    
    to_stream s a;
    to_string_buffer_stream s
  in
  out_fun
    

let rec list_to_stream ?first ?last s to_str_el l separator_str = 
  (match first with Some x -> s.stream_add_str x | None -> ());
  begin match l with
  | [] -> ()
  | [h] -> to_str_el s h
  | h::rest -> 
    to_str_el s h;
    s.stream_add_str separator_str;
    list_to_stream s to_str_el rest separator_str
  end;
  (match last with Some x -> s.stream_add_str x | None -> ())


(* Opens a file [filename] and return a formatter writing into the
   opened file. If [append] is true and the file exists it is opened
   for appending, otherwise it is truncated to zero length if it
   exists. Return the formatter writing to stdout if [filename] is
   "-".  The [Sys_error] exception is not caught here but passed to
   the calling function. *)
let formatter_of_filename append filename =

  (* Output to stdout? *)
  if String.O.(filename = "-") then 

    (* Use formatter for stdout *)
    Format.std_formatter

  else

    (* Opening mode for file *)
    let open_flags = 

      (* Append to file only? *)
      if append then 

	(* Append to file, create if not existing and use text mode *)
	[Open_append; Open_creat; Open_text]

      else
	
	(* Write to file, create if not existing, truncate if existing
	   and use text mode, this is the default from open_out in
	   OCaml's pervasives.ml *)
	[Open_wronly; Open_creat; Open_trunc; Open_text]

    in

    (* Permissions if file is created, this is the default from
       open_out in OCaml's pervasives.ml *)
    let open_perm =  0o666 in
    
    (* Open file for writing or appending *)
    let formatter_channel = 
      Stdlib.open_out_gen open_flags open_perm filename 
    in
    
    (* Return formatter writing to file *)
    Format.formatter_of_out_channel formatter_channel


(* Print an array of any type with separator from an index on *)
let rec pp_any_array' pp_a sep ppf array = function
  | i when i > Array.length array -> ()
  | i when i < 0 -> ()
  | i when i = Array.length array - 1 -> 
      Format.fprintf ppf "%a" pp_a array.(i)
  | i -> 
      Format.fprintf ppf "%a%s" pp_a array.(i) sep; 
      pp_any_array' pp_a sep ppf array (succ i)

(* Print an array of any type with separator *)
let pp_any_array pp_a sep ppf array = 
  pp_any_array' pp_a sep ppf array 0


(* Print a list of any type with separator *)
let rec pp_any_list pp_a sep ppf = function
  | [] -> ()

  | [a] -> 
      Format.fprintf ppf "@[<h>%a@]" pp_a a

  | a::tl -> 

      Format.fprintf 
	ppf 
	"@[<h>%a@]%s@," 
	pp_a a sep; 
      
      pp_any_list pp_a sep ppf tl


(* Print a list of strings with separator *)
let pp_string_list = pp_any_list Format.pp_print_string


(* Print a list of strings with separator *)
let pp_string_array sep array = pp_any_array Format.pp_print_string sep array


(* Print a list of strings with separator *)
let pp_int_list = pp_any_list Format.pp_print_int


(* Print an array of strings with separator *)
let pp_int_array sep array = pp_any_array Format.pp_print_int sep array


(* Print a list of floats with separator *)
let pp_float_list = pp_any_list Format.pp_print_float


(* Print an array of floats with separator *)
let pp_float_array sep array = pp_any_array Format.pp_print_float sep array


(* Print an 'a option value *)
let pp_option pp none_str ppf = function
  | None -> Format.fprintf ppf "%s" none_str
  | Some s -> Format.fprintf ppf "%a" pp s


(* Print a string option value *)
let pp_string_option none_str ppf str = 
  pp_option Format.pp_print_string none_str ppf str


(* Return a string of a string option value *)
let string_of_string_option none_str str =
  ignore (Format.flush_str_formatter ());
  Format.fprintf Format.str_formatter "%a" (pp_string_option none_str) str;
  Format.flush_str_formatter ()


(* Examples a bit old: *)

(*

  1)

  let b = Buffer.create 10000 in
  let s = {stream = b;
  stream_add_char = Buffer.add_char b;
  stream_add_str  = Buffer.add_string b}   

(* strings, chars are added at the end*)
  s.add_str "first line\n"; 
  s.add_str "second line\n";

  2) (*        stdout          *)

  let out_model model = 
  let s = 
  {stream = stdin;
  stream_add_char = print_char ;
  stream_add_str  = print_string}   
  in
  model_to_stream s model

  3)

  let s =  {stream = out_channel;
  stream_add_char = output_char out_channel;
  stream_add_str  = output_string out_channel;
  } in
  bench_to_buffer s formula; 
  flush out_channel

  -----------------
(*if string is needed then *)
  let b_string =  (Buffer.contents b)

(* if out buffer to channel then *)
  let fun_out out_ch = Buffer.output_buffer out_ch b in

 *)



let param_to_string el_to_string elp = 
  match elp with 
  |Def(el) -> el_to_string el 
  |Undef   -> "Undef"


let param_to_stream el_to_stream s elp = 
  match elp with 
  |Def(el) -> el_to_stream s el 
  |Undef   -> s.stream_add_str "Undef"

(*---------strings-----------*)

(*string filled with n sep_chars *)
let space_str_sep sep_char n = 
  if n>0 then
    String.make n sep_char
  else 
    (* string_of_char sep_char *)
    ""

let space_str n = 
  space_str_sep ' ' n

let to_stream_space_sep sep_char s n = 
  for j=1 to n do 
    s.stream_add_char sep_char
  done

let to_stream_space s n = 
  to_stream_space_sep ' ' s n


(* add spaces to str to reach distance *)
(*if the distance is less than or equal to str then just one space is added*)
(*(used for formatting output) *)

let space_padding_str_sep sep_char distance str =
  let name_ln = String.length str in
  let padding = space_str_sep sep_char (distance - name_ln) in
  sprintf "%s%s" str padding

let space_padding_str distance str = 
  space_padding_str_sep ' ' distance str

(* Equivalent for streams *)

let space_padding_stream_sep sep_char distance stream str =
  let name_ln = String.length str in
  stream.stream_add_str str;
  to_stream_space_sep sep_char stream (distance - name_ln)

let space_padding_stream distance stream str = 
  space_padding_stream_sep ' ' distance stream str

(*
let rec list_to_string to_str_el l separator_str =  
  match l with
    []->""
  | h::[] -> to_str_el h
  | h::rest -> 
      (to_str_el h)^separator_str^(list_to_string to_str_el rest separator_str)
*)

let list_to_string ?(first="") ?(last="") to_str_el l sep =  
  (* let str_list = List.map to_str_el l in 
  String.concat sep str_list *)
  match l with
  | [] ->
    first^last
  | [x] ->
    Printf.sprintf "%s%s%s" first (to_str_el x) last
  | hd::tl ->
    let buf = Buffer.create 512 in
    Buffer.add_string buf first;
    Buffer.add_string buf (to_str_el hd);
    tl |> List.iter (fun x -> 
      Buffer.add_string buf sep;
      Buffer.add_string buf (to_str_el x)
    );
    Buffer.add_string buf last;
    Buffer.contents buf

let list_of_str_to_str ?(first="") ?(last="") str_list sep = 
  list_to_string ~first ~last (fun x->x) str_list sep
   


(*------------*)

let remove_dollars_str str = String.concat "" (Str.split (Str.regexp "[$]+") str)


(*----------- round robin-------------------*)
(*
type tmp =  'a -> 'a
*)

(* round_robin [(n,f,a),..,] *)
type 'a round_robin_spec = (int * ('a -> unit) * 'a)  list 

let iter_fun_n (n,f,a) = 
  for i = 1 to n 
  do (f a)
  done
    
(* round_robin [(n,f,a),..,]*)
let round_robin_single spec_list = 
  List.iter iter_fun_n spec_list
    
let round_robin_n n spec_list = 
  for i = 1 to n 
  do 
    round_robin_single spec_list 
  done
    
let round_robin_inf spec_list =   
  while true do
      round_robin_single spec_list 
  done

(*----------------reals----------*)

(* decimal reals *)
type real = 
    {
     (* real_fraction Ee exp *)
     real_string      : string; 
     real_fraction    : float;
     real_exponent    : int; 
   }

let real_to_string r = 
  r.real_string
(*
  if r.real_exponent = 0 then  (* currently all exponents are 0 as reals are translated to floating point numbers *)
    sprintf "%f" r.real_fraction
  else
    sprintf "%fe%d"
      r.real_fraction
      r.real_exponent
*)

(*--------Named modules----------------------*)

module type NameM = 
  sig
    val name : string
  end



(*--------------Global Time Limits-------------------*)

(* time limit in seconds *)
(* time_limit can be reassigned, there are number of points where it is checked*)

exception Timeout

(*---------Discount time limits can be checked in all related modules-------*)
(* After Timeout using discount can be incomplete (bit still sound) *)
let discount_time_limit  = ref Undef
let start_discount_time  = ref Undef

let assign_discount_time_limit (x:float) = discount_time_limit := Def(x)
let unassign_discount_time_limit () = discount_time_limit := Undef
    
let assign_discount_start_time () = 
  start_discount_time := Def((Unix.gettimeofday ()))

let get_start_disc_time () = 
  match !start_discount_time with 
  |Def(t) -> t
  |Undef  -> failwith "Discount: start_time is Undef"

let get_disc_time_limit () = 
  match !discount_time_limit with 
  |Def(t) -> t
  |Undef  -> failwith "Discount: discount_time_limit is Undef"

let check_disc_time_limit () = 
  match !discount_time_limit with
  | Def(t_limit) -> 
      if Float.O.(Unix.gettimeofday() -. get_start_disc_time() > t_limit)
      then raise Timeout
      else ()
  | Undef -> ()



(*-------------int--------*)
module IntKey = 
  struct 
    type t = int
    let compare = compare
    let equal = (=)
    let hash (i:t) = i
  end

(* module IntMap = Map.Make(struct type t = int let compare = compare end *)


module IntMap = Map.Make(IntKey)
module IntHtbl = Hashtbl.Make(IntKey)
module IntSet = Set.Make(IntKey)

(*----------------------*)
module PairIntKey = 
  struct 
    type t = int * int
    let compare = Pair.compare compare compare
    let equal = Pair.equal (=) (=)
    let hash (p:t) = Hashtbl.hash p
  end

module PairIntMap = Map.Make(PairIntKey)
module PairIntHtbl = Hashtbl.Make(PairIntKey)
module PairIntSet = Set.Make(PairIntKey)

(*-------------Str--------*)

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)




(*----------Indexed  *)
module type IndexedSig = sig
  type el
  type t
  val create : unit -> t
  val add : el -> t -> t
  val find_ind : el -> t -> int
  val find_el : int -> t -> el
  val size : t -> int
end
      
      
module Indexed(Ord : Ordered) : IndexedSig with type el = Ord.t = struct
  module OMap = Map.Make(Ord)

  type el = Ord.t 
  type t =
      {
       fwd: int OMap.t;     (* t -> ind *)
       bwd: Ord.t IntMap.t; (* ind -> t *)
       size : int;
     }

  let empty = {
    fwd = OMap.empty;
    bwd = IntMap.empty;
    size = 0;
  }
      
  let create () = empty
    
  let add el t =
    if OMap.mem el t.fwd then t
    else      
      {
       fwd = OMap.add el t.size t.fwd;
       bwd = IntMap.add t.size el t.bwd;
       size = t.size + 1;
     }
        
  let find_ind el t =
    OMap.find el t.fwd

  let find_el ind t = 
    IntMap.find ind t.bwd
      
  let mem_el el t =
    OMap.mem el t

  let mem_ind ind t =
    ind < t.size && 0 <= ind
    
  let size t = t.size
      
end
      
(*----------------*)

let list_remove_int_duplicates int_list =
  let f rest i =      
    IntSet.add i rest
  in
  let int_set = List.fold_left f IntSet.empty int_list in
  IntSet.elements int_set

 
(*----- re_run_prover ------------*)  

(* arg_str can contain double quotes for etc.  not finished *)
(* let process_arg_str arg_str = *)
  

let re_run_prover extra_args = 
  let args = Sys.argv in
  let iprover_cmd = args.(0) in
  let iprover_dir = Filename.dirname iprover_cmd in

  let iprover_full_cmd = Filename.concat iprover_dir iprover_cmd  in

  (* out_str ("iprover args: "^(String.concat "," args_list)); *)
  let args_orig_list = Array.to_list args in
  let args_list_rev = List.rev args_orig_list in 

  assert (List.compare_length_with args_list_rev 1 = Ord.gt);  (* there are some options; first arg is iproveropt *)

  out_str (sprintf "num of org args: %d\n" (List.length args_orig_list));
  out_str (sprintf "orig args: %s" (String.concat "," args_orig_list));

  let new_args_list_rev = 
    match args_list_rev with 
    | input_file :: tl ->   (* first rev arg is the input file *)
      assert (Sys.file_exists input_file);
      input_file::"none"::"--schedule"::tl 
    | [] -> failwith "re_run_prover should not happen"
  in
  let new_args_list = List.rev new_args_list_rev in 
  let new_args = Array.of_list new_args_list in 

  out_str (sprintf "num of new args: %d\n" (List.length new_args_list));
  out_str (sprintf "new args: %s" (String.concat "," new_args_list));

  if List.compare_length_with args_orig_list 6 = Ord.gt
  then ()
  else ignore (Unix.execv iprover_full_cmd new_args)

(*  
  let _ = (re_run_prover (); )
*)
