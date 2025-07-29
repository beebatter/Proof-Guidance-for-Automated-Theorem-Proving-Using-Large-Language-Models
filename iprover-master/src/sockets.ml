(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2022 Konstantin Korovin and The University of Manchester. 
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

(** Interactive mode where iProver communicates with External Agent server (EA) for guidance 
    Currently two modes are supported:

     1. passive clause scores mode, where EA assigns priority scores to passive clauses, 
        the given clause is selected based on the highest score

     2. given clause mode, where EA explicitly selects the given clause.
*)

open Lib
open Logic_interface 
module PropSolver = Prop_solver_exchange.PropSolver

(*----- debug modifiable part-----*)

let dbg_flag = true

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

type json = Yojson.Basic.t 

(* TODO: move connections to Lib *)
type connection = 
    {
     con_id:       int;
     socket_addr:  Unix.sockaddr; 
     socket_descr: Unix.file_descr; 
     out_ch:       out_channel;
     in_ch:        in_channel;
   }

let con_cnt_ref = ref 0

let connections = ref []

let open_connection addr_str port = 
  let socket_addr = Unix.ADDR_INET(Unix.inet_addr_of_string addr_str, port) in
  let socket_descr = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket_descr socket_addr;
  let new_conn = 
    {
     con_id = !con_cnt_ref;
     socket_addr;
     socket_descr;
     out_ch = Unix.out_channel_of_descr socket_descr;
     in_ch  = Unix.in_channel_of_descr socket_descr;
   }
  in
  con_cnt_ref := !con_cnt_ref + 1; 
  connections:= new_conn::!connections;
  new_conn
    
let close_connection conn = 
(* If several channels are created on the same descriptor, one of the channels must be closed, but not the others. *)
  Out_channel.close conn.out_ch;
  connections := List.filter (fun x -> not (x.con_id = conn.con_id)) !connections
  

   
(*------------*)


type external_connection = 
    {
     mutable en_connection     : connection; 
     mutable en_score_cache    : float CMap.t;
     mutable en_reg_cls        : CSet.t; (* registed clauses*)
     mutable en_reg_cls_id_map : clause IntMap.t; 
   }


let external_connection = ref None 

let assign_external_connection conn = 
  external_connection := 
    Some 
      { en_connection = conn; 
        en_score_cache = CMap.empty;
        en_reg_cls = CSet.empty;
        en_reg_cls_id_map = IntMap.empty;
      }
      
let external_is_connected () = 
  Option.is_some !external_connection
    
let get_external_connection () = 
  Option.get !external_connection
    
(*-----------*)
let json_delimiter_str = "\n\x00\n"

let out_json_conn conn json = 
  Yojson.Basic.to_channel conn.out_ch json; 
  output_string conn.out_ch json_delimiter_str;
  flush conn.out_ch

let get_json_conn conn = 
  let jstr = channel_first_non_empty_ln conn.in_ch in       
  let json = Yojson.Basic.from_string jstr in
  json

(*-----------*)
let get_clause_enn_conn en_conn cl_id = 
  try 
    IntMap.find cl_id en_conn.en_reg_cls_id_map
  with 
    Not_found -> failwith (sprintf "sockets.ml:get_clause_enn_conn: cl_id %i is not in en_conn" cl_id)

(* currently reset for all components in proof_search_schedule; 
   possible change: cache/reset per component ?
 *)
        
let reset_score_cache en_conn = 
  en_conn.en_score_cache <- CMap.empty
      
(*-----------*)
type component = Input | Inst | Res | Sup | Proof

let comp_to_string comp = 
  match comp with  
  | Input -> "input" 
  | Inst  -> "inst"
  | Res   -> "res" 
  | Sup   -> "sup" 
  | Proof -> "proof"

let component_id_glb = -1

(** All communication messages have json "tag" field 
    tags ending with _req require response from the other communication side ending with _res 
    other tags are for notification of the other side and do not require response. 
    EA are expected to 
*)
    
type tag =
    
 (* iProver requests to EA server *) 
  | Scores_req              (* send by iProver in passive scores mode *)
  | Scores_res              (* send by EA in passive scores mode *)
  | Given_clause_req        (* send by iProver in given clause mode *)
  | Given_clause_res        (* send by EA in given clause mode *)
      
 (* server tag requests *)
  | Server_queries_start    (* send by iProver, requesting start of the server query sequence *)
  | Cls_sat_eval_req        (* send by EA *)
  | Cls_sat_eval_res        (* send by iProver *)
  | Cls_sat_eval_gr_req     (* send by EA *)
  | Cls_sat_eval_gr_res     (* send by iProver *)
  | Sat_solver_exec_req     (* send by EA *)
  | Sat_solver_exec_res     (* send by iProver *)      
  | Server_queries_end      (* send by EA, to finish the query seqence *)

 (* send by iProver *)     
  | Register_clauses       
  | Given_clause  (* actual given clause selected by iProver *)
  | Passive_clauses
  | Simplified_clauses 
  | Moved_from_active_to_passive
  | SZS_result_out 
  | Proof_out
  
let tag_to_string q = 
  match q with 
  | Scores_req  -> "scores_req"
  | Scores_res  -> "scores_res"
  | Given_clause_req -> "given_clause_req"
  | Given_clause_res -> "given_clause_res"
        
  | Server_queries_start   -> "server_queries_start"
  | Cls_sat_eval_gr_req  -> "cls_sat_eval_gr_req"
  | Cls_sat_eval_gr_res  -> "cls_sat_eval_gr_res"
  | Cls_sat_eval_req     -> "cls_sat_eval_req"
  | Cls_sat_eval_res     -> "cls_sat_eval_res"        
  | Sat_solver_exec_req  -> "sat_solver_exec_req"
  | Sat_solver_exec_res  -> "sat_solver_exec_res"        
  | Server_queries_end   -> "server_queries_end"

  | Register_clauses -> "register_clauses"
  | Given_clause -> "given_clause"
  | Passive_clauses -> "passive_clauses"
  | Simplified_clauses -> "simplified_clauses"
  | Moved_from_active_to_passive -> "moved_from_active_to_passive"        
  | SZS_result_out -> "szs_result_out"
  | Proof_out -> "proof_out"


let tag_of_string str = 
  match str with 
  | "scores_req" -> Scores_req
  | "scores_res" -> Scores_res  
  | "given_clause_req" -> Given_clause_req
  | "given_clause_res" -> Given_clause_res
  | "server_queries_start" -> Server_queries_start
  | "cls_sat_eval_gr_req" -> Cls_sat_eval_gr_req
  | "cls_sat_eval_gr_res" -> Cls_sat_eval_gr_res
  | "cls_sat_eval_req" -> Cls_sat_eval_req
  | "cls_sat_eval_res" -> Cls_sat_eval_res
  | "sat_solver_exec_req" -> Sat_solver_exec_req
  | "sat_solver_exec_res" -> Sat_solver_exec_res 
  | "server_queries_end" -> Server_queries_end

  | "register_clauses" -> Register_clauses
  | "given_clause" -> Given_clause 
  | "passive_clauses" -> Passive_clauses
  | "simplified_clauses" -> Simplified_clauses
  | "moved_from_active_to_passive" ->  Moved_from_active_to_passive
  | "szs_result_out" -> SZS_result_out
  | "proof_out" -> Proof_out
        
  | _-> failwith (sprintf "tag_of_string: unsupported value %s" str)
        
(*------*)

let get_tag_json json = 
  let open Yojson.Basic.Util in
  json |> member "tag" |> to_string |> tag_of_string
    
let get_cl_ids_json json = 
  let open Yojson.Basic.Util in
  json |> member "clause_ids" |> to_list |> List.map to_int 

let get_cls_json en_conn json =
  let cl_ids = get_cl_ids_json json in
  List.map (get_clause_enn_conn en_conn) cl_ids

let get_component_id_json json =
  let open Yojson.Basic.Util in
  json |> member "component_id" |> to_int 
  
(*-----------*)
let get_clause_id c = Clause.get_fast_key c

let pp_no_spaces_cluase ppf c =
  let orig_ppf_functions = Format.pp_get_formatter_out_functions ppf () in
  let empty_spaces_ppf_functions =
    {orig_ppf_functions with
     out_newline = (fun () -> ());
     out_spaces = (fun _i -> ());
     out_indent = (fun _i -> ());
   }
  in
  Format.pp_set_formatter_out_functions ppf empty_spaces_ppf_functions;
  TstpProof.pp_clause_with_source_gs ~clausify_proof: false ppf c;
  Format.pp_set_formatter_out_functions ppf orig_ppf_functions
    
let clause_to_string_with_source c =
(*  Format.asprintf "@[%a @]@." (pp_no_spaces_cluase) c *)
      Format.asprintf "%a" (pp_no_spaces_cluase) c

    (*
  Format.asprintf "@[%a @]@."
    (TstpProof.pp_clause_with_source_gs ~clausify_proof: false ) c
*)
  
let clause_params_to_json c =
  `Assoc [
           ("basic_clause_id", `Int (Clause.get_bc_fast_key c));  
           ("conj_dist", `Int (Clause.(conj_dist_to_int (get_conj_dist c))));
           ("born", `Int (Clause.get_ps_when_born c));
           ("horn", `Bool (Clause.is_horn c));
           ("epr",  `Bool (Clause.is_epr c));
         ]


let clause_to_json c = 
  `Assoc [   
(*  ("clause", `String (Clause.to_tptp c)); *)
  ("clause", `String (clause_to_string_with_source c));
  ("clause_id", `Int (get_clause_id c));
  ("clause_features", (clause_params_to_json c));
 ]

let add_reg_clause en_conn cl = 
  en_conn.en_reg_cls <- CSet.add cl en_conn.en_reg_cls; 
  en_conn.en_reg_cls_id_map <- IntMap.add (Clause.get_fast_key cl) cl en_conn.en_reg_cls_id_map

let add_reg_clauses en_conn cls = 
  List.iter (add_reg_clause en_conn) cls


(** Register clauses, other requests related to these clauses will be sent via ids 
    no resonse expected  *)

let register_clauses_to_json clauses = 
  `Assoc [
  ("tag", `String (tag_to_string Register_clauses));
  ("clauses", `List (List.map clause_to_json clauses));  
]
    
let register_clauses en_conn clauses = 
  let conn = en_conn.en_connection in
  let to_register = List.filter (fun c -> not (CSet.mem c en_conn.en_reg_cls)) clauses in 
  if (List.X.is_nonempty to_register) then 
    (
    let json = register_clauses_to_json to_register in

    dbg D_trace @@ lazy (sprintf "registering: %i \n %s" (List.length to_register) (Yojson.Basic.pretty_to_string json));  
    out_json_conn conn json;
    add_reg_clauses en_conn to_register
    )
    
(*-----------------*)
type clause_batch = 
    {
     tag               : tag; 
     clauses           : Clause.clause list; 
     component         : component;
     component_id      : int;
   }

let create_clause_batch ~tag ~component ~component_id ~clauses = 
  {
   tag; 
   clauses; 
   component;
   component_id;
 }


let filter_non_cached_scores en_conn clauses = 
  List.filter (fun c -> not (CMap.mem c en_conn.en_score_cache)) clauses

(*----------------*)
let cl_list_to_ids_json clauses = 
  `List (List.map (fun c -> `Int (get_clause_id c)) clauses)

let clause_batch_to_json cb = 
  `Assoc [
  ("tag", `String (tag_to_string cb.tag));
  ("clause_ids", cl_list_to_ids_json cb.clauses);
  ("component", `String (comp_to_string cb.component)); 
  ("component_id", `Int (cb.component_id));
]

(*
  `Assoc [("batch_clauses", `List (List.map clause_to_json cb.clauses));
          ("component", `String (comp_to_string cb.component)); 
          ("component_id", `Int (cb.component_id));
          ("tag", `String (tag_to_string cb.tag));
        ]
*)


(* Send batch of clauses with some tag
     example of tag: cb.tag = Passive_clauses or cb.tag = Scores *)

let send_batch en_conn cb = 
  let conn = en_conn.en_connection in
  register_clauses en_conn cb.clauses; 
  let cb_json = clause_batch_to_json cb in
  dbg D_trace @@ lazy (sprintf "sending batch: %i \n %s" (List.length cb.clauses) (Yojson.Basic.pretty_to_string cb_json));  
  out_json_conn conn cb_json


      
(*------- Server eval requests -------*)
      
let send_start_server_queries en_conn =
  let json =
    `Assoc [
    ("tag", `String (tag_to_string Server_queries_start));
  ]
  in
  let conn = en_conn.en_connection in
  dbg D_trace @@ lazy (sprintf "sending:\n%s" (Yojson.Basic.pretty_to_string json));
  out_json_conn conn json
    

(*----------*)  
let cl_sat_eval_gr_json clauses lit_val_lists = 
  `Assoc [
  ("tag", `String (tag_to_string Cls_sat_eval_gr_res));
  ("clause_ids", cl_list_to_ids_json clauses);
  ("sat_lit_gr_vals", `List (List.map
                               (fun lit_val_list ->
                                 `List (List.map (fun bv -> `Bool (bv)) lit_val_list))
                               lit_val_lists));    
]

    
let send_cl_sat_eval en_conn ~ground_flag clauses =
  let conn = en_conn.en_connection in
  let open Prop_solver_exchange in
  List.iter add_clause_to_solver clauses;
  let get_solver_lit_val =
    if ground_flag then
        get_solver_lit_val_gr
    else
      get_solver_lit_val
  in
  let lit_val_lists =
    List.map (fun clause -> 
      Clause.get_lits clause |> List.map get_solver_lit_val) clauses in
  let json = cl_sat_eval_gr_json clauses lit_val_lists in
  dbg D_trace @@ lazy (sprintf "sending:\n%s" (Yojson.Basic.pretty_to_string json));
  out_json_conn conn json


(*--------*)    
let sat_solver_exec_res_json res_str =
  `Assoc [
  ("tag", `String (tag_to_string Sat_solver_exec_res));
  ("result", `String res_str); (* sat/unsat*)
]
  
let send_sat_solver_res en_conn res_str =
  let conn = en_conn.en_connection in
  let open Yojson.Basic.Util in
  let json = sat_solver_exec_res_json res_str in
  dbg D_trace @@ lazy (sprintf "sending:\n%s" (Yojson.Basic.pretty_to_string json));
  out_json_conn conn json
  
    
let rec process_server_queries en_conn =
  let conn = en_conn.en_connection in
  let open Yojson.Basic.Util in
  let json = get_json_conn conn in
  let tag = get_tag_json json in
  
  dbg D_trace @@ lazy (sprintf "process_server_queries: %s" (tag_to_string tag));

  match tag with
    (* Start_server_queries is sent by iProver *)
    (*
  |Start_server_queries ->
      process_server_queries en_conn
   *)  
  | Cls_sat_eval_req ->
      let clauses = get_cls_json en_conn json in
      send_cl_sat_eval en_conn ~ground_flag:false clauses;
      process_server_queries en_conn
        
  | Cls_sat_eval_gr_req ->
      let clauses = get_cls_json en_conn json in
      send_cl_sat_eval en_conn ~ground_flag:true clauses;
      process_server_queries en_conn

  | Sat_solver_exec_req ->
      if Prop_solver_exchange.solve ~soft:true () == PropSolver.Unsat
      then 
	(
         send_sat_solver_res en_conn "unsat";
	 raise Unsatisfiable_gr
	)
      else
        (
         send_sat_solver_res en_conn "sat";
         process_server_queries en_conn
        )
        
  | Server_queries_end -> () (* end of server queries *)
        
  |_-> failwith (sprintf "unsupported server tag: %s"  (tag_to_string tag))
 
    
(*------ Scores ----------------------*)

(* assume cb.tag = Scores *)


let update_cache_external_scores_cb en_conn cb = 
  assert(Stdlib.(cb.tag = Scores_req));
  let conn = en_conn.en_connection in
  let new_clauses = filter_non_cached_scores en_conn cb.clauses in 
  if (List.X.is_nonempty new_clauses) then 
    begin
      let cb = {cb with clauses = new_clauses} in 
      send_batch en_conn cb;

      (* process any queries server has *)
      send_start_server_queries en_conn;
      process_server_queries en_conn;
      
      dbg D_trace @@ lazy (sprintf "before: reading");
      let open Yojson.Basic.Util in 
      let scores_json = get_json_conn conn in
      assert(scores_json |> get_tag_json == Scores_res);
      let scores_list = scores_json |> member "scores" |> to_list |> List.map to_float in 
      dbg D_trace @@ lazy (sprintf "reading:\n%s" (Yojson.Basic.pretty_to_string scores_json));
      dbg D_trace @@ lazy (sprintf "reading: %s" (list_to_string string_of_float scores_list ","));
      let new_score_cache = 
        List.fold_left2 (fun cache clause score -> CMap.add clause score cache) en_conn.en_score_cache cb.clauses scores_list in
      en_conn.en_score_cache <- new_score_cache 
    end
  else () (* all clauses in batch are already in score cache *)

let get_clause_external_score en_conn c  = 
  try 
    CMap.find c en_conn.en_score_cache
  with 
    Not_found -> 
      failwith "sockets.ml: get_clause_external_score: clauses should be cached in batch before getting individual scores"

let update_cache_external_scores en_conn ~component ~component_id clauses = 
  let clause_batch = create_clause_batch ~tag:Scores_req ~component ~component_id ~clauses in
  update_cache_external_scores_cb en_conn clause_batch
  
let cmp_clause_external c_1 c_2 = 
  let en_conn = get_external_connection () in 
  let score_1 = get_clause_external_score en_conn c_1 in 
  let score_2 = get_clause_external_score en_conn c_2 in 
  Stdlib.compare score_1 score_2



(*----- SZS -----*)

let szs_str_to_json szs_str =     
  `Assoc [
  ("tag", `String (tag_to_string SZS_result_out));
  ("szs_status",  `String szs_str);
]

let send_szs_result conn szs_str = 
  let szs_json = szs_str_to_json szs_str in
  dbg D_trace @@ lazy (sprintf "writing szs_statis:\n%s" (Yojson.Basic.pretty_to_string szs_json));     
  out_json_conn conn szs_json
    

(*--- Proof -------*)

(* new clauses are clauses which were not registered in the connection before *)

let proof_to_json proof_cls =     
  `Assoc [
  ("tag", `String (tag_to_string Proof_out));
  ("proof_clauses",  cl_list_to_ids_json proof_cls);
]

let send_proof en_conn proof_clauses = 
  let proof_batch = create_clause_batch 
      ~tag:Proof_out ~component:Proof ~component_id:component_id_glb ~clauses:proof_clauses in   
  send_batch en_conn proof_batch
  
   
(*---- Getting given clause ------*)

let given_clause_request_json ~component ~component_id = 
  `Assoc [
  ("tag", `String (tag_to_string Given_clause_req));
  ("component", `String (comp_to_string component)); 
  ("component_id", `Int (component_id));
]


(* expects json: {passive_is_empty:bool;  given_cluase:clause_id} *)    

let get_given_clause en_conn ~component ~component_id = 
  let conn = en_conn.en_connection in
  
  let request_json = given_clause_request_json ~component ~component_id in  
  dbg D_trace @@ lazy (sprintf "given clause request: %s" (Yojson.Basic.pretty_to_string request_json));  
  out_json_conn conn request_json; 

  (* process any queries server has *)
  send_start_server_queries en_conn;
  process_server_queries en_conn;
 
  let open Yojson.Basic.Util in
  let given_cl_json = get_json_conn conn in
  
(* passive_is_empty ? *)  
  let passive_is_empty = given_cl_json |> member "passive_is_empty" |> to_bool in
  if passive_is_empty then
    (
     dbg D_trace @@ lazy (sprintf "component %i: given_clause: passive_is_empty" component_id);
     None
       )
  else
    (
     let given_cl_id = given_cl_json |> member (tag_to_string Given_clause)  |> to_int in
     let given_cl = get_clause_enn_conn en_conn given_cl_id in    
     dbg D_trace @@ lazy (sprintf "getting: given_clause \n%s" (Clause.to_tptp given_cl));
     Some(given_cl)
    )


(*---- Sending given clause ------*)

let send_given_clause en_conn ~component ~component_id clause = 
  let clause_batch = create_clause_batch ~tag:Given_clause ~component ~component_id ~clauses:[clause] in   
  send_batch en_conn clause_batch 

(*---- Simplified ------*)

let send_simplified_clauses en_conn ~component ~component_id clauses = 
  if (List.X.is_nonempty clauses) 
  then 
    let clause_batch = create_clause_batch ~tag:Simplified_clauses ~component ~component_id ~clauses in   
    send_batch en_conn clause_batch 
  else 
    () (* empty clauses *)

(*---- Passive ------*)

let send_passive_clauses en_conn ~component ~component_id clauses = 
  if (List.X.is_nonempty clauses) 
  then 
    let clause_batch = create_clause_batch ~tag:Passive_clauses ~component ~component_id ~clauses in   
    send_batch en_conn clause_batch 
  else 
    () (* empty clauses *)

(*---- Active to Passive ----*)

let send_moved_from_active_to_passive en_conn ~component ~component_id clauses = 
  if (List.X.is_nonempty clauses) 
  then 
    let clause_batch = create_clause_batch ~tag:Moved_from_active_to_passive ~component ~component_id ~clauses in   
    send_batch en_conn clause_batch 
  else 
    () (* empty clauses *)
      


