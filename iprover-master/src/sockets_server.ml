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

(* mock socket server which reads json {"clause" :"clause"} and outputs float score 1/clause_length *)

open Lib
open Options
open Logic_interface 
open Sockets
  
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

let _ = out_str "%---------------- External Agent Test Server \n\n "
    
(*----- debug -----*)

let addr = Unix.ADDR_INET(Unix.inet_addr_of_string !global_options.external_ip_address, !global_options.external_port)

let s_descr = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 

let _ = dbg D_trace @@ lazy (sprintf "before:Unix.bind s_descr addr") 

let () = Unix.bind s_descr addr

let _ = dbg D_trace @@ lazy (sprintf "before: Unix.listen s_descr")
let () = Unix.listen s_descr 2 (* upto 2 connections *)

let _ =  dbg D_trace @@ lazy (sprintf "after: Unix.listen s_descr")

(* will wait until connected *)
let (sock_descr, sock_addr) = Unix.accept s_descr 

(* connected *)
let in_ch = Unix.in_channel_of_descr sock_descr
let out_ch = Unix.out_channel_of_descr sock_descr

let out_json_ch ch json = 
  Yojson.Basic.to_channel ch json; 
  output_string out_ch "\n";
  flush out_ch    
  
let mock_score clause_str = 
  let str_length = String.length clause_str in
  let inv_str_length = 1.0 /. (float_of_int str_length) in 
  inv_str_length

(*------------*)
let score_list_to_json scores = 
  `Assoc [
  ("tag", `String (tag_to_string Scores_res));
  ("scores", `List (List.map (fun score -> `Float (score)) scores))
]

(*------------*)


(* flush after writing! *)

(*
type server_state = 
    {
     mutable st_clauses : CSet.t; 
     mutable st_cls_id_map : clause IntMap.t; 
     mutable st_active : CSet.t; 
     mutable st_passive : clause list;
   }

let create_st () = 
  {
   st_clauses = CSet.empty;
   st_cls_id_map = IntMap.empty;
   st_active = CSet.empty;
   st_passive = [];
}

(*------*)
let add_reg_clause st cl = 
  st.st_clauses <- CSet.add cl st.st_clauses; 
  st.st_cls_id_map <- IntMap.add (Clause.get_fast_key cl) cl st.st_cls_id_map
      
let add_reg_clauses en_conn cls = 
  List.iter (register_sent_clause en_conn) cls

*)

type server_state = 
    {
     mutable st_clauses : StrSet.t; 
     mutable st_cls_id_map : string IntMap.t; 
     mutable st_active : (int list) IntMap.t;  (* component_id -> st_active *) 
     mutable st_passive : (int list) IntMap.t; (* component_id -> st_passive *)
   }

let create_st () = 
  {
   st_clauses = StrSet.empty;
   st_cls_id_map = IntMap.empty;
   st_active = IntMap.empty;
   st_passive = IntMap.empty;
}

(*------*)

let add_reg_clause st (cl_id, cl) = 
  st.st_clauses <- StrSet.add cl st.st_clauses; 
  st.st_cls_id_map <- IntMap.add cl_id cl st.st_cls_id_map
      
let add_reg_clauses st cls_ids = 
  List.iter (add_reg_clause st) cls_ids

let get_st_cl st cl_id = 
  IntMap.find cl_id st.st_cls_id_map

let get_st_cls st cl_ids = 
  List.map (get_st_cl st) cl_ids

(*------*)
let add_to_passive st comp_id cl_ids = 
  st.st_passive <- IntMap.update comp_id
      (fun comp_pass_opt ->
        match comp_pass_opt with 
        |Some(comp_pass) -> Some(comp_pass@cl_ids) (* add at the end: replace by a queue *)
        |None -> Some(cl_ids)
      ) st.st_passive
  

  
let get_given_clause st comp_id =
  try
    let comp_passive = IntMap.find comp_id st.st_passive in 
    match comp_passive with 
    |h::tl -> 
        let new_passive = tl in
        st.st_passive <- IntMap.add comp_id new_passive st.st_passive; (* [] can be added but it is ok *)
        st.st_active <-
          IntMap.update comp_id
            (fun comp_active_opt ->
              match comp_active_opt with
                Some(comp_active) -> Some(h::comp_active)
              |None -> Some([h])
            )
            st.st_active;
        Some(h)
    |[] -> None
  with
    Not_found -> None

let given_opt_to_json comp_id given_opt =
  let given_cl_tag = ("tag", `String (tag_to_string Given_clause_res)) in
  let comp_el = ("component_id",`Int(comp_id)) in 
  match given_opt with 
  |Some cl_id -> 
      `Assoc [
      given_cl_tag;
      comp_el;
      ("passive_is_empty",`Bool(false));
      ("given_clause", `Int(cl_id))        
    ]
  |None -> 
      `Assoc [
      given_cl_tag;
      comp_el;
      ("passive_is_empty",`Bool(true));
    ]

(*------*)

(* batch clauses processing *) 

 
        
let batch_json_get_cls st json = 
  let cl_ids = get_cl_ids_json json in 
  List.map (fun id -> IntMap.find id st.st_cls_id_map) cl_ids
   
(*------*)
(*
let get_tag json = 
  let open Yojson.Basic.Util in
  json |> member "tag" |> to_string
*)


(*---------- matching over all queries -------------------*)

let rec process_tag st json = 
  let open Yojson.Basic.Util in
  let tag = get_tag_json json in
  dbg D_trace @@ lazy (sprintf "process tag: %s" (tag_to_string tag));
  match tag with 
  | Register_clauses -> 
      (
       let cls_ids = 
         json |> member "clauses" |> to_list |> 
         List.map (fun jclause ->  
           let cl_id = (member "clause_id" jclause) |> to_int in
           let cl_str = (member "clause" jclause) |> to_string in
           (cl_id, cl_str)
                  )
       in       

       add_reg_clauses st cls_ids;

       dbg D_trace @@ lazy (sprintf "registring clause_ids: %i \n%s" 
                              (List.length cls_ids) (list_to_string (fun (_cl_id,cl) -> cl) cls_ids "\n"));

      )
  
  | Moved_from_active_to_passive | Passive_clauses -> 
      let cl_ids = get_cl_ids_json json in
      let comp_id = get_component_id_json json in 
      dbg D_trace @@ lazy (sprintf "component: %i passive clauses: %i \n[%s]\n" 
                             comp_id (List.length cl_ids) (list_to_string string_of_int cl_ids ","));
      
      add_to_passive st comp_id cl_ids

  | Given_clause_req ->
      let comp_id = get_component_id_json json in 
      let given_opt = get_given_clause st comp_id in 
      let json_given = given_opt_to_json comp_id given_opt in
      (* process server queries *)
      (match given_opt with
      | Some (cl_id) -> process_sever_queries st [cl_id]
      | None ->
          let jstr = channel_first_non_empty_ln in_ch in       
          let json = Yojson.Basic.from_string jstr in
          let open Yojson.Basic.Util in
          let tag = get_tag_json json in
          assert(tag == Server_queries_start);
          dbg D_trace @@ lazy (sprintf "json in:\n %s\n" (Yojson.Basic.pretty_to_string json));  

          let out_json =
            `Assoc [
            ("tag", `String (tag_to_string Server_queries_end)); (* end server queries*)
          ]
          in
          dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string out_json));
          out_json_ch out_ch out_json 
      );
      
      dbg_env D_trace 
        (fun () ->
          match given_opt with 
          | Some cl_id -> 
              let cl = IntMap.find cl_id st.st_cls_id_map in
              dbg D_trace @@ lazy (sprintf "component: %i given clause: \n %s\n" comp_id cl);
          |None ->  dbg D_trace @@ lazy (sprintf "compontn: %i passive is empty\n" comp_id);
        );
      dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string json_given));  
      out_json_ch out_ch json_given
             
  | Given_clause  ->  ()


  | Scores_req -> 

      let cls = batch_json_get_cls st json in
      dbg D_trace @@ lazy (sprintf "scoring clauses: %i \n %s\n" 
                             (List.length cls) (list_to_string id_fun cls "\n"));

      let cl_ids = get_cl_ids_json json in
      process_sever_queries st cl_ids;
      
      let scores = List.map mock_score cls in      
      let json_scores = score_list_to_json scores in
      dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string json_scores));  

      out_json_ch out_ch json_scores

  | SZS_result_out  -> 
      let szs_str =  json |> member "szs_status" |> to_string in 
      out_str (sprintf "%s\n" szs_str)
      
  | Proof_out -> ()
                       
  | Simplified_clauses  -> () (* todo:  remove from passive *)



  | _ -> failwith (sprintf "json tag is not supported: %s" (tag_to_string tag))

and
    
    process_sever_queries st cl_ids =
  let jstr = channel_first_non_empty_ln in_ch in       
  let json = Yojson.Basic.from_string jstr in
  let open Yojson.Basic.Util in
  let tag = get_tag_json json in
  dbg D_trace @@ lazy (sprintf "json in:\n %s\n" (Yojson.Basic.pretty_to_string json));  
  dbg D_trace @@ lazy (sprintf "processing tag: %s \n" (tag_to_string tag));
  match tag with     
  | Server_queries_start ->
      generate_server_queries st cl_ids
  | _ -> failwith (sprintf "process_sever_queries: should start with %s" (tag_to_string Server_queries_start))

and
    generate_server_queries st cl_ids =
  let cl_ids_to_json_list cl_ids =
    `List (List.map (fun id -> `Int(id)) cl_ids) in
  let cl_ids_json_list = cl_ids_to_json_list cl_ids in
  let eval_gr_out_json =
    `Assoc [
    ("tag", `String (tag_to_string Cls_sat_eval_gr_req));
    ("clause_ids", cl_ids_json_list);
  ]
  in
  dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string eval_gr_out_json));  
  out_json_ch out_ch eval_gr_out_json;

  let jstr = channel_first_non_empty_ln in_ch in
  
  let in_json = Yojson.Basic.from_string jstr in
  dbg D_trace @@ lazy (sprintf "json in:\n %s\n" (Yojson.Basic.pretty_to_string in_json));

  let out_json =
    `Assoc [
    ("tag", `String (tag_to_string  Sat_solver_exec_req));
  ]
  in
      
  dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string out_json));
  out_json_ch out_ch out_json;

  let jstr = channel_first_non_empty_ln in_ch in
  
  let in_json = Yojson.Basic.from_string jstr in
  dbg D_trace @@ lazy (sprintf "json in:\n %s\n" (Yojson.Basic.pretty_to_string in_json));

  (* try clause eval again after sat solver *)
  dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string eval_gr_out_json));
  out_json_ch out_ch eval_gr_out_json;

  let jstr = channel_first_non_empty_ln in_ch in
  let in_json = Yojson.Basic.from_string jstr in
  dbg D_trace @@ lazy (sprintf "json in:\n %s\n" (Yojson.Basic.pretty_to_string in_json));
      
  let out_json =
    `Assoc [
    ("tag", `String (tag_to_string Server_queries_end)); (* end server queries*)
  ]
  in
  dbg D_trace @@ lazy (sprintf "json out:\n %s\n" (Yojson.Basic.pretty_to_string out_json));
  out_json_ch out_ch out_json
    
(*---- main loop -------*)
let _ = 
  let st = create_st () in 
  try 
    while true do 
      dbg D_trace @@ lazy (sprintf "while loop\n");
(*      let jstr = input_line in_ch in *)
      let jstr = channel_first_non_empty_ln in_ch in       
      let json = Yojson.Basic.from_string jstr in
      dbg D_trace @@ lazy (sprintf "read in:\n %s\n" (Yojson.Basic.pretty_to_string json));  

      process_tag st json
    done
  with End_of_file ->
    out_str "end of input; exiting"
