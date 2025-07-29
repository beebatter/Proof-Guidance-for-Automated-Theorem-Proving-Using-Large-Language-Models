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
open Logic_interface 


(*----- debug modifiable part-----*)

let dbg_flag = false

type dbg_gr = 
  | D_trace
  | D_trace_param 
  | D_parents
  | D_dot_red

let dbg_gr_to_str = function 
  | D_trace   -> "trace"
  | D_trace_param   -> "trace_param"
  | D_parents -> "parents"
  | D_dot_red -> "dot_red"

let dbg_groups =
  [
   D_trace; 
   D_trace_param;
   D_parents; 

   D_dot_red;
 ]
    
let module_name = "tstpProof"

(*----- debug fixed part --------*)

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy = 
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f = 
  Lib.dbg_env_set dbg_flag dbg_groups group f
    
(*----- debug -----*)


(*----------------- KK ------------------*)

type fof_map = (string * (int list)) IntMap.t (* maps fof_id into (fof_string, parents_ids) *)

let get_clausification_map () = 
  if    
    (* Must output clausification for FOF problems *)
    (Poly.(!Parser_types.input_problem_type = Some Parser_types.FOF || !Parser_types.input_problem_type = Some Parser_types.TFF))
      &&    
    (* but cannot clausify again when input is stdin *)
    (not !global_options.stdin)
      
  then
    begin

     (* Get command and options for clausification *)
     let clausify_cmd, clausify_arg =
       ParseFiles.clausifier_cmd_options ~pure_cnf_flag:false
     in
     
     (* $TPTP for includes has already been set in ParseFiles on
	first clausification *)
     
     (* Array of arguments for clausify command *)
     let clausify_cmd_args =
       Array.of_list
	 (clausify_cmd ::
	  (Str.split (Str.regexp "[ ]+") (clausify_arg)))
     in    
     (
      
      try
	  (* Clausify one input file and store proof in hash
	     table *)
	let clausify_file clausification_map file =
	 
          (* Additional arguments for clausification proof *)
	  dbg D_trace (lazy ( "Using clausifier: "^ clausify_cmd^"\n"));
	  let clausify_cmd_args_proof = 
	    match Filename.basename clausify_cmd with
	    | "vclausify_rel" -> 	(* Vampire clausifier *)
		[| 
                   "--print_clausifier_premises"; "on";
		   "--proof"; "tptp";
                   "--output_axiom_names"; "on";
(*                  "--input_syntax"; "tptp"; *)
                   (* "--mode"; "tclausify"; *)
		   "--input_file"; file
                 |]

	    | "eprover" -> (* E clausifier *)
		dbg D_trace (lazy ("\n Using E clausifier\n"));
		[| "--proof-object"; file |]

	    |_-> raise Not_found 
	  in
	  
	  let clausify_cmd_args' =  Array.append clausify_cmd_args clausify_cmd_args_proof in
	    
          dbg D_trace (lazy ( "clausify_cmd_args': "^(String.concat " " (Array.to_list clausify_cmd_args'))^"\n"));

	  (* Copy of environment *)
	  let env = Unix.environment () in
	    
	  (* Create pipe for stderr of command *)
	  let cmd_stderr_out, cmd_stderr_in =
	    Unix.pipe ()
	  in
	    
	  (* Create input channel of pipe output *)
	  let cmd_stderr_out_ch =
	    Unix.in_channel_of_descr cmd_stderr_out
	  in
	    
	  (* Open /dev/null for reading *)
	  let devnull_in =
	    Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o640
	  in
	    
	  (* Open /dev/null for writing *)
	  let devnull_out =
	    Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o640
	  in
	    (* in vclausify_rel output is split: fof part into stderr and cnf into stdout *)
	    (* Create process *)
	    let cmd_pid =
	      Unix.create_process_env
		clausify_cmd
		clausify_cmd_args'
		env
		devnull_in
(*		devnull_out  *)
		cmd_stderr_in  
		cmd_stderr_in
	    in	    
	    (* Close all files of process to prevent blocks *)
	    Unix.close devnull_in;
	    Unix.close devnull_out;
	    Unix.close cmd_stderr_in; 
	    
            dbg D_trace (lazy ( "ext clausify: finished")); 

	    (* Parse output of clausifier *)
	    let clausification_map =
	      Lexer_fof.parse clausification_map cmd_stderr_out_ch
	    in
            dbg D_trace (lazy ( "parse: finished")); 

	    (* Wait for process to terminate *)
	    let _cmd_pid_, _cmd_status =
	      Unix.waitpid [Unix.WUNTRACED] cmd_pid
	    in
            dbg D_trace (lazy ( "clausify process: finished")); 
	    Unix.close cmd_stderr_out;
            clausification_map
	in
	let clausification_map = 
          List.fold_left clausify_file IntMap.empty !global_options.problem_files 
        in
	(* Clausify all input files again *)

        Some(clausification_map)

      with Not_found ->   
        (
         dbg D_trace (lazy ("tstpProof: Unsupported clausifier "));
         None
        )   (* Unsupported clausifier *)
          
     )
    end
 else 
    (None)
  


(* fof_id from names in the sources of leaf clauses *)
(* can raise Not_found if name is not of the specified format *)

let get_fof_id_from_name name =
  try 
    Scanf.sscanf name "u%d" (function i -> i) (* vclausify id *)
  with  Scanf.Scan_failure _ ->
    try 
      Scanf.sscanf name "c_0_%d" (function i -> i) (* eprover id *)
    with 
      Scanf.Scan_failure _ -> raise Not_found 


(*-----------------*)
let prop_impl_justification_fun max_clause_id clause =
  try
    let parents =
      Prop_solver_exchange.justify_prop_impl
        max_clause_id
        clause
    in
    parents
  with
  |Failure _ ->
      (
       out_warning ("tstpProof.ml:global_subsumption:failed to get parents for"
                    ^(Clause.to_string clause));
       []
      )

(*--------------- New Proof Graph ---------*)

type proof_formula = 
  | Int_clause of clause 
  | Ext_formula of int * string * (int list)
      (* (fof_id, formula_str, parents_ids); note fof_ids can clash with clause ids *)

let proof_formula_to_string pf = 
  match pf with 
  | Int_clause c -> Clause.to_tptp c 
  | Ext_formula (_id, s, _) -> s

let pf_is_ext pf = 
  match pf with 
  | Ext_formula _ -> true 
  | _ -> false

let pf_is_cl pf = 
  match pf with 
  | Int_clause _ -> true 
  | _ -> false

let clause_to_pf c = Int_clause c
let clauses_to_pf cls = List.map clause_to_pf cls

let pf_to_clause pf = match pf with Int_clause c -> c | _ -> raise Not_found
let pf_to_clauses pfs = List.map pf_to_clause pfs

module PFKey = struct
  type t      = proof_formula

  let equal pf1 pf2  = 
    match (pf1, pf2) with 
    | (Int_clause c1, Int_clause c2) -> 
        Clause.equal c1 c2
    | (Ext_formula (f1_id,_,_), Ext_formula (f2_id,_,_)) -> f1_id = f2_id
    | _ -> false
          
  let compare pf1 pf2  = 
    match (pf1, pf2) with
    | (Int_clause c1, Int_clause c2) -> 
        Clause.compare c1 c2
    | (Ext_formula (f1_id,_,_), Ext_formula (f2_id,_,_)) -> Stdlib.compare f1_id f2_id
    | (Ext_formula _, Int_clause _) -> -1 (* this order is used in topological order so fof comes first *) 
    | (Int_clause _, Ext_formula _) -> 1  

  let hash pf = 
    match pf with 
    | Int_clause c -> Clause.hash c
    | Ext_formula (f_id,_,_) -> f_id
end


module PFMap = Map.Make(PFKey)
    
module PFSet = Set.Make(PFKey)

(* insted use Graph.Persistent.Digraph.ConcreteBidirectionalLabeled ?*)

(* module DGS = Graph.Persistent.Digraph.Concrete(PFKey) *)
module DGS = Graph.Persistent.Digraph.ConcreteBidirectional(PFKey) 

(*-------- pp individual clause with prop just.  ---------*)

let pp_clause_with_source_gs ?(clausify_proof = true) ppf clause =
  Clause.pp_clause_with_source ppf 
    ~prop_impl_justification_fun:(Some(prop_impl_justification_fun)) 
    ~clausify_proof clause;
  dbg_env D_trace_param 
    (fun () ->            
      Format.printf "@[%a @]@." 
        (Clause.pp_clause_params  Clause.param_out_list_all) clause;
    )

(*-------- as above for pf  ---------*)
let pp_pf_with_source_gs ?(clausify_proof = true) ppf pf =
  match pf with 
  | Ext_formula (_fof_id, fof_str, _fof_parents)  ->  
      Format.fprintf ppf "%s@." fof_str

  | Int_clause clause -> pp_clause_with_source_gs ~clausify_proof ppf clause


(*---------------------------*)
type proof_graph = DGS.t

type proof = 
    {
     pf_graph         : proof_graph;
     pf_nodes         : PFSet.t; (* all formulas in the proof *)
     pf_leaves        : PFSet.t; 
     pf_cnf_leaves_upto_fof : CSet.t; (* note this is different from (pf_leaves - fof) *)

(* fixed at init *)
     pf_roots         : proof_formula list; (* root formulas *)
     pf_fof_map       : fof_map; (* clausification map *)
   }

(* pf_fof_map should be created first *)
let init_proof pf_fof_map pf_roots = 
  {
   pf_graph = DGS.empty; 
   pf_nodes = PFSet.empty; 
   pf_leaves = PFSet.empty; 
   pf_cnf_leaves_upto_fof = CSet.empty;
   pf_roots;
   pf_fof_map;
 }

let get_roots proof = proof.pf_roots

(* proof_graph maps parents -> concl *)
let add_pg pgraph concl parents = 
  List.fold_left (fun pgraph parent -> DGS.add_edge pgraph parent concl) pgraph parents

let add_internal_cl_pg pgraph concl parents = 
  let int_concl =  Int_clause concl in
  List.fold_left (fun pgraph parent -> DGS.add_edge pgraph (Int_clause parent) int_concl) pgraph parents

let add_edge_pf_pg pgraph concl parents = 
  List.fold_left (fun pgraph parent -> DGS.add_edge pgraph parent concl) pgraph parents
    
(* can raise Not_found *)

let get_pf_fof_map fof_map fof_id = 
  let (fof_str, parents) = IntMap.find fof_id fof_map in 
  Ext_formula(fof_id, fof_str, parents)

(* ignore if not in the map *)

let get_pf_list_fof_map fof_map fof_ids = 
  if Stdlib.(fof_map != IntMap.empty) then 
    let f rest fof_id = 
      try 
        let pf = get_pf_fof_map fof_map fof_id in 
        pf::rest
      with 
        Not_found ->
          (
           out_warning (sprintf "extend_proof: missing formula from the clausifier: %i" fof_id);
           rest
          )
    in
    List.fold_left f [] fof_ids
  else []
              
(*----------- proof construction ----------------*)

let rec extend_proof ~proof ?(ignore_parents=(fun x-> false)) pf_accum =
  match pf_accum with   
  | [] -> proof
	
  | (pf :: tl) when ((ignore_parents pf) || (PFSet.mem pf proof.pf_nodes)) ->

      dbg D_parents @@ lazy 
                         (sprintf (if (ignore_parents pf) then "ignored: %s" else "visited: %s") (proof_formula_to_string pf));
      
      extend_proof ~proof ~ignore_parents tl
        
  | pf::tl -> (* New formula *)
      
      dbg D_parents @@ lazy (sprintf "new: %s" (proof_formula_to_string pf));

      let proof = 
        {proof with 
         pf_nodes = PFSet.add pf proof.pf_nodes;}
       in
      begin 
        match pf with 
        | Int_clause clause ->
            begin
              dbg D_parents @@ lazy 
              (Format.asprintf  "clause: \n @[%a @]@."
                 (Clause.pp_clause_with_source ~prop_impl_justification_fun:None ~clausify_proof:false) clause);
              
              (* Get source of clause *)
              match Clause.get_tstp_source clause with                
          (* Clause is either from a file or from clausifier output *)
              | Clause.TSTP_external_source (Clause.TSTP_file_source (_, fof_name)) ->
                  begin
                  try 
                    (* Scan name of clausfied fof name into id *)
	            let fof_id = get_fof_id_from_name fof_name in
                    try 
                      let fof_formula = get_pf_fof_map proof.pf_fof_map fof_id in 
                      let proof = 
                        {
                         proof with 
                         pf_graph = add_edge_pf_pg proof.pf_graph pf [fof_formula];
                         pf_cnf_leaves_upto_fof = CSet.add clause proof.pf_cnf_leaves_upto_fof;
                       }
                      in
                      let pf_accum = fof_formula :: tl in
                      extend_proof ~proof ~ignore_parents pf_accum                        
                    with 
                      Not_found -> 
                        ( 
                          if Stdlib.(proof.pf_fof_map != IntMap.empty) then 
                            out_warning (sprintf "extend_proof: missing formula from the clausifier: %s" fof_name);

                          extend_proof ~proof ~ignore_parents tl
                         )
                  with 
                    Not_found -> (* from a file rather than clausifier *)
                      let proof = 
                        {
                         proof with 
                         pf_graph = DGS.add_vertex proof.pf_graph pf;
                         pf_leaves  = PFSet.add pf  proof.pf_leaves; 
                         pf_cnf_leaves_upto_fof = CSet.add clause proof.pf_cnf_leaves_upto_fof;
                       }
                      in
                      extend_proof ~proof ~ignore_parents tl

                  end (* Clause.TSTP_external_source *)

             (* Clause is from theory axioms *)
             (* or obtained from init/lemmas by lifting them up *)

              | Clause.TSTP_inference_record (Clause.TSTP_bmc1_init_or_property_axiom, _)
              | Clause.TSTP_external_source (TSTP_theory _)
              | Clause.TSTP_internal_source _ ->
	          let proof = 
                    {
                     proof with 
                     pf_graph = DGS.add_vertex proof.pf_graph pf;
                     pf_leaves  = PFSet.add pf  proof.pf_leaves; 
                     pf_cnf_leaves_upto_fof = CSet.add clause proof.pf_cnf_leaves_upto_fof;
                   }
                  in
                  extend_proof ~proof ~ignore_parents tl
                  
                  
	                (* Clause is from global propositional subsumption *)
              | Clause.TSTP_inference_record
	          (Clause.Global_subsumption max_clause_id, [parent]) ->
	                
	                (* Get justification for simplification of clause *)
	                let parents' = prop_impl_justification_fun max_clause_id clause in
	                (
                         dbg D_parents (lazy ("TSTP_inference_record: Global_subsumption: parent:"
                                              ^(Clause.to_tptp parent)));      
                         dbg D_parents (lazy ("TSTP_inference_record: Global_subsumption: parents:"
                                              ^(Clause.clause_list_to_tptp parents')));


                         let parents = parent::parents' in
                         let pf_parents = clauses_to_pf parents in 
                         let just_source = Clause.tstp_source_global_subsumption_just parents in 
                         Clause.reassign_tstp_source just_source clause; 
                                                 
                         let proof = 
                           {
                            proof with 
                            pf_graph = add_edge_pf_pg proof.pf_graph pf pf_parents;
                          }
                         in
                         let pf_accum = pf_parents @ tl in
                         extend_proof ~proof ~ignore_parents pf_accum
                           

	                ) 

                  | Clause.TSTP_inference_record
	              (Clause.Prop_impl max_clause_id, []) ->
	   
	                (* Get justification for simplification of clause *)
                        let parents = prop_impl_justification_fun max_clause_id clause in 
	    
                        dbg D_parents @@ lazy (sprintf "TSTP_inference_record: Global_subsumption: parents: %s"
                                                 (Clause.clause_list_to_tptp parents));

                        let just_source = Clause.tstp_source_prop_impl_just parents in 
                        Clause.reassign_tstp_source just_source clause; 

                        let pf_parents = clauses_to_pf parents in 
                        let proof = 
                          {
                           proof with 
                           pf_graph = add_edge_pf_pg proof.pf_graph pf pf_parents;
                         }
                        in
                        let pf_accum = pf_parents @ tl in
                        extend_proof ~proof ~ignore_parents pf_accum
                    

(* Clause is from other inference *)
                  | Clause.TSTP_inference_record (_, parents) ->
                      
                      let pf_parents = clauses_to_pf parents in 
                      let proof = 
                        {
                         proof with 
                         pf_graph = add_edge_pf_pg proof.pf_graph pf pf_parents;
                       }
                      in
                      let pf_accum = pf_parents @ tl in
                      extend_proof ~proof ~ignore_parents pf_accum
                                            
            end

        | Ext_formula (fof_id, fof_str, parent_ids) ->
            
            let pf_parents = get_pf_list_fof_map proof.pf_fof_map parent_ids in
                        
            let (pf_graph, pf_leaves) =
              if (List.X.is_nonempty pf_parents) then
                (add_edge_pf_pg proof.pf_graph pf pf_parents, proof.pf_leaves)
              else
                (DGS.add_vertex proof.pf_graph pf, PFSet.add pf proof.pf_leaves)
            in  
            let proof = 
              {proof with 
               pf_graph;
               pf_leaves;
             }
            in
            let pf_accum = pf_parents @ tl in
            extend_proof ~proof ~ignore_parents pf_accum
      end
        
let get_proof ?(with_clausification=true) ?(ignore_parents=(fun x-> false)) pf_formulas =  
  let pf_fof_map = 
    if with_clausification then      
      match (get_clausification_map ()) with 
      |Some(fof_map) -> fof_map 
      |None -> IntMap.empty            
    else 
      IntMap.empty
  in
  let proof = init_proof pf_fof_map pf_formulas in
  extend_proof ~proof ~ignore_parents pf_formulas 
        
let get_proof_clauses ?(with_clausification=true) ?(ignore_parents=(fun x-> false)) clauses = 
  get_proof ~with_clausification ~ignore_parents (clauses_to_pf clauses)
    

(*---------------------------*)

(* topolgical and stable: Ext_formula are first then Int_clauses *)

module DGS_TOPO = Graph.Topological.Make_stable(DGS)
 

(* lists all formulas in the proof graph in topological order *)
let pf_graph_to_list pf_graph = 
  List.rev (DGS_TOPO.fold (fun v rest -> v::rest) pf_graph [])

(* lists all formulas in the proof in topological order *)
let proof_to_list proof  = 
  let proof_list = pf_graph_to_list proof.pf_graph in 
  proof_list

let proof_to_list_cls proof = 
  let proof_list = pf_graph_to_list proof.pf_graph in 
  proof_list |> List.filter pf_is_cl |> pf_to_clauses 

let get_leaves proof = 
  PFSet.elements proof.pf_leaves 

(* excluding clausification *)
let get_leaves_cls proof = 
  CSet.elements proof.pf_cnf_leaves_upto_fof


let pp_tstp_proof ppf proof =
  Statistics.(time out_proof_time) @@ fun () -> 
    let proof_list = proof_to_list proof in
    List.iter (pp_pf_with_source_gs ppf) proof_list 


(*-------------------------*)
let get_proof_graph proof = proof.pf_graph 

(*------------ reduce graph -------------*)

(* nodes are assumed to be reduced *)
let rec reduce_proof_graph' filter proof_graph nodes accum_graph =  
  match nodes with 
    | [] -> accum_graph 
    | h::tl -> 
        dbg D_dot_red @@ lazy (sprintf "node: %s\n " 
                                 (proof_formula_to_string h));

        let pred_nodes = DGS.pred proof_graph h in
        let reduced_pred_nodes = reduce_nodes filter proof_graph pred_nodes [] in  
        dbg D_dot_red @@ lazy (sprintf "reduced_pred_nodes: \n %s\n " 
                                 ((list_to_string proof_formula_to_string) reduced_pred_nodes "\n"));

(* add all reduced_pred_nodes -> h *)
        let accum_graph = List.fold_left (fun gr red -> DGS.add_edge gr red  h) accum_graph reduced_pred_nodes in 
        let nodes = reduced_pred_nodes@tl in
        reduce_proof_graph' filter proof_graph nodes accum_graph
and 
    reduce_nodes filter proof_graph nodes acc = 
  match nodes with 
  |[] -> acc 
  |h :: tl -> 
      if (filter h) then 
        let acc = h::acc in 
        reduce_nodes filter proof_graph tl acc 
      else
        let pred_nodes = DGS.pred proof_graph h in 
        let tl = pred_nodes@tl in
        reduce_nodes filter proof_graph tl acc 

let reduce_proof_graph ~filter ~proof_graph root_nodes = 
  let nodes = reduce_nodes filter proof_graph root_nodes [] in (* reduce nodes *)
  reduce_proof_graph' filter proof_graph nodes DGS.empty

(*------ DOT output ----------*)

module ProofDot = Graph.Graphviz.Dot(struct
  include DGS
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name v = sprintf "\"%s\"" (proof_formula_to_string v)
  let vertex_attributes _ = [`Shape `Box]
  let get_subgraph _ = None

  let default_edge_attributes _ = []      
  let edge_attributes e = [] 

end)


let out_proof_dot file_name proof_graph = 
  let file = open_out_bin file_name in
  ProofDot.output_graph file proof_graph

