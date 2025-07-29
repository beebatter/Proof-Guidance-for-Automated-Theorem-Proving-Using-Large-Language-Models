open Lib
open Logic_interface 
(* open Yojson.Basic.Util *)

type json = Yojson.Basic.t 
      
type connection 

(**  open_connection address port  *)
val open_connection: string -> int -> connection 
val close_connection: connection -> unit 


(** connection with clause score cache *)

type external_connection = 
    {
     mutable en_connection     : connection; 
     mutable en_score_cache    : float CMap.t;
     mutable en_reg_cls        : CSet.t; (* registed clauses*)
     mutable en_reg_cls_id_map : clause IntMap.t; 
   }


val external_connection : external_connection option ref

(** assigns external_connection with empty score cache *)
val assign_external_connection : connection -> unit 

val external_is_connected : unit -> bool

(** can raise Invalid_argument if no connection established *)
val get_external_connection : unit -> external_connection 

(** register clauses before usage; ok if some clauses already registered *)
val register_clauses : external_connection ->  clause list -> unit

(*-------- batch scores ---------*)
type component = Input | Inst | Res | Sup | Proof

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

val tag_to_string : tag -> string
val tag_of_string : string -> tag

val get_tag_json : json -> tag 
    
type clause_batch 

(* note: component_ids are different from component ids in proof_search_loop; 
   this is rather state_id for sup/inst/res defined by Logic_interface.next_proof_state_id (); 
   in a proof_search_loop component a state can be reset due to e.g. deepening or inst learning which results (as it should) in different proof_state_id 
  *)
      
val create_clause_batch: tag:tag -> component:component -> component_id:int -> clauses: (Clause.clause list) -> clause_batch


(** Send batch of clauses with some tag
     example of tag: cb.tag = Passive_clauses *)

val send_batch : external_connection -> clause_batch -> unit 

val get_cl_ids_json : Yojson.Basic.t -> int list
val get_component_id_json : Yojson.Basic.t -> int
    
(*---- Sending/Getting--------*)

(* None if passive is empty *)

val get_given_clause: external_connection -> component:component -> component_id:int -> clause option

val send_given_clause:  external_connection -> component:component -> component_id:int -> clause -> unit

val send_simplified_clauses: external_connection  -> component:component -> component_id:int -> clause list -> unit

val send_moved_from_active_to_passive: external_connection  -> component:component -> component_id:int -> clause list -> unit
    
val send_passive_clauses: external_connection  -> component:component -> component_id:int -> clause list -> unit

    
val send_szs_result: connection -> string -> unit

val send_proof: external_connection  -> clause list -> unit

(*---- Scores ------*)

(** Updates external scores for new clauses form the server; previously cached scores are not updated *)

val update_cache_external_scores: external_connection  -> component:component -> component_id:int -> clause list -> unit

(* clause should be in on of the batches in update_cache_external_scores_cb *)
val get_clause_external_score: external_connection -> Clause.clause -> float

val reset_score_cache: external_connection -> unit

(** external_connection should be assigned *)
val cmp_clause_external : Clause.clause -> Clause.clause -> int

(*------------*)

val send_start_server_queries: external_connection -> unit
val process_server_queries: external_connection -> unit
