open Lib
open Logic_interface



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



type elt = {
  mutable simplified: int;
  mutable generated: int;
}

type t = 
  elt BCHtbl.t

let empty ?(size=10001) () = 
  BCHtbl.create size

let add_entry table clause = 
  dassert (fun () -> not (BCHtbl.mem table clause));
  BCHtbl.add table clause {simplified = 0; generated = 0}

let incr_simplified_by table clause n = 
  match BCHtbl.find_opt table clause with
  | Some x -> x.simplified <- x.simplified + n
  | None -> BCHtbl.add table clause {simplified = n; generated = 0}

let incr_generated_by table clause n = 
  match BCHtbl.find_opt table clause with
  | Some x -> x.generated <- x.generated + n
  | None -> BCHtbl.add table clause {simplified = 0; generated = n}
  
let incr_simplified table clause = 
  incr_simplified_by table clause 1
 
let incr_generated table clause = 
  incr_generated_by table clause 1
  
let get table clause = 
  (* Old behaviour: add if not in table. But this is unnecessary given the
     above behaviour of incr_* *) 
  (* match BCHtbl.find_opt table clause with
  | Some x -> x
  | None -> 
    let x = {simplified=0; generated=0} in
    BCHtbl.add table clause x;
    x *)
  BCHtbl.find_opt table clause

let iter = BCHtbl.iter

let fold = BCHtbl.fold



let score_to_string {simplified; generated} = 
  sprintf "sim: %d gen: %d" simplified generated



(*---------------- get best scores ----------*)

let sim x = x.simplified 
let gen x = x.generated 
 
let score_type_to_fun score_type = 
  let open Options in
  match score_type with 
  | Sup_score_sim       -> (fun e -> sim e )
  | Sup_score_sim_gen   -> (fun e -> sim e - gen e)
  | Sup_score_sim_d_gen -> (fun e -> int_of_float (100000. *. (float_of_int (sim e + 1)) /. (float_of_int (gen e + 1))))
        
(* if a clause is not the score table then it is smaller than any clause that has score *)
let cmp_score score_fun scores c1 c2 = 
  let score1 = get scores c1 in 
  let score2 = get scores c2 in 
  match score1, score2 with 
  | Some score1, Some score2 -> 
    Int.compare (score_fun score1) (score_fun score2)
  | Some _, None -> +1
  | None, Some _ -> -1
  | None, None   ->  0

let score_type_to_cl_cmp score_type scores = 
  let score_fun = score_type_to_fun score_type in
  let cmp_fun c1 c2 = cmp_score score_fun scores c1 c2 in
  cmp_fun
                   


(* get_best_scores_clauses score_fun scores *)
(* returns (max_score, clauses), max_score: f ~sim_score ~gen_score; *)
(* the higher the better; *)
(* examples of f: sim, f:  sim - gen or (sim+1)/(gen+1)  *)

let get_best_scores_clauses score_fun cmp_scores scores =   
  BCHtbl.fold (fun cl elt (best_score, cl_list) ->
    let score = score_fun elt in
    let r = cmp_scores score best_score in
    if r = Ord.gt then
      (score, [cl])
    else if r = Ord.eq then
      (score, cl::cl_list)
    else
      (best_score, cl_list) 
  ) scores (0,[])
    
let select_clauses_from_scores score_pred scores = 
  BCHtbl.fold (fun cl elt cl_list ->
    if score_pred elt then 
      cl::cl_list 
    else 
      cl_list
  ) scores []

(* frac_top_score 0.1 then all clasues which score is no more than 10% away from top_score *)
(* if number of such clauses is greater max_num_cl then take top first max_num_cl; if max_num_cl = 0 then no clauses are shared *)
let select_top_clauses ?(worst=false) ~frac_top_score ~max_num_cl ~score_fun scores = 
  if max_num_cl <= 0 then (
    []
  ) else (
    (* Get best/worst score *)
    let cmp_scores = if worst == false then Int.compare else Ord.reverse_f Int.compare in
    let (best_score, best_clauses) = get_best_scores_clauses score_fun cmp_scores scores in
    dbg_env D_trace (fun () ->
      dbg D_trace @@ lazy (sprintf "best_score: %d; best_clauses:" best_score); 
      best_clauses |> List.iter (fun c -> dbg D_trace @@ lazy ("  " ^ Clause.to_string_tptp c));
    );
    (* Get scores within frac_top_score of best/worst *)
    let score_bound = 
      if worst == false then
        int_of_float ((1. -. frac_top_score) *. float_of_int best_score) 
      else
        int_of_float (frac_top_score *. float_of_int best_score)
    in
    let score_pred elt = 
      let score = score_fun elt in 
      cmp_scores score score_bound = Ord.gt
    in  
    let selected_clauses = select_clauses_from_scores score_pred scores in  
    dbg D_trace @@ lazy (sprintf "num_of_sel_cl: %d" (List.length selected_clauses));
    (* If more clauses than max_num_cl, sort and get best/worst *)
    (* TODO can be done with a linear scan rather than sorting *)
    let top_clauses =
      if List.compare_length_with selected_clauses max_num_cl = Ord.gt then 
        let sorted = 
          let cmp = lex_combination2 
            (Ord.reverse_f (cmp_score score_fun scores)) Clause.cmp_num_symb (* need reverse since List.sort sorts in increasing order *)
          in
          List.sort cmp selected_clauses
        in
        let top_cl = List.X.take max_num_cl sorted in 
        (* dbg D_trace @@ (lazy (sprintf "top max_num_cl: %d; %s" max_num_cl (Clause.clause_list_to_string top_cl))); *)
        top_cl
      else 
        selected_clauses
    in
    (* dbg D_trace @@ lazy (sprintf "top clauses: %s" (Clause.clause_list_to_string top_clauses)); *)
    top_clauses        
  )
