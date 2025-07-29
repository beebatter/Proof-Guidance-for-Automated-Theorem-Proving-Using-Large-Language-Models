open Lib
  

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

let () = dbg_flag_msg dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set dbg_flag dbg_groups group f

(*----- debug -----*)



module Make (Feat:Map.OrderedType) (* (Elt:OrderedType) (ESet : Set.S with type elt=Elt.t) *) = struct
(*  type elt = Elt.t *)
  type feat_list = Feat.t list
  module FMap = Map.Make (Feat)
  type 'a index =
    |Node of ('a index FMap.t)
    |Leaf of 'a
    |Empty

  let empty = Empty
      
(* finds Leaf(x) corresponding to the feat_list,
   applies f (Some x) to it and returns new index with updated leaf
   if the path does not exists then it is created based on feat_list and Leaf (f None) is created
   if f returns None then path is deleted
 *)

  let rec find feat_list ind =
    match feat_list with
    |[] ->
        (match ind with
        |Leaf x -> x
        |_ -> raise Not_found
        )
    |h::tl ->
        begin
          let ind_map =
            match ind with
            | Leaf _x ->
                raise Not_found
                (* failwith "feature cache: get_geq: index path to Leaf shorter than feature list" *)
                  
            | Node ind_map -> ind_map
            | Empty -> raise Not_found
          in          
          let new_ind =
           (* can raise Not_found *)
            FMap.find h ind_map 
          in
          find tl new_ind
        end
          
  
    
  let app_leaf f feat_list ind =
    let rec g feat_list ind_opt =
      dbg D_trace @@ lazy (sprintf "feat_list:len: %i" (List.length feat_list)); 
      match feat_list with
      |[] ->
          begin
            match ind_opt with
            | Some(Leaf x) ->
                (
                 dbg D_trace @@ lazy (sprintf "feat_list:[]: Some(Leaf x)"); 
                 let res_opt = f (Some(x)) in
                 Option.lift (fun res -> Leaf(res)) res_opt
                   (*
                 match res_opt with
                 |Some (res) -> 
                     Some (Leaf (res))
                     | None -> None
                     *)
                )
                    
            | Some(Node _) -> failwith "feature cache: app_leaf: feature list shorter then index path"
            | Some(Empty) | None ->
                dbg D_trace @@ lazy (sprintf "feat_list:[]: Some(Empty) | None"); 
                let res_opt = f None in
                Option.lift (fun res -> Leaf(res)) res_opt
           (*       
            | None ->
                dbg D_trace @@ lazy (sprintf "feat_list:[]: None"); 
                    None
                    *)
          end
            
      | h::tl ->
          begin            
            match ind_opt with
            |None ->
                dbg D_trace @@ lazy (sprintf "feat_list:h::tl : None");
                let ind_opt_new = g tl (Some(Empty)) in
                
                Option.lift (fun ind_new -> Node(FMap.singleton h ind_new)) ind_opt_new
(*                 match ind_opt_new with
                 | Some ind_new ->
                     Some(Node(FMap.singleton h ind_opt_new))
                 |None -> None
               *)

(*                failwith "feature cache: h::tl : ind_opt should not be None "; *)
(*                None *)
            |Some(ind) ->
                dbg D_trace @@ lazy (sprintf "feat_list:h::tl : Some(ind)"); 
                let ind_map =
                  match ind with
                  | Leaf _x ->
                      failwith "feature cache: app_leaf: index path to Leaf shorter than feature list"
                        
                  | Node ind_map ->
                      dbg D_trace @@ lazy (sprintf "feat_list:h::tl : Node ind_map ");
                      ind_map
                  | Empty ->
                      dbg D_trace @@ lazy (sprintf "feat_list:h::tl : Empty: FMap.empty");
                      FMap.empty 
                in
                let ind_map_new = 
                  FMap.update h (g tl) ind_map
                in
                if (FMap.is_empty ind_map_new) then
                  (dbg D_trace @@ lazy (sprintf "feat_list:h::tl : FMap.is_empty ind_map_new "); 
                   None)
                else
                  (dbg D_trace @@ lazy (sprintf "feat_list:h::tl : (FMap.is_empty ind_map_new): else: Some (Node(ind_map_new))");                    
                   Some (Node(ind_map_new))
                  )
          end
    in
    g feat_list (Some(ind))

(* get an new index consisting of all instances where at least one feature is strictly less then the correcponding feature in feat_list *)
      
  let get_less feat_list ind =
    let rec g feat_list ind =     
      match feat_list with
      |[] -> Empty
      |h::tl ->
          begin
            let ind_map =
              match ind with
              | Leaf _x ->
                  failwith "feature cache: get_geq: index path to Leaf shorter than feature list"
                        
              | Node ind_map -> ind_map
              | Empty -> FMap.empty
            in            
            let (l_map,h_map_opt,r_map) = FMap.split h ind_map in
            dbg D_trace @@ lazy (sprintf "get_geq: |l_map|: %i |h|: %i |r_map|: %i "
                                   (FMap.cardinal l_map)
                                   (match h_map_opt with Some h -> 1 | None -> 0)
                                   (FMap.cardinal r_map)
                                );                                 
            let r_h_map = 
              match h_map_opt with
              |Some (h_map) -> FMap.add h h_map r_map
              |None -> r_map
            in
            let new_fmap = 
              FMap.fold (fun feat f_ind acc ->
                let new_f_ind = g tl f_ind in
                if (not Stdlib.(Empty = new_f_ind)) then
                  FMap.add feat new_f_ind acc
                else
                  acc
                        ) r_h_map FMap.empty
            in
            let final_fmap = FMap.union
                (fun key a1 a1 -> failwith "get_geq should not happen")
                l_map
                new_fmap
            in
            if (not (FMap.is_empty final_fmap)) then
              Node (final_fmap)
            else              
              Empty                  
          end
    in
    g feat_list ind

(*---------*)      
  let rec iter f ind =
    match ind with
    | Leaf x -> f x
    | Node x -> FMap.iter (fun _feat ind -> iter f ind) x
    | Empty -> ()
    
end
    
  
