let rec remove_doubles cmp = function
  | [] -> []
  | [a] -> [a]
  | a::b::r when (cmp a b) = 0 -> remove_doubles cmp (a::r)
  | a::b::r -> a::(remove_doubles cmp (b::r))

let sort_uniq cmp l = remove_doubles cmp (List.sort cmp l)

open Cnf;;
open Fof;;
open Features;;
open Args;;
open Format;;
open Logic;;

exception CleanExit;;
exception ResourceOut of string;;
exception Solved;;
exception DeadEnd;;

let cleanexit _ = raise (ResourceOut "Signal");;
Sys.signal Sys.sigint (Sys.Signal_handle cleanexit);;
Sys.signal Sys.sigterm (Sys.Signal_handle cleanexit);;

let log_channel = ref stdout;;
let log_fmt = ref std_formatter;;

type tree_kind = Open | Unexplored | Lost | Won;;
type tree = {
  mutable kind : tree_kind;
  p : float;            (* prediction value *)
  mutable w : float;    (* W = wins *)
  mutable n : int;      (* N = visit count *)
  mutable b : tree list;(* subtrees for actions *)
  mutable nn_response : (float * float list) option; (* estimated value, prior probabilities *)
};;
let is_theorem = ref [];;

let rec pp_print_list ?(pp_sep = pp_print_cut) pp_v ppf = function
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list ~pp_sep pp_v ppf vs

(* Used in the interactive mode and in "-debug" *)
let rec print_tree more fmt t =
  let kind2string = function Lost -> "F" | Won -> "W" | Unexplored -> "U" | Open -> "o" in
  if t.kind = Open && more > 0 then pp_open_box fmt 2;
  fprintf fmt "%s " (kind2string t.kind);
  fprintf fmt "%.2f (%.6f %d)" t.p (t.w /. float t.n) t.n;
  if t.kind = Open then
    if more > 0 then begin
      pp_print_string fmt " ["; pp_force_newline fmt ();
      pp_print_list ~pp_sep:(fun f () -> pp_print_string f "; "; pp_force_newline fmt ())
                    (print_tree (more - 1)) fmt t.b;
      pp_print_char fmt ']';
      pp_close_box fmt ()
    end else pp_print_string fmt " ...";;
let print_tree2 = print_tree 2;;

(* Some counters *)
let bigsteps, opened, closed, edges, totfea, playouts, nn_queries = ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0;;

let check_limits () =
  if !bigsteps >= !max_moves then raise (ResourceOut "Moves");
;;

let get_nn_response node state actions =
  match node.nn_response with
  | Some response -> response
  | None ->
      incr nn_queries;
      Export_state.print_for_nn state actions;
      let value = read_float () in
      let rec read_priors () =
	let p = read_float () in
	if p < 0. then []
	else p::(read_priors ())
      in
      let priors = read_priors () in
      let response = (value, priors) in
      node.nn_response <- Some response;
      response;;

let next_state st acts act_i =
  let acts_sorted = acts |> sort_uniq (fun x y
    -> compare (Logic.hash_to_fl_index x) (Logic.hash_to_fl_index y)) in
  let action = List.nth acts_sorted act_i in
  Logic.extend st action

(* Initial tree with one unexplored node *)
let itree = {kind=Unexplored; p=1.; w=0.; n=0; b=[]; nn_response=None};;

let do_tree tree thist (i, (st, acts)) =
  let fail tree = if tree.kind <> Lost then begin incr closed; tree.kind <- Lost end in
  match i with
  | 1 -> tree.kind <- Won; is_theorem := thist;
         if !thm_play_count >= 0 then play_count := !thm_play_count;
         (st, tree, tree :: thist, [])
  | -1 -> fail tree; (st, tree, tree :: thist, [])
  | _ ->
     if acts = [] then (fail tree; (st, tree, tree :: thist, [])) else
     match tree.kind with
     | Won | Lost -> (st, tree, tree :: thist, [])
     | Open ->
        if List.exists (fun x -> x.kind <> Lost) tree.b then
          (st, tree, tree :: thist, acts)
        else
          (fail tree; (st, tree, tree :: thist, []))
     | Unexplored ->
        incr opened;
        let (_, l) = get_nn_response tree st acts in
        let b = List.map (fun p -> {kind=Unexplored; p; w=0.; n=0; b=[]; nn_response=None}) l in
        if (not !one_per_play) then tree.kind <- Open;
        tree.b <- b;
        (st, tree, tree :: thist, acts);;

(* 'arg_max get_val l' computes the _index_ of element of list l which has maximal get_val *)
let arg_max get_val = function
  | h :: t ->
     let rec aux ind indmax maxval = function
       | [] -> indmax
       | h :: t ->
          let v = get_val h in
          if v > maxval then aux (ind + 1) ind v t else aux (ind + 1) indmax maxval t
     in aux 1 0 (get_val h) t
  | _ -> failwith "arg_max: empty list";;


let ucb sum_visits prior wins visits =
(*  let wins = if visits = 0 then 1.0 else wins in*)
  let visits = max 1.0 (float visits) and sum_visits = max 1.0 (float sum_visits) in
  let factor =
    match !ucb_mode with
    | 1 -> sqrt (sum_visits /. visits)       (* UCB no logarithm *)
    | 2 -> sqrt sum_visits /. visits         (* PUCB from Alpha Zero *)
    | _ -> sqrt ((log sum_visits) /. visits) (* Original Csaba Szepesvari *)
  in
  if !do_ucb then ((wins /. visits) ** (1. /. !gen_average)) +. !ucb_const *. prior *. factor else Random.float 1. *. prior;;

let get_rel sum_visits t =
  match t.kind with
  | Lost -> -1.
  | _ -> ucb sum_visits t.p t.w t.n;;

let reward node state actions =
  match node.kind with
  | Won -> 1. | Lost -> 0.
  | _ ->
      let (value, _) = get_nn_response node state actions in
      value

let rec playout depth (st, tree, thist, acts) =
  (* check_limits (); *)
  if tree.kind = Open && depth >= 0 then
    let i = arg_max (get_rel tree.n) tree.b in
    playout (depth - 1) (do_tree (List.nth tree.b i) thist (next_state st acts i))
  else begin
    incr playouts;
    if tree.kind = Unexplored then tree.kind <- Open;
    let update_tree win t = t.w <- t.w +. win ** !gen_average; t.n <- t.n + 1 in
    List.iter (update_tree (reward tree st acts)) thist
  end;;

let print_guide plus st acts t =
  fprintf !log_fmt (if plus then "+" else "~");
  fprint_state !log_fmt st (global_features 0 st);
  fprintf !log_fmt ",(";
  let acts_sorted = acts |> sort_uniq (fun x y
    -> compare (Logic.hash_to_fl_index x) (Logic.hash_to_fl_index y)) in
  let cvs = List.map2 (fun t a -> t.w, t.n, a) t.b acts_sorted in
  let pp_print_contrval fmt (w, n, c) = fprintf !log_fmt "\n    (%f,%i,%i,%s)" w n c (Hashtbl.find no_contrstr c) in
  pp_iter !log_fmt pp_print_contrval "," cvs;
  fprintf !log_fmt ").\n";
  pp_print_flush std_formatter ();;

let rec print_guides tree (_, (st, acts)) =
  if tree.kind = Open then begin
    print_guide (is_solved st) st acts tree;
    let subtrees = List.map fst (List.filter (fun (_,n) -> n >= !save_above) (List.mapi (fun i t -> (i, t.n)) tree.b)) in
    List.iter (fun i -> print_guides (List.nth tree.b i) (next_state st acts i)) subtrees
  end
;;
let print_guides istate = if !save_above >= 0 then print_guides itree istate;;

let bigstep_hist = ref [];;

let rec bigstep ((st, tree, thist, acts) as state) =
  if tree.kind = Unexplored then tree.kind <- Open; (* freshly visited *)
  (*
  while !i < !play_count do
    incr i; playout !play_dep state;
    check_limits ();
  done;
   *)
  while tree.n < !play_count do
    playout !play_dep state;
  done;
  
  if tree.kind = Won then raise Solved;
  if !debug then (print_tree 1 !log_fmt tree; fprintf !log_fmt "\n%!");
  if tree.kind = Lost then raise DeadEnd;
  if not !debug && !save_above < 0 then print_guide true st acts tree;
  let i =
    if !is_theorem = [] || !thm_play_count = -1 then
      arg_max (fun t -> if t.n = 0 then 0. else float t.n +. t.w /. float t.n) tree.b
    else
      arg_max (fun t -> if t.kind = Won then 2 else if List.mem t !is_theorem then 1 else 0) tree.b
  in
  incr bigsteps;
  check_limits ();
  bigstep_hist := i :: !bigstep_hist;
  bigstep (do_tree (List.nth tree.b i) thist (next_state st acts i));;

at_exit (fun () ->
  print_string "END ";
  print_string (if !is_theorem = [] then "unsolved" else "solved");
  print_char ' ';
  print_int !bigsteps;
  print_char '\n';
  Format.print_flush ();
  fprintf !log_fmt "%% Proof: %s\n" (String.concat " " (List.map string_of_int (List.rev !bigstep_hist)));
  fprintf !log_fmt "%% Bigsteps: %i Inf: %i Op: %i Cl: %i Ed:%i TotFea:%i Play:%i NNQ:%i\n"
    !bigsteps !infer !opened !closed !edges !totfea !playouts !nn_queries;
  fprintf !log_fmt "%% Total time %f\n%!" (gettime () -. start_time);
  close_out !log_channel
);;

if !tosolve <> "" then
  let init_state = Logic.start !tosolve in
  try
    log_channel := open_out !log_fname;
    log_fmt := formatter_of_out_channel !log_channel;
    let init_tree = do_tree itree [] init_state in
    playout !play_dep init_tree; bigstep init_tree
  with Solved -> print_guides init_state; fprintf !log_fmt "%% SZS status Theorem (fast)\n%!";
     | Failure x -> fprintf !log_fmt "%% SZS status Error\n%%%s\n%!" x
     | DeadEnd -> print_guides init_state; fprintf !log_fmt "%% SZS status DeadEnd\n%!";
     | CleanExit ->
        print_guides init_state; fprintf !log_fmt "%% SZS status %s\n%!" (if !is_theorem = [] then "Unknown" else "Theorem (slow)")
     | ResourceOut x ->
        print_guides init_state; fprintf !log_fmt "%% SZS status %s\n%!" (if !is_theorem = [] then "ResourceOut: " ^ x else "Theorem (slow)")
     | Parsing.Parse_error -> fprintf !log_fmt "%% SZS status Error\n%%Parse_error\n%!";;
