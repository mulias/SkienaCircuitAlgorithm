(* ALGORITHMS HOMEWORK NUMBER 6, ELIAS MULHALL ON 11/20/2015
 * Implementing the algorithms described in section 6-2 of Skiena.
 * I meant to do something reasonable for this problem and now I am so sorry.
 * Graph is a custom graph library, attached. Core is a common OCaml library
 * that adds a bunch of nice functions.
 * licensed GPL, your fault if you try to use it.
 *)

open Graph
open Core.Std

(* make a complete graph representing the circuit board *)
let build_circuit_graph (num_points : int) (board_size : int) : Graph.t =
  Random.self_init ();
  let gen_contact_point () =
    let range = board_size + 1 in
    let new_x = Random.int range in
    let new_y = Random.int range in
    (new_x, new_y)
  in
  let rec points_list i lst =
    if i = 0
    then lst
    else points_list (i - 1) ((gen_contact_point ())::lst)
  in
  let graph = Graph.add_nodes Graph.empty (points_list num_points []) in
  complete_graph graph

(* when we don't have a better way to order nodes, order them from closest to 
 * farthest x-axis position. *)
let compare_nodes (n1 : Graph.node) (n2 : Graph.node) : int =
  compare n1.x_pos n2.x_pos

(* each arm will follow a path from test point to test point *)
type path = Graph.node list

(* naive implementation *)
let simple_traversal_order (circuit : Graph.t) : path * path =
  let nodes = List.sort circuit.nodes ~cmp:compare_nodes in
  match nodes with
  | [] -> ([], [])
  | (first::remaining) -> 
      let right_arm_path = remaining in
      let left_arm_path = List.map remaining ~f:(fun _ -> first) in
      (right_arm_path, left_arm_path)

(* fancy implementation. cluster_size is an optional argument, default to 10,
 * meaning a cluster is composed of a center node and all nodes within weight
 * 10 of the center. It turns out that the cluster size doesn't change the 
 * resulting simulation too much. *)
let better_traversal_order ?(cluster_size = 10) (circuit : Graph.t) : path * path =
  let mst = Graph.min_span_tree circuit in
  let clusters = Graph.mst_clusters mst cluster_size in
  (* in each cluster we use the x_pos ordering rule *)
  let clusters' = List.map clusters ~f:(fun cluster ->
    List.sort cluster ~cmp:compare_nodes) in
  match clusters' with
  | [] -> ([], [])
  | _ ->
    (* the right arm visits all but the first node in each cluster *)
    let right_arm_clusters = List.map clusters' ~f:List.tl_exn in
    (* the left arm visits the first node in each cluster, and stays there
     * while the right arm moves around *)
    (* this is the node we visit for each cluster *)
    let left_arm_nodes = List.map clusters' ~f:List.hd_exn in
    (* this is what the arm does for each cluster *)
    let left_arm_clusters = List.zip_exn left_arm_nodes right_arm_clusters 
      |> List.map ~f:(fun (node, cluster) -> List.map cluster ~f:(fun _ -> node)) in
    let right_arm_path = List.concat right_arm_clusters in
    let left_arm_path = List.concat left_arm_clusters in
  (right_arm_path, left_arm_path)

(* we save each arm move to replay/analyze *)
type sim_step =
  { start : Graph.node;
    dest  : Graph.node;
    trip  : int;
  }

(* print a series of movements for an arm *)
let print_steps (steps : sim_step list) : unit =
  List.iteri steps ~f:(fun step_num step ->
    printf "%d. at (%d,%d) move to (%d,%d) in %d\n" step_num 
      (step.start.x_pos) (step.start.y_pos) (step.dest.x_pos) (step.dest.y_pos)
      step.trip)

(* at each step one arm has to wait for the other arm to get to it's destination
 * before the next step can happen. So the total runtime is the sum of the
 * longest trips made at each step. *) 
let total_runtime (right_arm : sim_step list) (left_arm : sim_step list) : int =
  List.fold2_exn right_arm left_arm ~init:0 ~f:(fun accum right_step left_step ->
    (max right_step.trip left_step.trip) + accum)

(* produce the simulation description of what the arm does to follow the path *)
let run_arm (arm_path : path) : sim_step list =
  let start_pos = Graph.make_node 0 0 in
  let start = ref start_pos in
  let dest  = ref start_pos in
  List.map arm_path ~f:(fun node ->
    dest := node;
    let step = { start = !start; dest  = !dest;
                 trip  = Graph.distance !start !dest; } in
    start := node;
    step)

(* print one simulation, both arms *)
let report_sim (path_rule : (Graph.t -> (path * path))) (num_points : int) : unit =
  let circuit = build_circuit_graph num_points 99 in
  let (right_arm, left_arm) = path_rule circuit in
  let right_arm_steps = run_arm right_arm in
  let left_arm_steps = run_arm left_arm in
  printf "Run %d contact point board:\n" num_points;
  print_string "Run right arm\n";
  print_steps right_arm_steps;
  print_string "Run left arm\n";
  print_steps left_arm_steps;
  let total_time = total_runtime right_arm_steps left_arm_steps in
  let avg_trip_len = total_time / (Graph.num_nodes circuit) in
  printf "total time, accounting for both arms: %d\n" total_time;
  printf "average time per move, accounting for both arms: %d\n\n" avg_trip_len

(* print average total time for 1000 simulations *)
let report_avg_result (path_rule : (Graph.t -> (path * path))) (num_points : int) : unit =
  let total = ref 0 in
  for _ = 0 to 999 do
    let circuit = build_circuit_graph num_points 99 in
    let (right_arm, left_arm) = path_rule circuit in
    let right_arm_steps = run_arm right_arm in
    let left_arm_steps = run_arm left_arm in
    total := !total + (total_runtime right_arm_steps left_arm_steps)
  done;
  printf "Run %d contact point board 1000 times, average time is %d\n" 
    num_points (!total / 1000)

(* main *)
let () : unit =
  List.iter [10; 20; 40] ~f:(fun num_points ->
    printf "\nSimple traversal, full report\n";
    report_sim simple_traversal_order num_points;
    printf "\nBetter traversal, full report\n";
    report_sim better_traversal_order num_points;
    print_newline ());
  List.iter [10; 20; 40; 60; 80; 100] ~f:(fun num_points ->
    printf "\nSimple traversal, average time report\n";
    report_avg_result simple_traversal_order num_points;
    printf "\nBetter traversal, average time report\n";
    report_avg_result (better_traversal_order ~cluster_size:0) num_points;
    print_newline ())

