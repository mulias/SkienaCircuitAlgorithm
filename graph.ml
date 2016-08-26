(* The start of a library for describing locations in cartesian space as a
 * graph.
 * licensed GPL, yo.
 *)

open Core.Std

(* NODE *)
(* I decided not to call them vertices. *)
(******************************************************************************)

type pos = int

type node = { x_pos : pos; y_pos : pos; }

let make_node (x : int) (y : int) : node = 
  { x_pos = x; y_pos = y; }

let distance (n1 : node) (n2 : node) : int =
  let x_dist = abs (n1.x_pos - n2.x_pos) in
  let y_dist = abs (n1.y_pos - n2.y_pos) in
  max x_dist y_dist

(* EDGE *)
(* the weight of an edge (n1, n2) is 'distance n1 n2' *)
(******************************************************************************)

type weight = int

type edge = { n1 : node; w : weight; n2: node; }

let make_edge (n1 : node) (n2 : node) : edge =
  let w = distance n1 n2 in { n1 = n1; w = w; n2 = n2; }

let compare_edges (e1 : edge) (e2 : edge) : int = compare e1.w e2.w

(* sort edges in increasing order of weight *)
let sort_edges (edges : edge list) : edge list =
  List.sort edges ~cmp:compare_edges

(* GRAPH *)
(* A graph is a list of nodes and a list of edges. Also provided is type 
 * adj_list_graph, which keeps track of lists of adjacent nodes. *)
(******************************************************************************)

(* the main type of a module is traditionally named t *)
type t = { nodes : node list; edges : edge list; }

let empty : t = { nodes = []; edges = []; }

let add_nodes (graph : t) (pos_pairs : (pos * pos) list) : t =
  let nodes = List.map pos_pairs ~f:(fun (x, y) -> make_node x y) in
  { nodes = List.append nodes graph.nodes; edges = graph.edges; }

let add_edges (graph : t) (node_pairs : (node * node) list) : t =
  let edges = List.map node_pairs ~f:(fun (n1, n2) -> make_edge n1 n2) in
  { nodes = graph.nodes; edges = List.append edges graph.edges; }

let num_nodes (graph : t) : int = List.length graph.nodes

let num_edges (graph : t) : int = List.length graph.edges
  
(* take a graph with nodes and add all edges *)
let complete_graph (graph : t) : t =
  let rec all_edges nodes accum : (node * node) list =
    match nodes with
    | [] -> accum
    | n::tl ->
        let new_edges = List.map tl ~f:(fun tl_n -> (n, tl_n)) in
        all_edges tl (List.append new_edges accum)
  in
  let edge_pairs = all_edges graph.nodes [] in
  add_edges { nodes = graph.nodes; edges = []; } edge_pairs

(* MST and CLUSTERS *)
(* We can build an MST from the basic graph structure, but to find good point
 * clusters we convert the graph to an adjacency list. We can then find 
 * clusters, which are described as lists of points. *)
(******************************************************************************)

(* Use Kruskal's and offload tricky bits to the OCaml Set module. If the graph is
 * not connected then the MST will not be connected. *)
let min_span_tree (graph : t) : t =
  (* each component is a set of edges, and all components are in a set *)
  let components = List.fold graph.nodes ~init:Set.Poly.empty 
    ~f:(fun accum n -> Set.add accum (Set.Poly.of_list [n]))
  in
  (* If an edge connects 2 nodes in the same component set, then it would make
   * a loop. Only keep edges that connect 2 componenets, then update comp sets
   * accordingly. *)
  let (mst_edge_list, _) = List.fold (sort_edges graph.edges) 
    ~init:([], components) 
    ~f:(fun (mst_edges, comps) edge ->
      let n1_set = Set.find_exn comps ~f:(fun comp -> Set.mem comp edge.n1) in
      let n2_set = Set.find_exn comps ~f:(fun comp -> Set.mem comp edge.n2) in
      if Set.equal n1_set n2_set then
        (mst_edges, comps)
      else
        let new_comp = Set.union n1_set n2_set in
        let same_comps = Set.remove (Set.remove comps n1_set) n2_set in
        let comps' = Set.add same_comps new_comp in
        ((edge::mst_edges), comps'))
  in
  { nodes = graph.nodes; edges = mst_edge_list; } 

(* Each node is associated with a list of nodes adjacent to it *)
type adj_list_graph = (node, node list) List.Assoc.t

let graph_to_adj_list (graph : t) : adj_list_graph =
  let nodes_list = List.map graph.nodes ~f:(fun node -> (node, [])) in
  let add_edge (adj_list : adj_list_graph) (edge : edge) : adj_list_graph =
    let n1_adjs = List.Assoc.find_exn adj_list edge.n1 in
    let n2_adjs = List.Assoc.find_exn adj_list edge.n2 in
    let adj_list' = List.Assoc.add adj_list edge.n1 (edge.n2::n1_adjs) in
    List.Assoc.add adj_list' edge.n2 (edge.n1::n2_adjs)
  in
  List.fold graph.edges ~init:nodes_list ~f:add_edge

(* A cluster is a node and the nodes directly close to it, where close
 * means adjacent in the mst and weight < dist. We want to make a bunch of
 * clusters with a 1 node overlap between clusters. *)
let mst_clusters (graph : t) (dist : int) : node list list =
  let adj_list = graph_to_adj_list graph in
  (* every other node will be the center of a cluster *)
  let rec cluster_centers (start : node) (accum : node list) = 
    let accum' = (start::accum) in
    let adj_ns = List.Assoc.find_exn adj_list start in
    let ns_2 = List.concat_map adj_ns ~f:(List.Assoc.find_exn adj_list) in
    let ns_2' = List.filter ns_2 ~f:(fun adj -> not (List.mem accum' adj)) in
    List.fold ns_2' ~init:accum' ~f:(fun a n -> cluster_centers n a)
  in
  (* make all the clusters centered on a node *)
  let make_clusters (center : node) =
    let adj_ns = List.Assoc.find_exn adj_list center in
    let (close, far) = List.partition_tf adj_ns 
      ~f:(fun adj_n -> (distance center adj_n) < dist) in
    let bridges = List.map far ~f:(fun adj_n -> [center;adj_n]) in
    if (close = [])
    then bridges 
    else (center::close)::bridges
  in
  match adj_list with
  | [] -> []
  | (n, _)::_ -> 
      (* there's some kind of error with my centers code that is making
       * duplicates, don't want to bother fixing it. *)
      let centers = List.dedup (cluster_centers n []) in
      List.concat_map centers ~f:make_clusters
