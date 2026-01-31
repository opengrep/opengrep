module G = Call_graph.G
module Components = Graph.Components.Make (G)

(** Graph condensation *)

(* Condensation of a graph G is a graph in which:
 * - Nodes are strongly commected components of G, 
 * - Edges are collections of edges.
 * For our purposes, we don't need labels on edges of the condensed graph, 
 * because to calculate join points we only need to know if a path exists. *)

module Condensed =
  Graph.Imperative.Digraph.ConcreteBidirectional
    (struct
      type t = int
      let compare = Int.compare
      let hash = Int.hash
      let equal = Int.equal
    end)

(* Strongly connected components are identified by ints, as returned by
 * Components.scc. *)

type condensation = {
  cg : Condensed.t;
  num_of_components : int;
  vertices_in_components : G.V.t list array
}

let condense (g : G.t) : condensation =
  let num_of_components, component_of = Components.scc g in
  (* Put vertices in the buckets represented by component ids *)
  let vertices_in_components = Array.make num_of_components [] in
  g |> G.iter_vertex (fun v ->
    let i = component_of v in
    vertices_in_components.(i) <- v :: vertices_in_components.(i));
  (* Create the condensed graph *)
  let cg = Condensed.create ~size:num_of_components () in
  (* Add vertices to the condensed graph *)
  for i = 0 to num_of_components - 1 do
    Condensed.add_vertex cg i;
  done;
  (* Add edges to the condensed graph *)
  g |> G.iter_edges_e (fun e ->
    let src = component_of (G.E.src e) in
    let dst = component_of (G.E.dst e) in
    Condensed.add_edge cg src dst);
  { cg; num_of_components; vertices_in_components }


(** Join points *)

(* A join point is a vertex such that there is a path to it from a source,
 * and a path to it from a sink. We consider only "lub"s, which means that
 * we need the vertex to be the place where two paths connect, not only extend
 * paths that joined previously. For example, in the following graph, func3
 * is a join point, but func4 is not. 
 * 
 *   source ──> func1 ───┐
 *                       v
 *                     func3 ────> func4
 *                       ^
 *   sink ────> func2 ───┘
 *) 

(* The "status" of a component records if there is a path to it from a
 * source and/or from a sink. *)
type status = {
  from_source : bool;
  from_sink : bool;
}

let empty_status = {
  from_source = false;
  from_sink = false
}

let is_status_nontrivial (s : status) : bool =
  s.from_source || s.from_sink

let is_status_candidate (s : status) : bool =
  s.from_source && s.from_sink

let combine_statuses (s1 : status) (s2 : status) : status = {
  from_source = s1.from_source || s2.from_source;
  from_sink = s1.from_sink || s2.from_sink
}

(* The secend elem of the result is the number of nontrivial statuses,
 * which we use to determine if a given component is a join point or simply 
 * extend a path from a join point *)
let combine_status_list (ss : status list) : status * int =
  List.fold_left (fun (ss, n) s ->
    (combine_statuses s ss, n + Bool.to_int (is_status_nontrivial s)))
    (empty_status, 0)
    ss

module VertexSet = Set.Make (G.V)

let join_points (sources : G.V.t list) (sinks : G.V.t list) (c : condensation) : G.V.t list =
  (* Since we expect each component to contain max a few elements (in most
   * cases only one), while sinks and sources can be bigger and they don't
   * vary during the computation, we convert the latter to sets to speed up
   * checking if a given component contains sources or sinks. *)
  let sources = VertexSet.of_list sources in
  let sinks = VertexSet.of_list sinks in
  let statuses = Array.make c.num_of_components empty_status in
  (* A graph condensation is always a DAG. Moreover, Components.scc numbers
   * the components in a topological order, so we can calculate the join
   * points iterating the components in that order. *)
  let rec loop (i : int) (acc : G.V.t list list) : G.V.t list list =
    if i < 0 then acc else
    let vs = c.vertices_in_components.(i) in
    let node_status = {
      from_source = List.exists (fun x -> VertexSet.mem x sources) vs;
      from_sink = List.exists (fun x -> VertexSet.mem x sinks) vs }
    in
    let pred_statuses =
      Condensed.pred c.cg i |> List.map (fun i -> statuses.(i))
    in
    let combined_status, num_of_nontrivial_statuses =
      combine_status_list (node_status :: pred_statuses)
    in
    statuses.(i) <- combined_status;
    (* A component is a join point if one of the two holds:
     * 1. at least two paths (from two distincs predecessors) join here,
     * 2. the component has both source and sink. *)
    if (* 1 *) is_status_candidate combined_status &&
               num_of_nontrivial_statuses > 1
    || (* 2 *) is_status_candidate node_status
    then
      loop (i - 1) (vs :: acc)
    else
      loop (i - 1) acc
  in
  loop (c.num_of_components - 1) []
  |> List.concat


(** Downward closure *)

(* The relevant graph is the downward closure of the set of join points, *)
(* which consists of all paths from a vertex to a vertex that belongs to *)
(* a join point. *)

module Rev = struct
  include G
  let iter_succ f g v = G.iter_pred f g v
end

module RBfs = Graph.Traverse.Bfs (Rev)

let reverse_reachable_subgraph (g : G.t) (targets : G.V.t list) : G.t =
  let sg = G.create () in
  let visit v =
    (* add all incoming edges u->v while walking backwards *)
    G.iter_pred_e (fun edge -> G.add_edge_e sg edge) g v
  in
  List.iter
    (fun t ->
      if not (G.mem_vertex sg t) then G.add_vertex sg t;
      if G.mem_vertex g t then RBfs.iter_component visit g t)
    targets;
  sg


(** Entry point *)

let compute_relevant_subgraph (g : G.t) ~(sources : G.V.t list) ~(sinks : G.V.t list) : G.t =
  condense g
  |> join_points sources sinks
  |> reverse_reachable_subgraph g
