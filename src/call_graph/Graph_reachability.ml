module G = Call_graph.G
module Bfs = Graph.Traverse.Bfs (G)
module Oper = Graph.Oper.I (G)
module Comp = Graph.Components.Make (G)

type graph = G.t
type vertex = G.V.t

(* reverse BFS view: successors := predecessors *)
module Rev = struct
  include G

  let iter_succ f g v = G.iter_pred f g v
end

module RBfs = Graph.Traverse.Bfs (Rev)

let reverse_reachable_subgraph (g : graph) (targets : vertex list) : graph =
  List.fold_left
    (fun sg t ->
      if not (G.mem_vertex sg t) then G.add_vertex sg t;
      if G.mem_vertex g t then
        RBfs.fold_component
          (fun v sg -> G.fold_pred_e (fun e sg -> G.add_edge_e sg e; sg) g v sg)
          sg g t
      else sg)
    (G.create ()) targets

module VSet = Set.Make (G.V)

(* Batch: compute SET of reachable vertices from multiple starts using Bfs.fold_component *)
let reachable_vertices_batch (g : graph) (starts : vertex list) : VSet.t =
  List.fold_left
    (fun visited s ->
      if G.mem_vertex g s && not (VSet.mem s visited) then
        Bfs.fold_component (fun v acc -> VSet.add v acc) visited g s
      else visited)
    VSet.empty starts

(* Compute the subgraph containing only functions relevant for taint flow
   from sources to sinks. Optimized batched version using sets. *)
let compute_relevant_subgraph (graph : Call_graph.G.t)
    ~(sources : Function_id.t list) ~(sinks : Function_id.t list) : Call_graph.G.t =
  match (sources, sinks) with
  | [], _ | _, [] ->
      Call_graph.G.create ()
  | _ :: _, _ :: _ ->
      (* Batch: compute reachable vertex SETS (no intermediate graphs) *)
      let from_sources = reachable_vertices_batch graph sources in
      let from_sinks = reachable_vertices_batch graph sinks in
      (* Fast set intersection *)
      let common = VSet.inter from_sources from_sinks in
      (* Reverse BFS from all common descendants to get ancestor edges *)
      reverse_reachable_subgraph graph (VSet.elements common)
