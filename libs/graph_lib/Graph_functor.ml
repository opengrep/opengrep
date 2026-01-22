module type BIDIR = sig
  include Graph.Sig.I

  (* Note that ConcreteBidirectional grpahs have that for free*)
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
end

module Reachable (G_in : BIDIR) : sig
  module G : BIDIR with type t = G_in.t and type V.t = G_in.V.t

  type graph = G.t
  type vertex = G.V.t

  (* various versions of reachable graphs *)
  val reachable_subgraph : graph -> vertex -> graph
  val reachable_subgraph_many : graph -> vertex list -> graph
  val reverse_reachable_subgraph : graph -> vertex list -> graph

  (* Main: subgraph containing all paths from s1|s2 to the NEAREST common descendants.
     "Nearest" is computed cycle-safely via SCC condensation minima inside R1 ∩ R2. *)
  val nearest_common_descendant_subgraph : graph -> vertex -> vertex -> graph
end = struct
  module G = G_in
  module Bfs = Graph.Traverse.Bfs (G)
  module Oper = Graph.Oper.I (G)
  module Comp = Graph.Components.Make (G)

  type graph = G.t
  type vertex = G.V.t

  (*reachable is simply a iter_component on the element.
   contentsNote that iter_component keeps some sort of visited queue so it
   traverses every vertex only once *)
  let reachable_subgraph (g : graph) (s : vertex) : graph =
    let sg = G.create () in
    if G.mem_vertex g s then
      Bfs.iter_component
        (fun v -> G.iter_succ_e (fun edge -> G.add_edge_e sg edge) g v)
        g s;
    (* in case there are no outvertices from s *)
    if not (G.mem_vertex sg s) then G.add_vertex sg s;
    sg

  let reachable_subgraph_many (g : graph) (seeds : vertex list) : graph =
    (* simple & robust: union the single-source subgraphs *)
    List.fold_left
      (fun acc s -> Oper.union acc (reachable_subgraph g s))
      (G.create ()) seeds

  (* reverse BFS view: successors := predecessors *)
  module Rev = struct
    include G

    let iter_succ f g v = G.iter_pred f g v
  end

  module RBfs = Graph.Traverse.Bfs (Rev)

  let reverse_reachable_subgraph (g : graph) (targets : vertex list) : graph =
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

  module VSet = Set.Make (G.V)

  (* Helper: Collect all vertices that belong to Strongly connected components
     in the intersection_graph with no incoming edges from other SCCs
      (inside the intersection_graph) as a set.
      *)
  let minimal_scc_vertices (intersection_graph : G.t) : VSet.t =
    let sccs : G.V.t list list = Comp.scc_list intersection_graph in

    let scc_is_minimal vs =
      let sccset = List.fold_left (fun acc v -> VSet.add v acc) VSet.empty vs in
      List.for_all
        (fun v ->
          G.fold_pred
            (fun u ok ->
              ok
              && ((not (G.mem_vertex intersection_graph u)) || VSet.mem u sccset))
            intersection_graph v true)
        vs
    in

    List.fold_left
      (fun acc vs ->
        if scc_is_minimal vs then
          List.fold_left (fun acc v -> VSet.add v acc) acc vs
        else acc)
      VSet.empty sccs

  let nearest_common_descendant_subgraph (g : graph) (s1 : vertex) (s2 : vertex)
      : graph =
    (* Special case: if source and sink are the same vertex,
       return that vertex plus all its ancestors (functions it calls).
       We need the ancestors to extract their signatures for the analysis. *)
    if G.V.equal s1 s2 then
      reverse_reachable_subgraph g [s1]
    else
      (* forward descendants *)
      let gr1 = reachable_subgraph g s1 in
      let gr2 = reachable_subgraph g s2 in
    (* prune to relevant edges; find nodes reachable from both *)
    let gri = Oper.intersect gr1 gr2 in
    (* now obtain the "nearest common descendants"
    Note that we need to use sccs as otherwise therem might not be any
    closest descendants. Indeed mutually recursive functions can create a graph like
    a ->b; b ->c; c ->b and d -> c.
    In this case the intersection of descendants of a and d are b and c and none of
     them are "closest"*)
    let mins = minimal_scc_vertices gri in
    (* reverse BFS on the ORIGINAL graph from all minima, adding all ancestor edges.
       This ensures every function that calls something in the result is also included. *)
    let sg = G.create () in
    VSet.iter
      (fun seed ->
        if not (G.mem_vertex sg seed) then G.add_vertex sg seed;

        RBfs.iter_component
          (fun v -> G.iter_pred_e (fun edge -> G.add_edge_e sg edge) g v)
          g seed)
      mins;
    sg
end
