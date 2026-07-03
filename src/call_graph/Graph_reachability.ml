module G = Call_graph.G

type graph = G.t
type vertex = G.V.t

module VSet = Set.Make (G.V)

let mem_vertex_either (g : graph) ?(g_global : graph option) (v : vertex)
    : bool =
  G.mem_vertex g v ||
  (match g_global with Some sg -> G.mem_vertex sg v | None -> false)

let fold_pred_either (f : vertex -> 'a -> 'a) (g : graph)
    ?(g_global : graph option) (v : vertex) (init : 'a) : 'a =
  let acc =
    if G.mem_vertex g v then G.fold_pred f g v init else init
  in
  match g_global with
  | Some sg when G.mem_vertex sg v -> G.fold_pred f sg v acc
  | _ -> acc

let iter_succ_e_either (f : G.E.t -> unit) (g : graph)
    ?(g_global : graph option) (v : vertex) : unit =
  if G.mem_vertex g v then G.iter_succ_e f g v;
  match g_global with
  | Some sg when G.mem_vertex sg v -> G.iter_succ_e f sg v
  | _ -> ()

let iter_pred_e_either (f : G.E.t -> unit) (g : graph)
    ?(g_global : graph option) (v : vertex) : unit =
  if G.mem_vertex g v then G.iter_pred_e f g v;
  match g_global with
  | Some sg when G.mem_vertex sg v -> G.iter_pred_e f sg v
  | _ -> ()

(* Depth-limited BFS; only [Call] edges charge the budget ([Dispatch] is
   free — it selects a body, not a call frame).  [max_depth] < 0 is unbounded. *)
let bfs_vertices ?(g_global : graph option)
    (iter_edges : (G.E.t -> unit) -> graph -> ?g_global:graph ->
      vertex -> unit)
    (neighbour_of : G.E.t -> vertex)
    (g : graph) (starts : vertex list) (max_depth : int) : VSet.t =
  (* [Dispatch] edges cost 0 and [Call] edges cost 1 — a 0-1 shortest-path
     problem.  Expand each depth level's full 0-cost [Dispatch] closure
     before charging a [Call] step, so every vertex is first reached (and
     marked visited) at its minimal depth.  A plain FIFO BFS would instead
     finalize a vertex via a 1-cost path popped before a cheaper 0-cost one,
     then prune its onward [Call] expansion at the budget. *)
  let visited = ref VSet.empty in
  (* Grow [seed] by all reachable 0-cost [Dispatch] edges, marking every
     newly-reached vertex visited; return the whole newly-added set. *)
  let dispatch_closure (seed : vertex list) : VSet.t =
    let q = Queue.create () in
    let added = ref VSet.empty in
    let push v =
      if not (VSet.mem v !visited) then begin
        visited := VSet.add v !visited;
        added := VSet.add v !added;
        Queue.push v q
      end
    in
    List.iter push seed;
    while not (Queue.is_empty q) do
      iter_edges (fun (e : G.E.t) ->
        match (G.E.label e).Call_graph.kind with
        | Call_graph.Dispatch -> push (neighbour_of e)
        | Call_graph.Call -> ())
        g ?g_global (Queue.pop q)
    done;
    !added
  in
  (* [Call] successors of [level] not yet visited — the seeds for depth+1. *)
  let call_successors (level : VSet.t) : vertex list =
    VSet.fold (fun v acc ->
      let acc = ref acc in
      iter_edges (fun (e : G.E.t) ->
        match (G.E.label e).Call_graph.kind with
        | Call_graph.Call ->
          let w = neighbour_of e in
          if not (VSet.mem w !visited) then acc := w :: !acc
        | Call_graph.Dispatch -> ())
        g ?g_global v;
      !acc) level []
  in
  let seeds = List.filter (mem_vertex_either g ?g_global) starts in
  let level = ref (dispatch_closure seeds) in
  let depth = ref 0 in
  while not (VSet.is_empty !level) && (max_depth < 0 || !depth < max_depth) do
    level := dispatch_closure (call_successors !level);
    incr depth
  done;
  !visited

let%test "bfs_vertices: reaches a vertex via a cheaper 0-cost dispatch path" =
  (* Edges (kind): S -Call-> W, S -Dispatch-> A, A -Dispatch-> W, W -Call-> X.
     Only [Call] edges charge the budget, so the true cost of X is
     0 (S->A) + 0 (A->W) + 1 (W->X) = 1 <= budget 1, and X MUST be reached.
     A plain FIFO BFS finalizes W at the inflated depth 1 (via the direct
     [Call] edge S->W, popped before the cheaper S->A->W path) and then
     prunes W->X at the budget, wrongly dropping X. *)
  let g = G.create () in
  let tok = Tok.unsafe_fake_tok "" in
  let mk name = Function_id.of_string_and_tok name tok in
  let s = mk "S" and w = mk "W" and a = mk "A" and x = mk "X" in
  List.iter (G.add_vertex g) [ s; w; a; x ];
  Call_graph.add_edge g ~kind:Call_graph.Call ~src:s ~dst:w ~call_tok:tok;
  Call_graph.add_edge g ~kind:Call_graph.Dispatch ~src:s ~dst:a ~call_tok:tok;
  Call_graph.add_edge g ~kind:Call_graph.Dispatch ~src:a ~dst:w ~call_tok:tok;
  Call_graph.add_edge g ~kind:Call_graph.Call ~src:w ~dst:x ~call_tok:tok;
  let reached = bfs_vertices iter_succ_e_either G.E.dst g [ s ] 1 in
  VSet.mem x reached

let induced_subgraph ?(g_global : graph option) (g : graph)
    (vertices : VSet.t) : graph =
  let sg = G.create () in
  VSet.iter (fun v -> G.add_vertex sg v) vertices;
  VSet.iter (fun v ->
    if G.mem_vertex g v then
      G.iter_pred_e (fun e ->
        if VSet.mem (G.E.src e) vertices then
          G.add_edge_e sg e) g v) vertices;
  (match g_global with
   | None -> ()
   | Some ag ->
     VSet.iter (fun v ->
       if G.mem_vertex ag v then
         G.iter_pred_e (fun e ->
           if VSet.mem (G.E.src e) vertices then
             G.add_edge_e sg e) ag v) vertices);
  sg

(* Compute the subgraph containing only functions relevant for taint flow
   from sources to sinks. [depth] caps hops; [g_global] is read-only. *)
let compute_relevant_subgraph ?(g_global : Call_graph.G.t option)
    ?(depth : int option)
    (graph : Call_graph.G.t)
    ~(sources : Function_id.t list) ~(sinks : Function_id.t list)
    : Call_graph.G.t =
  match (sources, sinks) with
  | [], _ | _, [] ->
      Call_graph.G.create ()
  | _ :: _, _ :: _ ->
      let max_depth =
        match depth with
        | Some d -> d
        | None -> -1 (* unbounded *)
      in
      let source_set = VSet.of_list sources in
      let sink_set = VSet.of_list sinks in
      let is_source_or_sink v =
        VSet.mem v source_set || VSet.mem v sink_set
      in

      (* Edges are callee -> caller; the two successor-BFSes intersect at common ancestors of sources and sinks. *)
      let from_sources =
        bfs_vertices ?g_global iter_succ_e_either G.E.dst
          graph sources max_depth
      in
      let from_sinks =
        bfs_vertices ?g_global iter_succ_e_either G.E.dst
          graph sinks max_depth
      in
      let common = VSet.inter from_sources from_sinks in

      (* A node is relevant if:
         1. It's a source or sink, OR
         2. It has a predecessor that is source/sink or in XOR (entry point), OR
         3. It has multiple predecessors in common (bridge between groups) *)
      let is_relevant v =
        is_source_or_sink v ||
        (let preds =
           fold_pred_either (fun p acc -> p :: acc) graph ?g_global v []
         in
         let is_entry = List.exists (fun pred ->
           is_source_or_sink pred ||
           (VSet.mem pred from_sources <> VSet.mem pred from_sinks)
         ) preds in
         let preds_in_common =
           List.filter (fun p -> VSet.mem p common) preds
         in
         is_entry || List.length preds_in_common > 1)
      in
      let relevant = VSet.filter is_relevant common in

      let callee_vertices =
        bfs_vertices ?g_global iter_pred_e_either G.E.src
          graph (VSet.elements relevant) max_depth
      in

      (* Pull in dispatch predecessors (impls) of interface vertices, else
         bodiless interface methods yield empty caller-poisoning sigs (iterated
         to a fixpoint: an impl may call further interfaces needing impls). *)
      let with_impls =
        match g_global with
        | None -> callee_vertices
        | Some sg ->
          let augment (current : VSet.t) : VSet.t =
            VSet.fold (fun (v : vertex) (acc : VSet.t) ->
              if G.mem_vertex sg v then
                G.fold_pred_e (fun (e : G.E.t) (acc : VSet.t) ->
                  match (G.E.label e).Call_graph.kind with
                  | Call_graph.Dispatch ->
                    let impl = G.E.src e in
                    if VSet.mem impl acc then acc
                    else
                      let acc = VSet.add impl acc in
                      if G.mem_vertex sg impl then
                        (* Only Call edges; the impl's own Dispatch edges are handled by the next outer iteration. *)
                        G.fold_pred_e (fun (e : G.E.t) (acc : VSet.t) ->
                          match (G.E.label e).Call_graph.kind with
                          | Call_graph.Call -> VSet.add (G.E.src e) acc
                          | Call_graph.Dispatch -> acc
                        ) sg impl acc
                      else acc
                  | Call_graph.Call -> acc
                ) sg v acc
              else acc
            ) current current
          in
          let rec loop (n : int) (current : VSet.t) : VSet.t =
            (* [n = 0], not [n <= 0]: negative [max_depth] is unbounded (runs to the monotone fixpoint). *)
            if n = 0 then current
            else
              let next = augment current in
              if VSet.equal next current then current
              else loop (n - 1) next
          in
          loop max_depth callee_vertices
      in
      induced_subgraph ?g_global graph with_impls
