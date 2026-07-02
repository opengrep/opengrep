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
  let visited = ref VSet.empty in
  let queue = Queue.create () in
  List.iter (fun (s : vertex) ->
    if mem_vertex_either g ?g_global s
       && not (VSet.mem s !visited) then begin
      visited := VSet.add s !visited;
      Queue.push (s, 0) queue
    end) starts;
  while not (Queue.is_empty queue) do
    let v, d = Queue.pop queue in
    iter_edges (fun (e : G.E.t) ->
      let w = neighbour_of e in
      if not (VSet.mem w !visited) then begin
        match (G.E.label e).Call_graph.kind with
        | Call_graph.Dispatch ->
          visited := VSet.add w !visited;
          Queue.push (w, d) queue
        | Call_graph.Call ->
          if max_depth < 0 || d < max_depth then begin
            visited := VSet.add w !visited;
            Queue.push (w, d + 1) queue
          end
      end) g ?g_global v
  done;
  !visited

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
