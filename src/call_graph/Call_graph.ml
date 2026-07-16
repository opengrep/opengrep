module Log = Log_call_graph.Log

(** Call graph node - just a function identifier (IL.name) *)
type node = Function_id.t

let hash_node = Function_id.hash

let compare_node = Function_id.compare

let equal_node = Function_id.equal

let show_node = Function_id.show

type edge_kind =
  | Call
  | Dispatch  (** implementation <-> interface *)
[@@deriving show, eq, ord]

 (** Call graph edge - represents a call from one function to another *)
type edge = {
  call_site : Pos.t;
  kind : edge_kind;
}
[@@deriving show, eq, ord]

(* Required by OCamlGraph's ConcreteBidirectionalLabeled functor.
   Never actually used since we always call G.add_edge_e with explicit labels. *)
let default_edge = {
  call_site = { Pos.bytepos = 0; line = 0; column = 0; file = Fpath.v "." };
  kind = Call;
}

(** The call graph module - bidirectional labeled graph *)
module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = node
      let compare = compare_node
      let hash = hash_node
      let equal = equal_node
    end)
    (struct
      type t = edge
      let compare = compare_edge
      let default = default_edge
    end)

(** For DOT export *)
module Display = struct
  include G

  let vertex_name (v : node) =
    let fn_name = show_node v in
    let file, line, col = Function_id.to_file_line_col v in
    Printf.sprintf "\"%s at l:%d c:%d\\n%s\"" fn_name line col file

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes (_, label, _) =
    let pos = label.call_site in
    [ `Label (Printf.sprintf "l:%d c:%d" pos.Pos.line pos.Pos.column) ]
  let get_subgraph _ = None
end

module Dot = Graph.Graphviz.Dot (Display)
    
(* OCamlgraph: Use built-in algorithms *)
module Topo = Graph.Topological.Make (G)
module SCC = Graph.Components.Make (G)

(** Helpers **)

let pos_of_tok (tok : Tok.t) : Pos.t =
  if Tok.is_fake tok then
    { Pos.bytepos = 0; line = 0; column = 0; file = Fpath.v "." }
  else
    let loc = Tok.unsafe_loc_of_tok tok in
    { Pos.bytepos = loc.pos.bytepos; line = loc.pos.line; column = loc.pos.column; file = loc.pos.file }

(* Helper to create an edge label from call site token and optional kind *)
let mk_edge_label ?(kind : edge_kind = Call) (call_tok : Tok.t) : edge =
  { call_site = pos_of_tok call_tok; kind }

(* Helper to add an edge to the graph: src -> dst with call site info *)
let add_edge ?(kind : edge_kind = Call) (graph : G.t) ~(src : node) ~(dst : node) ~(call_tok : Tok.t) =
  let edge_label = mk_edge_label ~kind call_tok in
  G.add_edge_e graph (G.E.create src edge_label dst)

(* Query call graph to find callee node based on call site token.
   Note: edges go callee -> caller, so we use pred_e to get edges into caller. *)
let lookup_callee_from_graph (graph : G.t option)
    (caller_node : node option) (call_tok : Tok.t) : node option =
  match graph, caller_node with
  | None, _ ->
      Log.debug (fun m ->
          m "CALL_GRAPH: Graph is None during lookup!");
      None
  | _, None ->
      Log.debug (fun m ->
          m "CALL_GRAPH: caller_node is None during lookup!");
      None
  | Some g, Some caller ->
      if not (G.mem_vertex g caller) then (
        Log.debug (fun m ->
            m "CALL_GRAPH: Caller %s not in graph"
              (Function_id.show_debug caller));
        None
      ) else
        let call_pos = pos_of_tok call_tok in
        let incoming_edges = G.pred_e g caller in

        Log.debug (fun m ->
            m "CALL_GRAPH LOOKUP: caller=%s call_pos=(%s:%d:%d bytepos=%d) incoming_edges=%d"
              (show_node caller)
              (Fpath.to_string call_pos.Pos.file)
              call_pos.Pos.line call_pos.Pos.column call_pos.Pos.bytepos
              (List.length incoming_edges));

        (* Find edge with matching call_site position *)
        let exact_match =
          incoming_edges
          |> List.find_opt (fun edge ->
              let label = G.E.label edge in
              Pos.equal label.call_site call_pos)
        in
        (match exact_match with
        | Some edge ->
            Some (G.E.src edge)
        | None ->
            (* Match by line/column/file: dispatch edges have bytepos=0, so Pos.equal against real-byte-offset AST tokens always fails. *)
            (* The whole iteration lives inside the Log.debug callback so
               the per-call-site hot path does no work when debug is off. *)
            Log.debug (fun m ->
                let same_line_edges =
                  incoming_edges
                  |> List.filter_map (fun edge ->
                         let (site : Pos.t) = (G.E.label edge).call_site in
                         if Int.equal site.line call_pos.Pos.line then
                           Some
                             (Printf.sprintf "callee=%s site=(%s:%d:%d bytepos=%d)"
                                (show_node (G.E.src edge))
                                (Fpath.to_string site.file)
                                site.line site.column site.bytepos)
                         else None)
                in
                m "CALL_GRAPH LOOKUP: same-line edges: %s"
                  (String.concat "; " same_line_edges));
            let lc_match =
              incoming_edges
              |> List.find_opt (fun edge ->
                  let (site : Pos.t) = (G.E.label edge).call_site in
                  Int.equal site.line call_pos.Pos.line
                  && Int.equal site.column call_pos.Pos.column
                  && Fpath_.equal site.file call_pos.Pos.file)
            in
            match lc_match with
            | Some edge ->
                Some (G.E.src edge)
            | None -> None)

(* Implementations of an interface node (incoming Dispatch edges). *)
let dispatch_predecessors (graph : G.t) (node : node) : node list =
  if not (G.mem_vertex graph node) then []
  else
    G.pred_e graph node
    |> List.filter_map (fun edge ->
      let label = G.E.label edge in
      match label.kind with
      | Dispatch -> Some (G.E.src edge)
      | Call -> None)
    (* Sort: OCamlGraph's pred_e is Hashtbl-backed, so raw order is random. *)
    |> List.sort Function_id.compare

(* New graph with all paths absolute (relative resolved against [project_root]; already-absolute and fake-token vertices unchanged). *)
let make_paths_absolute (project_root : Fpath.t) (graph : G.t) : G.t =
  let abs_pos (pos : Pos.t) : Pos.t =
    if Fpath.is_abs pos.Pos.file then pos
    else
      let abs_file = Fpath.(project_root // pos.Pos.file) |> Fpath.normalize in
      { pos with Pos.file = abs_file }
  in
  let new_graph = G.create () in
  G.iter_edges_e
    (fun edge ->
      let src = Function_id.make_absolute project_root (G.E.src edge) in
      let dst = Function_id.make_absolute project_root (G.E.dst edge) in
      let label = G.E.label edge in
      let label = { label with call_site = abs_pos label.call_site } in
      G.add_edge_e new_graph (G.E.create src label dst))
    graph;
  (* Preserve isolated vertices (no edges). *)
  G.iter_vertex
    (fun v ->
      let v' = Function_id.make_absolute project_root v in
      if not (G.mem_vertex new_graph v') then G.add_vertex new_graph v')
    graph;
  new_graph
