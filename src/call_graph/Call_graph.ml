module Log = Log_call_graph.Log

(** Call graph node - just a function identifier (IL.name) *)
type node = Function_id.t

(* Helper to extract normalized key for node comparison.
   Resolves file paths to canonical form to handle different representations
   of the same file (e.g., /foo/bar vs /foo/baz/../bar). *)

let hash_node = Function_id.hash

let compare_node = Function_id.compare

let equal_node = Function_id.equal

let show_node = Function_id.show

 (** Call graph edge - represents a call from one function to another *)
type edge = {
  call_site : Pos.t;
}
[@@deriving show, eq, ord]

(* Required by OCamlGraph's ConcreteBidirectionalLabeled functor.
   Never actually used since we always call G.add_edge_e with explicit labels. *)
let default_edge = {
  call_site = { Pos.bytepos = 0; line = 0; column = 0; file = Fpath.v "." };
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
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graph.Graphviz.Dot (Display)
    
(* OCamlgraph: Use built-in algorithms *)
module Topo = Graph.Topological.Make (G)
module SCC = Graph.Components.Make (G)

let node_key (n : node) =
  let name = Function_id.show n in 
  let filename, line, col = Function_id.to_file_line_col n in
  Printf.sprintf "%s|%s|%d|%d" name filename line col

(** Helpers **)

let pos_of_tok (tok : Tok.t) : Pos.t =
  if Tok.is_fake tok then
    { Pos.bytepos = 0; line = 0; column = 0; file = Fpath.v "." }
  else
    let loc = Tok.unsafe_loc_of_tok tok in
    { Pos.bytepos = loc.pos.bytepos; line = loc.pos.line; column = loc.pos.column; file = loc.pos.file }

(* Helper to create an edge label from callee node and call site token *)
let mk_edge_label (call_tok : Tok.t) : edge =
  { call_site = pos_of_tok call_tok }

(* Helper to add an edge to the graph: src -> dst with call site info *)
let add_edge (graph : G.t) ~(src : node) ~(dst : node) ~(call_tok : Tok.t) =
  let edge_label = mk_edge_label call_tok in
  G.add_edge_e graph (G.E.create src edge_label dst)

(* Query call graph to find callee node based on call site token *)
let lookup_callee_from_graph (graph : G.t option)
    (caller_node : node option) (call_tok : Tok.t) : node option =
  match graph with
  | None ->
      Log.debug (fun m ->
          m "CALL_GRAPH: Graph is None during lookup! caller=%s, call_tok=%s"
            (Option.fold ~none:"<none>" ~some:show_node caller_node)
            (Tok.content_of_tok call_tok));
      None
  | Some g ->
      (* Don't check mem_vertex - just search ALL edges for matching call_site token.
         We match purely on call site location. *)
      Log.debug (fun m ->
          m "CALL_GRAPH: Looking for call_tok at %s in caller context %s"
            (Tok.content_of_tok call_tok)
            (Option.fold ~none:"<none>" ~some:show_node caller_node));

      (* Iterate through ALL edges in the graph *)
      (* Convert call_tok to Pos.t for comparison *)
      let call_pos = pos_of_tok call_tok in
      let all_edges = ref [] in
      G.iter_edges_e (fun e -> all_edges := e :: !all_edges) g;
      Log.debug (fun m -> m "CALL_GRAPH: Searching %d total edges for call_tok at L%d:C%d"
        (List.length !all_edges) call_pos.Pos.line call_pos.Pos.column);
      (* Log all edges for debugging *)
      List.iter (fun edge ->
          let label = G.E.label edge in
          let src = G.E.src edge in
          let dst = G.E.dst edge in
          Log.debug (fun m ->
              m "CALL_GRAPH: Edge: %s -> %s (call_site=L%d:C%d)"
                (show_node src)
                (show_node dst)
                label.call_site.Pos.line label.call_site.Pos.column))
        !all_edges;

      (* Find edge with matching call_site position *)
      let exact_match =
        !all_edges
        |> List.find_opt (fun edge ->
            let label = G.E.label edge in
            (* Compare by position *)
            let matches = Pos.equal label.call_site call_pos in
            if matches then
              Log.debug (fun m ->
                  m "CALL_GRAPH: MATCH FOUND! call_site=L%d:C%d, callee=%s"
                    label.call_site.Pos.line label.call_site.Pos.column
                    (show_node (G.E.src edge)));
            matches)
      in
      match exact_match with
      | Some edge ->
          Some (G.E.src edge) 
      | None ->
          (* Fallback: check for implicit/HOF edges by matching line 0 (fake position) *)
          !all_edges
          |> List.find_opt (fun edge ->
              let label = G.E.label edge in
              (* Implicit edges have line 0 - match by callee name if call is to same function *)
              Int.equal label.call_site.Pos.line 0)
          |> Option.map G.E.src
