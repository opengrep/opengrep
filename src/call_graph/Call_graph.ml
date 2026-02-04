module Log = Log_call_graph.Log

(** Call graph node - just a function identifier (IL.name) *)
type node = Function_id.t

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

(* Query call graph to find callee node based on call site token.
   Note: edges go callee -> caller, so we use pred_e to get edges into caller *)
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
            m "CALL_GRAPH: Caller %s not in graph" (show_node caller));
        None
      ) else
        let call_pos = pos_of_tok call_tok in
        (* Get edges coming INTO the caller (callee -> caller) *)
        let incoming_edges = G.pred_e g caller in

        (* Find edge with matching call_site position *)
        let exact_match =
          incoming_edges
          |> List.find_opt (fun edge ->
              let label = G.E.label edge in
              Pos.equal label.call_site call_pos)
        in
        match exact_match with
        | Some edge ->
            Some (G.E.src edge)
        | None ->
            (* No fallback - return None so external calls use direct signature lookup.
               Previously there was a line 0 fallback that matched implicit/HOF edges,
               but this caused wrong signature lookups for external method calls. *)
            None
