(* Call Graph Types - core types for call graph construction and analysis *)

(** Position in a file (line/column) *)
type position = { line : int; column : int [@key "character"] }
[@@deriving yojson, eq, show, ord]

(** Range in a file (start/end positions) *)
type range = { start : position; end_ : position [@key "end"] }
[@@deriving yojson, eq, show, ord]

(** Function identifier - IL.name contains name string + token with file/position *)
type function_id = IL.name option
[@@deriving show, eq, ord]

(* Hash by name string, filename (basename), and position from token.
   Using basename instead of full path avoids duplicate nodes when the same file
   is accessed via different path representations (e.g., absolute vs relative). *)
let hash_function_id (fn : function_id) : int =
  match fn with
  | None -> 0
  | Some n ->
      let tok = snd n.IL.ident in
      if Tok.is_fake tok then
        Hashtbl.hash (fst n.IL.ident)
      else
        let file = Tok.file_of_tok tok in
        let filename = Fpath.basename file in
        let line = Tok.line_of_tok tok in
        let col = Tok.col_of_tok tok in
        Hashtbl.hash (fst n.IL.ident, filename, line, col)

(** Call graph node - just a function identifier (IL.name) *)
type node = IL.name
[@@deriving show]

(* Helper to extract normalized key for node comparison.
   Resolves file paths to canonical form to handle different representations
   of the same file (e.g., /foo/bar vs /foo/baz/../bar). *)
let node_compare_key (v : node) =
  let tok = snd v.IL.ident in
  if Tok.is_fake tok then
    (fst v.IL.ident, "", 0, 0)
  else
    let file = Tok.file_of_tok tok in
    (* Use realpath to get canonical absolute path, fall back to normalized if file doesn't exist *)
    let normalized_file =
      try Unix.realpath (Fpath.to_string file)
      with Unix.Unix_error _ -> Fpath.to_string (Fpath.normalize file)
    in
    let line = Tok.line_of_tok tok in
    let col = Tok.col_of_tok tok in
    (fst v.IL.ident, normalized_file, line, col)

let hash_node (v : node) =
  Hashtbl.hash (node_compare_key v)

let compare_node n1 n2 =
  compare (node_compare_key n1) (node_compare_key n2)

let equal_node n1 n2 =
  (node_compare_key n1) = (node_compare_key n2)

(** Call graph edge - represents a call from one function to another *)
type edge = {
  callee_fn_id : IL.name;
  call_site : Pos.t;
}
[@@deriving show, eq, ord]

(* Required by OCamlGraph's ConcreteBidirectionalLabeled functor.
   Never actually used since we always call G.add_edge_e with explicit labels. *)
let default_edge = {
  callee_fn_id = IL.{
    ident = ("", Tok.unsafe_fake_tok "");
    sid = AST_generic.SId.unsafe_default;
    id_info = AST_generic.empty_id_info ();
  };
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
    let fn_name = fst v.IL.ident in
    let tok = snd v.IL.ident in
    let file, line, col =
      if Tok.is_fake tok then ("unknown", 0, 0)
      else (Fpath.to_string (Tok.file_of_tok tok), Tok.line_of_tok tok, Tok.col_of_tok tok)
    in
    Printf.sprintf "\"%s at l:%d c:%d\\n%s\"" fn_name line col file

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graph.Graphviz.Dot (Display)

(** Node tracking for incremental updates *)
let removed_node_keys : (string, unit) Hashtbl.t = Hashtbl.create 1000

let node_key (n : node) =
  let (name, filename, line, col) = node_compare_key n in
  Printf.sprintf "%s|%s|%d|%d" name filename line col

let clear_removed_nodes () = Hashtbl.clear removed_node_keys
let mark_node_removed (n : node) = Hashtbl.add removed_node_keys (node_key n) ()
let was_node_removed (n : node) = Hashtbl.mem removed_node_keys (node_key n)
