(* Graph serialization - save/load/export functions for call graphs *)

open Call_graph_types

(* Convert path to file:// URI *)
let fpath_to_uri path = "file://" ^ Fpath.to_string path

(* Save graph to marshal file *)
let save_graph (graph : G.t) (path : string) =
  let oc = open_out_bin path in
  Marshal.to_channel oc graph [];
  close_out oc

(* Load graph from marshal file *)
let load_graph (path : string) : G.t =
  let ic = open_in_bin path in
  let graph : G.t = Marshal.from_channel ic in
  close_in ic;
  graph

(* Export graph to DOT format *)
let export_dot (graph : G.t) (path : string) =
  let oc = open_out path in
  Dot.output_graph oc graph;
  close_out oc

(* Export graph to JSON format (Graphology format for Cytoscape.js viewer) *)
let graph_to_json (graph : G.t) : Yojson.Basic.t =
  (* Generate unique node ID from name, file, line, col - avoids hash collisions *)
  let node_unique_id v =
    let name = fst v.IL.ident in
    let tok = snd v.IL.ident in
    if Tok.is_fake tok then
      Printf.sprintf "%s|unknown|0|0" name
    else
      let file = Fpath.to_string (Tok.file_of_tok tok) in
      let line = Tok.line_of_tok tok in
      let col = Tok.col_of_tok tok in
      Printf.sprintf "%s|%s|%d|%d" name file line col
  in
  let node_to_json v =
    let id = node_unique_id v in
    let label = fst v.IL.ident in
    let tok = snd v.IL.ident in
    let file, line =
      if Tok.is_fake tok then ("unknown", 0)
      else (fpath_to_uri (Tok.file_of_tok tok), Tok.line_of_tok tok)
    in
    let label = if label = "<top_level>" then
      let filename = if Tok.is_fake tok then "unknown_file"
        else Filename.basename @@ Fpath.to_string (Tok.file_of_tok tok)
      in
      label ^ "\n" ^ filename
    else label in
    (id, `Assoc [
      ("key", `String id);
      ("attributes", `Assoc [
        ("label", `String label);
        ("size", `Int 5);
        ("color", `String "#0074D9");
        ("file", `String file);
        ("line", `Int line);
        ("endLine", `Int (line + 20));
        ("fullLabel", `String label);
        ("nodeType", `String "normal");
        ("sources", `List []);
        ("sinks", `List [])
      ])
    ])
  in
  (* Collect nodes from both vertices AND edge endpoints *)
  let node_map = Hashtbl.create 100 in
  G.iter_vertex (fun v ->
    let (id, json) = node_to_json v in
    Hashtbl.replace node_map id json
  ) graph;
  G.iter_edges_e (fun e ->
    let src, dst = G.E.src e, G.E.dst e in
    let (src_id, src_json) = node_to_json src in
    let (dst_id, dst_json) = node_to_json dst in
    if not (Hashtbl.mem node_map src_id) then Hashtbl.add node_map src_id src_json;
    if not (Hashtbl.mem node_map dst_id) then Hashtbl.add node_map dst_id dst_json
  ) graph;
  let nodes = Hashtbl.fold (fun _ json acc -> json :: acc) node_map [] in

  let edges = G.fold_edges_e (fun edge acc ->
    let src, label, dst = G.E.src edge, G.E.label edge, G.E.dst edge in
    (* Filter out implicit edges (line 0) from display *)
    if label.call_site.line = 0 then acc
    else
      let src_id = node_unique_id src in
      let dst_id = node_unique_id dst in
      (* Use call_site file - this is where the call happens (in the caller) *)
      let call_file = fpath_to_uri label.call_site.file in
      let edge_id = Printf.sprintf "%s->%s@%d" src_id dst_id label.call_site.line in
      let edge_obj = `Assoc [
        ("key", `String edge_id);
        ("source", `String src_id);
        ("target", `String dst_id);
        ("attributes", `Assoc [
          ("line", `Int label.call_site.line);
          ("column", `Int label.call_site.column);
          ("file", `String call_file)
        ])
      ] in
      edge_obj :: acc
  ) graph [] in

  `Assoc [
    ("nodes", `List nodes);
    ("edges", `List edges);
  ]
