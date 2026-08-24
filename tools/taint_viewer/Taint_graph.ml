(* Convert taint traces to Cytoscape elements by recursively walking
   the CliCall/CliLoc trace structure. *)

module G = Call_graph.G

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_of_type = function
  | "source" -> "#2ecc71"
  | "sink" -> "#e74c3c"
  | "call" -> "#9b59b6"
  | "intermediate" -> "#3498db"
  | _ -> "#3498db"

(*****************************************************************************)
(* FuncIndex: file+line -> Function_id *)
(*****************************************************************************)

module FuncIndex = struct
  type t = (string, (int * Function_id.t) list) Hashtbl.t

  let build (graph : G.t) : t =
    let tbl : (string, (int * Function_id.t) list) Hashtbl.t = Hashtbl.create 1024 in
    G.iter_vertex (fun v ->
      let file, line, _ = Function_id.to_file_line_col v in
      let existing = try Hashtbl.find tbl file with Not_found -> [] in
      Hashtbl.replace tbl file ((line, v) :: existing)
    ) graph;
    Hashtbl.iter (fun file funcs ->
      Hashtbl.replace tbl file (List.sort (fun (l1, _) (l2, _) -> Int.compare l2 l1) funcs)
    ) tbl;
    tbl

  (* Start line of the next function in the file after [line], if any. *)
  let next_def_line (idx : t) (file : string) (line : int) : int option =
    match Hashtbl.find_opt idx file with
    | None -> None
    | Some funcs ->
        (* [funcs] is sorted by line descending: the last match is the smallest. *)
        List.fold_left (fun acc (l, _) -> if l > line then Some l else acc)
          None funcs

  let find (idx : t) (file : string) (line : int) : Function_id.t option =
    match Hashtbl.find_opt idx file with
    | None -> None
    | Some funcs ->
        List.find_map (fun (func_line, fid) ->
          if func_line <= line then Some fid else None
        ) funcs
end

(*****************************************************************************)
(* FileCache: read source lines with caching *)
(*****************************************************************************)

module FileCache = struct
  type t = (string, string array) Hashtbl.t

  let create () : t = Hashtbl.create 64

  let read_file (cache : t) (file : string) : string array =
    match Hashtbl.find_opt cache file with
    | Some arr -> arr
    | None ->
        let arr =
          try
            let ic = open_in file in
            let rec read acc =
              match input_line ic with
              | l -> read (l :: acc)
              | exception End_of_file -> close_in ic; Array.of_list (List.rev acc)
            in
            read []
          with _ -> [||]
        in
        Hashtbl.add cache file arr;
        arr

  let get_lines (cache : t) (file : string) (line : int) : (int * string) list =
    let lines_arr = read_file cache file in
    let start_line = max 1 (line - 3) in
    let end_line = min (Array.length lines_arr) (line + 3) in
    let result = ref [] in
    for i = end_line downto start_line do
      if i >= 1 && i <= Array.length lines_arr then
        result := (i, lines_arr.(i - 1)) :: !result
    done;
    !result

  (* Lines [line-1 .. stop], clamped to the file. *)
  let get_func_lines (cache : t) (file : string) (line : int) ~(stop : int)
      : (int * string) list =
    let lines_arr = read_file cache file in
    let start_line = max 1 (line - 1) in
    let end_line = min (Array.length lines_arr) stop in
    if end_line < start_line then []
    else
      List.init (end_line - start_line + 1) (fun i ->
        let ln = start_line + i in
        (ln, lines_arr.(ln - 1)))
end

(*****************************************************************************)
(* Typed trace types *)
(*****************************************************************************)

type trace_loc = {
  path : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

type trace_var = {
  location : trace_loc;
  content : string;
}

type call_trace =
  | CliLoc of trace_loc * string
  | CliCall of (trace_loc * string) * trace_var list * call_trace

type dataflow_trace = {
  taint_source : call_trace option;
  intermediate_vars : trace_var list;
  taint_sink : call_trace option;
}

(*****************************************************************************)
(* JSON parsing into typed trace *)
(*****************************************************************************)

let parse_location (json : Yojson.Basic.t) : trace_loc =
  match json with
  | `Assoc fields ->
      (* opengrep --json paths are relative to the scan cwd; the call graph
         uses absolute paths, so resolve against the cwd here. *)
      let path = match List.assoc_opt "path" fields with
        | Some (`String s) when s <> "" && Filename.is_relative s ->
            Filename.concat (Sys.getcwd ()) s
        | Some (`String s) -> s | _ -> "" in
      let start_line, start_col = match List.assoc_opt "start" fields with
        | Some (`Assoc s) ->
            (match List.assoc_opt "line" s with Some (`Int l) -> l | _ -> 0),
            (match List.assoc_opt "col" s with Some (`Int c) -> c | _ -> 0)
        | _ -> 0, 0 in
      let end_line, end_col = match List.assoc_opt "end" fields with
        | Some (`Assoc e) ->
            (match List.assoc_opt "line" e with Some (`Int l) -> l | _ -> 0),
            (match List.assoc_opt "col" e with Some (`Int c) -> c | _ -> 0)
        | _ -> 0, 0 in
      { path; start_line; start_col; end_line; end_col }
  | _ -> { path = ""; start_line = 0; start_col = 0; end_line = 0; end_col = 0 }

let parse_intermediate_var (json : Yojson.Basic.t) : trace_var option =
  match json with
  | `Assoc fields ->
      let location = match List.assoc_opt "location" fields with
        | Some loc -> parse_location loc
        | None -> { path = ""; start_line = 0; start_col = 0; end_line = 0; end_col = 0 } in
      let content = match List.assoc_opt "content" fields with
        | Some (`String s) -> s | _ -> "" in
      Some { location; content }
  | _ -> None

let parse_intermediate_vars (json : Yojson.Basic.t) : trace_var list =
  match json with
  | `List items -> List.filter_map parse_intermediate_var items
  | _ -> []

let rec parse_call_trace (json : Yojson.Basic.t) : call_trace option =
  match json with
  | `List [`String "CliLoc"; `List [loc_json; `String content]] ->
      let loc = parse_location loc_json in
      Some (CliLoc (loc, content))
  | `List [`String "CliCall"; `List [loc_and_content; vars_json; inner_json]] ->
      let call_site_loc, call_site_content = match loc_and_content with
        | `List [loc_json; `String content] -> parse_location loc_json, content
        | _ -> { path = ""; start_line = 0; start_col = 0; end_line = 0; end_col = 0 }, ""
      in
      let vars = parse_intermediate_vars vars_json in
      (match parse_call_trace inner_json with
       | Some inner -> Some (CliCall ((call_site_loc, call_site_content), vars, inner))
       | None -> None)
  | _ -> None

let parse_dataflow_trace (json : Yojson.Basic.t) : dataflow_trace option =
  match json with
  | `Assoc fields ->
      let taint_source = match List.assoc_opt "taint_source" fields with
        | Some j -> parse_call_trace j | None -> None in
      let intermediate_vars = match List.assoc_opt "intermediate_vars" fields with
        | Some j -> parse_intermediate_vars j | None -> [] in
      let taint_sink = match List.assoc_opt "taint_sink" fields with
        | Some j -> parse_call_trace j | None -> None in
      Some { taint_source; intermediate_vars; taint_sink }
  | _ -> None

(*****************************************************************************)
(* Graph element types — fully immutable *)
(*****************************************************************************)

type node_type = Source | Sink | Intermediate | CallSite

let string_of_node_type = function
  | Source -> "source"
  | Sink -> "sink"
  | Intermediate -> "intermediate"
  | CallSite -> "call"

type cyto_node = {
  id : string;
  parent : string option;
  label : string;
  node_type : node_type;
  file : string;
  line : int;
  col : int;
  code_context : (int * string) list;
}

type func_box = {
  id : string;
  label : string;
  file : string;
  line : int;
  code_context : (int * string) list;
  resolved : bool;  (* true = FuncIndex found the function name *)
}

type edge_style = Solid | Dashed

type cyto_edge = {
  id : string;
  source : string;
  target : string;
  style : edge_style;
  label : string;
  verified : bool;  (* false = the call graph couldn't confirm the cross-function transition *)
}

type graph_acc = {
  nodes : cyto_node list;
  func_boxes : func_box list;
  edges : cyto_edge list;
  counter : int;
}

let empty_acc = { nodes = []; func_boxes = []; edges = []; counter = 0 }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let top_location_of = function
  | CliLoc (loc, _) -> loc
  | CliCall ((loc, _), _, _) -> loc

let with_parent parent n = { n with parent = Some parent }

let make_node ~file_cache id node_type loc content =
  { id; parent = None; label = content; node_type;
    file = loc.path; line = loc.start_line; col = loc.start_col;
    code_context = FileCache.get_lines file_cache loc.path loc.start_line; }

(* One box per function: same function anywhere in the trace maps to the
   same box id, so revisits merge instead of duplicating the box. *)
let fid_box_id (fid : Function_id.t) : string =
  let f, l, c = Function_id.to_file_line_col fid in
  Printf.sprintf "fb_%d_%d_%d" (Hashtbl.hash f) l c

let box_of_fid ~func_index ~file_cache (fid : Function_id.t) : func_box =
  let f, l, _ = Function_id.to_file_line_col fid in
  (* The function runs until the next function in the file (capped for the
     last one); the index has no end positions. *)
  let stop = match FuncIndex.next_def_line func_index f l with
    | Some next_l -> next_l - 1
    | None -> l + 300
  in
  { id = fid_box_id fid; label = Function_id.show fid; file = f; line = l;
    code_context = FileCache.get_func_lines file_cache f l ~stop; resolved = true; }

let make_func_box ~func_index ~file_cache id loc =
  let fid = FuncIndex.find func_index loc.path loc.start_line in
  match fid with
  | Some fid -> box_of_fid ~func_index ~file_cache fid
  | None ->
      { id; label = Printf.sprintf "%s:%d" (Filename.basename loc.path) loc.start_line;
        file = loc.path; line = loc.start_line; code_context = []; resolved = false; }

let chain_edges_list node_ids style =
  let rec go = function
    | [] | [_] -> []
    | a :: (b :: _ as rest) ->
        { id = Printf.sprintf "e_%s_%s" a b;
          source = a; target = b;
          style; label = ""; verified = true; } :: go rest
  in go node_ids

(*****************************************************************************)
(* Call graph: check if a location is a call site *)
(*****************************************************************************)

(* Get a function and all its dispatch-equivalent variants
   (interface <-> implementation) in the call graph *)
let dispatch_successors (g : G.t) (fid : Function_id.t) : Function_id.t list =
  if not (G.mem_vertex g fid) then []
  else
    G.succ_e g fid
    |> List.filter_map (fun edge ->
      match (G.E.label edge).Call_graph.kind with
      | Call_graph.Dispatch -> Some (G.E.dst edge)
      | Call_graph.Call -> None)

let dispatch_variants (g : G.t) (fid : Function_id.t) : Function_id.t list =
  fid
  :: Call_graph.dispatch_predecessors g fid
  @ dispatch_successors g fid

(* Check if fid matches target or any of its dispatch variants *)
let matches_with_dispatch (g : G.t) (fid : Function_id.t) (target : Function_id.t) : bool =
  Function_id.equal fid target
  || List.exists (fun v -> Function_id.equal fid v) (dispatch_variants g target)

(* Check if the call graph has a call edge at the given location.
   Edges are stored callee -> caller, so the call sites inside function F
   are F's INCOMING edges (their call_site position lies in F's body).
   Match by file+line, and require the callee token to fall inside the
   trace var's span (graph columns are 0-based, trace columns 1-based) so
   other vars on the call line don't match.
   Returns Some callee_name if it's a call site, None otherwise. *)
let is_call_site ~call_graph ~func_index (loc : trace_loc) : string option =
  match call_graph with
  | None -> None
  | Some g ->
      let caller_fid = FuncIndex.find func_index loc.path loc.start_line in
      match caller_fid with
      | None -> None
      | Some caller ->
          if not (G.mem_vertex g caller) then None
          else
            let incoming = G.pred_e g caller in
            List.find_map (fun edge ->
              let site = (G.E.label edge).Call_graph.call_site in
              let site_col = site.Pos.column + 1 in
              if Int.equal site.Pos.line loc.start_line
                 && String.equal (Fpath.to_string site.Pos.file) loc.path
                 && loc.start_col <= site_col
                 && (Int.equal loc.end_col 0 || site_col <= loc.end_col)
              then Some (Function_id.show (G.E.src edge))
              else None
            ) incoming

(* Check if call graph has any call edge between two functions (either direction).
   Accounts for dispatch edges (interface <-> implementation). *)
let funcs_connected ~call_graph (fid_a : Function_id.t option) (fid_b : Function_id.t option) : bool =
  match call_graph, fid_a, fid_b with
  | Some g, Some a, Some b ->
      let a_variants = dispatch_variants g a in
      let b_variants = dispatch_variants g b in
      let a_calls_b = List.exists (fun av ->
        G.mem_vertex g av
        && G.pred_e g av |> List.exists (fun edge ->
            List.exists (fun bv ->
              Function_id.equal (G.E.src edge) bv) b_variants)
      ) a_variants in
      let b_calls_a = List.exists (fun bv ->
        G.mem_vertex g bv
        && G.pred_e g bv |> List.exists (fun edge ->
            List.exists (fun av ->
              Function_id.equal (G.E.src edge) av) a_variants)
      ) b_variants in
      a_calls_b || b_calls_a
  | _ -> false

(* Verify a CliCall inter-function edge against the call graph.
   The trace already encodes the call relationship; we just confirm that
   the call graph agrees these two functions are connected (via call or dispatch). *)
let verify_call_edge ~call_graph ~func_index
    ~(call_site : trace_loc) ~(callee_loc : trace_loc) =
  match call_graph with
  | None -> `NoGraph
  | Some _ ->
      let caller_fid = FuncIndex.find func_index call_site.path call_site.start_line in
      let callee_fid = FuncIndex.find func_index callee_loc.path callee_loc.start_line in
      if funcs_connected ~call_graph caller_fid callee_fid then `Verified
      else `Unverified

(*****************************************************************************)
(* Group consecutive vars by owning function *)
(*****************************************************************************)

(* A group of consecutive vars that belong to the same function *)
type var_group = {
  func_id : Function_id.t option;
  vars : trace_var list;  (* in order *)
}

let same_func a b =
  match a, b with
  | Some fa, Some fb -> Function_id.equal fa fb
  | _ -> false

(* Group a list of vars into consecutive runs by owning function.
   Each var is looked up via FuncIndex. *)
let group_vars_by_func ~func_index (vars : trace_var list) : var_group list =
  match vars with
  | [] -> []
  | first :: rest ->
      let first_fid = FuncIndex.find func_index first.location.path first.location.start_line in
      let groups_rev, cur_fid, cur_vars_rev =
        List.fold_left (fun (groups, cur_fid, cur_vars) (v : trace_var) ->
          let v_fid = FuncIndex.find func_index v.location.path v.location.start_line in
          if same_func cur_fid v_fid then
            (groups, cur_fid, v :: cur_vars)
          else
            let finished = { func_id = cur_fid; vars = List.rev cur_vars } in
            (finished :: groups, v_fid, [v])
        ) ([], first_fid, [first]) rest
      in
      let last = { func_id = cur_fid; vars = List.rev cur_vars_rev } in
      List.rev (last :: groups_rev)

(*****************************************************************************)
(* Process intermediate vars into grouped function boxes *)
(*****************************************************************************)

(* Result of processing a list of intermediate vars *)
type vars_result = {
  vr_nodes : cyto_node list;
  vr_boxes : func_box list;
  vr_edges : cyto_edge list;
  vr_first_id : string option;  (* id of first node for chaining *)
  vr_last_id : string option;   (* id of last node for chaining *)
  vr_counter : int;
}

(* Process a list of intermediate vars, grouping consecutive vars by function.
   Groups in the same function as parent_func_id are parented to parent_box_id.
   Groups in a different function get their own box.
   Cross-function transitions are dashed and colored red if the call graph can't verify. *)
let process_vars ~func_index ~file_cache ~call_graph ~prefix
    ~parent_box_id ~parent_func_id
    (vars : trace_var list) (counter : int) : vars_result =
  match vars with
  | [] -> { vr_nodes = []; vr_boxes = []; vr_edges = [];
            vr_first_id = None; vr_last_id = None; vr_counter = counter }
  | _ ->
      let groups = group_vars_by_func ~func_index vars in
      let counter, all_nodes_rev, all_boxes_rev, all_edges_rev, group_boundaries_rev, _ =
        List.fold_left (fun (c, nodes_acc, boxes_acc, edges_acc, boundaries_acc, left_parent) group ->
          (* Merge into parent only if we haven't left it yet *)
          let matches_parent = match parent_func_id with
            | Some _ -> same_func group.func_id parent_func_id
            | None -> false
          in
          let in_parent = matches_parent && not left_parent in
          let left_parent = left_parent || not matches_parent in
          let box_id, new_box, c =
            if in_parent then
              (parent_box_id, None, c)
            else
              match group.func_id with
              | Some fid ->
                  let box = box_of_fid ~func_index ~file_cache fid in
                  (box.id, Some box, c)
              | None ->
                  let box_id = Printf.sprintf "%s_vbox%d" prefix c in
                  let loc = (List.hd group.vars).location in
                  let box =
                    { id = box_id;
                      label = Printf.sprintf "%s:%d" (Filename.basename loc.path) loc.start_line;
                      file = loc.path; line = loc.start_line; code_context = []; resolved = false; }
                  in
                  (box_id, Some box, c + 1)
          in
          (* Create nodes for each var in the group *)
          let c, var_nodes_rev = List.fold_left (fun (c, ns) (v : trace_var) ->
            let vid = Printf.sprintf "%s_v%d" prefix c in
            let ntype = match is_call_site ~call_graph ~func_index v.location with
              | Some _ -> CallSite
              | None -> Intermediate
            in
            let node = { (make_node ~file_cache vid ntype v.location v.content)
                         with parent = Some box_id } in
            (c + 1, node :: ns)
          ) (c, []) group.vars in
          let var_nodes = List.rev var_nodes_rev in
          let var_ids = List.map (fun (n : cyto_node) -> n.id) var_nodes in
          (* Intra-group edges *)
          let intra = chain_edges_list var_ids Solid in
          let first_id = List.hd var_ids in
          let last_id = List.nth var_ids (List.length var_ids - 1) in
          let new_boxes = match new_box with Some b -> [b] | None -> [] in
          (c,
           List.rev_append var_nodes nodes_acc,
           List.rev_append new_boxes boxes_acc,
           List.rev_append intra edges_acc,
           (first_id, last_id, group.func_id) :: boundaries_acc,
           left_parent)
        ) (counter, [], [], [], [], false) groups
      in
      let all_nodes = List.rev all_nodes_rev in
      let all_boxes = List.rev all_boxes_rev in
      let all_edges = List.rev all_edges_rev in
      let group_boundaries = List.rev group_boundaries_rev in
      (* Inter-group edges: solid within same function, dashed across functions.
         Cross-function dashed edges colored red if the call graph can't verify. *)
      let inter_edges = let rec go = function
        | [] | [_] -> []
        | (_, last_id, fid_a) :: ((first_id, _, fid_b) :: _ as rest) ->
            let same = same_func fid_a fid_b in
            let style = if same then Solid else Dashed in
            let verified = same || funcs_connected ~call_graph fid_a fid_b in
            { id = Printf.sprintf "inter_%s_%s" last_id first_id;
              source = last_id; target = first_id;
              style; label = ""; verified; } :: go rest
        in go group_boundaries
      in
      let first_id = match group_boundaries with
        | (fid, _, _) :: _ -> Some fid | [] -> None in
      let last_id = match List.rev group_boundaries with
        | (_, lid, _) :: _ -> Some lid | [] -> None in
      { vr_nodes = all_nodes;
        vr_boxes = all_boxes;
        vr_edges = all_edges @ inter_edges;
        vr_first_id = first_id;
        vr_last_id = last_id;
        vr_counter = counter; }

(*****************************************************************************)
(* Recursive trace walker *)
(*****************************************************************************)

type trace_result = {
  entry_node_id : string;
  entry_node : cyto_node;
  inner_nodes : cyto_node list;
  inner_boxes : func_box list;
  inner_edges : cyto_edge list;
  tr_counter : int;
}

let rec process_call_trace
    ~func_index ~file_cache ~call_graph
    ~prefix ~node_type
    ~caller_box_id ~caller_func_id
    (trace : call_trace)
    (counter : int)
  : trace_result =
  match trace with
  | CliLoc (loc, content) ->
      let node_id = Printf.sprintf "%s_n%d" prefix counter in
      let node = make_node ~file_cache node_id node_type loc content in
      { entry_node_id = node_id;
        entry_node = node;
        inner_nodes = [];
        inner_boxes = [];
        inner_edges = [];
        tr_counter = counter + 1; }

  | CliCall ((call_site_loc, call_site_content), inner_vars, inner_trace) ->
      (* Step 1: Determine callee function and whether to merge into caller *)
      let callee_top_loc = top_location_of inner_trace in
      let callee_fid = FuncIndex.find func_index callee_top_loc.path callee_top_loc.start_line in
      let merge_into_caller = same_func callee_fid caller_func_id in

      let box_id, new_box, inner_caller_box, inner_caller_fid, counter =
        if merge_into_caller then
          (* Same function as caller — merge into caller's box *)
          (caller_box_id, None, caller_box_id, caller_func_id, counter)
        else
          (* Different function — box for the callee (canonical per function) *)
          let bid = Printf.sprintf "%s_func%d" prefix counter in
          let counter = counter + 1 in
          let box = make_func_box ~func_index ~file_cache bid callee_top_loc in
          (box.id, Some box, box.id, callee_fid, counter)
      in

      (* Step 2: Recurse into inner trace with appropriate caller context *)
      let inner_result = process_call_trace
          ~func_index ~file_cache ~call_graph
          ~prefix:(prefix ^ "i") ~node_type
          ~caller_box_id:inner_caller_box ~caller_func_id:inner_caller_fid
          inner_trace counter
      in
      let counter = inner_result.tr_counter in

      (* Step 3: Parent entry node to box *)
      let parented_entry = with_parent box_id inner_result.entry_node in

      (* Step 4: Process intermediate vars with box as parent *)
      let vr = process_vars ~func_index ~file_cache ~call_graph
          ~prefix:(prefix ^ "v")
          ~parent_box_id:box_id
          ~parent_func_id:(if merge_into_caller then caller_func_id else callee_fid)
          inner_vars counter in
      let counter = vr.vr_counter in

      (* Step 5: Chain entry → vars within the box.
         Source: entry → first_var ... last_var → boundary
         Sink:   boundary → first_var ... last_var → entry *)
      let connecting_edges, boundary_node_id = match node_type with
        | Source ->
            let edges = match vr.vr_first_id with
              | Some fid ->
                  [{ id = Printf.sprintf "e_%s_%s" inner_result.entry_node_id fid;
                     source = inner_result.entry_node_id; target = fid;
                     style = Solid; label = ""; verified = true; }]
              | None -> []
            in
            let boundary = match vr.vr_last_id with
              | Some lid -> lid
              | None -> inner_result.entry_node_id
            in
            (edges, boundary)
        | Sink ->
            let edges = match vr.vr_last_id with
              | Some lid ->
                  [{ id = Printf.sprintf "e_%s_%s" lid inner_result.entry_node_id;
                     source = lid; target = inner_result.entry_node_id;
                     style = Solid; label = ""; verified = true; }]
              | None -> []
            in
            let boundary = match vr.vr_first_id with
              | Some fid -> fid
              | None -> inner_result.entry_node_id
            in
            (edges, boundary)
        | Intermediate | CallSite ->
            let edges = match vr.vr_first_id with
              | Some fid ->
                  [{ id = Printf.sprintf "e_%s_%s" inner_result.entry_node_id fid;
                     source = inner_result.entry_node_id; target = fid;
                     style = Solid; label = ""; verified = true; }]
              | None -> []
            in
            let boundary = match vr.vr_last_id with
              | Some lid -> lid
              | None -> inner_result.entry_node_id
            in
            (edges, boundary)
      in

      (* Step 6: Create call-site node, parented to the caller's box *)
      let call_site_id = Printf.sprintf "%s_n%d" prefix counter in
      let counter = counter + 1 in
      let call_site_node =
        { (make_node ~file_cache call_site_id CallSite call_site_loc call_site_content)
          with parent = Some caller_box_id } in

      (* Step 7: Edge from boundary to call-site.
         Merged (same func): solid intra-function edge.
         Not merged: dashed inter-function edge, call-graph-verified. *)
      let src_id, tgt_id = match node_type with
        | Source -> boundary_node_id, call_site_id
        | Sink -> call_site_id, boundary_node_id
        | Intermediate | CallSite -> boundary_node_id, call_site_id
      in
      let edge_style, edge_verified, edge_label =
        if merge_into_caller then
          (Solid, true, "")
        else
          let verification = verify_call_edge ~call_graph ~func_index
              ~call_site:call_site_loc ~callee_loc:callee_top_loc in
          let label = match verification with
            | `Verified -> "" | `Unverified -> "unverified" | `NoGraph -> "" in
          let verified = match verification with
            | `Verified -> true | `Unverified -> false | `NoGraph -> true in
          (Dashed, verified, label)
      in
      let transition_edge = {
        id = Printf.sprintf "inter_%s_%s" src_id tgt_id;
        source = src_id; target = tgt_id;
        style = edge_style; label = edge_label; verified = edge_verified;
      } in

      (* Collect *)
      let new_boxes = match new_box with Some b -> [b] | None -> [] in
      { entry_node_id = call_site_id;
        entry_node = call_site_node;
        inner_nodes =
          parented_entry
          :: vr.vr_nodes
          @ inner_result.inner_nodes;
        inner_boxes =
          new_boxes
          @ vr.vr_boxes
          @ inner_result.inner_boxes;
        inner_edges =
          transition_edge
          :: connecting_edges
          @ vr.vr_edges
          @ inner_result.inner_edges;
        tr_counter = counter; }

(*****************************************************************************)
(* Top-level: build graph from a dataflow_trace *)
(*****************************************************************************)

let build_trace_graph ~func_index ~file_cache ~call_graph (trace : dataflow_trace) : graph_acc =
  let counter = ref 0 in

  (* Step 1: Deduce reporting function *)
  let reporting_loc = match trace.taint_source, trace.taint_sink with
    | Some src, _ -> Some (top_location_of src)
    | _, Some snk -> Some (top_location_of snk)
    | None, None -> None
  in
  let report_box = match reporting_loc with
    | Some loc -> make_func_box ~func_index ~file_cache "report_func" loc
    | None -> { id = "report_func"; label = "unknown"; file = ""; line = 0; code_context = []; resolved = false }
  in
  let report_box_id = report_box.id in
  let reporting_fid = match reporting_loc with
    | Some loc -> FuncIndex.find func_index loc.path loc.start_line
    | None -> None in

  let process_side prefix node_type trace_opt =
    match trace_opt with
    | None -> (None, [], [], [])
    | Some tr ->
        let result = process_call_trace
            ~func_index ~file_cache ~call_graph
            ~prefix ~node_type
            ~caller_box_id:report_box_id ~caller_func_id:reporting_fid
            tr !counter
        in
        counter := result.tr_counter;
        (Some result, result.inner_nodes, result.inner_boxes, result.inner_edges)
  in

  (* Step 2: Process source trace *)
  let src_result, src_nodes, src_boxes, src_edges =
    process_side "src" Source trace.taint_source in

  (* Step 3: Process top-level intermediate vars — grouped by function *)
  let ivars = trace.intermediate_vars in
  let vr = process_vars ~func_index ~file_cache ~call_graph
      ~prefix:"ri"
      ~parent_box_id:report_box_id ~parent_func_id:reporting_fid
      ivars !counter in
  counter := vr.vr_counter;

  (* Step 4: Process sink trace *)
  let snk_result, snk_nodes, snk_boxes, snk_edges =
    process_side "snk" Sink trace.taint_sink in

  (* Step 5: Parent the entry nodes to the reporting function box *)
  let src_entry_node = match src_result with
    | Some r -> [with_parent report_box_id r.entry_node]
    | None -> [] in
  let snk_entry_node = match snk_result with
    | Some r -> [with_parent report_box_id r.entry_node]
    | None -> [] in

  (* Step 6: Chain: source_entry -> var_groups -> sink_entry
     Use dashed edges when crossing function boundaries *)
  let src_entry_id = match src_result with Some r -> Some r.entry_node_id | None -> None in
  let snk_entry_id = match snk_result with Some r -> Some r.entry_node_id | None -> None in

  let chain_edges =
    (* source entry -> first var group (all within reporting func) *)
    (match src_entry_id, vr.vr_first_id with
     | Some s, Some f ->
         [{ id = Printf.sprintf "e_%s_%s" s f;
            source = s; target = f; style = Solid; label = ""; verified = true; }]
     | _ -> [])
    @
    (* last var group -> sink entry (all within reporting func) *)
    (match vr.vr_last_id, snk_entry_id with
     | Some l, Some s ->
         [{ id = Printf.sprintf "e_%s_%s" l s;
            source = l; target = s; style = Solid; label = ""; verified = true; }]
     | _ -> [])
    @
    (* direct source -> sink if no vars *)
    (match src_entry_id, vr.vr_first_id, vr.vr_last_id, snk_entry_id with
     | Some s, None, None, Some k ->
         [{ id = Printf.sprintf "e_%s_%s" s k;
            source = s; target = k; style = Solid; label = ""; verified = true; }]
     | _ -> [])
  in

  (* Assemble *)
  { nodes = src_entry_node @ src_nodes
            @ vr.vr_nodes
            @ snk_entry_node @ snk_nodes;
    func_boxes = report_box :: src_boxes @ vr.vr_boxes @ snk_boxes;
    edges = chain_edges @ src_edges @ vr.vr_edges @ snk_edges;
    counter = !counter; }

(*****************************************************************************)
(* Serialization to Cytoscape JSON *)
(*****************************************************************************)

let code_context_to_json ctx =
  `List (List.map (fun (ln, txt) ->
    `Assoc [("line", `Int ln); ("text", `String txt)]
  ) ctx)

let acc_to_cytoscape_json (acc : graph_acc) : Yojson.Basic.t =
  (* Same function may be referenced from several trace arms; keep one box per id. *)
  let unique_boxes =
    List.fold_left (fun (seen, acc') (fb : func_box) ->
      if List.exists (fun s -> String.equal s fb.id) seen then (seen, acc')
      else (fb.id :: seen, fb :: acc'))
      ([], []) acc.func_boxes
    |> snd |> List.rev
  in
  let func_box_nodes = List.map (fun (fb : func_box) ->
    `Assoc [("data", `Assoc [
      ("id", `String fb.id);
      ("label", `String fb.label);
      ("isParent", `Bool true);
      ("resolved", `Bool fb.resolved);
      ("file", `String fb.file);
      ("line", `Int fb.line);
      ("code_context", code_context_to_json fb.code_context);
    ])]
  ) unique_boxes in
  let child_nodes = List.map (fun n ->
    let typ_str = string_of_node_type n.node_type in
    `Assoc [("data", `Assoc (
      [("id", `String n.id);
       ("label", `String n.label);
       ("type", `String typ_str);
       ("color", `String (color_of_type typ_str));
       ("file", `String n.file);
       ("line", `Int n.line);
       ("col", `Int n.col);
       ("taint_len", `Int (String.length n.label));
       ("code_context", code_context_to_json n.code_context);
      ] @ (match n.parent with
           | Some p -> [("parent", `String p)]
           | None -> [])
    ))]
  ) acc.nodes in
  let edges = List.map (fun (e : cyto_edge) ->
    let color = match e.style, e.verified with
      | Dashed, false -> "#e74c3c"  (* red = unverified cross-function *)
      | Solid, _ -> "#3498db"
      | Dashed, true -> "#9b59b6"
    in
    let line_style = match e.style with Solid -> "solid" | Dashed -> "dashed" in
    `Assoc [("data", `Assoc (
      [("id", `String e.id);
       ("source", `String e.source);
       ("target", `String e.target);
       ("color", `String color);
       ("lineStyle", `String line_style);
      ] @ (if String.length e.label > 0 then [("label", `String e.label)] else [])
    ))]
  ) acc.edges in
  `List (func_box_nodes @ child_nodes @ edges)

(*****************************************************************************)
(* Public API *)
(*****************************************************************************)

let process_dataflow_trace ~func_index ~file_cache ~call_graph
    (trace_json : Yojson.Basic.t option) : Yojson.Basic.t =
  match trace_json with
  | None -> `List []
  | Some json ->
      match parse_dataflow_trace json with
      | None -> `List []
      | Some trace ->
          let acc = build_trace_graph ~func_index ~file_cache ~call_graph trace in
          acc_to_cytoscape_json acc
