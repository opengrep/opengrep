(* On-demand per-file intrafile call graphs for server mode.
 *
 * Graphs are built the same way as 'opengrep show dump-intrafile-graph'
 * (parse the file, Graph_from_AST.build_call_graph) and cached by
 * (repo, relative path) with (mtime, size) freshness. Concurrent builds of
 * the same file are deduplicated through the [building] promise table.
 *
 * The cache key deliberately identifies graphs as per-file ("intrafile");
 * a future repo-wide/interfile graph is an additive extension.
 *)

module Out = Semgrep_output_v1_t
open Server_types

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type key = repo_id * string (* repo-relative path *)
type entry = { mtime : float; size : int; graph : Call_graph.G.t }

type t = {
  entries : (key, entry) Hashtbl.t;
  building : (key, (Call_graph.G.t, error) result Lwt.t) Hashtbl.t;
}

let create () = { entries = Hashtbl.create 16; building = Hashtbl.create 16 }

(*****************************************************************************)
(* Path and language resolution *)
(*****************************************************************************)

let resolve_file ~(root : Fpath.t) ~(rel_path : string) : (Fpath.t, error) result
    =
  match Fpath.of_string rel_path with
  | Error (`Msg msg) ->
      Error (error ~code:"invalid_path" "invalid file path %s: %s" rel_path msg)
  | Ok p ->
      if Fpath.is_abs p then
        Error
          (error ~code:"invalid_path" "file must be repo-relative: %s" rel_path)
      else
        let file = Fpath.normalize Fpath.(root // p) in
        if not (Fpath.is_prefix root file) then
          Error
            (error ~code:"invalid_path" "file escapes the repo root: %s"
               rel_path)
        else if
          (not (Sys.file_exists (Fpath.to_string file)))
          || Sys.is_directory (Fpath.to_string file)
        then Error (error ~code:"not_found" "no such file: %s" rel_path)
        else Ok file

let lang_for ?lang (file : Fpath.t) : (Lang.t, error) result =
  match lang with
  | Some s -> (
      match Lang.of_string_opt s with
      | Some l -> Ok l
      | None -> Error (error ~code:"unknown_language" "unknown language: %s" s))
  | None -> (
      match Lang.langs_of_filename file with
      | [ l ] -> Ok l
      | [] ->
          Error
            (error ~code:"unsupported_file" "no language known for %s"
               (Fpath.to_string file))
      | ls ->
          Error
            (error ~code:"ambiguous_language"
               "ambiguous language for %s (%s); pass an explicit lang parameter"
               (Fpath.to_string file)
               (ls |> List_.map Lang.to_string |> String.concat ", ")))

(*****************************************************************************)
(* Graph building *)
(*****************************************************************************)

let build ~lang (file : Fpath.t) : Call_graph.G.t Lwt.t =
  Lwt_platform.detach
    (fun () ->
      let ast = Parse_target.parse_and_resolve_name_warn_if_partial lang file in
      Graph_from_AST.build_call_graph ~lang ast)
    ()

let get_graph t ~(repo_id : repo_id) ~(root : Fpath.t) ?lang
    ~(rel_path : string) () : (Call_graph.G.t, error) result Lwt.t =
  match resolve_file ~root ~rel_path with
  | Error e -> Lwt.return (Error e)
  | Ok file -> (
      match lang_for ?lang file with
      | Error e -> Lwt.return (Error e)
      | Ok lang -> (
          let key = (repo_id, rel_path) in
          let st = Unix.stat (Fpath.to_string file) in
          let mtime = st.Unix.st_mtime in
          let size = st.Unix.st_size in
          match Hashtbl.find_opt t.entries key with
          | Some e when Float.equal e.mtime mtime && e.size = size ->
              Lwt.return (Ok e.graph)
          | _ -> (
              match Hashtbl.find_opt t.building key with
              | Some promise -> promise
              | None ->
                  let promise =
                    Lwt.catch
                      (fun () ->
                        let%lwt graph = build ~lang file in
                        Hashtbl.replace t.entries key { mtime; size; graph };
                        Hashtbl.remove t.building key;
                        Lwt.return (Ok graph))
                      (fun exn ->
                        Hashtbl.remove t.building key;
                        Lwt.return
                          (Error
                             (error ~code:"graph_build_failed"
                                "failed to build call graph for %s: %s" rel_path
                                (Printexc.to_string exn))))
                  in
                  Hashtbl.add t.building key promise;
                  promise)))

(*****************************************************************************)
(* Serialization *)
(*****************************************************************************)

let to_graphology = Graph_serialization.graph_to_graphology

let to_dot (graph : Call_graph.G.t) : string =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Call_graph.Dot.fprint_graph fmt graph;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(*****************************************************************************)
(* Findings overlay *)
(*****************************************************************************)

let json_of_finding (m : Out.cli_match) : Yojson.Basic.t =
  `Assoc
    [
      ("rule_id", `String (Rule_ID.to_string m.check_id));
      ("message", `String m.extra.message);
      ( "severity",
        `String
          (Semgrep_output_v1_j.string_of_match_severity m.extra.severity
          |> Yojson.Basic.from_string
          |> function
          | `String s -> s
          | _ -> "unknown") );
      ("start_line", `Int m.start.line);
      ("end_line", `Int m.end_.line);
    ]

(* Attach findings to the graphology nodes of their enclosing functions.
   Findings that fall outside any function land in a top-level
   "file_findings" array. *)
let annotate_with_findings ~lang ~(file : Fpath.t)
    ~(matches : Out.cli_match list) (json : Yojson.Basic.t) :
    Yojson.Basic.t Lwt.t =
  match matches with
  | [] -> Lwt.return json
  | _ ->
    let%lwt assignments =
      Lwt_platform.detach
        (fun () ->
          let ast =
            Parse_target.parse_and_resolve_name_warn_if_partial lang file
          in
          matches
          |> List_.map (fun (m : Out.cli_match) ->
                 let range =
                   Range.{ start = m.start.offset; end_ = m.end_.offset }
                 in
                 let fns =
                   Graph_from_AST.find_functions_containing_ranges ~lang ast
                     [ range ]
                 in
                 (m, fns)))
        ()
    in
    let by_node : (string, Yojson.Basic.t list) Hashtbl.t = Hashtbl.create 16 in
    let file_findings = ref [] in
    assignments
    |> List.iter (fun (m, fns) ->
           let finding = json_of_finding m in
           match fns with
           | [] -> file_findings := finding :: !file_findings
           | fns ->
               fns
               |> List.iter (fun fn ->
                      let node_id = Graph_serialization.node_unique_id fn in
                      let prev =
                        Hashtbl.find_opt by_node node_id
                        |> Option.value ~default:[]
                      in
                      Hashtbl.replace by_node node_id (finding :: prev)));
    let annotate_node (node : Yojson.Basic.t) : Yojson.Basic.t =
      match node with
      | `Assoc fields -> (
          match List.assoc_opt "key" fields with
          | Some (`String node_id) -> (
              match Hashtbl.find_opt by_node node_id with
              | None -> node
              | Some findings ->
                  let fields =
                    fields
                    |> List_.map (fun (k, v) ->
                           match (k, v) with
                           | "attributes", `Assoc attrs ->
                               ( k,
                                 `Assoc
                                   (attrs
                                   @ [
                                       ("findings", `List findings);
                                       ( "finding_count",
                                         `Int (List.length findings) );
                                     ]) )
                           | _ -> (k, v))
                  in
                  `Assoc fields)
          | _ -> node)
      | _ -> node
    in
    let json =
      match json with
      | `Assoc fields ->
          let fields =
            fields
            |> List_.map (fun (k, v) ->
                   match (k, v) with
                   | "nodes", `List nodes ->
                       (k, `List (List_.map annotate_node nodes))
                   | _ -> (k, v))
          in
          `Assoc (fields @ [ ("file_findings", `List !file_findings) ])
      | other -> other
    in
    Lwt.return json
