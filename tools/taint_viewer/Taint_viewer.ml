(* Taint trace visualization server *)

let process_findings ~call_graph json_str =
  (* Build function index once for all findings *)
  let func_index = match call_graph with
    | Some g ->
        Printf.printf "Building function index...\n%!";
        Taint_graph.FuncIndex.build g
    | None -> Hashtbl.create 0
  in
  (* Shared file cache *)
  let file_cache = Taint_graph.FileCache.create () in
  let json = Yojson.Basic.from_string json_str in
  match json with
  | `Assoc fields ->
      (match List.assoc_opt "results" fields with
       | Some (`List results) ->
           let total = List.length results in
           Printf.printf "Processing %d findings...\n%!" total;
           let processed = ref 0 in
           List.filter_map (fun result ->
             incr processed;
             if !processed mod 100 = 0 then
               Printf.printf "  %d/%d findings\n%!" !processed total;
             match result with
             | `Assoc rfields ->
                 let extra = List.assoc_opt "extra" rfields in
                 let dataflow_trace = match extra with
                   | Some (`Assoc efields) -> List.assoc_opt "dataflow_trace" efields
                   | _ -> None
                 in
                 let elements = Taint_graph.process_dataflow_trace ~func_index ~file_cache ~call_graph dataflow_trace in
                 let rule_id = match List.assoc_opt "check_id" rfields with
                   | Some (`String s) -> s | _ -> "unknown" in
                 let file = match List.assoc_opt "path" rfields with
                   | Some (`String s) -> s | _ -> "unknown" in
                 Some (`Assoc [
                   ("rule_id", `String rule_id);
                   ("file", `String file);
                   ("elements", elements);
                 ])
             | _ -> None
           ) results
       | _ -> [])
  | _ -> []

let serve_html ~json_str ~call_graph =
  let findings = process_findings ~call_graph json_str in
  let findings_json = Yojson.Basic.to_string (`List findings) in
  Static_files.viewer_html
  |> Str.global_replace (Str.regexp_string "{{FINDINGS_DATA}}") findings_json

let callback ~json_str ~call_graph _conn req _body =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let uri = Request.uri req in
  match Uri.path uri with
  | "/" | "/index.html" ->
      let body = serve_html ~json_str ~call_graph in
      let headers = Header.init_with "Content-Type" "text/html; charset=utf-8" in
      Server.respond_string ~status:`OK ~headers ~body ()
  | "/viewer.css" ->
      let headers = Header.init_with "Content-Type" "text/css" in
      Server.respond_string ~status:`OK ~headers ~body:Static_files.viewer_css ()
  | "/viewer.js" ->
      let headers = Header.init_with "Content-Type" "application/javascript" in
      Server.respond_string ~status:`OK ~headers ~body:Static_files.viewer_js ()
  | _ ->
      Server.respond_not_found ()

let get_findings_json ~json_str ~call_graph =
  let findings = process_findings ~call_graph json_str in
  Yojson.Basic.pretty_to_string (`List findings)

let start_server ~json_str ~call_graph ~port =
  let open Cohttp_lwt_unix in
  let callback = callback ~json_str ~call_graph in
  let server = Server.make ~callback () in
  Server.create ~mode:(`TCP (`Port port)) server

(* Serve with pre-computed findings JSON *)
let serve_html_with_findings ~findings_json =
  Static_files.viewer_html
  |> Str.global_replace (Str.regexp_string "{{FINDINGS_DATA}}") findings_json

let callback_with_findings ~findings_json _conn req _body =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let uri = Request.uri req in
  match Uri.path uri with
  | "/" | "/index.html" ->
      let body = serve_html_with_findings ~findings_json in
      let headers = Header.init_with "Content-Type" "text/html; charset=utf-8" in
      Server.respond_string ~status:`OK ~headers ~body ()
  | "/viewer.css" ->
      let headers = Header.init_with "Content-Type" "text/css" in
      Server.respond_string ~status:`OK ~headers ~body:Static_files.viewer_css ()
  | "/viewer.js" ->
      let headers = Header.init_with "Content-Type" "application/javascript" in
      Server.respond_string ~status:`OK ~headers ~body:Static_files.viewer_js ()
  | _ ->
      Server.respond_not_found ()

let start_server_with_findings ~findings_json ~port =
  let open Cohttp_lwt_unix in
  let callback = callback_with_findings ~findings_json in
  let server = Server.make ~callback () in
  Server.create ~mode:(`TCP (`Port port)) server
