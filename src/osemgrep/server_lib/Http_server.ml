(* HTTP facade for opengrep server mode (read-only, PR 1).
 *
 * Routes:
 *   GET  /health
 *   GET  /v1/repos
 *   GET  /v1/scans
 *   POST /v1/scans                     (no body: rescan all repos)
 *   GET  /v1/scans/:id
 *   GET  /v1/scans/:id/results
 *   GET  /v1/repos/:id/call-graph?file=REL[&format=graphology|dot]
 *        [&lang=L][&annotate=findings][&scan=latest|SCAN_ID]
 *
 * [handle] is independent of sockets so tests can drive it directly with
 * synthetic requests. Every non-2xx response body is
 * {"error":{"code":...,"message":...}}.
 *)

module Server = Cohttp_lwt_unix.Server
open Server_types

(*****************************************************************************)
(* Response helpers *)
(*****************************************************************************)

let json_headers = Cohttp.Header.init_with "content-type" "application/json"

let respond_json ?(status = `OK) (json : Yojson.Safe.t) =
  Server.respond_string ~status ~headers:json_headers
    ~body:(Yojson.Safe.to_string json)
    ()

let respond_error ~status (e : error) =
  respond_json ~status (json_of_error e)

let respond_not_found () =
  respond_error ~status:`Not_found (error ~code:"not_found" "no such route")

(* Map a core error to a sensible HTTP status from its code *)
let status_of_error (e : error) =
  match e.code with
  | "not_found"
  | "unknown_repo"
  | "unknown_scan" ->
      `Not_found
  | "scan_running" -> `Conflict
  | _ -> `Bad_request

(*****************************************************************************)
(* Route handlers *)
(*****************************************************************************)

let handle_health () =
  respond_json
    (`Assoc
      [ ("status", `String "ok"); ("version", `String Version.version) ])

let handle_list_repos core =
  let repos =
    Server_core.list_repos core
    |> List_.map (fun (r : Server_core.repo_entry) ->
           `Assoc
             [
               ("id", `String r.repo_id);
               ("root", `String (Fpath.to_string r.root));
             ])
  in
  respond_json (`Assoc [ ("repos", `List repos) ])

let handle_list_scans core =
  let scans = Server_core.list_scans core |> List_.map json_of_scan_record in
  respond_json (`Assoc [ ("scans", `List scans) ])

let handle_rescan core =
  let ids = Server_core.start_scan_all core in
  respond_json ~status:`Accepted
    (`Assoc [ ("scan_ids", `List (List_.map (fun id -> `String id) ids)) ])

let handle_get_scan core scan_id =
  match Server_core.get_scan core scan_id with
  | None ->
      respond_error ~status:`Not_found
        (error ~code:"unknown_scan" "unknown scan: %s" scan_id)
  | Some record -> respond_json (json_of_scan_record record)

let handle_get_results core scan_id =
  match Server_core.get_scan_entry core scan_id with
  | None ->
      respond_error ~status:`Not_found
        (error ~code:"unknown_scan" "unknown scan: %s" scan_id)
  | Some { record = { status = Running; _ }; _ } ->
      respond_error ~status:`Conflict
        (error ~code:"scan_running" "scan %s is still running" scan_id)
  | Some { record = { status = Failed msg; _ }; _ } ->
      respond_error ~status:`Conflict
        (error ~code:"scan_failed" "scan %s failed: %s" scan_id msg)
  | Some { results = None; _ } ->
      respond_error ~status:`Internal_server_error
        (error ~code:"missing_results" "scan %s has no recorded results"
           scan_id)
  | Some { results = Some results; _ } ->
      respond_json (json_of_scan_results results)

let handle_call_graph core graphs repo_id uri =
  match Server_core.find_repo core repo_id with
  | None ->
      respond_error ~status:`Not_found
        (error ~code:"unknown_repo" "unknown repo: %s" repo_id)
  | Some repo -> (
      match Uri.get_query_param uri "file" with
      | None ->
          respond_error ~status:`Bad_request
            (error ~code:"missing_param" "missing required query param: file")
      | Some rel_path -> (
          let lang = Uri.get_query_param uri "lang" in
          let format =
            Uri.get_query_param uri "format"
            |> Option.value ~default:"graphology"
          in
          let%lwt graph_res =
            Server_graphs.get_graph graphs ~repo_id:repo.repo_id
              ~root:repo.root ?lang ~rel_path ()
          in
          match graph_res with
          | Error e -> respond_error ~status:(status_of_error e) e
          | Ok graph -> (
              match format with
              | "dot" ->
                  Server.respond_string ~status:`OK
                    ~headers:
                      (Cohttp.Header.init_with "content-type"
                         "text/vnd.graphviz")
                    ~body:(Server_graphs.to_dot graph)
                    ()
              | "graphology" -> (
                  let json = Server_graphs.to_graphology graph in
                  let annotate =
                    Uri.get_query_param uri "annotate" = Some "findings"
                  in
                  if not annotate then
                    Server.respond_string ~status:`OK ~headers:json_headers
                      ~body:(Yojson.Basic.to_string json)
                      ()
                  else
                    (* Overlay findings from the requested scan (default:
                       latest per-file results) onto the graph nodes *)
                    let scan_id =
                      match Uri.get_query_param uri "scan" with
                      | None
                      | Some "latest" ->
                          None
                      | Some id -> Some id
                    in
                    match
                      ( Server_graphs.resolve_file ~root:repo.root ~rel_path,
                        Server_graphs.lang_for ?lang
                          (Fpath.append repo.root (Fpath.v rel_path)) )
                    with
                    | Error e, _
                    | _, Error e ->
                        respond_error ~status:(status_of_error e) e
                    | Ok file, Ok lang ->
                        let matches =
                          Server_core.matches_for_file core repo ?scan_id file
                        in
                        let%lwt json =
                          Server_graphs.annotate_with_findings ~lang ~file
                            ~matches json
                        in
                        Server.respond_string ~status:`OK ~headers:json_headers
                          ~body:(Yojson.Basic.to_string json)
                          ())
              | other ->
                  respond_error ~status:`Bad_request
                    (error ~code:"unknown_format" "unknown format: %s" other))))

(*****************************************************************************)
(* Dispatch *)
(*****************************************************************************)

let handle ~(core : Server_core.t) ~(graphs : Server_graphs.t)
    (req : Cohttp.Request.t) (_body : Cohttp_lwt.Body.t) :
    (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
  let uri = Cohttp.Request.uri req in
  let segments =
    Uri.path uri |> String.split_on_char '/'
    |> List.filter (fun s -> s <> "")
  in
  let meth = Cohttp.Request.meth req in
  Lwt.catch
    (fun () ->
      match (meth, segments) with
      | `GET, [ "health" ] -> handle_health ()
      | `GET, [ "v1"; "repos" ] -> handle_list_repos core
      | `GET, [ "v1"; "scans" ] -> handle_list_scans core
      | `POST, [ "v1"; "scans" ] -> handle_rescan core
      | `GET, [ "v1"; "scans"; scan_id ] -> handle_get_scan core scan_id
      | `GET, [ "v1"; "scans"; scan_id; "results" ] ->
          handle_get_results core scan_id
      | `GET, [ "v1"; "repos"; repo_id; "call-graph" ] ->
          handle_call_graph core graphs repo_id uri
      | _ -> respond_not_found ())
    (fun exn ->
      Logs.err (fun m ->
          m "server: exception handling %s: %s" (Uri.path uri)
            (Printexc.to_string exn));
      respond_error ~status:`Internal_server_error
        (error ~code:"internal_error" "%s" (Printexc.to_string exn)))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let serve ~(host : string) ~(port : int) ~(core : Server_core.t)
    ~(graphs : Server_graphs.t) () : unit Lwt.t =
  let callback _conn req body = handle ~core ~graphs req body in
  let%lwt ctx = Conduit_lwt_unix.init ~src:host () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  Logs.app (fun m -> m "opengrep server listening on http://%s:%d" host port);
  Server.create ~ctx ~mode:(`TCP (`Port port)) (Server.make ~callback ())
