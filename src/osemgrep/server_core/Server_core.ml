(* Protocol-neutral state and verbs for opengrep server mode.
 *
 * This wraps the language server's Session/Scan_helpers machinery: one
 * single-root Session.t per registered repo, so target enumeration,
 * runner configuration and per-file result caching are reused as-is.
 *
 * Rules and repos are fixed at creation time for now (read-only server);
 * mutation verbs will be added in a later PR.
 *
 * Mutation invariant: all Hashtbl/session mutations happen on the Lwt main
 * thread; detached (preemptive) work only returns values. Scans are globally
 * serialized by [scan_lock] because the engine relies on mutable globals
 * (see src/engine/Globals.ml).
 *)

module Out = Semgrep_output_v1_t
open Server_types

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type repo_entry = { repo_id : repo_id; root : Fpath.t; session : Session.t }
type scan_entry = { record : scan_record; results : scan_results option }

type t = {
  repos : (repo_id, repo_entry) Hashtbl.t;
  repo_order : repo_id list;
  rules : Rule.t list;
  scans : (scan_id, scan_entry) Hashtbl.t;
  latest_scan : (repo_id, scan_id) Hashtbl.t;
  scan_lock : Lwt_mutex.t;
  mutable scan_counter : int;
}

let max_scan_history = 50

(*****************************************************************************)
(* Creation *)
(*****************************************************************************)

let mk_repo_id existing root =
  let base =
    match Fpath.basename root with
    | ""
    | "."
    | "/" ->
        "repo"
    | b -> b
  in
  if not (List.mem base existing) then base
  else
    let rec loop i =
      let candidate = Printf.sprintf "%s-%d" base i in
      if List.mem candidate existing then loop (i + 1) else candidate
    in
    loop 2

let mk_session (caps : Session.caps) rules root =
  let session = Session.create caps (Lsp.Types.ServerCapabilities.create ()) in
  (* The server always scans everything, not just git-dirty files *)
  let user_settings =
    { session.Session.user_settings with User_settings.only_git_dirty = false }
  in
  let session = { session with Session.user_settings = user_settings } in
  let session = Session.update_workspace_folders ~added:[ root ] session in
  session.Session.cached_session.Session.initialized <- true;
  session.Session.cached_session.Session.rules <- rules;
  session

let create ~(caps : Session.caps) ~(rules : Rule.t list)
    ~(roots : Fpath.t list) : (t, error) result =
  if rules = [] then Error (error ~code:"no_rules" "no valid rules were loaded")
  else
    let invalid_root =
      roots
      |> List.find_opt (fun root ->
             not
               (Sys.file_exists (Fpath.to_string root)
               && Sys.is_directory (Fpath.to_string root)))
    in
    match invalid_root with
    | Some root ->
        Error
          (error ~code:"invalid_repo_root" "not a directory: %s"
             (Fpath.to_string root))
    | None ->
        let repos = Hashtbl.create 8 in
        let repo_order =
          roots
          |> List.fold_left
               (fun acc root ->
                 let repo_id = mk_repo_id acc root in
                 let session = mk_session caps rules root in
                 Hashtbl.replace repos repo_id { repo_id; root; session };
                 acc @ [ repo_id ])
               []
        in
        Ok
          {
            repos;
            repo_order;
            rules;
            scans = Hashtbl.create 16;
            latest_scan = Hashtbl.create 8;
            scan_lock = Lwt_mutex.create ();
            scan_counter = 0;
          }

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let list_repos t =
  t.repo_order |> List_.filter_map (fun id -> Hashtbl.find_opt t.repos id)

let find_repo t repo_id = Hashtbl.find_opt t.repos repo_id
let get_scan_entry t scan_id = Hashtbl.find_opt t.scans scan_id

let get_scan t scan_id =
  Option.map (fun e -> e.record) (get_scan_entry t scan_id)

let list_scans t =
  Hashtbl.fold (fun _ e acc -> e.record :: acc) t.scans []
  |> List.sort (fun a b -> compare b.started_at a.started_at)

let latest_scan_id t repo_id = Hashtbl.find_opt t.latest_scan repo_id

(* Findings for one file: either from a specific scan or from the per-file
   cache maintained by Session.record_results (i.e. the latest results). *)
let matches_for_file t (repo : repo_entry) ?scan_id (file : Fpath.t) :
    Out.cli_match list =
  match scan_id with
  | None ->
      Session.previous_scan_of_file repo.session file
      |> Option.value ~default:[]
  | Some scan_id -> (
      match get_scan_entry t scan_id with
      | Some { results = Some r; _ } ->
          r.matches
          |> List.filter (fun (m : Out.cli_match) -> Fpath.equal m.path file)
      | _ -> [])

(*****************************************************************************)
(* Scans *)
(*****************************************************************************)

let update_scan t scan_id f =
  match Hashtbl.find_opt t.scans scan_id with
  | Some e -> Hashtbl.replace t.scans scan_id (f e)
  | None -> ()

let mark_failed t scan_id msg =
  update_scan t scan_id (fun e ->
      {
        e with
        record =
          {
            e.record with
            status = Failed msg;
            finished_at = Some (Unix.gettimeofday ());
          };
      })

let mark_completed t scan_id results =
  update_scan t scan_id (fun e ->
      {
        record =
          {
            e.record with
            status = Completed;
            finished_at = Some (Unix.gettimeofday ());
          };
        results = Some results;
      })

(* Bound memory: drop the oldest finished scans beyond the history limit *)
let prune_scans t =
  let finished =
    Hashtbl.fold
      (fun id e acc ->
        match e.record.status with
        | Running -> acc
        | Completed
        | Failed _ ->
            (id, e.record.started_at) :: acc)
      t.scans []
    |> List.sort (fun (_, a) (_, b) -> compare a b)
  in
  let excess = Hashtbl.length t.scans - max_scan_history in
  if excess > 0 then
    finished |> List.to_seq |> Seq.take excess
    |> Seq.iter (fun (id, _) -> Hashtbl.remove t.scans id)

let run_scan t scan_id (repo : repo_entry) =
  Lwt_mutex.with_lock t.scan_lock (fun () ->
      Lwt.catch
        (fun () ->
          Session.cache_workspace_targets repo.session;
          let%lwt matches, scanned =
            Scan_helpers.run_semgrep_detached ~rules:t.rules repo.session
          in
          Session.record_results repo.session matches scanned;
          mark_completed t scan_id { matches; scanned };
          Lwt.return_unit)
        (fun exn ->
          mark_failed t scan_id (Printexc.to_string exn);
          Lwt.return_unit))

(* Must be called from within the Lwt main loop. Returns immediately with the
   scan id; the scan itself runs in the background under the global lock. *)
let start_scan t repo_id : (scan_id, error) result =
  match find_repo t repo_id with
  | None -> Error (error ~code:"unknown_repo" "unknown repo: %s" repo_id)
  | Some repo ->
      t.scan_counter <- t.scan_counter + 1;
      let scan_id = Printf.sprintf "scan-%d" t.scan_counter in
      let record =
        {
          scan_id;
          repo = repo_id;
          status = Running;
          started_at = Unix.gettimeofday ();
          finished_at = None;
        }
      in
      Hashtbl.replace t.scans scan_id { record; results = None };
      Hashtbl.replace t.latest_scan repo_id scan_id;
      prune_scans t;
      Lwt.dont_wait
        (fun () -> run_scan t scan_id repo)
        (fun exn -> mark_failed t scan_id (Printexc.to_string exn));
      Ok scan_id

(* Start a scan of every registered repo (startup scan / bare POST rescan) *)
let start_scan_all t : scan_id list =
  t.repo_order
  |> List_.filter_map (fun repo_id ->
         match start_scan t repo_id with
         | Ok id -> Some id
         | Error e ->
             Logs.err (fun m ->
                 m "failed to start scan of %s: %s" repo_id e.message);
             None)
