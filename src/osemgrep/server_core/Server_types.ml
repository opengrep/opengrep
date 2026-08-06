(* Types shared by the opengrep server-mode core and its facades.
 *
 * Ids are plain strings on the wire; findings reuse the ATD cli_match type
 * so the JSON payload is identical to 'opengrep scan --json' findings.
 *)

module Out = Semgrep_output_v1_t
module OutJ = Semgrep_output_v1_j

type repo_id = string
type scan_id = string
type scan_status = Running | Completed | Failed of string

type scan_record = {
  scan_id : scan_id;
  repo : repo_id;
  status : scan_status;
  started_at : float;
  finished_at : float option;
}

type scan_results = { matches : Out.cli_match list; scanned : Fpath.t list }
type error = { code : string; message : string }

let error ~code fmt = Printf.ksprintf (fun message -> { code; message }) fmt

let string_of_status = function
  | Running -> "running"
  | Completed -> "completed"
  | Failed _ -> "failed"

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

let json_of_scan_record (r : scan_record) : Yojson.Safe.t =
  let base =
    [
      ("id", `String r.scan_id);
      ("repo", `String r.repo);
      ("status", `String (string_of_status r.status));
      ("started_at", `Float r.started_at);
    ]
  in
  let finished =
    match r.finished_at with
    | Some t -> [ ("finished_at", `Float t) ]
    | None -> []
  in
  let err =
    match r.status with
    | Failed msg -> [ ("error", `String msg) ]
    | _ -> []
  in
  `Assoc (base @ finished @ err)

let json_of_error (e : error) : Yojson.Safe.t =
  `Assoc
    [
      ( "error",
        `Assoc [ ("code", `String e.code); ("message", `String e.message) ] );
    ]

let json_of_scan_results (r : scan_results) : Yojson.Safe.t =
  let matches =
    r.matches
    |> List_.map (fun m -> OutJ.string_of_cli_match m |> Yojson.Safe.from_string)
  in
  let scanned =
    r.scanned |> List_.map (fun f -> `String (Fpath.to_string f))
  in
  `Assoc [ ("matches", `List matches); ("scanned", `List scanned) ]
