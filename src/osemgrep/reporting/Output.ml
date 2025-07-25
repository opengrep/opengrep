open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' output.

   Partially translated from output.py

   We're using CapConsole.print() below, not Logs.app(), because we want to
   output findings on stdout (Logs.app uses stderr). That also mean semgrep will
   display findings even with --quiet.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* This is part of Scan_CLI.conf
 *
 * See also Out.format_context for the runtime params (e.g., is_logged_in).
 *)
type conf = {
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  (* alt: maybe we should define an Output_option.t, or add a record to
   * Output_format.Text as those fields are only valid for Text output *)
  max_chars_per_line : int;
  max_lines_per_finding : int;
  force_color : bool;
  (* For Text and SARIF *)
  show_dataflow_traces : bool;
  (* TODO: why strict part of an output conf? *)
  strict : bool;
  (* a.k.a. dryrun in Scan_CLI.conf *)
  fixed_lines : bool;
  (* true when using --verbose or --debug in Scan_CLI.ml *)
  skipped_files : bool;
  (* alt: in CLI_common.conf *)
  max_log_list_entries : int;
}
[@@deriving show]

let default : conf =
  {
    output_format = Output_format.Text;
    force_color = false;
    max_chars_per_line = 160;
    max_lines_per_finding = 10;
    show_dataflow_traces = false;
    strict = false;
    fixed_lines = false;
    skipped_files = false;
    max_log_list_entries = 100;
  }

(* used with max_log_list_entries *)
let too_much_data =
  "<SKIPPED DATA (too many entries; adjust with --max-log-list-entries)>"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_severity (severity : Out.match_severity) : string =
  Out.string_of_match_severity severity
  |> JSON.remove_enclosing_quotes_of_jstring

let start_time_from_profiler_opt (profiler : Profiler.t) : Timedesc.Timestamp.t option =
  match Hashtbl.find_opt profiler "total_time" with
  | Some (Profiler.Start t) -> Some (Timedesc.Timestamp.of_float_s t)
  | _ -> None

(*****************************************************************************)
(* Format dispatcher *)
(*****************************************************************************)

(* called also from RPC_return.ml *)
let format
    (* XXX: This is only passed in --experimental mode. *)
    ?(profiler : Profiler.t option)
    (kind : Output_format.t)
    (cli_output : Out.cli_output) : string list =
  match kind with
  | Text
  | Sarif
  | Files_with_matches
  | Incremental ->
      failwith (spf "format not supported here: %s" (Output_format.show kind))
  | Json ->
      [ Out.string_of_cli_output cli_output ]
  | Junit_xml -> [ Junit_xml_output.junit_xml_output cli_output ]
  | Gitlab_sast ->
      let start_time = Option.map start_time_from_profiler_opt profiler |> Option.join in
      let gitlab_sast_json = Gitlab_output.sast_output ?start_time cli_output.results in
      [ Yojson.Basic.to_string gitlab_sast_json ]
  | Gitlab_secrets ->
      let start_time = Option.map start_time_from_profiler_opt profiler |> Option.join in
      let gitlab_secrets_json =
        Gitlab_output.secrets_output ?start_time cli_output.results
      in
      [ Yojson.Basic.to_string gitlab_secrets_json ]
  | Vim ->
      cli_output.results
      |> List_.map (fun (m : Out.cli_match) ->
             match m with
             | { check_id; path; start; extra = { message; severity; _ }; _ } ->
                 let parts =
                   [
                     !!path;
                     spf "%d" start.line;
                     spf "%d" start.col;
                     (* TOPORT? restrict to just I|E|W ? *)
                     spf "%c" (string_of_severity severity).[0];
                     Rule_ID.to_string check_id;
                     message;
                   ]
                 in
                 String.concat ":" parts)
  | Emacs ->
      (* TOPORT? sorted(rule_matches, key=lambda r: (r.path, r.rule_id)) *)
      cli_output.results
      |> List_.map (fun (m : Out.cli_match) ->
             match m with
             | {
              check_id;
              path;
              start;
              end_;
              extra = { message; severity; _ };
              _;
             } ->
                 let severity =
                   String.lowercase_ascii (string_of_severity severity)
                 in
                 let severity_and_ruleid =
                   if check_id =*= Rule_ID.dash_e then severity
                   else
                     match Rule_ID.last_elt_opt check_id with
                     | None -> severity
                     | Some x -> spf "%s(%s)" severity x
                 in
                 let line =
                   (* ugly: redoing the work done in cli_match_of_core_match.
                    * we can't use m.extra.lines because this field actually
                    * contains a string, not a string list.
                    *)
                   match
                     Semgrep_output_utils.lines_of_file_at_range_exn
                       (start, end_) path
                   with
                   | [] -> ""
                   | x :: _ -> x (* TOPORT rstrip? *)
                 in
                 let parts =
                   [
                     !!path;
                     spf "%d" start.line;
                     spf "%d" start.col;
                     (* TOPORT? restrict to just I|E|W ? *)
                     severity_and_ruleid;
                     line;
                     message;
                   ]
                 in
                 String.concat ":" parts)

let dispatch_output_format
    (caps : < Cap.stdout >)
    (profiler : Profiler.t)
    (conf : conf)
    (cli_output : Out.cli_output)
    (hrules : Rule.hrules) : unit =
  let print = CapConsole.print caps#stdout in
  match conf.output_format with
  (* matches have already been displayed in a file_match_results_hook *)
  | Incremental -> ()
  | Vim -> format Vim cli_output |> List.iter print
  | Emacs -> format Emacs cli_output |> List.iter print
  | Junit_xml -> format Junit_xml cli_output |> List.iter print
  | Gitlab_sast -> format ~profiler Gitlab_sast cli_output |> List.iter print
  | Gitlab_secrets -> format ~profiler Gitlab_secrets cli_output |> List.iter print
  | Json -> format Json cli_output |> List.iter print
  | Text ->
      (* TODO: we should switch to Fmt_.with_buffer_to_string +
       * some CapConsole.print_no_nl, but then is_atty fail on
       * a string buffer and we lose the colors
       *)
      Matches_report.pp_cli_output ~max_chars_per_line:conf.max_chars_per_line
        ~max_lines_per_finding:conf.max_lines_per_finding
          (* nosemgrep: forbid-console *)
        ~color_output:conf.force_color Format.std_formatter cli_output
  | Sarif ->
      let engine_label =
        match cli_output.engine_requested with
        | Some `OSS
        | None -> "OSS"
        (* FIXME: Remove. *)
        | Some `PRO -> "PRO"
      in
      let hide_nudge = true
      in
      let sarif_json =
        Sarif_output.sarif_output hrules cli_output hide_nudge engine_label
          conf.show_dataflow_traces
      in
      print (Sarif.Sarif_v_2_1_0_j.string_of_sarif_json_schema sarif_json)
  | Files_with_matches ->
      cli_output.results
      |> List_.map (fun (x : Out.cli_match) -> !!(x.path))
      |> Set_.of_list |> Set_.elements |> List_.sort |> String.concat "\n"
      |> print

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* This function takes a core runner output and makes it suitable for the user,
 * by filtering out nosem, setting messages, adding fingerprinting etc.
 * TODO? remove this intermediate?
 *)
let preprocess_result ~fixed_lines (res : Core_runner.result) : Out.cli_output =
  let cli_output : Out.cli_output =
    Cli_json_output.cli_output_of_runner_result ~fixed_lines res.core res.hrules
      res.scanned
  in
  cli_output |> fun results ->
  {
    results with
    (* TODO? why not do that in cli_output_of_core_results? *)
    results = Cli_json_output.index_match_based_ids results.results;
  }

(* python: mix of output.OutputSettings(), output.OutputHandler(), and
 * output.output() all at once.
 *)
let output_result (caps : < Cap.stdout >) (conf : conf)
    (profiler : Profiler.t)
    (res : Core_runner.result) : Out.cli_output =
  (* In theory, we should build the JSON CLI output only for the
   * Json conf.output_format, but cli_output contains lots of data-structures
   * that are useful for the other formats (e.g., Vim, Emacs), so we build
   * it here.
   *)
  let (cli_output : Out.cli_output) =
    Profiler.record profiler ~name:"ignores_times" (fun () ->
        preprocess_result ~fixed_lines:conf.fixed_lines res)
  in
  (* TODO: adjust conf.time *)
  let cli_output =
    if not conf.skipped_files then
      {
        cli_output with
        paths = { scanned = cli_output.paths.scanned; skipped = None };
      }
    else cli_output
  in
  (* the actual output on stdout *)
  dispatch_output_format caps profiler conf cli_output res.hrules;
  (* we return cli_output as the caller might use it *)
  cli_output
[@@profiling]
