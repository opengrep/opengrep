(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests for the SARIF output of the scan subcommand.
 *
 * Where the output goes, rather than what a SARIF document contains, is
 * tested in Test_scan_subcommand_output.ml.
 *)

(*****************************************************************************)
(* Individual tests                                                           *)
(*****************************************************************************)

(* Port of: test_sarif_output. Parameterised over rule + dataflow_traces. *)
let test_basic_sarif
    (caps : Scan_subcommand.caps)
    ~(rule : string)
    ~(target : string)
    ~(dataflow_traces : bool)
    ()
  =
  let extra_args : string list =
    "--verbose" :: (if dataflow_traces then [ "--dataflow-traces" ] else [])
  in
  run_scan caps ~rule ~targets:[ target ] ~extra_args ()

(* Port of: test_sarif_output_include_nosemgrep.
 * Verifies that nosemgrep-suppressed findings appear with a suppressions entry
 * in SARIF. *)
let test_sarif_nosemgrep (caps : Scan_subcommand.caps) () =
  run_scan caps
    ~rule:"rules/regex/regex-nosemgrep.yaml"
    ~targets:[ "targets/basic/regex-nosemgrep.txt" ]
    ()

(* Port of: test_sarif_output_rule_board.
 * Verifies rule-board metadata (metadata.semgrep.policy) reaches SARIF. *)
let test_sarif_rule_board (caps : Scan_subcommand.caps) () =
  run_scan caps
    ~rule:"rules/rule-board-eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ()

(* Port of: test_sarif_output_with_source.
 * Verifies that rules expose [helpUri] derived from metadata.source. The
 * Python test has a secondary MOCK_USING_REGISTRY run that is python-wrapper
 * specific; we keep only the behaviour exercised by the recorded snapshot. *)
let test_sarif_with_source (caps : Scan_subcommand.caps) () =
  run_scan caps
    ~rule:"rules/eqeq-source.yml"
    ~targets:[ "targets/basic/stupid.py" ]
    ()

(* Port of: test_sarif_output_with_source_edit.
 * Verifies that rich rule [help] (markdown + text) reaches SARIF. *)
let test_sarif_with_source_edit (caps : Scan_subcommand.caps) () =
  run_scan caps
    ~rule:"rules/eqeq-meta.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ()

(* Port of: test_sarif_output_with_autofix.
 * Verifies autofix suggestions appear as SARIF fixes. *)
let test_sarif_autofix (caps : Scan_subcommand.caps) () =
  run_scan caps
    ~rule:"rules/autofix/autofix.yaml"
    ~targets:[ "targets/autofix/autofix.py" ]
    ~extra_args:[ "--autofix"; "--dryrun" ]
    ()

(* Port of: test_sarif_output_with_dataflow_traces. *)
let test_sarif_dataflow_traces (caps : Scan_subcommand.caps) () =
  run_scan caps
    ~rule:"rules/taint.yaml"
    ~targets:[ "targets/taint/taint.py" ]
    ~extra_args:[ "--dataflow-traces" ]
    ()

(* A SARIF file asked for with --sarif-output reports the suppressed findings
 * just like --sarif does, even though the format on stdout is text and would
 * hide them. *)
let test_sarif_output_file_nosemgrep (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/regex/regex-nosemgrep.yaml"
    ~targets:[ "targets/basic/regex-nosemgrep.txt" ]
    ~extra_args:[ "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* The suppressed findings SARIF asks for must not reach the JSON on stdout,
 * which reports them as ordinary findings carrying is_ignored. *)
let test_sarif_output_file_keeps_json_clean (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/regex/regex-nosemgrep.yaml"
    ~targets:[ "targets/basic/regex-nosemgrep.txt" ]
    ~extra_args:[ "--json"; "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* SARIF keeps the suppressed findings so it can report them, but they are
 * suppressed, so --error must not fail a scan that found nothing else. *)
(* A scanning root that does not exist is a fatal error: the SARIF document
 * carries it as a tool execution notification, exit code 2. *)
let test_sarif_missing_root (caps : Scan_subcommand.caps) () =
  run_scan caps ~rule:"rules/eqeq.yaml" ~targets:[]
    ~extra_args:[ "targets/basic/inexistent.py" ]
    ~check:Exit_code.Check.fatal ()

let test_sarif_error_only_suppressed (caps : Scan_subcommand.caps) () =
  run_scan caps ~rule:"rules/regex/regex-nosemgrep.yaml"
    ~targets:[ "targets/basic/regex-all-noopengrep.txt" ]
    ~extra_args:[ "--error" ] ()

(*****************************************************************************)
(* Entry point                                                                *)
(*****************************************************************************)

let basic_cases : (string * string * string) list =
  [
    ("eqeq", "rules/eqeq.yaml", "targets/basic/stupid.py");
    ("cwe_tag", "rules/cwe_tag.yaml", "targets/basic/stupid.py");
    ("metavariable_type",
     "rules/metavariable_type.yaml",
     "targets/basic/stupid.py");
    (* taint labels and requires; the matchBasedId/v1 is the fingerprint,
     * see Test_scan_subcommand.test_fingerprints *)
    ("taint labels", "rules/taint_trace.yaml", "targets/taint/taint_trace.cpp");
  ]

let tests (caps : < Scan_subcommand.caps >) =
  let basic_tests : Testo.t list =
    List.concat_map
      (fun ((label : string), (rule : string), (target : string)) ->
        [ true; false ]
        |> List.map (fun (dataflow_traces : bool) ->
               let suffix : string =
                 if dataflow_traces then " (dataflow-traces)" else ""
               in
               t
                 (Printf.sprintf "SARIF: basic %s%s" label suffix)
                 ~checked_output:(Testo.stdout ()) ~normalize:normalise
                 (test_basic_sarif caps ~rule ~target ~dataflow_traces)))
      basic_cases
  in
  Testo.categorize "Osemgrep Scan SARIF (e2e)"
    (basic_tests
     @ [
         t "SARIF: nosemgrep suppressions"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_nosemgrep caps);
         t "SARIF: rule-board metadata"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_rule_board caps);
         t "SARIF: rule metadata.source drives helpUri"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_with_source caps);
         t "SARIF: rule metadata drives rich help"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_with_source_edit caps);
         t "SARIF: autofix --dryrun"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_autofix caps);
         t "SARIF: taint --dataflow-traces"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_dataflow_traces caps);
         t "SARIF: --sarif-output file keeps nosemgrep suppressions"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_output_file_nosemgrep caps);
         t "SARIF: --sarif-output file keeps the JSON on stdout clean"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_output_file_keeps_json_clean caps);
         t "SARIF: --error ignores noopengrep-suppressed findings"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_error_only_suppressed caps);
         t "SARIF: missing scanning root, fatal error in the document"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_missing_root caps);
       ])
