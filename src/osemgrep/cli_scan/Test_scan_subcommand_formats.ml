(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of the output formats other than text and SARIF, on the
 * same fixture: the JSON, emacs, vim, GitLab SAST, GitLab secrets and JUnit
 * XML documents that a scan of rules/eqeq.yaml over targets/basic/stupid.py
 * prints on stdout. python: test_output_format, test_junit_xml_output
 *
 * SARIF has Test_scan_subcommand_sarif.ml and where the output goes has
 * Test_scan_subcommand_output.ml.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The GitLab documents carry the tool version, in the analyzer, the scanner
   and the scan, and the timestamps of the scan. The top-level "version" is
   the version of the report format, which is stable and not masked. *)
let normalise_gitlab : (string -> string) list =
  normalise
  @ [
      Testo.mask_pcre_pattern {|"url":"https://opengrep.dev","version":"([^"]*)"|};
      Testo.mask_pcre_pattern {|"version":"([^"]*)","status"|};
      Testo.mask_pcre_pattern {|"(?:start_time|end_time)":"([^"]*)"|};
    ]

let formats : (string * string * (string -> string) list) list =
  [
    ("JSON", "--json", normalise);
    ("emacs", "--emacs", normalise);
    ("vim", "--vim", normalise);
    ("GitLab SAST", "--gitlab-sast", normalise_gitlab);
    ("GitLab secrets", "--gitlab-secrets", normalise_gitlab);
    ("JUnit XML", "--junit-xml", normalise);
  ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan formats (e2e)"
    ((formats
     |> List.map
          (fun ((label : string), (flag : string), (normalize : (string -> string) list)) ->
            t (Printf.sprintf "%s output of a basic scan" label)
              ~checked_output:(Testo.stdout ()) ~normalize
              (run_scan caps ~format_args:[ flag ] ~rule:"rules/eqeq.yaml"
                 ~targets:[ "targets/basic/stupid.py" ])))
    @ [
        (* python: test_json_output_with_dataflow_traces *)
        t "JSON output with --dataflow-traces" ~checked_output:(Testo.stdout ())
          ~normalize:normalise
          (run_scan caps ~format_args:[ "--json" ]
             ~extra_args:[ "--dataflow-traces" ] ~rule:"rules/taint_trace.yaml"
             ~targets:[ "targets/taint/taint_trace.cpp" ]);
        (* python: test_output_matching_explanations *)
        t "JSON output with --matching-explanations"
          ~checked_output:(Testo.stdout ()) ~normalize:normalise
          (run_scan caps ~format_args:[ "--json" ]
             ~extra_args:[ "--matching-explanations" ]
             ~rule:"rules/eqeq-basic.yaml"
             ~targets:[ "targets/basic/stupid.js" ]);
      ])
