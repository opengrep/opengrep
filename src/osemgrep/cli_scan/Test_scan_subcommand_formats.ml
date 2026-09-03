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
(* Findings *)
(*****************************************************************************)

(* The findings of a scan in JSON, on a rule and a target of interest. *)
let json_findings (caps : Scan_subcommand.caps) ~(rule : string)
    ~(targets : string list) () =
  run_scan caps ~format_args:[ "--json" ] ~rule ~targets ()

(* Scanned as a directory: one of its files has no extension. *)
let sort_findings_dir : Testutil_files.t =
  Testutil_files.dir "sort-findings"
    (Testutil_files.read Fpath.(fixtures_root / "targets" / "sort-findings"))

let findings_tests (caps : < Scan_subcommand.caps >) =
  [
    (* python: test_taint_mode; the sink is under 'if True', the tainted
       branch is dead *)
    t "findings: taint in a dead branch" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (json_findings caps ~rule:"rules/taint.yaml"
         ~targets:[ "targets/taint/taint_dead_branch.py" ]);
    (* python: test_taint_mode_reaches_sink_through_branch; the same with a
       condition that cannot be folded away *)
    t "findings: taint through a branch" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (json_findings caps ~rule:"rules/taint.yaml"
         ~targets:[ "targets/taint/taint_branches.py" ]);
    (* python: test_multiline *)
    t "findings: multiline patterns" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (json_findings caps ~rule:"rules/multiline.yaml"
         ~targets:[ "targets/multiline/stupid.py" ]);
    (* Two rules with the same message on one range give one finding.
       python: test_deduplication_same_message *)
    t "findings: deduplicated on the same message"
      ~checked_output:(Testo.stdout ()) ~normalize:normalise
      (json_findings caps ~rule:"rules/deduplication/duplication-same-message.yaml"
         ~targets:[ "targets/deduplication/deduplication.py" ]);
    (* ... and two with different messages give two.
       python: test_deduplication_different_message *)
    t "findings: kept on different messages" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (json_findings caps
         ~rule:"rules/deduplication/duplication-different-message.yaml"
         ~targets:[ "targets/deduplication/deduplication.py" ]);
    (* python: test_sort_json_findings *)
    t "findings: sorted in JSON" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/sort-findings.yaml"
         ~targets:[] ~extra_files:[ sort_findings_dir ]
         ~extra_args:[ "sort-findings" ]);
    (* python: test_critical_severity *)
    t "findings: CRITICAL severity" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (json_findings caps ~rule:"rules/severity_critical.yaml"
         ~targets:[ "targets/basic.py" ]);
    (* python: multi_focus_metavariable, never collected there as its name
       lacks the prefix *)
    t "findings: several focus-metavariable" ~checked_output:(Testo.stdout ())
      ~normalize:normalise
      (json_findings caps ~rule:"rules/multi-focus-metavariable.yaml"
         ~targets:[ "targets/multi-focus-metavariable.py" ]);
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
      ]
    @ findings_tests caps)
