(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of what a scan reports rather than how it prints it:
 * which matches become findings, which are dropped as duplicates or by a
 * nosem comment or a severity filter, and in which order they come out.
 *
 * The output formats are in Test_scan_subcommand_formats.ml.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The findings of a scan in JSON, on a rule and a target of interest. *)
let json_findings (caps : Scan_subcommand.caps) ~(rule : string)
    ~(targets : string list) () =
  run_scan caps ~format_args:[ "--json" ] ~rule ~targets ()

(* The same on a single file, which is also the scanning root: a regex rule
   matches any file, the rule file of the scan included. *)
let json_findings_on_file (caps : Scan_subcommand.caps) ~(rule : string)
    ~(target : string) () =
  run_scan caps ~format_args:[ "--json" ] ~rule ~targets:[ target ]
    ~extra_args:[ Filename.basename target ]
    ()

(* Scanned as a directory: one of its files has no extension. *)
let sort_findings_dir : Testutil_files.t =
  Testutil_files.dir "sort-findings"
    (Testutil_files.read Fpath.(fixtures_root / "targets" / "sort-findings"))

(* Files of several languages, each with nosem comments. *)
let basic_dir : Testutil_files.t =
  Testutil_files.dir "basic"
    (Testutil_files.read Fpath.(fixtures_root / "targets" / "basic"))

(* A rule copied into the repo under rules/, which gives its id the "rules."
   prefix. The nosem comments below use the id with that prefix. *)
let rules_dir (name : string) : Testutil_files.t =
  Testutil_files.dir "rules"
    [ Testutil_files.File (name, read_fixture ("rules/" ^ name)) ]

(* The generic matching engines, each rule over the file it is written for.
   The patterns are generic and would match the rule files too, so each scan
   is given its target. python: test_aliengrep, test_spacegrep *)
let aliengrep_cases : (string * string) list =
  [
    ("html", "html.mustache");
    ("markdown", "markdown.md");
    ("httpresponse", "httpresponse.txt");
    ("dockerfile", "dockerfile");
    ("multi-lines", "multi-lines.java");
    ("terraform", "terraform.tf");
    ("begin-end", "begin-end.log");
    ("long-match", "long-match.txt");
    ("metavariable-pattern", "metavariable-pattern.conf");
  ]

let spacegrep_cases : (string * string) list =
  [
    ("html", "html.mustache");
    ("markdown", "markdown.md");
    ("httpresponse", "httpresponse.txt");
    ("dockerfile", "root.Dockerfile");
    ("dockerfile", "dockerfile");
    ("multi-lines", "multi-lines.java");
    ("terraform", "terraform.tf");
  ]

(* One scan of a generic rule over its target. *)
let generic_engine_test (caps : Scan_subcommand.caps) ~(engine : string)
    ~(rule : string) ~(target : string) =
  t
    (Printf.sprintf "findings: %s %s on %s" engine rule target)
    ~checked_output:(Testo.stdout ()) ~normalize:normalise
    (run_scan caps ~format_args:[ "--json" ]
       ~rule:(Printf.sprintf "rules/%s/%s.yaml" engine rule)
       ~targets:[ Printf.sprintf "targets/%s/%s" engine target ]
       ~extra_args:[ target ])

(* The message of a finding interpolates the metavariables the rule bound,
   including those bound by a pattern-inside. Each pair is a rule and the
   file it has something to say about.
   python: test_message_interpolation *)
let message_interpolation : (string * string) list =
  [
    ("pattern-inside", "pattern_inside_basic");
    ("pattern-inside", "pattern_inside_complex");
    ("propagated-constant", "propagated_constant");
    (* pattern-not-inside is not interpolated; these check it stays that
       way *)
    ("pattern-not-inside", "pattern_not_inside_basic");
    ("pattern-not-inside", "pattern_not_inside_complex");
    ("pattern-either", "pattern_either_basic");
    ("multi-pattern-inside", "multi_pattern_inside");
    ("multi-pattern-inside", "multi_pattern_inside_nested");
  ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan findings (e2e)"
    ([
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
        (json_findings caps
           ~rule:"rules/deduplication/duplication-same-message.yaml"
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
      (* A nosemgrep comment silences a regex rule too.
         python: test_regex_rule__nosemgrep *)
      t "findings: nosemgrep in a regex rule" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings caps ~rule:"rules/regex/regex-nosemgrep.yaml"
           ~targets:[ "targets/basic/regex-nosemgrep.txt" ]);
      (* The nosem comments of each language. python: test_nosem_rule *)
      t "findings: nosem comments" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~targets:[]
           ~extra_files:[ rules_dir "nosem.yaml"; basic_dir ]
           ~extra_args:[ "--config"; "rules/nosem.yaml"; "basic" ]);
      (* ... and what they silence comes back with the flag.
         python: test_nosem_rule__with_disable_nosem *)
      t "findings: nosem comments disabled" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~targets:[]
           ~extra_files:[ rules_dir "nosem.yaml"; basic_dir ]
           ~extra_args:
             [ "--disable-nosem"; "--config"; "rules/nosem.yaml"; "basic" ]);
      (* One nosem comment names both rule ids, so neither is reported.
         python: test_nosem_with_multiple_ids *)
      t "findings: nosem naming several ids" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~targets:[]
           ~extra_files:
             [
               rules_dir "two_matches.yaml";
               Testutil_files.File
                 ( "multiple-nosemgrep.py",
                   read_fixture "targets/nosemgrep/multiple-nosemgrep.py" );
             ]
           ~extra_args:
             [ "--config"; "rules/two_matches.yaml"; "multiple-nosemgrep.py" ]);
      (* --severity keeps the rules of that severity. rules/inside.yaml has
         one ERROR rule, so the other two severities report nothing.
         python: test_severity_error *)
      t "findings: --severity ERROR" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/inside.yaml"
           ~targets:[] ~extra_files:[ basic_dir ]
           ~extra_args:[ "--severity"; "ERROR"; "basic" ]);
      (* python: test_severity_info *)
      t "findings: --severity INFO" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/inside.yaml"
           ~targets:[] ~extra_files:[ basic_dir ]
           ~extra_args:[ "--severity"; "INFO"; "basic" ]);
      (* python: test_severity_warning *)
      t "findings: --severity WARNING" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/inside.yaml"
           ~targets:[] ~extra_files:[ basic_dir ]
           ~extra_args:[ "--severity"; "WARNING"; "basic" ]);
      (* python: test_severity_multiple *)
      t "findings: --severity twice" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/inside.yaml"
           ~targets:[] ~extra_files:[ basic_dir ]
           ~extra_args:
             [ "--severity"; "INFO"; "--severity"; "WARNING"; "basic" ]);
      (* The same rule without a severity filter.
         python: test_equivalence of test_metavariable_matching.py *)
      t "findings: pattern-inside over a directory"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/inside.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: multi_focus_metavariable, never collected there as its name
         lacks the prefix *)
      t "findings: several focus-metavariable" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings caps ~rule:"rules/multi-focus-metavariable.yaml"
           ~targets:[ "targets/multi-focus-metavariable.py" ]);
      (* metavariable-comparison over the basic targets, in its plain form
         and with the base and strip options.
         python: test_metavariable_comparison_rule *)
      t "findings: metavariable-comparison" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/metavariable-comparison/metavariable-comparison.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: test_metavariable_comparison_rule_base *)
      t "findings: metavariable-comparison with a base"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:
             "rules/metavariable-comparison/metavariable-comparison-base.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: test_metavariable_comparison_rule_strip *)
      t "findings: metavariable-comparison with strip"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:
             "rules/metavariable-comparison/metavariable-comparison-strip.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* The compared content does not parse as a number, so nothing is
         reported. python: test_metavariable_comparison_rule_bad_content *)
      t "findings: metavariable-comparison on content that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:
             "rules/metavariable-comparison/metavariable-comparison-bad-content.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* The compared metavariable holds a propagated constant.
         python: test_metavariable_propagation_comparison *)
      t "findings: metavariable-comparison on a propagated constant"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings caps
           ~rule:
             "rules/metavariable_propagation/metavariable-comparison-propagation.yaml"
           ~targets:
             [
               "targets/metavariable_propagation/metavariable-comparison-propagation.py";
             ]);
      (* The content of the metavariable does not parse as Bash. The scan
         reports it and --strict, which the Python harness always passes,
         turns the warning into a fatal exit.
         python: test1 of test_metavariable_pattern.py *)
      t "findings: metavariable-pattern on content that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/metavariable-pattern/test1.json"
           ~targets:[ "targets/metavariable-pattern/test1.yml" ]
           ~extra_args:[ "--strict"; "test1.yml" ]
           ~check:Exit_code.Check.fatal);
      (* A metavariable-pattern whose inner language differs from the outer
         one. python: test2 of test_metavariable_pattern.py *)
      t "findings: metavariable-pattern in another language"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings caps ~rule:"rules/metavariable-pattern/test2.yaml"
           ~targets:[ "targets/metavariable-pattern/test2.php" ]);
      (* pattern-regex at the top of a rule, and as a child of patterns.
         python: test_regex_rule__top *)
      t "findings: pattern-regex at the top" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/regex/regex-top.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: test_regex_rule__child *)
      t "findings: pattern-regex under patterns" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/regex/regex-child.yaml" ~targets:[]
           ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* A regex over a file that is not valid UTF-8, and over a PNG.
         python: test_regex_rule__utf8 *)
      t "findings: regex on a UTF-8 file" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings_on_file caps ~rule:"rules/regex/regex-utf8.yaml"
           ~target:"targets/basic/regex-utf8.txt");
      (* python: test_regex_rule__utf8_on_image *)
      t "findings: regex on an image" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings_on_file caps ~rule:"rules/regex/regex-utf8.yaml"
           ~target:"targets/image/semgrep.png");
      (* python: test_regex_rule__not *)
      t "findings: pattern-not-regex" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/pattern-not-regex/regex-not.yaml"
           ~target:"targets/basic/stupid.py");
      (* python: test_regex_rule__not2 *)
      t "findings: pattern-not-regex on HTML" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/pattern-not-regex/regex-not2.yaml"
           ~target:"targets/basic/regex-any-language.html");
      (* python: test_regex_rule__pattern_regex_and_pattern_not_regex *)
      t "findings: pattern-regex with pattern-not-regex"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/pattern-not-regex/regex-not-with-pattern-regex.yaml"
           ~target:"targets/basic/regex-any-language.html");
      (* python: test_regex_rule__issue2465 *)
      t "findings: pattern-not-regex on a requirements file"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/pattern-not-regex/issue2465.yaml"
           ~target:"targets/pattern-not-regex/issue2465.requirements.txt");
      (* metavariable-regex, on its own, twice in a rule, and twice over
         one metavariable. python: test_metavariable_regex_rule *)
      t "findings: metavariable-regex" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/metavariable-regex/metavariable-regex.yaml" ~targets:[]
           ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: test_metavariable_regex_multi_rule *)
      t "findings: metavariable-regex in two rules"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/metavariable-regex/metavariable-regex-multi-rule.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: test_metavariable_multi_regex_rule *)
      t "findings: two metavariable-regex in one rule"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/metavariable-regex/metavariable-regex-multi-regex.yaml"
           ~targets:[] ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]);
      (* python: test_metavariable_regex_const_prop *)
      t "findings: metavariable-regex on a propagated constant"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/metavariable-regex/metavariable-regex-const-prop.yaml"
           ~target:
             "targets/metavariable_propagation/metavariable-regex-const-prop.dockerfile");
      (* python: test_metavariable_propagation_regex *)
      t "findings: metavariable-regex through propagation"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:
             "rules/metavariable_propagation/metavariable-regex-propagation.yaml"
           ~target:
             "targets/metavariable_propagation/metavariable-regex-propagation.py");
      (* A regex rule for every language, spelled 'generic' and 'none'.
         python: test_regex_with_any_language_rule *)
      t "findings: regex for any language" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_findings_on_file caps ~rule:"rules/regex/regex-any-language.yaml"
           ~target:"targets/basic/regex-any-language.html");
      (* python: test_regex_with_any_language_multiple_rule *)
      t "findings: two regexes for any language"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/regex/regex-any-language-multiple.yaml"
           ~target:"targets/basic/regex-any-language.html");
      (* python: test_regex_with_any_language_rule_none_alias *)
      t "findings: regex for the 'none' language"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/regex/regex-any-language-alias-none.yaml"
           ~target:"targets/basic/regex-any-language.html");
      (* python: test_regex_with_any_language_multiple_rule_none_alias *)
      t "findings: two regexes for the 'none' language"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps
           ~rule:"rules/regex/regex-any-language-multiple-alias-none.yaml"
           ~target:"targets/basic/regex-any-language.html");
      (* differs from the Python wrapper: it reports the invalid regex as a
         matching error of exit code 2 and scans on; opengrep rejects the
         rule and, as for every configuration it cannot load, exits 7 with
         an error of code 4 *)
      (* python: test_regex_rule__invalid_expression *)
      t "findings: an invalid regex" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/regex/regex-invalid.yaml" ~targets:[]
           ~extra_files:[ basic_dir ] ~extra_args:[ "basic" ]
           ~check:Exit_code.Check.missing_config);
      (* A regex that matches the empty string reports the empty file, at
         its line 1. python: test_pattern_regex_empty_file *)
      t "findings: pattern-regex over an empty file"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings_on_file caps ~rule:"rules/pattern-regex-empty-file.yaml"
           ~target:"targets/empty/totally_empty_file");
      (* A nosem comment in an HTML file, with the rule ids left as written.
         python: test_aliengrep_nosem *)
      t "findings: aliengrep nosem-html on nosem.html"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/aliengrep/nosem-html.yaml"
           ~targets:[ "targets/aliengrep/nosem.html" ]
           ~extra_args:[ "--no-rewrite-rule-ids"; "nosem.html" ]);
      (* python: test_spacegrep_nosem *)
      t "findings: spacegrep nosem-html on nosem.html"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ]
           ~rule:"rules/spacegrep/nosem-html.yaml"
           ~targets:[ "targets/spacegrep/nosem.html" ]
           ~extra_args:[ "--no-rewrite-rule-ids"; "nosem.html" ]);
      (* A target that does not parse: the JSON carries the syntax error,
         the spans it covers and the skipped path, and with --strict the
         scan ends with the exit code of invalid target code.
         differs from the Python wrapper: the skipped path also carries a
         "details" field with the unexpected text
         python: test_file_parser__failure__error_messages, invalid_go.go *)
      t "findings: a Go target that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/eqeq-basic.yaml"
           ~targets:[ "targets/bad/invalid_go.go" ]
           ~extra_args:[ "--verbose"; "--strict"; "invalid_go.go" ]
           ~check:Exit_code.Check.invalid_code);
      (* differs from the Python wrapper: the same extra "details" field
         python: test_file_parser__failure__error_messages,
         invalid_python.py *)
      t "findings: a Python target that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/eqeq-python.yaml"
           ~targets:[ "targets/bad/invalid_python.py" ]
           ~extra_args:[ "--verbose"; "--strict"; "invalid_python.py" ]
           ~check:Exit_code.Check.invalid_code);
      (* A metavariable of the message keeps its own value, so a message
         that already holds one is not interpolated twice.
         python: test_no_double_interpolation *)
      t "findings: the message is interpolated once"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_findings caps
           ~rule:"rules/message_interpolation/interpolated_message.yaml"
           ~targets:
             [ "targets/message_interpolation/target_with_metavariable.py" ]);
    ]
    @ (aliengrep_cases
      |> List.map (fun ((rule : string), (target : string)) ->
             generic_engine_test caps ~engine:"aliengrep" ~rule ~target))
    @ (spacegrep_cases
      |> List.map (fun ((rule : string), (target : string)) ->
             generic_engine_test caps ~engine:"spacegrep" ~rule ~target))
    @ (message_interpolation
      |> List.map (fun ((rule : string), (target : string)) ->
             t
               (Printf.sprintf "findings: message of %s on %s" rule target)
               ~checked_output:(Testo.stdout ()) ~normalize:normalise
               (json_findings caps
                  ~rule:("rules/message_interpolation/" ^ rule ^ ".yaml")
                  ~targets:
                    [ "targets/message_interpolation/" ^ target ^ ".py" ]))))
