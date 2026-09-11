(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

let t = Testo.create

module F = Testutil_files

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing end-to-end (e2e) the test subcommand.
 * Since `semgrep test` is itself a test, we are actually most interested in
 * the `--matching-diagnosis` flag, and its associated output.
 *)

(*****************************************************************************)
(* Test cases *)
(*****************************************************************************)

let unexpected_match_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      all:
      - pattern: foo(...)
      - not: foo(goood)
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_match_test_content =
  {|
# ruleid: no-foo-unless-good
foo()
# ok: no-foo-unless-good
foo(good)
|}

let unexpected_match_multiple_rule_content =
  {|
rules:
  - id: no-foo-bar-unless-good
    match:
      any:
        - all:
          - pattern: foo(...)
          - not: foo(goood)
        - all:
          - pattern: bar(...)
          - not: bar(goood)
    message: "don't allow foo/bar unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_match_multiple_test_content =
  {|
# ruleid: no-foo-bar-unless-good
foo()
# ok: no-foo-bar-unless-good
foo(good)
# ruleid: no-foo-bar-unless-good
bar()
# ok: no-foo-bar-unless-good
bar(good)
|}

let unexpected_no_match_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      all:
        - pattern: foo(...)
        - not: foo($X)
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_no_match_test_content =
  {|
# ruleid: no-foo-unless-good
foo(bad)
# ruleid: no-foo-unless-good
foo(good)
|}

let _unexpected_no_match_never_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      pattern: nonexistent
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_no_match_redundant_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      all:
        - pattern: foo(...)
        - not: foo($X)
        - not: $Y
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

(* the test file path of a failed check is absolute *)
let normalize = [ Testutil_logs.mask_time; Testutil.mask_temp_paths () ]

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let mk_matching_explanation_tests (caps : Test_subcommand.caps) =
  let tests =
    [
      ( "matching diagnosis unexpected match",
        unexpected_match_rule_content,
        unexpected_match_test_content );
      ( "matching diagnosis unexpected match (multiple)",
        unexpected_match_multiple_rule_content,
        unexpected_match_multiple_test_content );
      ( "matching diagnosis unexpected no match",
        unexpected_no_match_rule_content,
        unexpected_no_match_test_content );
      (* TODO: started to fail at https://github.com/semgrep/semgrep-proprietary/pull/2199
            ( "matching diagnosis unexpected no match (never matched)",
              unexpected_no_match_never_rule_content,
              unexpected_no_match_test_content );
      *)
      ( "matching diagnosis unexpected no match (redundant not)",
        unexpected_no_match_redundant_rule_content,
        unexpected_no_match_test_content );
    ]
  in
  List_.map
    (fun (test_name, rule, test_content) ->
      t ~checked_output:(Testo.split_stdout_stderr ()) ~normalize test_name (fun () ->
          Logs.app (fun m -> m "Snapshot for %s" test_name);
          let files =
            [ F.File ("test.yaml", rule); F.File ("test.py", test_content) ]
          in
          Testutil_files.with_tempfiles ~verbose:true ~chdir:true files
            (fun _cwd ->
              let exit_code =
                Test_subcommand.main caps
                  [| "opengrep-test"; "."; "--matching-diagnosis" |]
              in
              Exit_code.Check.findings exit_code)))
    tests

(* every test target that does not exist is reported, and the run aborts *)
let test_missing_targets (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("test.yaml", unexpected_match_rule_content);
      F.File ("test.py", unexpected_match_test_content);
    ]
  in
  Testutil_files.with_tempfiles ~chdir:true files (fun _cwd ->
      let run (argv : string list) : string =
        match Test_subcommand.main caps (Array.of_list argv) with
        | exception Error.Semgrep_error (msg, None) -> msg
        | _ -> Alcotest.fail "expected the test run to abort"
      in
      Alcotest.(check string)
        "a directory without --config" "File not found: nope"
        (run [ "opengrep-test"; "nope" ]);
      Alcotest.(check string)
        "files with --config"
        "File not found: nope.py\nFile not found: nope2.py"
        (run
           [
             "opengrep-test"; "--config"; "test.yaml"; "test.py"; "nope.py";
             "nope2.py";
           ]))

(*****************************************************************************)
(* Fixtests *)
(*****************************************************************************)
(* The rule and target pairs of tests/fixtest, in text and JSON output,
 * with the target's .fixed file when there is one. The test file path of
 * a failed check and the keys of the JSON matches are absolute, hence the
 * temp path mask.
 *)

let fixtest_fixtures_root : Fpath.t = Fpath.v "tests/fixtest"

(* (rule in rules/, target in targets/, expected exit code) *)
let fixtest_cases : (string * string * (Exit_code.t -> unit)) list =
  [
    (* the fixtest passes *)
    ("basic_fix.yaml", "test1.py", Exit_code.Check.ok);
    (* no fixtest for a rule with a fix: config_missing_fixtests *)
    ("basic_fix.yaml", "test2.py", Exit_code.Check.ok);
    (* the fixtest fails: the diff is reported *)
    ("other_fix.yaml", "test3.py", Exit_code.Check.findings);
    (* the checks fail too: the missed lines are reported *)
    ("other_pattern.yaml", "test4.py", Exit_code.Check.findings);
    (* a fix-regex without fixtest: config_missing_fixtests *)
    ("basic_fix_regex.yaml", "no_associated_fixed.py", Exit_code.Check.ok);
    (* the trailing newlines of a fix: are not added to the fixed file *)
    ("fix_trailing_newline.yaml", "basic.go", Exit_code.Check.ok);
  ]

let run_fixtest (caps : Test_subcommand.caps) ~(rule : string)
    ~(target : string) ~(json : bool) ~(check : Exit_code.t -> unit) () =
  let read (rel : string) : string =
    UFile.read_file Fpath.(fixtest_fixtures_root // v rel)
  in
  let fixed : string =
    let stem, ext = Fpath_.split_ext ~multi:true (Fpath.v target) in
    Fpath.(to_string (add_ext (".fixed" ^ ext) stem))
  in
  let target_files : F.t list =
    F.File (target, read ("targets/" ^ target))
    ::
    (if Sys.file_exists Fpath.(to_string (fixtest_fixtures_root / "targets" / fixed))
     then [ F.File (fixed, read ("targets/" ^ fixed)) ]
     else [])
  in
  let files : F.t list =
    [
      F.Dir ("rules", [ F.File (rule, read ("rules/" ^ rule)) ]);
      F.Dir ("targets", target_files);
    ]
  in
  Testutil_files.with_tempfiles ~verbose:true ~chdir:true files (fun _cwd ->
      let argv : string list =
        [ "opengrep-test"; "--config"; "rules/" ^ rule; "targets/" ^ target ]
        @ if json then [ "--json" ] else []
      in
      check (Test_subcommand.main caps (Array.of_list argv)))

let mk_fixtest_tests (caps : Test_subcommand.caps) : Testo.t list =
  fixtest_cases
  |> List.concat_map (fun ((rule : string), (target : string), check) ->
         [ ("text", false); ("json", true) ]
         |> List_.map (fun ((label : string), (json : bool)) ->
                t
                  (Printf.sprintf "fixtest: %s %s %s" rule target label)
                  ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
                  (run_fixtest caps ~rule ~target ~json ~check)))

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)
(* The rule and target pairs of tests/test_subcommand, run the way the
 * Python tests ran them: the config and the target are named on the command
 * line, and the checks are reported in JSON or in text.
 *)

let checks_fixtures_root : Fpath.t = Fpath.v "tests/test_subcommand"

(* A rule or a target of those fixtures, a file or a directory, copied into
   the temporary directory under its own name. *)
let fixture_entry (kind : string) (name : string) : F.t =
  let path : Fpath.t = Fpath.(checks_fixtures_root / kind / name) in
  if Sys.is_directory (Fpath.to_string path) then
    F.dir name (Testutil_files.read path)
  else F.File (name, UFile.read_file path)

let run_checks (caps : Test_subcommand.caps) ?(extra_flags : string list = [])
    ?(check : Exit_code.t -> unit = Exit_code.Check.ok) ~(rule : string)
    ~(target : string) ~(json : bool) () =
  let files : F.t list =
    [
      F.dir "rules" [ fixture_entry "rules" rule ];
      F.dir "targets" [ fixture_entry "targets" target ];
    ]
  in
  Testutil_files.with_tempfiles ~verbose:true ~chdir:true files (fun _cwd ->
      let argv : string list =
        [ "opengrep-test"; "--config"; "rules/" ^ rule; "targets/" ^ target ]
        @ extra_flags
        @ if json then [ "--json" ] else []
      in
      check (Test_subcommand.main caps (Array.of_list argv)))

(* (test name, rule in rules/, target in targets/, JSON output) *)
let checks_cases : (string * string * string * bool) list =
  [
    (* python: test_cli_test_basic *)
    ("a rule file over a target file, JSON", "basic.yaml", "basic.py", true);
    (* the same pair in text, which the Python test ran through the
       installed entry point. python: test_cli_test_from_entrypoint *)
    ("a rule file over a target file, text", "basic.yaml", "basic.py", false);
    (* a directory of rules over a directory of targets.
       python: test_cli_test_directory *)
    ("a rule directory over a target directory, JSON", "directory", "directory",
      true);
    (* the test file of a rule about YAML is named .test.yaml, since the rule
       file already takes the .yaml name.
       python: test_cli_test_yaml_language *)
    ("a YAML rule and its .test.yaml target, JSON", "yaml_language",
      "yaml_language", true);
    (* a rule file name made of several suffixes, this.that.check.yaml, pairs
       with this.that.check.py. python: test_cli_test_suffixes *)
    ("a rule file name with several suffixes, JSON", "suffixes", "suffixes",
      true);
    (* one annotation line naming two rule ids.
       python: test_cli_test_multiple_annotations *)
    ("several rule ids on one annotation line, text", "overlapping_rules.yaml",
      "multiple_annotations.py", false);
    (* the 'paths: include:' of the rule is ignored, so the rule runs on a
       test file whose name does not satisfy it.
       python: test_cli_test_ignore_rule_paths *)
    ("the rule's paths include is ignored, JSON",
      "rule_with_paths_include_bar_xml.yaml", "foo.xml", true);
    (* a match annotated with todook: is not reported.
       python: test_cli_todook_filtering *)
    ("a todook annotation is not reported, JSON", "basic.yaml", "todook.py",
      true);
  ]

let mk_checks_tests (caps : Test_subcommand.caps) : Testo.t list =
  (checks_cases
  |> List_.map
       (fun
           ( (name : string),
             (rule : string),
             (target : string),
             (json : bool) )
         ->
         t ("checks: " ^ name) ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
           (run_checks caps ~rule ~target ~json)))
  @ [
      (* the rule never finishes within the default time limit, and a shorter
         one keeps the test quick. The rule then reports nothing and the
         annotated lines are missed. python: test_timeout *)
      t "checks: a rule that times out, JSON" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (run_checks caps ~extra_flags:[ "--timeout"; "1" ]
           ~check:Exit_code.Check.findings ~rule:"rule_that_timeout.yaml"
           ~target:"long.py" ~json:true);
      (* a rule file the loader rejects is reported in config_with_errors and
         the run goes on with the other files, but it fails the run *)
      t "checks: a rule file that does not load, JSON"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (run_checks caps ~check:Exit_code.Check.findings
           ~rule:"no_pattern.yaml" ~target:"no_pattern.py" ~json:true);
      (* --strict does not change that verdict *)
      t "checks: a rule file that does not load with --strict, text"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (run_checks caps ~extra_flags:[ "--strict" ]
           ~check:Exit_code.Check.findings ~rule:"no_pattern.yaml"
           ~target:"no_pattern.py" ~json:false);
    ]

(*****************************************************************************)
(* Pairing rules with targets, and the todo annotations *)
(*****************************************************************************)

(* Run the subcommand over [files] and return its exit code and its JSON
   report. *)
let run_test_json (caps : Test_subcommand.caps) (files : F.t list)
    (argv : string list) : Exit_code.t * Semgrep_output_v1_t.tests_result =
  Testutil_files.with_tempfiles ~chdir:true files (fun _cwd ->
      let exit_code, out =
        Testo.with_capture stdout (fun () ->
            Test_subcommand.main caps
              (Array.of_list (("opengrep-test" :: argv) @ [ "--json" ])))
      in
      (exit_code, Semgrep_output_v1_j.tests_result_of_string out))

let eqeq_rule_content =
  {|
rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

let other_rule_content =
  {|
rules:
  - id: no-print
    pattern: print(...)
    message: "no print"
    languages: [python]
    severity: ERROR
|}

(* one expected finding of eqeq-is-bad on its second line *)
let eqeq_target_content = "# ruleid: eqeq-is-bad\nx == x\n"

(* 'test --config rules targets', without the trailing slashes, pairs each
   rule with the target that carries its name. python: test.py asked the file
   system whether the target was a file (Path.is_file), where a syntactic
   test held for every path and paired every rule with every target. *)
let test_rules_and_targets_directories (caps : Test_subcommand.caps) () =
  let files =
    [
      F.dir "rules"
        [
          F.File ("eqeq.yaml", eqeq_rule_content);
          F.File ("other.yaml", other_rule_content);
        ];
      F.dir "targets"
        [
          F.File ("eqeq.py", eqeq_target_content);
          F.File ("other.py", "# ruleid: no-print\nprint(1)\n");
          F.File ("orphan.py", "y = 1\n");
        ];
    ]
  in
  let checked (args : string list) : (string * bool) list =
    let exit_code, res = run_test_json caps files args in
    Exit_code.Check.ok exit_code;
    (* the key of a rule file keeps the config path as given, so only
       the checks themselves are compared *)
    res.results
    |> List.concat_map
         (fun ((_rule_file : string), (checks : Semgrep_output_v1_t.checks)) ->
           checks.checks
           |> List_.map
                (fun
                  ((rule_id : string), (r : Semgrep_output_v1_t.rule_result))
                -> (rule_id, r.passed)))
    |> List.sort (fun ((a : string), _) ((b : string), _) -> String.compare a b)
  in
  let expected = [ ("eqeq-is-bad", true); ("no-print", true) ] in
  Alcotest.(check (list (pair string bool)))
    "without the trailing slashes" expected
    (checked [ "--config"; "rules"; "targets" ]);
  Alcotest.(check (list (pair string bool)))
    "with the trailing slashes" expected
    (checked [ "--config"; "rules/"; "targets/" ])

(* Check the verdict and the two line sets of a report that holds one rule
   file with one check. *)
let check_single_result (name : string) (res : Semgrep_output_v1_t.tests_result)
    ~(passed : bool) ~(expected_lines : int list) ~(reported_lines : int list) :
    unit =
  match res.results with
  | [ (_rule_file, { checks = [ (_rule_id, rule_result) ] }) ] -> (
      Alcotest.(check bool) (name ^ ": the verdict") passed rule_result.passed;
      match rule_result.matches with
      | [ (_target, lines) ] ->
          Alcotest.(check (list int))
            (name ^ ": the expected lines")
            expected_lines lines.expected_lines;
          Alcotest.(check (list int))
            (name ^ ": the reported lines")
            reported_lines lines.reported_lines
      | _ -> Alcotest.fail (name ^ ": expected one target"))
  | _ -> Alcotest.fail (name ^ ": expected one rule with one check")

(* python: test.py takes the todook: and todoruleid: lines out of both the
   expected and the reported set before comparing them, so a stale annotation
   of either kind cannot fail the check. *)
let test_todo_annotations (caps : Test_subcommand.caps) () =
  let check (name : string) (target : string) : unit =
    let files =
      [ F.File ("eqeq.yaml", eqeq_rule_content); F.File ("eqeq.py", target) ]
    in
    let exit_code, res =
      run_test_json caps files [ "--config"; "eqeq.yaml"; "eqeq.py" ]
    in
    Exit_code.Check.ok exit_code;
    check_single_result name res ~passed:true ~expected_lines:[ 2 ]
      ~reported_lines:[ 2 ]
  in
  (* the engine now matches a line the annotation says it should not *)
  check "todoruleid"
    "# ruleid: eqeq-is-bad\nx == x\n# todoruleid: eqeq-is-bad\ny == y\n";
  (* the engine no longer matches a line the annotation says it does *)
  check "todook"
    "# ruleid: eqeq-is-bad\nx == x\n# todook: eqeq-is-bad\nz == w\n"

(* A rule with a fix and two test targets is listed once among the configs
   without a fixtest, and --matching-diagnosis reports no diagnosis for it
   rather than stopping the run. *)
let test_two_targets_for_one_rule (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File
        ( "dup.yaml",
          {|
rules:
  - id: dup-rule
    pattern: foo($X)
    fix: bar($X)
    message: m
    languages: [python, js]
    severity: ERROR
|}
        );
      F.File ("dup.py", "# ruleid: dup-rule\nfoo(1)\n");
      F.File ("dup.js", "// ruleid: dup-rule\nfoo(1)\n");
    ]
  in
  let exit_code, res = run_test_json caps files [ "."; "--matching-diagnosis" ] in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string))
    "the rule file is listed once" [ "dup.yaml" ]
    (List_.map Fpath.to_string res.config_missing_fixtests)

(*****************************************************************************)
(* Parity with the Python wrapper *)
(*****************************************************************************)

let eqeq_and_print_rule_content =
  {|
rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
  - id: no-print
    pattern: print(...)
    message: "no print"
    languages: [python]
    severity: ERROR
|}

(* a target named on the command line is scanned whatever its extension,
   like a file named on the scan command line. *)
let test_target_with_unknown_extension (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File ("eqeq.weird", eqeq_target_content);
    ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "eqeq.yaml"; "eqeq.weird" ]
  in
  Exit_code.Check.ok exit_code;
  check_single_result "an extension no language claims" res ~passed:true
    ~expected_lines:[ 2 ] ~reported_lines:[ 2 ]

(* an annotation may follow code on the same line *)
let test_annotation_after_code (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File ("eqeq.py", "x = 1 # ruleid: eqeq-is-bad\nx == x\n");
    ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "eqeq.yaml"; "eqeq.py" ]
  in
  Exit_code.Check.ok exit_code;
  check_single_result "an annotation after code" res ~passed:true
    ~expected_lines:[ 2 ] ~reported_lines:[ 2 ]

(* a todook: line is excluded from the comparison of the rule it names only *)
let test_todo_annotations_are_per_rule_id (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("both.yaml", eqeq_and_print_rule_content);
      (* the todook: names no-print only; both rules match its line, and
         eqeq-is-bad is annotated further down so that every rule that
         matches is named in the file. The 'ok: no-print' puts no-print in
         the set the rule id mismatch check compares, which a todook: line
         does not do. *)
      F.File
        ( "both.py",
          "# ok: no-print\nz = 1\n# todook: no-print\nprint(x == x)\n# ruleid: \
           eqeq-is-bad\nx == x\n" );
    ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "both.yaml"; "both.py" ]
  in
  Exit_code.Check.findings exit_code;
  let checks =
    res.results
    |> List.concat_map
         (fun ((_rule_file : string), (checks : Semgrep_output_v1_t.checks)) ->
           checks.checks
           |> List_.map
                (fun
                  ((rule_id : string), (r : Semgrep_output_v1_t.rule_result))
                ->
                  ( rule_id,
                    r.passed,
                    r.matches
                    |> List.concat_map (fun (_target, lines) ->
                           lines.Semgrep_output_v1_t.reported_lines) )))
    |> List.sort (fun ((a : string), _, _) ((b : string), _, _) ->
           String.compare a b)
  in
  Alcotest.(check (list (triple string bool (list int))))
    "the todook: of one rule does not hide the match of the other"
    [ ("eqeq-is-bad", false, [ 4; 6 ]); ("no-print", true, []) ]
    checks

(* a match on a line with the ignore annotation is not reported *)
let test_custom_ignore_pattern (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File
        ( "eqeq.py",
          "# ruleid: eqeq-is-bad\nx == x\n# ok: eqeq-is-bad\ny == y # noaikido\n"
        );
    ]
  in
  let exit_code, res =
    run_test_json caps files
      [
        "--config"; "eqeq.yaml"; "eqeq.py"; "--opengrep-ignore-pattern";
        "noaikido";
      ]
  in
  Exit_code.Check.ok exit_code;
  check_single_result "a line with a custom ignore comment" res ~passed:true
    ~expected_lines:[ 2 ] ~reported_lines:[ 2 ]

(* python: without a target, the wrapper tested the current directory. *)
let test_no_target_means_current_directory (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File ("eqeq.py", eqeq_target_content);
    ]
  in
  let exit_code, res = run_test_json caps files [] in
  Exit_code.Check.ok exit_code;
  check_single_result "no target on the command line" res ~passed:true
    ~expected_lines:[ 2 ] ~reported_lines:[ 2 ]

(* only one target directory is allowed, and the message names the ones given *)
let test_two_target_directories (caps : Test_subcommand.caps) () =
  let files =
    [
      F.dir "rules" [ F.File ("eqeq.yaml", eqeq_rule_content) ];
      F.dir "a" [ F.File ("eqeq.py", eqeq_target_content) ];
      F.dir "b" [ F.File ("eqeq.py", eqeq_target_content) ];
    ]
  in
  Testutil_files.with_tempfiles ~chdir:true files (fun _cwd ->
      match
        Test_subcommand.main caps
          [| "opengrep-test"; "--config"; "rules"; "a"; "b" |]
      with
      | exception Error.Semgrep_error (msg, None) ->
          Alcotest.(check string)
            "the message names both directories"
            "only one target directory is allowed, got: a b" msg
      | _ -> Alcotest.fail "expected the test run to abort")

(* python: check_rule_id_mismatch. The annotations of a file must name the
   rules that matched in it, no more and no fewer; otherwise the run exits
   2 before any report. *)
let test_rule_id_mismatch (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("both.yaml", eqeq_and_print_rule_content);
      (* no-print matches the second line, which carries no annotation *)
      F.File ("both.py", "# ruleid: eqeq-is-bad\nx == x\nprint(1)\n");
    ]
  in
  Testutil_files.with_tempfiles ~chdir:true files (fun _cwd ->
      let exit_code =
        Test_subcommand.main caps
          [| "opengrep-test"; "--config"; "both.yaml"; "both.py" |]
      in
      Exit_code.Check.fatal exit_code)

(* python: test.py built the set it compares from the 'ruleid:',
   'todoruleid:' and 'ok:' lines only, so a 'todook:' naming a rule that never
   fires does not stop the run; and it keyed the checks on the 'ruleid:' and
   'todoruleid:' lines, so such a rule id is not a check of its own. *)
let test_todook_for_a_rule_that_never_matches (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("both.yaml", eqeq_and_print_rule_content);
      F.File
        ("both.py", "# ruleid: eqeq-is-bad\nx == x\n# todook: no-print\nz = 1\n");
    ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "both.yaml"; "both.py" ]
  in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list (pair string bool)))
    "the annotated rule is the only check"
    [ ("eqeq-is-bad", true) ]
    (res.results
    |> List.concat_map
         (fun ((_rule_file : string), (checks : Semgrep_output_v1_t.checks)) ->
           checks.checks
           |> List_.map
                (fun
                  ((rule_id : string), (r : Semgrep_output_v1_t.rule_result))
                -> (rule_id, r.passed)))
    |> List.sort (fun ((a : string), _) ((b : string), _) -> String.compare a b))

(* python: test.py pairs a .fixed file with its target whatever the rule file
   contains and runs the autofix pass, so a rule with no fix: leaves the
   target unchanged and the fixtest fails unless the fixed file is a copy of
   the target. *)
let test_fixtest_for_a_rule_without_a_fix (caps : Test_subcommand.caps) () =
  let check (name : string) (fixed : string) ~(passed : bool)
      ~(argv : string list) ~(check_exit : Exit_code.t -> unit) : unit =
    let files =
      [
        F.File ("eqeq.yaml", eqeq_rule_content);
        F.File ("eqeq.py", eqeq_target_content);
        F.File ("eqeq.fixed.py", fixed);
      ]
    in
    let exit_code, res = run_test_json caps files argv in
    check_exit exit_code;
    Alcotest.(check (list (pair string bool)))
      name
      [ ("eqeq.py", passed) ]
      (res.fixtest_results
      |> List_.map
           (fun
             ( (target : string),
               (r : Semgrep_output_v1_t.fixtest_result) )
           -> (target, r.passed)));
    Alcotest.(check (list string))
      (name ^ ": the rule file has a fixtest")
      []
      (List_.map Fpath.to_string res.config_missing_fixtests)
  in
  let config_argv = [ "--config"; "eqeq.yaml"; "eqeq.py" ] in
  check "a fixed file that differs from the target"
    "# ruleid: eqeq-is-bad\ny == y\n" ~passed:false ~argv:config_argv
    ~check_exit:Exit_code.Check.findings;
  check "a fixed file that is a copy of the target" eqeq_target_content
    ~passed:true ~argv:config_argv ~check_exit:Exit_code.Check.ok;
  (* the same pair found through the root '.', where the target of a rule
     file is reported without the './' prefix, as the rule file is *)
  check "a fixed file reached through the root '.'" eqeq_target_content
    ~passed:true ~argv:[ "." ] ~check_exit:Exit_code.Check.ok

(* python: a file with no 'ruleid:', 'todoruleid:' or 'ok:' line is not part
   of the comparison of annotated ids, so a match in it fails its check
   instead of stopping the run. *)
let test_match_in_an_unannotated_file (caps : Test_subcommand.caps) () =
  let files =
    [ F.File ("eqeq.yaml", eqeq_rule_content); F.File ("eqeq.py", "x == x\n") ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "eqeq.yaml"; "eqeq.py" ]
  in
  Exit_code.Check.findings exit_code;
  check_single_result "a match with no annotation" res ~passed:false
    ~expected_lines:[] ~reported_lines:[ 1 ]

(* python: test.py listed the target tree with rglob and paired a rule file
   with every file under a directory carrying its stem, at any depth. A rule
   file there is not a target, a .fixed file is not either. *)
let test_directory_named_like_the_rule_file (caps : Test_subcommand.caps) () =
  let files =
    [
      F.dir "rules"
        [
          F.File ("eqeq.yaml", eqeq_rule_content);
          F.dir "eqeq"
            [
              F.File ("case1.py", eqeq_target_content);
              (* neither of these two is a target of eqeq.yaml *)
              F.File ("case1.fixed.py", eqeq_target_content);
              F.File ("nested.yaml", other_rule_content);
              F.dir "sub" [ F.File ("case2.py", eqeq_target_content) ];
              (* sorts first by path and last by base name *)
              F.dir "a" [ F.File ("zz.py", eqeq_target_content) ];
            ];
        ];
    ]
  in
  let exit_code, res = run_test_json caps files [ "rules" ] in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string))
    "the rule file under the directory has no target of its own"
    [ "rules/eqeq/nested.yaml" ]
    (List_.map Fpath.to_string res.config_missing_tests);
  match res.results with
  | [ (_rule_file, { checks = [ (rule_id, rule_result) ] }) ] ->
      Alcotest.(check string) "the rule id" "eqeq-is-bad" rule_id;
      Alcotest.(check bool) "the verdict" true rule_result.passed;
      (* the files at the top of the directory and below it, reported in path
         order: 'eqeq/a/zz.py' before 'eqeq/case1.py' before
         'eqeq/sub/case2.py', which is not the order of their base names *)
      Alcotest.(check (list (pair string (list int))))
        "every file under the directory, in path order"
        [ ("zz.py", [ 2 ]); ("case1.py", [ 2 ]); ("case2.py", [ 2 ]) ]
        (rule_result.matches
        |> List_.map
             (fun
               ( (target : string),
                 (lines : Semgrep_output_v1_t.expected_reported) )
             ->
               (* the report gives absolute paths *)
               (Filename.basename target, lines.reported_lines)))
  | _ -> Alcotest.fail "expected one rule file with one check"

(* python: filter(None, ...) dropped the empty id a trailing comma leaves. *)
let test_trailing_comma_in_the_id_list (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File ("eqeq.py", "# ruleid: eqeq-is-bad,\nx == x\n");
    ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "eqeq.yaml"; "eqeq.py" ]
  in
  Exit_code.Check.ok exit_code;
  check_single_result "a trailing comma" res ~passed:true ~expected_lines:[ 2 ]
    ~reported_lines:[ 2 ]

(* python: test.py dropped a 'deepok'/'prook'/'deepruleid'/'proruleid' prefix
   from the id list and kept the kind of the line. *)
let test_deep_prefix_in_the_id_list (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File
        ( "eqeq.py",
          "# ruleid: eqeq-is-bad\nx == x\n# ruleid: deepok: eqeq-is-bad\ny == y\n"
        );
    ]
  in
  let exit_code, res =
    run_test_json caps files [ "--config"; "eqeq.yaml"; "eqeq.py" ]
  in
  Exit_code.Check.ok exit_code;
  check_single_result "a deepok: prefix" res ~passed:true
    ~expected_lines:[ 2; 4 ] ~reported_lines:[ 2; 4 ]

let eqeq_go_rule_content =
  {|
rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [go]
    severity: ERROR
|}

(* 'str ing' is not a type name, so the Go parser reports the span it could
   not read and the scan raises a PartialParsing error of severity Warning *)
let invalid_go_target_content = "package p\n\ntype T struct {\n\tS str ing\n}\n"

(* The rule matches nothing and the target carries no annotation, so the rule
   file produces no check and the error of the scan is attached to none. The
   run passes without --strict and fails with it: --strict on 'test' has the
   meaning it has for 'scan'.
   differs from the Python wrapper: it ignores an error of the scan under
   --strict *)
let test_strict_fails_on_a_scan_error (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_go_rule_content);
      F.File ("eqeq.go", invalid_go_target_content);
    ]
  in
  let exit_code, res = run_test_json caps files [ "." ] in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string))
    "the rule file produced no check" []
    (res.results
    |> List.concat_map
         (fun ((_rule_file : string), (checks : Semgrep_output_v1_t.checks)) ->
           checks.checks |> List_.map fst));
  let strict_exit_code, _res = run_test_json caps files [ "."; "--strict" ] in
  Exit_code.Check.findings strict_exit_code

(* a rule file listed under '.' is reported without the './' prefix *)
let test_rule_file_keys_of_a_dot_root (caps : Test_subcommand.caps) () =
  let files =
    [
      F.File ("eqeq.yaml", eqeq_rule_content);
      F.File ("eqeq.py", eqeq_target_content);
      F.File ("lonely.yaml", other_rule_content);
    ]
  in
  let exit_code, res = run_test_json caps files [ "." ] in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string))
    "the key of the tested rule file" [ "eqeq.yaml" ]
    (List_.map fst res.results);
  Alcotest.(check (list string))
    "the rule file without a target" [ "lonely.yaml" ]
    (List_.map Fpath.to_string res.config_missing_tests)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Test_subcommand.caps >) =
  Testo.categorize "Osemgrep Test (e2e)"
    (mk_matching_explanation_tests caps
    @ [
        t "missing test targets abort the run" (test_missing_targets caps);
        t "a rules directory and a targets directory pair by name"
          (test_rules_and_targets_directories caps);
        t "todoruleid: and todook: lines are left out of the comparison"
          (test_todo_annotations caps);
        t "a rule with two test targets" (test_two_targets_for_one_rule caps);
        t "a target whose extension no language claims"
          (test_target_with_unknown_extension caps);
        t "an annotation after code on the same line"
          (test_annotation_after_code caps);
        t "todook: and todoruleid: apply to their own rule id"
          (test_todo_annotations_are_per_rule_id caps);
        t "--opengrep-ignore-pattern hides a match"
          (test_custom_ignore_pattern caps);
        t "no target means the current directory"
          (test_no_target_means_current_directory caps);
        t "two target directories are refused"
          (test_two_target_directories caps);
        t "the rule file keys of a '.' root carry no './'"
          (test_rule_file_keys_of_a_dot_root caps);
        t "a rule that matches without an annotation fails the run"
          (test_rule_id_mismatch caps);
        t "a todook: for a rule that never matches is not a check of its own"
          (test_todook_for_a_rule_that_never_matches caps);
        t "a fixtest beside a rule file with no fix: is run"
          (test_fixtest_for_a_rule_without_a_fix caps);
        t "a match in a file with no annotation fails its check"
          (test_match_in_an_unannotated_file caps);
        t "a directory carrying the rule file's stem holds its test targets"
          (test_directory_named_like_the_rule_file caps);
        t "a trailing comma in an annotation adds no rule id"
          (test_trailing_comma_in_the_id_list caps);
        t "a deepok: prefix keeps the kind of the annotation"
          (test_deep_prefix_in_the_id_list caps);
        t "--strict fails the run on an error of the scan"
          (test_strict_fails_on_a_scan_error caps);
      ]
    @ mk_fixtest_tests caps
    @ mk_checks_tests caps)
