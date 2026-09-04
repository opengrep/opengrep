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
      t ~checked_output:(Testo.stdxxx ()) ~normalize test_name (fun () ->
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
                  ~checked_output:(Testo.stdxxx ()) ~normalize
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
    (* a rule file the loader rejects is reported in config_with_errors and
       the run goes on, so without --strict the exit code is 0 *)
    ("a rule file that does not load, JSON", "no_pattern.yaml", "no_pattern.py",
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
         t ("checks: " ^ name) ~checked_output:(Testo.stdxxx ()) ~normalize
           (run_checks caps ~rule ~target ~json)))
  @ [
      (* the rule never finishes, so --timeout is needed: the test subcommand
         runs with no time limit by default. The rule then reports nothing and
         the annotated lines are missed. python: test_timeout *)
      t "checks: a rule that times out, JSON" ~checked_output:(Testo.stdxxx ())
        ~normalize
        (run_checks caps ~extra_flags:[ "--timeout"; "1" ]
           ~check:Exit_code.Check.findings ~rule:"rule_that_timeout.yaml"
           ~target:"long.py" ~json:true);
      (* --strict makes the rule file the loader rejects fail the run *)
      t "checks: a rule file that does not load with --strict, text"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (run_checks caps ~extra_flags:[ "--strict" ]
           ~check:Exit_code.Check.findings ~rule:"no_pattern.yaml"
           ~target:"no_pattern.py" ~json:false);
    ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Test_subcommand.caps >) =
  Testo.categorize "Osemgrep Test (e2e)"
    (mk_matching_explanation_tests caps
    @ [ t "missing test targets abort the run" (test_missing_targets caps) ]
    @ mk_fixtest_tests caps
    @ mk_checks_tests caps)
