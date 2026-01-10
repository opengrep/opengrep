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
(* Testing end-to-end (e2e) the scan subcommand.
 *
 * Note that we already have lots of e2e pytest tests for the scan command, but
 * here we add a few tests using Testo and testing just osemgrep. Indeed,
 * in the past we had osemgrep regressions that could not be catched by our
 * pytests because many of those pytests are still marked as @osemfail and
 * so do not exercise osemgrep.
 *
 * This is similar to part of cli/tests/e2e/test_output.py
 * LATER: we should port all of test_output.py to Testo in this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* coupling: similar to cli/tests/.../rules/eqeq-basic.yaml *)
let eqeq_basic_content =
  {|
rules:
  - id: eqeq-bad
    patterns:
      - pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

let eqeq_with_max_match =
  {|
rules:
  - id: eqeq-bad
    options:
      max_match_per_file: 1
    patterns:
      - pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

(* coupling: similar to cli/tests/.../targets/basic/stupid.py *)
let stupid_py_content = {|
def foo(a, b):
    return a + b == a + b
|}

let stupid_py_content_ignore_pat = {|
def foo(a, b):
    # noopengrep
    return a + b == a + b
|}

let py_content_nosem = {|
def foo(a, b):
    # nosem
    return a + b == a + b

def also-foo(a, b):
    # noopengrep
    return a + b == a + b

def bar (a, b):
    return a + b == a + b
|}

let java_arg_paren_yaml_content =
  {|
rules:
  - id: function-param
    patterns:
      - pattern: foo($X);
    message: "argument is: $X"
    languages: [java]
    severity: ERROR
|}

let java_arg_paren_java_content = {|
public class A {
  public void f() {
    foo((2+3)*(3+4));
  }
}
|}

let three_findings_py_content = {|
def foo(a, b):
    f = a + b == a + b
    g = a + b == a + b
    return a + b == a + b
|}

let dummy_app_token = "FAKETESTINGAUTHTOKEN"

(* coupling: subset of cli/tests/conftest.py ALWAYS_MASK *)
let normalize =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_line ~after:"Opengrep version: " ();
    Testo.mask_pcre_pattern {|\{"version":"([^"]+)","results":\[|}
  ]

let without_settings f =
  Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml" f

(* Please run all tests with this to ensure reproducibility from one
   host to another. *)
let with_env_app_token ?(token = dummy_app_token) f =
  Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" token f

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_basic_output
    (caps : Scan_subcommand.caps)
    ?(rules_file = "rules.yml")
    ?(rules_content = eqeq_basic_content)
    ?(code_file = "stupid.py")
    ?(code_content = stupid_py_content)
    () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File (rules_file, rules_content);
          F.File (code_file, code_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; rules_file;
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_output_enclosing_context (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--output-enclosing-context";
                    "--json"
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_output_max_match (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("code.py", three_findings_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--max-match-per-file"; "1"
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_output_max_match_in_rule (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_with_max_match);
          F.File ("code.py", three_findings_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml"
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_output_ignore_pattern (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content_ignore_pat);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--opengrep-ignore-pattern"; "noopengrep";
                    "--json"
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_output_nosem_incremental (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", py_content_nosem);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--incremental-output"; "--incremental-output-postprocess";
                    "--json"
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_output_nosem_incremental_disabled (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", py_content_nosem);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--incremental-output"; "--incremental-output-postprocess";
                    "--disable-nosem"; "--json"
                  |])
          in
          Exit_code.Check.ok exit_code))

(* This test fails for me (Martin) when run alone with e.g.

     ./test -s "basic verbose output"

   In this case, it fails to print these two lines that it normally prints
   when run as part of the full test suite ('./test'):

     [<MASKED TIMESTAMP>][INFO]: Running external command: 'git' 'ls-remote' '--get-url'
     [<MASKED TIMESTAMP>][INFO]: error output: fatal: No remote configured to list refs from.

   TODO: figure out why and fix it
*)
let test_basic_verbose_output (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan";
                    "--experimental";
                    "--config";
                    "rules.yml";
                    "--verbose";
                  |])
          in
          Exit_code.Check.ok exit_code))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan (e2e)"
    [
      t "basic output" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output caps);
      t "basic output with --output-enclosing-context" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output_enclosing_context caps);
      t "basic output with --opengrep-ignore-pattern" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output_ignore_pattern caps);
      t "incremental output with --incremental-output-postprocess"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output_nosem_incremental caps);
      t "incremental output with --incremental-output-postprocess and --disable-nosem"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output_nosem_incremental_disabled caps);
      t "basic verbose output"
        ~skipped:"captured output depends on which tests run before it"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_verbose_output caps);
      t "precise range for parenthesized expression" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output caps
           ~rules_file:"java_arg_paren.yaml"
           ~rules_content:java_arg_paren_yaml_content
           ~code_file:"java_arg_paren.java"
           ~code_content:java_arg_paren_java_content);
      t "basic output with --max-match-per-file" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output_max_match caps);
      t "basic output with max-match-per-file rule option" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_basic_output_max_match_in_rule caps);
    ]
