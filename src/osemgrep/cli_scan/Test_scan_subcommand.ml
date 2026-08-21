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

(* The sink is reached only through a call from another file, so this finding
   comes out of interfile dispatch rather than a per-target scan. *)
let taint_interfile_content =
  {|
rules:
  - id: interfile-taint
    mode: taint
    options:
      taint_interfile: true
    pattern-sources:
      - pattern: source()
    pattern-sinks:
      - pattern: sink(...)
    message: "cross-file taint"
    languages: [python]
    severity: ERROR
|}

let interfile_caller_py_content = {|
from sinks import leak

def go():
    leak(source())
|}

let interfile_sink_py_content = {|
def leak(v):
    sink(v)
|}

(* Two call sites feeding the same sink: distinct sources, one sink line. *)
let interfile_two_sources_py_content = {|
from sinks import leak

def go():
    leak(source())

def go2():
    leak(source())
|}

(* Same taint rule but without the [taint_interfile] option: findings dedup
   on the sink alone. *)
let taint_intrafile_content =
  {|
rules:
  - id: intrafile-taint
    mode: taint
    pattern-sources:
      - pattern: source()
    pattern-sinks:
      - pattern: sink(...)
    message: "intrafile taint"
    languages: [python]
    severity: ERROR
|}

(* The interfile two-source shape folded into a single file. *)
let intrafile_two_sources_py_content = {|
def leak(v):
    sink(v)

def go():
    leak(source())

def go2():
    leak(source())
|}

(* The baseline revision of the helper: same signature, no sink yet. *)
let interfile_sink_py_baseline_content = {|
def leak(v):
    pass
|}

(* Cosmetically edited helper: the sink is untouched and stays on the same
   line, so any finding it carries is pre-existing rather than introduced. *)
let interfile_sink_py_touched_content = {|
def leak(v):
    sink(v)

# unrelated trailing comment added by this commit
|}

(* coupling: similar to cli/tests/.../targets/basic/stupid.py *)
let stupid_py_content = {|
def foo(a, b):
    return a + b == a + b
|}

let stupid_py_content_ignore_pat = {|
def foo(a, b):
    # nosem
    return a + b == a + b

    # noopengrep
    return a + b == a + b

    # nosec
    return a + b == a + b

    # nosec2
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

let terraform_aws_lb_yaml_content =
  {|
rules:
  - id: aws-lb-block
    pattern: |
      resource "aws_lb" $NAME {
        ...
      }
    message: "aws_lb block"
    languages: [terraform]
    severity: WARNING
|}

(* A 'resource' block that is never closed: the file ends mid-block. *)
let truncated_terraform_content =
  {|resource "aws_lb" "x" {
  internal = false
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

(* [Testutil_git.mask_temp_git_hash] only masks the root commit's line.  A test
   that commits a second time prints [[main <short hash>] msg], whose hash
   varies per run and would otherwise make the snapshot unstable. *)
let normalize_multi_commit =
  normalize @ [ Testo.mask_line ~after:"[main " ~before:"]" () ]

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
                    "--opengrep-ignore-pattern"; "(nosec|nosec2)";
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

(* A truncated Terraform file whose 'resource' block is never closed. The
   parser's error recovery inserts a synthetic '}' at EOF, so a rule matching
   the block gets an end position one line past the file's content. Reading the
   lines around the match (for nosemgrep suppression and for the output) must
   not raise and abort the whole scan. *)
let test_truncated_terraform_block (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", terraform_aws_lb_yaml_content);
          F.File ("main.tf", truncated_terraform_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                  |])
          in
          Exit_code.Check.ok exit_code))

(* [--incremental-output] suppresses the final render on the assumption that
   every finding was already streamed per file.  Interfile dispatch produces its
   matches outside the per-target path, so unless it streams them too they are
   counted in the summary and the exit code but never printed. *)
let test_interfile_incremental_output (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_interfile_content);
          F.File ("main.py", interfile_caller_py_content);
          F.File ("sinks.py", interfile_sink_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--taint-interfile"; "--incremental-output";
                  |])
          in
          Exit_code.Check.ok exit_code))

(* Sources and sinks are extracted only over the scan's target files, so a
   partial scan — one file here, but equally a diff scan or a CI changed-files
   run — sees just one side of the flow.  Scanning only the sink file must
   still follow the call back into the untargeted companion that supplies the
   taint, rather than concluding from the empty source list that no flow
   exists. *)
let test_interfile_partial_target (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_interfile_content);
          F.File ("main.py", interfile_caller_py_content);
          F.File ("sinks.py", interfile_sink_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--taint-interfile"; "sinks.py";
                  |])
          in
          Exit_code.Check.ok exit_code))

(* Two distinct sources reach the same sink line: interfile dedup keys on
   source+sink, so both findings must be reported.  [--dataflow-traces] prints
   each finding's trace, pinning in the snapshot that the sources differ. *)
let test_interfile_source_sink_dedup (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_interfile_content);
          F.File ("main.py", interfile_two_sources_py_content);
          F.File ("sinks.py", interfile_sink_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--taint-interfile"; "--dataflow-traces";
                  |])
          in
          Exit_code.Check.ok exit_code))

(* The intrafile counterpart of the test above: without interfile, the dedup
   key omits the source, so the same two flows collapse into one finding. *)
let test_intrafile_same_sink_dedup (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_intrafile_content);
          F.File ("code.py", intrafile_two_sources_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--taint-intrafile"; "--dataflow-traces";
                  |])
          in
          Exit_code.Check.ok exit_code))

(* The CI shape of a partial scan: a commit adds a sink to a helper that
   pre-existing, untouched code already feeds a source into.  Only the helper
   is added-or-modified, so the caller carrying the source is not a target and
   the flow is only found if companions are analysed.  The finding itself lands
   in the changed file, so it does belong in a diff scan's output. *)
let test_interfile_diff_scan (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_interfile_content);
          F.File ("main.py", interfile_caller_py_content);
          F.File ("sinks.py", interfile_sink_py_baseline_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          (* Second commit: introduce the sink, leaving main.py untouched. *)
          UFile.write_file ~file:(Fpath.v "sinks.py") interfile_sink_py_content;
          Git_wrapper.add [ Fpath.v "." ];
          Git_wrapper.commit "Add the sink";
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--taint-interfile"; "--baseline-commit"; "HEAD~1";
                  |])
          in
          Exit_code.Check.ok exit_code))

(* The reverse of the diff-scan test above: the cross-file flow already exists
   in the baseline and this commit only appends a comment, so the diff must
   report NOTHING.  The baseline replay has to rescan enough files to reproduce
   an interfile finding — replaying only the files that carry a match leaves out
   the caller supplying the taint, so the baseline comes up empty and a
   pre-existing finding is misreported as newly introduced. *)
let test_interfile_diff_scan_preexisting (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_interfile_content);
          F.File ("main.py", interfile_caller_py_content);
          F.File ("sinks.py", interfile_sink_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          UFile.write_file ~file:(Fpath.v "sinks.py")
            interfile_sink_py_touched_content;
          Git_wrapper.add [ Fpath.v "." ];
          Git_wrapper.commit "Touch the sink file";
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "opengrep-scan"; "--experimental"; "--config"; "rules.yml";
                    "--taint-interfile"; "--baseline-commit"; "HEAD~1";
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
      t "interfile pre-existing findings are not reported by a diff scan"
        ~checked_output:(Testo.stdxxx ()) ~normalize:normalize_multi_commit
        (test_interfile_diff_scan_preexisting caps);
      t "interfile findings in a diff scan"
        ~checked_output:(Testo.stdxxx ()) ~normalize:normalize_multi_commit
        (test_interfile_diff_scan caps);
      t "interfile findings from a partial target set"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_interfile_partial_target caps);
      t "interfile same-sink findings keep distinct sources"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_interfile_source_sink_dedup caps);
      t "intrafile same-sink findings dedup to one"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_intrafile_same_sink_dedup caps);
      t "interfile findings with --incremental-output"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_interfile_incremental_output caps);
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
      t "truncated terraform block does not abort scan"
        (test_truncated_terraform_block caps);
    ]
