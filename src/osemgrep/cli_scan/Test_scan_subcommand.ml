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
open Common

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

(* Caller at the repo ROOT, importing the helper from the app/ package: the
   companion supplying the taint sits outside the directory the scan is
   launched from in the subdirectory tests. *)
let interfile_caller_pkg_py_content = {|
from app.sinks import leak

def go():
    leak(source())
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
    Test_scan_helpers.mask_test_temp_paths ();
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

let with_no_color f = Semgrep_envvars.with_envvar "NO_COLOR" "1" f

(* The basic targets of the fixtures, scanned as a directory: files of
   several languages, without extension, binary, and a README. *)
let basic_targets_dir : F.t =
  F.dir "basic" (F.read Fpath.(Test_scan_helpers.fixtures_root / "targets" / "basic"))

(* Two rules under rules/two_rules, so that their ids take the same prefix
   from the config path as in the Python test. *)
let two_rules_dir : F.t =
  F.dir "rules"
    [
      F.dir "two_rules"
        (F.read Fpath.(Test_scan_helpers.fixtures_root / "rules" / "two_rules"));
    ]

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_basic_output
    (caps : Scan_subcommand.caps)
    ?(rules_file = "rules.yml")
    ?(rules_content = eqeq_basic_content)
    ?(code_file = "stupid.py")
    ?(code_content = stupid_py_content)
    ?(extra_args : string list = [])
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
                  (Array.of_list
                     ([ "opengrep-scan"; "--experimental"; "--config"; rules_file ]
                     @ extra_args)))
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

(* A target counts as scanned when a rule applies to it, whether the rule
   runs per target or through interfile dispatch; the JSON paths.scanned
   and the byte counts of --time must be those of a plain scan. *)
let test_interfile_paths_scanned (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", taint_interfile_content);
          F.File ("main.py", interfile_caller_py_content);
          F.File ("sinks.py", interfile_sink_py_content);
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--json"; "--time";
                        "--config"; "rules.yml"; "--taint-interfile";
                      |]))
          in
          Exit_code.Check.ok exit_code;
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          Alcotest.(check int) "one interfile finding" 1 (List.length out.results);
          Alcotest.(check (list string))
            "both targets scanned" [ "main.py"; "sinks.py" ]
            (out.paths.scanned |> List_.map Fpath.to_string |> List_.sort);
          let (time : Semgrep_output_v1_t.profile) =
            match out.time with
            | Some time -> time
            | None -> Alcotest.fail "no --time profile in the output"
          in
          time.targets
          |> List.iter (fun (t : Semgrep_output_v1_t.target_times) ->
                 Alcotest.(check bool)
                   (spf "%s has a byte count" (Fpath.to_string t.path))
                   true (t.num_bytes > 0))))

(* The process-wide [--max-memory] bounds interfile dispatch too: exceeding
   it aborts the analysis of a rule, reported as an out-of-memory error
   against that rule. *)
let test_interfile_limits (caps : Scan_subcommand.caps) () =
  let scan (args : string list) : Semgrep_output_v1_t.cli_output * Exit_code.t =
    with_env_app_token (fun () ->
        let repo_files =
          [
            F.File ("rules.yml", taint_interfile_content);
            F.File ("main.py", interfile_caller_py_content);
            F.File ("sinks.py", interfile_sink_py_content);
          ]
        in
        Testutil_git.with_git_repo repo_files (fun _cwd ->
            let exit_code, stdout_output =
              Testo.with_capture stdout (fun () ->
                  without_settings (fun () ->
                      Scan_subcommand.main caps
                        (Array.of_list
                           ([
                              "opengrep-scan"; "--experimental"; "--json";
                              "--config"; "rules.yml"; "--taint-interfile";
                            ]
                           @ args))))
            in
            (Semgrep_output_v1_j.cli_output_of_string stdout_output, exit_code)))
  in
  let out, exit_code = scan [ "--max-memory"; "1" ] in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list (pair string (option string))))
    "the memory limit is reported against the rule"
    [ ("Out of memory", Some "interfile-taint") ]
    (out.errors
    |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
           ( Error.string_of_error_type e.type_,
             Option.map Rule_ID.to_string e.rule_id )))

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

(* A plain interfile scan launched from a repo SUBDIRECTORY: the index is
   built from the project root discovered by walking up from the targets, so
   the caller outside the launch directory must still be indexed and supply
   the taint.  Discovery used to return cwd-relative paths that consumers
   resolved against the project root — the two agree only when the scan runs
   from the root, so from a subdirectory every file failed to read and the
   index came out empty, silently downgrading the rule to intrafile. *)
let test_interfile_scan_from_subdir (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("main.py", interfile_caller_pkg_py_content);
          F.Dir
            ( "app",
              [
                F.File ("rules.yml", taint_interfile_content);
                F.File ("sinks.py", interfile_sink_py_content);
              ] );
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            F.with_chdir (Fpath.v "app") (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--config";
                        "rules.yml"; "--taint-interfile";
                      |]))
          in
          Exit_code.Check.ok exit_code))

(* The diff-scan shape of the test above: the flow pre-exists and this commit
   only appends a comment, so the diff must report NOTHING.  The baseline
   replay must rediscover targets and project roots inside its worktree the
   same way the head scan discovered them in the real checkout — replaying
   bare paths with the launch cwd as the root builds the baseline graph
   without the out-of-subdirectory caller, fails to reproduce the
   pre-existing finding, and misreports it as newly introduced. *)
let test_interfile_diff_scan_subdir (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("main.py", interfile_caller_pkg_py_content);
          F.Dir
            ( "app",
              [
                F.File ("rules.yml", taint_interfile_content);
                F.File ("sinks.py", interfile_sink_py_content);
              ] );
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          UFile.write_file
            ~file:(Fpath.v "app/sinks.py")
            interfile_sink_py_touched_content;
          Git_wrapper.add [ Fpath.v "." ];
          Git_wrapper.commit "Touch the sink file";
          let exit_code =
            F.with_chdir (Fpath.v "app") (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--config";
                        "rules.yml"; "--taint-interfile"; "--baseline-commit";
                        "HEAD~1";
                      |]))
          in
          Exit_code.Check.ok exit_code))

(*****************************************************************************)
(* Fingerprints *)
(*****************************************************************************)
(* The match-based ids below are the ones the Python wrapper computed for
 * these findings (cli/tests/default/e2e fixtures). The rule id and the
 * relative path are part of the hash, so the fixture layout is reproduced.
 * See Formula_string.ml. *)

let metavariable_regex_rule_content =
  {|
rules:
  - id: metavar-test
    patterns:
      - pattern: "metavariable_regex_test($X)"
      - metavariable-regex:
          metavariable: "$X"
          regex: '("test"|"example")'
    message: "Metavariable regex test"
    languages: [python]
    severity: ERROR
|}

let metavariable_regex_py_content =
  {|metavariable_regex_test("test")
metavariable_regex_test("example")
metavariable_regex_test(7)
|}

let taint_labels_rule_content =
  {|
rules:
  - id: taint-trace
    message: found an error
    languages:
      - cpp
      - c
    severity: WARNING
    mode: taint
    pattern-sources:
      - label: USER_CONTROLLED
        patterns:
          - pattern: SOURCE()
      - label: SCALAR
        requires: USER_CONTROLLED
        patterns:
          - pattern-either:
              - pattern: $LHS + $RHS
          - focus-metavariable:
              - $RHS
              - $LHS
    pattern-sinks:
      - requires: USER_CONTROLLED and SCALAR
        patterns:
          - pattern-either:
              - pattern: SINK(<... $SRC ...>)
          - focus-metavariable: $SRC
|}

let taint_labels_cpp_content =
  {|void foo() {
  void* curBase;
  size_t curLen;

  while (curIov < count) {
    ssize_t res1 = 0;
    ssize_t res2 = 0;

    if (isRead) {
      res1 = SOURCE();
    } else {
      res2 = pass_the_taint(curBase);
    }

    curBase = (void*)((char*)curBase + res1);

    SINK(res2);
  }
}
|}

let eqeq_is_bad_rule_content =
  {|
rules:
  - id: eqeq-is-bad
    patterns:
      - pattern-not-inside: |
          def __eq__(...):
              ...
      - pattern-not-inside: assert(...)
      - pattern-not-inside: assertTrue(...)
      - pattern-not-inside: assertFalse(...)
      - pattern-either:
          - pattern: $X == $X
          - pattern: $X != $X
          - patterns:
              - pattern-inside: |
                  def __init__(...):
                      ...
              - pattern: self.$X == self.$X
      - pattern-not: 1 == 1
    message: "useless comparison operation `$X == $X` or `$X != $X`"
    languages: [python]
    severity: ERROR
|}

let cwe_tag_rule_content =
  {|
rules:
  - id: rule-with-cwe-tag
    pattern: $X + $Y
    message: Fake message.
    metadata:
      cwe:
        - "CWE-99999999: Fake CWE"
        - "CWE-99999999: Fake CWE"
        - "CWE-88888888: Another fake CWE"
    languages:
      - python
    severity: ERROR
|}

(* two rules with the same pattern: the index suffix counts per rule and
 * per file *)
let duplicates_rule_content =
  {|
rules:
  - id: eqeq-bad
    patterns:
      - pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
  - id: eqeq-bad-dup
    patterns:
      - pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

let duplicates_py_content =
  {|if 5 == 5:
    print("True!")

if 5 == 5:
    print("False!")
|}

(* (path, line, fingerprint) of the findings of a --json scan of the
 * targets, sorted *)
let fingerprints_of_scan (caps : Scan_subcommand.caps)
    ?(config : string option) ~(rule_path : string) ~(rule : string)
    ~(targets : (string * string) list) () : (string * int * string) list =
  (* the spelling of --config, which the rule ids and so the fingerprints
     depend on; the rule file itself is always written at rule_path *)
  let config : string = Option.value config ~default:rule_path in
  let rec tree (path : string list) (contents : string) : F.t =
    match path with
    | [ file ] -> F.File (file, contents)
    | dir :: rest -> F.Dir (dir, [ tree rest contents ])
    | [] -> assert false
  in
  let repo_files =
    tree (String.split_on_char '/' rule_path) rule
    :: List_.map
         (fun ((path : string), (contents : string)) ->
           tree (String.split_on_char '/' path) contents)
         targets
  in
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let (), stdout_output =
            Testo.with_capture stdout (fun () ->
                let exit_code =
                  without_settings (fun () ->
                      Scan_subcommand.main caps
                        (Array.of_list
                           ([
                              "opengrep-scan";
                              "--experimental";
                              "--json";
                              "--config";
                              config;
                            ]
                           @ List_.map fst targets)))
                in
                Exit_code.Check.ok exit_code)
          in
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          out.results
          |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) ->
                 (Fpath.to_string m.path, m.start.line, m.extra.fingerprint))
          |> List.sort compare))

let test_fingerprints (caps : Scan_subcommand.caps) () =
  let check name ?config expected ~rule_path ~rule ~targets =
    Alcotest.(check (list (triple string int string)))
      name expected
      (fingerprints_of_scan caps ?config ~rule_path ~rule ~targets ())
  in
  check "search rule" ~rule_path:"rules/eqeq.yaml" ~rule:eqeq_is_bad_rule_content
    ~targets:[ ("targets/basic/stupid.py", stupid_py_content) ]
    [
      ( "targets/basic/stupid.py",
        3,
        "62b4a09c4569768898c43c09fa0a5b95b7e93257ef3a0911a5c379b6265b4d49fa4aecd5782461632e9aef4779af02d7cad4405b9a5318a0e5ffe9a5bd8daeae_0"
      );
    ];
  (* the './' of a config path is not a directory of the rule id, so the
     fingerprint is the one the plain spelling gives; python:
     convert_config_id_to_prefix strips the leading '.' and '/' *)
  check "config path spelled with './'" ~config:"./rules/eqeq.yaml"
    ~rule_path:"rules/eqeq.yaml" ~rule:eqeq_is_bad_rule_content
    ~targets:[ ("targets/basic/stupid.py", stupid_py_content) ]
    [
      ( "targets/basic/stupid.py",
        3,
        "62b4a09c4569768898c43c09fa0a5b95b7e93257ef3a0911a5c379b6265b4d49fa4aecd5782461632e9aef4779af02d7cad4405b9a5318a0e5ffe9a5bd8daeae_0"
      );
    ];
  check "two matches of one rule on one line" ~rule_path:"rules/cwe_tag.yaml"
    ~rule:cwe_tag_rule_content
    ~targets:[ ("targets/basic/stupid.py", stupid_py_content) ]
    [
      ( "targets/basic/stupid.py",
        3,
        "c14751d4f27243ab3fc24c8e9993358c58673b1f8cb907fc90f66a33826c9183a0386cc160cf4457cf7e175c9efb083d250a3b52fe3154a003fc9b9955802118_0"
      );
      ( "targets/basic/stupid.py",
        3,
        "c14751d4f27243ab3fc24c8e9993358c58673b1f8cb907fc90f66a33826c9183a0386cc160cf4457cf7e175c9efb083d250a3b52fe3154a003fc9b9955802118_1"
      );
    ];
  check "metavariable-regex condition"
    ~rule_path:"rules/metavariable-regex/metavariable-regex.yaml"
    ~rule:metavariable_regex_rule_content
    ~targets:
      [ ("targets/basic/metavariable-regex.py", metavariable_regex_py_content) ]
    [
      ( "targets/basic/metavariable-regex.py",
        1,
        "6c1d60637645bb7356297820f59fc963e19fdcb7f1da555aedcf2a1c3bd5e7f9c867e98545763e8296b507e40e8bb6a606e19e1c885e95b15b688355e8c5be22_0"
      );
      ( "targets/basic/metavariable-regex.py",
        2,
        "603d26223512a912db15b1640ce11cb5d1f9de820739bcfe80e6374ca148282f14fab86f0d6a92f8d62817739c3ccc2be025d884abea7ba8c8b37f9633cfa14d_0"
      );
    ];
  check "taint with labels" ~rule_path:"rules/taint_trace.yaml"
    ~rule:taint_labels_rule_content
    ~targets:[ ("targets/taint/taint_trace.cpp", taint_labels_cpp_content) ]
    [
      ( "targets/taint/taint_trace.cpp",
        17,
        "c103a760f6ce7176c2d5127a8c5afa47e83d41bef95586322d99febf0f148c2808bc6b168b31948e8fd10576f7f4edd69fb6d341c25ed9c259cde4e9164d7b96_0"
      );
    ];
  check "duplicate matches indexed per rule and file"
    ~rule_path:"rules/match_based_id/duplicates.yaml"
    ~rule:duplicates_rule_content
    ~targets:
      [
        ("targets/match_based_id/duplicates/duplicate1.py", duplicates_py_content);
        ("targets/match_based_id/duplicates/duplicate2.py", duplicates_py_content);
      ]
    [
      ( "targets/match_based_id/duplicates/duplicate1.py",
        1,
        "35c3c5253d3c06bda40ad1ba791b92585b9e5de07d9118cc4301f4e8cb955a1d6e510fe59341c04c6a8fb964918e1909590e37bd76511b7705d604438be156ca_0"
      );
      ( "targets/match_based_id/duplicates/duplicate1.py",
        1,
        "f0ff0732807b02ac1b1c69b85f9530b42e70ee5b9dcd8caab9c9238bb0c0768bf0a638f45379b4ce9d37ab209c712ee2ae1585ecf0bc2ea77b5fa55cce7371ec_0"
      );
      ( "targets/match_based_id/duplicates/duplicate1.py",
        4,
        "35c3c5253d3c06bda40ad1ba791b92585b9e5de07d9118cc4301f4e8cb955a1d6e510fe59341c04c6a8fb964918e1909590e37bd76511b7705d604438be156ca_1"
      );
      ( "targets/match_based_id/duplicates/duplicate1.py",
        4,
        "f0ff0732807b02ac1b1c69b85f9530b42e70ee5b9dcd8caab9c9238bb0c0768bf0a638f45379b4ce9d37ab209c712ee2ae1585ecf0bc2ea77b5fa55cce7371ec_1"
      );
      ( "targets/match_based_id/duplicates/duplicate2.py",
        1,
        "76ef6b1c1d5e528a4653a7700d0f8d3c6096fac1e5ae06cae9546f6eb4b3db95a9221562cb7ad517f2d5936284f6bc1439a4e1d4b4831820a946d5a6e618cb2e_0"
      );
      ( "targets/match_based_id/duplicates/duplicate2.py",
        1,
        "d3d094efc363029d2993f4b046b759137f3eb9fd4e950c3bc62256f104919f35ea8e7e07412c9514d682f0d83f84d40cd19fc3aa98c278dff16152a0d851d7fb_0"
      );
      ( "targets/match_based_id/duplicates/duplicate2.py",
        4,
        "76ef6b1c1d5e528a4653a7700d0f8d3c6096fac1e5ae06cae9546f6eb4b3db95a9221562cb7ad517f2d5936284f6bc1439a4e1d4b4831820a946d5a6e618cb2e_1"
      );
      ( "targets/match_based_id/duplicates/duplicate2.py",
        4,
        "d3d094efc363029d2993f4b046b759137f3eb9fd4e950c3bc62256f104919f35ea8e7e07412c9514d682f0d83f84d40cd19fc3aa98c278dff16152a0d851d7fb_1"
      );
    ]

(* Match-based ids survive changes of formatting and of the code an
 * ellipsis spans, and change with the matched code and the metavariable
 * contents. Both versions of a target are scanned at the same path, as the
 * path is part of the id. *)

let formatting_change_rule_content =
  {|
rules:
  - id: formatting-change
    patterns:
      - pattern: $X = 1+1;... $X = 2+2;
    message: "useless comparison"
    languages: [c]
    severity: ERROR
|}

let operator_change_rule_content =
  {|
rules:
  - id: operator-change
    pattern-either:
      - pattern: $Y = 1+1
      - pattern: $X = 2+2
    message: "useless"
    languages: [c]
    severity: ERROR
|}

let classic_taint_rule_content =
  {|
rules:
  - id: classic
    mode: taint
    pattern-sources:
      - pattern: source(...)
      - pattern: source1(...)
    pattern-sinks:
      - pattern: sink(...)
      - pattern: sink1(...)
      - pattern: eval(...)
    pattern-sanitizers:
      - pattern: sanitize(...)
      - pattern: sanitize1(...)
    message: A user input source() went into a dangerous sink()
    languages: [python, javascript]
    severity: WARNING
|}

(* (name, rule, target extension, before, after, the id changes) *)
let id_change_cases =
  [
    ( "formatting",
      formatting_change_rule_content,
      "c",
      "int main(){ int x = 0; x = 1+1; x = 2+2; }\n",
      "int main() {\n  int x = 0;\n  x = 1 + 1;\n  x = 2 + 2;\n}\n",
      false );
    ( "code spanned by an ellipsis",
      formatting_change_rule_content,
      "c",
      {|int main() {
  int x = 0;
  x = 1 + 1;
  int useless_var = 1;
  if(1==1){
    //...
  }
  x = 2 + 2;
}
|},
      {|int main() {
  int x = 0;
  x = 1 + 1;
  int useless_var = 1;
  if(1==1){
    //...
  }
  if (1 == 1) {
    if (1 == 1) {
      //...
      if (1 == 1) {
        //...
      }
    }
    //...
  }
  x = 2 + 2;
}
|},
      false );
    ( "other source and sink of the same taint rule",
      classic_taint_rule_content,
      "py",
      "a = source()\nb = a\nsink(b)\n",
      "a = source1()\nb = a\nsink1(b)\n",
      false );
    ( "other pattern of a pattern-either",
      operator_change_rule_content,
      "c",
      "int main() {\n  int x = 0;\n  x = 1 + 1;\n}\n",
      "int main() {\n  int x = 0;\n  x = 2 + 2;\n}\n",
      true );
    ( "metavariable content",
      formatting_change_rule_content,
      "c",
      "int main() {\n  int x = 0;\n  x = 1 + 1;\n  x = 2 + 2;\n}\n",
      "int main() {\n  int y = 0;\n  y = 1 + 1;\n  y = 2 + 2;\n}\n",
      true );
  ]

let test_id_change (caps : Scan_subcommand.caps) () =
  id_change_cases
  |> List.iter (fun (name, rule, ext, before, after, changes) ->
         let fingerprint (target : string) : string =
           match
             fingerprints_of_scan caps ~rule_path:"rules.yaml" ~rule
               ~targets:[ ("targets/match_based_id." ^ ext, target) ]
               ()
           with
           | [ (_, _, fingerprint) ] -> fingerprint
           | _ -> failwith (name ^ ": expected exactly one finding")
         in
         Alcotest.(check bool)
           name changes
           (not (String.equal (fingerprint before) (fingerprint after))))

(*****************************************************************************)
(* Text output *)
(*****************************************************************************)

let two_rules_same_message_content =
  {|
rules:
  - id: rule1
    pattern: print(...)
    message: Same message as other rule.
    languages: [python]
    severity: ERROR
  - id: rule2
    pattern: print(...)
    message: Same message as other rule.
    languages: [python]
    severity: ERROR
|}

let autofix_rules_content =
  {|
rules:
  - id: use-dict-get
    patterns:
      - pattern: $DICT[$KEY]
    fix: |
      $DICT.get(
        $KEY)
    message: Use `.get()` method to avoid a KeyNotFound error
    languages: [python]
    severity: ERROR
  - id: no-debug
    pattern: debug(...)
    fix: ""
    message: Remove the debug call
    languages: [python]
    severity: WARNING
|}

let autofix_py_content = {|inputs = {}
x = inputs["key"]
debug(x)
|}

(* longer than the width the text output wraps at *)
let long_word = String.make 70 'a'

let long_rule_id_content =
  spf
    {|
rules:
  - id: rule-%s
    pattern: $X == $X
    message: A message with a long word %s in it.
    severity: WARNING
    languages: [python]
|}
    long_word long_word

let long_line_py_content =
  spf {|print("id = 1 AND %s = TRUE" == "id = 1 AND %s = TRUE")
|} long_word
    long_word

(* the times of the --time summary *)
let mask_times =
  Testo.mask_pcre_pattern ~replace:(fun _ -> "x.xxx") {|[0-9]+\.[0-9]+|}

(* --time: the JSON carries the times of the command and of the rule
   parsing next to the engine's *)
let test_time_json (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--json"; "--time";
                        "--config"; "rules.yml";
                      |]))
          in
          Exit_code.Check.ok exit_code;
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          match out.time with
          | None -> Alcotest.fail "no time field"
          | Some time ->
              Alcotest.(check (list string))
                "the times of the command"
                [ "config_time"; "core_time"; "ignores_time"; "total_time" ]
                (List_.map fst time.profiling_times);
              Alcotest.(check bool)
                "the rules were parsed" true
                (time.rules_parse_time > 0.0);
              match time.targets with
              | [ target ] ->
                  Alcotest.(check string)
                    "the target" "stupid.py"
                    (Fpath.to_string target.path);
                  Alcotest.(check (list string))
                    "the rule that ran on it, with its matching time"
                    [ "eqeq-bad" ]
                    (target.match_times
                    |> List_.filter_map
                         (fun ((id : Rule_ID.t), (match_time : float)) ->
                           if match_time > 0.0 then Some (Rule_ID.to_string id)
                           else None));
                  Alcotest.(check bool)
                    "the target was parsed" true (target.parse_time > 0.0);
                  Alcotest.(check bool)
                    "the run covers parsing and matching" true
                    (target.run_time >= target.parse_time)
              | targets ->
                  Alcotest.fail
                    (spf "expected one target, got %d" (List.length targets))))

(* --time reports the times of a scan that found nothing, as the Python
 * wrapper did: the field is about the run, not about the findings. *)
let test_time_json_no_findings (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("nothing.py", "x = 1\n");
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--json"; "--time";
                        "--config"; "rules.yml";
                      |]))
          in
          Exit_code.Check.ok exit_code;
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          Alcotest.(check int) "no finding" 0 (List.length out.results);
          match out.time with
          | None -> Alcotest.fail "no time field"
          | Some time ->
              Alcotest.(check (list string))
                "the times of the command"
                [ "config_time"; "core_time"; "ignores_time"; "total_time" ]
                (List_.map fst time.profiling_times)))

(*****************************************************************************)
(* Text taken from a target that is not UTF-8 *)
(*****************************************************************************)
(* Nothing makes a target UTF-8, but every output format has to be: the
 * bytes that cannot be decoded become U+FFFD, as they did in pysemgrep,
 * which read its targets with errors="replace".
 *)
let latin1_rule_content =
  {|
rules:
  - id: find-eval
    pattern: eval($X)
    fix: safe_eval($X)
    message: found
    languages: [python]
    severity: WARNING
|}

(* 0xff, 0xfe and 0xe9 are not valid UTF-8; each becomes one U+FFFD *)
let latin1_py_content : string = "eval(\"\xff\xfe caf\xe9\") # bad\n"
let replacement : string = "\xef\xbf\xbd"

let test_invalid_utf8 (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", latin1_rule_content);
          F.File ("badline.py", latin1_py_content);
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--json";
                        "--config"; "rules.yml"; "badline.py";
                      |]))
          in
          Exit_code.Check.ok exit_code;
          Alcotest.(check string)
            "the output is valid UTF-8" stdout_output
            (Utf8.sanitize stdout_output);
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          match out.results with
          | [ m ] ->
              Alcotest.(check string)
                "the matched line"
                (spf "eval(\"%s%s caf%s\") # bad" replacement replacement
                   replacement)
                m.extra.lines;
              Alcotest.(check (option string))
                "the autofix"
                (Some
                   (spf "safe_eval(\"%s%s caf%s\")" replacement replacement
                      replacement))
                m.extra.fix
          | results ->
              Alcotest.fail
                (spf "expected one finding, got %d" (List.length results))))

(* An error entry quotes text of the target too: a parse error names what it
 * choked on, and the whole document is undecodable when those bytes reach
 * it raw. *)
let latin1_syntax_error_content : string = "x = '\xff\xfe'\nbad_\xff == bad_\xff\n"

let test_invalid_utf8_in_error (caps : Scan_subcommand.caps) () =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", latin1_rule_content);
          F.File ("badsyntax.py", latin1_syntax_error_content);
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let (_ : Exit_code.t), stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan"; "--experimental"; "--json";
                        "--config"; "rules.yml"; "badsyntax.py";
                      |]))
          in
          Alcotest.(check string)
            "the output is valid UTF-8" stdout_output
            (Utf8.sanitize stdout_output);
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          match out.errors with
          | [ e ] ->
              Alcotest.(check bool)
                "the parse error quotes replacement characters" true
                (String_.contains
                   ~term:(spf "bad_%s == bad_%s" replacement replacement)
                   (Option.value ~default:"" e.message))
          | errors ->
              Alcotest.fail
                (spf "expected one error, got %d" (List.length errors))))

(*****************************************************************************)
(* Severities that never run *)
(*****************************************************************************)

let inventory_rule_content =
  {|
rules:
  - id: inventory
    pattern: print(...)
    message: inventory
    languages: [python]
    severity: INVENTORY
|}

let mixed_severities_rule_content =
  {|
rules:
  - id: shown
    pattern: print(...)
    message: shown
    languages: [python]
    severity: ERROR
  - id: inventory
    pattern: print(...)
    message: inventory
    languages: [python]
    severity: INVENTORY
  - id: experiment
    pattern: print(...)
    message: experiment
    languages: [python]
    severity: EXPERIMENT
|}

(* the check ids of the findings of a --json scan and its exit code *)
(* the JSON document of a scan of target.py with the rules given *)
let json_scan_output (caps : Scan_subcommand.caps) ~(rule : string)
    ~(target : string) (extra_args : string list) :
    Semgrep_output_v1_t.cli_output * Exit_code.t =
  with_env_app_token (fun () ->
      let repo_files =
        [ F.File ("rules.yml", rule); F.File ("target.py", target) ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      (Array.of_list
                         ([ "opengrep-scan"; "--experimental"; "--json"; "--config"; "rules.yml" ]
                         @ extra_args @ [ "target.py" ]))))
          in
          (Semgrep_output_v1_j.cli_output_of_string stdout_output, exit_code)))

(* the ids of the findings of such a scan *)
let json_scan (caps : Scan_subcommand.caps) ~(rule : string) ~(target : string)
    (extra_args : string list) : string list * Exit_code.t =
  let out, exit_code = json_scan_output caps ~rule ~target extra_args in
  ( out.results
    |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) ->
           Rule_ID.to_string m.check_id),
    exit_code )

let test_inventory_and_experiment_rules_never_run
    (caps : Scan_subcommand.caps) () =
  let target = "print(1)\n" in
  let check_ids, exit_code =
    json_scan caps ~rule:mixed_severities_rule_content ~target [ "--error" ]
  in
  Exit_code.Check.findings exit_code;
  Alcotest.(check (list string)) "only the ERROR rule ran" [ "shown" ] check_ids;
  (* a config made only of such rules is valid, finds nothing, and its
     target is still listed as scanned; python: test_omit_inventory *)
  let out, exit_code =
    json_scan_output caps ~rule:inventory_rule_content ~target [ "--error" ]
  in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string)) "no findings" [] (out.results |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) -> Rule_ID.to_string m.check_id));
  Alcotest.(check (list string))
    "the target was scanned" [ "target.py" ]
    (out.paths.scanned |> List_.map Fpath.to_string)

(*****************************************************************************)
(* Rule errors *)
(*****************************************************************************)
(* A rule that cannot be loaded is reported in the JSON errors with the
 * type and code of the error, which is also the exit code of the scan.
 *)

let unknown_language_rule_content =
  {|
rules:
  - id: arg-reassign
    pattern: $X = 1
    message: "$X is being assigned to one"
    languages: [intercal]
    severity: WARNING
|}

(* a 'pattern' has no semantic meaning for a regex-only rule *)
let pattern_in_regex_rule_content =
  {|
rules:
  - id: bad
    message: cannot use 'pattern' with language 'regex'
    languages: [regex]
    severity: WARNING
    patterns:
      - pattern: $X
|}

let invalid_pattern_rule_content =
  {|
rules:
  - id: bad-pat
    pattern: "("
    message: cannot be parsed
    languages: [python]
    severity: WARNING
|}

(* the (type, code, rule id) of the JSON errors of the scan and its exit
 * code; the rule is passed with --config, or with -e when it is None *)
let json_errors (caps : Scan_subcommand.caps) ~(rule : string option)
    (extra_args : string list) : (string * int * string option) list * Exit_code.t
    =
  with_env_app_token (fun () ->
      let rule_files, config_args =
        match rule with
        | Some rule -> ([ F.File ("rules.yml", rule) ], [ "--config"; "rules.yml" ])
        | None -> ([], [])
      in
      let repo_files = rule_files @ [ F.File ("target.py", stupid_py_content) ] in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      (Array.of_list
                         ([ "opengrep-scan"; "--experimental"; "--json" ]
                         @ config_args @ extra_args @ [ "target.py" ]))))
          in
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          ( out.errors
            |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
                   ( Error.string_of_error_type e.type_,
                     e.code,
                     Option.map Rule_ID.to_string e.rule_id )),
            exit_code )))

let test_rule_errors (caps : Scan_subcommand.caps) () =
  let check name ~rule extra_args expected_errors
      (expected_exit : Exit_code.t -> unit) =
    let errors, exit_code = json_errors caps ~rule extra_args in
    Alcotest.(check (list (triple string int (option string))))
      name expected_errors errors;
    expected_exit exit_code
  in
  (* the run aborts on the configuration, so the exit code is the same for
     every kind of error; the code of each entry still names the kind *)
  check "unknown language" ~rule:(Some unknown_language_rule_content) []
    [ ("Unknown language", 8, Some "arg-reassign") ]
    Exit_code.Check.missing_config;
  check "pattern in a regex rule" ~rule:(Some pattern_in_regex_rule_content) []
    [ ("Invalid rule schema", 4, Some "bad") ]
    Exit_code.Check.missing_config;
  check "invalid pattern in a rule" ~rule:(Some invalid_pattern_rule_content) []
    [ ("Rule parse error", 4, Some "bad-pat") ]
    Exit_code.Check.missing_config;
  check "invalid -e pattern" ~rule:None [ "-e"; "("; "-l"; "python" ]
    [ ("Rule parse error", 4, Some "-") ]
    Exit_code.Check.missing_config

(*****************************************************************************)
(* nosem with an invalid or unknown rule id *)
(*****************************************************************************)
(* A 'nosem' comment whose id is not a valid rule id, or matches no rule,
 * suppresses nothing. Under --strict, each such comment is a warning in the
 * errors and the file counts as partially analysed; without it, the scan
 * reports nothing about them. python: test_nosem_rule__invalid_id
 *)

let nosem_rule_content =
  {|
rules:
  - id: test-nosem
    message: test-nosem-message
    severity: WARNING
    languages: [javascript]
    pattern: test_nosem_func(...)
|}

(* the scan of tests/nosemgrep/nosem_invalid_id.js: its exit code, and its
 * stdout when captured (for the JSON), "" otherwise (for a snapshot) *)
let scan_nosem_invalid_id (caps : Scan_subcommand.caps) ~(capture_stdout : bool)
    (format_args : string list) : Exit_code.t * string =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", nosem_rule_content);
          F.File
            ( "nosem_invalid_id.js",
              UFile.read_file (Fpath.v "tests/nosemgrep/nosem_invalid_id.js") );
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let scan () : Exit_code.t =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  (Array.of_list
                     ([ "opengrep-scan"; "--experimental"; "--config"; "rules.yml" ]
                     @ format_args @ [ "nosem_invalid_id.js" ])))
          in
          if capture_stdout then Testo.with_capture stdout scan
          else (scan (), "")))

let test_nosem_invalid_id_json (caps : Scan_subcommand.caps) () =
  let errors_of (format_args : string list) :
      (string * Fpath.t option) list * string list * Exit_code.t =
    let exit_code, stdout_output =
      scan_nosem_invalid_id caps ~capture_stdout:true format_args
    in
    let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
    ( out.errors
      |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
             (Error.string_of_error_type e.type_, e.path)),
      out.results
      |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) ->
             Int.to_string m.start.line),
      exit_code )
  in
  let path = Some (Fpath.v "nosem_invalid_id.js") in
  let errors, lines, exit_code = errors_of [ "--json"; "--strict" ] in
  Exit_code.Check.fatal exit_code;
  Alcotest.(check (list string)) "the findings, none suppressed"
    [ "2"; "6"; "12" ] lines;
  Alcotest.(check (list (pair string (option (testable Fpath.pp Fpath.equal)))))
    "one warning per unknown or invalid id with --strict"
    [
      ("SemgrepWarning", path); ("SemgrepWarning", path); ("SemgrepWarning", path);
    ]
    errors;
  let errors, lines, exit_code = errors_of [ "--json" ] in
  (* WARNING findings do not fail the scan without --error *)
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string)) "the same findings" [ "2"; "6"; "12" ] lines;
  Alcotest.(check int) "no warning without --strict" 0 (List.length errors)

(* the text output with --strict, with the partially analysed summary line *)
let test_nosem_invalid_id_text (caps : Scan_subcommand.caps) () =
  let exit_code, _no_stdout =
    scan_nosem_invalid_id caps ~capture_stdout:false [ "--strict" ]
  in
  Exit_code.Check.fatal exit_code

(*****************************************************************************)
(* Log file *)
(*****************************************************************************)
(* $OPENGREP_LOG_FILE (or $SEMGREP_LOG_FILE) gets a copy of the logs at the
 * level of stderr; its directory is created. python: test_last_log_exists
 *)
let scan_for_log_file (caps : Scan_subcommand.caps) (extra_args : string list) :
    Exit_code.t =
  let exit_code, _stdout =
    Testo.with_capture stdout (fun () ->
        without_settings (fun () ->
            Scan_subcommand.main caps
              (Array.of_list
                 ([
                    "opengrep-scan";
                    "--experimental";
                    "--json";
                    "--config";
                    "rules.yml";
                  ]
                 @ extra_args @ [ "stupid.py" ]))))
  in
  exit_code

let with_log_file_repo (f : Fpath.t -> unit) : unit =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo repo_files f)

let test_log_file (caps : Scan_subcommand.caps) () =
  with_log_file_repo (fun (cwd : Fpath.t) ->
      let log_file = Fpath.(cwd / "logs" / "nested" / "opengrep.log") in
      Semgrep_envvars.with_envvar "OPENGREP_LOG_FILE"
        (Fpath.to_string log_file)
        (fun () -> ignore (scan_for_log_file caps [ "--verbose" ]));
      let contents = UFile.read_file log_file in
      Alcotest.(check bool)
        "info records with --verbose" true
        (String_.contains ~term:"[INFO]" contents);
      Alcotest.(check bool)
        "no debug records with --verbose" false
        (String_.contains ~term:"[DEBUG]" contents);
      (* the file is truncated by the next run, at the default level *)
      Semgrep_envvars.with_envvar "SEMGREP_LOG_FILE" (Fpath.to_string log_file)
        (fun () -> ignore (scan_for_log_file caps []));
      let contents = UFile.read_file log_file in
      Alcotest.(check bool)
        "no info records at the default level" false
        (String_.contains ~term:"[INFO]" contents))

(* A log file that cannot be created is a diagnostic that is lost, not a scan
 * that fails. *)
let test_log_file_unwritable (caps : Scan_subcommand.caps) () =
  with_log_file_repo (fun (cwd : Fpath.t) ->
      (* stupid.py is a regular file, so it cannot hold a directory *)
      let log_file = Fpath.(cwd / "stupid.py" / "opengrep.log") in
      Semgrep_envvars.with_envvar "OPENGREP_LOG_FILE"
        (Fpath.to_string log_file)
        (fun () -> Exit_code.Check.ok (scan_for_log_file caps []));
      Alcotest.(check bool)
        "no log file was created" false
        (Sys.file_exists (Fpath.to_string log_file)))


(*****************************************************************************)
(* Process limits *)
(*****************************************************************************)
(* --timeout, --timeout-threshold and --max-memory on the rules and targets
 * of tests/process_limits, whose rules time out on open_redirect.py.
 * python: test_process_limits.py
 *)

let process_limits_root : Fpath.t = Fpath.v "tests/process_limits"

(* the scan of the files of tests/process_limits copied into a temp repo;
 * the JSON errors (type, rule id) and the exit code when captured, else the
 * output on stdout and stderr for a snapshot *)
let scan_process_limits (caps : Scan_subcommand.caps) ~(capture_stdout : bool)
    (args : string list) : (string * string option) list * Exit_code.t =
  let read (rel : string) : string =
    UFile.read_file Fpath.(process_limits_root // v rel)
  in
  let repo_files =
    [
      F.Dir
        ( "rules",
          [
            F.File ("long.yaml", read "rules/long.yaml");
            F.File ("multiple-long.yaml", read "rules/multiple-long.yaml");
          ] );
      F.Dir
        ( "targets",
          [
            F.File ("open_redirect.py", read "targets/open_redirect.py");
            F.File ("gnu-lgplv2.txt", read "targets/gnu-lgplv2.txt");
          ] );
    ]
  in
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let scan () : Exit_code.t =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  (Array.of_list ([ "opengrep-scan"; "--experimental"; "-j"; "1" ] @ args)))
          in
          if capture_stdout then
            let exit_code, stdout_output = Testo.with_capture stdout scan in
            let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
            ( out.errors
              |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
                     ( Error.string_of_error_type e.type_,
                       Option.map Rule_ID.to_string e.rule_id )),
              exit_code )
          else ([], scan ())))

let test_process_limits_json (caps : Scan_subcommand.caps) () =
  let check name (args : string list) expected =
    let errors, exit_code = scan_process_limits caps ~capture_stdout:true args in
    Exit_code.Check.ok exit_code;
    Alcotest.(check (list (pair string (option string)))) name expected errors
  in
  let multiple_long = [ "--json"; "--timeout"; "1"; "--config"; "rules/multiple-long.yaml"; "targets/open_redirect.py" ] in
  (* the rules run in the order of the file: the threshold stops the file
     after the first ones *)
  check "timeout threshold 1"
    ([ "--timeout-threshold"; "1" ] @ multiple_long)
    [ ("Timeout", Some "rules.forcetimeout") ];
  check "timeout threshold 2"
    ([ "--timeout-threshold"; "2" ] @ multiple_long)
    [ ("Timeout", Some "rules.forcetimeout"); ("Timeout", Some "rules.forcetimeout2") ];
  check "spacegrep timeout"
    [ "--json"; "--timeout"; "1"; "-l"; "generic"; "-e"; "$A ... $B ... $C ... Frob ... Yoyodyne"; "targets/gnu-lgplv2.txt" ]
    [ ("Timeout", Some "-") ];
  (* the limit strikes when memprof samples an allocation: during the
     parsing of the target, with no rule to attribute the error to, or
     while the rule runs *)
  let errors, exit_code =
    scan_process_limits caps ~capture_stdout:true
      [ "--json"; "--max-memory"; "1"; "--config"; "rules/long.yaml"; "targets/open_redirect.py" ]
  in
  Exit_code.Check.ok exit_code;
  Alcotest.(check (list string)) "max memory: the error" [ "Out of memory" ]
    (List_.map fst errors);
  Alcotest.(check bool) "max memory: the rule, if any" true
    (List.for_all
       (fun (_type, rule_id) ->
         match rule_id with
         | None
         | Some "rules.forcetimeout" ->
             true
         | Some _ -> false)
       errors)

(* $SEMGREP_TIMEOUT and its $OPENGREP_TIMEOUT alias set --timeout; the flag
 * wins over both, and the alias wins over the legacy name.
 * python: scan.py declared envvar="SEMGREP_TIMEOUT" on --timeout.
 * The spacegrep pattern below needs about forty seconds on this target, so a
 * one-second limit that was honoured reports a timeout at once, and a limit
 * that was not honoured shows up as the absence of the error.
 *)
let test_timeout_env (caps : Scan_subcommand.caps) () =
  let slow_scan (args : string list) : string list =
    let errors, exit_code =
      scan_process_limits caps ~capture_stdout:true
        ([
           "--json"; "-l"; "generic"; "-e";
           "$A ... $B ... $C ... Frob ... Yoyodyne";
         ]
        @ args
        @ [ "targets/gnu-lgplv2.txt" ])
    in
    Exit_code.Check.ok exit_code;
    List_.map fst errors
  in
  let check (name : string) (vars : (string * string) list)
      (args : string list) : unit =
    let run () =
      Alcotest.(check (list string)) name [ "Timeout" ] (slow_scan args)
    in
    List.fold_right
      (fun ((var : string), (value : string)) (k : unit -> unit) () ->
        Semgrep_envvars.with_envvar var value k)
      vars run ()
  in
  check "SEMGREP_TIMEOUT is honoured" [ ("SEMGREP_TIMEOUT", "1") ] [];
  check "OPENGREP_TIMEOUT is honoured" [ ("OPENGREP_TIMEOUT", "1") ] [];
  check "--timeout wins over the variable"
    [ ("SEMGREP_TIMEOUT", "600") ]
    [ "--timeout"; "1" ];
  check "OPENGREP_TIMEOUT wins over SEMGREP_TIMEOUT"
    [ ("SEMGREP_TIMEOUT", "600"); ("OPENGREP_TIMEOUT", "1") ]
    []

(* Opengrep has no version check, so the variable pysemgrep validated is
 * ignored rather than failing the scan on a value it cannot read. *)
let test_version_check_env_ignored (caps : Scan_subcommand.caps) () =
  with_log_file_repo (fun (_cwd : Fpath.t) ->
      [ "SEMGREP_ENABLE_VERSION_CHECK"; "OPENGREP_ENABLE_VERSION_CHECK" ]
      |> List.iter (fun (var : string) ->
             Semgrep_envvars.with_envvar var "notabool" (fun () ->
                 Exit_code.Check.ok (scan_for_log_file caps []))))

(* the text output: the timeout warning, the stopped file and the verbose
   listing of the partially analysed file with its rule *)
let test_process_limits_text (caps : Scan_subcommand.caps) () =
  let _no_errors, exit_code =
    scan_process_limits caps ~capture_stdout:false
      [ "--verbose"; "--timeout"; "1"; "--timeout-threshold"; "1"; "--config"; "rules/multiple-long.yaml"; "targets/open_redirect.py" ]
  in
  Exit_code.Check.ok exit_code

(*****************************************************************************)
(* YAML block scalars *)
(*****************************************************************************)
(* The findings of key: $VALUE on tests/yaml/target.yaml, whose values are
 * block scalars: the lines of a finding end at its last non-blank
 * character, the message is the metavariable content and the content
 * spans exactly its offsets. python: test_yaml_metavariables and
 * test_quiet_mode_has_empty_stderr
 *)
let yaml_fixtures_root : Fpath.t = Fpath.v "tests/yaml"

let read_yaml_fixture (rel : string) : string =
  UFile.read_file Fpath.(yaml_fixtures_root / rel)

let scan_yaml_target (caps : Scan_subcommand.caps) (args : string list) :
    Exit_code.t * string * string =
  let repo_files =
    [
      F.File ("yaml_key.yaml", read_yaml_fixture "yaml_key.yaml");
      F.File ("target.yaml", read_yaml_fixture "target.yaml");
    ]
  in
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let (exit_code, stdout_output), stderr_output =
            Testo.with_capture stderr (fun () ->
                Testo.with_capture stdout (fun () ->
                    without_settings (fun () ->
                        Scan_subcommand.main caps
                          (Array.of_list
                             ([ "opengrep-scan"; "--experimental"; "--json"; "--config"; "yaml_key.yaml" ]
                             @ args @ [ "target.yaml" ])))))
          in
          (exit_code, stdout_output, stderr_output)))

let test_yaml_block_scalars (caps : Scan_subcommand.caps) () =
  let exit_code, stdout_output, _stderr = scan_yaml_target caps [] in
  Exit_code.Check.ok exit_code;
  let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
  Alcotest.(check (list string))
    "the lines of each finding"
    [
      {|- key: "one"|};
      {|- key: 'two'|};
      "- key: |\n    three";
      "- key: |\n   four";
      "- key: |       \n    fi\n\n    ve";
      "- key: |\n    si\n\n      x";
      "- key: >\n    seven";
      "- key: >\n    eig\n\n    ht";
    ]
    (out.results |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) -> m.extra.lines));
  out.results
  |> List.iter (fun (m : Semgrep_output_v1_t.cli_match) ->
         let (value : Semgrep_output_v1_t.metavar_value) =
           match m.extra.metavars with
           | Some metavars -> List.assoc "$VALUE" metavars
           | None -> Alcotest.fail "no metavariables in the finding"
         in
         let content = value.abstract_content in
         Alcotest.(check string) "the message is the value" (content ^ "\n")
           m.extra.message;
         Alcotest.(check int) "the value spans its offsets"
           (value.end_.offset - value.start.offset)
           (String.length content))

(* --quiet prints nothing on stderr, so that the JSON is usable where the
   two streams are mixed *)
let test_quiet_json (caps : Scan_subcommand.caps) () =
  let exit_code, stdout_output, stderr_output =
    scan_yaml_target caps [ "--quiet" ]
  in
  Exit_code.Check.ok exit_code;
  Alcotest.(check string) "empty stderr" "" stderr_output;
  ignore (Semgrep_output_v1_j.cli_output_of_string stdout_output)

(*****************************************************************************)
(* Missing scanning roots *)
(*****************************************************************************)

(* the scan of the roots, with a rule and one existing target; the JSON on
 * stdout and the exit code *)
let scan_roots (caps : Scan_subcommand.caps) ~(format_args : string list)
    (roots : string list) : string * Exit_code.t =
  with_env_app_token (fun () ->
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let exit_code, stdout_output =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      (Array.of_list
                         ([ "opengrep-scan"; "--experimental"; "--config"; "rules.yml" ]
                         @ format_args @ roots))))
          in
          (stdout_output, exit_code)))

let test_missing_roots_json (caps : Scan_subcommand.caps) () =
  let stdout_output, exit_code =
    scan_roots caps ~format_args:[ "--json" ] [ "nope.py"; "stupid.py"; "nope/" ]
  in
  Exit_code.Check.fatal exit_code;
  let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
  Alcotest.(check (list string))
    "one fatal error per missing root"
    [
      "2 `Error Semgrep_output_v1_t.SemgrepError File not found: nope.py";
      (* the trailing slash of the root is not part of the path, and the
         Python wrapper dropped it from this message too *)
      "2 `Error Semgrep_output_v1_t.SemgrepError File not found: nope";
    ]
    (out.errors
    |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
           spf "%d %s %s %s" e.code
             (Semgrep_output_v1_t.show_error_severity e.level)
             (Semgrep_output_v1_t.show_error_type e.type_)
             (e.message ||| "<no message>")));
  Alcotest.(check int) "no results" 0 (List.length out.results);
  Alcotest.(check (list string)) "nothing scanned" [] (out.paths.scanned |> List_.map Fpath.to_string)

let test_missing_roots_text (caps : Scan_subcommand.caps) () =
  match scan_roots caps ~format_args:[] [ "nope.py"; "stupid.py" ] with
  | exception Error.Semgrep_error (msg, Some exit_code) ->
      Exit_code.Check.fatal exit_code;
      Alcotest.(check string) "the error message" "File not found: nope.py" msg
  | _ -> Alcotest.fail "expected the scan to abort"

let test_missing_roots_test_mode (caps : Scan_subcommand.caps) () =
  match scan_roots caps ~format_args:[ "--test" ] [ "stupid.py"; "nope.py" ] with
  | exception Error.Semgrep_error (msg, None) ->
      Alcotest.(check string) "the error message" "File not found: nope.py" msg
  | _ -> Alcotest.fail "expected the test run to abort"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan (e2e)"
    [
      t "basic output" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output caps);
      t "basic output with --output-enclosing-context" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output_enclosing_context caps);
      t "basic output with --opengrep-ignore-pattern" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output_ignore_pattern caps);
      t "incremental output with --incremental-output-postprocess"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output_nosem_incremental caps);
      t "interfile pre-existing findings are not reported by a diff scan"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalize_multi_commit
        (test_interfile_diff_scan_preexisting caps);
      t "interfile findings when scanning from a subdirectory"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_interfile_scan_from_subdir caps);
      t "interfile diff scan from a subdirectory reports nothing pre-existing"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalize_multi_commit
        (test_interfile_diff_scan_subdir caps);
      t "interfile findings in a diff scan"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalize_multi_commit
        (test_interfile_diff_scan caps);
      t "interfile findings from a partial target set"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_interfile_partial_target caps);
      t "interfile same-sink findings keep distinct sources"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_interfile_source_sink_dedup caps);
      t "interfile targets are scanned targets"
        (test_interfile_paths_scanned caps);
      t "interfile limits: memory" (test_interfile_limits caps);
      t "intrafile same-sink findings dedup to one"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_intrafile_same_sink_dedup caps);
      t "interfile findings with --incremental-output"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_interfile_incremental_output caps);
      t "incremental output with --incremental-output-postprocess and --disable-nosem"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output_nosem_incremental_disabled caps);
      t "basic verbose output"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_verbose_output caps);
      t "precise range for parenthesized expression" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output caps
           ~rules_file:"java_arg_paren.yaml"
           ~rules_content:java_arg_paren_yaml_content
           ~code_file:"java_arg_paren.java"
           ~code_content:java_arg_paren_java_content);
      t "basic output with --max-match-per-file" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output_max_match caps);
      t "basic output with max-match-per-file rule option" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output_max_match_in_rule caps);
      t "truncated terraform block does not abort scan"
        (test_truncated_terraform_block caps);
      t "fingerprints equal the python wrapper's" (test_fingerprints caps);
      t "fingerprints change with the match, not the formatting"
        (test_id_change caps);
      t "text output: two rules with the same message"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output caps ~rules_content:two_rules_same_message_content
           ~code_content:"print(1 == 1)\n");
      t "text output: the autofix line" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (test_basic_output caps ~rules_content:autofix_rules_content
           ~code_content:autofix_py_content);
      t "text output: long rule ids, messages and lines are wrapped"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output caps ~rules_content:long_rule_id_content
           ~code_content:long_line_py_content
           ~extra_args:[ "--max-chars-per-line"; "60" ]);
      t "--time: the summary of the text output"
        ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize:(mask_times :: normalize)
        (test_basic_output caps ~extra_args:[ "--time" ]);
      (* the captured output is not a tty, so colour only appears when forced *)
      t "colour: --force-color styles the text output"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_basic_output caps ~extra_args:[ "--force-color" ]);
      t "colour: NO_COLOR gives plain output" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (fun () -> with_no_color (test_basic_output caps));
      t "colour: --force-color wins over NO_COLOR"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (fun () ->
          with_no_color (test_basic_output caps ~extra_args:[ "--force-color" ]));
      t "--time: the times in the JSON output" (test_time_json caps);
      t "--time: the times of a scan without a finding"
        (test_time_json_no_findings caps);
      t "a target that is not UTF-8 is reported with replacement characters"
        (test_invalid_utf8 caps);
      t "a parse error of a target that is not UTF-8 is reported with \
         replacement characters"
        (test_invalid_utf8_in_error caps);
      t "$SEMGREP_TIMEOUT and $OPENGREP_TIMEOUT set the timeout"
        (test_timeout_env caps);
      t "$SEMGREP_ENABLE_VERSION_CHECK is ignored"
        (test_version_check_env_ignored caps);
      t "INVENTORY and EXPERIMENT rules never run"
        (test_inventory_and_experiment_rules_never_run caps);
      t "rule errors: type, code and exit code" (test_rule_errors caps);
      t "nosem with an invalid or unknown id: JSON warnings with --strict"
        (test_nosem_invalid_id_json caps);
      t "log file: a copy of the logs at the level of stderr"
        (test_log_file caps);
      t "log file: an unwritable path warns and the scan carries on"
        (test_log_file_unwritable caps);
      t "yaml: the lines of a match on a block" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (test_basic_output caps ~rules_file:"yaml_capture.yaml"
           ~rules_content:(read_yaml_fixture "yaml_capture.yaml")
           ~code_file:"yaml_capture_target.yaml"
           ~code_content:(read_yaml_fixture "yaml_capture_target.yaml"));
      t "yaml block scalars: lines, message and offsets of the findings"
        (test_yaml_block_scalars caps);
      t "--quiet: nothing on stderr with the JSON" (test_quiet_json caps);
      t "process limits: timeouts and memory limit in the JSON errors"
        (test_process_limits_json caps);
      t "process limits: timeout warnings in the text output"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_process_limits_text caps);
      t "nosem with an invalid or unknown id: text output with --strict"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_nosem_invalid_id_text caps);
      t "missing scanning roots: fatal error reported in the JSON output"
        (test_missing_roots_json caps);
      t "missing scanning roots: fatal error with text output"
        (test_missing_roots_text caps);
      t "missing scanning roots: fatal error with --test"
        (test_missing_roots_test_mode caps);
      (* The text output of a scan of a directory, with colour.
         python: test_terminal_output *)
      t "text output of a directory scan" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (Test_scan_helpers.run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
           ~targets:[]
           ~extra_files:[ F.dir "targets" [ basic_targets_dir ] ]
           ~extra_args:[ "--force-color"; "targets/basic" ]);
      (* python: test_terminal_output_quiet *)
      t "text output of a directory scan with --quiet"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (Test_scan_helpers.run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
           ~targets:[]
           ~extra_files:[ F.dir "targets" [ basic_targets_dir ] ]
           ~extra_args:[ "--force-color"; "--quiet"; "targets/basic" ]);
      (* python: test_verbose *)
      t "verbose text output of a file scan" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (Test_scan_helpers.run_scan caps ~format_args:[]
           ~rule:"rules/basic.yaml" ~targets:[ "targets/basic.py" ]
           ~extra_args:[ "--force-color"; "--verbose" ]);
      (* python: test_show_supported_languages *)
      t "--show-supported-languages" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (Test_scan_helpers.run_scan caps ~format_args:[]
           ~rule:"rules/basic.yaml" ~targets:[ "targets/basic.py" ]
           ~extra_args:[ "--show-supported-languages" ]);
      (* python: test_sort_text_findings *)
      t "text output: the findings sorted" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (Test_scan_helpers.run_scan caps ~format_args:[]
           ~rule:"rules/sort-findings.yaml" ~targets:[]
           ~extra_files:[ Test_scan_subcommand_findings.sort_findings_dir ]
           ~extra_args:[ "sort-findings" ]);
      (* An error the engine attaches to a file does not fail the scan.
         python: test_exit_code_warning_error *)
      t "target error: the scan still ends with 0"
        (Test_scan_helpers.run_scan caps ~format_args:[]
           ~rule:"rules/stuff_stmt.yaml"
           ~targets:[ "targets/error_management/r2c_was_fatal.py" ]
           ~extra_args:[ "--verbose" ]);
      (* ... but --strict turns it into a fatal error.
         python: test_exit_code_with_strict *)
      t "target error: --strict makes it fatal"
        (Test_scan_helpers.run_scan caps ~format_args:[]
           ~rule:"rules/stuff_stmt.yaml"
           ~targets:[ "targets/error_management/r2c_was_fatal.py" ]
           ~extra_args:[ "--verbose"; "--strict" ]
           ~check:Exit_code.Check.fatal);
      (* A file that does not parse is reported once even though two rules
         run over it.
         differs from the Python wrapper: it counts the skipped lines next
         to the file name
         python: test_parse_errors *)
      t "text output: a target parse error reported once"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (Test_scan_helpers.run_scan caps ~format_args:[] ~targets:[]
           ~extra_files:
             [
               two_rules_dir;
               F.File
                 ( "invalid_javascript.js",
                   Test_scan_helpers.read_fixture
                     "targets/bad/invalid_javascript.js" );
             ]
           ~extra_args:
             [
               "--config"; "rules/two_rules/"; "--force-color"; "--verbose";
               "invalid_javascript.js";
             ]);
    ]
