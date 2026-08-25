(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing end-to-end (e2e) the ci subcommand, similar to
 * cli/tests/default/e2e-other/test_ci.py but exercising only the OCaml
 * implementation.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let blocking_rule_content =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

(* "dev.semgrep.actions" without "block" makes the rule non-blocking *)
let nonblocking_rule_content =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
    metadata:
      dev.semgrep.actions: []
|}

let clean_py_content = {|
def foo(a, b):
    return a + b
|}

let finding_py_content = {|
def foo(a, b):
    return a + b == a + b
|}

(* coupling: Test_scan_subcommand.normalize *)
let normalize =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_line ~after:"Opengrep version: " ();
    Testo.mask_line ~after:"versions    - opengrep " ();
  ]

(* the merge-base machinery logs full commit hashes, different on each run *)
let normalize_commit_hashes =
  normalize
  @ [ Testo.mask_pcre_pattern ~replace:(fun _ -> "<HASH>") "[a-f0-9]{40}" ]

let without_settings f =
  Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml" f

(* Run the ci subcommand in a throw-away git repo holding a rule file and
 * one target (under [target_dir] when given), and check the exit code. *)
let run_ci (caps : Ci_subcommand.caps) ~(rule : string) ~(target : string)
    ?(target_dir : string option) ?(extra_args : string list = [])
    ?(check : Exit_code.t -> unit = Exit_code.Check.ok)
    ?(before_scan : unit -> string list = fun () -> []) () =
  let target_file = F.File ("foo.py", target) in
  let repo_files =
    F.File ("rules.yaml", rule)
    ::
    (match target_dir with
    | None -> [ target_file ]
    | Some dir -> [ F.Dir (dir, [ target_file ]) ])
  in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
      let late_args = before_scan () in
      let argv =
        Array.of_list
          ([ "opengrep-ci"; "--experimental"; "--config"; "rules.yaml" ]
          @ extra_args @ late_args)
      in
      let exit_code =
        without_settings (fun () -> Ci_subcommand.main caps argv)
      in
      check exit_code)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_no_findings (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:clean_py_content ()

let test_blocking_findings (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~check:Exit_code.Check.findings ()

let test_nonblocking_findings (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:nonblocking_rule_content ~target:finding_py_content ()

let test_audit_mode (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--audit-on"; "unknown" ] ()

let test_subdir_outside_cwd_suppressed (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--subdir"; "/etc" ] ()

let test_subdir_outside_cwd_fatal (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--subdir"; "/etc"; "--no-suppress-errors" ]
    ~check:Exit_code.Check.fatal ()

(* an explicit false in the environment must turn suppression off, exactly
 * like the --no-suppress-errors flag *)
let test_suppress_errors_env_false (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "SEMGREP_SUPPRESS_ERRORS" "false" (fun () ->
      run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
        ~extra_args:[ "--subdir"; "/etc" ]
        ~check:Exit_code.Check.fatal ())

let test_subdir_findings (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~target_dir:"sub"
    ~extra_args:[ "--subdir"; "sub" ]
    ~check:Exit_code.Check.findings ()

(* the baseline (as a rev, like --baseline-commit main) removes the
 * pre-existing finding; only the one added on top of it remains *)
let test_baseline_rev (caps : Ci_subcommand.caps) () =
  let caps_exec = (caps :> < Cap.exec >) in
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~check:Exit_code.Check.findings
    ~before_scan:(fun () ->
      let baseline = Git_wrapper.command caps_exec [ "rev-parse"; "HEAD" ] in
      UFile.write_file ~file:(Fpath.v "foo.py")
        (finding_py_content ^ "\ndef bar(c, d):\n    return c + d == c + d\n");
      let _ = Git_wrapper.command caps_exec [ "add"; "foo.py" ] in
      let _ =
        Git_wrapper.command caps_exec [ "commit"; "-m"; "add a finding" ]
      in
      [ "--baseline-commit"; baseline ])
    ()

(* same as test_baseline_rev but everything happens under --subdir *)
let test_baseline_rev_in_subdir (caps : Ci_subcommand.caps) () =
  let caps_exec = (caps :> < Cap.exec >) in
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~target_dir:"sub" ~check:Exit_code.Check.findings
    ~before_scan:(fun () ->
      let baseline = Git_wrapper.command caps_exec [ "rev-parse"; "HEAD" ] in
      UFile.write_file
        ~file:(Fpath.v "sub/foo.py")
        (finding_py_content ^ "\ndef bar(c, d):\n    return c + d == c + d\n");
      let _ = Git_wrapper.command caps_exec [ "add"; "sub/foo.py" ] in
      let _ =
        Git_wrapper.command caps_exec [ "commit"; "-m"; "add a finding" ]
      in
      [ "--baseline-commit"; baseline; "--subdir"; "sub" ])
    ()

(* the API shortcut for the GitHub merge base answers garbage; the local
 * merge-base computation is used instead *)
let test_github_branchoff_api_failure (caps : Ci_subcommand.caps) () =
  let caps_exec = (caps :> < Cap.exec >) in
  let repo_files = [ F.File ("foo.py", clean_py_content) ] in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun cwd ->
      (* the origin holds main and a feature branch; the merge-base
       * machinery runs in a clone, like on a CI runner *)
      let base = Git_wrapper.command caps_exec [ "rev-parse"; "HEAD" ] in
      let _ = Git_wrapper.command caps_exec [ "checkout"; "-b"; "feature" ] in
      UFile.write_file ~file:(Fpath.v "foo.py") finding_py_content;
      let _ = Git_wrapper.command caps_exec [ "add"; "foo.py" ] in
      let _ = Git_wrapper.command caps_exec [ "commit"; "-m"; "change" ] in
      let head = Git_wrapper.command caps_exec [ "rev-parse"; "HEAD" ] in
      let _ =
        Git_wrapper.command caps_exec
          [ "clone"; Fpath.to_string cwd; "clone" ]
      in
      Testutil_files.with_chdir (Fpath.v "clone") @@ fun () ->
      let event : Yojson.Basic.t =
        `Assoc
          [
            ( "pull_request",
              `Assoc
                [
                  ("base", `Assoc [ ("ref", `String "main") ]);
                  ( "head",
                    `Assoc [ ("ref", `String "feature"); ("sha", `String head) ]
                  );
                ] );
          ]
      in
      let gha_env : Github_metadata.env =
        {
          _GITHUB_EVENT_JSON = event;
          _GITHUB_REPOSITORY = Some "example/repo";
          _GITHUB_REPOSITORY_ID = None;
          _GITHUB_REPOSITORY_OWNER_ID = None;
          _GITHUB_API_URL = Some (Uri.of_string "https://api.github.example");
          _GITHUB_SERVER_URL = Uri.of_string "https://github.com";
          _GITHUB_SHA = None;
          _GITHUB_REF = None;
          _GITHUB_HEAD_REF = None;
          _GITHUB_RUN_ID = None;
          _GITHUB_EVENT_NAME = Some "pull_request";
          _GH_TOKEN = Some "dummy-token";
        }
      in
      let git_env : Git_metadata.env =
        {
          _SEMGREP_REPO_NAME = None;
          _SEMGREP_REPO_DISPLAY_NAME = None;
          _SEMGREP_REPO_URL = None;
          _SEMGREP_COMMIT = None;
          _SEMGREP_JOB_URL = None;
          _SEMGREP_PR_ID = None;
          _SEMGREP_PR_TITLE = None;
          _SEMGREP_BRANCH = None;
        }
      in
      Http_mock_client.with_testing_client
        (fun _req _body ->
          (* recorded in the checked output: proves the shortcut was tried *)
          print_endline "github API merge-base request received";
          Lwt.return
            (Http_mock_client.basic_response
               (Cohttp_lwt.Body.of_string "not json")))
        (fun () ->
          let meta =
            new Github_metadata.meta
              (caps :> < Cap.exec ; Cap.network >)
              ~cli_baseline_ref:None git_env gha_env
          in
          match meta#merge_base_ref with
          | Some (Find_targets.Commit sha) ->
              Alcotest.(check string)
                "merge base is the branch-off commit" base
                (Digestif.SHA1.to_hex sha)
          | _else_ -> Alcotest.fail "expected a commit merge base")
        ())

let test_gitlab_environment (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "GITLAB_CI" "true" (fun () ->
      Semgrep_envvars.with_envvar "CI_PIPELINE_SOURCE" "merge_request_event"
        (fun () ->
          run_ci caps ~rule:blocking_rule_content ~target:clean_py_content ()))

let test_github_environment (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "GITHUB_ACTIONS" "true" (fun () ->
      Semgrep_envvars.with_envvar "GITHUB_EVENT_NAME" "push" (fun () ->
          run_ci caps ~rule:blocking_rule_content ~target:clean_py_content ()))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Ci_subcommand.caps >) =
  Testo.categorize "Osemgrep Ci (e2e)"
    [
      t "no findings exits ok" ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_no_findings caps);
      t "blocking findings exit with findings" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_blocking_findings caps);
      t "non-blocking findings exit ok" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_nonblocking_findings caps);
      t "audit mode exits ok despite findings"
        ~checked_output:(Testo.stdxxx ()) ~normalize (test_audit_mode caps);
      t "subdir outside cwd is suppressed to ok"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_outside_cwd_suppressed caps);
      t "subdir outside cwd is fatal without suppression"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_outside_cwd_fatal caps);
      t "suppress-errors env var set to false"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_suppress_errors_env_false caps);
      t "baseline rev keeps only the new finding"
        ~checked_output:(Testo.stdxxx ()) ~normalize (test_baseline_rev caps);
      t "subdir restricts the scan and finds findings"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_findings caps);
      t "baseline rev in subdir keeps only the new finding"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_baseline_rev_in_subdir caps);
      t "github merge-base API failure falls back to git"
        ~checked_output:(Testo.stdxxx ()) ~normalize:normalize_commit_hashes
        (test_github_branchoff_api_failure caps);
      t "gitlab environment is detected" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_gitlab_environment caps);
      t "github environment is detected" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_github_environment caps);
    ]
