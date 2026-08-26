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

(* the scalar form of "dev.semgrep.actions" *)
let scalar_blocking_rule_content =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
    metadata:
      dev.semgrep.actions: block
|}

let scalar_nonblocking_rule_content =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
    metadata:
      dev.semgrep.actions: comment
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

(* a second rule file, for the SEMGREP_RULES list test; the pattern also
 * fires on finding_py_content *)
let second_rule_content =
  {|
rules:
  - id: return-stmt
    pattern: return $X
    message: "return statement"
    languages: [python]
    severity: ERROR
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
    ?(target_dir : string option) ?(extra_files : F.t list = [])
    ?(config_args : string list = [ "--config"; "rules.yaml" ])
    ?(extra_args : string list = [])
    ?(check : Exit_code.t -> unit = Exit_code.Check.ok)
    ?(before_scan : unit -> string list = fun () -> []) () =
  let target_file = F.File ("foo.py", target) in
  let repo_files =
    (F.File ("rules.yaml", rule) :: extra_files)
    @ (match target_dir with
      | None -> [ target_file ]
      | Some dir -> [ F.Dir (dir, [ target_file ]) ])
  in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
      let late_args = before_scan () in
      let argv =
        Array.of_list
          ([ "opengrep-ci"; "--experimental" ]
          @ config_args @ extra_args @ late_args)
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

let test_scalar_blocking_findings (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:scalar_blocking_rule_content ~target:finding_py_content
    ~check:Exit_code.Check.findings ()

let test_scalar_nonblocking_findings (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:scalar_nonblocking_rule_content ~target:finding_py_content
    ()

let test_audit_mode (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--audit-on"; "unknown" ] ()

(* the environment variable holds a whitespace-separated list of event
 * names; only the second one matches the "unknown" event, so a value
 * taken as one name would not enable audit mode *)
let test_audit_env_list (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "SEMGREP_AUDIT_ON" "push unknown" (fun () ->
      run_ci caps ~rule:blocking_rule_content ~target:finding_py_content ())

let test_subdir_outside_cwd_suppressed (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--subdir"; "/etc" ] ()

let test_subdir_outside_cwd_fatal (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--subdir"; "/etc"; "--no-suppress-errors" ]
    ~check:Exit_code.Check.fatal ()

(* a nonexistent subdir must be reported as not found, not as being
 * outside the current directory *)
let test_subdir_nonexistent_suppressed (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--subdir"; "no-such-dir" ]
    ()

let test_subdir_nonexistent_fatal (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "--subdir"; "no-such-dir"; "--no-suppress-errors" ]
    ~check:Exit_code.Check.fatal ()

(* a bad output destination is an error like any other: suppressed by
 * default, fatal with --no-suppress-errors *)
let test_output_conflict_suppressed (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:[ "-o"; "out.json"; "--json-output=out.json" ]
    ()

let test_output_conflict_fatal (caps : Ci_subcommand.caps) () =
  run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
    ~extra_args:
      [ "-o"; "out.json"; "--json-output=out.json"; "--no-suppress-errors" ]
    ~check:Exit_code.Check.fatal ()

(* an explicit false in the environment must turn suppression off, exactly
 * like the --no-suppress-errors flag *)
let test_suppress_errors_env_false (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "SEMGREP_SUPPRESS_ERRORS" "false" (fun () ->
      run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
        ~extra_args:[ "--subdir"; "/etc" ]
        ~check:Exit_code.Check.fatal ())

(* the environment variable holds a whitespace-separated list of rule
 * sources; both files must be loaded and both rules fire *)
let test_rules_env_list (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "SEMGREP_RULES" "rules.yaml rules2.yaml"
    (fun () ->
      run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
        ~extra_files:[ F.File ("rules2.yaml", second_rule_content) ]
        ~config_args:[] ~check:Exit_code.Check.findings ())

(* an even-length short sha must classify as a rev: the lenient hex parser
 * would zero-pad it into a wrong full commit id *)
let test_short_sha_is_a_rev () =
  Semgrep_envvars.with_envvar "SEMGREP_COMMIT" "deadbeef" (fun () ->
      let env = Git_metadata.env_from_environment () in
      match env._SEMGREP_COMMIT with
      | Some (Git_metadata.Commit_rev rev) ->
          Alcotest.(check string) "rev" "deadbeef" rev
      | Some (Git_metadata.Commit_sha _) ->
          Alcotest.fail "short sha parsed as a full commit id"
      | None -> Alcotest.fail "SEMGREP_COMMIT not read")

(* SEMGREP_COMMIT takes any git rev, not just a full commit id; the rev is
 * resolved to the commit it names and the scan proceeds *)
let test_commit_rev (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "SEMGREP_COMMIT" "HEAD" (fun () ->
      run_ci caps ~rule:blocking_rule_content ~target:finding_py_content
        ~check:Exit_code.Check.findings ())

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

(* the CircleCI pull request id is the url's last nonempty segment, found
 * even when the url has a trailing slash *)
let test_circleci_pr_id_trailing_slash (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "CIRCLE_PULL_REQUEST"
    "https://github.com/org/repo/pull/17/" (fun () ->
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
      let meta =
        new Circleci_metadata.meta
          (caps :> < Cap.exec >)
          ~cli_baseline_ref:None git_env
      in
      Alcotest.(check (option string)) "pr id" (Some "17") meta#pr_id)

let test_circleci_environment (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "CIRCLECI" "true" (fun () ->
      Semgrep_envvars.with_envvar "CIRCLE_PULL_REQUEST"
        "https://github.com/org/repo/pull/17/" (fun () ->
          run_ci caps ~rule:blocking_rule_content ~target:clean_py_content ()))

(* the job token spliced into the merge-base fetch URL must not reach the
 * logs: the command is logged with the URL credentials redacted, at info
 * level (--verbose) and in the failure warning *)
let test_gitlab_fetch_token_redacted (caps : Ci_subcommand.caps) () =
  Semgrep_envvars.with_envvar "GITLAB_CI" "true" (fun () ->
      Semgrep_envvars.with_envvar "CI_PIPELINE_SOURCE" "merge_request_event"
        (fun () ->
          Semgrep_envvars.with_envvar "CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
            "main" (fun () ->
              (* .invalid is reserved (RFC 2606): the fetch fails at DNS
                 resolution, nothing is contacted *)
              Semgrep_envvars.with_envvar "CI_MERGE_REQUEST_PROJECT_URL"
                "https://gitlab.invalid/org/repo" (fun () ->
                  Semgrep_envvars.with_envvar "CI_JOB_TOKEN"
                    "fake-64_wFuiRFQk9t841JHKQnAT" (fun () ->
                      run_ci caps ~rule:blocking_rule_content
                        ~target:clean_py_content
                        ~extra_args:[ "--verbose" ]
                        ())))))

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
      t "scalar block action exits with findings"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_scalar_blocking_findings caps);
      t "scalar non-block action exits ok" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_scalar_nonblocking_findings caps);
      t "audit mode exits ok despite findings"
        ~checked_output:(Testo.stdxxx ()) ~normalize (test_audit_mode caps);
      t "SEMGREP_AUDIT_ON is a whitespace-separated list"
        ~checked_output:(Testo.stdxxx ()) ~normalize (test_audit_env_list caps);
      t "SEMGREP_RULES is a whitespace-separated list"
        ~checked_output:(Testo.stdxxx ()) ~normalize (test_rules_env_list caps);
      t "subdir outside cwd is suppressed to ok"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_outside_cwd_suppressed caps);
      t "subdir outside cwd is fatal without suppression"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_outside_cwd_fatal caps);
      t "nonexistent subdir is reported as not found"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_nonexistent_suppressed caps);
      t "nonexistent subdir is fatal without suppression"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_subdir_nonexistent_fatal caps);
      t "output conflict is suppressed to ok" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_output_conflict_suppressed caps);
      t "output conflict is fatal without suppression"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_output_conflict_fatal caps);
      t "suppress-errors env var set to false"
        ~checked_output:(Testo.stdxxx ()) ~normalize
        (test_suppress_errors_env_false caps);
      t "short SEMGREP_COMMIT is a rev" test_short_sha_is_a_rev;
      t "SEMGREP_COMMIT accepts any rev" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_commit_rev caps);
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
      t "circleci pr id survives a trailing slash"
        (test_circleci_pr_id_trailing_slash caps);
      t "circleci environment is detected" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_circleci_environment caps);
      t "gitlab fetch token is redacted in logs"
        ~checked_output:(Testo.stdxxx ()) ~normalize:normalize_commit_hashes
        (test_gitlab_fetch_token_redacted caps);
      t "gitlab environment is detected" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_gitlab_environment caps);
      t "github environment is detected" ~checked_output:(Testo.stdxxx ())
        ~normalize (test_github_environment caps);
    ]
