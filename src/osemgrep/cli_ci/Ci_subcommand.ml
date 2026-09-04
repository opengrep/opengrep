module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse an opengrep-ci command, execute it and exit.
 *
 * Translated from the ci command of ci.py: detect the CI environment,
 * run the scan of the current repository (diffed against the merge-base in
 * PR/MR context), split the findings into blocking and non-blocking, and
 * exit 1 when blocking findings remain.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* Cap.exec is for git, Cap.network for rule fetching and the GitHub
 * merge-base API; the rest is Scan_subcommand.caps. *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(* excluded via --exclude regardless of other ignore configuration *)
let always_exclude_patterns = [ ".semgrep/"; ".semgrep_logs/" ]

(* excluded via --exclude when the project has its own .semgrepignore;
 * ci.py (get_exclude_paths) keeps this surprising condition with a comment
 * that "This logic isn't clear to me ... leaving it not to potentially
 * break things" *)
let default_exclude_patterns = [ "test/"; "tests/"; "*_test.go" ]

(*****************************************************************************)
(* Project metadata *)
(*****************************************************************************)

(* from generate_meta_from_environment in meta.py, minus the app-managed
 * scanning *)
let generate_meta_from_environment (caps : < Cap.exec ; Cap.network >)
    ~(cli_baseline_ref : string option) ~(subdir : string option) :
    Git_metadata.meta_t =
  let get = Opengrep_env.getenv_opt in
  let is_true var =
    match get var with
    | Some "true" -> true
    | _else_ -> false
  in
  let env = Git_metadata.env_from_environment () in
  let caps_exec = (caps :> < Cap.exec >) in
  if is_true "GITHUB_ACTIONS" then
    new Github_metadata.meta
      caps ?subdir ~cli_baseline_ref env
      (Github_metadata.env_from_environment ())
  else if is_true "GITLAB_CI" then
    new Gitlab_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else if is_true "CIRCLECI" then
    new Circleci_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else if Option.is_some (get "JENKINS_URL") then
    new Jenkins_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else if Option.is_some (get "BITBUCKET_BUILD_NUMBER") then
    new Bitbucket_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else if Option.is_some (get "BUILD_BUILDID") then
    new Azure_pipelines_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else if is_true "BUILDKITE" then
    new Buildkite_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else if is_true "TRAVIS" then
    new Travis_metadata.meta caps_exec ?subdir ~cli_baseline_ref env
  else
    new Git_metadata.meta
      caps_exec ?subdir ~scan_environment:"git" ~cli_baseline_ref env

(* "GHA can checkout the incorrect commit for a PR (it will create a fake
 * merge commit), so we need to reset the head to the actual PR branch head
 * before continuing." (fix_head_if_github_action in ci.py; the original
 * checkout is restored when the command exits) *)
let fix_head_if_github_action (caps : < Cap.exec >)
    (meta : Git_metadata.meta_t) : unit =
  if meta#is_pull_request_event then
    match meta#head_branch_hash with
    | Some head ->
        Logs.app (fun m -> m "Fixing git state for github action pull request");
        let stashed_rev = Git_wrapper.command caps [ "rev-parse"; "HEAD" ] in
        Logs.app (fun m ->
            m "Not on head ref: %a; checking that out now." Digestif.SHA1.pp
              head);
        let _ =
          Git_wrapper.command caps [ "checkout"; Digestif.SHA1.to_hex head ]
        in
        (* the hook runs at exit from whatever directory the process has
         * then, so the restore names the directory of the checkout *)
        let cwd = Sys.getcwd () in
        Hooks.exit :=
          (fun () ->
            ignore
              (Git_wrapper.command caps [ "-C"; cwd; "checkout"; stashed_rev ]))
          :: !Hooks.exit
    | None ->
        (* pyopengrep dies on an assert here; a warning serves better *)
        Logs.warn (fun m ->
            m
              "The GitHub event reports a pull request but no head commit; \
               leaving the checkout as it is")

let report_scan_environment (meta : Git_metadata.meta_t) : unit =
  Logs.app (fun m -> m "%a" Fmt_.pp_heading "Debugging Info");
  Logs.app (fun m -> m "  %a" Fmt.(styled `Underline string) "SCAN ENVIRONMENT");
  Logs.app (fun m ->
      m "  versions    - opengrep %a on OCaml %a"
        Fmt.(styled `Bold string)
        Version.version
        Fmt.(styled `Bold string)
        Sys.ocaml_version);
  Logs.app (fun m ->
      m "  environment - running in environment %a, triggering event is %a@."
        Fmt.(styled `Bold string)
        meta#scan_environment
        Fmt.(styled `Bold string)
        meta#event_name)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* like in ci.py: --subdir must name a directory under the current one; the
 * comparison is on resolved paths so symlinks do not confuse it *)
let resolve_subdir (subdir : string option) :
    (string option, Exit_code.t) result =
  match subdir with
  | None -> Ok None
  | Some dir -> (
      let err () =
        Logs.app (fun m ->
            m
              "`opengrep ci --subdir` must be given a directory that is \
               actually a subdirectory of the current directory");
        Error (Exit_code.fatal ~__LOC__)
      in
      match (Rfpath.of_string dir, Rfpath.of_string ".") with
      | Ok subdir_real, Ok cwd_real -> (
          let subdir_real = Rpath.to_fpath (Rfpath.to_rpath subdir_real) in
          let cwd_real = Rpath.to_fpath (Rfpath.to_rpath cwd_real) in
          if Fpath.equal subdir_real cwd_real then Ok (Some ".")
          else
            match Fpath.rem_prefix cwd_real subdir_real with
            | Some rel -> Ok (Some (Fpath.to_string rel))
            | None -> err ())
      (* the directory cannot be resolved because it does not exist (or is
       * not accessible) *)
      | Error _, _ ->
          Logs.err (fun m -> m "File not found: %s" dir);
          Error (Exit_code.fatal ~__LOC__)
      | _, Error _ -> err ())

let is_git_repo_root_approx () : bool =
  Sys.file_exists ".git" && Sys.is_directory ".git"

(* python get_exclude_paths(None) *)
let ci_excludes (user_excludes : string list) : string list =
  let semgrepignore_defaults =
    if
      Sys.file_exists Semgrepignore.default_semgrepignore_filename
      && not (Sys.is_directory Semgrepignore.default_semgrepignore_filename)
    then default_exclude_patterns
    else []
  in
  user_excludes @ always_exclude_patterns @ semgrepignore_defaults

let warn_ignored_flags (ci_conf : Ci_CLI.conf) : unit =
  if Option.is_some ci_conf.opengrep_ignore_pattern then
    Logs.app (fun m ->
        m
          "WARNING: --opengrep-ignore-pattern is set but will be ignored: all \
           results are returned by the ci command");
  if
    ci_conf.scan_conf.targeting_conf.apply_includes_excludes_to_file_targets
  then
    Logs.app (fun m ->
        m
          "WARNING: --force-exclude is set but will be ignored: no explicit \
           targets are passed to the ci command");
  if ci_conf.inline_metavariables then
    Logs.app (fun m ->
        m "WARNING: --inline-metavariables is set but will be ignored.")

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_ci_conf (caps : < caps ; .. >) (ci_conf : Ci_CLI.conf) : Exit_code.t =
  let conf = ci_conf.scan_conf in
  warn_ignored_flags ci_conf;
  (* inside the suppressed region: a bad destination is suppressed like any
   * other error *)
  Output.check_destinations conf.output_conf;
  match resolve_subdir ci_conf.subdir with
  | Error exit_code -> exit_code
  | Ok subdir -> (
      if not (is_git_repo_root_approx ()) then
        Logs.app (fun m ->
            m
              "WARNING: `opengrep ci` is meant to be run from the root of a \
               git repo.\n\
               When `opengrep ci` is not run from a git repo, it will not be \
               able to perform all operations.\n\
               When `opengrep ci` is run from a git repo, but not the root, \
               links in the uploaded findings may be broken.\n\n\
               To run `opengrep ci` on only a subdirectory of a git repo, see \
               `--subdir`.");
      let meta =
        generate_meta_from_environment
          (caps :> < Cap.exec ; Cap.network >)
          ~cli_baseline_ref:ci_conf.baseline_commit ~subdir
      in
      report_scan_environment meta;
      fix_head_if_github_action (caps :> < Cap.exec >) meta;
      (* the targeting the flags could not know: the current directory (or
       * --subdir) as the only root, the ci excludes, and the baseline from
       * the CI metadata *)
      (* not "./" ^ dir as in ci.py: pathlib normalises the "./" away but
       * Fpath does not, and the diff filter in Diff_scan.scan_baseline
       * compares against the plain relative paths git reports *)
      let target = Option.value subdir ~default:"." in
      let conf : Scan_CLI.conf =
        {
          conf with
          target_roots = [ Scanning_root.of_fpath (Fpath.v target) ];
          targeting_conf =
            {
              conf.targeting_conf with
              exclude = ci_excludes conf.targeting_conf.exclude;
              baseline_commit = meta#merge_base_ref;
            };
        }
      in
      let profiler = Profiler.make () in
      Profiler.start profiler ~name:"total_time";
      Core_profiling.profiling := conf.core_runner_conf.time_flag;
      let rules_and_origins, fatal_errors =
        Profiler.record profiler ~name:"config_time" (fun () ->
            Scan_subcommand.rules_from_rules_source
              (caps :> < Cap.network ; Cap.tmp >)
              ~skip_invalid_configs:conf.skip_invalid_configs
              ~rewrite_rule_ids:conf.rewrite_rule_ids
              ~strict:conf.core_runner_conf.strict
              (Rule_fetching.classify conf.rules_source))
      in
      match fatal_errors with
      | _ :: _ ->
          let core_errors =
            Scan_subcommand.core_errors_of_fatal_rule_errors fatal_errors
          in
          Scan_subcommand.output_and_exit_from_fatal_core_errors_exn
            ~text_message:(Rule_errors_report.invalid_configs_message core_errors)
            (caps :> < Cap.stdout >)
            conf profiler core_errors
      | [] -> (
          match
            Scan_subcommand.get_targets_or_exit
              (caps :> < Cap.stdout >)
              conf profiler
          with
          | Error exit_code -> exit_code
          | Ok targets_and_skipped -> (
              let res =
                Scan_subcommand.check_targets_with_rules ~print_summary:false
                  (caps
                    :> < Cap.stdout
                       ; Cap.chdir
                       ; Cap.tmp
                       ; Cap.fork
                       ; Cap.time_limit
                       ; Cap.memory_limit >)
                  conf profiler rules_and_origins targets_and_skipped
              in
              match res with
              | Error exit_code ->
                  Logs.app (fun m -> m "Encountered error when running rules");
                  exit_code
              | Ok (rules, _res, cli_output) ->
                  let num_blocking_findings =
                    cli_output.results
                    |> List.filter (fun (m : Out.cli_match) ->
                           Matches_report.is_blocking m.extra.metadata)
                    |> List.length
                  in
                  Logs.app (fun m -> m "CI scan completed successfully.");
                  Logs.app (fun m ->
                      m "  Found %s (%d blocking) from %s."
                        (String_.unit_str
                           (List.length cli_output.results)
                           "finding")
                        num_blocking_findings
                        (String_.unit_str (List.length rules) "rule"));
                  if num_blocking_findings > 0 then
                    if List.mem meta#event_name ci_conf.audit_on then (
                      Logs.app (fun m ->
                          m
                            "  Audit mode is on for %s, so exiting with code \
                             0 even if matches found"
                            meta#event_name);
                      Exit_code.ok ~__LOC__)
                    else (
                      Logs.app (fun m ->
                          m
                            "  Has findings for blocking rules so exiting \
                             with code 1");
                      Exit_code.findings ~__LOC__)
                  else (
                    Logs.app (fun m ->
                        m "  No blocking findings so exiting with code 0");
                    Exit_code.ok ~__LOC__))))

(*****************************************************************************)
(* Error suppression *)
(*****************************************************************************)

(* python: handle_command_errors + ErrorHandler.suppress. Any exit other
 * than ok or findings counts as an error and, unless --no-suppress-errors,
 * becomes a success. *)
let run_and_suppress_errors (caps : < caps ; .. >) (ci_conf : Ci_CLI.conf) :
    Exit_code.t =
  let exit_code =
    try run_ci_conf caps ci_conf with
    | Error.Semgrep_error (s, opt_exit_code) -> (
        Logs.err (fun m -> m "%s" s);
        match opt_exit_code with
        | None -> Exit_code.fatal ~__LOC__
        | Some code -> code)
    | Error.Exit_code code -> code
    (* coupling: CLI.safe_run maps the two exceptions below the same way *)
    (* a failed git command is already explained by Git_wrapper's own
     * warning; no backtrace needed *)
    | Git_wrapper.Error msg ->
        Logs.err (fun m -> m "%s" msg);
        Exit_code.fatal ~__LOC__
    | Common.UnixExit i ->
        Exit_code.of_int ~__LOC__ ~code:i ~description:"rogue UnixExit"
    | Failure msg ->
        Logs.err (fun m -> m "Error: %s%!" msg);
        Exit_code.fatal ~__LOC__
    | e ->
        let trace = Printexc.get_backtrace () in
        Logs.err (fun m ->
            m "Error: exception %s\n%s%!" (Printexc.to_string e) trace);
        Exit_code.fatal ~__LOC__
  in
  match Exit_code.to_int exit_code with
  | 0
  | 1 ->
      exit_code
  | _ when ci_conf.suppress_errors ->
      Logs.err (fun m ->
          m
            "There were errors during analysis but the scan will succeed \
             because there were no blocking findings, use \
             --no-suppress-errors if you want it to fail when there are \
             errors.");
      Exit_code.ok ~__LOC__
  | _ -> exit_code

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (ci_conf : Ci_CLI.conf) : Exit_code.t =
  let conf = ci_conf.scan_conf in
  CLI_common.setup_logging ~force_color:conf.output_conf.force_color
    ~level:conf.common.logging_level;
  Logs.info (fun m -> m "Opengrep version: %s" Version.version);
  Logs.debug (fun m -> m "conf = %s" (Ci_CLI.show_conf ci_conf));
  run_and_suppress_errors caps ci_conf

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Ci_CLI.parse_argv argv in
  run_conf caps conf
