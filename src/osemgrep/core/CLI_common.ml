open Cmdliner

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Shared CLI flags, CLI processing helpers, and help messages for the
   semgrep CLI.
*)

(*************************************************************************)
(* Types and constants *)
(*************************************************************************)

type conf = {
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
  (* osemgrep-only: pad poor's man profiling info for now *)
  profile : bool;
  (* osemgrep-only: mix of --experimental, --develop *)
  maturity : Maturity.t;
}
[@@deriving show]

(*************************************************************************)
(* Verbosity options (mutually exclusive) *)
(*************************************************************************)

(* alt: we could use Logs_cli.level(), but by defining our own flags
 * we can give better ~doc:. We lose the --verbosity=Level though.
 * TODO: maybe "findings" below is to cli_scan specific
 *)
let o_quiet : bool Term.t =
  let info = Arg.info [ "q"; "quiet" ] ~doc:{|Only output findings.|} in
  Arg.value (Arg.flag info)

(* TODO: same, maybe we should take the doc as a paramter so each
 * cli_xxx command can give a different help
 *)
let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        {|Show more details about what rules are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

let o_logging : Logs.level option Term.t =
  let combine debug quiet verbose =
    match (verbose, debug, quiet) with
    | false, false, false -> (* default *) Some Logs.Warning
    | true, false, false -> (* --verbose *) Some Logs.Info
    | false, true, false -> (* --debug *) Some Logs.Debug
    | false, false, true -> (* --quiet *) None
    | _ ->
        (* TOPORT: list the possibilities *)
        Error.abort "mutually exclusive options --quiet/--verbose/--debug"
  in
  Term.(const combine $ o_debug $ o_quiet $ o_verbose)

(*************************************************************************)
(* Color options *)
(*************************************************************************)

(* alt: could use Fmt_cli.style_renderer, which supports --color=xxx but
 * better be backward compatible with how semgrep was doing it before
 *)
(* [default] is the force_color of the caller's default output
 * configuration; this library cannot name that record itself. *)
let o_force_color ~(default : bool) : bool Term.t =
  Cmdliner_.negatable_flag_with_env [ "force-color" ]
    ~neg_options:[ "no-force-color" ] ~default
      (* NO_COLOR (https://no-color.org/) and SEMGREP_FORCE_NO_COLOR are
       * handled in setup_logging below; forcing colour wins.
       *)
    ~env:"SEMGREP_FORCE_COLOR"
    ~doc:
      {|Always include ANSI color in the output, even if not writing to
a TTY; defaults to using the TTY status
|}

(* Writing the logs to a side file is a diagnostic convenience: when the path
 * cannot be created or opened we warn and carry on rather than stop the scan.
 *)
let openable_log_file (file : Fpath.t) : (unit, string) result =
  try
    UFile.make_directories (Fpath.parent file);
    UFile.write_file ~file "";
    Ok ()
  with
  | Unix.Unix_error (err, _, (arg : string)) ->
      Error (Common.spf "%s: %s" arg (Unix.error_message err))
  | Sys_error (msg : string) -> Error msg

let setup_logging ~force_color ~level =
  (* The file of $OPENGREP_LOG_FILE (or $SEMGREP_LOG_FILE) gets a copy of
   * the logs at the same level as stderr, so that it costs nothing unless
   * --verbose or --debug is passed too. pysemgrep wrote its own logs there
   * at the debug level whatever the console level, and to
   * ~/.semgrep/semgrep.log when the variable was not set.
   *)
  let copy_to_file, log_file_error =
    match Opengrep_env.getenv_with_name_opt "SEMGREP_LOG_FILE" with
    | None -> (None, None)
    | Some ((var : string), (value : string)) -> (
        let file = Fpath.v value in
        match openable_log_file file with
        | Ok () -> (Some file, None)
        | Error (msg : string) -> (None, Some (var, msg)))
  in
  (* Colour is decided once for every output: --force-color (or
   * $SEMGREP_FORCE_COLOR) wins, then $NO_COLOR or $SEMGREP_FORCE_NO_COLOR
   * turns all styling off, otherwise the tty decides. Same precedence as
   * pysemgrep, which however applied it per piece of output.
   *)
  let highlight_setting : Console.highlight_setting =
    if force_color then On else if !Semgrep_envvars.v.no_color then Off else Auto
  in
  Log_semgrep.setup ?copy_to_file ~highlight_setting ~level ();
  log_file_error
  |> Option.iter (fun ((var : string), (msg : string)) ->
         Logs.warn (fun m ->
             m "cannot write the log file of $%s: %s; continuing without it"
               var msg));
  Logs.debug (fun m ->
      m "Logging setup for opengrep: force_color=%B level=%s" force_color
        (Logs.level_to_string level));
  Logs.debug (fun m ->
      m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "))

(*************************************************************************)
(* Profiling options *)
(*************************************************************************)

(* osemgrep-only:  *)
let o_profile : bool Term.t =
  let info = Arg.info [ "profile" ] ~doc:{|<undocumented>|} in
  Arg.value (Arg.flag info)

(*************************************************************************)
(* Term for all common CLI flags *)
(*************************************************************************)

let o_common : conf Term.t =
  let combine logging profile maturity =
    { logging_level = logging; profile; maturity }
  in
  Term.(const combine $ o_logging $ o_profile $ Maturity.o_maturity)

(*************************************************************************)
(* Misc *)
(*************************************************************************)

(* The exit codes opengrep really returns. Without them cmdliner documents its
 * own defaults (123, 124, 125), which no code path here ever produces.
 * coupling: Exit_code.ml and Cli_json_output.exit_code_of_error_type
 *)
let exit_ok = Cmd.Exit.info ~doc:"on success, with nothing to report." 0

let exit_findings =
  Cmd.Exit.info ~doc:"when findings are reported as errors, see $(b,--error)." 1

let exit_fatal =
  Cmd.Exit.info ~doc:"on a fatal error, including an error on the command line."
    2

let exit_invalid_code =
  Cmd.Exit.info ~doc:"when a target file could not be parsed." 3

let exit_invalid_pattern =
  Cmd.Exit.info ~doc:"when a rule pattern could not be parsed." 4

let exit_unparseable_yaml =
  Cmd.Exit.info ~doc:"when a rule file is not valid YAML." 5

let exit_missing_config =
  Cmd.Exit.info ~doc:"when no valid configuration could be loaded." 7

let exit_invalid_language =
  Cmd.Exit.info ~doc:"when a rule names a language that is not supported." 8

(* only for a subcommand that writes to stdout *)
let exit_broken_pipe =
  Cmd.Exit.info ~doc:"when the reader of the output closed the pipe." 141

(* The rules the subcommand could not load: Core_error.error_of_rule_error
 * gives each one a type and Cli_json_output.exit_code_of_error_type its
 * code. *)
let exits_of_invalid_rules =
  [
    exit_invalid_pattern;
    exit_unparseable_yaml;
    exit_missing_config;
    exit_invalid_language;
  ]

(* a scan adds the parse errors of its targets to the codes above *)
let exits_scan : Cmd.Exit.info list =
  [ exit_ok; exit_findings; exit_fatal; exit_invalid_code ]
  @ exits_of_invalid_rules
  @ [ exit_broken_pipe ]

(* 'ci' takes its code from the findings; a rule it cannot load makes the
 * configuration missing *)
let exits_ci : Cmd.Exit.info list =
  [ exit_ok; exit_findings; exit_fatal; exit_missing_config; exit_broken_pipe ]

(* a failed check is a finding; a rule file that does not load makes the
 * configuration missing *)
let exits_test : Cmd.Exit.info list =
  [ exit_ok; exit_findings; exit_fatal; exit_missing_config; exit_broken_pipe ]

(* 'validate' has no target to parse: only the rules and the matches of the
 * metachecking rules, which are fatal *)
let exits_validate : Cmd.Exit.info list =
  [ exit_ok; exit_fatal ] @ exits_of_invalid_rules @ [ exit_broken_pipe ]

let exits_show : Cmd.Exit.info list =
  [
    exit_ok;
    exit_fatal;
    exit_invalid_code;
    exit_invalid_pattern;
    exit_missing_config;
    exit_broken_pipe;
  ]

let exits_lsp : Cmd.Exit.info list = [ exit_ok; exit_fatal; exit_broken_pipe ]

(* 'install-ci' writes nothing to stdout *)
let exits_install_ci : Cmd.Exit.info list = [ exit_ok; exit_fatal ]

let help_page_bottom =
  [
    `S Manpage.s_bugs;
    `P
      "If you encounter an issue, please report it at\n\
      \      https://github.com/opengrep/opengrep/issues";
  ]

(* Small wrapper around Cmdliner.Cmd.eval_value.
 * Note that I didn't put this helper function in Cmdliner_helpers.ml because
 * it's using Exit_code.ml and Error.ml which are semgrep-specific.
 *)
let eval_value ~argv cmd =
  (* the ~catch:false is to let non-cmdliner exn (e.g., Error.Semgrep_error)
   * to bubble up; those exns will then be caught in CLI.safe_run.
   *)
  (* ~env makes cmdliner honor the OPENGREP_* alias of every SEMGREP_* env var
   * referenced via Cmd.Env.info (e.g., SEMGREP_RULES, SEMGREP_BASELINE_COMMIT),
   * with the OPENGREP_* name taking precedence when both are set. *)
  match Cmd.eval_value ~catch:false ~env:Opengrep_env.getenv_opt ~argv cmd with
  (* alt: could define a new Exit_code for those kinds of errors *)
  | Error (`Term | `Parse) -> Error.exit_code_exn (Exit_code.fatal ~__LOC__)
  (* this should never happen, because of the ~catch:false above *)
  | Error `Exn -> assert false
  | Ok ok -> (
      match ok with
      | `Ok config -> config
      | `Version
      | `Help ->
          Error.exit_code_exn (Exit_code.ok ~__LOC__))
