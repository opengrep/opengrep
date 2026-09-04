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

let setup_logging ~force_color ~level =
  (* The file of $OPENGREP_LOG_FILE (or $SEMGREP_LOG_FILE) gets a copy of
   * the logs at the same level as stderr, so that it costs nothing unless
   * --verbose or --debug is passed too. pysemgrep wrote its own logs there
   * at the debug level whatever the console level, and to
   * ~/.semgrep/semgrep.log when the variable was not set.
   *)
  let copy_to_file : Fpath.t option =
    Opengrep_env.getenv_opt "SEMGREP_LOG_FILE" |> Option.map Fpath.v
  in
  copy_to_file
  |> Option.iter (fun (file : Fpath.t) ->
         UFile.make_directories (Fpath.parent file));
  (* Colour is decided once for every output: --force-color (or
   * $SEMGREP_FORCE_COLOR) wins, then $NO_COLOR or $SEMGREP_FORCE_NO_COLOR
   * turns all styling off, otherwise the tty decides. Same precedence as
   * pysemgrep, which however applied it per piece of output.
   *)
  let highlight_setting : Console.highlight_setting =
    if force_color then On else if !Semgrep_envvars.v.no_color then Off else Auto
  in
  Log_semgrep.setup ?copy_to_file ~highlight_setting ~level ();
  Logs.debug (fun m ->
      m "Logging setup for osemgrep: force_color=%B level=%s" force_color
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
