(*
   'opengrep ci' command-line parsing.
*)

type conf = {
  (* exit 0 for the events named here even with blocking findings *)
  audit_on : string list;
  (* errors other than findings exit 0 (python: ErrorHandler.suppress) *)
  suppress_errors : bool;
  (* scan only this subdirectory of the current directory *)
  subdir : string option;
  (* the raw --baseline-commit rev, handed to the metadata layer as the
   * cli_baseline_ref (the scan's baseline is set from the metadata) *)
  baseline_commit : string option;
  (* accepted like in ci.py but ignored by ci, warned about at runtime;
   * their Scan_CLI.conf counterparts are scrubbed to the defaults *)
  opengrep_ignore_pattern : string option;
  inline_metavariables : bool;
  (* 'opengrep ci' shares most of its flags with 'opengrep scan' *)
  scan_conf : Scan_CLI.conf;
}
[@@deriving show]

(*
   Usage: parse_argv [| "opengrep-ci"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
