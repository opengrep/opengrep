module Out = Semgrep_output_v1_j

(* Display options *)
type conf = {
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  (* destination of the primary output_format: file or URL set with
   * -o/--output (None means stdout) *)
  output : string option;
  (* extra outputs set with --<format>-output=<destination>; the None key
   * means stdout *)
  outputs : (string option, Output_format.t) Map_.t;
  (* for Text *)
  max_chars_per_line : int;
  max_lines_per_finding : int;
  force_color : bool;
  (* for text and SARIF *)
  show_dataflow_traces : bool;
  (* misc *)
  strict : bool;
  (* a.k.a. dryrun in Scan_CLI.conf *)
  fixed_lines : bool;
  (* true when using --verbose or --debug in Scan_CLI.ml *)
  skipped_files : bool;
  (* Used when displaying rule ids or skipped files. If above the limit,
   * the entries will not be displayed and replaced by a <SKIPPED DATA>
   * in the log output.
   *)
  max_log_list_entries : int;
}
[@@deriving show]

val default : conf

(* used with max_log_list_entries *)
val too_much_data : string

(* All the (destination, format) pairs that will be produced: conf.outputs
 * plus the primary (conf.output, conf.output_format) pair.
 * Aborts (like pysemgrep's OutputSettings.normalize()) if the -o destination
 * is already used by a --<format>-output flag, so this is also called at
 * CLI-parsing time in Scan_CLI.ml to report conflicts before the scan starts.
 *)
val effective_outputs : conf -> (string option, Output_format.t) Map_.t

(* Output the core results on stdout (and in the files given by
 * -o/--output and --<format>-output) depending on flags in conf.
 *
 * The format_context are parameters that are determined at runtime
 * that can also affect the output. For example, if a user is not logged in,
 * then in the SARIF output format, we include a message to nudge the user
 * to log in and try Pro.
 *)
val output_result :
  < Cap.stdout > ->
  conf ->
  Profiler.t ->
  Core_runner.result ->
  Out.cli_output

(* helper used in output_result() and other callsites.
 * This handles nosemgrep, interpolating messages, and more.
 *)
val preprocess_result : fixed_lines:bool -> Core_runner.result -> Out.cli_output

(* used by RPC_return.ml for Vim/Emacs/Junit_xml/Gitlab_xxx for now *)
val format :
  ?profiler : Profiler.t ->
  Output_format.t -> Out.cli_output -> string list
