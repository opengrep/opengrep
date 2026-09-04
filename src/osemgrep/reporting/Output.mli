module Out = Semgrep_output_v1_j

(* Display options *)
type conf = {
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  (* destination of the primary output_format: file or URL set with
   * -o/--output (None means stdout) *)
  output : string option;
  (* extra outputs set with --<format>-output=<destination>. Those flags
   * always name a file, so the key is always Some; the option is there to
   * share the type with check_destinations, where None is stdout. *)
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
  (* true for 'opengrep ci': the Text format then keeps blocking and
   * non-blocking findings in separate groups and appends the
   * "RULES FIRED" sections *)
  is_ci_invocation : bool;
}
[@@deriving show]

val default : conf

(* used with max_log_list_entries *)
val too_much_data : string

(* Aborts on a destination we will not write to: a URL, a symlink, or one
 * named by both -o and a --<format>-output flag. Called at CLI-parsing time
 * so that the scan does not run first.
 *)
val check_destinations : conf -> unit

(* Whether any of the outputs in conf wants the nosem-ignored matches, so
 * that they must be left in the results instead of being filtered out.
 * Only SARIF wants them, as it reports them as suppressed.
 *)
val keeps_ignores : conf -> bool

(* Output the core results on stdout (and in the files given by
 * -o/--output and --<format>-output) depending on flags in conf.
 *
 * The format_context are parameters that are determined at runtime
 * that can also affect the output. For example, if a user is not logged in,
 * then in the SARIF output format, we include a message to nudge the user
 * to log in and try Pro.
 *)
val output_result :
  keep_ignored:bool ->
  < Cap.stdout > ->
  conf ->
  Profiler.t ->
  Core_runner.result ->
  Out.cli_output

(* helper used in output_result() and other callsites.
 * This handles nosemgrep, interpolating messages, and more.
 * keep_ignored keeps the matches a 'nosemgrep' comment suppressed; they are
 * dropped after the match-based ids are indexed, so that the index counts
 * them as pysemgrep's did.
 *)
val preprocess_result :
  fixed_lines:bool -> keep_ignored:bool -> Core_runner.result -> Out.cli_output

(* used by RPC_return.ml for Vim/Emacs/Junit_xml/Gitlab_xxx for now *)
val format :
  ?profiler : Profiler.t ->
  Output_format.t -> Out.cli_output -> string list
