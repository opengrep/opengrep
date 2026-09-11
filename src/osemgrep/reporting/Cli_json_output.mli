module Out = Semgrep_output_v1_j

(* Entry point.
 * when fixed_lines is true, the JSON output contains the fixed lines
 * (and in Scan_subcommand.ml we do not apply the autofix on the files).
 *)
val cli_output_of_runner_result :
  fixed_lines:bool ->
  (* those 3 parameters are essentially Core_runner.result *)
  Out.core_output ->
  Rule.hrules ->
  Fpath.t Set_.t ->
  Out.cli_output

(* internals used in Scan_subcommant.ml *)
val exit_code_of_error_type : Out.error_type -> Exit_code.t

(* Whether an error of this severity fails a run on its own: an Error does,
 * a warning or an info does not. Used by the scan subcommand. *)
val is_real_error_severity : Out.error_severity -> bool

(* The error whose code a failed run exits with: the last one of severity
 * Error, or None when every error is a warning or below. Used by the scan
 * and validate subcommands. *)
val last_real_error : Core_error.t list -> Core_error.t option

(* The exit code of a run that collected these errors, looking at the last
 * one: its code when its severity is Error, and, with ~strict, its code
 * whatever its severity. Ok when there is no error, and, without ~strict,
 * when the last one is a warning or below. Used by the scan and test
 * subcommands. *)
val exit_code_of_errors : strict:bool -> Out.core_error list -> Exit_code.t

(* used by the test subcommand for the errors of a rule file *)
val cli_error_of_core_error : Out.core_error -> Out.cli_error

(* internals used also for incremental display of matches. The Fixed_lines.env is
 * used for deciding whether a fixed_lines elements is included in the
 * cli_match. This depends on whether an overlapping fix was already included in
 * an earlier cli_match in the same list of matches. *)
(* [cwd] is the directory the match-based id makes the path relative to. The
 * caller reads it once, so that a run does not ask the system for it once
 * per match. *)
val cli_match_of_core_match :
  cwd:Fpath.t ->
  fixed_lines:bool ->
  Fixed_lines.env ->
  Rule.hrules ->
  Out.core_match ->
  Out.cli_match

val index_match_based_ids : Out.cli_match list -> Out.cli_match list
(** [index_match_based_ids matches] will append an index to the match based id
  * where the index is what # finding of the same rule kind in the same file
  * it is. This is needed for the App to do deduplication
  *)
