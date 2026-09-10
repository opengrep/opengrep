(* One error of the scan as a line of the text output: the level label, the
   message and its location. Printed on stderr, before the findings. *)
val pp_cli_error : Format.formatter -> Semgrep_output_v1_t.cli_error -> unit

(* The errors the text output reports: a warning only with --verbose, and
   never a timeout or a missing plugin, which have their own lines. *)
val cli_errors_to_report :
  verbose:bool ->
  Semgrep_output_v1_t.cli_error list ->
  Semgrep_output_v1_t.cli_error list

(* The files a scan left out or could not finish, counted and worded.
   Reads the filesystem to tell an ignored directory from an ignored file. *)
val summary_of_skipped :
  respect_gitignore:bool ->
  is_git_repo:bool ->
  is_baseline_scan:bool ->
  maturity:Maturity.t ->
  max_target_bytes:int ->
  skipped_groups:Skipped_report.skipped_targets_grouped ->
  unplaced_warnings:int ->
  unit ->
  Skin_model.Summary.t

(* The legacy rendering of the above: the "Scan Summary" heading, which is
   printed even when nothing was left out, then the block of counts. *)
val pp_summary : Skin_model.Summary.t Fmt.t

(* The timeouts of the scan: one entry per file with the ids of the rules
   that timed out, sorted by path. *)
val timeouts_of_errors :
  timeout_threshold:int ->
  Semgrep_output_v1_t.cli_error list ->
  Skin_model.Timeouts.t

(* The legacy rendering of the above, and whether --timeout-threshold
   stopped a file. Printed on stderr in text mode. *)
val pp_timeout_warnings : Skin_model.Timeouts.t Fmt.t
