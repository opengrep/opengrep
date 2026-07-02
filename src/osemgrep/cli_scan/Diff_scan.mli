(* [explicit_targets] replaces the table of the targets named on the command
   line. The baseline scan names its targets relative to the current
   directory, another form than the command line's, and passes a table in
   that form: a target is looked up in it by its path, for the size and
   '.min.js' bypass and for '--scan-unknown-extensions'. *)
type diff_scan_func =
  ?explicit_targets:Find_targets.Explicit_targets.t ->
  Target_and_root.t list ->
  Rule.rules ->
  Core_result.result_or_exn

val scan_baseline :
  < Cap.chdir ; Cap.tmp > ->
  Profiler.t ->
  Find_targets.baseline_ref ->
  Target_and_root.t list ->
  Rule.rules ->
  explicit_targets:Find_targets.Explicit_targets.t ->
  diff_scan_func ->
  Core_result.result_or_exn
