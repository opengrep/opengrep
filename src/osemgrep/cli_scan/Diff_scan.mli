(* [explicit_targets] replaces the table of the targets named on the command
   line. The baseline scan reads its targets from paths relative to the
   current directory, another form than the command line's, and passes a
   table in that form: a target is looked up in it by the path its content
   is read from, for the size and '.min.js' bypass and for
   '--scan-unknown-extensions'.
   [content_path] maps a target to the file its content is read from, when
   that is not the target itself: the baseline scan reads the files of the
   baseline worktree and reports and filters its targets as the head scan. *)
type diff_scan_func =
  ?explicit_targets:Find_targets.Explicit_targets.t ->
  ?content_path:(Fpath.t -> Fpath.t) ->
  scanning_roots:Scanning_root.directory list ->
  Target_and_root.t list ->
  Rule.rules ->
  Core_result.result_or_exn

(* [head_scan_func] runs the scan whose findings are reported (it may
   stream them incrementally); [baseline_scan_func] replays the baseline
   commit purely to build the dedup set, so it must NOT stream findings —
   pass one without a file_match_hook. *)
val scan_baseline :
  < Cap.chdir ; Cap.tmp > ->
  Scan_CLI.conf ->
  Profiler.t ->
  Find_targets.baseline_ref ->
  Target_and_root.t list ->
  Rule.rules ->
  explicit_targets:Find_targets.Explicit_targets.t ->
  scanning_roots:Scanning_root.directory list ->
  head_scan_func:diff_scan_func ->
  baseline_scan_func:diff_scan_func ->
  Core_result.result_or_exn
