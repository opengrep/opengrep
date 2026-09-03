type diff_scan_func =
  Target_and_root.t list -> Rule.rules -> Core_result.result_or_exn

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
  head_scan_func:diff_scan_func ->
  baseline_scan_func:diff_scan_func ->
  Core_result.result_or_exn
