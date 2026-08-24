type diff_scan_func =
  Fpath.t list -> Rule.rules -> Core_result.result_or_exn

val scan_baseline :
  < Cap.chdir ; Cap.tmp > ->
  Profiler.t ->
  Find_targets.baseline_ref ->
  Fpath.t list ->
  Rule.rules ->
  diff_scan_func ->
  Core_result.result_or_exn
