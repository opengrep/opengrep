(* This module is essentially an adapter around Core_scan.scan()
 * (and Deep_scan.scan()) so that it can be used from osemgrep. We try to
 * imitate what is done in pysemgrep which calls semgrep-core
 * (or semgrep-core-proprietary) and its underlying Core_scan (or Deep_scan).
 *)

(* input *)
type conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  max_match_per_file : int;
  timeout : float;
  dynamic_timeout : bool;
  dynamic_timeout_max_multiplier : int;
  dynamic_timeout_unit_kb : int;
  allow_rule_timeout_control : bool;
  timeout_threshold : int;
  (* time limit of the interfile analysis of a rule *)
  interfile_timeout : int;
  (* features *)
  nosem : bool;
  strict : bool;
  (* useful for debugging rules *)
  time_flag : bool;
  inline_metavariables : bool;
  matching_explanations : bool;
  dataflow_traces : bool;
  taint_intrafile : bool;
  effect_guards : bool;
  (* a rule runs interfile only when this is set and its own options set
   * taint_interfile; otherwise that option is ignored *)
  enable_interfile : bool;
  taint_interfile_depth : int;
  (* override taint_intrafile, enable_interfile and the rule options
   * taint_intrafile and taint_interfile *)
  disable_intrafile : bool;
  interfile_dedup_by : Core_match.interfile_dedup_by;
  (* Engine configuration for various features *)
  engine_config : Engine_config.t;
}
[@@deriving show]

(* output *)
type result = {
  core : Semgrep_output_v1_t.core_output;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
  interfile_dedup_by : Core_match.interfile_dedup_by;
}

(* This type is similar to Core_scan.func, but taking a list of
 * rules and targets and a simpler conf type instead of the very
 * large Core_scan_config.t
 *
 * The list of targets below are final targets (not scanning roots).
 * no further scanning of the filesystem shall be performed.
 * The Find_targets.conf argument is for explicit target management.
 * git_repo says whether the targets came from git, for the scan status.
 * on_plan receives the plan once the language jobs pair targets with rules;
 * the runner prints nothing itself, and builds no plan without on_plan.
 * on_lang_jobs receives the language jobs at the same point, before on_plan
 * and before the engine runs.
 *)
type func = {
  run :
    ?file_match_hook:(Fpath.t -> Core_result.matches_single_file -> unit) ->
    ?on_plan:(Scan_plan.t -> unit) ->
    ?on_lang_jobs:(Lang_job.t list -> unit) ->
    ?progress_hook:(Core_scan_config.progress -> unit) ->
    git_repo:bool ->
    scanning_roots:Scanning_root.directory list ->
    conf ->
    Find_targets.conf ->
    Match_patterns.matching_conf ->
    Rule_error.rules_and_invalid ->
    Target_and_root.t list ->
    Core_result.result_or_exn;
}

val default_conf : conf

(* The interfile rules among [rules] that run per file because
   [conf.enable_interfile] is not set; none under [conf.disable_intrafile].
   [rules] are those before [restrict_rule_taint_modes]. *)
val rules_run_per_file_without_enable_interfile :
  conf -> Rule.rules -> Rule_ID.t list

(* The languages of [lang_jobs] that have a target and a taint rule that
   runs the cross-function analysis, but for which that analysis is not
   supported (Lang_config.supports_intrafile). The rules are those of
   [restrict_rule_taint_modes]. *)
val intrafile_unsupported_languages : conf -> Lang_job.t list -> Lang.t list

(* The rules with their taint_intrafile/taint_interfile options set by
   Rule.restrict_taint_modes from [conf.taint_intrafile],
   [conf.disable_intrafile] and [conf.enable_interfile]. *)
val restrict_rule_taint_modes : conf -> Rule.rules -> Rule.rules

(* builder *)
val mk_result :
  ?inline:bool ->
  ?interfile_dedup_by:Core_match.interfile_dedup_by ->
  Rule.rule list ->
  Core_result.t ->
  result

(* Core_scan.func adapter to be used in osemgrep.

   This will eventually call a core scan like pysemgrep but without
   creating a subprocess.

   The first argument is usually Core_scan.scan, but it can also be
   Deep_scan.scan when running in Pro Interfile mode and when called from
   Steps_scan.scan in Semgrep Pro.

   LATER: This function should go away.
*)
val mk_core_run_for_osemgrep : Core_scan.func -> func


(* Helper used in Test_subcommand.ml: the targets of the files given and
   the rules as a scan runs them *)
val targets_and_rules_for_files :
  Fpath.t list -> Rule.t list -> Target.t list * Rule.t list

(* the targets of each language of the rules *)
val split_jobs_by_language :
  Find_targets.conf -> Rule.t list -> Target_and_root.t list -> Lang_job.t list
