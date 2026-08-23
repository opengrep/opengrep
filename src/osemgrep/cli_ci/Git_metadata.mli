(* Gather metadata from local filesystem (we expect, at least, a Git
    repository) of from semgrep-specific environment variables
*)

type env = {
  _SEMGREP_REPO_NAME : string option;
  _SEMGREP_REPO_DISPLAY_NAME : string option;
  _SEMGREP_REPO_URL : Uri.t option;
  _SEMGREP_COMMIT : Digestif.SHA1.t option;
  _SEMGREP_JOB_URL : Uri.t option;
  _SEMGREP_PR_ID : string option;
  _SEMGREP_PR_TITLE : string option;
  _SEMGREP_BRANCH : string option;
}

(* Read via Opengrep_env: the OPENGREP_* alias of each variable wins over
 * the SEMGREP_* name, and empty values count as unset. Aborts when
 * SEMGREP_COMMIT is set but not a full commit id. *)
val env_from_environment : unit -> env

(* For the provider subclasses: the OPENGREP_*/SEMGREP_* override wins over
 * the provider's own variable, which is read via Opengrep_env (an empty
 * value counts as unset, like pyopengrep's falsy os.getenv values). *)
val override_or_getenv : string option -> string -> string option
val uri_override_or_getenv : Uri.t option -> string -> Uri.t option

val sha_override_or_getenv :
  Digestif.SHA1.t option -> string -> Digestif.SHA1.t option

(* The surface shared by all the provider metadata classes; every method
 * is there to be overridden in the children.
 * is_pull_request_event and head_branch_hash are meaningful on GitHub
 * Actions only (false/None elsewhere); they let 'opengrep ci' fix up the
 * checked-out head in PR context. *)
class type meta_t = object
  method scan_environment : string
  method project_metadata : Semgrep_output_v1_t.project_metadata
  method branch : string option
  method ci_job_url : Uri.t option
  method commit_sha : Digestif.SHA1.t option
  method event_name : string
  method is_full_scan : bool
  method pr_id : string option
  method pr_title : string option
  method repo_name : string
  method repo_display_name : string
  method repo_url : Uri.t option
  method merge_base_ref : Find_targets.baseline_ref option
  method is_pull_request_event : bool
  method head_branch_hash : Digestif.SHA1.t option
end

(* cli_baseline_ref is the raw --baseline-commit rev (any git rev, not
 * necessarily a commit id); merge_base_ref classifies it, or in children
 * computes the base from the CI provider's machinery.
 * subdir is the --subdir of 'opengrep ci', qualifying repo_display_name. *)
class meta :
  < Cap.exec > ->
  ?subdir:string ->
  scan_environment:string ->
  cli_baseline_ref:string option ->
  env ->
  meta_t
