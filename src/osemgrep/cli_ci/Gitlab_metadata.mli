(* Gather metadata from GitLab CI (10.0+); the CI_* provider variables are
 * read from the environment directly, the SEMGREP_* overrides come from
 * Git_metadata.env *)

(* Fetch the merge-request target branch with the job token (from the
 * CI_* variables) and return its merge base with head_sha. The token
 * stays off the command line; supports_config_env is the result of the
 * Git_wrapper probe, a parameter so tests can force the old-git path. *)
val fetch_branch_get_merge_base :
  < Cap.exec > ->
  supports_config_env:bool ->
  branch_name:string ->
  head_sha:string ->
  Digestif.SHA1.t option

class meta :
  < Cap.exec > ->
  ?subdir:string ->
  cli_baseline_ref:string option ->
  Git_metadata.env ->
  Git_metadata.meta_t
