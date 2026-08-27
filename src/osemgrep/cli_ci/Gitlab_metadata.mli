(* Gather metadata from GitLab CI (10.0+); the CI_* provider variables are
 * read from the environment directly, the SEMGREP_* overrides come from
 * Git_metadata.env *)

class meta :
  < Cap.exec > ->
  ?subdir:string ->
  cli_baseline_ref:string option ->
  Git_metadata.env ->
  Git_metadata.meta_t
