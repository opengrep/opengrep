(* Gather metadata from Circle CI; the CIRCLE_* provider variables are read
 * from the environment directly, the SEMGREP_* overrides come from
 * Git_metadata.env *)

class meta :
  < Cap.exec > ->
  cli_baseline_ref:string option ->
  Git_metadata.env ->
  Git_metadata.meta_t
