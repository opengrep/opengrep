(* A target file path paired with its absolute project root, resolved once
   during target discovery in Find_targets and used by the interfile taint
   engine to absolutize paths for comparison against AST token paths.

   Lives in semgrep.target so both semgrep.targeting (producer) and
   osemgrep_core (consumer via Lang_job) can reference it without a
   circular dependency. *)
type t = {
  target_fpath : Fpath.t;
  project_root : Fpath.t option;
}
