(* A target file path paired with its absolute project root, resolved once
   during target discovery in Find_targets and used by the interfile taint
   engine to absolutize paths for comparison against AST token paths.

   Lives in semgrep.target so both semgrep.targeting (producer) and
   osemgrep_core (consumer via Lang_job) can reference it without a
   circular dependency. *)

(* The path that the 'paths:' of a rule are matched against: a file found
   under a directory target is matched by its path relative to the project
   root, a file given as a target by the path as given. *)
type target_proj_path = Not_in_project | In_project of Ppath.t
[@@deriving show]

type t = {
  target_fpath : Fpath.t;
  project_root : Fpath.t option;
  target_proj_path : target_proj_path;
}
