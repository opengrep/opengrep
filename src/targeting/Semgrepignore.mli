(*
   Parse and interpret '.semgrepignore' files.

   The patterns they contain specify file paths to exclude from Semgrep scans.

   See the ml file for compatibility issues.
*)

(*
   We have to support the legacy built-in semgrepignore patterns
   when scanning for source code but we want something different
   or empty when scanning for secrets.

   The 'Empty' case is useful for testing.
*)
type default_semgrepignore_patterns = Empty | Semgrep_scan_legacy
[@@deriving show]

val default_semgrepignore_filename : string

type exclusion_mechanism = { use_semgrepignore_files : bool }

(*
   Initialize the data used to filter paths.
   The project_root path must exist. It is used to
   locate .semgrepignore files.

   This is an instantiation of Gitignore_filter.t specific to Semgrep.

   Use Project.find_any_project_root to determine the root of the
   project.

   The '.semgrepignore' of a folder applies to the paths under that folder,
   like a '.gitignore' does. The folder the command runs from is not
   special: its file applies to what it holds, not to a scanning root
   elsewhere.
*)
val create :
  ?cli_patterns:string list ->
  ?semgrepignore_filename:string ->
  default_semgrepignore_patterns:default_semgrepignore_patterns ->
  exclusion_mechanism:exclusion_mechanism ->
  project_root:Fpath.t ->
  unit ->
  builtin_patterns_root:Ppath.t ->
  Gitignore.filter
(* The function returned gives the filter of a scanning root: the built-in
   patterns apply from [builtin_patterns_root] downwards, the folder given
   as a target, or the project root. Its filters share the ignore files of
   the project. *)
