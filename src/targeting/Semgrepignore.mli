(*
   Parse and interpret '.semgrepignore' files in addition to '.gitignore'
   files.

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

type exclusion_mechanism = {
  use_gitignore_files : bool;
  use_semgrepignore_files : bool;
}

(*
   Initialize the data used to filter paths.
   The project_root path must exist. It is used to
   locate .gitignore and .semgrepignore files.

   This is an instantiation of Gitignore_filter.t specific to Semgrep.

   Use Git_project.find_project_root to determine the root of the
   git project.

   working_directory is the folder the command runs from, when it is not
   the project root: the patterns of its ignore file that match at any
   depth apply to the paths outside it, as they did for pysemgrep.
*)
val create :
  ?cli_patterns:string list ->
  ?working_directory:Fpath.t ->
  ?semgrepignore_filename:string ->
  default_semgrepignore_patterns:default_semgrepignore_patterns ->
  exclusion_mechanism:exclusion_mechanism ->
  project_root:Fpath.t ->
  unit ->
  Gitignore.filter
