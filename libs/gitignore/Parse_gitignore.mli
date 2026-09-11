(* Parsing functions. They will raise exceptions if the input is malformed.

   The anchor is the pattern that matches the path from the git project
   root to the work folder, typically the one containing the gitignore file.

   The default selection mode is Ignore.
*)

(* the patterns of an ignore file, given its path and its contents *)
val from_file_contents :
  anchor:Glob.Pattern.t ->
  format:Gitignore.format ->
  source_kind:string ->
  Fpath.t ->
  string ->
  Gitignore.path_selectors

val from_string :
  anchor:Glob.Pattern.t ->
  name:string ->
  source_kind:string ->
  string ->
  Gitignore.path_selectors

(* Same as from_string for the '--include' and '--exclude' patterns of the
   command line, which match anywhere in a path where a gitignore pattern
   containing a slash is anchored at the folder of its file. *)
val cli_patterns_from_string :
  anchor:Glob.Pattern.t ->
  name:string ->
  source_kind:string ->
  string ->
  Gitignore.path_selectors

(* Same as from_string but keeping only the patterns that are not anchored
   to the folder of the file, i.e. those that match at any depth and are
   therefore meaningful for a path outside that folder. [source_path] is
   the file the string comes from; its ':include' directives are expanded
   relative to that file's folder. *)
val unanchored_from_string :
  anchor:Glob.Pattern.t ->
  name:string ->
  source_kind:string ->
  source_path:Fpath.t option ->
  string ->
  Gitignore.path_selectors

(* Lower-level function that can be used to create custom matchers that
   combine multiple patterns. *)
val parse_pattern :
  source:Glob.Match.loc ->
  anchor:Glob.Pattern.t ->
  string ->
  Glob.Match.compiled_pattern

(* Same as parse_pattern for a pattern of the command line: it returns the
   patterns any of which selects the path. *)
val parse_cli_pattern :
  source:Glob.Match.loc ->
  anchor:Glob.Pattern.t ->
  string ->
  Glob.Match.compiled_pattern list
