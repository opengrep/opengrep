(* Parsing functions. [parse_pattern] and [parse_cli_pattern] raise
   Glob.Lexer.Syntax_error for a pattern that is not a valid glob; the
   functions that read lines print a warning for such a line and leave it
   out.

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

(* The pattern of an '--exclude' value of the command line, possibly
   negated: the value is one pattern, which is not split into lines, and
   in which neither a leading '#' nor a trailing space is special. *)
val from_cli_pattern :
  anchor:Glob.Pattern.t ->
  name:string ->
  source_kind:string ->
  string ->
  Gitignore.path_selectors

(* Lower-level function that can be used to create custom matchers that
   combine multiple patterns. The list is empty for a pattern that matches
   nothing, such as '/'. *)
val parse_pattern :
  source:Glob.Match.loc ->
  anchor:Glob.Pattern.t ->
  string ->
  Glob.Match.compiled_pattern list

(* Same as parse_pattern for a pattern of the command line: it returns the
   patterns any of which selects the path. *)
val parse_cli_pattern :
  source:Glob.Match.loc ->
  anchor:Glob.Pattern.t ->
  string ->
  Glob.Match.compiled_pattern list

(* The reason a pattern of the command line, possibly negated, is not a
   valid pattern; None when it is valid. *)
val cli_pattern_error : string -> string option

(* The warning for the line [line] of [source_name], whose pattern is not a
   valid glob for the reason given, and which then matches nothing. *)
val warn_invalid_pattern :
  source_name:string -> line_number:int -> string -> string -> unit

(* The contents of the ignore file [path], or the reason they are left out:
   the file cannot be read, or it is in UTF-16. *)
val ignore_file_contents : Fpath.t -> (string, string) result

(* The warning for the ignore file [path], left out for the reason given. *)
val warn_ignored_file : Fpath.t -> string -> unit
