(*
   Matching of a glob pattern against a path.
   This is purely syntactic: the file system is not accessed.
*)

(* A compiled pattern matcher. *)
type compiled_pattern [@@deriving show]

(* The location of a pattern, for logging and troubleshooting. *)
type loc = {
  (* File name or other source location name useful to a human reader
     in error messages. *)
  source_name : string;
  (* Classify the source if you wish, for better error messages. *)
  source_kind : string option;
  (* Line number, starting from 1. *)
  line_number : int;
  line_contents : string;
}
[@@deriving show]

(*
   Compile the pattern into a PCRE2 regular expression. The source should
   be the original glob pattern before parsing; it is kept for the logs
   and for the reason a target is skipped
   (Find_targets.get_reason_for_exclusion reads its source kind).
   With [match_leading_period], '*' and '?' at the start of a path segment
   match a leading period, as in gitignore, and a segment that does not
   begin with a literal '.' does not match the segments '.' and '..';
   otherwise a leading period is matched only by a literal '.', as in a
   shell without 'dotglob'.
   May raise Glob.Lexer.Syntax_error when PCRE2 rejects the translation.
*)
val compile :
  match_leading_period:bool -> source:loc -> Pattern.t -> compiled_pattern

(*
   Match a path against a pattern:
   - The pattern is anchored: the beginning of the pattern must match
     the beginning of the path, and the end of the pattern must match
     the end of the path.
   - The path must be slash-separated (not backslash-separated like on
     Windows).
   - If the pattern starts with a slash, the path must start with a slash
     as well. In both cases, the matching starts from the beginning.
   - Matching is purely syntactic. No file system lookup will be attempted.

   Examples:

   absolute pattern: /*.c
   matching paths: /foo.c /bar.c
   non-matching paths: foo.c bar.c /tmp/foo.c /tmp/bar.c

   relative pattern: *.c
   matching paths: bar.c
   non-matching paths: /bar.c foo.c/bar bar/foo.c

   sliding pattern: **/*.c
   matching paths: foo.c bar/foo.c /foo.c
   non-matching paths: foo.c/bar

   folder pattern: foo/
   matching paths: foo/
   non-matching paths: foo bar/foo/ /foo/ /foo
*)
val run : compiled_pattern -> string -> bool

(* Create a location from a pattern string rather than a location in a file. *)
val string_loc :
  ?source_name:string -> source_kind:string option -> string -> loc

val source : compiled_pattern -> loc
