type parsed = (Spacegrep.Doc_AST.t * Spacegrep.Src_file.t) option
(** The document of a target, or [None] when the target is skipped as binary
    or minified. *)

val parse_target :
  nested_formula:bool ->
  Rule_options_t.generic_comment_style option ->
  Fpath.t ->
  parsed

val equal_comment_style :
  Rule_options_t.generic_comment_style option ->
  Rule_options_t.generic_comment_style option ->
  bool

val matches_of_spacegrep :
  doc:parsed option ->
  Match_env.xconfig ->
  (Spacegrep.Pattern_AST.t * Xpattern.pattern_id * string) list ->
  Fpath.t ->
  Origin.t ->
  Core_profiling.times Core_result.match_result
