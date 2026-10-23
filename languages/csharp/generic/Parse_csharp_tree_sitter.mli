val parse :
  Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

(* [parse_string file text] parses [text], which has the lines and byte
   columns of [file]: positions are computed from [file]. *)
val parse_string :
  Fpath.t ->
  string ->
  (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t
