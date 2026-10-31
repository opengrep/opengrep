(* In each parser, [python2] holds when the string literals follow Python 2. *)

val parse :
  python2:bool ->
  Fpath.t ->
  (AST_python.program, unit) Tree_sitter_run.Parsing_result.t

val parse_string :
  python2:bool ->
  file:string (* filename *) ->
  contents:string ->
  (AST_python.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  python2:bool -> string -> (AST_python.any, unit) Tree_sitter_run.Parsing_result.t
