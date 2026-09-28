val parse :
  Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t

val self_type_name : string
(** The strict keyword [Self], which the grammar parses as an identifier. *)

val is_self_type : AST_generic.type_ -> bool
(** The type is the identifier [Self]. *)

val is_shorthand_receiver : AST_generic.parameter_classic -> bool
(** The receiver is written [self], [mut self], [&self] or [&mut self]. *)
