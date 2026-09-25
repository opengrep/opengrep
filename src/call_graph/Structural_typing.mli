type method_ = {
  name : string;
  entity : AST_generic.entity option;
  fdef : AST_generic.function_definition;
}

type equal_type =
  required:AST_generic.type_ -> candidate:AST_generic.type_ -> bool option

val method_arity : lang:Lang.t -> method_ -> int

val method_satisfies :
  lang:Lang.t -> equal_type:equal_type -> required:method_ -> method_ -> bool
