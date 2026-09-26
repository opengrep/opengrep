type 't signature = {
  arity : int;
  parameters : 't option list option;
  return : 't option;
}

type 't equal_type = required:'t -> candidate:'t -> bool option

val signature :
  lang:Lang.t ->
  declared:(AST_generic.type_ -> 't) ->
  AST_generic.entity option ->
  AST_generic.function_definition ->
  't signature

val satisfies :
  equal_type:'t equal_type -> required:'t signature -> 't signature -> bool
