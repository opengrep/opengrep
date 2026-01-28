(* Special case for go *)
val extract_go_receiver_type : AST_generic.function_definition -> string option

val build_call_graph :
  lang : Lang.t ->
  ?object_mappings : (AST_generic.name * AST_generic.name) list ->
  AST_generic.program ->
  Call_graph.G.t

val find_functions_containing_ranges :
  lang : Lang.t -> AST_generic.program -> Range.t list -> Function_id.t list
