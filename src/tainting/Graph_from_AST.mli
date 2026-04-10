(* Special case for go *)
val extract_go_receiver_type : AST_generic.function_definition -> string option

val top_level_il_name :
  ?current_file:Fpath.t -> AST_generic.program -> IL.name

val module_candidates_of_path : Fpath.t -> string list list

val canonical_lookup_candidates :
  ?current_file:Fpath.t -> string list -> string list list

val build_call_graph :
  lang : Lang.t ->
  ?object_mappings : (AST_generic.name * AST_generic.name) list ->
  AST_generic.program ->
  Call_graph.G.t

val build_project_call_graph :
  lang:Lang.t ->
  (Target.path * AST_generic.program * (AST_generic.name * AST_generic.name) list) list ->
  Call_graph.G.t

val find_functions_containing_ranges :
  lang : Lang.t ->
  ?current_file:Fpath.t ->
  AST_generic.program ->
  Range.t list ->
  Function_id.t list
