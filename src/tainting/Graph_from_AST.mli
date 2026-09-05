(* Special case for go *)
val extract_go_receiver_type : AST_generic.function_definition -> string option

val uses_new_keyword : Lang.t -> bool

(* Anchored at the AST's first real token so [Function_id.key] embeds the
   file path; engine checker and projidx must produce identical ids. *)
val top_level_name_of_ast : AST_generic.program -> IL.name

val build_call_graph :
  lang : Lang.t ->
  AST_generic.program ->
  Call_graph.G.t

(* Scope path, outermost to innermost. *)
type fn_id = Func_info.fn_id

type func_info = Func_info.t = {
  fn_id : fn_id;
  entity : AST_generic.entity option;
  fdef : AST_generic.function_definition;
}

val fn_id_of_entity :
  lang : Lang.t ->
  AST_generic.entity option ->
  fn_id ->
  AST_generic.function_definition ->
  fn_id option

val fn_id_to_node : fn_id -> Function_id.t option

val resolved_name_of_fn_id :
  ?allow_located_fake:bool -> fn_id -> AST_generic.resolved_name option

type fdef_edges = {
  calls : (fn_id * Tok.t) list;
  callbacks : (fn_id * Tok.t * IL.name option) list;
  unresolved_call_sites : int;
}

val extract_calls :
  lang : Lang.t ->
  ?all_funcs : func_info list ->
  ?func_lookup : Func_lookup.t ->
  ?type_state : Type_state.t ->
  ?caller_parent_path : fn_id ->
  AST_generic.function_definition ->
  fdef_edges

val extract_decorator_calls :
  lang : Lang.t ->
  ?all_funcs : func_info list ->
  ?func_lookup : Func_lookup.t ->
  ?type_state : Type_state.t ->
  ?caller_parent_path : fn_id ->
  AST_generic.attribute list ->
  (fn_id * Tok.t) list

val extract_toplevel_calls :
  lang : Lang.t ->
  ?all_funcs : func_info list ->
  ?func_lookup : Func_lookup.t ->
  ?type_state : Type_state.t ->
  AST_generic.program ->
  (fn_id * Tok.t) list

val extract_toplevel_hof_callbacks :
  lang : Lang.t ->
  ?all_funcs : func_info list ->
  ?func_lookup : Func_lookup.t ->
  AST_generic.program ->
  (fn_id * Tok.t) list

val find_functions_containing_ranges :
  lang : Lang.t -> AST_generic.program -> Range.t list -> Function_id.t list
