(* Function identifier as a path from outermost to innermost scope.
 * See [Graph_from_AST] for the orchestration that consumes these. *)
type fn_id = IL.name option list
[@@deriving show, eq, ord]

(* Function information including its AST node, used as the resolution scope. *)
type func_info = {
  fn_id : fn_id;
  entity : AST_generic.entity option;
  fdef : AST_generic.function_definition;
}

(* Graph node type - reuse from Call_graph for consistency *)
type node = Call_graph.node

val get_fn_name : fn_id -> IL.name option

val fn_id_to_node : fn_id -> node option

val func_info_name_matches : func_info -> string -> bool

val find_func_in_scope :
  func_info list -> IL.name option list -> string -> func_info option

val resolve_constructor_from_type :
  lang:Lang.t -> all_funcs:func_info list -> AST_generic.type_ -> fn_id option

val identify_callee :
  lang:Lang.t ->
  ?object_mappings:(AST_generic.name * AST_generic.name) list ->
  ?all_funcs:func_info list ->
  ?caller_parent_path:IL.name option list ->
  ?call_arity:int ->
  AST_generic.expr ->
  fn_id option
