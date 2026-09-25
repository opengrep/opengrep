(* Function identifier as a path from outermost to innermost scope.
 * See [Graph_from_AST] for the orchestration that consumes these. *)
type fn_id = Func_info.fn_id
[@@deriving show, eq, ord]

(* Function information including its AST node, used as the resolution scope. *)
type func_info = Func_info.t = {
  fn_id : fn_id;
  entity : AST_generic.entity option;
  fdef : AST_generic.function_definition;
}

(* Graph node type - reuse from Call_graph for consistency *)
type node = Call_graph.node

val get_fn_name : fn_id -> IL.name option

val fn_id_to_node : fn_id -> node option

val prefer_concrete : func_info list -> func_info list


val uses_new_keyword : Lang.t -> bool

val expr_of_type_name : AST_generic.type_ -> AST_generic.expr option

type callee_use =
  | Name_use of AST_generic.SId.t
  | Member_use of {
      receiver : AST_generic.SId.t;
      receiver_type : AST_generic.type_ option;
      member : string;
    }

type static_type =
  | Declared_class of AST_generic.SId.t
  | Builtin_type of Type.builtin_type

val argument_types :
  lang:Lang.t ->
  type_of_call:(AST_generic.expr -> static_type option) ->
  AST_generic.argument list ->
  static_type option list

type static_typing = {
  type_of_call : AST_generic.expr -> static_type option;
  is_class : AST_generic.SId.t -> bool;
}

val narrow_by_call :
  lang:Lang.t ->
  typing:static_typing ->
  AST_generic.argument list option ->
  func_info list ->
  func_info list

val typing :
  lang:Lang.t ->
  resolve:(AST_generic.expr -> func_info list) ->
  is_class:(AST_generic.SId.t -> bool) ->
  static_typing

val static_type_of_argument :
  lang:Lang.t -> AST_generic.expr -> static_type option

module Callee_use_tbl :
  Hashtbl.S with type key = callee_use * static_type option list option

val use_binding : AST_generic.id_info -> AST_generic.SId.t option

val callee_use_of_name : AST_generic.id_info -> callee_use option

val callee_use_of_member :
  receiver:AST_generic.id_info -> string -> callee_use option

type call_site_resolver =
  caller_parent_path:IL.name option list ->
  call_args:AST_generic.argument list option ->
  AST_generic.expr ->
  fn_id list

type construction_resolver =
  call_args:AST_generic.argument list -> AST_generic.type_ -> fn_id list

type invocation_resolver =
  caller_parent_path:IL.name option list -> AST_generic.expr -> fn_id list

type argument_typer =
  caller_parent_path:IL.name option list ->
  AST_generic.argument list ->
  static_type option list

val resolve_outside_file :
  lang:Lang.t ->
  table:Symbol_table.t ->
  func_lookup:Func_lookup.t ->
  caller:Function_id.t option ->
  caller_parent_path:IL.name option list ->
  use:Symbol_table.use ->
  AST_generic.expr ->
  Symbol_table.resolution

val resolve_construction_outside_file :
  table:Symbol_table.t ->
  func_lookup:Func_lookup.t ->
  caller_parent_path:IL.name option list ->
  AST_generic.type_ ->
  Symbol_table.resolution

val extension_visible : func_lookup:Func_lookup.t -> string -> func_info -> bool
