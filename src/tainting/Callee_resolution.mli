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

val is_free_named : func_info -> string -> bool

val find_func_in_scope :
  func_info list -> IL.name option list -> string -> func_info option

val prefer_concrete : func_info list -> func_info list

val uses_new_keyword : Lang.t -> bool

val expr_of_type_name : AST_generic.type_ -> AST_generic.expr option

type construction_resolver =
  call_arity:int -> AST_generic.type_ -> fn_id option

type invocation_resolver =
  caller_parent_path:IL.name option list -> string -> fn_id option

val resolve_constructor_from_type :
  lang:Lang.t -> all_funcs:func_info list -> AST_generic.type_ -> fn_id option

type call = { arity : int; arg_types : AST_generic.type_ option list }
(** A call as the resolver sees it: the number of its arguments and the type
    of each, where the declaration of an argument variable gives one. *)

val call_of_arguments : lang:Lang.t -> AST_generic.argument list -> call

val call_key : call -> int * AST_generic.canonical_name option list
(** The arity and the argument classes of a call, which decide its
    resolution: a key for a memo of resolutions. *)

type call_site_resolver =
  ?func_lookup:Func_lookup.t ->
  ?caller_parent_path:IL.name option list ->
  ?call:call ->
  ?allow_constructor:bool ->
  AST_generic.expr ->
  fn_id option

val identify_callee :
  lang:Lang.t ->
  ?all_funcs:func_info list ->
  ?func_lookup:Func_lookup.t ->
  ?type_state:Type_state.t ->
  ?caller_parent_path:IL.name option list ->
  ?call:call ->
  ?allow_constructor:bool ->
  AST_generic.expr ->
  fn_id option

val constructor_of_class :
  lang:Lang.t ->
  func_lookup:Func_lookup.t ->
  Names.Class_qn.t ->
  func_info list

val identify_callee_interfile :
  lang:Lang.t ->
  type_state:Type_state.t ->
  ?func_lookup:Func_lookup.t ->
  ?caller_parent_path:IL.name option list ->
  ?call:call ->
  ?allow_constructor:bool ->
  AST_generic.expr ->
  fn_id option
