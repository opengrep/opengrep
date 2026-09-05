(* Emits Dispatch edges from the other members of an overload group (same
   scope, name and arity, concrete bodies) to its representative, the
   earliest by position, and records them as its alternatives. *)
val emit_overload_edges :
  lang:Lang.t -> graph:Call_graph.G.t -> Graph_from_AST.func_info list -> int

(* Emits [C.M <- I.M] Dispatch edges (structural interface satisfaction) so the topo fold sees impls before interfaces. *)

val emit_dispatch_edges :
  cfg:Index_lang_rules.t ->
  type_state:Type_state.t ->
  func_def_file:(Graph_from_AST.func_info -> string option) ->
  class_infos:Types.class_info list ->
  graph:Call_graph.G.t ->
  int
