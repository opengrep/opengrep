(* Emits [C.M <- I.M] Dispatch edges (structural interface satisfaction) so the topo fold sees impls before interfaces. *)

val emit_dispatch_edges :
  cfg:Index_lang_rules.t ->
  type_state:Type_state.t ->
  func_def_file:(Graph_from_AST.func_info -> string option) ->
  class_infos:Types.class_info list ->
  graph:Call_graph.G.t ->
  int
