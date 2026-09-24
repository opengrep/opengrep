(* Emits Dispatch edges from the other members of an overload group (same
   scope, name and arity, concrete bodies) to its representative, the
   earliest by position, and records them as its alternatives. The scope
   spans files for a language whose top level scope is the project. *)
val emit_overload_edges :
  lang:Lang.t ->
  cfg:Index_lang_rules.t ->
  graph:Call_graph.G.t ->
  class_table:Class_table.t ->
  Graph_from_AST.func_info list ->
  int

(* Emits [C.M <- I.M] Dispatch edges (structural interface satisfaction) so the topo fold sees impls before interfaces. *)

val emit_dispatch_edges :
  lang:Lang.t ->
  class_table:Class_table.t ->
  graph:Call_graph.G.t ->
  int
