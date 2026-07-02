val compute_relevant_subgraph :
  ?g_global:Call_graph.G.t ->
  ?depth:int ->
  Call_graph.G.t ->
  sources:Function_id.t list ->
  sinks:Function_id.t list ->
  Call_graph.G.t
(** Subgraph of [graph] (and read-only [g_global] if given) on paths between
    [sources] and [sinks]; [depth] caps reachability hops. *)
