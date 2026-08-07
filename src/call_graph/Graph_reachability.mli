val compute_seeded_subgraph :
  ?g_global:Call_graph.G.t ->
  ?depth:int ->
  Call_graph.G.t ->
  seeds:Function_id.t list ->
  Call_graph.G.t
(** Subgraph around [seeds] — their callers and those callers' callees — for
    the one-sided case, where a partial scan saw only sources or only sinks and
    the counterpart may lie in an untargeted companion file. *)

val compute_relevant_subgraph :
  ?g_global:Call_graph.G.t ->
  ?depth:int ->
  Call_graph.G.t ->
  sources:Function_id.t list ->
  sinks:Function_id.t list ->
  Call_graph.G.t
(** Subgraph of [graph] (and read-only [g_global] if given) on paths between
    [sources] and [sinks]; [depth] caps reachability hops. *)
