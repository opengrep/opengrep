(* various versions of reachable graphs *)
val reachable_subgraph :
  Call_graph.G.t -> Call_graph.G.V.t -> Call_graph.G.t

val reachable_subgraph_many :
  Call_graph.G.t -> Call_graph.G.V.t list -> Call_graph.G.t

val reverse_reachable_subgraph :
  Call_graph.G.t -> Call_graph.G.V.t list -> Call_graph.G.t

(* Subgraph containing all paths from s1|s2 to the NEAREST common descendants.
 * "Nearest" is computed cycle-safely via SCC condensation minima inside R1 ∩ R2. *)
val nearest_common_descendant_subgraph :
  Call_graph.G.t -> Call_graph.G.V.t -> Call_graph.G.V.t -> Call_graph.G.t
