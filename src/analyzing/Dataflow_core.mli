type nodei = int

(* A set of nodes (via their indices),
 * used for example in the reaching analysis.
 *)
module NodeiSet : Set.S with type elt = Int.t

(* Return value of a dataflow analysis.
 * The array is indexed by nodei.
 *)
type 'env mapping = 'env inout array
and 'env inout = { in_env : 'env; out_env : 'env }

(* The transition/transfer function. It is usually made from the
 * gens and kills.
 *
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)
type 'env transfn = 'env mapping -> nodei -> 'env inout

(* The iteration strategy of [fixpoint]. The client states it, because the
 * choice depends on where the client's lattice starts:
 *
 * - [Ascending]: the initial value is the bottom of the lattice, the
 *   identity of [join]. A component head of the weak topological order
 *   joins its new IN and OUT with the previous ones from its second visit
 *   on, and the iteration ends when the heads stop changing. Clients: the
 *   taint fixpoint (initial empty [Lval_env]) and the product fixpoint of
 *   [Path_feasibility] (initial [Unreachable]).
 *
 * - [Recomputation]: the initial value is the top of the lattice. Every
 *   visit recomputes a node's IN from its predecessors' OUT as the transfer
 *   does, nothing is joined with a previous value, and a node visited more
 *   than [visits_per_node] times keeps its value and schedules nothing.
 *   An unvisited predecessor contributes top, and a head's value is
 *   recovered on the visit after its back edge is computed. Client: svalue
 *   propagation, whose empty environment is [NotCst] for every variable.
 *
 * Under both strategies [fixpoint] stops after
 * [min 100000 (number of nodes * visits_per_node)] visits and returns the
 * mapping reached with [`Timeout]; the client uses that mapping as its
 * result and logs the timeout.
 *)
type 'env iteration_strategy =
  | Ascending of { join : 'env -> 'env -> 'env; visits_per_node : int }
  | Recomputation of { visits_per_node : int }

(* helpers *)
val ns_to_str : NodeiSet.t -> string

(* we use now a functor so we can reuse the same code for dataflow on
 * the IL (IL.cfg) or generic AST (Controlflow.flow)
 *)
module type Flow = sig
  type node
  type edge
  type flow = (node, edge) CFG.t

  val short_string_of_node : node -> string
end

module Make (F : Flow) : sig
  (* main entry point *)
  val fixpoint :
    eq_env:('env -> 'env -> bool) ->
    strategy:'env iteration_strategy ->
    init:'env mapping ->
    trans:'env transfn ->
    flow:F.flow ->
    'env mapping * [ `Ok | `Timeout ]

  val new_node_array : F.flow -> 'a -> 'a array

  (* debugging output *)
  val display_mapping : F.flow -> 'env mapping -> ('env -> string) -> unit
end
