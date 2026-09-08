(* The signature fixpoint of a taint rule over a call graph, shared by the
   intrafile and the interfile mode. A plain topological pass visits the
   members of a cycle in arbitrary order, so a caller inside a cycle can be
   summarised before its mutual callee and get an incomplete signature
   (mutual recursion, and indirect impl<->interface dispatch cycles). Each
   cyclic component is iterated until every member's signature set is
   stable; a component without a cycle runs once. *)

type db = Shape_and_sig.signature_database

val sccs_callees_first : Call_graph.G.t -> Function_id.t list list
(** The strongly connected components of the graph, a component before
    the components that call into it. *)

val recursive_members :
  Call_graph.G.t -> Function_id.t list list -> Function_id.t list
(** The members of the cyclic components: a component of several
    functions, or one function that calls itself. *)

val store :
  ?max_shape_depth:int ->
  Function_id.t ->
  Shape_and_sig.extended_sig list ->
  db ->
  db
(** Replaces the function's signatures with these, so that the rounds of a
    cycle do not accumulate several signatures of one arity (which makes
    [find_by_arity] give up). A stored shape is cut at [max_shape_depth]:
    a self-recursive builder nests its return shape one level deeper per
    round, with no fixpoint in the shape domain, and the cut at the store
    point ends that chain where its cost is paid. *)

val run :
  rule_id:Rule_ID.t ->
  graph:Call_graph.G.t ->
  sccs:Function_id.t list list ->
  analyze:(Function_id.t -> db -> db) ->
  db ->
  db
(** [analyze] summarises one function from its callees' signatures in the
    store and stores its own (with [store]); it emits no finding. A cyclic
    component that is not stable after [max_rounds] keeps its current
    store, with a warning naming the rule. *)

val max_rounds : int
