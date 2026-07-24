(** Generic SCC-aware fixed-point engine over a directed graph.

    Iterates a per-node analysis to a fixed point on a user-supplied
    lattice, one SCC at a time. Singleton SCCs without a self-loop run the
    analysis exactly once -- preserving the behaviour of a plain
    topological fold. SCCs with cycles iterate until a snapshot of every
    member's lattice element compares equal across two consecutive rounds.

    The engine is graph- and domain-agnostic: it knows nothing about
    function signatures, taint, or files, and it does not interpret edge
    direction. The caller passes the SCCs in the desired processing order
    -- typically arranged so that nodes whose summaries are consumed by
    other nodes are processed first. *)

module type LATTICE = sig
  type t

  val equal : t -> t -> bool
  (** Equality on the lattice. Used as the fixed-point termination test. *)
end

module type GRAPH = sig
  type t

  module V : sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

  val mem_edge : t -> V.t -> V.t -> bool
end

module type STORE = sig
  type t
  type node
  type lattice

  val get : node -> t -> lattice
  val set : node -> lattice -> t -> t
end

module Make
    (G : GRAPH)
    (L : LATTICE)
    (S : STORE with type node = G.V.t and type lattice = L.t) : sig
  type analyze = G.V.t -> S.t -> S.t
  (** Per-node analysis. Reads the current store, returns an updated store.
      Must be monotone with respect to [L] for guaranteed termination. Must
      not produce side effects observable outside the store -- in
      particular, must not emit matches/findings during fixpoint
      iteration. *)

  val run :
    ?max_iter:int ->
    ?on_max_iter:(G.V.t list -> unit) ->
    sccs:G.V.t list list ->
    graph:G.t ->
    analyze:analyze ->
    S.t ->
    S.t
  (** [run ~sccs ~graph ~analyze store] processes [sccs] in the given list
      order and returns the final store. The caller is responsible for
      arranging [sccs] so that any SCC whose nodes are consumed by another
      SCC's analysis appears first.

      Per SCC: a singleton without a self-loop runs [analyze] once. A
      cycle (multi-element SCC, or singleton with a self-loop) iterates
      [analyze] over every member, comparing each member's lattice element
      before and after via [L.equal], until no member changes -- or until
      [max_iter] rounds elapse, in which case [on_max_iter] is invoked
      with the SCC's members and the current store is returned for that
      SCC.

      [max_iter] defaults to 20. [on_max_iter] defaults to [ignore]. *)
end
