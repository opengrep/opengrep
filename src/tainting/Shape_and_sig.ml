(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(** Taint types. *)

open Common
module R = Rule
module T = Taint
module Log = Log_tainting.Log

(*****************************************************************************)
(* Taint shapes *)
(*****************************************************************************)

module Fields = Map.Make (struct
  type t = T.offset

  (* In taint shapes we consider 'Ofld' and 'Ostr' to be the same, given that
     in some languages like JS/TS you can treat records as if they were dicts
     with string keys. *)
  let compare (o1 : t) (o2 : t) =
    match (o1, o2) with
    | Ofld fld1, Ofld fld2 -> String.compare (fst fld1.ident) (fst fld2.ident)
    | Ostr str1, Ostr str2 -> String.compare str1 str2
    | Ofld fld1, Ostr str2 -> String.compare (fst fld1.ident) str2
    | Ostr str1, Ofld fld2 -> String.compare str1 (fst fld2.ident)
    | Oint i1, Oint i2 -> Int.compare i1 i2
    | Oslice n1, Oslice n2 -> Int.compare n1 n2
    | Oany, Oany -> 0
    | (Ofld _ | Ostr _), (Oint _ | Oslice _ | Oany) -> -1
    | Oint _, (Oslice _ | Oany) -> -1
    | Oslice _, Oany -> -1
    | Oany, (Ofld _ | Ostr _ | Oint _ | Oslice _) -> 1
    | Oslice _, (Ofld _ | Ostr _ | Oint _) -> 1
    | Oint _, (Ofld _ | Ostr _) -> 1
end)

(** The program point that creates an object: a record or list literal, or
    the call whose result it is ([Built_at]), or a write through an l-value
    whose prefix held no object, at that many offsets below the variable
    ([Written_at]). *)
type site = Built_at of T.call_loc | Written_at of T.call_loc * int
[@@deriving eq, ord]

module Sites = Set.Make (struct
  type t = site

  let compare = compare_site
end)

(* A directed graph over the integers from 0, as an array of successor
   lists, for the algorithms of ocamlgraph. *)
module Adjacency = struct
  type t = int list array

  let is_directed = true

  module V = struct
    type t = int

    let compare = Int.compare
    let hash (i : int) : int = i
    let equal = Int.equal
  end

  let fold_vertex f (successors : t) acc =
    Array.to_seq successors
    |> Seq.fold_lefti (fun acc (i : int) (_ : int list) -> f i acc) acc

  let iter_vertex (f : int -> unit) (successors : t) : unit =
    Array.iteri (fun (i : int) (_ : int list) -> f i) successors

  let fold_succ f (successors : t) (i : int) acc =
    List.fold_left (fun acc (j : int) -> f j acc) acc successors.(i)

  let iter_succ (f : int -> unit) (successors : t) (i : int) : unit =
    List.iter f successors.(i)
end

module Adjacency_components = Graph.Components.Make (Adjacency)
module Adjacency_dfs = Graph.Traverse.Dfs (Adjacency)
module Adjacency_bfs = Graph.Traverse.Bfs (Adjacency)

(* The least solution of [value i = join (local i) (value j)] over the
   successors [j] of every [i] (Nielson, Nielson and Hankin 1999, 1.3), by
   the data flow analysis of fix: [local i] flows into [i], and the value of
   [j] into each predecessor of [j]. [leq_join p q] is the join of [p] and
   [q], [q] itself when [p] is below [q]. Every vertex is a root, so every
   vertex has a solution. *)
let least_fixpoint (type value) ~(leq_join : value -> value -> value)
    ~(local : int -> value) (successors : Adjacency.t) : value array =
  let count = Array.length successors in
  let predecessors = Array.make count [] in
  Array.iteri
    (fun (i : int) (next : int list) ->
      List.iter (fun (j : int) -> predecessors.(j) <- i :: predecessors.(j)) next)
    successors;
  let module Solution =
    Fix.DataFlow.ForIntSegment
      (struct
        let n = count
      end)
      (struct
        type property = value

        let leq_join = leq_join
      end)
      (struct
        type variable = int
        type property = value

        let foreach_root (contribute : int -> value -> unit) : unit =
          Array.iteri (fun (i : int) (_ : int list) -> contribute i (local i)) successors

        let foreach_successor (j : int) (value : value) (yield : int -> value -> unit) :
            unit =
          List.iter (fun (i : int) -> yield i value) predecessors.(j)
      end)
  in
  Array.init count (fun (i : int) -> Option.get (Solution.solution i))

(** A shape approximates an object or data structure, and tracks the taint
 * associated with its fields and indexes.
 *
 * Taint shapes are a bit like types. Right now this is mainly to support
 * field- and index-sensitivity, but shapes also provide a good foundation to
 * later add alias analysis.  This is somewhat inspired by
 *
 *     "Polymorphic type, region and effect inference"
 *     by Jean-Pierre Talpin and Pierre Jouvelot
 *
 * History
 * -------
 * Previously, we had a flat environment from l-values to their taint, and we had
 * to "reconstruct" the shape of objects when needed. For example, to check if a
 * variable was a struct, we looked for l-values in the environment that were an
 * "extension" of that variable. By recording shapes explicitly, implementing
 * field-sensitivity becomes more natural.
 *
 * Example
 * -------
 * For example, a record expression `{ a: "taint", b: "safe" }` would have
 * the shape `Obj { .a -> Cell({"taint"}, _|_) }`, recording that the field `a`
 * is tainted by the string literal `"taint"`. A field like '.a' (the dot '.'
 * indicates that it's a field) or an index like '[0]' will always have a 'cell'
 * shape, because they denote l-values. The first argument of a 'Cell' is its
 * xtaint or "taint status" (see 'Xtaint.t'). For each field and index, we track
 * its xtaint individually (field- and index-sensitivity). Field '.a' in
 * `Obj { .a -> Cell({"taint"}, _|_) }` has the the taint set {"taint"} attached.
 * The second argument of 'Cell' is the shape of the objects stored in that cell.
 * The shape of field '.a' is '_|_' ("bottom") which is given to primitive types,
 * or whenever we "don't care" (or to act as "to-do" as well).
 *
 * TODO: Add 'Ptr' shapes and track aliasing.
 *)
module rec Shape : sig
  type shape =
    | Bot  (** _|_, don't know or don't care *)
    | Obj of { sites : Sites.t; summary : bool; fields : obj }
        (** An "object" or struct-like thing.

            Tuples or lists are also represented by 'Obj' shapes! We just treat
            constant indexes as if they were fields, and use 'Oany' to capture
            the non-constant indexes.

            [sites] are the sites that create the objects it stands for.
            [summary] is set when it also stands for the objects of its
            sites that were nested inside it; every cycle of a ['Graph']
            passes through a node with [summary] set. *)
    | Graph of graph
        (** A value whose unfolding is infinite: a finite rooted graph whose
            unfolding is the value, a regular tree (Courcelle 1983). A node
            is an object or a closure set, an edge is a cell, and several
            edges may have one target node, which a cycle is a case of. A
            read continues in the target node.

            INVARIANT(graph):
              1. A ['Graph'] starts a whole value: a variable's cell, an
                 effect's shape, a read's result. No [Leaf], ['Obj'] field or
                 captured cell of a ['Fun'] holds one, so a tree is finite.
              2. Every node reachable from [root] has an infinite unfolding;
                 a finite part is a [Leaf] tree.
              3. No two nodes reachable from [root] are bisimilar (Park
                 1981), labels compared with sites and guards: the graph is
                 the minimal one of its unfolding.
              4. Node indices are the depth-first pre-order from the [root]
                 of the value the array was built for, successors in key
                 order.
              5. Every cycle passes through a node with [summary] set.
              6. INVARIANT(cell) holds for every edge. *)
    | Arg of Taint.formal * Taint.offset list list
        (** Represents the yet-unknown shape of a function/method parameter
            or of a variable captured by a closure,
            optionally extended with one or more offset paths into the
            parameter. Each inner [Taint.offset list] is a single path; the
            outer list is a disjunction of paths — a [cb] bound across two
            branches as [opts.a] in one and [opts.b] in the other has
            [Arg (opts, [[Ostr "a"]; [Ostr "b"]])]. Bare parameters use
            [Arg (arg, [[]])]: one path, empty. At HOF dispatch the engine
            enumerates each path; at call-site instantiation each path is
            resolved against the caller's actual argument. *)
    | Fun of closure * closure list
        (** The closures a function value may be, at least one, each of a
            different definition, in increasing [Function_id.compare] order
            of their definitions.
            These enable Semgrep to handle HOFs. *)

  and graph = {
    nodes : node array;
    root : int;  (** The node the value starts at. *)
    all_taints : Taint.taints array;
        (** For each node, [gather_all_taints_in_shape] of the value that
            starts at it. *)
    has_relevant_content : bool array;
        (** For each node, [shape_has_relevant_content] of the value that
            starts at it. *)
  }

  and node =
    | Object of { sites : Sites.t; summary : bool; edges : edge Fields.t }
        (** An object, as ['Obj']. *)
    | Closures of graph_closure * graph_closure list
        (** A closure set, as ['Fun']. *)

  and graph_closure = edge closure_of
  and graph_env_entry = edge env_entry_of
  and edge = { xtaint : Xtaint.t; target : target }

  and target =
    | Node of int
    | Leaf of shape  (** A tree, never a ['Graph']. *)

  and 'value closure_of = {
    def : Function_id.t;
        (** The function definition whose code the closure runs. *)
    sig_ : Signature.t;
    env : (IL.name * 'value env_entry_of) list;
        (** Binds the variables the code captures; one definition always
            captures the same variables, in the same order. *)
  }
  (** A closure whose captured values are ['value]: cells in a tree, edges
      in a graph. *)

  and closure = cell closure_of
  and env = (IL.name * env_entry) list

  and 'value env_entry_of =
    | Ref of Taint.lval  (** Captured by reference: the variable itself. *)
    | Val of 'value  (** Captured by value: the value at creation. *)

  and env_entry = cell env_entry_of

  and cell =
    | Cell of Xtaint.t * shape
        (** A cell or "reference" represents the "storage" of a value, like
            a variable in C.

            A cell may be explicitly tainted ('`Tainted'), not explicitly tainted
            ('`None' / "0"),  or explicitly clean ('`Clean' / "C").

            A cell that is not explicitly tainted inherits any taints from "parent"
            refs. A cell that is explicitly clean it is clean regardless.

            For example, given a variable `x` and the following statements:

                x.a := "taint";
                x.a.u := "clean";

            We could assign the following shape to `x`:

                Cell(`None, Obj {
                        .a -> Cell({"taint"}, Obj {
                                .u -> Cell(`Clean, _|_)
                                })
                        })

            We have that `x` itself has no taint directly assigned to it, but `x.a` is
            tainted (by the string `"taint"`). Other fields like `x.b` are not tainted.
            When it comes to `x.a`, we have that `x.a.u` has been explicitly marked clean,
            so `x.a.u` will be considered clean despite `x.a` being tainted. Any other field
            of `x.a` such as `x.a.v` will inherit the same taint as `x.a`.

            INVARIANT(cell): To keep shapes minimal:
              1. If the xtaint is '`None', then the shape is not 'Bot' and we can reach
                 another 'cell' whose xtaint is either '`Tainted' or '`Clean'.
              2. If the xtaint is '`Clean', then the shape is 'Bot'.
                 (If we add aliasing we may need to revisit this, and instead just mark
                  every reachable 'cell' as clean too.)

            TODO: We can attach "region ids" to refs and assign taints to regions rather than
              to refs directly, then we can have alias analysis.
          *)

  and obj = cell Fields.t
  (**
      * This a mapping from a 'Taint.offset' to a shape 'cell'.
      *
      * If an 'Obj' shape tracks an 'Oany' offset (an arbitrary index,
      * see 'Taint.offset'), then the taint and shape given to 'Oany' would
      * also be the taint and shape given to any field that is not being
      * explicitly tracked. If there is no 'Oany' in the 'Obj' shape, then a
      * field that is not explicitly tracked would just have an arbitrary or
      * "don't care" shape, and the taint that it inherits from its "parent"
      * 'cell's.
      *
      * THINK: Instead of 'Oany' maybe have an explicit field ?
      *
      * For example, given the assignment `x = { a: "taint", b: "safe" }`,
      * the shape of `x` would be `Cell(`None, Obj { .a -> Cell({"taint"}, _|_) })`.
      * The field `b` is omitted in the shape, and if we ask for it's taint and
      * shape we would get the empty taint set (because `x`'s outermost 'Cell'
      * has no taint), and the shape '_|_' because, given that we are not
      * tracking `b`, it means we don't care about it's shape. In a shape like
      * `{ [*] -> Cell({"taint"}, _|_) }}` where `[*]` denotes 'Oany', the taint
      * and shape  of any concrete index would be given by the taint and shape
      * of '[*]'.
      *)

  val equal_cell : cell -> cell -> bool

  val equal_cell_with_guards : cell -> cell -> bool
  (** Like [equal_cell] but a difference only in a taint's guard counts as
      a difference, recursively ([Fun] shapes compare via
      [Signature.equal_with_guards]). For the dataflow fixpoint's stability
      test; identity and fusion keying keep using [equal_cell]. *)

  val equal_shape : shape -> shape -> bool

  val equal_shape_with_guards : shape -> shape -> bool
  (** [equal_shape] with the guards of [equal_cell_with_guards]. *)

  val equal_env : env -> env -> bool
  val equal_env_with_guards : env -> env -> bool
  val compare_shape : shape -> shape -> int

  val union_sites : traces:Taint.kept_traces -> shape -> shape -> shape
  (** [union_sites shape1 shape2], where [compare_shape shape1 shape2 = 0],
      is [shape1] with the sites of [shape2] added at each position. *)

  val gather_all_taints_in_cell_acc :
    traces:Taint.kept_traces -> Taint.taints -> cell -> Taint.taints

  val gather_all_taints_in_shape_acc :
    traces:Taint.kept_traces -> Taint.taints -> shape -> Taint.taints

  val shape_has_relevant_content : shape -> bool

  val minimise : traces:Taint.kept_traces -> node array -> target -> shape
  (** [minimise nodes root] is the value that starts at [root], in the form
      INVARIANT(graph) requires. [nodes] may hold unreachable nodes,
      bisimilar nodes and nodes whose unfolding is finite; a [Leaf] target
      holds a tree. *)

  val canonical : traces:Taint.kept_traces -> shape -> shape
  (** The canonical form of an ['Obj'] or ['Fun'] whose cells may hold a
      ['Graph'] (INVARIANT(graph).1): a ['Graph'] when one of them does,
      else the shape itself. *)

  val append_graph : node Dynarray.t -> graph -> int
  (** Appends the nodes reachable from [root] and returns the index of the
      root's copy. *)

  val target_of_shape : node Dynarray.t -> shape -> target
  (** A ['Graph'] appended ([append_graph]), a tree as a [Leaf]. *)

  val node_edges : node -> edge list
  (** Objects: in key order; closure sets: the captured cells in the order
      of the closures and of their environments. *)

  val map_edges : (edge -> edge) -> node -> node
  val map_targets : (target -> target) -> node -> node

  val successors : node -> int list
  (** The indices of the [Node] targets of [node_edges]. *)

  val preorder : int list array -> int -> int list
  (** [preorder successors root]: the vertices reachable from [root] in
      depth-first pre-order, successors in list order. *)

  val unfold : shape -> shape
  (** A ['Graph'] as the ['Obj'] or ['Fun'] of its root node, whose cells
      hold the values that start at the edges' targets; a tree as it is.
      For reads only: the result may hold a ['Graph'] below its root. *)

  val show_cell : cell -> string
  val show_shape : shape -> string
  val show_obj : obj -> string
end = struct
  type shape =
    | Bot
    | Obj of {
        sites : (Sites.t[@equal Sites.equal]);
        summary : bool;
        fields : obj;
      }
    | Graph of graph
    | Arg of T.formal * T.offset list list
    | Fun of closure * closure list
  and graph = {
    nodes : node array;
    root : int;
    all_taints : (T.taints[@equal T.Taint_set.equal]) array;
    has_relevant_content : bool array;
  }
  and node =
    | Object of {
        sites : (Sites.t[@equal Sites.equal]);
        summary : bool;
        edges : edge Fields.t;
      }
    | Closures of graph_closure * graph_closure list
  and graph_closure = edge closure_of
  and graph_env_entry = edge env_entry_of
  and edge = { xtaint : Xtaint.t; target : target }
  and target = Node of int | Leaf of shape
  and 'value closure_of = {
    def : (Function_id.t[@equal Function_id.equal]);
    sig_ : Signature.t;
    env :
      ((IL.name[@equal fun n1 n2 -> Int.equal (IL.compare_name n1 n2) 0])
      * 'value env_entry_of)
      list;
  }
  and closure = cell closure_of
  and env =
    ((IL.name[@equal fun n1 n2 -> Int.equal (IL.compare_name n1 n2) 0])
    * env_entry)
    list
  and 'value env_entry_of =
    | Ref of (T.lval[@equal fun l1 l2 -> Int.equal (T.compare_lval l1 l2) 0])
    | Val of 'value
  and env_entry = cell env_entry_of
  and cell = Cell of Xtaint.t * shape
  and obj = cell Fields.t
  [@@deriving eq]

  (*************************************)
  (* Graphs *)
  (*************************************)

  (* Objects: in key order; closure sets: the captured cells in the order of
     the closures and of their environments. *)
  let node_edges (node : node) : edge list =
    match node with
    | Object { edges; _ } ->
        Fields.fold (fun _ (edge : edge) edges -> edge :: edges) edges []
        |> List.rev
    | Closures (c, cs) ->
        List.concat_map
          (fun (closure : graph_closure) ->
            List.filter_map
              (fun ((_, entry) : IL.name * graph_env_entry) ->
                match entry with
                | Val edge -> Some edge
                | Ref _ -> None)
              closure.env)
          (c :: cs)

  let map_edges (f : edge -> edge) (node : node) : node =
    match node with
    | Object ({ edges; _ } as node) ->
        Object { node with edges = Fields.map f edges }
    | Closures (c, cs) ->
        let map_closure (closure : graph_closure) : graph_closure =
          {
            closure with
            env =
              List.map
                (fun ((x, entry) as binding : IL.name * graph_env_entry) ->
                  match entry with
                  | Val edge -> (x, (Val (f edge) : graph_env_entry))
                  | Ref _ -> binding)
                closure.env;
          }
        in
        Closures (map_closure c, List.map map_closure cs)

  let map_targets (f : target -> target) (node : node) : node =
    map_edges (fun (edge : edge) -> { edge with target = f edge.target }) node

  let successors (node : node) : int list =
    List.filter_map
      (fun (edge : edge) ->
        match edge.target with
        | Node j -> Some j
        | Leaf _ -> None)
      (node_edges node)

  (* The vertices reachable from [root] in depth-first pre-order, successors
     in the order of [successors]: [Adjacency_dfs.fold_component] visits
     first the successor it pushed last, so it is given the lists
     reversed. *)
  let preorder (successors : int list array) (root : int) : int list =
    Adjacency_dfs.fold_component
      (fun (i : int) (order : int list) -> i :: order)
      [] (Array.map List.rev successors) root
    |> List.rev

  (* The pairs of nodes of two graphs that their unfoldings reach at one
     position: the reachable part of the product of the two graphs (Rabin
     and Scott 1959), for the algorithms of ocamlgraph. *)
  module Pairs = struct
    type t = graph * graph

    let is_directed = true

    module V = struct
      type t = int * int

      let compare ((i1, j1) : t) ((i2, j2) : t) : int =
        match Int.compare i1 i2 with
        | 0 -> Int.compare j1 j2
        | other -> other

      let hash ((i, j) : t) : int = Hashtbl.hash (i, j)

      let equal ((i1, j1) : t) ((i2, j2) : t) : bool =
        Int.equal i1 i2 && Int.equal j1 j2
    end

    let fold_vertex f ((g1, g2) : t) acc =
      Seq.fold_lefti
        (fun acc (i : int) (_ : node) ->
          Seq.fold_lefti
            (fun acc (j : int) (_ : node) -> f (i, j) acc)
            acc (Array.to_seq g2.nodes))
        acc (Array.to_seq g1.nodes)

    let iter_vertex (f : V.t -> unit) (graphs : t) : unit =
      fold_vertex (fun (pair : V.t) () -> f pair) graphs ()

    (* The pairs of the targets of the edges at one key, or at one captured
       position, that are nodes on both sides, in key order. *)
    let succ ((g1, g2) : t) ((i, j) : V.t) : V.t list =
      match (g1.nodes.(i), g2.nodes.(j)) with
      | Object object1, Object object2 ->
          Fields.fold
            (fun (o : T.offset) (edge1 : edge) (pairs : V.t list) ->
              match (edge1.target, Fields.find_opt o object2.edges) with
              | Node i', Some { target = Node j'; _ } -> (i', j') :: pairs
              | (Node _ | Leaf _), _ -> pairs)
            object1.edges []
          |> List.rev
      | Closures _, Closures _ -> (
          let edges1 = node_edges g1.nodes.(i) in
          let edges2 = node_edges g2.nodes.(j) in
          match List.compare_lengths edges1 edges2 with
          | 0 ->
              List.fold_left2
                (fun (pairs : V.t list) (edge1 : edge) (edge2 : edge) ->
                  match (edge1.target, edge2.target) with
                  | Node i', Node j' -> (i', j') :: pairs
                  | (Node _ | Leaf _), _ -> pairs)
                [] edges1 edges2
              |> List.rev
          | _ -> [])
      | Object _, Closures _
      | Closures _, Object _ ->
          []

    let fold_succ f (graphs : t) (pair : V.t) acc =
      List.fold_left (fun acc (pair : V.t) -> f pair acc) acc (succ graphs pair)

    let iter_succ (f : V.t -> unit) (graphs : t) (pair : V.t) : unit =
      List.iter f (succ graphs pair)
  end

  module Pair_dfs = Graph.Traverse.Dfs (Pairs)
  module Pair_bfs = Graph.Traverse.Bfs (Pairs)
  module Pair_tbl = Hashtbl.Make (Pairs.V)

  let append_graph (builder : node Dynarray.t) (g : graph) : int =
    let order = preorder (Array.map successors g.nodes) g.root in
    let base = Dynarray.length builder in
    let index = Array.make (Array.length g.nodes) (-1) in
    List.iteri (fun (k : int) (i : int) -> index.(i) <- base + k) order;
    List.iter
      (fun (i : int) ->
        Dynarray.add_last builder
          (map_targets
             (fun (target : target) ->
               match target with
               | Node j -> Node index.(j)
               | Leaf _ -> target)
             g.nodes.(i)))
      order;
    index.(g.root)

  let target_of_shape (builder : node Dynarray.t) (shape : shape) : target =
    match shape with
    | Graph g -> Node (append_graph builder g)
    | Bot
    | Obj _
    | Arg _
    | Fun _ ->
        Leaf shape

  let shape_of_node (cell_of_edge : edge -> cell) (node : node) : shape =
    match node with
    | Object { sites; summary; edges } ->
        Obj { sites; summary; fields = Fields.map cell_of_edge edges }
    | Closures (c, cs) ->
        let closure_of (closure : graph_closure) : closure =
          {
            def = closure.def;
            sig_ = closure.sig_;
            env =
              List.map
                (fun ((x, entry) : IL.name * graph_env_entry) ->
                  match entry with
                  | Ref lval -> (x, (Ref lval : env_entry))
                  | Val edge -> (x, Val (cell_of_edge edge)))
                closure.env;
          }
        in
        Fun (closure_of c, List.map closure_of cs)

  let unfold (shape : shape) : shape =
    match shape with
    | Graph g ->
        shape_of_node
          (fun (edge : edge) ->
            match edge.target with
            | Node i -> Cell (edge.xtaint, Graph { g with root = i })
            | Leaf leaf -> Cell (edge.xtaint, leaf))
          g.nodes.(g.root)
    | Bot
    | Obj _
    | Arg _
    | Fun _ ->
        shape

  (*************************************)
  (* Equality *)
  (*************************************)
  (* TODO: Should we just define these in terms of `compare_*` ? *)

  let equal_env_by (equal_cell : cell -> cell -> bool) (env1 : env)
      (env2 : env) : bool =
    List.equal
      (fun (x1, e1) (x2, e2) ->
        Int.equal (IL.compare_name x1 x2) 0
        &&
        match (e1, e2) with
        | Ref l1, Ref l2 -> Int.equal (T.compare_lval l1 l2) 0
        | Val c1, Val c2 -> equal_cell c1 c2
        | Ref _, Val _
        | Val _, Ref _ ->
            false)
      env1 env2

  let equal_closures_by (equal_sig : Signature.t -> Signature.t -> bool)
      (equal_cell : cell -> cell -> bool) ((c1, cs1) : closure * closure list)
      ((c2, cs2) : closure * closure list) : bool =
    let equal_closure (c1 : closure) (c2 : closure) =
      Function_id.equal c1.def c2.def
      && equal_sig c1.sig_ c2.sig_
      && equal_env_by equal_cell c1.env c2.env
    in
    equal_closure c1 c2 && List.equal equal_closure cs1 cs2

  let phys_equal_graph (g1 : graph) (g2 : graph) : bool =
    phys_equal g1 g2
    || (phys_equal g1.nodes g2.nodes && Int.equal g1.root g2.root)

  (* The labels of two nodes: everything but the nodes that their edges
     reach, which the caller pairs. *)
  let equal_labels ~(equal_xtaint : Xtaint.t -> Xtaint.t -> bool)
      ~(equal_tree : shape -> shape -> bool)
      ~(equal_sig : Signature.t -> Signature.t -> bool) (node1 : node)
      (node2 : node) : bool =
    let equal_edges (edge1 : edge) (edge2 : edge) : bool =
      (phys_equal edge1.xtaint edge2.xtaint || equal_xtaint edge1.xtaint edge2.xtaint)
      &&
      match (edge1.target, edge2.target) with
      | Node _, Node _ -> true
      | Leaf shape1, Leaf shape2 -> phys_equal shape1 shape2 || equal_tree shape1 shape2
      | Node _, Leaf _
      | Leaf _, Node _ ->
          false
    in
    match (node1, node2) with
    | Object object1, Object object2 ->
        Sites.equal object1.sites object2.sites
        && Bool.equal object1.summary object2.summary
        && Fields.equal equal_edges object1.edges object2.edges
    | Closures (c1, cs1), Closures (c2, cs2) ->
        List.equal
          (fun (closure1 : graph_closure) (closure2 : graph_closure) ->
            Function_id.equal closure1.def closure2.def
            && equal_sig closure1.sig_ closure2.sig_
            && List.equal
                 (fun ((x1, e1) : IL.name * graph_env_entry)
                      ((x2, e2) : IL.name * graph_env_entry) ->
                   Int.equal (IL.compare_name x1 x2) 0
                   &&
                   match (e1, e2) with
                   | Ref l1, Ref l2 -> Int.equal (T.compare_lval l1 l2) 0
                   | Val edge1, Val edge2 -> equal_edges edge1 edge2
                   | Ref _, Val _
                   | Val _, Ref _ ->
                       false)
                 closure1.env closure2.env)
          (c1 :: cs1) (c2 :: cs2)
    | Object _, Closures _
    | Closures _, Object _ ->
        false

  (* Whether the values that start at the roots of [g1] and [g2] are
     bisimilar (Park 1981): the labels are equal at every pair of nodes that
     the two unfoldings reach at one position. On minimal graphs this is
     isomorphism. *)
  let bisimilar ~(equal_xtaint : Xtaint.t -> Xtaint.t -> bool)
      ~(equal_tree : shape -> shape -> bool)
      ~(equal_sig : Signature.t -> Signature.t -> bool) (g1 : graph)
      (g2 : graph) : bool =
    match
      Pair_dfs.iter_component
        ~pre:(fun ((i, j) : int * int) ->
          if
            not
              (equal_labels ~equal_xtaint ~equal_tree ~equal_sig g1.nodes.(i)
                 g2.nodes.(j))
          then raise_notrace Exit)
        (g1, g2) (g1.root, g2.root)
    with
    | () -> true
    | exception Exit -> false

  let equal_obj_node ~(equal_fields : obj -> obj -> bool) (sites1 : Sites.t)
      (summary1 : bool) (fields1 : obj) (sites2 : Sites.t) (summary2 : bool)
      (fields2 : obj) : bool =
    Sites.equal sites1 sites2 && Bool.equal summary1 summary2
    && equal_fields fields1 fields2

  let rec equal_cell cell1 cell2 =
    phys_equal cell1 cell2
    ||
    let (Cell (taints1, shape1)) = cell1 in
    let (Cell (taints2, shape2)) = cell2 in
    Xtaint.equal taints1 taints2 && equal_shape shape1 shape2

  and equal_shape shape1 shape2 =
    match (shape1, shape2) with
    | Bot, Bot -> true
    | ( Obj { sites = sites1; summary = summary1; fields = fields1 },
        Obj { sites = sites2; summary = summary2; fields = fields2 } ) ->
        equal_obj_node ~equal_fields:equal_obj sites1 summary1 fields1 sites2
          summary2 fields2
    | Graph g1, Graph g2 ->
        phys_equal_graph g1 g2
        || bisimilar ~equal_xtaint:Xtaint.equal ~equal_tree:equal_shape
             ~equal_sig:Signature.equal g1 g2
    | Arg (formal1, offsets1), Arg (formal2, offsets2) ->
        T.equal_formal formal1 formal2
        && Int.equal
             (List.compare (List.compare T.compare_offset) offsets1 offsets2)
             0
    | Fun (c1, cs1), Fun (c2, cs2) ->
        equal_closures_by Signature.equal equal_cell (c1, cs1) (c2, cs2)
    | Bot, _
    | Obj _, _
    | Graph _, _
    | Arg _, _
    | Fun _, _ ->
        false

  and equal_obj obj1 obj2 = Fields.equal equal_cell obj1 obj2

  (* Guard-aware twin of the chain above; structure identical, but cell
   * taints compare via [Xtaint.equal_with_guards] and [Fun] shapes via
   * [Signature.equal_with_guards]. *)
  let rec equal_cell_with_guards cell1 cell2 =
    phys_equal cell1 cell2
    ||
    let (Cell (taints1, shape1)) = cell1 in
    let (Cell (taints2, shape2)) = cell2 in
    Xtaint.equal_with_guards taints1 taints2
    && equal_shape_with_guards shape1 shape2

  and equal_shape_with_guards shape1 shape2 =
    match (shape1, shape2) with
    | Bot, Bot -> true
    | ( Obj { sites = sites1; summary = summary1; fields = fields1 },
        Obj { sites = sites2; summary = summary2; fields = fields2 } ) ->
        equal_obj_node ~equal_fields:equal_obj_with_guards sites1 summary1
          fields1 sites2 summary2 fields2
    | Graph g1, Graph g2 ->
        phys_equal_graph g1 g2
        || bisimilar ~equal_xtaint:Xtaint.equal_with_guards
             ~equal_tree:equal_shape_with_guards
             ~equal_sig:Signature.equal_with_guards g1 g2
    | Arg (formal1, offsets1), Arg (formal2, offsets2) ->
        T.equal_formal formal1 formal2
        && Int.equal
             (List.compare (List.compare T.compare_offset) offsets1 offsets2)
             0
    | Fun (c1, cs1), Fun (c2, cs2) ->
        equal_closures_by Signature.equal_with_guards equal_cell_with_guards
          (c1, cs1) (c2, cs2)
    | Bot, _
    | Obj _, _
    | Graph _, _
    | Arg _, _
    | Fun _, _ ->
        false

  and equal_obj_with_guards obj1 obj2 =
    Fields.equal equal_cell_with_guards obj1 obj2

  let equal_env env1 env2 = equal_env_by equal_cell env1 env2
  let equal_env_with_guards env1 env2 = equal_env_by equal_cell_with_guards env1 env2

  (*************************************)
  (* Comparison *)
  (*************************************)

  (* The order of two graphs modulo sites: their labels compared at the
     pairs of nodes that the two unfoldings reach at one position, in
     shortlex order of the positions (the breadth-first order, successors in
     key order), each pair once; the first difference decides. A difference
     below a pair met again appears first below its first visit, so the
     pruning is exact, and the result is 0 exactly for values bisimilar
     modulo sites. *)
  let compare_graphs ~(compare_tree : shape -> shape -> int) (g1 : graph)
      (g2 : graph) : int =
    let exception Decided of int in
    let compare_edges (edge1 : edge) (edge2 : edge) : int =
      match Xtaint.compare edge1.xtaint edge2.xtaint with
      | 0 -> (
          match (edge1.target, edge2.target) with
          | Leaf shape1, Leaf shape2 -> compare_tree shape1 shape2
          | Node _, Node _ -> 0
          | Leaf _, Node _ -> -1
          | Node _, Leaf _ -> 1)
      | other -> other
    in
    let compare_closures (c1 : graph_closure) (c2 : graph_closure) : int =
      match Function_id.compare c1.def c2.def with
      | 0 -> (
          match Signature.compare c1.sig_ c2.sig_ with
          | 0 ->
              List.compare
                (fun ((x1, e1) : IL.name * graph_env_entry)
                     ((x2, e2) : IL.name * graph_env_entry) ->
                  match IL.compare_name x1 x2 with
                  | 0 -> (
                      match (e1, e2) with
                      | Ref l1, Ref l2 -> T.compare_lval l1 l2
                      | Val edge1, Val edge2 -> compare_edges edge1 edge2
                      | Ref _, Val _ -> -1
                      | Val _, Ref _ -> 1)
                  | other -> other)
                c1.env c2.env
          | other -> other)
      | other -> other
    in
    let compare_labels (node1 : node) (node2 : node) : int =
      match (node1, node2) with
      | Object object1, Object object2 -> (
          match Bool.compare object1.summary object2.summary with
          | 0 -> Fields.compare compare_edges object1.edges object2.edges
          | other -> other)
      | Closures (c1, cs1), Closures (c2, cs2) ->
          List.compare compare_closures (c1 :: cs1) (c2 :: cs2)
      | Object _, Closures _ -> -1
      | Closures _, Object _ -> 1
    in
    match
      Pair_bfs.iter_component
        (fun ((i, j) : int * int) ->
          match compare_labels g1.nodes.(i) g2.nodes.(j) with
          | 0 -> ()
          | other -> raise_notrace (Decided other))
        (g1, g2) (g1.root, g2.root)
    with
    | () -> 0
    | exception Decided other -> other

  let rec compare_cell cell1 cell2 =
    if phys_equal cell1 cell2 then 0
    else
    let (Cell (taints1, shape1)) = cell1 in
    let (Cell (taints2, shape2)) = cell2 in
    match Xtaint.compare taints1 taints2 with
    | 0 -> compare_shape shape1 shape2
    | other -> other

  and compare_shape shape1 shape2 =
    if phys_equal shape1 shape2 then 0
    else
    match (shape1, shape2) with
    | Bot, Bot -> 0
    | ( Obj { sites = _; summary = summary1; fields = fields1 },
        Obj { sites = _; summary = summary2; fields = fields2 } ) -> (
        match Bool.compare summary1 summary2 with
        | 0 -> compare_obj fields1 fields2
        | other -> other)
    | Graph g1, Graph g2 ->
        if phys_equal_graph g1 g2 then 0
        else compare_graphs ~compare_tree:compare_shape g1 g2
    | Arg (formal1, offsets1), Arg (formal2, offsets2) -> (
        match T.compare_formal formal1 formal2 with
        | 0 -> List.compare (List.compare T.compare_offset) offsets1 offsets2
        | other -> other)
    | Fun (c1, cs1), Fun (c2, cs2) -> (
        match compare_closure c1 c2 with
        | 0 -> List.compare compare_closure cs1 cs2
        | other -> other)
    | Bot, (Obj _ | Graph _ | Arg _ | Fun _)
    | Obj _, (Graph _ | Arg _ | Fun _)
    | Graph _, (Arg _ | Fun _)
    | Arg _, Fun _ ->
        -1
    | Obj _, Bot
    | Graph _, (Bot | Obj _)
    | Arg _, (Bot | Obj _ | Graph _)
    | Fun _, (Bot | Obj _ | Graph _ | Arg _) ->
        1

  and compare_obj obj1 obj2 = Fields.compare compare_cell obj1 obj2

  and compare_closure (c1 : closure) (c2 : closure) =
    if phys_equal c1 c2 then 0
    else
    match Function_id.compare c1.def c2.def with
    | 0 -> (
        match Signature.compare c1.sig_ c2.sig_ with
        | 0 -> compare_env c1.env c2.env
        | other -> other)
    | other -> other

  and compare_env env1 env2 =
    List.compare
      (fun (x1, e1) (x2, e2) ->
        match IL.compare_name x1 x2 with
        | 0 -> (
            match (e1, e2) with
            | Ref l1, Ref l2 -> T.compare_lval l1 l2
            | Val c1, Val c2 -> compare_cell c1 c2
            | Ref _, Val _ -> -1
            | Val _, Ref _ -> 1)
        | other -> other)
      env1 env2

  (*************************************)
  (* Collect/union all taints *)
  (*************************************)

  (* THINK: Generalize to "fold" ? *)
  let rec gather_all_taints_in_cell_acc ~(traces : T.kept_traces) acc cell =
    let (Cell (xtaint, shape)) = cell in
    match xtaint with
    | `Clean ->
        (* Due to INVARIANT(cell) we can just stop here. *)
        acc
    | `None -> gather_all_taints_in_shape_acc ~traces acc shape
    | `Tainted taints ->
        gather_all_taints_in_shape_acc ~traces
          (T.Taint_set.union ~traces taints acc)
          shape

  and gather_all_taints_in_shape_acc ~(traces : T.kept_traces) acc = function
    | Bot -> acc
    | Obj { fields; _ } -> gather_all_taints_in_obj_acc ~traces acc fields
    | Graph g -> T.Taint_set.union ~traces g.all_taints.(g.root) acc
    | Arg (arg, offsets) ->
        (* One [Shape_var] per alternative offset. *)
        List.fold_left
          (fun acc off ->
            let lval = { T.base = T.base_of_formal arg; offset = off } in
            let taint = T.taint_of_orig (T.Shape_var lval) in
            T.Taint_set.add_taint ~traces taint acc)
          acc offsets
    | Fun _ ->
        (* Consider a third-party/opaque function to which we pass a record that
         * contains a function object. Should be gather the taints in the function
         * shape? In principle, no, since taints within a function shape aren't
         * reachable until the function gets called...
         *
         * TODO: We could perhaps consider gathering the concrete taint sources
         * that may be reachable if the function ever gets called? *)
        acc

  and gather_all_taints_in_obj_acc ~(traces : T.kept_traces) acc obj =
    Fields.fold
      (fun _ o_cell acc -> gather_all_taints_in_cell_acc ~traces acc o_cell)
      obj acc

  (* Does [shape] carry any content relevant to taint propagation?
   * - [`Tainted] xtaint on any cell — direct taint.
   * - [Arg _] — polymorphic caller-supplied taint yet to be instantiated.
   * - [Fun _] — function reference; HOF analysis tracks the callback's
   *   signature via this shape, so an assignment of a lambda to a
   *   variable is not a sanitizer even though no [`Tainted] cell is
   *   reachable through the shape.
   * - [`Clean] cell with [Bot] subshape — literal construction observed
   *   no taint here; does NOT count as content.
   * - [Bot] — nothing. *)
  let rec shape_has_relevant_content = function
    | Bot -> false
    | Graph g -> g.has_relevant_content.(g.root)
    | Arg _
    | Fun _ ->
        true
    | Obj { fields; _ } ->
        Fields.exists (fun _ cell -> cell_has_relevant_content cell) fields

  and cell_has_relevant_content (Cell (xtaint, shape)) =
    Xtaint.is_tainted xtaint || shape_has_relevant_content shape

  (*************************************)
  (* Minimisation *)
  (*************************************)

  module Label_tbl = Hashtbl.Make (Int)

  module Letter = struct
    type t = int

    let compare = Int.compare
    let print = Int.to_string
  end

  (* A hash of a label that equal labels share ([equal_labels] with sites and
     guards): its kind, summary, sites and keys, and for each edge its taint's
     kind and size and its target's kind. *)
  let label_hash (node : node) : int =
    let edge_hash (edge : edge) : int =
      Hashtbl.hash
        ( (match edge.xtaint with
          | `None -> -1
          | `Clean -> -2
          | `Tainted taints -> T.Taint_set.cardinal taints),
          match edge.target with
          | Node _ -> -1
          | Leaf Bot -> -2
          | Leaf (Obj { fields; _ }) -> Fields.cardinal fields
          | Leaf (Graph _) -> -3
          | Leaf (Arg (_, offsets)) -> -4 - List.length offsets
          | Leaf (Fun (_, cs)) -> -100 - List.length cs )
    in
    match node with
    | Object { summary; edges; sites } ->
        Hashtbl.hash
          ( 0,
            summary,
            Sites.cardinal sites,
            Fields.fold
              (fun (o : T.offset) (edge : edge) (hashes : int list) ->
                Hashtbl.hash
                  ( (match o with
                    | T.Ofld name -> Hashtbl.hash (fst name.ident)
                    | T.Ostr name -> Hashtbl.hash name
                    | T.Oint i -> i
                    | T.Oslice i -> -i
                    | T.Oany -> -1),
                    edge_hash edge )
                :: hashes)
              edges [] )
    | Closures (c, cs) ->
        Hashtbl.hash
          ( 1,
            List.map
              (fun (closure : graph_closure) -> Function_id.hash closure.def)
              (c :: cs),
            List.map edge_hash (node_edges node) )

  (* [nodes] and [root]: a graph that may have unreachable nodes, bisimilar
     nodes and nodes whose unfolding is finite. The strongly connected
     components (Tarjan 1972) decide which nodes have an infinite unfolding;
     the others become [Leaf] trees; partition refinement merges the
     bisimilar ones; the classes are numbered in depth-first pre-order from
     the root, and the derived attributes are computed component by
     component, successors first. *)
  let minimise ~(traces : T.kept_traces) (nodes : node array) (root : target) :
      shape =
    match root with
    | Leaf shape -> shape
    | Node root ->
        let order = Array.of_list (preorder (Array.map successors nodes) root) in
        let count = Array.length order in
        let local = Array.make (Array.length nodes) (-1) in
        Array.iteri (fun (k : int) (i : int) -> local.(i) <- k) order;
        let local_successors =
          Array.map
            (fun (i : int) -> List.map (fun (j : int) -> local.(j)) (successors nodes.(i)))
            order
        in
        (* An arc from [k] to [k'] gives [k]'s component an index at least
           [k']'s, so the components in index order come successors first. *)
        let components = Adjacency_components.scc_array local_successors in
        let component_of = Array.make count 0 in
        Array.iteri
          (fun (c : int) (members : int list) ->
            List.iter (fun (k : int) -> component_of.(k) <- c) members)
          components;
        let infinite = Array.make (Array.length components) false in
        Array.iteri
          (fun (c : int) (members : int list) ->
            infinite.(c) <-
              (match members with
              | _ :: _ :: _ -> true
              | [ k ] -> List.exists (Int.equal k) local_successors.(k)
              | [] -> false)
              || List.exists
                   (fun (k : int) ->
                     List.exists
                       (fun (k' : int) -> infinite.(component_of.(k')))
                       local_successors.(k))
                   members)
          components;
        let is_infinite (k : int) : bool = infinite.(component_of.(k)) in
        let trees = Array.make count None in
        let rec tree_of (k : int) : shape =
          match trees.(k) with
          | Some shape -> shape
          | None ->
              let shape =
                shape_of_node
                  (fun (edge : edge) -> Cell (edge.xtaint, leaf_of edge.target))
                  nodes.(order.(k))
              in
              trees.(k) <- Some shape;
              shape
        and leaf_of (target : target) : shape =
          match target with
          | Leaf shape -> shape
          | Node j -> tree_of local.(j)
        in
        if not (is_infinite 0) then tree_of 0
        else
          let states =
            List.filter is_infinite (List.init count Fun.id) |> Array.of_list
          in
          let state_of = Array.make count (-1) in
          Array.iteri (fun (p : int) (k : int) -> state_of.(k) <- p) states;
          let labelled =
            Array.map
              (fun (k : int) ->
                map_targets
                  (fun (target : target) ->
                    match target with
                    | Node j when is_infinite local.(j) ->
                        Node state_of.(local.(j))
                    | Node j -> Leaf (tree_of local.(j))
                    | Leaf _ -> target)
                  nodes.(order.(k)))
              states
          in
          let equal_labels =
            equal_labels ~equal_xtaint:Xtaint.equal_with_guards
              ~equal_tree:equal_shape_with_guards
              ~equal_sig:Signature.equal_with_guards
          in
          (* The initial partition: states of equal labels, grouped by
             [label_hash] and compared within a group. *)
          let groups = Label_tbl.create (Array.length labelled) in
          Array.iteri
            (fun (p : int) (node : node) ->
              let hash = label_hash node in
              let classes = Option.value (Label_tbl.find_opt groups hash) ~default:[] in
              Label_tbl.replace groups hash
                (match
                   List.partition
                     (fun ((representative, _) : int * int list) ->
                       equal_labels labelled.(representative) node)
                     classes
                 with
                | [ (representative, members) ], others ->
                    (representative, p :: members) :: others
                | _, _ -> (p, [ p ]) :: classes))
            labelled;
          let initial =
            Label_tbl.fold
              (fun _ (classes : (int * int list) list) (initial : int list list) ->
                List.fold_left
                  (fun (initial : int list list) ((_, members) : int * int list) ->
                    members :: initial)
                  initial classes)
              groups []
          in
          (* The transitions of the automaton: from a state to the state of
             the edge's target, labelled by the edge's position in
             [node_edges]. *)
          let transition_table =
            Array.to_list labelled
            |> List.mapi (fun (p : int) (node : node) ->
                   List.mapi (fun (letter : int) (edge : edge) -> (p, letter, edge.target))
                     (node_edges node)
                   |> List.filter_map (fun ((p, letter, target) : int * int * target) ->
                          match target with
                          | Node q -> Some (p, letter, q)
                          | Leaf _ -> None))
            |> List.concat |> Array.of_list
          in
          (* The coarsest partition that respects [initial] and the
             transitions, the classes of the largest bisimulation (Valmari
             2012). Every state is initial and final: the minimal automaton
             keeps all of them. *)
          let module States =
            Fix.Indexing.Const (struct
              let cardinal = Array.length labelled
            end)
          in
          let module Transitions =
            Fix.Indexing.Const (struct
              let cardinal = Array.length transition_table
            end)
          in
          let module Minimal =
            Fix.Minimize.Minimize
              (Letter)
              (struct
                type states = States.n

                let states = States.n

                type state = states Fix.Indexing.index
                type transitions = Transitions.n

                let transitions = Transitions.n

                type transition = transitions Fix.Indexing.index

                let at (t : transition) : int * int * int =
                  transition_table.(Fix.Indexing.Index.to_int t)

                let label (t : transition) : int =
                  let _, letter, _ = at t in
                  letter

                let source (t : transition) : state =
                  let p, _, _ = at t in
                  Fix.Indexing.Index.of_int states p

                let target (t : transition) : state =
                  let _, _, q = at t in
                  Fix.Indexing.Index.of_int states q

                let all : state Fix.Enum.enum =
                  Fix.Enum.enum (fun (yield : state -> unit) ->
                      Fix.Indexing.Index.iter states yield)

                let initials = all
                let finals = all
                let debug = false

                let groups =
                  Fix.Enum.list
                    (List.map
                       (fun (members : int list) ->
                         Fix.Enum.list
                           (List.map (Fix.Indexing.Index.of_int states) members))
                       initial)
              end)
          in
          let block_of =
            Array.init (Array.length labelled) (fun (p : int) ->
                match
                  Minimal.transport_state (Fix.Indexing.Index.of_int States.n p)
                with
                | Some block -> Fix.Indexing.Index.to_int block
                | None -> -1)
          in
          let blocks = Fix.Indexing.cardinal Minimal.states in
          let representative =
            Array.init blocks (fun (block : int) ->
                Fix.Indexing.Index.to_int
                  (Minimal.backport_state_one
                     (Fix.Indexing.Index.of_int Minimal.states block)))
          in
          let numbered =
            preorder
              (Array.map
                 (fun (p : int) ->
                   List.map (fun (q : int) -> block_of.(q)) (successors labelled.(p)))
                 representative)
              block_of.(state_of.(0))
            |> Array.of_list
          in
          let index = Array.make blocks (-1) in
          Array.iteri (fun (i : int) (block : int) -> index.(block) <- i) numbered;
          let canonical =
            Array.map
              (fun (block : int) ->
                map_targets
                  (fun (target : target) ->
                    match target with
                    | Node q -> Node index.(block_of.(q))
                    | Leaf _ -> target)
                  labelled.(representative.(block)))
              numbered
          in
          (* A read gathers through the edges of objects that are not
             [Clean], and a closure set holds nothing a read gathers. *)
          let dependencies =
            Array.map
              (fun (node : node) ->
                match node with
                | Object { edges; _ } ->
                    Fields.fold
                      (fun _ (edge : edge) (dependencies : int list) ->
                        match (edge.xtaint, edge.target) with
                        | (`None | `Tainted _), Node j -> j :: dependencies
                        | `Clean, _
                        | _, Leaf _ ->
                            dependencies)
                      edges []
                | Closures _ -> [])
              canonical
          in
          let own_edges (i : int) : edge list =
            match canonical.(i) with
            | Object { edges; _ } ->
                Fields.fold (fun _ (edge : edge) edges -> edge :: edges) edges []
                |> List.filter (fun (edge : edge) ->
                       match edge.xtaint with
                       | `Clean -> false
                       | `None
                       | `Tainted _ ->
                           true)
            | Closures _ -> []
          in
          let all_taints =
            least_fixpoint
              ~leq_join:(fun (taints1 : T.taints) (taints2 : T.taints) ->
                let taints = T.Taint_set.union ~traces taints1 taints2 in
                if T.Taint_set.equal_with_guards taints taints2 then taints2 else taints)
              ~local:(fun (i : int) ->
                List.fold_left
                  (fun (taints : T.taints) (edge : edge) ->
                    let taints =
                      match edge.xtaint with
                      | `Tainted own -> T.Taint_set.union ~traces own taints
                      | `None
                      | `Clean ->
                          taints
                    in
                    match edge.target with
                    | Leaf shape -> gather_all_taints_in_shape_acc ~traces taints shape
                    | Node _ -> taints)
                  T.Taint_set.empty (own_edges i))
              dependencies
          in
          let has_relevant_content =
            least_fixpoint ~leq_join:( || )
              ~local:(fun (i : int) ->
                match canonical.(i) with
                | Closures _ -> true
                | Object _ ->
                    List.exists
                      (fun (edge : edge) ->
                        Xtaint.is_tainted edge.xtaint
                        ||
                        match edge.target with
                        | Leaf shape -> shape_has_relevant_content shape
                        | Node _ -> false)
                      (own_edges i))
              dependencies
          in
          Graph { nodes = canonical; root = 0; all_taints; has_relevant_content }

  let canonical ~(traces : T.kept_traces) (shape : shape) : shape =
    let holds_graph (Cell (_, shape) : cell) : bool =
      match shape with
      | Graph _ -> true
      | Bot
      | Obj _
      | Arg _
      | Fun _ ->
          false
    in
    let captures_graph (closure : closure) : bool =
      List.exists
        (fun ((_, entry) : IL.name * env_entry) ->
          match entry with
          | Val cell -> holds_graph cell
          | Ref _ -> false)
        closure.env
    in
    let minimise_node (to_node : (cell -> edge) -> node) : shape =
      let builder = Dynarray.create () in
      let node =
        to_node (fun (Cell (xtaint, shape) : cell) ->
            { xtaint; target = target_of_shape builder shape })
      in
      Dynarray.add_last builder node;
      minimise ~traces (Dynarray.to_array builder)
        (Node (Dynarray.length builder - 1))
    in
    match shape with
    | Obj { sites; summary; fields }
      when Fields.exists (fun _ (cell : cell) -> holds_graph cell) fields ->
        minimise_node (fun (edge_of : cell -> edge) ->
            Object { sites; summary; edges = Fields.map edge_of fields })
    | Fun (c, cs) when List.exists captures_graph (c :: cs) ->
        minimise_node (fun (edge_of : cell -> edge) ->
            let graph_closure (closure : closure) : graph_closure =
              {
                def = closure.def;
                sig_ = closure.sig_;
                env =
                  List.map
                    (fun ((x, entry) : IL.name * env_entry) ->
                      match entry with
                      | Ref lval -> (x, (Ref lval : graph_env_entry))
                      | Val cell -> (x, Val (edge_of cell)))
                    closure.env;
              }
            in
            let c = graph_closure c in
            Closures (c, List.map graph_closure cs))
    | Bot
    | Obj _
    | Graph _
    | Arg _
    | Fun _ ->
        shape

  (*************************************)
  (* Union of sites *)
  (*************************************)

  let rec union_sites_in_cell ~(traces : T.kept_traces)
      (Cell (xtaint, shape1) as cell1 : cell) (Cell (_, shape2) : cell) : cell =
    let shape = union_sites ~traces shape1 shape2 in
    if phys_equal shape shape1 then cell1 else Cell (xtaint, shape)

  (* On two graphs, the pairs of nodes at one position form a bisimulation
     modulo sites; the result has one node per pair with the union of the
     two site sets. *)
  and union_sites_in_graph ~(traces : T.kept_traces) (g1 : graph) (g2 : graph) :
      shape option =
    let union_edges (edge1 : edge) (edge2 : edge) : target option =
      match (edge1.target, edge2.target) with
      | Leaf shape1, Leaf shape2 ->
          let shape = union_sites ~traces shape1 shape2 in
          if phys_equal shape shape1 then None else Some (Leaf shape)
      | (Node _ | Leaf _), _ -> None
    in
    let adds_sites ((i, j) : int * int) : bool =
      match (g1.nodes.(i), g2.nodes.(j)) with
      | Object object1, Object object2 ->
          (not (Sites.subset object2.sites object1.sites))
          || Fields.exists
               (fun (o : T.offset) (edge1 : edge) ->
                 match Fields.find_opt o object2.edges with
                 | Some edge2 -> Option.is_some (union_edges edge1 edge2)
                 | None -> false)
               object1.edges
      | (Object _ | Closures _), _ -> (
          let edges1 = node_edges g1.nodes.(i) in
          let edges2 = node_edges g2.nodes.(j) in
          match List.compare_lengths edges1 edges2 with
          | 0 ->
              List.exists2
                (fun (edge1 : edge) (edge2 : edge) ->
                  Option.is_some (union_edges edge1 edge2))
                edges1 edges2
          | _ -> false)
    in
    match
      Pair_dfs.iter_component
        ~pre:(fun (pair : int * int) -> if adds_sites pair then raise_notrace Exit)
        (g1, g2) (g1.root, g2.root)
    with
    | () -> None
    | exception Exit ->
        let pairs =
          Pair_dfs.fold_component
            (fun (pair : int * int) (pairs : (int * int) list) -> pair :: pairs)
            [] (g1, g2) (g1.root, g2.root)
          |> List.rev |> Array.of_list
        in
        let index = Pair_tbl.create (Array.length pairs) in
        Array.iteri (fun (k : int) (pair : int * int) -> Pair_tbl.add index pair k) pairs;
        (* The nodes of [pairs] first, then the copies of the parts of [g1]
           that [g2] does not pair. *)
        let builder = Dynarray.of_array (Array.map (fun ((i, _) : int * int) -> g1.nodes.(i)) pairs) in
        let copy (target : target) : target =
          match target with
          | Node i -> Node (append_graph builder { g1 with root = i })
          | Leaf _ -> target
        in
        let paired (edge1 : edge) (edge2 : edge) : edge =
          match (edge1.target, edge2.target) with
          | Node i, Node j -> { edge1 with target = Node (Pair_tbl.find index (i, j)) }
          | _ -> (
              match union_edges edge1 edge2 with
              | Some target -> { edge1 with target }
              | None -> { edge1 with target = copy edge1.target })
        in
        Array.iteri
          (fun (k : int) ((i, j) : int * int) ->
            Dynarray.set builder k
              (match (g1.nodes.(i), g2.nodes.(j)) with
              | Object object1, Object object2 ->
                  Object
                    {
                      object1 with
                      sites =
                        (if Sites.subset object2.sites object1.sites then
                           object1.sites
                         else Sites.union object1.sites object2.sites);
                      edges =
                        Fields.mapi
                          (fun (o : T.offset) (edge1 : edge) ->
                            match Fields.find_opt o object2.edges with
                            | Some edge2 -> paired edge1 edge2
                            | None -> { edge1 with target = copy edge1.target })
                          object1.edges;
                    }
              | Closures (c1, cs1), Closures (c2, cs2)
                when Int.equal (List.compare_lengths cs1 cs2) 0 ->
                  let paired_closure (closure1 : graph_closure)
                      (closure2 : graph_closure) : graph_closure =
                    {
                      closure1 with
                      env =
                        List.map2
                          (fun ((x, entry1) as binding : IL.name * graph_env_entry)
                               ((_, entry2) : IL.name * graph_env_entry) ->
                            match (entry1, entry2) with
                            | Val edge1, Val edge2 ->
                                (x, (Val (paired edge1 edge2) : graph_env_entry))
                            | Val edge1, Ref _ ->
                                (x, Val { edge1 with target = copy edge1.target })
                            | Ref _, _ -> binding)
                          closure1.env closure2.env;
                    }
                  in
                  Closures (paired_closure c1 c2, List.map2 paired_closure cs1 cs2)
              | node1, _ -> map_targets copy node1))
          pairs;
        Some (minimise ~traces (Dynarray.to_array builder) (Node 0))

  and union_sites ~(traces : T.kept_traces) (shape1 : shape) (shape2 : shape) :
      shape =
    if phys_equal shape1 shape2 then shape1
    else
      match (shape1, shape2) with
      | ( Obj { sites = sites1; summary; fields = fields1 },
          Obj { sites = sites2; fields = fields2; _ } ) ->
          let sites =
            if Sites.subset sites2 sites1 then sites1
            else Sites.union sites1 sites2
          in
          let fields =
            Fields.fold
              (fun o cell1 fields ->
                match Fields.find_opt o fields2 with
                | Some cell2 ->
                    let cell = union_sites_in_cell ~traces cell1 cell2 in
                    if phys_equal cell cell1 then fields
                    else Fields.add o cell fields
                | None -> fields)
              fields1 fields1
          in
          if phys_equal sites sites1 && phys_equal fields fields1 then shape1
          else Obj { sites; summary; fields }
      | Fun (c1, cs1), Fun (c2, cs2)
        when Int.equal (List.length cs1) (List.length cs2) ->
          let union_closure (closure1 : closure) (closure2 : closure) :
              closure =
            let env =
              List.map2
                (fun ((var, entry1) as binding : IL.name * env_entry)
                     ((_, entry2) : IL.name * env_entry) ->
                  match (entry1, entry2) with
                  | Val cell1, Val cell2 ->
                      let cell = union_sites_in_cell ~traces cell1 cell2 in
                      if phys_equal cell cell1 then binding else (var, Val cell)
                  | (Val _ | Ref _), _ -> binding)
                closure1.env closure2.env
            in
            if List.for_all2 phys_equal env closure1.env then closure1
            else { closure1 with env }
          in
          let c = union_closure c1 c2 in
          let cs = List.map2 union_closure cs1 cs2 in
          if phys_equal c c1 && List.for_all2 phys_equal cs cs1 then shape1
          else Fun (c, cs)
      | Graph g1, Graph g2 -> (
          match union_sites_in_graph ~traces g1 g2 with
          | Some shape -> shape
          | None -> shape1)
      | (Bot | Obj _ | Graph _ | Arg _ | Fun _), _ -> shape1

  (*************************************)
  (* Pretty-printing *)
  (*************************************)

  let rec show_cell cell =
    let (Cell (xtaint, shape)) = cell in
    spf "cell<%s>(%s)" (Xtaint.show xtaint) (show_shape shape)

  and show_shape = function
    | Bot -> "_|_"
    | Obj { fields; _ } -> spf "obj {|%s|}" (show_obj fields)
    | Graph g ->
        preorder (Array.map successors g.nodes) g.root
        |> List.map (fun (i : int) -> spf "%d: %s" i (show_node g.nodes.(i)))
        |> String.concat "; "
        |> spf "graph<%d> {|%s|}" g.root
    | Arg (arg, []) ->
        (* No offsets recorded — should not arise from normal
           construction. *)
        "'{" ^ T.show_formal arg ^ "}"
    | Arg (arg, [ [] ]) ->
        (* Single empty offset — bare-parameter shape. *)
        "'{" ^ T.show_formal arg ^ "}"
    | Arg (arg, [ off ]) ->
        (* Single non-empty offset. *)
        "'{" ^ T.show_formal arg
        ^ (off |> List.map T.show_offset |> String.concat "")
        ^ "}"
    | Arg (arg, offsets) ->
        (* Disjunctive — multiple alternative offsets, rendered
           [arg<off1 | off2 | ...>]. *)
        let show_offset_path off =
          off |> List.map T.show_offset |> String.concat ""
        in
        let offsets_str =
          offsets |> List.map show_offset_path |> String.concat " | "
        in
        "'{" ^ T.show_formal arg ^ offsets_str ^ "}"
    | Fun (c, cs) -> c :: cs |> List.map show_closure |> String.concat " | "

  and show_closure (c : closure) =
    match c.env with
    | [] -> Signature.show c.sig_
    | env -> spf "%s with [%s]" (Signature.show c.sig_) (show_env env)

  and show_env env =
    env
    |> List.map (fun ((x : IL.name), entry) ->
           match entry with
           | Ref lval -> spf "%s -> &%s" (fst x.ident) (T.show_lval lval)
           | Val cell -> spf "%s -> %s" (fst x.ident) (show_cell cell))
    |> String.concat "; "

  and show_obj obj =
    obj |> Fields.to_seq
    |> Seq.map (fun (o, o_cell) ->
           spf "%s: %s" (T.show_offset o) (show_cell o_cell))
    |> List.of_seq |> String.concat "; "

  and show_node (node : node) : string =
    let show_edge (edge : edge) : string =
      match edge.target with
      | Node i -> spf "cell<%s>(node<%d>)" (Xtaint.show edge.xtaint) i
      | Leaf shape -> show_cell (Cell (edge.xtaint, shape))
    in
    match node with
    | Object { edges; _ } ->
        edges |> Fields.to_seq
        |> Seq.map (fun ((o, edge) : T.offset * edge) ->
               spf "%s: %s" (T.show_offset o) (show_edge edge))
        |> List.of_seq |> String.concat "; " |> spf "obj {|%s|}"
    | Closures (c, cs) ->
        c :: cs
        |> List.map (fun (closure : graph_closure) ->
               match closure.env with
               | [] -> Signature.show closure.sig_
               | env ->
                   env
                   |> List.map (fun ((x, entry) : IL.name * graph_env_entry) ->
                          match entry with
                          | Ref lval -> spf "%s -> &%s" (fst x.ident) (T.show_lval lval)
                          | Val edge -> spf "%s -> %s" (fst x.ident) (show_edge edge))
                   |> String.concat "; "
                   |> spf "%s with [%s]" (Signature.show closure.sig_))
        |> String.concat " | "
end

(*****************************************************************************)
(* Taint results & signatures *)
(*****************************************************************************)
and Effect : sig
  type sink = { pm : Core_match.t; rule_sink : R.taint_sink }
  (** A sink match with its corresponding sink specification (one of the
      `pattern-sinks`). *)

  type taint_to_sink_item = {
    taint : Taint.taint;
    sink_trace : unit Taint.call_trace;
        (** This trace is from the current calling context of the taint finding,
            to the sink. It's a `unit` call_trace because we don't actually need
            the item at the end, and we need to be able to dispatch on the
            particular variant of taint (source or arg). *)
    guard : Effect_guard.t;
        (** The guard the taint carried when it reached the sink
            ([Taint.guarded_taint]); evaluated, like the effect-level
            [guards], when the effect becomes a match — an item whose guard
            folds to false is not reported. Ignored by [compare]; fused
            per-item by [fuse_guards]. *)
  }

  type taints_to_sink = {
    taints_with_precondition : taint_to_sink_item list * Rule.precondition;
        (** Taints reaching the sink and the precondition for the sink to apply.
        *)
    sink : sink;
    merged_env : Metavariable.bindings;
        (** The metavariable environment that results of merging the environment
            from * matching the source and the one from matching the sink. *)
    guards : Effect_guard.t;
        (** Engine-emitted arity/shape/branch guard. Evaluated at signature
            instantiation against the caller's actual arguments; if it folds
            to definitely-false the effect is dropped before producing a
            finding. [Effect_guard.top] = unconstrained. *)
  }

  type taints_to_return = {
    data_taints : Taint.taints;
        (** The taints of the data being returned (typical data propagated via
            data flow). *)
    data_shape : Shape.shape;  (** The shape of the data being returned. *)
    multiple_results : bool;
        (** The positions of [data_shape] are the function's results, as in
            Go's [return v, err], not a level of one value. *)
    control_taints : Taint.taints;
        (** The taints propagated via the control flow (cf., `control: true`
            sources) * used for reachability queries. *)
    return_tok : AST_generic.tok;
    guards : Effect_guard.t;
        (** See note on [taints_to_sink.guards]. *)
  }

  type taints_to_lval = {
    taints : Taint.taints;
    shape : Shape.shape;  (** The shape of the value written to [lval]. *)
    lval : Taint.lval;
    guards : Effect_guard.t;
        (** See note on [taints_to_sink.guards]. *)
  }

  type args_taints = (Taint.taints * Shape.shape) IL.argument list [@@deriving eq]
  (** The taints and shapes associated with the actual arguments in a * function
      call. *)

  (** Function-level result. * * 'ToSink' results where a taint source reaches a
      sink are candidates for * actual Semgrep findings, although some may be
      dropped by deduplication. * * Results are computed for each
      function/method definition, and formulated * using 'lval' taints to act as
      placeholders of the taint that may be passed * by an arbitrary caller via
      the function arguments. Thus the results are *
      polymorphic/context-sensitive, as the 'lval' taints can be instantiated *
      accordingly at each call site. *)
  type t =
    | ToSink of taints_to_sink
        (** Taints reach a sink.
        *
        * For example:
        *
        *     def foo(x):
        *         y = x
        *         sink(y)
        *
        * The parameter `x` could be tainted depending on the calling context,
        * so we infer:
        *
        *     ToSink { taints_with_precondition = (["taint"], PBool true);
        *              sink = "sink(y)";
        *              ... }
        *)
    | ToReturn of taints_to_return
        (** Taints reach a `return` statement. * * For example: * * def foo(): *
            x = "taint" * return x * * We infer: * * ToReturn(["taint"], Bot,
            ...) *)
    | ToLval of taints_to_lval
        (** Taints reach an l-value in the scope of the function/method. * * For
            example: * * x = ["ok"] * * def foo(): * global x * x[0] = "taint" *
            * We infer: * * ToLval {taints = ["taint"]; lval = "x[0]"; guards =
            []} * * TODO: Record taint shapes. *)
    | ToSinkInCall of {
        callee : IL.exp;
            (** The function expression being called, it is used for recording a
                taint trace. *)
        arg : Taint.formal;
            (** The formal (a parameter, or a variable captured by the
                closure) holding the function, this is what we instantiate
                at a specific call site. *)
        arg_offset : Taint.offset list;
            (** When the callback was obtained via indexing/field access into
                [arg] (e.g. [callback = impl[0]] after destructuring a packed
                argument list), this is the offset path from [arg] to the
                callback. Empty when [arg] itself is the callback. *)
        args_taints : args_taints;
        guards : Effect_guard.t;
            (** See note on [taints_to_sink.guards]. *)
      }
        (** Essentially a preliminary form of "effect variable". It represents *
            the effects of a function call where the function is not *
            yet known (the function is an argument to be instantiated at call *
            site). The call's result is the base [BCall] of that call, which *
            resolves to what the callback returns. *)

  val compare : t -> t -> int
  (** Guard-excluding: two effects equal up to their [guards] compare as 0,
      so an [Effects] set holds one element per guard-less effect identity
      and fuses guards on insertion (see [fuse_guards]). *)

  val fuse_guards : traces:Taint.kept_traces -> t -> t -> t
  (** [fuse_guards e1 e2], where [compare e1 e2 = 0], is [e1] with every
      guard-bearing payload fused disjunctively: the effect-level guard, the
      per-item [ToSink] guards, and the guards of the guarded taints inside the [Taints.t]
      payloads. The fused effect applies iff either of the two would. *)

  val guards_equal : t -> t -> bool
  val shapes_shared : t -> t -> bool
  val traces_shared : t -> t -> bool
  (** Whether two identity-equal effects carry the same guards in every
      guard-bearing payload. Insertion no-op checks and fixpoint stability
      tests must use this; effect identity ([compare]) is guard-blind. *)

  val show : ?truncate_guards:bool -> t -> string

  val show_written_shape : Shape.shape -> string
  (** The shape a [ToLval] writes, as printed: nothing for [Bot]. *)

  val add_guards : Effect_guard.t -> t -> t
  (** [add_guards g eff] returns [eff] with [g] composed via [And] into its
      [guards] field. [Effect_guard.top] returns [eff] unchanged. Used by the
      transfer when stamping effects with the branch context they were
      recorded under. *)

  val guards_of : t -> Effect_guard.t
  (** Return the guard stamped on an effect. *)

  val compare_taints_to_return : taints_to_return -> taints_to_return -> int
  val compare_taints_to_sink : taints_to_sink -> taints_to_sink -> int
  (* Mainly for debugging *)
  val show_sink : sink -> string
  val show_args_taints : ?truncate_guards:bool -> args_taints -> string
  val show_taints_to_sink : ?truncate_guards:bool -> taints_to_sink -> string

  val show_taints_to_return :
    ?truncate_guards:bool -> taints_to_return -> string
end = struct
  module Taints = Taint.Taint_set

  type sink = { pm : Core_match.t; rule_sink : R.taint_sink }
  type taint_to_sink_item = {
    taint : T.taint;
    sink_trace : unit T.call_trace;
    guard : Effect_guard.t;
  }

  type taints_to_sink = {
    (* These taints were incoming to the sink, under a certain
       REQUIRES expression.
       When we discharge the taint signature, we will produce
       a certain number of findings suitable to how the sink was
       reached.
    *)
    taints_with_precondition : taint_to_sink_item list * R.precondition;
    sink : sink;
    merged_env : Metavariable.bindings;
    guards : Effect_guard.t;
  }

  type taints_to_return = {
    data_taints : Taint.taints;
    data_shape : Shape.shape;
    multiple_results : bool;
    control_taints : Taint.taints;
    return_tok : AST_generic.tok;
    guards : Effect_guard.t;
  }

  type taints_to_lval = {
    taints : T.taints;
    shape : Shape.shape;
    lval : T.lval;
    guards : Effect_guard.t;
  }

  type args_taints = (Taints.t * Shape.shape) IL.argument list
  [@@deriving ord, eq]

  type t =
    | ToSink of taints_to_sink
    | ToReturn of taints_to_return
    | ToLval of taints_to_lval
    | ToSinkInCall of {
        callee : IL.exp;
        arg : Taint.formal;
        arg_offset : Taint.offset list;
        args_taints : args_taints;
        guards : Effect_guard.t;
      }

  (*************************************)
  (* Comparison *)
  (*************************************)

  let compare_sink { pm = pm1; rule_sink = sink1 }
      { pm = pm2; rule_sink = sink2 } =
    match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
    | 0 -> T.compare_matches pm1 pm2
    | other -> other

  let compare_taint_to_sink_item { taint = taint1; sink_trace = _; guard = _ }
      { taint = taint2; sink_trace = _; guard = _ } =
    T.compare_taint taint1 taint2

  (* The [compare_*] below ignore [guards]: an effect's identity is its
   * guard-less content, and [Effects] fuses the guards of same-identity
   * effects on insertion (see [fuse_guards]). *)
  let compare_taints_to_sink
      {
        taints_with_precondition = ttsis1, pre1;
        sink = sink1;
        merged_env = env1;
        guards = _;
      }
      {
        taints_with_precondition = ttsis2, pre2;
        sink = sink2;
        merged_env = env2;
        guards = _;
      } =
    match compare_sink sink1 sink2 with
    | 0 -> (
        match List.compare compare_taint_to_sink_item ttsis1 ttsis2 with
        | 0 -> (
            match R.compare_precondition pre1 pre2 with
            | 0 -> T.compare_metavar_env env1 env2
            | other -> other)
        | other -> other)
    | other -> other

  let compare_taints_to_return
      {
        data_taints = data_taints1;
        data_shape = data_shape1;
        multiple_results = multiple_results1;
        control_taints = control_taints1;
        return_tok = _;
        guards = _;
      }
      {
        data_taints = data_taints2;
        data_shape = data_shape2;
        multiple_results = multiple_results2;
        control_taints = control_taints2;
        return_tok = _;
        guards = _;
      } =
    (* [multiple_results] first: a constant-time comparison, ahead of the
     * taint sets, whose comparison compares sources. *)
    match Bool.compare multiple_results1 multiple_results2 with
    | 0 -> (
        match Taints.compare data_taints1 data_taints2 with
        | 0 -> (
            match Shape.compare_shape data_shape1 data_shape2 with
            | 0 -> Taints.compare control_taints1 control_taints2
            | other -> other)
        | other -> other)
    | other -> other

  (* Lexicographic on the lvalue, then the taints, then the shape: the
   * lvalue comparison is cheap and separates most pairs, so the taint sets,
   * whose comparison compares sources, are compared only between effects
   * on the same lvalue. *)
  let compare_taints_to_lval
      { taints = ts1; shape = shape1; lval = lv1; guards = _ }
      { taints = ts2; shape = shape2; lval = lv2; guards = _ } =
    match T.compare_lval lv1 lv2 with
    | 0 -> (
        match Taints.compare ts1 ts2 with
        | 0 -> Shape.compare_shape shape1 shape2
        | other -> other)
    | other -> other

  let compare_arg (arg1 : _ IL.argument) (arg2 : _ IL.argument) =
    let compare_taints_and_shape (taints1, shape1) (taints2, shape2) =
      match Taints.compare taints1 taints2 with
      | 0 -> Shape.compare_shape shape1 shape2
      | other -> other
    in
    match (arg1, arg2) with
    | Unnamed (taints1, shape1), Unnamed (taints2, shape2) ->
        compare_taints_and_shape (taints1, shape1) (taints2, shape2)
    | Named (name1, (taints1, shape1)), Named (name2, (taints2, shape2)) -> (
        match AST_generic.compare_ident name1 name2 with
        | 0 -> compare_taints_and_shape (taints1, shape1) (taints2, shape2)
        | other -> other)
    | Unnamed _, Named _ -> -1
    | Named _, Unnamed _ -> 1

  let compare r1 r2 =
    match (r1, r2) with
    | ToSink tts1, ToSink tts2 -> compare_taints_to_sink tts1 tts2
    | ToReturn ttr1, ToReturn ttr2 -> compare_taints_to_return ttr1 ttr2
    | ToLval ttl1, ToLval ttl2 -> compare_taints_to_lval ttl1 ttl2
    | ( ToSinkInCall
          {
            callee = fexp1;
            arg = fvar1;
            arg_offset = foff1;
            args_taints = args_taints1;
            guards = _;
          },
        ToSinkInCall
          {
            callee = fexp2;
            arg = fvar2;
            arg_offset = foff2;
            args_taints = args_taints2;
            guards = _;
          } ) -> (
        (* Comparing "fvar"s is cheap so better to do it first. *)
        match T.compare_formal fvar1 fvar2 with
        | 0 -> (
            match List.compare T.compare_offset foff1 foff2 with
            | 0 -> (
                match IL.compare_orig fexp1.eorig fexp2.eorig with
                | 0 -> List.compare compare_arg args_taints1 args_taints2
                | other -> other)
            | other -> other)
        | other -> other)
    | ToSink _, (ToReturn _ | ToLval _ | ToSinkInCall _) -> -1
    | ToReturn _, (ToLval _ | ToSinkInCall _) -> -1
    | ToLval _, ToSinkInCall _ -> -1
    | ToReturn _, ToSink _ -> 1
    | ToLval _, (ToSink _ | ToReturn _) -> 1
    | ToSinkInCall _, (ToSink _ | ToReturn _ | ToLval _) -> 1

  (*************************************)
  (* Pretty-printing *)
  (*************************************)

  let show_sink { rule_sink; pm } =
    let matched_str =
      let tok1, tok2 = pm.range_loc in
      let r = Range.range_of_token_locations tok1 tok2 in
      Range.content_at_range pm.path.internal_path_to_content r
    in
    let matched_line =
      let loc1, _ = pm.range_loc in
      loc1.Tok.pos.line
    in
    spf "(%s at l.%d by %s)" matched_str matched_line rule_sink.R.sink_id

  let show_taint_to_sink_item ?(truncate_guards = true)
      { taint; sink_trace; guard } =
    let sink_trace_str =
      match sink_trace with
      | T.PM _ -> ""
      | T.Call _ -> spf "@{%s}" (Taint.show_call_trace [%show: unit] sink_trace)
    in
    Printf.sprintf "%s%s%s" (T.show_taint taint)
      (Effect_guard.show_in_brackets ~truncate_guards guard)
      sink_trace_str

  let show_taints_and_traces ?(truncate_guards = true) taints =
    Common2.string_of_list (show_taint_to_sink_item ~truncate_guards) taints

  let show_taints_to_sink ?(truncate_guards = true)
      { taints_with_precondition = taints, _; sink; guards; _ } =
    Common.spf "%s%s ~~~> %s"
      (show_taints_and_traces ~truncate_guards taints)
      (Effect_guard.show_in_brackets ~truncate_guards guards)
      (show_sink sink)

  let show_taints_to_return ?(truncate_guards = true)
      { data_taints; data_shape; control_taints; guards; _ } =
    Printf.sprintf "return%s (%s & %s & CTRL:%s)"
      (Effect_guard.show_in_brackets ~truncate_guards guards)
      (T.show_taints ~truncate_guards data_taints)
      (Shape.show_shape data_shape)
      (T.show_taints ~truncate_guards control_taints)

  let show_arg ?(truncate_guards = true) (arg : _ IL.argument) =
    match arg with
    | Unnamed (taints, shape) ->
        spf "%s & %s" (T.show_taints ~truncate_guards taints) (Shape.show_shape shape)
    | Named (ident, (taints, shape)) ->
        spf "%s:(%s & %s)" (fst ident) (T.show_taints ~truncate_guards taints)
          (Shape.show_shape shape)

  let show_args_taints ?(truncate_guards = true) (args : _ IL.argument list) =
    spf "(%s)" (List_.map (show_arg ~truncate_guards) args |> String.concat ", ")

  let show_written_shape (shape : Shape.shape) : string =
    match shape with
    | Shape.Bot -> ""
    | Shape.Obj _
    | Shape.Graph _
    | Shape.Arg _
    | Shape.Fun _ ->
        " & " ^ Shape.show_shape shape

  let show ?(truncate_guards = true) = function
    | ToSink tts -> show_taints_to_sink ~truncate_guards tts
    | ToReturn ttr -> show_taints_to_return ~truncate_guards ttr
    | ToLval { taints; shape; lval; guards } ->
        Printf.sprintf "%s%s%s ----> %s" (T.show_taints ~truncate_guards taints)
          (show_written_shape shape)
          (Effect_guard.show_in_brackets ~truncate_guards guards) (T.show_lval lval)
    | ToSinkInCall { callee = _; arg; args_taints; guards; _ } ->
        Printf.sprintf "'call<%s>%s%s" (T.show_formal arg)
          (show_args_taints ~truncate_guards args_taints)
          (Effect_guard.show_in_brackets ~truncate_guards guards)

  let add_guards g eff =
    if Effect_guard.is_top g then eff
    else
      match eff with
      | ToSink tts ->
          ToSink { tts with guards = Effect_guard.compose_and tts.guards g }
      | ToReturn ttr ->
          ToReturn { ttr with guards = Effect_guard.compose_and ttr.guards g }
      | ToLval ttl ->
          ToLval { ttl with guards = Effect_guard.compose_and ttl.guards g }
      | ToSinkInCall r ->
          ToSinkInCall
            { r with guards = Effect_guard.compose_and r.guards g }

  let guards_of = function
    | ToSink { guards; _ } -> guards
    | ToReturn { guards; _ } -> guards
    | ToLval { guards; _ } -> guards
    | ToSinkInCall { guards; _ } -> guards

  (* Precondition: [compare e1 e2 = 0], so both are the same constructor.
   * The fused effect applies iff either input would, hence the [Or]. *)
  let fuse_guards ~(traces : T.kept_traces) e1 e2 =
    match (e1, e2) with
    | ToSink tts1, ToSink tts2 ->
        (* [compare e1 e2 = 0] gives pairwise identity-equal items in the same
         * order, so the per-item guards fuse positionally. *)
        let items1, pre = tts1.taints_with_precondition in
        let items2, _ = tts2.taints_with_precondition in
        let items =
          List.map2
            (fun (i1 : taint_to_sink_item) (i2 : taint_to_sink_item) ->
              let guard1 = Effect_guard.compose_and tts1.guards i1.guard in
              let guard2 = Effect_guard.compose_and tts2.guards i2.guard in
              let same_traces =
                T.same_trace i1.taint i2.taint
                && phys_equal i1.sink_trace i2.sink_trace
              in
              let i1 =
                match traces with
                | T.One_trace_per_guard
                  when (not same_traces)
                       && Effect_guard.equal guard1 guard2
                       && T.compare_traces i2.taint (Some i2.sink_trace)
                            i1.taint (Some i1.sink_trace)
                          < 0 ->
                    { i1 with taint = i2.taint; sink_trace = i2.sink_trace }
                | T.One_trace_per_guard
                | T.All_traces ->
                    i1
              in
              let taint =
                match traces with
                | T.One_trace_per_guard
                  when Effect_guard.equal guard1 guard2 || same_traces
                  ->
                    i1.taint
                | T.All_traces when same_traces -> i1.taint
                | T.One_trace_per_guard
                | T.All_traces ->
                    T.merge_items ~traces
                      ~kept:(guard1, i1.taint, i1.sink_trace)
                      ~other:(guard2, i2.taint, i2.sink_trace)
              in
              { i1 with taint; guard = Effect_guard.compose_or i1.guard i2.guard })
            items1 items2
        in
        ToSink
          { tts1 with
            taints_with_precondition = (items, pre);
            guards = Effect_guard.compose_or tts1.guards tts2.guards }
    | ToReturn ttr1, ToReturn ttr2 ->
        (* [Taints.union] fuses identity-equal guarded taints' guards via
         * [compose_or]; on identity-equal sets that is exactly per-guarded-taint
         * guard fusion. *)
        ToReturn
          { ttr1 with
            data_taints = Taints.union ~traces ttr1.data_taints ttr2.data_taints;
            data_shape = Shape.union_sites ~traces ttr1.data_shape ttr2.data_shape;
            control_taints =
              Taints.union ~traces ttr1.control_taints ttr2.control_taints;
            guards = Effect_guard.compose_or ttr1.guards ttr2.guards }
    | ToLval ttl1, ToLval ttl2 ->
        ToLval
          { ttl1 with
            taints = Taints.union ~traces ttl1.taints ttl2.taints;
            shape = Shape.union_sites ~traces ttl1.shape ttl2.shape;
            guards = Effect_guard.compose_or ttl1.guards ttl2.guards }
    | ToSinkInCall c1, ToSinkInCall c2 ->
        let fuse_arg (a1 : (Taints.t * Shape.shape) IL.argument)
            (a2 : (Taints.t * Shape.shape) IL.argument) :
            (Taints.t * Shape.shape) IL.argument =
          (* Shapes compare guard-blind and up to sites; the two shapes are
           * joined by their sites ([Shape.union_sites]). Guard refinement
           * inside [Fun] shapes is a known gap of the guard-blind
           * [Signature] equality. *)
          match (a1, a2) with
          | IL.Unnamed (t1, s1), IL.Unnamed (t2, s2) ->
              IL.Unnamed (Taints.union ~traces t1 t2, Shape.union_sites ~traces s1 s2)
          | IL.Named (id1, (t1, s1)), IL.Named (_, (t2, s2)) ->
              IL.Named
                (id1, (Taints.union ~traces t1 t2, Shape.union_sites ~traces s1 s2))
          | (IL.Unnamed _ | IL.Named _), _ ->
              a1 (* unreachable: identity-equal args have equal shape *)
        in
        ToSinkInCall
          { c1 with
            args_taints = List.map2 fuse_arg c1.args_taints c2.args_taints;
            guards = Effect_guard.compose_or c1.guards c2.guards }
    | _ -> e1 (* unreachable: differing constructors compare non-zero *)

  (* Whether two identity-equal effects ([compare] = 0) carry the same
   * guards, including every guard-bearing payload: [ToSink] item guards
   * and the guards of the guarded taints inside the [Taints.t] payloads (effect
   * identity compares those guard-blind). The [Effects] insertion no-op
   * check and the fixpoint stability tests must use this: comparing
   * [guards_of] alone misses payload-guard refinement, so a fused
   * [A or B] guard would be discarded for the narrower [A] — a lost
   * finding at any call site that refutes [A] but satisfies [B]. *)
  let guards_equal (e1 : t) (e2 : t) : bool =
    Effect_guard.equal (guards_of e1) (guards_of e2)
    &&
    match (e1, e2) with
    | ToSink tts1, ToSink tts2 ->
        List.for_all2
          (fun (i1 : taint_to_sink_item) (i2 : taint_to_sink_item) ->
            Effect_guard.equal i1.guard i2.guard)
          (fst tts1.taints_with_precondition)
          (fst tts2.taints_with_precondition)
    | ToReturn ttr1, ToReturn ttr2 ->
        Taints.equal_with_guards ttr1.data_taints ttr2.data_taints
        && Taints.equal_with_guards ttr1.control_taints ttr2.control_taints
    | ToLval ttl1, ToLval ttl2 ->
        Taints.equal_with_guards ttl1.taints ttl2.taints
    | ToSinkInCall c1, ToSinkInCall c2 ->
        List.for_all2
          (fun (a1 : (Taints.t * Shape.shape) IL.argument)
               (a2 : (Taints.t * Shape.shape) IL.argument) ->
            match (a1, a2) with
            | IL.Unnamed (t1, _), IL.Unnamed (t2, _)
            | IL.Named (_, (t1, _)), IL.Named (_, (t2, _)) ->
                Taints.equal_with_guards t1 t2
            | (IL.Unnamed _ | IL.Named _), _ -> true)
          c1.args_taints c2.args_taints
    | _ -> true

  let shapes_shared (e1 : t) (e2 : t) : bool =
    match (e1, e2) with
    | ToReturn ttr1, ToReturn ttr2 -> phys_equal ttr1.data_shape ttr2.data_shape
    | ToLval ttl1, ToLval ttl2 -> phys_equal ttl1.shape ttl2.shape
    | ToSinkInCall c1, ToSinkInCall c2 ->
        List.for_all2
          (fun (a1 : (Taints.t * Shape.shape) IL.argument)
               (a2 : (Taints.t * Shape.shape) IL.argument) ->
            match (a1, a2) with
            | IL.Unnamed (_, s1), IL.Unnamed (_, s2)
            | IL.Named (_, (_, s1)), IL.Named (_, (_, s2)) ->
                phys_equal s1 s2
            | (IL.Unnamed _ | IL.Named _), _ -> true)
          c1.args_taints c2.args_taints
    | _ -> true

  let traces_shared (e1 : t) (e2 : t) : bool =
    let shared (taints1 : Taints.t) (taints2 : Taints.t) : bool =
      List.for_all2
        (fun (b1 : T.guarded_taint) (b2 : T.guarded_taint) ->
          T.shares_trace b1.taint b2.taint)
        (Taints.elements taints1) (Taints.elements taints2)
    in
    match (e1, e2) with
    | ToSink tts1, ToSink tts2 ->
        List.for_all2
          (fun (i1 : taint_to_sink_item) (i2 : taint_to_sink_item) ->
            T.shares_trace i1.taint i2.taint
            && phys_equal i1.sink_trace i2.sink_trace)
          (fst tts1.taints_with_precondition)
          (fst tts2.taints_with_precondition)
    | ToReturn ttr1, ToReturn ttr2 ->
        shared ttr1.data_taints ttr2.data_taints
        && shared ttr1.control_taints ttr2.control_taints
    | ToLval ttl1, ToLval ttl2 -> shared ttl1.taints ttl2.taints
    | ToSinkInCall c1, ToSinkInCall c2 ->
        List.for_all2
          (fun (a1 : (Taints.t * Shape.shape) IL.argument)
               (a2 : (Taints.t * Shape.shape) IL.argument) ->
            match (a1, a2) with
            | IL.Unnamed (t1, _), IL.Unnamed (t2, _)
            | IL.Named (_, (t1, _)), IL.Named (_, (t2, _)) ->
                shared t1 t2
            | (IL.Unnamed _ | IL.Named _), _ -> true)
          c1.args_taints c2.args_taints
    | _ -> true
end

and Effects : sig
  type elt = Effect.t
  type t

  val empty : t
  val singleton : elt -> t
  val cardinal : t -> int
  val find_opt : elt -> t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val filter : (elt -> bool) -> t -> t
  val exists : (elt -> bool) -> t -> bool
  val elements : t -> elt list
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val equal_with_guards : t -> t -> bool
  (** Like [equal] but also requires the guards of identity-equal elements to
      be equal ([Effect_guard.equal]). [Effect.compare] ignores guards, so
      plain [equal] treats a pass that only refines an effect's guard (fusing
      in a new disjunct) as unchanged; a fixpoint stability check using it
      would stop with the narrower guard and drop effects the refined guard
      keeps. Use this for stability checks. *)

  val add : traces:Taint.kept_traces -> elt -> t -> t
  val union : traces:Taint.kept_traces -> t -> t -> t
  val of_list : traces:Taint.kept_traces -> elt list -> t
  val map : traces:Taint.kept_traces -> (elt -> elt) -> t -> t
  val filter_map : traces:Taint.kept_traces -> (elt -> elt option) -> t -> t
  val show : ?truncate_guards:bool -> t -> string
  val add_list : traces:Taint.kept_traces -> Effect.t list -> t -> t
  val union_list : traces:Taint.kept_traces -> t list -> t
end = struct
  (* A set keyed by an expensive total order does the least comparison work
   * when an insertion walks the tree once and the order compares its
   * cheapest, most discriminating component first (see
   * [Effect.compare_taints_to_lval]). The set is therefore a map from the
   * guard-less effect identity ([Effect.compare]) to the element carrying
   * the guards, traces and shapes, so that an insertion that fuses is one
   * [EffectMap.update]. The set operations are expressed over the map's
   * values, in key order. *)
  module EffectMap = Map.Make (struct
    type t = Effect.t

    let compare effect1 effect2 = Effect.compare effect1 effect2
  end)

  type elt = Effect.t
  type t = Effect.t EffectMap.t

  let empty : t = EffectMap.empty
  let singleton (e : elt) : t = EffectMap.singleton e e
  let cardinal (s : t) : int = EffectMap.cardinal s
  let find_opt (e : elt) (s : t) : elt option = EffectMap.find_opt e s
  let fold f (s : t) acc = EffectMap.fold (fun _ e acc -> f e acc) s acc

  let filter (p : elt -> bool) (s : t) : t =
    EffectMap.filter (fun _ e -> p e) s

  let exists (p : elt -> bool) (s : t) : bool =
    EffectMap.exists (fun _ e -> p e) s

  let elements (s : t) : elt list = EffectMap.bindings s |> List.map snd

  let equal (s1 : t) (s2 : t) : bool =
    EffectMap.equal (fun (_ : elt) (_ : elt) -> true) s1 s2

  let compare (s1 : t) (s2 : t) : int =
    EffectMap.compare (fun (_ : elt) (_ : elt) -> 0) s1 s2

  (* [Effect.compare] ignores guards, so the set holds one element per
   * guard-less effect identity. Every inserting operation below fuses the
   * guards of colliding elements via [Effect.fuse_guards]. Letting the map
   * keep one variant and discard the other's guard would be unsound: at
   * instantiation an effect whose guard folds to false is dropped, so at a
   * call site where the kept variant's guard folds to false the effect
   * would be dropped even though the discarded variant's guard would have
   * kept it. The [Effect.guards_equal] check makes re-adding an
   * already-fused effect a no-op (disjunction is idempotent under the
   * clause-set dedup), so the dataflow fixpoint still reaches a fixed
   * point; it compares every guard-bearing payload, not just the
   * effect-level guard — a refinement of only an item guard or a guarded taint's guard must
   * not be discarded. On that no-op [fuse_element] returns the existing
   * element physically, and [EffectMap.update] then returns the map itself,
   * physically (Stdlib [Map.S.update]). *)
  let fuse_element ~(traces : T.kept_traces) (existing : elt) (eff : elt) : elt
      =
    let fused = Effect.fuse_guards ~traces existing eff in
    match traces with
    | T.One_trace_per_guard
      when Effect.guards_equal fused existing
           && Effect.traces_shared fused existing
           && Effect.shapes_shared fused existing ->
        existing
    | T.One_trace_per_guard
    | T.All_traces ->
        fused

  let add ~(traces : T.kept_traces) (eff : elt) (set : t) : t =
    EffectMap.update eff
      (fun (existing : elt option) ->
        match existing with
        | None -> Some eff
        | Some existing -> Some (fuse_element ~traces existing eff))
      set

  let union ~(traces : T.kept_traces) (s1 : t) (s2 : t) : t =
    (* Union the smaller set into the larger one: on a collision the larger
     * set's element is the existing one. *)
    let larger, smaller =
      if cardinal s1 >= cardinal s2 then (s1, s2) else (s2, s1)
    in
    EffectMap.union
      (fun _ (existing : elt) (eff : elt) ->
        Some (fuse_element ~traces existing eff))
      larger smaller

  let of_list ~(traces : T.kept_traces) (elts : elt list) : t =
    List.fold_left (fun (set : t) (e : elt) -> add ~traces e set) empty elts

  let map ~(traces : T.kept_traces) (f : elt -> elt) (s : t) : t =
    fold (fun (e : elt) (acc : t) -> add ~traces (f e) acc) s empty

  let filter_map ~(traces : T.kept_traces) (f : elt -> elt option) (s : t) : t
      =
    fold
      (fun (e : elt) (acc : t) ->
        match f e with
        | Some e' -> add ~traces e' acc
        | None -> acc)
      s empty

  (* [EffectMap.equal] walks both maps in key order and pairs each element
   * with its identity-equal counterpart. [Effect.guards_equal] covers every
   * guard-bearing payload, not just the effect-level guard. *)
  let equal_with_guards (s1 : t) (s2 : t) : bool =
    EffectMap.equal Effect.guards_equal s1 s2

  let show ?(truncate_guards = true) (s : t) =
    s |> elements
    |> List_.map (Effect.show ~truncate_guards)
    |> String.concat "; "

  let add_list ~(traces : T.kept_traces) elts t =
    List.fold_left (fun set e -> add ~traces e set) t elts

  let union_list ~(traces : T.kept_traces) ts =
    List.fold_left (union ~traces) empty ts
end

(** A (polymorphic) taint signature: simply a set of results for a function.
 *
 * Note that this signature is polymorphic/context-sensitive given that the
 * potential taints coming into the function via its arguments are represented
 * by 'lval' taints, that can be instantiated as needed.
 *
 * For example given:
 *
 *     def foo(x):
 *         sink(x.a)
 *
 * We infer the signature (simplified):
 *
 *     x => {ToSink {taints_with_precondition = [(x#0).a]; sink = ... ; ...}}
 *
 * where '(x#0).a' is taint variable that denotes the taint of the offset `.a`
 * of the parameter `x` (where '#0' means it is the first argument) of `foo`.
 * The signature tells us that '(x#0).a' will reach a sink.
 *
 * Given a concrete call `foo(obj)`, Semgrep will instantiate this signature with
 * taint assigned to `obj.a` in that calling context. If it is tainted, then
 * Semgrep will report a finding.
 *
 * Also note that, within each function, if there are multiple paths through
 * which a taint source may reach a sink, we do not keep all of them but only
 * the shortest one.
 *
 * THINK: Could we have a "taint shape" for functions/methods ?
 *)
and Signature : sig
  type t = {
    params : Signature_params.params;
    params_il : IL.param list;
        (** The IL.param list the signature was extracted from. Added
            so that the call-site instantiator can rewrite [Fetch]es
            to those parameters inside guards stored on the
            signature's effects (see [Sig_inst.substitute_in_sig]).
            Excluded from [equal] and [compare]: this is
            instantiation metadata, not part of the signature's
            identity.

            TODO: [params] is fully derivable from [params_il] via
            [of_IL_params]. It was already present when [params_il]
            was added and is kept for now to avoid churning the
            consumers that read it. Consider deriving [params] lazily
            from [params_il] and dropping the field. *)
    captured : (IL.name * AST_generic.capture_mode) list;
        (** The variables of enclosing functions the code reads or writes;
            a closure's environment binds them (see [Shape.Fun]). *)
    effects : Effects.t;
  }
  (** * The 'params' act like an universal quantifier, we need them to later *
      instantiate the accompanying signature. *)

  val equal : t -> t -> bool

  val equal_with_guards : t -> t -> bool
  (** Like [equal] but a difference only in a guard counts as a difference
      ([Effects.equal_with_guards]). For the dataflow fixpoint's stability
      test, via [Shape.equal_cell_with_guards] on [Fun] shapes. *)

  val compare : t -> t -> int
  val show : ?truncate_guards:bool -> t -> string
end = struct
  (*************************************)
  (* Signatures *)
  (*************************************)

  type t = {
    params : Signature_params.params;
    params_il : IL.param list;
    captured : (IL.name * AST_generic.capture_mode) list;
    effects : Effects.t;
  }

  let compare_captured captured1 captured2 =
    List.compare
      (fun (x1, mode1) (x2, mode2) ->
        match IL.compare_name x1 x2 with
        | 0 -> AST_generic.compare_capture_mode mode1 mode2
        | other -> other)
      captured1 captured2

  (* [params_il] is instantiation metadata; identity is determined by
     [params], [captured] and [effects] alone. *)
  let equal
      { params = params1; params_il = _; captured = captured1; effects = effects1 }
      { params = params2; params_il = _; captured = captured2; effects = effects2 } =
    Signature_params.equal_params params1 params2
    && Int.equal (compare_captured captured1 captured2) 0
    && Effects.equal effects1 effects2

  let equal_with_guards
      { params = params1; params_il = _; captured = captured1; effects = effects1 }
      { params = params2; params_il = _; captured = captured2; effects = effects2 } =
    Signature_params.equal_params params1 params2
    && Int.equal (compare_captured captured1 captured2) 0
    && Effects.equal_with_guards effects1 effects2

  let compare (sig1 : t) (sig2 : t) =
    if phys_equal sig1 sig2 then 0
    else
    let { params = params1; params_il = _; captured = captured1; effects = effects1 } = sig1 in
    let { params = params2; params_il = _; captured = captured2; effects = effects2 } = sig2 in
    match Signature_params.compare_params params1 params2 with
    | 0 -> (
        match compare_captured captured1 captured2 with
        | 0 -> Effects.compare effects1 effects2
        | other -> other)
    | other -> other

  let show ?(truncate_guards = true) { params; params_il = _; captured; effects } =
    let captured =
      match captured with
      | [] -> ""
      | _ ->
          spf " captures %s"
            (captured
            |> List.map (fun ((x : IL.name), mode) ->
                   match (mode : AST_generic.capture_mode) with
                   | Capture_by_reference -> "&" ^ fst x.ident
                   | Capture_by_value -> fst x.ident)
            |> String.concat ", ")
    in
    spf "%s%s => {%s}" (Signature_params.show_params params) captured
      (Effects.show ~truncate_guards effects)
end

(*****************************************************************************)
(* Signature Database *)
(*****************************************************************************)

(* Function key for the signature database - uses just the function name (last element of fn_id).
   This matches the graph vertex type in Call_graph.ml. *)
type func_key = Function_id.t

module FunctionMap = Map.Make (Function_id)

(** Arity tag for disambiguating multi-arity function signatures.
    [Arity_exact n] matches call sites with exactly [n] arguments.
    [Arity_at_least n] matches call sites with >= [n] arguments (rest params). *)
type sig_arity = Arity_exact of int | Arity_at_least of int
[@@deriving show, eq, ord]

type extended_sig = {
  sig_ : Signature.t;
      [@printer
        fun fmt s ->
          Format.fprintf fmt "%s" (Signature.show ~truncate_guards:false s)]
  arity : sig_arity;
}
[@@deriving show]

module SignatureSet = struct
  include Set.Make (struct
    type t = extended_sig

    let compare = fun x y ->
      let sig_cmp = Signature.compare x.sig_ y.sig_ in
      if sig_cmp <> 0 then sig_cmp
      else compare_sig_arity x.arity y.arity
  end)

  (* [equal] pairs identity-equal elements positionally (both element lists
     are sorted by the guard-blind compare), so a parallel walk checks each
     signature against its counterpart. Fixpoint stability tests must use
     this: plain [equal] declares convergence while guards still refine,
     freezing whichever member last saw the narrower guard. *)
  let equal_with_guards s1 s2 =
    equal s1 s2
    && List.for_all2
         (fun (x : extended_sig) (y : extended_sig) ->
           Signature.equal_with_guards x.sig_ y.sig_)
         (elements s1) (elements s2)
end

(* [f] keeps the definition of each closure, so the order of the set holds;
   the list is physically unchanged when [f] changes none of its closures. *)
let map_closures (f : Shape.closure -> Shape.closure)
    (((c, cs) as closures) : Shape.closure * Shape.closure list) :
    Shape.closure * Shape.closure list =
  let cs' = List_.map f cs in
  let cs' = if List.for_all2 phys_equal cs' cs then cs else cs' in
  let c' = f c in
  if phys_equal c' c && phys_equal cs' cs then closures else (c', cs')

(* Rewrites the references of the closure environments in an effect's
   shapes. *)
let map_closure_refs ~(traces : T.kept_traces) (f : T.lval -> T.lval)
    (eff : Effect.t) : Effect.t =
  let rec map_shape (shape : Shape.shape) : Shape.shape =
    match shape with
    | Shape.Bot
    | Shape.Arg _ ->
        shape
    | Shape.Obj ({ fields; _ } as node) ->
        Shape.Obj { node with fields = Fields.map map_cell fields }
    | Shape.Graph g ->
        let map_target (target : Shape.target) : Shape.target =
          match target with
          | Shape.Node _ -> target
          | Shape.Leaf shape -> Shape.Leaf (map_shape shape)
        in
        let map_node (node : Shape.node) : Shape.node =
          match Shape.map_targets map_target node with
          | Shape.Object _ as node -> node
          | Shape.Closures (c, cs) ->
              let map_refs (closure : Shape.graph_closure) : Shape.graph_closure =
                {
                  closure with
                  env =
                    List_.map
                      (fun ((x, entry) as binding : IL.name * Shape.graph_env_entry) ->
                        match entry with
                        | Shape.Ref lval -> (x, (Shape.Ref (f lval) : Shape.graph_env_entry))
                        | Shape.Val _ -> binding)
                      closure.env;
                }
              in
              Shape.Closures (map_refs c, List_.map map_refs cs)
        in
        Shape.minimise ~traces (Array.map map_node g.nodes) (Shape.Node g.root)
    | Shape.Fun (c, cs) ->
        let c, cs =
          map_closures
            (fun (closure : Shape.closure) ->
              {
                closure with
                env =
                  List_.map
                    (fun (x, entry) ->
                      match entry with
                      | Shape.Ref lval -> (x, Shape.Ref (f lval))
                      | Shape.Val cell -> (x, Shape.Val (map_cell cell)))
                    closure.env;
              })
            (c, cs)
        in
        Shape.Fun (c, cs)
  and map_cell (Shape.Cell (xtaint, shape) : Shape.cell) : Shape.cell =
    Shape.Cell (xtaint, map_shape shape)
  in
  let map_arg = function
    | IL.Unnamed (taints, shape) -> IL.Unnamed (taints, map_shape shape)
    | IL.Named (id, (taints, shape)) -> IL.Named (id, (taints, map_shape shape))
  in
  match eff with
  | Effect.ToReturn ret ->
      Effect.ToReturn { ret with data_shape = map_shape ret.data_shape }
  | Effect.ToLval write ->
      Effect.ToLval { write with shape = map_shape write.shape }
  | Effect.ToSinkInCall call ->
      Effect.ToSinkInCall
        { call with args_taints = List_.map map_arg call.args_taints }
  | Effect.ToSink _ -> eff

(* Whether a reference of the closure environments in an effect's shapes
   satisfies [p]. *)
let exists_closure_ref (p : T.lval -> bool) (eff : Effect.t) : bool =
  let rec in_shape (shape : Shape.shape) : bool =
    match shape with
    | Shape.Bot
    | Shape.Arg _ ->
        false
    | Shape.Obj { fields; _ } ->
        Fields.exists (fun _ (Shape.Cell (_, shape)) -> in_shape shape) fields
    | Shape.Graph g ->
        Shape.preorder (Array.map Shape.successors g.nodes) g.root
        |> List.exists (fun (i : int) ->
               let node = g.nodes.(i) in
               List.exists
                 (fun (edge : Shape.edge) ->
                   match edge.target with
                   | Shape.Leaf shape -> in_shape shape
                   | Shape.Node _ -> false)
                 (Shape.node_edges node)
               ||
               match node with
               | Shape.Object _ -> false
               | Shape.Closures (c, cs) ->
                   List.exists
                     (fun (closure : Shape.graph_closure) ->
                       List.exists
                         (fun ((_, entry) : IL.name * Shape.graph_env_entry) ->
                           match entry with
                           | Shape.Ref lval -> p lval
                           | Shape.Val _ -> false)
                         closure.env)
                     (c :: cs))
    | Shape.Fun (c, cs) ->
        List.exists
          (fun (closure : Shape.closure) ->
            List.exists
              (fun (_, entry) ->
                match entry with
                | Shape.Ref lval -> p lval
                | Shape.Val (Shape.Cell (_, shape)) -> in_shape shape)
              closure.env)
          (c :: cs)
  in
  match eff with
  | Effect.ToReturn ret -> in_shape ret.data_shape
  | Effect.ToLval write -> in_shape write.shape
  | Effect.ToSinkInCall call ->
      List.exists
        (function
          | IL.Unnamed (_, shape)
          | IL.Named (_, (_, shape)) ->
              in_shape shape)
        call.args_taints
  | Effect.ToSink _ -> false

let fun_shape_of_definition ((def, sig_) : Function_id.t * Signature.t)
    (env : Shape.env) : Shape.shape =
  Shape.Fun ({ Shape.def; sig_; env }, [])

type signature_database = {
  signatures : SignatureSet.t FunctionMap.t;
}

(** Separate database for builtin function signatures.
    This is for builtin stdlib functions that aren't in the call graph. *)
module BuiltinMap = Map.Make(struct
  type t = string
  let compare = String.compare
end)

type builtin_signature_database = SignatureSet.t BuiltinMap.t

let empty_builtin_signature_database () : builtin_signature_database =
  BuiltinMap.empty

let add_builtin_signature (db : builtin_signature_database) (func_name : string)
    (signature : extended_sig) : builtin_signature_database =
  BuiltinMap.update func_name
    (fun existing_sigs ->
      match existing_sigs with
      | Some sigs -> Some (SignatureSet.add signature sigs)
      | None -> Some (SignatureSet.singleton signature))
    db

(** Extract the concrete arity from a [sig_arity] for comparison purposes. *)
let int_of_sig_arity : sig_arity -> int = function
  | Arity_exact n | Arity_at_least n -> n

(** Given a non-empty set of signatures, find the best match for [arity].
    Returns the unique sig if only one exists, then tries [Arity_exact arity],
    then falls back to the most specific [Arity_at_least n] where [n <= arity]. *)
let find_by_arity (sigs : SignatureSet.t) (arity : int) : extended_sig option =
  if Int.equal (SignatureSet.cardinal sigs) 1 then
    Some (SignatureSet.choose sigs)
  else
    let exact =
      SignatureSet.filter
        (fun (x : extended_sig) -> equal_sig_arity x.arity (Arity_exact arity))
        sigs
    in
    if Int.equal (SignatureSet.cardinal exact) 1 then
      Some (SignatureSet.choose exact)
    else
      (* Find the best Arity_at_least match: the largest n where n <= arity.
         In practice at most one variadic arity exists per function (Clojure,
         Python), so this fold typically finds zero or one match. *)
      SignatureSet.fold
        (fun (x : extended_sig) acc ->
          match x.arity with
          | Arity_at_least n when n <= arity -> (
              match acc with
              | None -> Some x
              | Some prev ->
                  if n > int_of_sig_arity prev.arity then Some x else acc)
          | Arity_at_least _ | Arity_exact _ -> acc)
        sigs None

let lookup_builtin_signature (db : builtin_signature_database)
    (func_name : string) (arity : int) : (Function_id.t * Signature.t) option =
  match BuiltinMap.find_opt func_name db with
  | Some sigs when not (SignatureSet.is_empty sigs) ->
    (* NOTE: We do not use [find_by_arity sigs arity] because for built-ins we require an exact
       arity match. *)
      let filtered_sigs =
        SignatureSet.filter (fun x -> equal_sig_arity x.arity (Arity_exact arity)) sigs
      in
      let signatures_card = SignatureSet.cardinal filtered_sigs in
      if Int.equal signatures_card 1 then
        Some
          ( Function_id.of_string_and_tok func_name
              (Tok.unsafe_fake_tok func_name),
            (SignatureSet.choose filtered_sigs).sig_ )
      else None
  | _ -> None

let show_name (name_opt : IL.name option) =
  match name_opt with
  | Some name -> IL.show_ident name.ident
  | None -> ""

let empty_signature_database () : signature_database =
  { signatures = FunctionMap.empty }

let lookup_signature (db : signature_database) (name : Function_id.t)
    (arity : int) : Signature.t option =
  match FunctionMap.find_opt name db.signatures with
  | Some sigs when not (SignatureSet.is_empty sigs) ->
      find_by_arity sigs arity |> Option.map (fun (ext : extended_sig) -> ext.sig_)
  | _ -> None

let lookup_definition (db : signature_database) (name : Function_id.t)
    (arity : int) : (Function_id.t * Signature.t) option =
  lookup_signature db name arity |> Option.map (fun sig_ -> (name, sig_))

let lookup_all_signatures (db : signature_database) (name : Function_id.t)
    : extended_sig list =
  match FunctionMap.find_opt name db.signatures with
  | Some sigs -> SignatureSet.elements sigs
  | None -> []

let add_signature (db : signature_database) (name : Function_id.t)
    (signature : extended_sig) : signature_database =
  let signatures =
    FunctionMap.update name
      (fun existing_sigs ->
        match existing_sigs with
        | Some sigs -> Some (SignatureSet.add signature sigs)
        | None -> Some (SignatureSet.singleton signature))
      db.signatures
  in
  { signatures }

(* Unlike add_signature, discards any prior entries for [name]. *)
let replace_signature (db : signature_database) (name : Function_id.t)
    (signature : extended_sig) : signature_database =
  let signatures =
    FunctionMap.add name (SignatureSet.singleton signature) db.signatures
  in
  { signatures }

let show_func_key (key : func_key) : string =
  Function_id.show_debug key

let show_signature_database (db : signature_database) : string =
  FunctionMap.fold
    (fun key signature acc ->
      let name_str = show_func_key key in
      let sig_str =
        String.concat ",\n---\n"
        @@ List.map show_extended_sig (SignatureSet.elements signature)
      in
      acc ^ Printf.sprintf "%s: %s\n" name_str sig_str)
    db.signatures ""
