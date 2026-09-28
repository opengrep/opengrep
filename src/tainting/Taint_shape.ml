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

open Common
module Log = Log_tainting.Log
module G = AST_generic
module R = Rule
module T = Taint
module Taints = T.Taint_set
open Shape_and_sig.Shape
module Fields = Shape_and_sig.Fields
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature

(*********************************************************)
(* Helpers *)
(*********************************************************)

(* UNSAFE: Violates INVARIANT(cell), see 'internal_UNSAFE_find_offset_in_obj' *)
let cell_none_bot = Cell (`None, Bot)

(* UNSAFE: Violates INVARIANT(cell), see 'internal_UNSAFE_find_offset_in_obj' *)
let edge_none_bot : edge = { xtaint = `None; target = Leaf Bot }

(* Temporarily breaks INVARIANT(cell) by initializing a field with the shape
 * 'cell<0>(_|_)', but right away the field should be either tainted or cleaned.
 * The caller must restore the invariant. *)
let internal_UNSAFE_find_offset_in_obj ~none o obj =
  match Fields.find_opt o obj with
  | Some _ -> (o, obj)
  | None ->
      let num_fields = Fields.cardinal obj in
      if num_fields <= Limits_semgrep.taint_MAX_OBJ_FIELDS then
        let obj = Fields.add o none obj in
        (o, obj)
      else (
        Log.warn (fun m ->
            m "Already tracking too many fields, will not track %s"
              (T.show_offset o));
        (Oany, obj))

let debug_offset offset =
  match offset with
  | [] -> "<NO OFFSET>"
  | _ :: _ -> offset |> List_.map T.show_offset |> String.concat ""

(*********************************************************)
(* Misc *)
(*********************************************************)

let shape_has_relevant_content = shape_has_relevant_content

let taints_and_shape_are_relevant taints shape =
  (* An assignment whose RHS carries neither taints nor tainted shape
   * content triggers the [Lval_env.clean] side-effect in the transfer
   * function — i.e. the RHS acts as a sanitizer for the LHS's prior
   * taint. Before literal record/dict construction could produce
   * [Obj {field = Cell (`Clean, Bot); …}] shapes (all fields known
   * clean), a non-[Bot] shape was guaranteed to carry taint by the
   * invariant, and a [shape ≠ Bot] check was enough. With the clean-
   * cell preservation in [add_field_to_obj_check_invariant], we must
   * now walk the shape and confirm it contains at least one [`Tainted]
   * cell (or an [Arg] polymorphic taint) before treating the RHS as
   * "relevant". *)
  (not (Taints.is_empty taints)) || shape_has_relevant_content shape

(* TODO: This should fix shapes too. *)
(* Deep-struct languages over large codebases explode poly-taint width
   at the default bound; lower them (none has a test needing more). See
   [taint_MAX_POLY_OFFSET]. *)
let max_poly_offset (lang : Lang.t) : int =
  match lang with
  (* Cap 2 measured on grafana (2026-07, 13 go rules): converges at ~4x
     the cap-1 scan time (488s vs ~120s) and removes 22 of 806 findings
     by source/sink identity — all field-confusion FPs (at cap 1 every
     field of a depth-1-truncated struct aliases, so e.g. an int
     threshold "flows" into a filepath.Join sink). No sink location
     gains or loses coverage. Raising the cap is a 4x-cost /
     trace-precision tradeoff; cap 4 explodes (composition width). *)
  | Lang.Go -> Limits_semgrep.taint_MAX_POLY_OFFSET_FLAT
  | _ -> Limits_semgrep.taint_MAX_POLY_OFFSET

(* A repeated segment is a composition cycle only when it is the same
   OCCURRENCE — [x = x.getX ()] re-composes the same field token each
   round.  Same-named distinct fields ([x.data.data]) are legitimate
   chains, and field idents carry default sids, so [T.equal_offset]
   alone cannot tell the two apart: compare source positions as well.
   Tokenless segments (ints, strings, slices) have no occurrence to
   compare; identical repetition still counts as a cycle there, and
   the cap bounds composition regardless. *)
let same_offset_occurrence (o1 : T.offset) (o2 : T.offset) : bool =
  T.equal_offset o1 o2
  &&
  match (o1, o2) with
  | T.Ofld n1, T.Ofld n2 -> (
      match
        (Tok.loc_of_tok (snd n1.ident), Tok.loc_of_tok (snd n2.ident))
      with
      | Ok l1, Ok l2 -> Pos.equal l1.Tok.pos l2.Tok.pos
      | _ -> true)
  | _ -> true

(* Append [offset]'s segments to [base] one at a time, under the same two
   bounds as [add_offset_to_lval] below: stop at the first segment already
   present as the same occurrence (the cycle guard, cf. [x = x.getX ()])
   and at [max_poly_offset] segments total.  [base] is respected as-is;
   only extensions are guarded. *)
(* [max] overrides [max_poly_offset lang]: a recursive call composes under
   the flat bound, see [Taint_rule_inst.recursive]. *)
let compose_offset ?(max : int option) ~(lang : Lang.t)
    (base : T.offset list) (offset : T.offset list) : T.offset list =
  let cap = Option.value max ~default:(max_poly_offset lang) in
  let rec go (rev_acc : T.offset list) (n : int) (os : T.offset list) =
    match os with
    | [] -> List.rev rev_acc
    | o :: rest ->
        if n >= cap then (
          (* Dropping segments loses field-sensitivity past the cap: the
             truncated taint over-approximates every sibling under the
             kept prefix (pinned by test_poly_offset_cap_python). Debug,
             not warn: this composes in the fixpoint's hottest loops. *)
          Log.debug (fun m ->
              m "compose_offset: dropping %d segment(s) past poly-offset cap %d: base=%s offset=%s"
                (List.length os) cap (debug_offset base) (debug_offset offset));
          List.rev rev_acc)
        else if List.exists (same_offset_occurrence o) rev_acc then
          List.rev rev_acc
        else go (o :: rev_acc) (n + 1) rest
  in
  go (List.rev base) (List.length base) offset

(* A field offset of function type is a method, unless name resolution marked
 * it as a data field of the receiver's type, which holds a function. *)
let is_method_offset (o : T.offset) : bool =
  match o with
  | T.Ofld n -> (
      match !(n.id_info.id_type) with
      | Some { t = G.TyFun _; _ } ->
          not (IdFlags.is_data_field !(n.id_info.id_flags))
      | _ -> false)
  | T.Oint _
  | T.Ostr _
  | T.Oslice _
  | T.Oany ->
      false

let fix_poly_taint_with_offset ?(max : int option) ~(lang : Lang.t)
    ~(traces : T.kept_traces) offset
    taints =
  let type_of_offset o =
    match o with
    | T.Ofld n -> !(n.id_info.id_type)
    | _ -> None
  in
  let add_offset_to_lval o ({ offset; _ } as orig_lval : T.lval) =
    let extended_lval = { orig_lval with offset = orig_lval.offset @ [ o ] } in
    if
      (* If the offset we are trying to take is already in the
           list of offsets, don't append it! This is so we don't
           never-endingly loop the dataflow and make it think the
           Arg taint is never-endingly changing.

           For instance, this code example would previously loop,
           if `x` started with an `Arg` taint:
           while (true) { x = x.getX(); }
      *)
      (* For perf reasons we don't allow offsets to get too long.
       * Otherwise in a long chain of function calls where each
       * function adds some offset, we could end up a very large
       * amount of polymorphic taint.
       * This actually happened with rule
       * semgrep.perf.rules.express-fs-filename from the Pro
       * benchmarks, and file
       * WebGoat/src/main/resources/webgoat/static/js/libs/ace.js.
       *
       * TODO: This is way less likely to happen if we had better
       *   type info and we used it to remove taint, e.g. if Boolean
       *   and integer expressions didn't propagate taint. *)
      (* Both bounds live in [compose_offset]; extension happened iff the
         composed offset is longer. *)
      List.compare_lengths (compose_offset ?max ~lang offset [ o ]) offset > 0
    then extended_lval
    else (
      (* Debug, not warn: fires per capped lval in the fixpoint's hottest
         loop — tens of millions of times on offset-heavy scans. *)
      Log.debug (fun m ->
          m "Taint_lval_env.fix_poly_taint_with_offset: %s is too long"
            (T.show_lval extended_lval));
      orig_lval)
  in
  offset
  |> List.fold_left
       (fun taints o ->
         match (type_of_offset o, o) with
         | Some { t = TyFun _; _ }, _ when is_method_offset o ->
            (* We have an l-value like `o.f` where `f` has a function type,
             * so it's a method call, we return nothing here. We cannot just
             * return `xtaint`, which is the taint of `o` in the environment;
             * whether that taint propagates or not is determined in
             * 'check_tainted_instr'/'Call'. Otherwise, if `o` had taint var
             * 'o@i', the call `o.getX()` would have taints '{o@i, o@i.x}'
             * when it should only have taints '{o@i.x}'. *)
            Taints.empty
         | __any__, ((Ofld _ | Ostr _ | Oint _ | Oslice _ | Oany) as o) ->
            (* Not a method call (to the best of our knowledge) or
             * an unresolved Java `getX` method. *)
             taints
             (* [f] rewrites the taint identity (Map key), so must re-key. *)
             |> Taints.map_taint ~traces (fun (taint : T.taint) ->
                    match taint.orig with
                    | Var lval ->
                        let lval' = add_offset_to_lval o lval in
                        { taint with orig = Var lval' }
                    | Shape_var lval ->
                        let lval' = add_offset_to_lval o lval in
                        { taint with orig = Shape_var lval' }
                    | Src _
                    | Control ->
                        taint))
       taints

(* The taints of an access path cut short: each polymorphic 'Var l' with
 * 'Shape_var l'. 'Var l' stands for the taints of the actual's cell at [l]
 * and 'Shape_var l' for every taint reachable from the actual's value at
 * [l] ('Sig_inst.instantiate_taint_var'), so together they cover 'Var l.w'
 * for every extension [w]. *)
let cut_poly_taints ~(traces : T.kept_traces) (taints : Taints.t) : Taints.t =
  match
    Taints.fold
      (fun (guarded : T.guarded_taint) (shape_vars : T.guarded_taint list) ->
        match guarded.taint.orig with
        | Var lval ->
            { guarded with taint = { guarded.taint with orig = Shape_var lval } }
            :: shape_vars
        | Src _
        | Shape_var _
        | Control ->
            shape_vars)
      taints []
  with
  | [] -> taints
  | shape_vars -> Taints.union ~traces taints (Taints.of_list ~traces shape_vars)

(* A read of [offset] on a parameter's shape 'Arg (arg, base_offsets)', whose
 * value carries [taints]: the polymorphic taints extended by [offset], under
 * the shape extended the same way. 'None' when [offset] is a method call. *)
let find_in_arg ?max ~lang ~(traces : T.kept_traces) ~taints offset arg
    base_offsets =
  (* Mirror the method-vs-field discriminator from
   * [fix_poly_taint_with_offset]: when any offset segment has a
   * function type ([TyFun]), this is a method call on an [Arg]-shaped
   * value (e.g. [arr.begin()] in C++). Extending the Arg shape through
   * the method would make the receiver look like a callback and fire
   * false HOF dispatch. Fall through to the poly-taint path instead. *)
  let offset_is_method = List.exists is_method_offset offset in
  if offset_is_method then None
  else
    (* Extend each alternative path with the additional [offset],
     * via [compose_offset] (cycle guard + [taint_MAX_POLY_OFFSET]
     * cap). The poly-taints
     * below are bounded by [fix_poly_taint_with_offset]; without the
     * same bound here the Arg shape's offset grows with structure depth
     * (e.g. a deep [x = x.f] forwarding chain). Since [Shape.equal] and
     * [Shape.compare] traverse the whole offset list, an unbounded
     * offset makes each shape comparison O(depth) and degrades
     * performance on such chains. *)
    let extended =
      base_offsets
      |> List.map (fun base_off -> compose_offset ?max ~lang base_off offset)
      |> List.sort_uniq (List.compare T.compare_offset)
    in
    let taints = fix_poly_taint_with_offset ?max ~lang ~traces offset taints in
    Some (Cell (Xtaint.of_taints taints, Arg (arg, extended)))

(*********************************************************)
(* Objects and closure sets *)
(*********************************************************)

(* An object with [fields]: 'Bot' when it has none, a 'Graph' when a field
 * holds one (INVARIANT(graph).1). *)
let obj_or_bot ~(traces : T.kept_traces) ~(sites : Shape_and_sig.Sites.t)
    ~(summary : bool) (fields : obj) : shape =
  if Fields.is_empty fields then Bot
  else canonical ~traces (Obj { sites; summary; fields })

let written_obj ~(write : T.call_loc) ~(depth : int) : shape =
  Obj
    {
      sites = Shape_and_sig.Sites.singleton (Shape_and_sig.Written_at (write, depth));
      summary = false;
      fields = Fields.empty;
    }

(* A tree object or closure set as a node whose targets are its cells'
 * trees. *)
let node_of_tree (shape : shape) : node option =
  let edge_of (Cell (xtaint, shape) : cell) : edge = { xtaint; target = Leaf shape } in
  match shape with
  | Obj { sites; summary; fields } ->
      Some (Object { sites; summary; edges = Fields.map edge_of fields })
  | Fun (c, cs) ->
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
      Some (Closures (graph_closure c, List.map graph_closure cs))
  | Bot
  | Graph _
  | Arg _ ->
      None

(* [nodes] with INVARIANT(cell) restored after edges were dropped: an object
 * with no edge left is 'Bot' ('obj_or_bot'), and a cell with no taint whose
 * value is 'Bot' is dropped (INVARIANT(cell).1), up to [root]. A node keeps
 * content when it is a closure set or has an edge with a taint, a 'Clean'
 * edge, an edge to a tree other than 'Bot', or an edge to a node that keeps
 * content: the least solution of these equations. *)
let restore_cell_invariant (nodes : node array) (root : int) : node array * target =
  let keeps_content =
    Shape_and_sig.least_fixpoint ~leq_join:( || )
      ~local:(fun (i : int) ->
        match nodes.(i) with
        | Closures _ -> true
        | Object { edges; _ } ->
            Fields.exists
              (fun _o (edge : edge) ->
                match (edge.xtaint, edge.target) with
                | (`Clean | `Tainted _), _ -> true
                | `None, Leaf Bot -> false
                | `None, Leaf (Obj _ | Graph _ | Arg _ | Fun _) -> true
                | `None, Node _ -> false)
              edges)
      (Array.map successors nodes)
  in
  let restore (edge : edge) : edge option =
    match (edge.xtaint, edge.target) with
    | `None, Node j when not keeps_content.(j) -> None
    | (`Clean | `Tainted _), Node j when not keeps_content.(j) ->
        Some { edge with target = Leaf Bot }
    | _, (Node _ | Leaf _) -> Some edge
  in
  let nodes =
    Array.map
      (fun (node : node) ->
        match node with
        | Object ({ edges; _ } as node) ->
            Object { node with edges = Fields.filter_map (fun _o -> restore) edges }
        | Closures _ ->
            map_edges
              (fun (edge : edge) ->
                match restore edge with
                | Some edge -> edge
                | None -> { edge with target = Leaf Bot })
              node)
      nodes
  in
  (nodes, if keeps_content.(root) then Node root else Leaf Bot)

(*********************************************************)
(* Unification (merging shapes) *)
(*********************************************************)

(* [cell] with every 'Clean' leaf, at any depth, given the taint a read of
 * the leaf gives on the other path: [leaf], the taint of that path's whole
 * value, extended by the leaf's [offset] from the joined cell
 * ('fix_poly_taint_with_offset', as 'option_of_find_result' extends a
 * carried taint; a method offset gives none). The leaf is dropped when a
 * read of it would carry exactly [leaf] down to it anyway ([carry] is what
 * the read carries into [cell]: the taint of the nearest enclosing cell that
 * has one, 'find_in_cell_w_carry'), else replaced by a cell holding that
 * taint. Every other cell keeps its own taint. 'None' when nothing is left
 * of [cell] (INVARIANT(cell)); [cell] itself when it has no 'Clean' leaf, in
 * one pass, so a join that changes nothing allocates nothing. [extended] is
 * [leaf] extended by the offsets above [cell], [offset] those below; when
 * [truncated] the offsets below are not added ('join_graphs'). *)
let rec replace_clean_leaves ~lang ~(traces : T.kept_traces) ~offset ~carry ~leaf
    ~extended ~(truncated : bool) (Cell (xtaint, shape) as cell) =
  match (xtaint, shape) with
  | `Clean, _ ->
      if Taints.equal leaf carry then None
      else
        let taints = fix_poly_taint_with_offset ~lang ~traces offset extended in
        if Taints.is_empty taints then None
        else Some (Cell (`Tainted taints, Bot))
  | _, Obj ({ fields = obj; _ } as node) -> (
      let carry =
        match xtaint with
        | `Tainted taints -> taints
        | `None
        | `Clean ->
            carry
      in
      let obj' =
        Fields.fold
          (fun o c acc ->
            match
              replace_clean_leaves ~lang ~traces
                ~offset:(if truncated then offset else offset @ [ o ])
                ~carry ~leaf ~extended ~truncated c
            with
            | Some c' when phys_equal c' c -> acc
            | Some c' -> Fields.add o c' acc
            | None -> Fields.remove o acc)
          obj obj
      in
      if phys_equal obj' obj then Some cell
      else
        match (xtaint, Fields.is_empty obj') with
        | (`None | `Clean), true -> None
        | `Tainted _, true -> Some (Cell (xtaint, Bot))
        | _, false -> Some (Cell (xtaint, Obj { node with fields = obj' })))
  | _, (Bot | Graph _ | Arg _ | Fun _) -> Some cell

let cell_of_join (xtaint : Xtaint.t) (shape : shape) : cell =
  match (xtaint, shape) with
  (* Restore INVARIANT(cell).2: 'Xtaint.union' gives 'Clean ∪ None = Clean'
   * while 'unify_shape' gives 'Bot ∪ shape = shape', so unifying
   * 'Cell(Clean, Bot)' with 'Cell(None, Obj _)' would produce
   * 'Cell(Clean, Obj _)'. The 'Clean' claim only held on one side, and a
   * join must not hide the taint recorded under the other side's shape
   * ('find_in_cell_w_carry' stops at a 'Clean' cell). *)
  | `Clean, (Obj _ | Graph _ | Arg _ | Fun _) -> Cell (`None, shape)
  | ( (`Clean | `None | `Tainted _),
      (Bot | Obj _ | Graph _ | Arg _ | Fun _) ) ->
      Cell (xtaint, shape)

(* 'cell_of_join' on an edge. *)
let edge_of_join (xtaint : Xtaint.t) (target : target) : edge =
  match target with
  | Node _ -> (
      match xtaint with
      | `Clean -> { xtaint = `None; target }
      | `None
      | `Tainted _ ->
          { xtaint; target })
  | Leaf shape ->
      let (Cell (xtaint, shape)) = cell_of_join xtaint shape in
      { xtaint; target = Leaf shape }

(* The taint of a cell as the other side of a join reads it for a field it
 * does not track ('whole' in 'taint_untracked_fields'). *)
let whole (xtaint : Xtaint.t) : Taints.t option =
  match xtaint with
  | `Tainted taints -> Some taints
  | `None
  | `Clean ->
      None

(* A state of the product construction (Rabin and Scott 1959) of two values
 * of which one at least is a graph: a pair of positions of the operands, a
 * node, 'Bot' or an 'Arg' (a read through a parameter gives an 'Arg' at
 * every offset, so such a position can be met again along a cycle), with
 * the taints of the cells that reach them ('whole') and whether the fields
 * of each side that the other does not track take what a read of them gives
 * on the other side ('taint_untracked_fields'); a node of an operand kept
 * as it is; or a node whose 'Clean' leaves take [leaf] extended to them
 * ('replace_clean_leaves'), with the taint [carry] a read carries into it.
 * [truncated] is set below the first node of a cycle on the path from the
 * root ('join_graphs'). A pair with a tree object or closure set is not a
 * state: its tree is a part of one operand, so no path meets it twice. *)
type state =
  | Pair of {
      left : target;
      left_taints : Taints.t option;
      right : target;
      right_taints : Taints.t option;
      untracked_fields : bool;
      truncated : bool;
    }
  | Kept of int
  | Replaced of {
      node : int;
      leaf : Taints.t;
      carry : Taints.t;
      extended : Taints.t;
      truncated : bool;
    }

(* On the positions of states: 'compare_shape' is exact on 'Bot' and
 * 'Arg', which have no sites. *)
let compare_position (target1 : target) (target2 : target) : int =
  match (target1, target2) with
  | Node i, Node j -> Int.compare i j
  | Leaf shape1, Leaf shape2 -> compare_shape shape1 shape2
  | Node _, Leaf _ -> -1
  | Leaf _, Node _ -> 1

let is_position (target : target) : bool =
  match target with
  | Node _
  | Leaf (Bot | Arg _) ->
      true
  | Leaf (Obj _ | Graph _ | Fun _) -> false

module State = struct
  type t = state

  let equal (state1 : t) (state2 : t) : bool =
    let equal_position (target1 : target) (target2 : target) : bool =
      Int.equal (compare_position target1 target2) 0
    in
    match (state1, state2) with
    | Pair pair1, Pair pair2 ->
        equal_position pair1.left pair2.left
        && equal_position pair1.right pair2.right
        && Bool.equal pair1.untracked_fields pair2.untracked_fields
        && Bool.equal pair1.truncated pair2.truncated
        && Option.equal Taints.equal_with_guards pair1.left_taints pair2.left_taints
        && Option.equal Taints.equal_with_guards pair1.right_taints pair2.right_taints
    | Kept i1, Kept i2 -> Int.equal i1 i2
    | Replaced replaced1, Replaced replaced2 ->
        Int.equal replaced1.node replaced2.node
        && Bool.equal replaced1.truncated replaced2.truncated
        && Taints.equal_with_guards replaced1.leaf replaced2.leaf
        && Taints.equal_with_guards replaced1.carry replaced2.carry
        && Taints.equal_with_guards replaced1.extended replaced2.extended
    | Pair _, (Kept _ | Replaced _)
    | Kept _, (Pair _ | Replaced _)
    | Replaced _, (Pair _ | Kept _) ->
        false

  (* The positions and the sizes of the taint sets, which equal states
   * share. *)
  let hash (state : t) : int =
    match state with
    | Pair { left; right; left_taints; right_taints; _ } ->
        (* The fields that 'T.equal_formal' compares. *)
        let hash_formal (formal : T.formal) : int =
          match formal with
          | Param { name; index } -> Hashtbl.hash (0, index, name)
          | Receiver -> 1
          | Captured name -> Hashtbl.hash (2, fst name.ident)
          | Result { loc; _ } -> Hashtbl.hash (3, loc)
        in
        let hash_position (target : target) : int =
          match target with
          | Node i -> i
          | Leaf (Arg (formal, offsets)) ->
              Hashtbl.hash (hash_formal formal, List.map List.length offsets)
          | Leaf (Bot | Obj _ | Graph _ | Fun _) -> -1
        in
        let size (taints : Taints.t option) : int =
          match taints with
          | Some taints -> Taints.cardinal taints
          | None -> -1
        in
        Hashtbl.hash
          (0, hash_position left, hash_position right, size left_taints, size right_taints)
    | Kept i -> Hashtbl.hash (1, i)
    | Replaced { node; leaf; carry; extended; _ } ->
        Hashtbl.hash
          (2, node, Taints.cardinal leaf, Taints.cardinal carry, Taints.cardinal extended)
end

module State_tbl = Hashtbl.Make (State)

(* One product construction: [operands] holds the nodes of both operands,
 * [nodes] the result's, [states] the index of each state's node. Both
 * tables are created by the join that runs the construction and owned by
 * the domain that runs it. *)
type product = {
  lang : Lang.t;
  traces : T.kept_traces;
  operands : node array;
  clean_leaves : bool array Lazy.t;
      (** For each node of [operands], whether a 'Clean' leaf is reachable
          from it through objects, where 'replace_clean_leaves' changes
          something. *)
  on_cycle : bool array Lazy.t;
      (** For each node of [operands], whether it lies on a cycle: its
          strongly connected component has several nodes, or an edge from
          the node to itself. *)
  nodes : node Dynarray.t;
  states : int State_tbl.t;
}

(* Whether the tree [shape] holds a 'Clean' cell that 'replace_clean_leaves'
 * reaches. *)
let rec has_clean_leaves (shape : shape) : bool =
  match shape with
  | Obj { fields; _ } ->
      Fields.exists
        (fun _o (Cell (xtaint, shape) : cell) ->
          match xtaint with
          | `Clean -> true
          | `None
          | `Tainted _ ->
              has_clean_leaves shape)
        fields
  | Bot
  | Graph _
  | Arg _
  | Fun _ ->
      false

let clean_leaves_of_operands (operands : node array) : bool array =
  Shape_and_sig.least_fixpoint ~leq_join:( || )
    ~local:(fun (i : int) ->
      match operands.(i) with
      | Object { edges; _ } ->
          Fields.exists
            (fun _o (edge : edge) ->
              match (edge.xtaint, edge.target) with
              | `Clean, _ -> true
              | (`None | `Tainted _), Leaf shape -> has_clean_leaves shape
              | (`None | `Tainted _), Node _ -> false)
            edges
      | Closures _ -> false)
    (Array.map
       (fun (node : node) ->
         match node with
         | Object _ -> successors node
         | Closures _ -> [])
       operands)

let empty_object : node =
  Object { sites = Shape_and_sig.Sites.empty; summary = false; edges = Fields.empty }

(* The nodes of two operands in one array, the second shifted after the
 * first unless both are values of one array, and the operands' roots. *)
let operands (shape1 : shape) (shape2 : shape) : node array * target * target =
  match (shape1, shape2) with
  | Graph g1, Graph g2 when phys_equal g1.nodes g2.nodes ->
      (g1.nodes, Node g1.root, Node g2.root)
  | Graph g1, Graph g2 ->
      let shift = Array.length g1.nodes in
      ( Array.append g1.nodes
          (Array.map
             (map_targets (fun (target : target) ->
                  match target with
                  | Node i -> Node (i + shift)
                  | Leaf _ -> target))
             g2.nodes),
        Node g1.root,
        Node (g2.root + shift) )
  | Graph g, _ -> (g.nodes, Node g.root, Leaf shape2)
  | _, Graph g -> (g.nodes, Leaf shape1, Node g.root)
  | (Bot | Obj _ | Arg _ | Fun _), (Bot | Obj _ | Arg _ | Fun _) ->
      ([||], Leaf shape1, Leaf shape2)

let node_of_state (product : product) (states : state list) (state : state) :
    int * state list =
  match State_tbl.find_opt product.states state with
  | Some index -> (index, states)
  | None ->
      let index = Dynarray.length product.nodes in
      Dynarray.add_last product.nodes empty_object;
      State_tbl.add product.states state index;
      (index, state :: states)

let keep (product : product) (states : state list) (edge : edge) : edge * state list =
  match edge.target with
  | Node i ->
      let index, states = node_of_state product states (Kept i) in
      ({ edge with target = Node index }, states)
  | Leaf _ -> (edge, states)

let keep_closure (product : product) (states : state list) (closure : graph_closure) :
    graph_closure * state list =
  let env, states =
    List.fold_left
      (fun ((env, states) : (IL.name * graph_env_entry) list * state list)
           ((x, entry) as binding : IL.name * graph_env_entry) ->
        match entry with
        | Val edge ->
            let edge, states = keep product states edge in
            ((x, (Val edge : graph_env_entry)) :: env, states)
        | Ref _ -> (binding :: env, states))
      ([], states) closure.env
  in
  ({ closure with env = List.rev env }, states)

let keep_node (product : product) (states : state list) (node : node) : node * state list =
  match node with
  | Object ({ edges; _ } as node) ->
      let edges, states =
        Fields.fold
          (fun (o : T.offset) (edge : edge) ((edges, states) : edge Fields.t * state list) ->
            let edge, states = keep product states edge in
            (Fields.add o edge edges, states))
          edges (Fields.empty, states)
      in
      (Object { node with edges }, states)
  | Closures (c, cs) ->
      let c, states = keep_closure product states c in
      let cs, states =
        List.fold_left
          (fun ((cs, states) : graph_closure list * state list) (closure : graph_closure) ->
            let closure, states = keep_closure product states closure in
            (closure :: cs, states))
          ([], states) cs
      in
      (Closures (c, List.rev cs), states)

let position_node (product : product) (target : target) : node option =
  match target with
  | Node i -> Some product.operands.(i)
  | Leaf shape -> node_of_tree shape

let rec unify_cell_in ~lang ~(traces : T.kept_traces) cell1 cell2 =
  if phys_equal cell1 cell2 then cell1
  else
  let (Cell (xtaint1, shape1)) = cell1 in
  let (Cell (xtaint2, shape2)) = cell2 in
  (* TODO: Apply 'Flag_semgrep.max_taint_set_size' here too ? *)
  let xtaint = Xtaint.union ~traces xtaint1 xtaint2 in
  let carry = Xtaint.to_taints xtaint in
  let shape =
    match (shape1, shape2) with
    (* A value joined with an untainted 'Bot' or 'Arg': every field reads
     * what it holds on the other side, so the value is the join, as
     * 'taint_untracked_fields' keeps it. *)
    | Graph _, (Bot | Arg _) when Option.is_none (whole xtaint2) -> shape1
    | (Bot | Arg _), Graph _ when Option.is_none (whole xtaint1) -> shape2
    | Graph _, _
    | _, Graph _ ->
        join_graphs ~lang ~traces shape1 shape2 (fun (product : product) ->
            join_targets product [] ~untracked_fields:true ~truncated:false
              ~left_taints:(whole xtaint1) ~right_taints:(whole xtaint2))
    | (Bot | Obj _ | Arg _ | Fun _), (Bot | Obj _ | Arg _ | Fun _) ->
        unify_shape_in ~lang ~traces
          ~process1:(fun ~other shape ->
            taint_untracked_fields ~lang ~traces ~carry ~other_xtaint:xtaint2
              ~other shape)
          ~process2:(fun ~other shape ->
            taint_untracked_fields ~lang ~traces ~carry ~other_xtaint:xtaint1
              ~other shape)
          shape1 shape2
  in
  if phys_equal xtaint xtaint1 && phys_equal shape shape1 then cell1
  else if phys_equal xtaint xtaint2 && phys_equal shape shape2 then cell2
  else cell_of_join xtaint shape

(* [shape] as it must be seen at a join whose other side is [other].
 *
 * A field of [shape] that [other] does not track is, on the path of [other],
 * what a read of it there gives ('find_in_cell_w_carry'): the 'Oany' entry
 * of an object, the field of a parameter, or else the taint of the whole
 * value. The first two are joined into the field. For the third only the
 * 'Clean' leaves of the field take the taint ('replace_clean_leaves'), and
 * only when a read of the leaf would not carry it there by itself. A cell
 * of the field with a taint of its own keeps just that, although a read of
 * it on the other path gives the whole value's taint too: joining that into
 * every cell of the field would store the same set once per cell, at every
 * depth, for every later walk of the shape to pay for. Otherwise the field
 * would survive the join as it is, because 'unify_shape' keeps the object
 * ('Bot ∪ Obj = Obj', 'Arg ∪ Obj = Obj') and 'unify_obj' keeps a field that
 * is on one side only, and a 'Clean' field would hide the taint of the whole
 * value, e.g. 'q' in
 *
 *     p, q = s.split("?", 1) if c else (s, "")
 *
 * A literal records its untainted fields as 'Clean', and so does a
 * sanitizer. A field that [other] tracks is left to 'unify_obj'. *)
and taint_untracked_fields ~lang ~(traces : T.kept_traces) ~carry
    ~other_xtaint:(xtaint : Xtaint.t) ~other:(other_shape : shape) shape =
  match shape with
  | Obj ({ fields = obj; _ } as node) ->
      let whole = whole xtaint in
      let read_on_other o =
        match (other_shape, xtaint) with
        | Obj { fields = other_obj; _ }, _ when Fields.mem o other_obj -> `Tracked
        | Obj { fields = other_obj; _ }, _ -> (
            match Fields.find_opt T.Oany other_obj with
            | Some any_cell -> `Cell any_cell
            | None -> `Whole)
        | Arg (arg, base_offsets), `Tainted taints -> (
            match find_in_arg ~lang ~traces ~taints [ o ] arg base_offsets with
            | Some cell -> `Cell cell
            | None -> `Whole)
        | Arg _, (`None | `Clean)
        | (Bot | Graph _ | Fun _), _ ->
            `Whole
      in
      let obj' =
        Fields.fold
          (fun o field acc ->
            match
              match (read_on_other o, whole) with
              | `Tracked, _
              | `Whole, None ->
                  Some field
              | `Cell cell, _ -> Some (unify_cell_in ~lang ~traces cell field)
              | `Whole, Some leaf ->
                  replace_clean_leaves ~lang ~traces ~offset:[ o ] ~carry ~leaf
                    ~extended:leaf ~truncated:false field
            with
            | Some field' when phys_equal field' field -> acc
            | Some field' -> Fields.add o field' acc
            | None -> Fields.remove o acc)
          obj obj
      in
      if phys_equal obj' obj then shape
      else obj_or_bot ~traces ~sites:node.sites ~summary:node.summary obj'
  | Bot
  | Graph _
  | Arg _
  | Fun _ ->
      shape

(* [process1] and [process2] give an object of one operand as it must appear
 * in the join with the [other] operand's shape ('taint_untracked_fields'). *)
and unify_shape_in ~lang ~(traces : T.kept_traces)
    ~(process1 : other:shape -> shape -> shape)
    ~(process2 : other:shape -> shape -> shape) shape1 shape2 =
  if phys_equal shape1 shape2 then shape1
  else
  match (shape1, shape2) with
  | Bot, shape ->
      (* 'Bot' acts like a do-not-care. *)
      process2 ~other:shape1 shape
  | shape, Bot -> process1 ~other:shape2 shape
  | Fun (c1, cs1), Fun (c2, cs2) ->
      let c, cs = unify_closure_sets ~lang ~traces (c1, cs1) (c2, cs2) in
      Fun (c, cs)
  | Arg (arg1, offsets1), Arg (arg2, offsets2) when T.equal_formal arg1 arg2 ->
      (* Same parameter — set-union the alternative offsets so a value
       * bound to different offsets across branches retains every
       * alternative. [sort_uniq] gives a canonical order and dedups. *)
      let merged =
        List.sort_uniq (List.compare T.compare_offset) (offsets1 @ offsets2)
      in
      Arg (arg1, merged)
  | Arg (arg1, _), Arg (arg2, _) ->
      (* Different parameters: the shape lattice would need constraint
       * solving to handle this precisely. See e.g.
       *
       *     def foo(a, b):
       *       tup = (a,)
       *       tup[0] = b
       *       return tup
       *
       * Here the signature of [foo] would ignore the shape of [b].
       * TODO: record and solve constraints. *)
      Log.warn (fun m ->
          m "Trying to unify two different arg shapes: %s ~ %s"
            (T.show_formal arg1) (T.show_formal arg2));
      shape1
  (* 'Arg' acts like a shape variable. *)
  | Arg _, (Obj _ as obj) -> process2 ~other:shape1 obj
  | (Obj _ as obj), Arg _ -> process1 ~other:shape2 obj
  | Arg _, (Fun _ as func)
  | (Fun _ as func), Arg _ ->
      func
  | Obj _, Obj _ -> (
      match (process1 ~other:shape2 shape1, process2 ~other:shape1 shape2) with
      | ( (Obj { sites = sites1; summary = summary1; fields = obj1 } as
           processed1),
          (Obj { sites = sites2; summary = summary2; fields = obj2 } as
           processed2) ) ->
          let fields = unify_obj_in ~lang ~traces obj1 obj2 in
          let subsumes (sites : Shape_and_sig.Sites.t) (summary : bool)
              (obj : obj) (other_sites : Shape_and_sig.Sites.t)
              (other_summary : bool) : bool =
            phys_equal fields obj
            && (summary || not other_summary)
            && (phys_equal sites other_sites
               || Shape_and_sig.Sites.subset other_sites sites)
          in
          if subsumes sites1 summary1 obj1 sites2 summary2 then processed1
          else if subsumes sites2 summary2 obj2 sites1 summary1 then
            processed2
          else
            Obj
              {
                sites = Shape_and_sig.Sites.union sites1 sites2;
                summary = summary1 || summary2;
                fields;
              }
      | Bot, shape -> shape
      | shape, _ -> shape)
  | Obj _, Fun _
  | Fun _, Obj _ ->
      (* This could be caused by bugs in Semgrep, or by an if-then-else in a
       * dynamic language like Python where the same variable has different types
       * in each branch, or by unsafe casts in C/C++ perhaps. *)
      Log.err (fun m ->
          m "Trying to unify incompatible shapes: %s ~ %s" (show_shape shape1)
            (show_shape shape2));
      (* Not sure what to do here, so we just pick one arbitrary shape. *)
      process1 ~other:shape2 shape1
  | Arg _, Graph _ -> shape2
  | Graph _, Arg _ -> shape1
  | Graph _, _
  | _, Graph _ ->
      join_graphs ~lang ~traces shape1 shape2 (fun (product : product) ->
          join_targets product [] ~untracked_fields:false ~truncated:false
            ~left_taints:None ~right_taints:None)

and unify_obj_in ~lang ~(traces : T.kept_traces) obj1 obj2 =
  (* THINK: Apply taint_MAX_OBJ_FIELDS limit ? *)
  if Fields.is_empty obj1 then obj2
  else
    Fields.fold
      (fun o cell2 obj ->
        Fields.update o
          (function
            | None -> Some cell2
            | Some cell1 -> Some (unify_cell_in ~lang ~traces cell1 cell2))
          obj)
      obj2 obj1

and unify_closure ~lang ~(traces : T.kept_traces) (c1 : closure)
    (c2 : closure) : closure =
  if phys_equal c1 c2 then c1
  else
    {
      c1 with
      sig_ =
        {
          c1.sig_ with
          Signature.effects =
            Effects.union ~traces c1.sig_.Signature.effects
              c2.sig_.Signature.effects;
        };
      env = unify_env ~lang ~traces c1.env c2.env;
    }

and unify_closure_sets ~lang ~(traces : T.kept_traces)
    ((c1, cs1) : closure * closure list)
    ((c2, cs2) : closure * closure list) : closure * closure list =
  match Function_id.compare c1.def c2.def with
  | 0 -> (unify_closure ~lang ~traces c1 c2, unify_closures ~lang ~traces cs1 cs2)
  | n when n < 0 -> (c1, unify_closures ~lang ~traces cs1 (c2 :: cs2))
  | _ -> (c2, unify_closures ~lang ~traces (c1 :: cs1) cs2)

and unify_closures ~lang ~(traces : T.kept_traces) (cs1 : closure list)
    (cs2 : closure list) :
    closure list =
  match (cs1, cs2) with
  | [], cs
  | cs, [] ->
      cs
  | c1 :: rest1, c2 :: rest2 ->
      let c, cs = unify_closure_sets ~lang ~traces (c1, rest1) (c2, rest2) in
      c :: cs

(* Both environments belong to the same code, so they bind the same
 * variables in the same order. *)
and unify_env ~lang ~(traces : T.kept_traces) (env1 : env) (env2 : env) : env =
  List.map2
    (fun ((x, entry1) as binding1) (_, entry2) ->
      match (entry1, entry2) with
      | Val cell1, Val cell2 -> (x, Val (unify_cell_in ~lang ~traces cell1 cell2))
      | Ref _, _
      | Val _, Ref _ ->
          binding1)
    env1 env2

(* The join of [shape1] and [shape2], one of them at least a graph, as the
 * product of the two (Rabin and Scott 1959) restricted to the states
 * reachable from the pair of roots: [root] gives the root's target and the
 * states it refers to, and each state's node is built once, so a state met
 * again, on the current path (a cycle) or on another (sharing), is an edge
 * to its node. The result is minimal, and an operand equal to it is
 * returned physically.
 *
 * A read through a parameter, and the taint of a whole value that a
 * 'Clean' leaf takes, are extended by the key of every edge from the root
 * to the first node of a cycle, that edge's key included, and truncated
 * below it: their polymorphic taints are those of the access path cut
 * there ('cut_poly_taints'). 'Var l' stands for the taints of the actual's
 * cell at [l] and 'Shape_var l' for every taint reachable from the
 * actual's value at [l] ('Sig_inst.instantiate_taint_var'), so together
 * they cover 'Var l.w' for every extension [w]. The truncated path is a
 * prefix of every path that the unrolling of the cycle would give to the
 * positions below, so every taint a read below the first node of the cycle
 * would carry is carried. The read is coarser than the unrolled read: the
 * taints of the siblings under [l] are included.
 *
 * Termination: a two-sided state is a pair of positions, one of each
 * operand in either order ('untracked_edge' puts the other operand's edge
 * first), with the taints of the edges that reach them and two flags, at
 * most eight times the product of the numbers of edges into the two
 * operands' positions, the roots counted as one edge each:
 * 8 (E1 + 1) (E2 + 1). A one-sided state (a pair with an 'Arg' or 'Bot'
 * side, or a 'Replaced' node) below the first node of a cycle carries the
 * argument, the taints and the extended taint as truncated there, and
 * otherwise only the taint of the nearest tainted edge above its node
 * ('Replaced.carry'), so the states in a cycle's component are at most the
 * edges into its nodes times the distinct truncated values at its entries;
 * above it, the paths from the root are acyclic. *)
and join_graphs ~lang ~(traces : T.kept_traces) (shape1 : shape) (shape2 : shape)
    (root : product -> target -> target -> target * state list) : shape =
  match (shape1, shape2) with
  | _ when phys_equal shape1 shape2 -> shape1
  | Graph _, Graph _ when equal_shape_with_guards shape1 shape2 -> shape1
  | _ ->
  let operands, target1, target2 = operands shape1 shape2 in
  let product =
    {
      lang;
      traces;
      operands;
      clean_leaves = lazy (clean_leaves_of_operands operands);
      on_cycle =
        lazy
          (let successors = Array.map successors operands in
           let count, component_of =
             Shape_and_sig.Adjacency_components.scc successors
           in
           let sizes = Array.make count 0 in
           Array.iteri
             (fun (i : int) (_ : int list) ->
               sizes.(component_of i) <- sizes.(component_of i) + 1)
             successors;
           Array.mapi
             (fun (i : int) (next : int list) ->
               sizes.(component_of i) > 1 || List.exists (Int.equal i) next)
             successors);
      nodes = Dynarray.create ();
      states = State_tbl.create 16;
    }
  in
  let target, roots = root product target1 target2 in
  (* [node_of_state] returns only the states it creates, so each state is
   * expanded once. *)
  let rec build (states : state list) : unit =
    List.iter (fun (state : state) -> build (expand product state)) states
  in
  build roots;
  let shape = minimise ~traces (Dynarray.to_array product.nodes) target in
  if equal_shape_with_guards shape shape1 then shape1
  else if equal_shape_with_guards shape shape2 then shape2
  else shape

(* Whether a position is a node on a cycle. *)
and on_cycle (product : product) (target : target) : bool =
  match target with
  | Node i -> (Lazy.force product.on_cycle).(i)
  | Leaf _ -> false

(* Builds the node of [state] and returns the states that it refers to. *)
and expand (product : product) (state : state) : state list =
  let index = State_tbl.find product.states state in
  let node, states =
    match state with
    | Pair { left; left_taints; right; right_taints; untracked_fields; truncated } -> (
        match
          pair_content product [] ~untracked_fields ~truncated ~left_taints ~right_taints
            left right
        with
        | Some node, states -> (node, states)
        | None, states -> (empty_object, states))
    | Kept i -> keep_node product [] product.operands.(i)
    | Replaced { node; leaf; carry; extended; truncated } -> (
        let extended, truncated =
          if (not truncated) && on_cycle product (Node node) then
            (cut_poly_taints ~traces:product.traces extended, true)
          else (extended, truncated)
        in
        match product.operands.(node) with
        | Object ({ edges; _ } as node) ->
            let edges, states =
              Fields.fold
                (fun (o : T.offset) (edge : edge)
                     ((edges, states) : edge Fields.t * state list) ->
                  let extended =
                    if truncated then Lazy.from_val extended
                    else
                      lazy
                        (fix_poly_taint_with_offset ~lang:product.lang
                           ~traces:product.traces [ o ] extended)
                  in
                  match replace product states ~truncated ~leaf ~carry ~extended edge with
                  | Some edge, states -> (Fields.add o edge edges, states)
                  | None, states -> (edges, states))
                edges (Fields.empty, [])
            in
            (Object { node with edges }, states)
        | Closures _ as node -> keep_node product [] node)
  in
  Dynarray.set product.nodes index node;
  states

(* 'unify_cell_in' on two edges. *)
and join_edges (product : product) (states : state list) ~(truncated : bool)
    (edge1 : edge) (edge2 : edge) : edge * state list =
  match (edge1.target, edge2.target) with
  | Leaf (Arg _), Leaf (Obj _)
  | Leaf (Obj _), Leaf (Arg _)
    when truncated ->
      (* The tree join would extend the 'Arg' by the object's keys. *)
      let target, states =
        join_targets product states ~untracked_fields:true ~truncated
          ~left_taints:(whole edge1.xtaint) ~right_taints:(whole edge2.xtaint)
          edge1.target edge2.target
      in
      (edge_of_join (Xtaint.union ~traces:product.traces edge1.xtaint edge2.xtaint) target, states)
  | Leaf shape1, Leaf shape2 ->
      let (Cell (xtaint, shape)) =
        unify_cell_in ~lang:product.lang ~traces:product.traces
          (Cell (edge1.xtaint, shape1))
          (Cell (edge2.xtaint, shape2))
      in
      ({ xtaint; target = Leaf shape }, states)
  | (Node _ | Leaf _), _ ->
      let target, states =
        join_targets product states ~untracked_fields:true ~truncated
          ~left_taints:(whole edge1.xtaint) ~right_taints:(whole edge2.xtaint)
          edge1.target edge2.target
      in
      (edge_of_join (Xtaint.union ~traces:product.traces edge1.xtaint edge2.xtaint) target, states)

(* 'unify_shape_in' on two targets, one at least a node. *)
and join_targets (product : product) (states : state list) ~untracked_fields
    ~(truncated : bool) ~left_taints ~right_taints (target1 : target)
    (target2 : target) : target * state list =
  match (target1, target2) with
  (* A node joined with itself: every field is on both sides, so the join
   * is the node, as 'unify_shape_in' returns a shape joined with itself. *)
  | Node i, Node j when Int.equal i j ->
      let index, states = node_of_state product states (Kept i) in
      (Node index, states)
  | _ when is_position target1 && is_position target2 ->
      let index, states =
        node_of_state product states
          (Pair
             {
               left = target1;
               left_taints;
               right = target2;
               right_taints;
               untracked_fields;
               truncated;
             })
      in
      (Node index, states)
  | _ -> (
      match
        pair_content product states ~untracked_fields ~truncated ~left_taints
          ~right_taints target1 target2
      with
      | None, states -> (Leaf Bot, states)
      | Some node, states ->
          Dynarray.add_last product.nodes node;
          (Node (Dynarray.length product.nodes - 1), states))

(* The node of the join at two positions; 'None' when it is 'Bot'. Below a
 * node on a cycle, the states it refers to are truncated. *)
and pair_content (product : product) (states : state list) ~untracked_fields
    ~(truncated : bool) ~left_taints ~right_taints (target1 : target)
    (target2 : target) : node option * state list =
  let truncated = truncated || on_cycle product target1 || on_cycle product target2 in
  let carry = union_whole ~traces:product.traces left_taints right_taints in
  let processed ~whole ~other (target : target) : node option * state list =
    match position_node product target with
    | Some node ->
        processed_node product states ~untracked_fields ~truncated ~carry ~whole ~other
          node
    | None -> (None, states)
  in
  match (target1, target2) with
  (* 'Bot' acts like a do-not-care. *)
  | Leaf Bot, _ -> processed ~whole:left_taints ~other:target1 target2
  | _, Leaf Bot -> processed ~whole:right_taints ~other:target2 target1
  (* 'Arg' acts like a shape variable. *)
  | Leaf (Arg _), _ -> processed ~whole:left_taints ~other:target1 target2
  | _, Leaf (Arg _) -> processed ~whole:right_taints ~other:target2 target1
  | _ ->
      pair_node product states ~untracked_fields ~truncated ~left_taints ~right_taints
        target1 target2

and union_whole ~(traces : T.kept_traces) (taints1 : Taints.t option)
    (taints2 : Taints.t option) : Taints.t =
  match (taints1, taints2) with
  | Some taints1, Some taints2 -> Taints.union ~traces taints1 taints2
  | Some taints, None
  | None, Some taints ->
      taints
  | None, None -> Taints.empty

(* The node of the join of two objects or closure sets; 'None' when the
 * result is 'Bot'. *)
and pair_node (product : product) (states : state list) ~untracked_fields
    ~(truncated : bool) ~left_taints ~right_taints (target1 : target)
    (target2 : target) : node option * state list =
  let carry = union_whole ~traces:product.traces left_taints right_taints in
  match (position_node product target1, position_node product target2) with
  | Some (Object object1), Some (Object object2) ->
      let untracked ~whole ~other (o : T.offset) (edge : edge) states =
        if untracked_fields then
          untracked_edge product states ~truncated ~carry ~whole ~other o edge
        else
          let edge, states = keep product states edge in
          (Some edge, states)
      in
      let fields, kept1, kept2, states =
        Fields.fold
          (fun (o : T.offset) (edge1 : edge)
               ((fields, kept1, kept2, states) : edge Fields.t * bool * bool * state list) ->
            match Fields.find_opt o object2.edges with
            | Some edge2 ->
                let edge, states = join_edges product states ~truncated edge1 edge2 in
                (Fields.add o edge fields, true, true, states)
            | None -> (
                match untracked ~whole:right_taints ~other:target2 o edge1 states with
                | Some edge, states -> (Fields.add o edge fields, true, kept2, states)
                | None, states -> (fields, kept1, kept2, states)))
          object1.edges (Fields.empty, false, false, states)
      in
      let fields, kept2, states =
        Fields.fold
          (fun (o : T.offset) (edge2 : edge)
               ((fields, kept2, states) : edge Fields.t * bool * state list) ->
            if Fields.mem o object1.edges then (fields, kept2, states)
            else
              match untracked ~whole:left_taints ~other:target1 o edge2 states with
              | Some edge, states -> (Fields.add o edge fields, true, states)
              | None, states -> (fields, kept2, states))
          object2.edges (fields, kept2, states)
      in
      let node =
        match (kept1, kept2) with
        | false, false -> None
        | true, false ->
            Some (Object { sites = object1.sites; summary = object1.summary; edges = fields })
        | false, true ->
            Some (Object { sites = object2.sites; summary = object2.summary; edges = fields })
        | true, true ->
            Some
              (Object
                 {
                   sites = Shape_and_sig.Sites.union object1.sites object2.sites;
                   summary = object1.summary || object2.summary;
                   edges = fields;
                 })
      in
      (node, states)
  | Some (Closures (c1, cs1)), Some (Closures (c2, cs2)) -> (
      match closure_sets product states ~truncated (c1 :: cs1) (c2 :: cs2) with
      | c :: cs, states -> (Some (Closures (c, cs)), states)
      | [], states -> (None, states))
  | Some node1, Some _ ->
      (* As in 'unify_shape_in': the left shape, as it must be seen at the
       * join. *)
      let show_target (target : target) : string =
        match target with
        | Node i -> spf "node<%d>" i
        | Leaf shape -> show_shape shape
      in
      Log.err (fun m ->
          m "Trying to unify incompatible shapes: %s ~ %s" (show_target target1)
            (show_target target2));
      processed_node product states ~untracked_fields ~truncated ~carry
        ~whole:right_taints ~other:target2 node1
  | None, _
  | _, None ->
      (None, states)

(* 'unify_closure_sets' on the closures of two closure sets, in increasing
 * order of their definitions. *)
and closure_sets (product : product) (states : state list) ~(truncated : bool)
    (cs1 : graph_closure list) (cs2 : graph_closure list) :
    graph_closure list * state list =
  match (cs1, cs2) with
  | [], cs
  | cs, [] ->
      List.fold_right
        (fun (closure : graph_closure) ((cs, states) : graph_closure list * state list) ->
          let closure, states = keep_closure product states closure in
          (closure :: cs, states))
        cs ([], states)
  | c1 :: rest1, c2 :: rest2 -> (
      match Function_id.compare c1.def c2.def with
      | 0 ->
          let c, states = closure_pair product states ~truncated c1 c2 in
          let cs, states = closure_sets product states ~truncated rest1 rest2 in
          (c :: cs, states)
      | n when n < 0 ->
          let c, states = keep_closure product states c1 in
          let cs, states = closure_sets product states ~truncated rest1 cs2 in
          (c :: cs, states)
      | _ ->
          let c, states = keep_closure product states c2 in
          let cs, states = closure_sets product states ~truncated cs1 rest2 in
          (c :: cs, states))

(* 'unify_closure' on two closures of one definition, whose captured cells
 * are joined as states of the product. *)
and closure_pair (product : product) (states : state list) ~(truncated : bool)
    (c1 : graph_closure) (c2 : graph_closure) : graph_closure * state list =
  let env, states =
    List.fold_left2
      (fun ((env, states) : (IL.name * graph_env_entry) list * state list)
           ((x, entry1) : IL.name * graph_env_entry)
           ((_, entry2) : IL.name * graph_env_entry) ->
        match (entry1, entry2) with
        | Val edge1, Val edge2 ->
            let edge, states = join_edges product states ~truncated edge1 edge2 in
            ((x, (Val edge : graph_env_entry)) :: env, states)
        | Val edge1, Ref _ ->
            let edge, states = keep product states edge1 in
            ((x, Val edge) :: env, states)
        | Ref _, _ -> ((x, entry1) :: env, states))
      ([], states) c1.env c2.env
  in
  let sig_ =
    if phys_equal c1.sig_ c2.sig_ then c1.sig_
    else
      {
        c1.sig_ with
        Signature.effects =
          Effects.union ~traces:product.traces c1.sig_.Signature.effects
            c2.sig_.Signature.effects;
      }
  in
  ({ c1 with sig_; env = List.rev env }, states)

(* 'taint_untracked_fields' on the edges of [node], whose other side is
 * [other] with the taint [whole]. *)
and processed_node (product : product) (states : state list) ~untracked_fields
    ~(truncated : bool) ~carry ~whole ~other (node : node) : node option * state list =
  match node with
  | Object ({ edges; _ } as node) ->
      let edges', states =
        Fields.fold
          (fun (o : T.offset) (edge : edge) ((edges, states) : edge Fields.t * state list) ->
            match
              if untracked_fields then
                untracked_edge product states ~truncated ~carry ~whole ~other o edge
              else
                let edge, states = keep product states edge in
                (Some edge, states)
            with
            | Some edge, states -> (Fields.add o edge edges, states)
            | None, states -> (edges, states))
          edges (Fields.empty, states)
      in
      if Fields.is_empty edges' && not (Fields.is_empty edges) then (None, states)
      else (Some (Object { node with edges = edges' }), states)
  | Closures _ ->
      let node, states = keep_node product states node in
      (Some node, states)

(* A field [o] of one side that the [other] side does not track: joined
 * with what a read of [o] gives on the other side, or its 'Clean' leaves
 * given the other side's [whole] taint ('taint_untracked_fields'). When
 * [truncated], the read through a parameter and the whole taint are not
 * extended by [o] but cut ('cut_poly_taints', 'join_graphs'). *)
and untracked_edge (product : product) (states : state list) ~(truncated : bool)
    ~carry ~whole ~(other : target) (o : T.offset) (edge : edge) :
    edge option * state list =
  let read_on_other =
    match (position_node product other, other, whole) with
    | Some (Object { edges; _ }), _, _ when Fields.mem o edges -> `Tracked
    | Some (Object { edges; _ }), _, _ -> (
        match Fields.find_opt T.Oany edges with
        | Some any -> `Edge any
        | None -> `Whole)
    | _, Leaf (Arg _), Some taints when truncated ->
        `Edge
          { xtaint = `Tainted (cut_poly_taints ~traces:product.traces taints); target = other }
    | _, Leaf (Arg (arg, base_offsets)), Some taints -> (
        match
          find_in_arg ~lang:product.lang ~traces:product.traces ~taints [ o ] arg
            base_offsets
        with
        | Some (Cell (xtaint, shape)) -> `Edge { xtaint; target = Leaf shape }
        | None -> `Whole)
    | _, _, _ -> `Whole
  in
  match (read_on_other, whole) with
  | `Tracked, _
  | `Whole, None ->
      let edge, states = keep product states edge in
      (Some edge, states)
  | `Edge any, _ ->
      let edge, states = join_edges product states ~truncated any edge in
      (Some edge, states)
  | `Whole, Some leaf ->
      replace product states ~truncated ~leaf ~carry
        ~extended:
          (if truncated then Lazy.from_val (cut_poly_taints ~traces:product.traces leaf)
           else
             lazy
               (fix_poly_taint_with_offset ~lang:product.lang ~traces:product.traces
                  [ o ] leaf))
        edge

(* 'replace_clean_leaves' on an edge; [extended] is [leaf] extended by the
 * offset of the edge from the join, computed only where a 'Clean' leaf is
 * below the edge: elsewhere the edge is kept as it is. *)
and replace (product : product) (states : state list) ~(truncated : bool) ~leaf
    ~carry ~(extended : Taints.t Lazy.t) (edge : edge) : edge option * state list =
  match (edge.xtaint, edge.target) with
  | `Clean, _ ->
      let extended = Lazy.force extended in
      if Taints.equal leaf carry || Taints.is_empty extended then (None, states)
      else (Some { xtaint = `Tainted extended; target = Leaf Bot }, states)
  | (`None | `Tainted _), Leaf shape when has_clean_leaves shape ->
      ( replace_clean_leaves ~lang:product.lang ~traces:product.traces ~offset:[]
          ~carry ~leaf ~extended:(Lazy.force extended) ~truncated
          (Cell (edge.xtaint, shape))
        |> Option.map (fun (Cell (xtaint, shape)) -> { xtaint; target = Leaf shape }),
        states )
  | (`None | `Tainted _), Node i when (Lazy.force product.clean_leaves).(i) -> (
      match product.operands.(i) with
      | Closures _ ->
          let edge, states = keep product states edge in
          (Some edge, states)
      | Object _ ->
          let carry =
            match edge.xtaint with
            | `Tainted taints -> taints
            | `None
            | `Clean ->
                carry
          in
          let index, states =
            node_of_state product states
              (Replaced
                 { node = i; leaf; carry; extended = Lazy.force extended; truncated })
          in
          (Some { edge with target = Node index }, states))
  | (`None | `Tainted _), (Leaf _ | Node _) ->
      let edge, states = keep product states edge in
      (Some edge, states)

let unify_cell ~lang ~(traces : T.kept_traces) cell1 cell2 =
  unify_cell_in ~lang ~traces cell1 cell2

let unify_shape ~lang ~(traces : T.kept_traces) shape1 shape2 =
  let keep ~other:(_ : shape) (shape : shape) : shape = shape in
  unify_shape_in ~lang ~traces ~process1:keep ~process2:keep shape1 shape2

let unify_obj ~lang ~(traces : T.kept_traces) obj1 obj2 =
  unify_obj_in ~lang ~traces obj1 obj2

(*********************************************************)
(* Object shapes *)
(*********************************************************)

let add_field_to_obj_check_invariant obj offset taints shape =
  match (Xtaint.of_taints taints, shape) with
  | `None, Bot ->
      (* Literal record/tuple construction observed this field with no
       * taint and no sub-structure. Record it as [`Clean] rather than
       * dropping it: a missing entry in an [Obj] makes
       * [find_in_obj_w_carry] fall through to [`Not_found] and leak
       * the caller's flattened poly-taint, breaking field-sensitive
       * taint across literal record/dict construction. [`Clean] blocks
       * that fallback in [find_in_cell_w_carry] while still letting
       * later writes (e.g. [obj.body = source()]) take effect via
       * [Xtaint.union `Clean (`Tainted _) = `Tainted _]. The cell
       * [(`Clean, Bot)] satisfies INVARIANT(cell).1 (xtaint ≠ None)
       * and INVARIANT(cell).2 (Clean ⇒ shape = Bot). *)
      Fields.add offset (Cell (`Clean, Bot)) obj
  | xtaint, shape -> Fields.add offset (Cell (xtaint, shape)) obj

let tuple_like_obj ~(traces : T.kept_traces) ~(site : T.call_loc)
    taints_and_shapes : shape =
  let _index, obj =
    taints_and_shapes
    |> List.fold_left
         (fun (i, obj) (taints, shape) ->
           let obj =
             add_field_to_obj_check_invariant obj (T.Oint i) taints shape
           in
           (i + 1, obj))
         (0, Fields.empty)
  in
  (* See INVARIANT(cell) *)
  obj_or_bot ~traces
    ~sites:(Shape_and_sig.Sites.singleton (Shape_and_sig.Built_at site))
    ~summary:false obj

let record_or_dict_like_obj ~lang ~(traces : T.kept_traces) ~(site : T.call_loc)
    taints_and_shapes : shape =
  let obj =
    taints_and_shapes
    |> List.fold_left
         (fun obj field ->
           match field with
           | `Field (name, taints, shape) ->
               add_field_to_obj_check_invariant obj (T.Ofld name) taints shape
           | `Entry (e, taints, shape) ->
               let offset =
                 match e.IL.e with
                 | Literal (Int pi) -> (
                     match Parsed_int.to_int_opt pi with
                     | None -> T.Oany
                     | Some i -> T.Oint i)
                 | Literal (String (_, (s, _), _)) -> Ostr s
                 | Literal (Atom (_, (s, _))) -> Ostr s
                 | __else__ -> T.Oany
               in
               add_field_to_obj_check_invariant obj offset taints shape
           | `Spread shape -> (
               match unfold shape with
               | Obj { fields = obj'; _ } -> unify_obj ~lang ~traces obj obj'
               | Bot
               | Graph _
               | Arg _
               | Fun _ ->
                   Log.err (fun m ->
                       m
                         "record_or_dict_like_obj: expected Obj shape but \
                          found %s"
                         (show_shape shape));
                   obj))
         Fields.empty
  in
  (* See INVARIANT(cell) *)
  obj_or_bot ~traces
    ~sites:(Shape_and_sig.Sites.singleton (Shape_and_sig.Built_at site))
    ~summary:false obj

(*********************************************************)
(* Collect/union all taints *)
(*********************************************************)

let gather_all_taints_in_cell ~(traces : T.kept_traces) =
  gather_all_taints_in_cell_acc ~traces Taints.empty

let gather_all_taints_in_shape ~(traces : T.kept_traces) =
  gather_all_taints_in_shape_acc ~traces Taints.empty

let gather_all_taints_in_args_taints ~(traces : T.kept_traces)
    (args_taints : (Taint.taints * shape) IL.argument list) : Taint.taints =
  args_taints
  |> List.fold_left
       (fun acc arg ->
         match arg with
         | IL.Named (_, (_, shape))
         | IL.Unnamed (_, shape) ->
             gather_all_taints_in_shape ~traces shape |> Taints.union ~traces acc)
       Taints.empty

(*********************************************************)
(* Depth widening (shape truncation) *)
(*********************************************************)

(* Widen a cell to at most [budget] further levels of [Obj] nesting.
 *
 * A self-recursive tree-builder (a function that wraps its own recursive
 * result in a fresh container) has no fixpoint in the shape domain: each
 * SCC round of the interfile fold nests its return shape one level deeper,
 * and branch unification can double the node count per round. This
 * truncates where the cost is incurred, when a signature is stored.
 *
 * Subtrees below the cutoff collapse into the cutoff cell's xtaint via
 * [gather_all_taints_in_cell]: fields below the cutoff then inherit the
 * cell's own taint (see the 'x.a.v' example in INVARIANT(cell)'s doc), so
 * reachability of the deep taints is preserved at the price of offset
 * precision. [`Clean] markers below the cutoff are dropped (conservative
 * towards taint). [Fun] shapes are left untouched here: truncating one would
 * drop its effects (real false negatives), they don't participate in the
 * tree-builder ascending chain bounded here, and their inner shapes were
 * already truncated when the lambda's own signature was stored. Their own
 * nesting is bounded separately by [bound_fun_shape].
 *
 * Returns [None] when the truncated cell carries no information
 * ([`None]/[Bot]), so obj entries can be dropped and INVARIANT(cell) is
 * preserved. *)
let rec truncate_cell ~(traces : T.kept_traces) ~budget cell : cell option =
  let (Cell (xtaint, shape)) = cell in
  match shape with
  | Bot
  | Graph _
  | Arg _
  | Fun _ ->
      Some cell
  | Obj ({ fields = obj; _ } as node) ->
      if budget <= 0 then (
        let deep = gather_all_taints_in_cell ~traces cell in
        if Taints.is_empty deep then None
        else Some (Cell (`Tainted deep, Bot)))
      else
        let obj' =
          Fields.filter_map
            (fun _o inner -> truncate_cell ~traces ~budget:(budget - 1) inner)
            obj
        in
        let shape' = obj_or_bot ~traces ~sites:node.sites ~summary:node.summary obj' in
        (match (xtaint, shape') with
        (* Restore INVARIANT(cell).1 *)
        | `None, Bot -> None
        (* Restore INVARIANT(cell).2, see 'unify_cell'. *)
        | `Clean, (Obj _ | Graph _ | Arg _ | Fun _) -> Some (Cell (`None, shape'))
        | ( (`Clean | `None | `Tainted _),
            (Bot | Obj _ | Graph _ | Arg _ | Fun _) ) ->
            Some (Cell (xtaint, shape')))

(* The depth of each object node of [g] (-1 when no path of objects reaches
 * it): its breadth-first distance from the root over the edges of objects,
 * the levels of 'Obj' nesting of the tree that unfolds the graph at its
 * first occurrence. A closure set is not descended into, as in
 * [truncate_cell]. *)
let object_depths (g : graph) : int array =
  let depths = Array.make (Array.length g.nodes) (-1) in
  Shape_and_sig.Adjacency_bfs.iter_component_dist
    (fun (i : int) (depth : int) -> depths.(i) <- depth)
    (Array.map
       (fun (node : node) ->
         match node with
         | Object _ -> successors node
         | Closures _ -> [])
       g.nodes)
    g.root;
  depths

(* Whether the edge of an object at [depth] reaches an object below
 * [max_depth], or a tree that exceeds the levels left. *)
let rec edge_exceeds (g : graph) (depths : int array) ~(max_depth : int)
    ~(depth : int) (edge : edge) : bool =
  match edge.target with
  | Node j -> (
      match g.nodes.(j) with
      | Object _ -> depths.(j) >= max_depth
      | Closures _ -> false)
  | Leaf shape -> shape_depth_exceeds ~budget:(max_depth - depth - 1) shape

(* Fast path for [truncate_shape]: [record_effects] truncates every effect
 * it records, and almost all shapes are nowhere near the cutoff, so don't
 * rebuild (reallocate) a shape that is already within budget. Short-circuits
 * via [Fields.exists]. *)
and cell_depth_exceeds ~budget (Cell (_xtaint, shape)) =
  shape_depth_exceeds ~budget shape

and shape_depth_exceeds ~budget shape =
  match shape with
  | Bot
  | Arg _
  | Fun _ ->
      false
  | Obj { fields; _ } ->
      budget <= 0
      || Fields.exists
           (fun _o cell -> cell_depth_exceeds ~budget:(budget - 1) cell)
           fields
  | Graph g ->
      let depths = object_depths g in
      Seq.exists
        (fun ((i, node) : int * node) ->
          match node with
          | Object { edges; _ } when depths.(i) >= 0 ->
              depths.(i) >= budget
              || Fields.exists
                   (fun _o (edge : edge) ->
                     edge_exceeds g depths ~max_depth:budget ~depth:depths.(i) edge)
                   edges
          | Object _
          | Closures _ ->
              false)
        (Array.to_seqi g.nodes)

(* Widen [shape] to at most [max_depth] levels of [Obj] nesting;
 * see [truncate_cell]. On a graph, an object at depth [max_depth] or below
 * ('object_depths') collapses into the taints of the cell that reaches it,
 * and a tree reached at a lower depth is truncated with the levels left. *)
let truncate_shape ~(traces : T.kept_traces) ~max_depth shape =
  if max_depth < 1 then shape
  else
    match shape with
    | Bot
    | Arg _
    | Fun _ ->
        shape
    | Obj ({ fields = obj; _ } as node) ->
        if not (shape_depth_exceeds ~budget:max_depth shape) then shape
        else
          let obj' =
            Fields.filter_map
              (fun _o cell -> truncate_cell ~traces ~budget:(max_depth - 1) cell)
              obj
          in
          obj_or_bot ~traces ~sites:node.sites ~summary:node.summary obj'
    | Graph g ->
        if not (shape_depth_exceeds ~budget:max_depth shape) then shape
        else
          let depths = object_depths g in
          let truncate_edge ~(depth : int) (edge : edge) : edge option =
            match edge.target with
            | Node j when edge_exceeds g depths ~max_depth ~depth edge ->
                let deep =
                  gather_all_taints_in_cell ~traces
                    (Cell (edge.xtaint, Graph { g with root = j }))
                in
                if Taints.is_empty deep then None
                else Some { xtaint = `Tainted deep; target = Leaf Bot }
            | Node _ -> Some edge
            | Leaf leaf ->
                truncate_cell ~traces ~budget:(max_depth - depth - 1)
                  (Cell (edge.xtaint, leaf))
                |> Option.map (fun (Cell (xtaint, leaf)) ->
                       { xtaint; target = Leaf leaf })
          in
          let nodes =
            Array.mapi
              (fun (i : int) (node : node) ->
                match node with
                | Object ({ edges; _ } as node)
                  when depths.(i) >= 0 && depths.(i) < max_depth ->
                    Object
                      {
                        node with
                        edges =
                          Fields.filter_map
                            (fun _o (edge : edge) ->
                              truncate_edge ~depth:depths.(i) edge)
                            edges;
                      }
                | Object _
                | Closures _ ->
                    node)
              g.nodes
          in
          let nodes, root = restore_cell_invariant nodes g.root in
          minimise ~traces nodes root

(* Widen the shapes an effect stores: [Obj] nesting by [truncate_shape],
 * [Fun] nesting by [bound_fun_shape]. Identity-preserving: returns [eff] itself
 * when no shape changed (both widenings return a shape physically unchanged
 * when it is within budget), so [Effects.map] in [truncate_signature] can
 * keep the original set without re-inserting structurally-equal effects. *)
let rec map_effect_shapes ~(widen : shape -> shape)
    (eff : Shape_and_sig.Effect.t) : Shape_and_sig.Effect.t =
  match eff with
  | Shape_and_sig.Effect.ToReturn tr ->
      let data_shape = widen tr.data_shape in
      if phys_equal data_shape tr.data_shape then eff
      else Shape_and_sig.Effect.ToReturn { tr with data_shape }
  | Shape_and_sig.Effect.ToSinkInCall r ->
      let widen_arg arg =
        match arg with
        | IL.Unnamed (taints, shape) ->
            let shape' = widen shape in
            if phys_equal shape' shape then arg
            else IL.Unnamed (taints, shape')
        | IL.Named (ident, (taints, shape)) ->
            let shape' = widen shape in
            if phys_equal shape' shape then arg
            else IL.Named (ident, (taints, shape'))
      in
      let args_taints = List_.map widen_arg r.args_taints in
      if List.for_all2 phys_equal args_taints r.args_taints then eff
      else Shape_and_sig.Effect.ToSinkInCall { r with args_taints }
  | Shape_and_sig.Effect.ToSink _
  | Shape_and_sig.Effect.ToLval _ ->
      eff

(* Bound the nesting of [Fun] shapes to [levels] below this point; see
 * [Limits_semgrep.taint_MAX_SIG_FUN_DEPTH] for why a stored signature
 * otherwise grows without bound. A [Fun] past the budget collapses to
 * [Bot]; within it, the same bound applies one level down to the shapes
 * its own effects store. Identity-preserving like [truncate_shape]. *)
and bound_fun_shape ~(traces : T.kept_traces) ~levels (shape : shape) : shape =
  match shape with
  | Bot
  | Arg _ ->
      shape
  | Graph g -> (
      (* The same bound on the trees of the edges and on the closure sets'
       * effects and captured trees; a closure set reached by an edge keeps
       * its node. *)
      let bound_edge ~(levels : int) (edge : edge) : edge =
        match edge.target with
        | Leaf inner ->
            let inner' = bound_fun_shape ~traces ~levels inner in
            if phys_equal inner' inner then edge else { edge with target = Leaf inner' }
        | Node _ -> edge
      in
      let bound_node (node : node) : node =
        match node with
        | Object ({ edges; _ } as object_) ->
            let edges' = Fields.map (bound_edge ~levels) edges in
            if Fields.equal phys_equal edges' edges then node
            else Object { object_ with edges = edges' }
        | Closures (c, cs) ->
            let bound_closure (closure : graph_closure) : graph_closure =
              let sig_ = closure.sig_ in
              let effects =
                Effects.map ~traces
                  (map_effect_shapes
                     ~widen:(bound_fun_shape ~traces ~levels:(levels - 1)))
                  sig_.Signature.effects
              in
              let env' =
                List_.map
                  (fun ((x, entry) as binding : IL.name * graph_env_entry) ->
                    match entry with
                    | Ref _ -> binding
                    | Val edge ->
                        let edge' = bound_edge ~levels:(levels - 1) edge in
                        if phys_equal edge' edge then binding
                        else (x, (Val edge' : graph_env_entry)))
                  closure.env
              in
              if
                phys_equal effects sig_.Signature.effects
                && List.for_all2 phys_equal env' closure.env
              then closure
              else { closure with sig_ = { sig_ with Signature.effects }; env = env' }
            in
            let c' = bound_closure c in
            let cs' = List_.map bound_closure cs in
            if phys_equal c' c && List.for_all2 phys_equal cs' cs then node
            else Closures (c', cs')
      in
      match g.nodes.(g.root) with
      | Closures _ when levels <= 0 -> Bot
      | Object _
      | Closures _ ->
          let nodes = Array.map bound_node g.nodes in
          if Array.for_all2 phys_equal nodes g.nodes then shape
          else minimise ~traces nodes (Node g.root))
  | Fun (c, cs) ->
      if levels <= 0 then Bot
      else
        let bound_closure (closure : closure) : closure =
          let sig_ = closure.sig_ in
          let effects =
            Effects.map ~traces
              (map_effect_shapes
                 ~widen:(bound_fun_shape ~traces ~levels:(levels - 1)))
              sig_.Signature.effects
          in
          let env' =
            List_.map
              (fun ((x, entry) as binding) ->
                match entry with
                | Ref _ -> binding
                | Val (Cell (xtaint, inner)) ->
                    let inner' = bound_fun_shape ~traces ~levels:(levels - 1) inner in
                    if phys_equal inner' inner then binding
                    else (x, Val (Cell (xtaint, inner'))))
              closure.env
          in
          if
            phys_equal effects sig_.Signature.effects
            && List.for_all2 phys_equal env' closure.env
          then closure
          else { closure with sig_ = { sig_ with Signature.effects }; env = env' }
        in
        let c', cs' = Shape_and_sig.map_closures bound_closure (c, cs) in
        if phys_equal c' c && phys_equal cs' cs then shape else Fun (c', cs')
  | Obj ({ fields = obj; _ } as node) ->
      let changed = ref false in
      let obj' =
        Fields.map
          (fun (Cell (xtaint, inner) as cell) ->
            let inner' = bound_fun_shape ~traces ~levels inner in
            if phys_equal inner' inner then cell
            else (
              changed := true;
              Cell (xtaint, inner')))
          obj
      in
      if !changed then Obj { node with fields = obj' } else shape

let truncate_effect ~(traces : T.kept_traces) ~max_depth
    (eff : Shape_and_sig.Effect.t) :
    Shape_and_sig.Effect.t =
  (* The positions holding multiple results are not a level of a value:
   * each result keeps [max_depth] levels. *)
  let max_depth =
    match eff with
    | Shape_and_sig.Effect.ToReturn { multiple_results = true; _ }
      when max_depth >= 1 ->
        max_depth + 1
    | _ -> max_depth
  in
  let widen shape =
    truncate_shape ~traces ~max_depth shape
    |> bound_fun_shape ~traces ~levels:Limits_semgrep.taint_MAX_SIG_FUN_DEPTH
  in
  map_effect_shapes ~widen eff

(* Widen every shape stored in a signature's effects; the entry point used
 * by the interfile fold when a signature is (re-)stored. [Effects.map]
 * returns the set physically unchanged when [truncate_effect] is the
 * identity on every element, so the common case allocates nothing. *)
let truncate_signature ~(traces : T.kept_traces) ~max_depth (s : Signature.t) :
    Signature.t =
  let effects = Effects.map ~traces (truncate_effect ~traces ~max_depth) s.effects in
  if phys_equal effects s.effects then s else { s with effects }

(*********************************************************)
(* Find an offset *)
(*********************************************************)

let cell_read_of_find_result ?max ~lang ~(traces : T.kept_traces) res :
    cell option =
  match res with
  | `Found cell -> Some cell
  | `Clean -> None
  | `Not_found (taints, _shape, offset) ->
      let taints = fix_poly_taint_with_offset ?max ~lang ~traces offset taints in
      if Taints.is_empty taints then None
      else Some (Cell (`Tainted taints, Bot))

let rec find_in_cell_w_carry ?max ~lang ~(traces : T.kept_traces) ~taints
    offset cell =
  let (Cell (xtaint, shape)) = cell in
  match offset with
  | [] -> `Found cell
  | _ :: _ -> (
      match xtaint with
      | `Clean ->
          if shape <> Bot then
            Log.err (fun m ->
                m "BUG: Taint_shape.find_in_cell: INVARIANT(cell).2 is broken");
          `Clean
      | `None ->
          find_in_shape_w_carry ?max ~lang ~traces ~taints offset shape
      | `Tainted taints ->
          find_in_shape_w_carry ?max ~lang ~traces ~taints offset shape)

and find_in_shape_w_carry ?max ~lang ~(traces : T.kept_traces) ~taints offset
    shape =
  let not_found () = `Not_found (taints, shape, offset) in
  match shape with
  (* offset <> [] *)
  | Bot -> not_found ()
  | Obj { fields = obj; _ } ->
      find_in_obj_w_carry ?max ~lang ~traces ~taints ~self:shape offset obj
  | Graph _ -> (
      (* The root node's edges, each to the value that starts at its
       * target. *)
      match unfold shape with
      | Obj { fields = obj; _ } ->
          find_in_obj_w_carry ?max ~lang ~traces ~taints ~self:shape offset obj
      | Bot
      | Graph _
      | Arg _
      | Fun _ ->
          Log.err (fun m ->
              m "Could not find offset %s in function shape %s"
                (debug_offset offset) (show_shape shape));
          not_found ())
  | Arg (arg, base_offsets) -> (
      match find_in_arg ?max ~lang ~traces ~taints offset arg base_offsets with
      | Some cell -> `Found cell
      | None ->
          Log.debug (fun m ->
              m "Could not find offset %s in polymorphic shape %s"
                (debug_offset offset) (show_shape shape));
          not_found ())
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not find offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      not_found ()

and find_in_obj_w_carry ?max ~lang ~(traces : T.kept_traces) ~taints
    ~(self : shape) (offset : T.offset list) obj =
  let not_found () = `Not_found (taints, self, offset) in
  (* offset <> [] *)
  match offset with
  | [] ->
      Log.err (fun m -> m "BUG: Taint_shape.fix_xtaint_obj: empty offset");
      not_found ()
  | o :: offset -> (
      match o with
      | Oany (* arbitrary index [*] *) -> (
          (* consider all fields/indexes *)
          match
            Fields.fold
              (fun _ cell acc ->
                match
                  ( acc,
                    cell_read_of_find_result ?max ~lang ~traces
                      (find_in_cell_w_carry ?max ~lang ~traces ~taints offset
                         cell) )
                with
                | acc, None -> acc
                | None, (Some _ as found) -> found
                | Some cell1, Some cell2 -> Some (unify_cell ~lang ~traces cell1 cell2))
              obj None
          with
          | None -> not_found ()
          | Some cell -> `Found cell)
      | Oslice n -> (
          (* Read the trailing-rest from index [n]. Filter [Fields] to
           * entries that intersect the slice [n, infinity), recurse with
           * the appropriate composed offset, unify all matches.
           * The composition law lets a coarser sub-slice [Oslice m]
           * (m < n) contribute its tail-from-(n-m):
           *   Oslice n :: Oslice m :: rest = Oslice (n-m) :: rest. *)
          match
            Fields.fold
              (fun key cell acc ->
                let recur_offset =
                  match key with
                  | Oint k when k >= n -> Some offset
                  | Oslice m when m >= n -> Some offset
                  | Oslice m ->
                      (* m < n by case order; n - m > 0 *)
                      Some (T.Oslice (n - m) :: offset)
                  | Oint _
                  | Ofld _
                  | Ostr _
                  | Oany ->
                      None
                in
                match recur_offset with
                | None -> acc
                | Some recur_offset -> (
                    match
                      ( acc,
                        cell_read_of_find_result ?max ~lang ~traces
                          (find_in_cell_w_carry ?max ~lang ~traces ~taints
                             recur_offset cell) )
                    with
                    | acc, None -> acc
                    | None, (Some _ as found) -> found
                    | Some c1, Some c2 -> Some (unify_cell ~lang ~traces c1 c2)))
              obj None
          with
          | None -> not_found ()
          | Some cell -> `Found cell)
      | Ofld _
      | Oint _
      | Ostr _ -> (
          match Fields.find_opt o obj with
          | Some o_cell ->
              find_in_cell_w_carry ?max ~lang ~traces ~taints offset o_cell
          | None -> (
              (* Per INVARIANT(obj) in [Shape_and_sig], an [Oany] entry
               * carries the taint and shape of any field that is not
               * explicitly tracked — e.g. after `arr[i] = tainted` the
               * taint lives under [Oany], and a read of the untracked
               * `arr[0]` must find it. A *tracked* field does not consult
               * [Oany]: writes through [Oany] weak-update every tracked
               * entry (see [update_offset_in_obj]), so tracked entries
               * are already up to date. *)
              match Fields.find_opt T.Oany obj with
              | None -> not_found ()
              | Some any_cell ->
                  find_in_cell_w_carry ?max ~lang ~traces ~taints offset
                    any_cell)))

let find_in_cell ?max ~lang ~(traces : T.kept_traces) offset cell =
  find_in_cell_w_carry ?max ~lang ~traces ~taints:Taints.empty offset cell

let option_of_find_result ?max ~lang ~(traces : T.kept_traces) res =
  match res with
  | `Clean -> None
  | `Not_found (taints, _shape, offset) ->
      (* TODO: Fix _shape too. *)
      let taints = fix_poly_taint_with_offset ?max ~lang ~traces offset taints in
      Some (taints, Bot)
  | `Found (Cell (xtaint, shape)) -> Some (Xtaint.to_taints xtaint, shape)

let find_in_cell_poly ?max ~lang ~(traces : T.kept_traces) offset cell =
  find_in_cell ?max ~lang ~traces offset cell
  |> option_of_find_result ?max ~lang ~traces

let find_in_shape_poly ?max ~lang ~(traces : T.kept_traces) ~taints offset shape =
  match offset with
  | [] -> Some (taints, shape)
  | _ :: _ ->
      find_in_shape_w_carry ?max ~lang ~traces ~taints offset shape
      |> option_of_find_result ?max ~lang ~traces

(*********************************************************)
(* Update the xtaint and shape of an offset *)
(*********************************************************)

let cell_of_update (xtaint : Xtaint.t) (shape : shape) : cell option =
  match (xtaint, shape) with
  (* Restore INVARIANT(cell).1 *)
  | `None, Bot -> None
  | `Tainted taints, Bot when Taints.is_empty taints -> None
  (* Restore INVARIANT(cell).2 *)
  | `Clean, (Obj _ | Graph _ | Arg _ | Fun _) ->
      (* If we are tainting an offset of this cell, the cell cannot be
         considered clean anymore. *)
      Some (Cell (`None, shape))
  | `Clean, Bot
  | `None, (Obj _ | Graph _ | Arg _ | Fun _)
  | `Tainted _, (Bot | Obj _ | Graph _ | Arg _ | Fun _) ->
      Some (Cell (xtaint, shape))

(* 'cell_of_update' on an edge. *)
let edge_of_update (builder : node Dynarray.t) (xtaint : Xtaint.t) (target : target) :
    edge option =
  match target with
  | Node _ -> (
      match xtaint with
      | `Clean -> Some { xtaint = `None; target }
      | `None
      | `Tainted _ ->
          Some { xtaint; target })
  | Leaf shape ->
      cell_of_update xtaint shape
      |> Option.map (fun (Cell (xtaint, shape)) ->
             { xtaint; target = target_of_shape builder shape })

(* The fields of [obj] that the first key of [offset] selects, each updated
 * by [update] with the rest of [offset]; 'None' removes the field. *)
let update_fields ~none ~update (offset : T.offset list) obj =
  match offset with
  | [] ->
      Log.err (fun m ->
          m "internal_UNSAFE_update_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = internal_UNSAFE_find_offset_in_obj ~none o obj in
      match o with
      | Oany (* arbitrary index [*] *) ->
          (* consider all fields/indexes *)
          Fields.filter_map (fun _o' -> update offset) obj
      | Oslice n ->
          (* Update the trailing-rest from index [n]. For each entry
           * intersecting [n, infinity), apply the update with the
           * appropriately composed inner offset; entries outside the
           * slice pass through unchanged. *)
          Fields.filter_map
            (fun key value ->
              match key with
              | Oint k when k >= n -> update offset value
              | Oslice m when m >= n -> update offset value
              | Oslice m ->
                  (* m < n by case order; n - m > 0 *)
                  update (T.Oslice (n - m) :: offset) value
              | Oint _
              | Ofld _
              | Ostr _
              | Oany ->
                  Some value)
            obj
      | Ofld _
      | Oint _
      | Ostr _ ->
          obj
          |> Fields.update o (fun opt_value ->
                 let* value = opt_value in
                 update offset value))

(* Finds an 'offset' within a 'cell' and updates it via 'f'. *)
let rec update_offset_in_cell_at ~(traces : T.kept_traces) ~(write : T.call_loc)
    ~(depth : int) ~f offset cell =
  let xtaint, shape =
    match (cell, offset) with
    | Cell (xtaint, shape), [] -> f xtaint shape
    | Cell (xtaint, shape), _ :: _ ->
        let shape = update_offset_in_shape ~traces ~write ~depth ~f offset shape in
        (xtaint, shape)
  in
  cell_of_update xtaint shape

and update_offset_in_shape ~(traces : T.kept_traces) ~(write : T.call_loc)
    ~(depth : int) ~f offset shape =
  match shape with
  | Bot
  | Arg _ ->
      let shape = written_obj ~write ~depth in
      update_offset_in_shape ~traces ~write ~depth ~f offset shape
  | Obj ({ fields = obj; _ } as node) -> (
      match update_offset_in_obj ~traces ~write ~depth ~f offset obj with
      | None -> Bot
      | Some obj -> canonical ~traces (Obj { node with fields = obj }))
  | Graph g -> update_offset_in_graph ~traces ~write ~depth ~f offset g
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not update offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      shape

and update_offset_in_obj ~(traces : T.kept_traces) ~(write : T.call_loc)
    ~(depth : int) ~f offset obj =
  let obj' =
    update_fields ~none:cell_none_bot
      ~update:(update_offset_in_cell_at ~traces ~write ~depth:(depth + 1) ~f)
      offset obj
  in
  if Fields.is_empty obj' then None else Some obj'

(* [offset] of the value [g] updated by [f]. A node on the path that is not a
 * summary is copied (path copying, Driscoll, Sarnak, Sleator and Tarjan
 * 1989), so every other edge into it keeps the old node. A summary node is
 * replaced in its slot, so every edge into it sees the new version, cycles
 * included: a weak update (Chase, Wegman and Zadeck 1990). The fields that
 * the write changes are set on the slot as it is after the writes below,
 * which may have reached the slot again through a cycle. *)
and update_offset_in_graph ~(traces : T.kept_traces) ~(write : T.call_loc)
    ~(depth : int) ~f offset (g : graph) : shape =
  let order = Array.of_list (preorder (Array.map successors g.nodes) g.root) in
  let nodes = Dynarray.create () in
  let root = append_graph nodes g in
  let rec write_node (k : int) (depth : int) (offset : T.offset list) : target =
    match Dynarray.get nodes k with
    | Closures _ ->
        (* This is an error, we just don't want to crash here. *)
        Log.err (fun m ->
            m "Could not update offset %s in function shape %s"
              (debug_offset offset) (show_shape (Graph g)));
        Node k
    | Object ({ edges; summary; _ } as node) ->
        let edges' =
          update_fields ~none:edge_none_bot ~update:(write_edge (depth + 1)) offset edges
        in
        if summary then (
          let changed (o : T.offset) : bool =
            match (Fields.find_opt o edges, Fields.find_opt o edges') with
            | Some edge, Some edge' -> not (phys_equal edge edge')
            | None, None -> false
            | Some _, None
            | None, Some _ ->
                true
          in
          (match Dynarray.get nodes k with
          | Object current ->
              let keys = Fields.union (fun _o edge _ -> Some edge) edges edges' in
              let edges =
                Fields.fold
                  (fun (o : T.offset) _ (current_edges : edge Fields.t) ->
                    if not (changed o) then current_edges
                    else
                      match Fields.find_opt o edges' with
                      | Some edge' -> Fields.add o edge' current_edges
                      | None -> Fields.remove o current_edges)
                  keys current.edges
              in
              Dynarray.set nodes k (Object { current with edges })
          | Closures _ -> ());
          Node k)
        else if Fields.is_empty edges' then Leaf Bot
        else (
          Dynarray.add_last nodes (Object { node with edges = edges' });
          Node (Dynarray.length nodes - 1))
  and write_edge (depth : int) (offset : T.offset list) (edge : edge) : edge option =
    match (edge.target, offset) with
    | Leaf shape, _ ->
        update_offset_in_cell_at ~traces ~write ~depth ~f offset
          (Cell (edge.xtaint, shape))
        |> Option.map (fun (Cell (xtaint, shape)) ->
               { xtaint; target = target_of_shape nodes shape })
    | Node i, [] ->
        (* A node copied from [g] is the node [order.(i)] of [g], whose value
         * is the old value at the offset; a node that this write built holds
         * its current value. *)
        let value =
          if i < Array.length order then Graph { g with root = order.(i) }
          else minimise ~traces (Dynarray.to_array nodes) (Node i)
        in
        let xtaint, shape = f edge.xtaint value in
        edge_of_update nodes xtaint (target_of_shape nodes shape)
    | Node i, _ :: _ -> edge_of_update nodes edge.xtaint (write_node i depth offset)
  in
  let target = write_node root depth offset in
  minimise ~traces (Dynarray.to_array nodes) target

let update_offset_in_cell ~(traces : T.kept_traces) ~(write : T.call_loc) ~f
    offset cell =
  update_offset_in_cell_at ~traces ~write ~depth:0 ~f offset cell

(* One map over the nodes of [g]: each object's sites by [sites], each
 * edge's taint by [xtaint], each tree by [shape] (a graph that it returns is
 * appended), each closure by [closure] (given the map of an edge and the
 * edge of a cell); every edge of an object restored to INVARIANT(cell) as
 * 'update_offset_in_cell' restores a cell, then 'restore_cell_invariant'
 * and 'minimise'. *)
let map_graph ~(traces : T.kept_traces)
    ~(sites : Shape_and_sig.Sites.t -> Shape_and_sig.Sites.t)
    ~(xtaint : Xtaint.t -> Xtaint.t) ~(shape : shape -> shape)
    ~(closure :
       inst_value:(edge -> edge) ->
       value_of_cell:(cell -> edge) ->
       graph_closure ->
       graph_closure) (g : graph) : shape =
  let nodes = Dynarray.create () in
  let root = append_graph nodes g in
  let map_edge (edge : edge) : edge =
    {
      xtaint = xtaint edge.xtaint;
      target =
        (match edge.target with
        | Node _ -> edge.target
        | Leaf leaf -> target_of_shape nodes (shape leaf));
    }
  in
  let edge_of_cell (Cell (xtaint, shape) : cell) : edge =
    { xtaint; target = target_of_shape nodes shape }
  in
  Array.iteri
    (fun (k : int) (node : node) ->
      Dynarray.set nodes k
        (match node with
        | Object ({ sites = object_sites; edges; _ } as node) ->
            Object
              {
                node with
                sites = sites object_sites;
                edges =
                  Fields.filter_map
                    (fun _o (edge : edge) ->
                      let edge = map_edge edge in
                      edge_of_update nodes edge.xtaint edge.target)
                    edges;
              }
        | Closures (c, cs) ->
            let closure = closure ~inst_value:map_edge ~value_of_cell:edge_of_cell in
            Closures (closure c, List.map closure cs)))
    (Dynarray.to_array nodes);
  let nodes, root = restore_cell_invariant (Dynarray.to_array nodes) root in
  minimise ~traces nodes root

(*********************************************************)
(* Updating an offset *)
(*********************************************************)

let update_offset_and_unify ~lang ~(traces : T.kept_traces) ~(write : T.call_loc)
    new_taints new_shape offset opt_cell =
  if taints_and_shape_are_relevant new_taints new_shape then
    let new_xtaint =
      (* THINK: Maybe Dataflow_tainting 'check_xyz' should be returning 'Xtaint.t'? *)
      Xtaint.of_taints new_taints
    in
    let cell = opt_cell ||| cell_none_bot in
    let add_new_taints xtaint shape =
      let shape = unify_shape ~lang ~traces new_shape shape in
      match xtaint with
      | `None
      | `Clean ->
          (* Since we're adding taint we cannot have `Clean here. *)
          (new_xtaint, shape)
      | `Tainted taints as xtaint ->
          if
            !Flag_semgrep.max_taint_set_size =|= 0
            || Taints.cardinal taints < !Flag_semgrep.max_taint_set_size
          then (Xtaint.union ~traces new_xtaint xtaint, shape)
          else (
            (* nosemgrep: no-logs-in-library *)
            Log.warn (fun m ->
                m
                  "TAINT_SET_SATURATED: offset=%s cardinal=%d dropping=%d"
                  (offset |> List_.map T.show_offset |> String.concat "")
                  (Taints.cardinal taints)
                  (match new_xtaint with
                   | `Tainted new_ts -> Taints.cardinal new_ts
                   | _ -> 0));
            (xtaint, shape))
    in
    update_offset_in_cell ~traces ~write ~f:add_new_taints offset cell
  else
    (* To maintain INVARIANT(cell) we cannot return 'cell_none_bot'! *)
    opt_cell

(*********************************************************)
(* Clean taint *)
(*********************************************************)

(* The fields of [obj] that the first key of [offset] selects, each cleaned
 * by [clean] with the rest of [offset]. *)
let clean_fields ~none ~clean (offset : T.offset list) obj =
  match offset with
  | [] ->
      Log.err (fun m -> m "clean_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = internal_UNSAFE_find_offset_in_obj ~none o obj in
      match o with
      | Oany -> Fields.map (clean offset) obj
      | o -> Fields.update o (Option.map (fun value -> clean offset value)) obj)

(* TODO: Reformulate in terms of 'update_offset_in_cell' *)
let rec clean_cell_at ~(traces : T.kept_traces) ~(write : T.call_loc)
    ~(depth : int) (offset : T.offset list) cell =
  let (Cell (xtaint, shape)) = cell in
  match offset with
  | [] ->
      (* See INVARIANT(cell)
       *
       * THINK: If we had aliasing, we would have to keep the previous shape
       *  and just clean it all ? And we would also need to remove the 'Clean'
       *  mark from other cells that may be pointing to this cell in order to
       *  maintain the invariant ? *)
      Cell (`Clean, Bot)
  | [ Oany ] ->
      (* If an object is tainted, and we clean all its fields/indexes, then we
       * just clean the object itself. For example, if we assume that an array `a`
       * is tainted, and then we see `a[*]` being sanitized, then we assume that
       * `a` itself is being sanitized; otherwise `sink(a)` could be reported. *)
      Cell (`Clean, Bot)
  | _ :: _ ->
      let shape = clean_shape ~traces ~write ~depth offset shape in
      Cell (xtaint, shape)

and clean_shape ~(traces : T.kept_traces) ~(write : T.call_loc) ~(depth : int)
    offset shape =
  match shape with
  | Bot
  | Arg _ ->
      let shape = written_obj ~write ~depth in
      clean_shape ~traces ~write ~depth offset shape
  (* A summary stands for several objects and the clean reaches one of
   * them, so the others keep their taint. *)
  | Obj { summary = true; _ } -> shape
  | Obj ({ fields; _ } as node) ->
      Obj { node with fields = clean_obj ~traces ~write ~depth offset fields }
  | Graph g -> clean_graph ~traces ~write ~depth offset g
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not update offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      shape

and clean_obj ~(traces : T.kept_traces) ~(write : T.call_loc) ~(depth : int)
    offset obj =
  clean_fields ~none:cell_none_bot
    ~clean:(clean_cell_at ~traces ~write ~depth:(depth + 1))
    offset obj

(* 'clean_shape' on a graph: the walk of 'update_offset_in_graph', which
 * stops at a summary node, as 'clean_shape' stops at a summary object. *)
and clean_graph ~(traces : T.kept_traces) ~(write : T.call_loc) ~(depth : int)
    offset (g : graph) : shape =
  let nodes = Dynarray.create () in
  let root = append_graph nodes g in
  let rec clean_node (k : int) (depth : int) (offset : T.offset list) : target =
    match Dynarray.get nodes k with
    | Object { summary = true; _ } -> Node k
    | Closures _ ->
        (* This is an error, we just don't want to crash here. *)
        Log.err (fun m ->
            m "Could not update offset %s in function shape %s"
              (debug_offset offset) (show_shape (Graph g)));
        Node k
    | Object ({ edges; _ } as node) ->
        let edges =
          clean_fields ~none:edge_none_bot ~clean:(clean_edge (depth + 1)) offset edges
        in
        Dynarray.add_last nodes (Object { node with edges });
        Node (Dynarray.length nodes - 1)
  and clean_edge (depth : int) (offset : T.offset list) (edge : edge) : edge =
    match (edge.target, offset) with
    | Leaf shape, _ ->
        let (Cell (xtaint, shape)) =
          clean_cell_at ~traces ~write ~depth offset (Cell (edge.xtaint, shape))
        in
        { xtaint; target = Leaf shape }
    | Node _, ([] | [ Oany ]) -> { xtaint = `Clean; target = Leaf Bot }
    | Node i, _ :: _ -> { edge with target = clean_node i depth offset }
  in
  let target = clean_node root depth offset in
  minimise ~traces (Dynarray.to_array nodes) target

let clean_cell ~(traces : T.kept_traces) ~(write : T.call_loc)
    (offset : T.offset list) cell =
  clean_cell_at ~traces ~write ~depth:0 offset cell

(*********************************************************)
(* Folding objects nested in an object of the same site *)
(*********************************************************)

module Defs = Set.Make (Function_id)

module Captured = Map.Make (struct
  type t = Function_id.t * int

  let compare ((def1, i1) : t) ((def2, i2) : t) : int =
    match Function_id.compare def1 def2 with
    | 0 -> Int.compare i1 i2
    | other -> other
end)

type node = {
  sites : Shape_and_sig.Sites.t;
  defs : Defs.t;
  summary : bool;
  edges : edge Fields.t;
  captured : edge Captured.t;
  original_shape : shape;
  component : int option;
}

type quotient = {
  nodes : node Dynarray.t;
  parent : int Dynarray.t;
  size : int Dynarray.t;
  members : int list Dynarray.t;
  class_sites : Shape_and_sig.Sites.t Dynarray.t;
  class_defs : Defs.t Dynarray.t;
  copies : int Fields.t Dynarray.t;
  back_edge_target : bool Dynarray.t;
  incoming : edge list Dynarray.t;
  visited : bool Dynarray.t;
  mutable unions : int;
}

type ancestor_class = { class_root : int; size_at_visit : int; depth : int }

type carried_taint = { object_taints : Taints.t; offset : T.offset list; carry : Taints.t }

let new_quotient () : quotient =
  {
    nodes = Dynarray.create ();
    parent = Dynarray.create ();
    size = Dynarray.create ();
    members = Dynarray.create ();
    class_sites = Dynarray.create ();
    class_defs = Dynarray.create ();
    copies = Dynarray.create ();
    back_edge_target = Dynarray.create ();
    incoming = Dynarray.create ();
    visited = Dynarray.create ();
    unions = 0;
  }

let add_node (q : quotient) (node : node) : int =
  let i = Dynarray.length q.nodes in
  Dynarray.add_last q.nodes node;
  Dynarray.add_last q.parent i;
  Dynarray.add_last q.size 1;
  Dynarray.add_last q.members [ i ];
  Dynarray.add_last q.class_sites node.sites;
  Dynarray.add_last q.class_defs node.defs;
  Dynarray.add_last q.copies Fields.empty;
  Dynarray.add_last q.back_edge_target false;
  Dynarray.add_last q.incoming [];
  Dynarray.add_last q.visited false;
  i

(* The captured cells of [closures] by definition and position. *)
let captured_by_position edge_of closures : edge Captured.t =
  List.fold_left
    (fun captured closure ->
      snd
        (List.fold_left
           (fun ((position, captured) : int * edge Captured.t) (_, entry) ->
             match entry with
             | Val value ->
                 ( position + 1,
                   Captured.add (closure.def, position) (edge_of value) captured )
             | Ref _ -> (position + 1, captured))
           (0, captured) closure.env))
    Captured.empty closures

(* The nodes of the quotient for [shape]: one per object and closure set of
 * a tree, one per node of a graph, each graph node with the strongly
 * connected component (Tarjan 1972) it lies in. *)
let rec graph_of_shape (q : quotient) (shape : shape) : target =
  let edge_of (Cell (xtaint, shape) : cell) : edge =
    { xtaint; target = graph_of_shape q shape }
  in
  match shape with
  | Obj { sites; summary; fields } ->
      let i =
        add_node q
          {
            sites;
            defs = Defs.empty;
            summary;
            edges = Fields.empty;
            captured = Captured.empty;
            original_shape = shape;
            component = None;
          }
      in
      let edges = Fields.map edge_of fields in
      Dynarray.set q.nodes i { (Dynarray.get q.nodes i) with edges };
      Node i
  | Fun (c, cs) ->
      let closures = c :: cs in
      let i =
        add_node q
          {
            sites = Shape_and_sig.Sites.empty;
            defs = Defs.of_list (List.map (fun (closure : closure) -> closure.def) closures);
            summary = false;
            edges = Fields.empty;
            captured = Captured.empty;
            original_shape = shape;
            component = None;
          }
      in
      let captured = captured_by_position edge_of closures in
      Dynarray.set q.nodes i { (Dynarray.get q.nodes i) with captured };
      Node i
  | Graph g ->
      let order = Array.of_list (preorder (Array.map successors g.nodes) g.root) in
      let base = Dynarray.length q.nodes in
      let index = Array.make (Array.length g.nodes) (-1) in
      Array.iteri (fun (k : int) (i : int) -> index.(i) <- base + k) order;
      let component_of =
        Shape_and_sig.Adjacency_components.scc
          (Array.map
             (fun (i : int) ->
               List.map (fun (j : int) -> index.(j) - base) (successors g.nodes.(i)))
             order)
        |> snd
      in
      Array.iteri
        (fun (k : int) (i : int) ->
          let defs =
            match g.nodes.(i) with
            | Object _ -> Defs.empty
            | Closures (c, cs) ->
                Defs.of_list
                  (List.map (fun (closure : graph_closure) -> closure.def) (c :: cs))
          in
          let sites, summary =
            match g.nodes.(i) with
            | Object { sites; summary; _ } -> (sites, summary)
            | Closures _ -> (Shape_and_sig.Sites.empty, false)
          in
          ignore
            (add_node q
               {
                 sites;
                 defs;
                 summary;
                 edges = Fields.empty;
                 captured = Captured.empty;
                 original_shape = Graph { g with root = i };
                 component = Some (base + component_of k);
               }))
        order;
      let edge_of (edge : edge) : edge =
        match edge.target with
        | Node j -> { edge with target = Node index.(j) }
        | Leaf shape -> { edge with target = graph_of_shape q shape }
      in
      Array.iteri
        (fun (k : int) (i : int) ->
          let node = Dynarray.get q.nodes (base + k) in
          Dynarray.set q.nodes (base + k)
            (match g.nodes.(i) with
            | Object { edges; _ } -> { node with edges = Fields.map edge_of edges }
            | Closures (c, cs) -> { node with captured = captured_by_position edge_of (c :: cs) }))
        order;
      Node base
  | Bot
  | Arg _ ->
      Leaf shape

let rec find (q : quotient) (i : int) : int =
  let parent = Dynarray.get q.parent i in
  if Int.equal parent i then i
  else
    let root = find q parent in
    Dynarray.set q.parent i root;
    root

let union (q : quotient) (i : int) (j : int) : int =
  let i = find q i in
  let j = find q j in
  if Int.equal i j then i
  else
    let root, child =
      if Dynarray.get q.size i >= Dynarray.get q.size j then (i, j) else (j, i)
    in
    Dynarray.set q.parent child root;
    Dynarray.set q.size root (Dynarray.get q.size root + Dynarray.get q.size child);
    Dynarray.set q.members root
      (List.merge Int.compare
         (Dynarray.get q.members root)
         (Dynarray.get q.members child));
    Dynarray.set q.class_sites root
      (Shape_and_sig.Sites.union
         (Dynarray.get q.class_sites root)
         (Dynarray.get q.class_sites child));
    Dynarray.set q.class_defs root
      (Defs.union (Dynarray.get q.class_defs root) (Dynarray.get q.class_defs child));
    q.unions <- q.unions + 1;
    root

let any_copy (q : quotient) (member : int) (o : T.offset) (any : int) : int =
  match Fields.find_opt o (Dynarray.get q.copies member) with
  | Some copy -> copy
  | None ->
      let node = Dynarray.get q.nodes any in
      match graph_of_shape q node.original_shape with
      | Node copy ->
          Dynarray.set q.copies member
            (Fields.add o copy (Dynarray.get q.copies member));
          copy
      | Leaf _ -> any

(* Whether [i] and [j] lie on one cycle of a graph value: for the target
 * [j] of an edge of [i], whether [j] reaches [i]. *)
let same_component (q : quotient) (i : int) (j : int) : bool =
  match ((Dynarray.get q.nodes i).component, (Dynarray.get q.nodes j).component) with
  | Some component_i, Some component_j -> Int.equal component_i component_j
  | (Some _ | None), _ -> false

let field_edges (q : quotient) (class_root : int) : edge list Fields.t =
  let members = Dynarray.get q.members class_root in
  let present =
    List.fold_left
      (fun present member ->
        Fields.fold
          (fun o edge present ->
            Fields.update o
              (fun edges -> Some (edge :: Option.value edges ~default:[]))
              present)
          (Dynarray.get q.nodes member).edges present)
      Fields.empty members
  in
  Fields.mapi
    (fun o reversed ->
      let reads_of_any =
        List.filter_map
          (fun member ->
            let edges = (Dynarray.get q.nodes member).edges in
            if Fields.mem o edges then None
            else
              Fields.find_opt T.Oany edges
              |> Option.map (fun (any : edge) ->
                     match any.target with
                     | Node any_node when not (same_component q member any_node) ->
                         { any with target = Node (any_copy q member o any_node) }
                     | Node _
                     | Leaf _ ->
                         any))
          members
      in
      List.rev_append reversed reads_of_any)
    present

let captured_edges (q : quotient) (class_root : int) : edge list Captured.t =
  List.fold_right
    (fun member captured ->
      Captured.fold
        (fun key edge captured ->
          Captured.update key
            (fun edges -> Some (edge :: Option.value edges ~default:[]))
            captured)
        (Dynarray.get q.nodes member).captured captured)
    (Dynarray.get q.members class_root)
    Captured.empty

let is_object (q : quotient) (i : int) : bool =
  Defs.is_empty (Dynarray.get q.nodes i).defs

let position_nodes (q : quotient) (edges : edge list) : int list =
  let nodes =
    List.filter_map
      (fun (edge : edge) ->
        match edge.target with
        | Node i -> Some i
        | Leaf _ -> None)
      edges
  in
  match List.filter (is_object q) nodes with
  | [] -> nodes
  | objects -> objects

let shares_site (q : quotient) (i : int) (j : int) : bool =
  (not
     (Shape_and_sig.Sites.disjoint
        (Dynarray.get q.class_sites i)
        (Dynarray.get q.class_sites j)))
  || not (Defs.disjoint (Dynarray.get q.class_defs i) (Dynarray.get q.class_defs j))

let class_edges (q : quotient) (class_root : int) : edge list list =
  if is_object q class_root then
    Fields.fold (fun _ edges all -> edges :: all) (field_edges q class_root) []
    |> List.rev
  else
    Captured.fold (fun _ edges all -> edges :: all) (captured_edges q class_root) []
    |> List.rev

let topmost_changed (q : quotient) (path : ancestor_class list) : int option =
  List.fold_left
    (fun topmost (ancestor_class : ancestor_class) ->
      let root = find q ancestor_class.class_root in
      if
        Int.equal root ancestor_class.class_root
        && Int.equal (Dynarray.get q.size root) ancestor_class.size_at_visit
      then topmost
      else Some ancestor_class.depth)
    None path

let rec close_class (q : quotient) (ancestors : ancestor_class list) (member : int) :
    int option =
  let class_root = find q member in
  let depth =
    match ancestors with
    | [] -> 0
    | (ancestor_class : ancestor_class) :: _ -> ancestor_class.depth + 1
  in
  let path =
    { class_root; size_at_visit = Dynarray.get q.size class_root; depth } :: ancestors
  in
  let first_visit = not (Dynarray.get q.visited class_root) in
  Dynarray.set q.visited class_root true;
  let rec close_fields (fields : edge list list) : int option =
    match fields with
    | [] -> None
    | edges :: rest -> (
        match position_nodes q edges with
        | [] -> close_fields rest
        | first :: others -> (
            let unions = q.unions in
            let x = find q (List.fold_left (union q) first others) in
            match
              if q.unions > unions then topmost_changed q path else None
            with
            | Some _ as restart -> restart
            | None -> (
                if first_visit then
                  Dynarray.set q.incoming x (Dynarray.get q.incoming x @ edges);
                if List.exists (fun (ancestor_class : ancestor_class) -> Int.equal ancestor_class.class_root x) path
                then (
                  Dynarray.set q.back_edge_target x true;
                  close_fields rest)
                else
                  match
                    List.filter
                      (fun (ancestor_class : ancestor_class) ->
                        shares_site q x ancestor_class.class_root)
                      path
                  with
                  | [] -> (
                      match close_class q path x with
                      | None -> close_fields rest
                      | Some _ as restart -> restart)
                  | hits ->
                      ignore
                        (List.fold_left
                           (fun x (ancestor_class : ancestor_class) -> union q x ancestor_class.class_root)
                           x hits);
                      topmost_changed q path)))
  in
  match close_fields (class_edges q class_root) with
  | Some restart when Int.equal restart depth -> close_class q ancestors class_root
  | outcome -> outcome

let rec close (q : quotient) (roots : edge list) : unit =
  let unions = q.unions in
  Dynarray.iteri (fun i _ -> Dynarray.set q.back_edge_target i false) q.back_edge_target;
  Dynarray.iteri (fun i _ -> Dynarray.set q.incoming i []) q.incoming;
  Dynarray.iteri (fun i _ -> Dynarray.set q.visited i false) q.visited;
  (match position_nodes q roots with
  | [] -> ()
  | first :: others ->
      let root = find q (List.fold_left (union q) first others) in
      Dynarray.set q.incoming root roots;
      ignore (close_class q [] root));
  if q.unions > unions then close q roots

let joined_xtaint ~(traces : T.kept_traces) (edges : edge list) : Xtaint.t =
  match edges with
  | [] -> `None
  | first :: rest ->
      List.fold_left
        (fun xtaint (edge : edge) -> Xtaint.union ~traces xtaint edge.xtaint)
        first.xtaint rest

let read_on_edge ~lang ~(traces : T.kept_traces) (q : quotient) (o : T.offset)
    (edge : edge) : [ `Kept | `Cell of edge | `Whole of Taints.t ] =
  match (edge.xtaint, edge.target) with
  | `Tainted taints, Node i ->
      let edges = (Dynarray.get q.nodes i).edges in
      if Fields.mem o edges || Fields.mem T.Oany edges then `Kept
      else `Whole taints
  | `Tainted taints, Leaf (Arg (arg, offsets)) -> (
      match find_in_arg ~lang ~traces ~taints [ o ] arg offsets with
      | Some (Cell (xtaint, shape)) -> `Cell { xtaint; target = Leaf shape }
      | None -> `Whole taints)
  | `Tainted taints, Leaf (Bot | Obj _ | Graph _ | Fun _) -> `Whole taints
  | (`None | `Clean), _ -> `Kept

(* The result of one fold: its nodes, and for each class the targets built
 * for it, by the context they were built in, so that a class reached again
 * in one context shares its node. The context is the derived edges, the
 * taint of the cell that reaches the class, the carried taints, and the
 * classes on the path above it that lie in its strongly connected
 * [component] of the classes, with their nodes: the only classes of the
 * path that the nodes built for the class can refer to, since a class they
 * refer to reaches it. Created by 'join_folded_by_site' for one fold and owned by
 * the domain that runs it. *)
type fold_memo = {
  result : Shape_and_sig.Shape.node Dynarray.t;
  component : int array;
  emitted :
    ((int * int) list * edge list * Xtaint.t * carried_taint list * target) list
    array;
}

let equal_derived (edges1 : edge list) (edges2 : edge list) : bool =
  List.equal
    (fun (edge1 : edge) (edge2 : edge) ->
      Xtaint.equal_with_guards edge1.xtaint edge2.xtaint
      &&
      match (edge1.target, edge2.target) with
      | Node i, Node j -> Int.equal i j
      | Leaf shape1, Leaf shape2 -> equal_shape_with_guards shape1 shape2
      | Node _, Leaf _
      | Leaf _, Node _ ->
          false)
    edges1 edges2

let equal_carried_taints (carried1 : carried_taint list)
    (carried2 : carried_taint list) : bool =
  List.equal
    (fun (carried_taint1 : carried_taint) (carried_taint2 : carried_taint) ->
      Taints.equal_with_guards carried_taint1.object_taints
        carried_taint2.object_taints
      && List.equal T.equal_offset carried_taint1.offset carried_taint2.offset
      && Taints.equal_with_guards carried_taint1.carry carried_taint2.carry)
    carried1 carried2

(* The cell that [edges] of one position give: a class on [path] is an edge
 * to its node there, as a back reference to it. *)
let rec cell_of_edges ~lang ~(traces : T.kept_traces) (q : quotient)
    (fold_memo : fold_memo) (path : (int * int) list) ~(derived : edge list)
    (carried_taints : carried_taint list) (edges : edge list) : edge option =
  let xtaint = joined_xtaint ~traces edges in
  match position_nodes q edges with
  | first :: _ -> (
      let x = find q first in
      match
        List.find_map
          (fun ((class_root, index) : int * int) ->
            if Int.equal class_root x then Some index else None)
          path
      with
      | Some index -> Some (edge_of_join xtaint (Node index))
      | None when not (is_object q x) ->
          Some (edge_of_join xtaint (closures_of_class ~lang ~traces q fold_memo path x))
      | None -> (
          match
            object_of_class ~lang ~traces q fold_memo path x ~derived ~xtaint
              carried_taints
          with
          | Leaf Bot when not (Xtaint.is_tainted xtaint) -> None
          | target -> Some (edge_of_join xtaint target)))
  | [] -> (
      let shape =
        List.fold_left
          (fun shape (edge : edge) ->
            match edge.target with
            | Leaf leaf -> unify_shape ~lang ~traces shape leaf
            | Node _ -> shape)
          Bot edges
      in
      match (cell_of_join xtaint shape, carried_taints) with
      | Cell (`Clean, _), _ :: _ ->
          let taints =
            carried_taints
            |> List.filter (fun (carried_taint : carried_taint) ->
                   not (Taints.equal carried_taint.object_taints carried_taint.carry))
            |> List.fold_left
                 (fun taints (carried_taint : carried_taint) ->
                   Taints.union ~traces taints
                     (fix_poly_taint_with_offset ~lang ~traces carried_taint.offset
                        carried_taint.object_taints))
                 Taints.empty
          in
          if Taints.is_empty taints then None
          else Some { xtaint = `Tainted taints; target = Leaf Bot }
      | Cell (xtaint, shape), _ -> Some { xtaint; target = Leaf shape })

and object_of_class ~lang ~(traces : T.kept_traces) (q : quotient)
    (fold_memo : fold_memo) (path : (int * int) list) (x : int)
    ~(derived : edge list) ~(xtaint : Xtaint.t)
    (carried_taints : carried_taint list) : target =
  let component_path =
    List.filter
      (fun ((class_root, _) : int * int) ->
        Int.equal fold_memo.component.(class_root) fold_memo.component.(x))
      path
  in
  match
    List.find_opt
      (fun ((component_path', derived', xtaint', carried_taints', _) :
             (int * int) list * edge list * Xtaint.t * carried_taint list * target) ->
        List.equal
          (fun ((class1, index1) : int * int) ((class2, index2) : int * int) ->
            Int.equal class1 class2 && Int.equal index1 index2)
          component_path component_path'
        && equal_derived derived derived'
        && Xtaint.equal_with_guards xtaint xtaint'
        && equal_carried_taints carried_taints carried_taints')
      fold_memo.emitted.(x)
  with
  | Some (_, _, _, _, target) -> target
  | None ->
      let index = Dynarray.length fold_memo.result in
      Dynarray.add_last fold_memo.result empty_object;
      let incoming = Dynarray.get q.incoming x @ derived in
      let carry = Xtaint.to_taints xtaint in
      let inherited =
        match xtaint with
        | `Tainted taints ->
            List.map (fun (carried_taint : carried_taint) -> { carried_taint with carry = taints }) carried_taints
        | `None
        | `Clean ->
            carried_taints
      in
      let path' = (x, index) :: path in
      let field_map = field_edges q x in
      let fields =
        Fields.fold
          (fun o edges fields ->
            let reads = List.map (read_on_edge ~lang ~traces q o) incoming in
            let derived =
              List.filter_map
                (function
                  | `Cell edge -> Some edge
                  | `Kept
                  | `Whole _ ->
                      None)
                reads
            in
            let carried_taints =
              List.filter_map
                (function
                  | `Whole object_taints -> Some { object_taints; offset = [ o ]; carry }
                  | `Kept
                  | `Cell _ ->
                      None)
                reads
              @ List.map
                  (fun (carried_taint : carried_taint) ->
                    { carried_taint with offset = carried_taint.offset @ [ o ] })
                  inherited
            in
            match
              cell_of_edges ~lang ~traces q fold_memo path' ~derived carried_taints
                (edges @ derived)
            with
            | Some edge -> Fields.add o edge fields
            | None -> fields)
          field_map Fields.empty
      in
      let target =
        if Fields.is_empty fields && not (Fields.is_empty field_map) then Leaf Bot
        else (
          Dynarray.set fold_memo.result index
            (Object
               {
                 sites = Dynarray.get q.class_sites x;
                 summary =
                   Dynarray.get q.back_edge_target x
                   || List.exists
                        (fun member -> (Dynarray.get q.nodes member).summary)
                        (Dynarray.get q.members x);
                 edges = fields;
               });
          Node index)
      in
      fold_memo.emitted.(x) <-
        (component_path, derived, xtaint, carried_taints, target) :: fold_memo.emitted.(x);
      target

and closures_of_class ~lang ~(traces : T.kept_traces) (q : quotient)
    (fold_memo : fold_memo) (path : (int * int) list) (x : int) : target =
  let index = Dynarray.length fold_memo.result in
  Dynarray.add_last fold_memo.result empty_object;
  let path = (x, index) :: path in
  let captured = captured_edges q x in
  let edge_of_cell (Cell (xtaint, shape) : cell) : edge =
    { xtaint; target = target_of_shape fold_memo.result shape }
  in
  let join_closures (first : closure) (others : closure list) : graph_closure =
    let sig_ =
      List.fold_left
        (fun (sig_ : Signature.t) (closure : closure) ->
          if phys_equal closure.sig_ sig_ then sig_
          else
            {
              sig_ with
              Signature.effects =
                Effects.union ~traces sig_.Signature.effects
                  closure.sig_.Signature.effects;
            })
        first.sig_ others
    in
    let env =
      List.mapi
        (fun position ((var, entry) : IL.name * env_entry) ->
          match (entry, Captured.find_opt (first.def, position) captured) with
          | Val cell, Some edges -> (
              match cell_of_edges ~lang ~traces q fold_memo path ~derived:[] [] edges with
              | Some edge -> (var, (Val edge : graph_env_entry))
              | None -> (var, Val (edge_of_cell cell)))
          | Val cell, None -> (var, Val (edge_of_cell cell))
          | Ref lval, _ -> (var, Ref lval))
        first.env
    in
    { def = first.def; sig_; env }
  in
  let closures =
    Dynarray.get q.members x
    |> List.concat_map (fun member ->
           match unfold (Dynarray.get q.nodes member).original_shape with
           | Fun (c, cs) -> c :: cs
           | Bot
           | Obj _
           | Graph _
           | Arg _ ->
               [])
    |> List.stable_sort (fun (c1 : closure) (c2 : closure) ->
           Function_id.compare c1.def c2.def)
  in
  let groups =
    List.fold_right
      (fun (closure : closure) (groups : closure list list) ->
        match groups with
        | (next :: _ as group) :: rest when Function_id.equal next.def closure.def ->
            (closure :: group) :: rest
        | _ -> [ closure ] :: groups)
      closures []
  in
  match
    List.filter_map
      (function
        | first :: others -> Some (join_closures first others)
        | [] -> None)
      groups
  with
  | first :: others ->
      Dynarray.set fold_memo.result index (Closures (first, others));
      Node index
  | [] -> Leaf Bot

let join_folded_by_site ~lang ~(traces : T.kept_traces) (previous : cell option)
    (computed : cell) : cell =
  let has_node (Cell (_, shape) : cell) : bool =
    match shape with
    | Obj _
    | Graph _
    | Fun _ ->
        true
    | Bot
    | Arg _ ->
        false
  in
  let holds_graph (Cell (_, shape) : cell) : bool =
    match shape with
    | Graph _ -> true
    | Bot
    | Obj _
    | Arg _
    | Fun _ ->
        false
  in
  match previous with
  | Some previous when phys_equal previous computed -> computed
  (* Bisimilar values: the fold of equal values is the value. *)
  | Some previous
    when (holds_graph previous || holds_graph computed)
         && equal_cell_with_guards previous computed ->
      computed
  | Some previous when not (has_node previous || has_node computed) ->
      unify_cell ~lang ~traces previous computed
  | None when not (has_node computed) -> computed
  | Some _
  | None -> (
      let q = new_quotient () in
      let roots =
        Option.to_list previous @ [ computed ]
        |> List.map (fun (Cell (xtaint, shape)) ->
               { xtaint; target = graph_of_shape q shape })
      in
      close q roots;
      if Option.is_none previous && Int.equal q.unions 0 then computed
      else
        let count = Dynarray.length q.nodes in
        let class_successors =
          Array.init count (fun (i : int) ->
              if not (Int.equal (find q i) i) then []
              else
                List.filter_map
                  (fun (edges : edge list) ->
                    match position_nodes q edges with
                    | first :: _ ->
                        let j = find q first in
                        if j < count then Some j else None
                    | [] -> None)
                  (class_edges q i))
        in
        let fold_memo =
          {
            result = Dynarray.create ();
            component =
              (let _, component_of =
                 Shape_and_sig.Adjacency_components.scc class_successors
               in
               Array.init count component_of);
            emitted = Array.make count [];
          }
        in
        match cell_of_edges ~lang ~traces q fold_memo [] ~derived:[] [] roots with
        | Some edge ->
            Cell
              ( edge.xtaint,
                minimise ~traces (Dynarray.to_array fold_memo.result) edge.target )
        | None -> Cell (joined_xtaint ~traces roots, Bot))

(*********************************************************)
(* Enumerate leaf cells, summary object cells and tainted object cells, with
 * taints and shapes *)
(*********************************************************)

let rec enum_in_cell cell : (T.offset list * Taints.t * shape) Seq.t =
  let (Cell (xtaint, shape)) = cell in
  enum_in_shape (Xtaint.to_taints xtaint) shape

and enum_in_shape (taints : Taints.t) (shape : shape) :
    (T.offset list * Taints.t * shape) Seq.t =
  let own_taints =
    if Taints.is_empty taints then Seq.empty else Seq.return ([], taints, Bot)
  in
  match shape with
  | Bot
  | Arg _
  | Fun _
  | Obj { summary = true; _ } ->
      Seq.return ([], taints, shape)
  | Obj { fields; _ } -> Seq.append own_taints (enum_in_obj fields)
  | Graph _ -> (
      (* The value that starts at a node is yielded whole at a summary node or
       * a closure set; every cycle passes through a summary node
       * (INVARIANT(graph).5), so the walk ends. *)
      match unfold shape with
      | Obj { summary = false; fields; _ } -> Seq.append own_taints (enum_in_obj fields)
      | Obj { summary = true; _ }
      | Bot
      | Graph _
      | Arg _
      | Fun _ ->
          Seq.return ([], taints, shape))

and enum_in_obj obj =
  obj
  |> Fields.to_seq
  |> Seq.map (fun (o, cell) ->
         enum_in_cell cell
         |> Seq.map (fun (offset, taints, shape) -> (o :: offset, taints, shape)))
  |> Seq.concat
