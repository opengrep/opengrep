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

(* Temporarily breaks INVARIANT(cell) by initializing a field with the shape
 * 'cell<0>(_|_)', but right away the field should be either tainted or cleaned.
 * The caller must restore the invariant. *)
let internal_UNSAFE_find_offset_in_obj o obj =
  match Fields.find_opt o obj with
  | Some _ -> (o, obj)
  | None ->
      let num_fields = Fields.cardinal obj in
      if num_fields <= Limits_semgrep.taint_MAX_OBJ_FIELDS then
        let obj = Fields.add o cell_none_bot obj in
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
  | Bot
  | Rec _ ->
      false
  | Arg _
  | Fun _ ->
      true
  | Obj { fields; _ } ->
      Fields.exists (fun _ cell -> cell_has_relevant_content cell) fields

and cell_has_relevant_content (Cell (xtaint, shape)) =
  Xtaint.is_tainted xtaint || shape_has_relevant_content shape

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

(* A field offset of function type is a method, unless naming found it to be
 * a data field of the receiver's type, which holds a function. *)
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
    ~(merge : T.trace_merge) offset
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
             |> Taints.map_taint ~merge (fun (taint : T.taint) ->
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

(* A read of [offset] on a parameter's shape 'Arg (arg, base_offsets)', whose
 * value carries [taints]: the polymorphic taints extended by [offset], under
 * the shape extended the same way. 'None' when [offset] is a method call. *)
let find_in_arg ?max ~lang ~(merge : T.trace_merge) ~taints offset arg
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
    let taints = fix_poly_taint_with_offset ?max ~lang ~merge offset taints in
    Some (Cell (Xtaint.of_taints taints, Arg (arg, extended)))

(*********************************************************)
(* Back references *)
(*********************************************************)

let obj_or_bot ~(sites : Shape_and_sig.Sites.t) ~(summary : bool) (fields : obj)
    : shape =
  if Fields.is_empty fields then Bot else Obj { sites; summary; fields }

let written_obj ~(write : T.call_loc) ~(depth : int) : shape =
  Obj
    {
      sites = Shape_and_sig.Sites.singleton (Shape_and_sig.Written_at (write, depth));
      summary = false;
      fields = Fields.empty;
    }

let is_summary (shape : shape) : bool =
  match shape with
  | Obj { summary; _ } -> summary
  | Bot
  | Rec _
  | Arg _
  | Fun _ ->
      false

let map_fields (f : cell -> cell) (fields : obj) : obj =
  Fields.fold
    (fun o cell acc ->
      let cell' = f cell in
      if phys_equal cell' cell then acc else Fields.add o cell' acc)
    fields fields

let map_cell_shape (f : shape -> shape) (Cell (xtaint, shape) as cell : cell) :
    cell =
  let shape' = f shape in
  if phys_equal shape' shape then cell else Cell (xtaint, shape')

let map_captured_cells (f : cell -> cell) (closure : closure) : closure =
  let env =
    List.map
      (fun ((var, entry) as binding : IL.name * env_entry) ->
        match entry with
        | Val cell ->
            let cell' = f cell in
            if phys_equal cell' cell then binding else (var, Val cell')
        | Ref _ -> binding)
      closure.env
  in
  if List.for_all2 phys_equal env closure.env then closure else { closure with env }

(* [shape] with each back reference that leaves it replaced by [outer j
 * depth], where [j] counts the objects and closure sets enclosing [shape]
 * that the reference skips and [depth] the objects and closure sets of
 * [shape] around the reference. *)
let rebind_free ~(outer : int -> int -> shape) (shape : shape) : shape =
  let rec rebind (depth : int) (shape : shape) : shape =
    match shape with
    | Rec n when n >= depth -> outer (n - depth) depth
    | Bot
    | Rec _
    | Arg _ ->
        shape
    | Fun (c, cs) ->
        let c', cs' =
          Shape_and_sig.map_closures
            (map_captured_cells (map_cell_shape (rebind (depth + 1))))
            (c, cs)
        in
        if phys_equal c' c && phys_equal cs' cs then shape else Fun (c', cs')
    | Obj ({ fields; _ } as node) ->
        let fields' = map_fields (map_cell_shape (rebind (depth + 1))) fields in
        if phys_equal fields' fields then shape
        else Obj { node with fields = fields' }
  in
  rebind 0 shape

let closures_of_fun (c : closure) (cs : closure list) : closure * closure list =
  let fun_shape = Fun (c, cs) in
  Shape_and_sig.map_closures
    (map_captured_cells
       (map_cell_shape
          (rebind_free ~outer:(fun (levels_above : int) (depth : int) ->
               if Int.equal levels_above 0 then fun_shape
               else Rec (depth + levels_above)))))
    (c, cs)

(* [shape], placed [levels] objects deeper than the objects it was found
 * under. *)
let shift_free ~(levels : int) (shape : shape) : shape =
  rebind_free ~outer:(fun j depth -> Rec (depth + j + levels)) shape

(* [shape], found under [enclosing] (nearest first), with each back
 * reference that leaves it replaced by the object it refers to, itself
 * closed: a regular tree unrolled once, which no longer depends on its
 * position. Only a summary is referred to. *)
let rec close_shape (enclosing : shape list) (shape : shape) : shape =
  if not (List.exists is_summary enclosing) then shape
  else
    let targets =
      enclosing
      |> List.mapi (fun i target ->
             lazy (close_shape (List.drop (i + 1) enclosing) target))
      |> Array.of_list
    in
    rebind_free ~outer:(fun j _depth -> Lazy.force targets.(j)) shape

let close_cell (enclosing : shape list) (cell : cell) : cell =
  map_cell_shape (close_shape enclosing) cell

(*********************************************************)
(* Unification (merging shapes) *)
(*********************************************************)

(* One side of a unification: the objects that enclose the position, nearest
 * first, and whether its back references count the levels of the result,
 * which stops once a back reference of this side was followed. *)
type side = { enclosing : shape list; aligned : bool }

(* [levels] holds, for each object enclosing the result's position, the
 * objects of both sides it joins. After a back reference was followed
 * ([unrolled]), a pair of objects met again, one of them a summary, is a
 * cycle of the result. *)
type join = {
  left : side;
  right : side;
  levels : (shape option * shape option) list;
  unrolled : bool;
}

let closed_join : join =
  {
    left = { enclosing = []; aligned = true };
    right = { enclosing = []; aligned = true };
    levels = [];
    unrolled = false;
  }

let flip (join : join) : join =
  {
    left = join.right;
    right = join.left;
    levels = List.map (fun (l, r) -> (r, l)) join.levels;
    unrolled = join.unrolled;
  }

let enter (join : join) (left : shape option) (right : shape option) : join =
  let push (side : side) (node : shape option) : side =
    match node with
    | Some node -> { side with enclosing = node :: side.enclosing }
    | None -> side
  in
  {
    join with
    left = push join.left left;
    right = push join.right right;
    levels = (left, right) :: join.levels;
  }

let both_aligned (join : join) : bool = join.left.aligned && join.right.aligned

let follow (side : side) (n : int) : (shape * side) option =
  match List.nth_opt side.enclosing n with
  | Some target ->
      Some (target, { enclosing = List.drop (n + 1) side.enclosing; aligned = false })
  | None -> None

(* [shape] of the side that [pick] selects from a level, moved to the
 * result's position: a back reference refers to the nearest level that joins
 * its object, or else to a closed copy of that object. *)
let relocate (join : join) ~(pick : shape option * shape option -> shape option)
    (side : side) (shape : shape) : shape =
  if side.aligned then shape
  else
    rebind_free
      ~outer:(fun j depth ->
        match List.nth_opt side.enclosing j with
        | None -> Rec (depth + j)
        | Some target -> (
            match
              List.find_index
                (fun level ->
                  match pick level with
                  | Some node -> phys_equal node target
                  | None -> false)
                join.levels
            with
            | Some m -> Rec (depth + m)
            | None -> close_shape (List.drop (j + 1) side.enclosing) target))
      shape

let relocate_left (join : join) (shape : shape) : shape =
  relocate join ~pick:fst join.left shape

let relocate_right (join : join) (shape : shape) : shape =
  relocate join ~pick:snd join.right shape

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
 * one pass, so a join that changes nothing allocates nothing. *)
let rec replace_clean_leaves ~lang ~(merge : T.trace_merge) ~offset ~carry ~leaf
    (Cell (xtaint, shape) as cell) =
  match (xtaint, shape) with
  | `Clean, _ ->
      if Taints.equal leaf carry then None
      else
        let taints = fix_poly_taint_with_offset ~lang ~merge offset leaf in
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
              replace_clean_leaves ~lang ~merge ~offset:(offset @ [ o ]) ~carry ~leaf c
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
  | _, (Bot | Rec _ | Arg _ | Fun _) -> Some cell

let cell_of_join (xtaint : Xtaint.t) (shape : shape) : cell =
  match (xtaint, shape) with
  (* Restore INVARIANT(cell).2: 'Xtaint.union' gives 'Clean ∪ None = Clean'
   * while 'unify_shape' gives 'Bot ∪ shape = shape', so unifying
   * 'Cell(Clean, Bot)' with 'Cell(None, Obj _)' would produce
   * 'Cell(Clean, Obj _)'. The 'Clean' claim only held on one side, and a
   * join must not hide the taint recorded under the other side's shape
   * ('find_in_cell_w_carry' stops at a 'Clean' cell). *)
  | `Clean, (Obj _ | Rec _ | Arg _ | Fun _) -> Cell (`None, shape)
  | ( (`Clean | `None | `Tainted _),
      (Bot | Obj _ | Rec _ | Arg _ | Fun _) ) ->
      Cell (xtaint, shape)

let rec unify_cell_in ~lang ~(merge : T.trace_merge) (join : join) cell1 cell2 =
  if phys_equal cell1 cell2 && both_aligned join then cell1
  else
  let (Cell (xtaint1, shape1)) = cell1 in
  let (Cell (xtaint2, shape2)) = cell2 in
  (* TODO: Apply 'Flag_semgrep.max_taint_set_size' here too ? *)
  let xtaint = Xtaint.union ~merge xtaint1 xtaint2 in
  let carry = Xtaint.to_taints xtaint in
  let shape =
    unify_shape_in ~lang ~merge join
      ~process1:(fun join ~other shape ->
        taint_untracked_fields ~lang ~merge join ~carry ~other_xtaint:xtaint2
          ~other shape)
      ~process2:(fun join ~other shape ->
        taint_untracked_fields ~lang ~merge (flip join) ~carry
          ~other_xtaint:xtaint1 ~other shape)
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
and taint_untracked_fields ~lang ~(merge : T.trace_merge) (join : join) ~carry
    ~other_xtaint:(xtaint : Xtaint.t) ~other:(other_shape : shape) shape =
  match shape with
  | Obj ({ fields = obj; _ } as node) ->
      let whole =
        match xtaint with
        | `Tainted taints -> Some taints
        | `None
        | `Clean ->
            None
      in
      let other_node =
        match other_shape with
        | Obj _ -> Some other_shape
        | Bot
        | Rec _
        | Arg _
        | Fun _ ->
            None
      in
      let read_on_other o =
        match (other_shape, xtaint) with
        | Obj { fields = other_obj; _ }, _ when Fields.mem o other_obj -> `Tracked
        | Obj { fields = other_obj; _ }, _ -> (
            match Fields.find_opt T.Oany other_obj with
            | Some any_cell -> `Cell any_cell
            | None -> `Whole)
        | Arg (arg, base_offsets), `Tainted taints -> (
            match find_in_arg ~lang ~merge ~taints [ o ] arg base_offsets with
            | Some cell -> `Cell cell
            | None -> `Whole)
        | Arg _, (`None | `Clean)
        | (Bot | Rec _ | Fun _), _ ->
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
              | `Cell cell, _ ->
                  Some
                    (unify_cell_in ~lang ~merge
                       (flip (enter join (Some shape) other_node))
                       cell field)
              | `Whole, Some leaf ->
                  replace_clean_leaves ~lang ~merge ~offset:[ o ] ~carry ~leaf
                    field
            with
            | Some field' when phys_equal field' field -> acc
            | Some field' -> Fields.add o field' acc
            | None -> Fields.remove o acc)
          obj obj
      in
      if phys_equal obj' obj then shape
      else obj_or_bot ~sites:node.sites ~summary:node.summary obj'
  | Rec n -> (
      match (xtaint, follow join.left n) with
      | `Tainted leaf, Some (target, side) -> (
          let closed = close_shape side.enclosing target in
          match
            replace_clean_leaves ~lang ~merge ~offset:[] ~carry ~leaf
              (Cell (`None, closed))
          with
          | Some (Cell (_, replaced)) when phys_equal replaced closed -> shape
          | Some (Cell (_, replaced)) -> replaced
          | None -> Bot)
      | (`None | `Clean), _
      | `Tainted _, None ->
          shape)
  | Bot
  | Arg _
  | Fun _ ->
      shape

(* [process1] and [process2] give an object of one side as the join with the
 * [other] side's shape must see it ('taint_untracked_fields'); a level
 * records the objects before that, so an object met again through a back
 * reference is recognised. *)
and unify_shape_in ~lang ~(merge : T.trace_merge) (join : join)
    ~(process1 : join -> other:shape -> shape -> shape)
    ~(process2 : join -> other:shape -> shape -> shape) shape1 shape2 =
  if phys_equal shape1 shape2 && both_aligned join then shape1
  else
  match (shape1, shape2) with
  | Bot, shape ->
      (* 'Bot' acts like a do-not-care. *)
      relocate_right join (process2 join ~other:shape1 shape)
  | shape, Bot -> relocate_left join (process1 join ~other:shape2 shape)
  | Rec n1, Rec n2 when both_aligned join && Int.equal n1 n2 -> shape1
  | Fun (c1, cs1), Fun (c2, cs2) ->
      let c, cs = unify_closure_sets ~lang ~merge (c1, cs1) (c2, cs2) in
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
  | Arg _, ((Obj _ | Rec _) as obj) ->
      relocate_right join (process2 join ~other:shape1 obj)
  | ((Obj _ | Rec _) as obj), Arg _ ->
      relocate_left join (process1 join ~other:shape2 obj)
  | Arg _, (Fun _ as func)
  | (Fun _ as func), Arg _ ->
      func
  | Rec n, _ -> (
      match follow join.left n with
      | Some (target, left) ->
          unify_shape_in ~lang ~merge
            { join with left; unrolled = true }
            ~process1 ~process2 target shape2
      | None -> shape1)
  | _, Rec n -> (
      match follow join.right n with
      | Some (target, right) ->
          unify_shape_in ~lang ~merge
            { join with right; unrolled = true }
            ~process1 ~process2 shape1 target
      | None -> shape2)
  | Obj _, Obj _ -> (
      let cycle =
        if join.unrolled && (is_summary shape1 || is_summary shape2) then
          List.find_index
            (fun level ->
              match level with
              | Some node1, Some node2 ->
                  phys_equal node1 shape1 && phys_equal node2 shape2
              | _ -> false)
            join.levels
        else None
      in
      match cycle with
      | Some m -> Rec m
      | None -> (
          match
            ( process1 join ~other:shape2 shape1,
              process2 join ~other:shape1 shape2 )
          with
          | ( (Obj { sites = sites1; summary = summary1; fields = obj1 } as
               processed1),
              (Obj { sites = sites2; summary = summary2; fields = obj2 } as
               processed2) ) ->
              let fields =
                unify_obj_in ~lang ~merge
                  (enter join (Some shape1) (Some shape2))
                  obj1 obj2
              in
              let absorbs (sites : Shape_and_sig.Sites.t) (summary : bool)
                  (obj : obj) (other_sites : Shape_and_sig.Sites.t)
                  (other_summary : bool) : bool =
                phys_equal fields obj
                && (summary || not other_summary)
                && (phys_equal sites other_sites
                   || Shape_and_sig.Sites.subset other_sites sites)
              in
              if absorbs sites1 summary1 obj1 sites2 summary2 then processed1
              else if absorbs sites2 summary2 obj2 sites1 summary1 then
                processed2
              else
                Obj
                  {
                    sites = Shape_and_sig.Sites.union sites1 sites2;
                    summary = summary1 || summary2;
                    fields;
                  }
          | Bot, shape -> relocate_right join shape
          | shape, _ -> relocate_left join shape))
  | Obj _, Fun _
  | Fun _, Obj _ ->
      (* This could be caused by bugs in Semgrep, or by an if-then-else in a
       * dynamic language like Python where the same variable has different types
       * in each branch, or by unsafe casts in C/C++ perhaps. *)
      Log.err (fun m ->
          m "Trying to unify incompatible shapes: %s ~ %s" (show_shape shape1)
            (show_shape shape2));
      (* Not sure what to do here, so we just pick one arbitrary shape. *)
      relocate_left join (process1 join ~other:shape2 shape1)

and unify_obj_in ~lang ~(merge : T.trace_merge) (join : join) obj1 obj2 =
  (* THINK: Apply taint_MAX_OBJ_FIELDS limit ? *)
  if both_aligned join then
    if Fields.is_empty obj1 then obj2
    else
      Fields.fold
        (fun o cell2 obj ->
          Fields.update o
            (function
              | None -> Some cell2
              | Some cell1 -> Some (unify_cell_in ~lang ~merge join cell1 cell2))
            obj)
        obj2 obj1
  else
    Fields.merge
      (fun _ x y ->
        match (x, y) with
        | Some x, Some y -> Some (unify_cell_in ~lang ~merge join x y)
        | Some x, None -> Some (map_cell_shape (relocate_left join) x)
        | None, Some y -> Some (map_cell_shape (relocate_right join) y)
        | None, None -> None)
      obj1 obj2

and unify_closure ~lang ~(merge : T.trace_merge) (c1 : closure)
    (c2 : closure) : closure =
  if phys_equal c1 c2 then c1
  else
    {
      c1 with
      sig_ =
        {
          c1.sig_ with
          Signature.effects =
            Effects.union ~merge c1.sig_.Signature.effects
              c2.sig_.Signature.effects;
        };
      env = unify_env ~lang ~merge c1.env c2.env;
    }

and unify_closure_sets ~lang ~(merge : T.trace_merge)
    ((c1, cs1) : closure * closure list)
    ((c2, cs2) : closure * closure list) : closure * closure list =
  match Function_id.compare c1.def c2.def with
  | 0 -> (unify_closure ~lang ~merge c1 c2, unify_closures ~lang ~merge cs1 cs2)
  | n when n < 0 -> (c1, unify_closures ~lang ~merge cs1 (c2 :: cs2))
  | _ -> (c2, unify_closures ~lang ~merge (c1 :: cs1) cs2)

and unify_closures ~lang ~(merge : T.trace_merge) (cs1 : closure list)
    (cs2 : closure list) :
    closure list =
  match (cs1, cs2) with
  | [], cs
  | cs, [] ->
      cs
  | c1 :: rest1, c2 :: rest2 ->
      let c, cs = unify_closure_sets ~lang ~merge (c1, rest1) (c2, rest2) in
      c :: cs

(* Both environments belong to the same code, so they bind the same
 * variables in the same order. *)
and unify_env ~lang ~(merge : T.trace_merge) (env1 : env) (env2 : env) : env =
  List.map2
    (fun ((x, entry1) as binding1) (_, entry2) ->
      match (entry1, entry2) with
      | Val cell1, Val cell2 ->
          (x, Val (unify_cell_in ~lang ~merge closed_join cell1 cell2))
      | Ref _, _
      | Val _, Ref _ ->
          binding1)
    env1 env2

let unify_cell ~lang ~(merge : T.trace_merge) cell1 cell2 =
  unify_cell_in ~lang ~merge closed_join cell1 cell2

let unify_shape ~lang ~(merge : T.trace_merge) shape1 shape2 =
  let keep (_ : join) ~other:(_ : shape) (shape : shape) : shape = shape in
  unify_shape_in ~lang ~merge closed_join ~process1:keep ~process2:keep shape1
    shape2

let unify_obj ~lang ~(merge : T.trace_merge) obj1 obj2 =
  unify_obj_in ~lang ~merge closed_join obj1 obj2

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

let tuple_like_obj ~(site : T.call_loc) taints_and_shapes : shape =
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
  obj_or_bot
    ~sites:(Shape_and_sig.Sites.singleton (Shape_and_sig.Built_at site))
    ~summary:false obj

let record_or_dict_like_obj ~lang ~(merge : T.trace_merge) ~(site : T.call_loc)
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
               match shape with
               | Obj { fields = obj'; _ } ->
                   unify_obj ~lang ~merge obj
                     (map_fields (close_cell [ shape ]) obj')
               | Bot
               | Rec _
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
  obj_or_bot
    ~sites:(Shape_and_sig.Sites.singleton (Shape_and_sig.Built_at site))
    ~summary:false obj

(*********************************************************)
(* Collect/union all taints *)
(*********************************************************)

(* THINK: Generalize to "fold" ? *)
let rec gather_all_taints_in_cell_acc ~(merge : T.trace_merge) acc cell =
  let (Cell (xtaint, shape)) = cell in
  match xtaint with
  | `Clean ->
      (* Due to INVARIANT(cell) we can just stop here. *)
      acc
  | `None -> gather_all_taints_in_shape_acc ~merge acc shape
  | `Tainted taints ->
      gather_all_taints_in_shape_acc ~merge (Taints.union ~merge taints acc)
        shape

and gather_all_taints_in_shape_acc ~(merge : T.trace_merge) acc = function
  | Bot
  | Rec _ ->
      acc
  | Obj { fields; _ } -> gather_all_taints_in_obj_acc ~merge acc fields
  | Arg (arg, offsets) ->
      (* One [Shape_var] per alternative offset. *)
      List.fold_left
        (fun acc off ->
          let lval = { T.base = T.base_of_formal arg; offset = off } in
          let taint = T.taint_of_orig (T.Shape_var lval) in
          Taints.add_taint ~merge taint acc)
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

and gather_all_taints_in_obj_acc ~(merge : T.trace_merge) acc obj =
  Fields.fold
    (fun _ o_cell acc -> gather_all_taints_in_cell_acc ~merge acc o_cell)
    obj acc

let gather_all_taints_in_cell ~(merge : T.trace_merge) =
  gather_all_taints_in_cell_acc ~merge Taints.empty

let gather_all_taints_in_shape ~(merge : T.trace_merge) =
  gather_all_taints_in_shape_acc ~merge Taints.empty

let gather_all_taints_in_args_taints ~(merge : T.trace_merge)
    (args_taints : (Taint.taints * shape) IL.argument list) : Taint.taints =
  args_taints
  |> List.fold_left
       (fun acc arg ->
         match arg with
         | IL.Named (_, (_, shape))
         | IL.Unnamed (_, shape) ->
             gather_all_taints_in_shape ~merge shape |> Taints.union ~merge acc)
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
let rec truncate_cell ~(merge : T.trace_merge) ~budget
    ~(enclosing : shape list) cell : cell option =
  let (Cell (xtaint, shape)) = cell in
  match shape with
  | Bot
  | Rec _
  | Arg _
  | Fun _ ->
      Some cell
  | Obj ({ fields = obj; _ } as node) ->
      if budget <= 0 then (
        let deep = gather_all_taints_in_cell ~merge (close_cell enclosing cell) in
        if Taints.is_empty deep then None
        else Some (Cell (`Tainted deep, Bot)))
      else
        let obj' =
          Fields.filter_map
            (fun _o inner ->
              truncate_cell ~merge ~budget:(budget - 1)
                ~enclosing:(shape :: enclosing) inner)
            obj
        in
        let shape' = obj_or_bot ~sites:node.sites ~summary:node.summary obj' in
        (match (xtaint, shape') with
        (* Restore INVARIANT(cell).1 *)
        | `None, Bot -> None
        (* Restore INVARIANT(cell).2, see 'unify_cell'. *)
        | `Clean, (Obj _ | Rec _ | Arg _ | Fun _) -> Some (Cell (`None, shape'))
        | ( (`Clean | `None | `Tainted _),
            (Bot | Obj _ | Rec _ | Arg _ | Fun _) ) ->
            Some (Cell (xtaint, shape')))

(* Fast path for [truncate_shape]: [record_effects] truncates every effect
 * it records, and almost all shapes are nowhere near the cutoff, so don't
 * rebuild (reallocate) a shape that is already within budget. Short-circuits
 * via [Fields.exists]. *)
let rec cell_depth_exceeds ~budget (Cell (_xtaint, shape)) =
  shape_depth_exceeds ~budget shape

and shape_depth_exceeds ~budget shape =
  match shape with
  | Bot
  | Rec _
  | Arg _
  | Fun _ ->
      false
  | Obj { fields; _ } ->
      budget <= 0
      || Fields.exists
           (fun _o cell -> cell_depth_exceeds ~budget:(budget - 1) cell)
           fields

(* Widen [shape] to at most [max_depth] levels of [Obj] nesting;
 * see [truncate_cell]. *)
let truncate_shape ~(merge : T.trace_merge) ~max_depth shape =
  if max_depth < 1 then shape
  else
    match shape with
    | Bot
    | Rec _
    | Arg _
    | Fun _ ->
        shape
    | Obj ({ fields = obj; _ } as node) ->
        if not (shape_depth_exceeds ~budget:max_depth shape) then shape
        else
          let obj' =
            Fields.filter_map
              (fun _o cell ->
                truncate_cell ~merge ~budget:(max_depth - 1) ~enclosing:[ shape ]
                  cell)
              obj
          in
          obj_or_bot ~sites:node.sites ~summary:node.summary obj'

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
and bound_fun_shape ~(merge : T.trace_merge) ~levels (shape : shape) : shape =
  match shape with
  | Bot
  | Rec _
  | Arg _ ->
      shape
  | Fun (c, cs) ->
      if levels <= 0 then Bot
      else
        let bound_closure (closure : closure) : closure =
          let sig_ = closure.sig_ in
          let effects =
            Effects.map ~merge
              (map_effect_shapes
                 ~widen:(bound_fun_shape ~merge ~levels:(levels - 1)))
              sig_.Signature.effects
          in
          let env' =
            List_.map
              (fun ((x, entry) as binding) ->
                match entry with
                | Ref _ -> binding
                | Val (Cell (xtaint, inner)) ->
                    let inner' = bound_fun_shape ~merge ~levels:(levels - 1) inner in
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
            let inner' = bound_fun_shape ~merge ~levels inner in
            if phys_equal inner' inner then cell
            else (
              changed := true;
              Cell (xtaint, inner')))
          obj
      in
      if !changed then Obj { node with fields = obj' } else shape

let truncate_effect ~(merge : T.trace_merge) ~max_depth
    (eff : Shape_and_sig.Effect.t) :
    Shape_and_sig.Effect.t =
  (* The positions holding several results are not a level of a value:
   * each result keeps [max_depth] levels. *)
  let max_depth =
    match eff with
    | Shape_and_sig.Effect.ToReturn { several_results = true; _ }
      when max_depth >= 1 ->
        max_depth + 1
    | _ -> max_depth
  in
  let widen shape =
    truncate_shape ~merge ~max_depth shape
    |> bound_fun_shape ~merge ~levels:Limits_semgrep.taint_MAX_SIG_FUN_DEPTH
  in
  map_effect_shapes ~widen eff

(* Widen every shape stored in a signature's effects; the entry point used
 * by the interfile fold when a signature is (re-)stored. [Effects.map]
 * returns the set physically unchanged when [truncate_effect] is the
 * identity on every element, so the common case allocates nothing. *)
let truncate_signature ~(merge : T.trace_merge) ~max_depth (s : Signature.t) :
    Signature.t =
  let effects = Effects.map ~merge (truncate_effect ~merge ~max_depth) s.effects in
  if phys_equal effects s.effects then s else { s with effects }

(*********************************************************)
(* Find an offset *)
(*********************************************************)

let cell_read_of_find_result ?max ~lang ~(merge : T.trace_merge) res :
    cell option =
  match res with
  | `Found cell -> Some cell
  | `Clean -> None
  | `Not_found (taints, _shape, offset) ->
      let taints = fix_poly_taint_with_offset ?max ~lang ~merge offset taints in
      if Taints.is_empty taints then None
      else Some (Cell (`Tainted taints, Bot))

let rec find_in_cell_w_carry ?max ~lang ~(merge : T.trace_merge) ~taints
    ~(enclosing : shape list) offset cell =
  let (Cell (xtaint, shape)) = cell in
  match offset with
  | [] -> `Found (close_cell enclosing cell)
  | _ :: _ -> (
      match xtaint with
      | `Clean ->
          if shape <> Bot then
            Log.err (fun m ->
                m "BUG: Taint_shape.find_in_cell: INVARIANT(cell).2 is broken");
          `Clean
      | `None ->
          find_in_shape_w_carry ?max ~lang ~merge ~taints ~enclosing offset shape
      | `Tainted taints ->
          find_in_shape_w_carry ?max ~lang ~merge ~taints ~enclosing offset shape)

and find_in_shape_w_carry ?max ~lang ~(merge : T.trace_merge) ~taints
    ~(enclosing : shape list) offset shape =
  let not_found () = `Not_found (taints, close_shape enclosing shape, offset) in
  match shape with
  (* offset <> [] *)
  | Bot -> not_found ()
  | Obj { fields = obj; _ } ->
      find_in_obj_w_carry ?max ~lang ~merge ~taints ~enclosing ~self:shape
        offset obj
  | Rec n -> (
      match List.nth_opt enclosing n with
      | Some target ->
          find_in_shape_w_carry ?max ~lang ~merge ~taints
            ~enclosing:(List.drop (n + 1) enclosing)
            offset target
      | None -> not_found ())
  | Arg (arg, base_offsets) -> (
      match find_in_arg ?max ~lang ~merge ~taints offset arg base_offsets with
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

and find_in_obj_w_carry ?max ~lang ~(merge : T.trace_merge) ~taints
    ~(enclosing : shape list) ~(self : shape) (offset : T.offset list) obj =
  let not_found () =
    `Not_found (taints, close_shape enclosing self, offset)
  in
  let enclosing = self :: enclosing in
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
                    cell_read_of_find_result ?max ~lang ~merge
                      (find_in_cell_w_carry ?max ~lang ~merge ~taints
                         ~enclosing offset cell) )
                with
                | acc, None -> acc
                | None, (Some _ as found) -> found
                | Some cell1, Some cell2 -> Some (unify_cell ~lang ~merge cell1 cell2))
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
                        cell_read_of_find_result ?max ~lang ~merge
                          (find_in_cell_w_carry ?max ~lang ~merge ~taints
                             ~enclosing recur_offset cell) )
                    with
                    | acc, None -> acc
                    | None, (Some _ as found) -> found
                    | Some c1, Some c2 -> Some (unify_cell ~lang ~merge c1 c2)))
              obj None
          with
          | None -> not_found ()
          | Some cell -> `Found cell)
      | Ofld _
      | Oint _
      | Ostr _ -> (
          match Fields.find_opt o obj with
          | Some o_cell ->
              find_in_cell_w_carry ?max ~lang ~merge ~taints ~enclosing offset
                o_cell
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
                  find_in_cell_w_carry ?max ~lang ~merge ~taints ~enclosing
                    offset any_cell)))

let find_in_cell ?max ~lang ~(merge : T.trace_merge) offset cell =
  find_in_cell_w_carry ?max ~lang ~merge ~taints:Taints.empty ~enclosing:[]
    offset cell

let option_of_find_result ?max ~lang ~(merge : T.trace_merge) res =
  match res with
  | `Clean -> None
  | `Not_found (taints, _shape, offset) ->
      (* TODO: Fix _shape too. *)
      let taints = fix_poly_taint_with_offset ?max ~lang ~merge offset taints in
      Some (taints, Bot)
  | `Found (Cell (xtaint, shape)) -> Some (Xtaint.to_taints xtaint, shape)

let find_in_cell_poly ?max ~lang ~(merge : T.trace_merge) offset cell =
  find_in_cell ?max ~lang ~merge offset cell
  |> option_of_find_result ?max ~lang ~merge

let find_in_shape_poly ?max ~lang ~(merge : T.trace_merge) ~taints offset shape =
  match offset with
  | [] -> Some (taints, shape)
  | _ :: _ ->
      find_in_shape_w_carry ?max ~lang ~merge ~taints ~enclosing:[] offset shape
      |> option_of_find_result ?max ~lang ~merge

(*********************************************************)
(* Update the xtaint and shape of an offset *)
(*********************************************************)

(* Finds an 'offset' within a 'cell' and updates it via 'f'. *)
let rec update_offset_in_cell_at ~(write : T.call_loc) ~(depth : int)
    ~(enclosing : shape list) ~f offset cell =
  let xtaint, shape =
    match (cell, offset) with
    | Cell (xtaint, shape), [] -> f xtaint (close_shape enclosing shape)
    | Cell (xtaint, shape), _ :: _ ->
        let shape =
          update_offset_in_shape ~write ~depth ~enclosing ~f offset shape
        in
        (xtaint, shape)
  in
  match (xtaint, shape) with
  (* Restore INVARIANT(cell).1 *)
  | `None, Bot -> None
  | `Tainted taints, Bot when Taints.is_empty taints -> None
  (* Restore INVARIANT(cell).2 *)
  | `Clean, (Obj _ | Rec _ | Arg _ | Fun _) ->
      (* If we are tainting an offset of this cell, the cell cannot be
         considered clean anymore. *)
      Some (Cell (`None, shape))
  | `Clean, Bot
  | `None, (Obj _ | Rec _ | Arg _ | Fun _)
  | `Tainted _, (Bot | Obj _ | Rec _ | Arg _ | Fun _) ->
      Some (Cell (xtaint, shape))

and update_offset_in_shape ~(write : T.call_loc) ~(depth : int)
    ~(enclosing : shape list) ~f offset shape =
  match shape with
  | Bot
  | Arg _ ->
      let shape = written_obj ~write ~depth in
      update_offset_in_shape ~write ~depth ~enclosing ~f offset shape
  | Rec n -> (
      match List.nth_opt enclosing n with
      | Some target ->
          update_offset_in_shape ~write ~depth ~enclosing ~f offset
            (shift_free ~levels:(n + 1) target)
      | None -> shape)
  | Obj ({ fields = obj; _ } as node) -> (
      match
        update_offset_in_obj ~write ~depth ~enclosing:(shape :: enclosing) ~f
          offset obj
      with
      | None -> Bot
      | Some obj -> Obj { node with fields = obj })
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not update offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      shape

and update_offset_in_obj ~(write : T.call_loc) ~(depth : int)
    ~(enclosing : shape list) ~f offset obj =
  let update_offset_in_cell =
    update_offset_in_cell_at ~write ~depth:(depth + 1) ~enclosing ~f
  in
  let obj' =
    match offset with
    | [] ->
        Log.err (fun m ->
            m "internal_UNSAFE_update_obj: Impossible happened: empty offset");
        obj
    | o :: offset -> (
        let o, obj = internal_UNSAFE_find_offset_in_obj o obj in
        match o with
        | Oany (* arbitrary index [*] *) ->
            (* consider all fields/indexes *)
            Fields.filter_map (fun _o' -> update_offset_in_cell offset) obj
        | Oslice n ->
            (* Update the trailing-rest from index [n]. For each entry
             * intersecting [n, infinity), apply the update with the
             * appropriately composed inner offset; entries outside the
             * slice pass through unchanged. *)
            Fields.filter_map
              (fun key cell ->
                match key with
                | Oint k when k >= n -> update_offset_in_cell offset cell
                | Oslice m when m >= n -> update_offset_in_cell offset cell
                | Oslice m ->
                    (* m < n by case order; n - m > 0 *)
                    update_offset_in_cell (T.Oslice (n - m) :: offset) cell
                | Oint _
                | Ofld _
                | Ostr _
                | Oany ->
                    Some cell)
              obj
        | Ofld _
        | Oint _
        | Ostr _ ->
            obj
            |> Fields.update o (fun opt_cell ->
                   let* cell = opt_cell in
                   update_offset_in_cell offset cell))
  in
  if Fields.is_empty obj' then None else Some obj'

let update_offset_in_cell ~(write : T.call_loc) ~f offset cell =
  update_offset_in_cell_at ~write ~depth:0 ~enclosing:[] ~f offset cell

(*********************************************************)
(* Updating an offset *)
(*********************************************************)

let update_offset_and_unify ~lang ~(merge : T.trace_merge) ~(write : T.call_loc)
    new_taints new_shape offset opt_cell =
  if taints_and_shape_are_relevant new_taints new_shape then
    let new_xtaint =
      (* THINK: Maybe Dataflow_tainting 'check_xyz' should be returning 'Xtaint.t'? *)
      Xtaint.of_taints new_taints
    in
    let cell = opt_cell ||| cell_none_bot in
    let add_new_taints xtaint shape =
      let shape = unify_shape ~lang ~merge new_shape shape in
      match xtaint with
      | `None
      | `Clean ->
          (* Since we're adding taint we cannot have `Clean here. *)
          (new_xtaint, shape)
      | `Tainted taints as xtaint ->
          if
            !Flag_semgrep.max_taint_set_size =|= 0
            || Taints.cardinal taints < !Flag_semgrep.max_taint_set_size
          then (Xtaint.union ~merge new_xtaint xtaint, shape)
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
    update_offset_in_cell ~write ~f:add_new_taints offset cell
  else
    (* To maintain INVARIANT(cell) we cannot return 'cell_none_bot'! *)
    opt_cell

(*********************************************************)
(* Clean taint *)
(*********************************************************)

(* TODO: Reformulate in terms of 'update_offset_in_cell' *)
let rec clean_cell_at ~(write : T.call_loc) ~(depth : int)
    ~(enclosing : shape list) (offset : T.offset list) cell =
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
      let shape = clean_shape ~write ~depth ~enclosing offset shape in
      Cell (xtaint, shape)

and clean_shape ~(write : T.call_loc) ~(depth : int) ~(enclosing : shape list)
    offset shape =
  match shape with
  | Bot
  | Arg _ ->
      let shape = written_obj ~write ~depth in
      clean_shape ~write ~depth ~enclosing offset shape
  (* A summary stands for several objects and the clean reaches one of
   * them, so the others keep their taint. *)
  | Rec _
  | Obj { summary = true; _ } ->
      shape
  | Obj ({ fields; _ } as node) ->
      Obj
        {
          node with
          fields =
            clean_obj ~write ~depth ~enclosing:(shape :: enclosing) offset
              fields;
        }
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not update offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      shape

and clean_obj ~(write : T.call_loc) ~(depth : int) ~(enclosing : shape list)
    offset obj =
  let clean_cell = clean_cell_at ~write ~depth:(depth + 1) ~enclosing in
  match offset with
  | [] ->
      Log.err (fun m -> m "clean_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = internal_UNSAFE_find_offset_in_obj o obj in
      match o with
      | Oany -> Fields.map (clean_cell offset) obj
      | o ->
          Fields.update o (Option.map (fun cell -> clean_cell offset cell)) obj)

let clean_cell ~(write : T.call_loc) (offset : T.offset list) cell =
  clean_cell_at ~write ~depth:0 ~enclosing:[] offset cell

(*********************************************************)
(* Folding objects nested in an object of the same site *)
(*********************************************************)

type target = Node of int | Leaf of shape

type entry = { xtaint : Xtaint.t; target : target }

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
  entries : entry Fields.t;
  captured : entry Captured.t;
  source : shape;
  ancestors : int list;
}

type quotient = {
  nodes : node Dynarray.t;
  parent : int Dynarray.t;
  size : int Dynarray.t;
  members : int list Dynarray.t;
  class_sites : Shape_and_sig.Sites.t Dynarray.t;
  class_defs : Defs.t Dynarray.t;
  copies : int Fields.t Dynarray.t;
  referenced : bool Dynarray.t;
  incoming : entry list Dynarray.t;
  recorded : bool Dynarray.t;
  mutable unions : int;
}

type path_class = { class_root : int; size_at_visit : int; depth : int }

type carried_taint = { leaf : Taints.t; offset : T.offset list; carry : Taints.t }

let new_quotient () : quotient =
  {
    nodes = Dynarray.create ();
    parent = Dynarray.create ();
    size = Dynarray.create ();
    members = Dynarray.create ();
    class_sites = Dynarray.create ();
    class_defs = Dynarray.create ();
    copies = Dynarray.create ();
    referenced = Dynarray.create ();
    incoming = Dynarray.create ();
    recorded = Dynarray.create ();
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
  Dynarray.add_last q.referenced false;
  Dynarray.add_last q.incoming [];
  Dynarray.add_last q.recorded false;
  i

let rec index (q : quotient) (ancestors : int list) (shape : shape) : target =
  let entry_of (i : int) (Cell (xtaint, shape) : cell) : entry =
    { xtaint; target = index q (i :: ancestors) shape }
  in
  match shape with
  | Obj { sites; summary; fields } ->
      let i =
        add_node q
          {
            sites;
            defs = Defs.empty;
            summary;
            entries = Fields.empty;
            captured = Captured.empty;
            source = shape;
            ancestors;
          }
      in
      let entries = Fields.map (entry_of i) fields in
      Dynarray.set q.nodes i { (Dynarray.get q.nodes i) with entries };
      Node i
  | Fun (c, cs) ->
      let closures = c :: cs in
      let i =
        add_node q
          {
            sites = Shape_and_sig.Sites.empty;
            defs = Defs.of_list (List.map (fun (closure : closure) -> closure.def) closures);
            summary = false;
            entries = Fields.empty;
            captured = Captured.empty;
            source = shape;
            ancestors;
          }
      in
      let captured =
        List.fold_left
          (fun captured (closure : closure) ->
            snd
              (List.fold_left
                 (fun ((position, captured) : int * entry Captured.t)
                      ((_, entry) : IL.name * env_entry) ->
                   match entry with
                   | Val cell ->
                       ( position + 1,
                         Captured.add (closure.def, position) (entry_of i cell)
                           captured )
                   | Ref _ -> (position + 1, captured))
                 (0, captured) closure.env))
          Captured.empty closures
      in
      Dynarray.set q.nodes i { (Dynarray.get q.nodes i) with captured };
      Node i
  | Rec n -> (
      match List.nth_opt ancestors n with
      | Some i -> Node i
      | None -> Leaf shape)
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
      match index q node.ancestors node.source with
      | Node copy ->
          Dynarray.set q.copies member
            (Fields.add o copy (Dynarray.get q.copies member));
          copy
      | Leaf _ -> any

let is_child (q : quotient) (parent : int) (i : int) : bool =
  match (Dynarray.get q.nodes i).ancestors with
  | nearest :: _ -> Int.equal nearest parent
  | [] -> false

let field_entries (q : quotient) (class_root : int) : entry list Fields.t =
  let members = Dynarray.get q.members class_root in
  let present =
    List.fold_left
      (fun present member ->
        Fields.fold
          (fun o entry present ->
            Fields.update o
              (fun entries -> Some (entry :: Option.value entries ~default:[]))
              present)
          (Dynarray.get q.nodes member).entries present)
      Fields.empty members
  in
  Fields.mapi
    (fun o reversed ->
      let reads_of_any =
        List.filter_map
          (fun member ->
            let entries = (Dynarray.get q.nodes member).entries in
            if Fields.mem o entries then None
            else
              Fields.find_opt T.Oany entries
              |> Option.map (fun (any : entry) ->
                     match any.target with
                     | Node any_node when is_child q member any_node ->
                         { any with target = Node (any_copy q member o any_node) }
                     | Node _
                     | Leaf _ ->
                         any))
          members
      in
      List.rev_append reversed reads_of_any)
    present

let captured_entries (q : quotient) (class_root : int) : entry list Captured.t =
  List.fold_right
    (fun member captured ->
      Captured.fold
        (fun key entry captured ->
          Captured.update key
            (fun entries -> Some (entry :: Option.value entries ~default:[]))
            captured)
        (Dynarray.get q.nodes member).captured captured)
    (Dynarray.get q.members class_root)
    Captured.empty

let is_object (q : quotient) (i : int) : bool =
  Defs.is_empty (Dynarray.get q.nodes i).defs

let position_nodes (q : quotient) (entries : entry list) : int list =
  let nodes =
    List.filter_map
      (fun (entry : entry) ->
        match entry.target with
        | Node i -> Some i
        | Leaf _ -> None)
      entries
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

let class_entries (q : quotient) (class_root : int) : entry list list =
  if is_object q class_root then
    Fields.fold (fun _ entries all -> entries :: all) (field_entries q class_root) []
    |> List.rev
  else
    Captured.fold (fun _ entries all -> entries :: all) (captured_entries q class_root) []
    |> List.rev

let topmost_changed (q : quotient) (path : path_class list) : int option =
  List.fold_left
    (fun topmost (path_class : path_class) ->
      let root = find q path_class.class_root in
      if
        Int.equal root path_class.class_root
        && Int.equal (Dynarray.get q.size root) path_class.size_at_visit
      then topmost
      else Some path_class.depth)
    None path

let rec close_class (q : quotient) (above : path_class list) (member : int) :
    int option =
  let class_root = find q member in
  let depth =
    match above with
    | [] -> 0
    | (path_class : path_class) :: _ -> path_class.depth + 1
  in
  let path =
    { class_root; size_at_visit = Dynarray.get q.size class_root; depth } :: above
  in
  let first_visit = not (Dynarray.get q.recorded class_root) in
  Dynarray.set q.recorded class_root true;
  let rec close_fields (fields : entry list list) : int option =
    match fields with
    | [] -> None
    | entries :: rest -> (
        match position_nodes q entries with
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
                  Dynarray.set q.incoming x (Dynarray.get q.incoming x @ entries);
                if List.exists (fun (path_class : path_class) -> Int.equal path_class.class_root x) path
                then (
                  Dynarray.set q.referenced x true;
                  close_fields rest)
                else
                  match
                    List.filter
                      (fun (path_class : path_class) ->
                        shares_site q x path_class.class_root)
                      path
                  with
                  | [] -> (
                      match close_class q path x with
                      | None -> close_fields rest
                      | Some _ as restart -> restart)
                  | hits ->
                      ignore
                        (List.fold_left
                           (fun x (path_class : path_class) -> union q x path_class.class_root)
                           x hits);
                      topmost_changed q path)))
  in
  match close_fields (class_entries q class_root) with
  | Some restart when Int.equal restart depth -> close_class q above class_root
  | outcome -> outcome

let rec close (q : quotient) (roots : entry list) : unit =
  let unions = q.unions in
  Dynarray.iteri (fun i _ -> Dynarray.set q.referenced i false) q.referenced;
  Dynarray.iteri (fun i _ -> Dynarray.set q.incoming i []) q.incoming;
  Dynarray.iteri (fun i _ -> Dynarray.set q.recorded i false) q.recorded;
  (match position_nodes q roots with
  | [] -> ()
  | first :: others ->
      let root = find q (List.fold_left (union q) first others) in
      Dynarray.set q.incoming root roots;
      ignore (close_class q [] root));
  if q.unions > unions then close q roots

let joined_xtaint ~(merge : T.trace_merge) (entries : entry list) : Xtaint.t =
  match entries with
  | [] -> `None
  | first :: rest ->
      List.fold_left
        (fun xtaint (entry : entry) -> Xtaint.union ~merge xtaint entry.xtaint)
        first.xtaint rest

let read_on_entry ~lang ~(merge : T.trace_merge) (q : quotient) (o : T.offset)
    (entry : entry) : [ `Kept | `Cell of entry | `Whole of Taints.t ] =
  match (entry.xtaint, entry.target) with
  | `Tainted taints, Node i ->
      let entries = (Dynarray.get q.nodes i).entries in
      if Fields.mem o entries || Fields.mem T.Oany entries then `Kept
      else `Whole taints
  | `Tainted taints, Leaf (Arg (arg, offsets)) -> (
      match find_in_arg ~lang ~merge ~taints [ o ] arg offsets with
      | Some (Cell (xtaint, shape)) -> `Cell { xtaint; target = Leaf shape }
      | None -> `Whole taints)
  | `Tainted taints, Leaf (Bot | Obj _ | Rec _ | Fun _) -> `Whole taints
  | (`None | `Clean), _ -> `Kept

let rec emit_cell ~lang ~(merge : T.trace_merge) (q : quotient)
    (path : int list) ~(derived : entry list) (carried_taints : carried_taint list)
    (entries : entry list) : cell option =
  let xtaint = joined_xtaint ~merge entries in
  match position_nodes q entries with
  | first :: _ -> (
      let x = find q first in
      match List.find_index (Int.equal x) path with
      | Some distance -> Some (cell_of_join xtaint (Rec distance))
      | None when not (is_object q x) ->
          Some (cell_of_join xtaint (emit_closures ~lang ~merge q path x))
      | None -> (
          match emit_object ~lang ~merge q path x ~derived ~xtaint carried_taints with
          | Bot when not (Xtaint.is_tainted xtaint) -> None
          | shape -> Some (cell_of_join xtaint shape)))
  | [] -> (
      let shape =
        List.fold_left
          (fun shape (entry : entry) ->
            match entry.target with
            | Leaf leaf -> unify_shape ~lang ~merge shape leaf
            | Node _ -> shape)
          Bot entries
      in
      match (cell_of_join xtaint shape, carried_taints) with
      | Cell (`Clean, _), _ :: _ ->
          let taints =
            carried_taints
            |> List.filter (fun (carried_taint : carried_taint) ->
                   not (Taints.equal carried_taint.leaf carried_taint.carry))
            |> List.fold_left
                 (fun taints (carried_taint : carried_taint) ->
                   Taints.union ~merge taints
                     (fix_poly_taint_with_offset ~lang ~merge carried_taint.offset
                        carried_taint.leaf))
                 Taints.empty
          in
          if Taints.is_empty taints then None
          else Some (Cell (`Tainted taints, Bot))
      | cell, _ -> Some cell)

and emit_object ~lang ~(merge : T.trace_merge) (q : quotient) (path : int list)
    (x : int) ~(derived : entry list) ~(xtaint : Xtaint.t)
    (carried_taints : carried_taint list) : shape =
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
  let path = x :: path in
  let field_map = field_entries q x in
  let fields =
    Fields.fold
      (fun o entries fields ->
        let reads = List.map (read_on_entry ~lang ~merge q o) incoming in
        let derived =
          List.filter_map
            (function
              | `Cell entry -> Some entry
              | `Kept
              | `Whole _ ->
                  None)
            reads
        in
        let carried_taints =
          List.filter_map
            (function
              | `Whole leaf -> Some { leaf; offset = [ o ]; carry }
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
          emit_cell ~lang ~merge q path ~derived carried_taints (entries @ derived)
        with
        | Some cell -> Fields.add o cell fields
        | None -> fields)
      field_map Fields.empty
  in
  if Fields.is_empty fields && not (Fields.is_empty field_map) then Bot
  else
    Obj
      {
        sites = Dynarray.get q.class_sites x;
        summary =
          Dynarray.get q.referenced x
          || List.exists
               (fun member -> (Dynarray.get q.nodes member).summary)
               (Dynarray.get q.members x);
        fields;
      }

and emit_closures ~lang ~(merge : T.trace_merge) (q : quotient)
    (path : int list) (x : int) : shape =
  let path = x :: path in
  let captured = captured_entries q x in
  let join_closures (first : closure) (others : closure list) : closure =
    let sig_ =
      List.fold_left
        (fun (sig_ : Signature.t) (closure : closure) ->
          if phys_equal closure.sig_ sig_ then sig_
          else
            {
              sig_ with
              Signature.effects =
                Effects.union ~merge sig_.Signature.effects
                  closure.sig_.Signature.effects;
            })
        first.sig_ others
    in
    let env =
      List.mapi
        (fun position ((var, entry) as binding : IL.name * env_entry) ->
          match (entry, Captured.find_opt (first.def, position) captured) with
          | Val _, Some entries -> (
              match emit_cell ~lang ~merge q path ~derived:[] [] entries with
              | Some cell -> (var, Val cell)
              | None -> binding)
          | (Val _ | Ref _), _ -> binding)
        first.env
    in
    { first with sig_; env }
  in
  let closures =
    Dynarray.get q.members x
    |> List.concat_map (fun member ->
           match (Dynarray.get q.nodes member).source with
           | Fun (c, cs) -> c :: cs
           | Bot
           | Obj _
           | Rec _
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
  | first :: others -> Fun (first, others)
  | [] -> Bot

let join_folded_by_site ~lang ~(merge : T.trace_merge) (previous : cell option)
    (computed : cell) : cell =
  let has_node (Cell (_, shape) : cell) : bool =
    match shape with
    | Obj _
    | Fun _ ->
        true
    | Bot
    | Rec _
    | Arg _ ->
        false
  in
  match previous with
  | Some previous when phys_equal previous computed -> computed
  | Some previous when not (has_node previous || has_node computed) ->
      unify_cell ~lang ~merge previous computed
  | None when not (has_node computed) -> computed
  | Some _
  | None -> (
      let q = new_quotient () in
      let roots =
        Option.to_list previous @ [ computed ]
        |> List.map (fun (Cell (xtaint, shape)) ->
               { xtaint; target = index q [] shape })
      in
      close q roots;
      if Option.is_none previous && Int.equal q.unions 0 then computed
      else
        match emit_cell ~lang ~merge q [] ~derived:[] [] roots with
        | Some cell -> cell
        | None -> Cell (joined_xtaint ~merge roots, Bot))

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
  | Rec _ -> own_taints

and enum_in_obj obj =
  obj
  |> Fields.to_seq
  |> Seq.map (fun (o, cell) ->
         enum_in_cell cell
         |> Seq.map (fun (offset, taints, shape) -> (o :: offset, taints, shape)))
  |> Seq.concat
