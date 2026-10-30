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
    | Ofld_any, Ofld_any -> 0
    | (Ofld _ | Ostr _ | Oint _ | Oslice _ | Oany), Ofld_any -> -1
    | Ofld_any, (Ofld _ | Ostr _ | Oint _ | Oslice _ | Oany) -> 1
end)

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
module FunctionMap = Map.Make (Function_id)
(* A method of a class, by the class and the attribute it defines. *)
module Member_map = Map.Make (struct
  type t = string * string

  let compare ((class1, member1) : t) ((class2, member2) : t) : int =
    let by_class = String.compare class1 class2 in
    if by_class <> 0 then by_class else String.compare member1 member2
end)

(* Where a closure was created, seen from the function whose table holds it:
   the calls through which it was returned, by their byte positions, the
   call of that function first; [[]] where the lambda is defined. A site
   keeps [Limits_semgrep.taint_MAX_CALLBACK_DEPTH] calls: a recursion that
   returns closures would otherwise give sites without bound. *)
module Creation_site = struct
  type t = int option list

  let compare (site1 : t) (site2 : t) : int =
    List.compare (Option.compare Int.compare) site1 site2

  let equal (site1 : t) (site2 : t) : bool = Int.equal (compare site1 site2) 0
  let at_definition : t = []

  (* Every creation site of a lambda, longer than any site [through_call]
     gives. *)
  let any : t = List.init (Limits_semgrep.taint_MAX_CALLBACK_DEPTH + 1) (fun (_ : int) -> None)
  let is_any (site : t) : bool = equal site any

  (* The site of a closure of a callee, seen from the caller through [call]:
     the innermost [Limits_semgrep.taint_MAX_CALLBACK_DEPTH] calls, those
     nearest the creation, are kept. *)
  let through_call (call : int option) (site : t) : t =
    if List.compare_length_with site Limits_semgrep.taint_MAX_CALLBACK_DEPTH >= 0 then site
    else call :: site

  (* [sites] of one lambda, or [any] past
     [Limits_semgrep.taint_MAX_CLOSURES_PER_LAMBDA] of them. *)
  let bound (sites : t list) : t list =
    if
      List.exists is_any sites
      || List.compare_length_with sites Limits_semgrep.taint_MAX_CLOSURES_PER_LAMBDA > 0
    then [ any ]
    else sites
end

(* A closure in a table: its lambda and where it was created. *)
module Closure_key = struct
  type t = Function_id.t * Creation_site.t

  let compare ((id1, site1) : t) ((id2, site2) : t) : int =
    match Function_id.compare id1 id2 with
    | 0 -> Creation_site.compare site1 site2
    | other -> other
end

module ClosureMap = Map.Make (Closure_key)

(* The entries of [table] that a reference to the closure [key] refers to:
   every entry of the lambda for [Creation_site.any], or else the entry of
   the site, or the entry of [Creation_site.any] that has replaced it. *)
let find_closures (table : 'a ClosureMap.t) ((id, site) : Closure_key.t) : 'a list =
  if Creation_site.is_any site then
    ClosureMap.to_seq_from (id, Creation_site.at_definition) table
    |> Seq.take_while (fun (((id', _), _) : Closure_key.t * 'a) -> Function_id.equal id id')
    |> Seq.map snd
    |> List.of_seq
  else
    match ClosureMap.find_opt (id, site) table with
    | Some entry -> [ entry ]
    | None -> Option.to_list (ClosureMap.find_opt (id, Creation_site.any) table)

module rec Shape : sig
  type shape =
    | Bot  (** _|_, don't know or don't care *)
    | Obj of obj
        (** An "object" or struct-like thing.

            Tuples or lists are also represented by 'Obj' shapes! We just treat
            constant indexes as if they were fields, and use 'Oany' to capture
            the non-constant indexes. *)
    | Arg of Taint.formal * Taint.offset list list
        (** Represents the yet-unknown shape of a function/method parameter,
            optionally extended with one or more offset paths into the
            parameter. Each inner [Taint.offset list] is a single path; the
            outer list is a disjunction of paths — a [cb] bound across two
            branches as [opts.a] in one and [opts.b] in the other has
            [Arg (opts, [[Ostr "a"]; [Ostr "b"]])]. Bare parameters use
            [Arg (arg, [[]])]: one path, empty. At HOF dispatch the engine
            enumerates each path; at call-site instantiation each path is
            resolved against the caller's actual argument. *)
    | Fun of Signature.t
        (** Function shapes. These enable Semgrep to handle HOFs. *)
    | Fun_ref of Function_id.t * Signature.t Lazy.t * closure_kind
        (** A closure: a lambda, by the identity of its signature in the
            signature database. The signature is the database's, never
            rewritten, so a walk of a shape does not enter it; it is
            extracted when it is first read. A call of the closure
            instantiates the signature with its captured variables holding
            the values that [closure_kind] gives them. *)
    | Funs of shape list
        (** One of several functions: two or more [Fun] and [Fun_ref] that
            are not the same function, ordered by [compare_shape] without
            duplicates. A call of it is a call of each. *)

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
      * explicitly tracked ('Ofld_any', an arbitrary field, likewise). If there
      * is no 'Oany' in the 'Obj' shape, then a
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
      *
      * INVARIANT(obj): a write through an arbitrary index or field updates
      * the entry of 'Oany' or 'Ofld_any' only, so a read of a tracked field
      * reads that entry too. The exception is a read that meets a part of
      * the field known clean ('`Clean'): such a part was written after the
      * last write through an arbitrary index or field, which updates every
      * tracked field where a read could meet a part known clean, and a join
      * with an object that has such an entry drops the clean marks of the
      * other side's fields.
      *)

  and closure_kind =
    | Open
        (** The lambda in the function that defines it: a captured variable
            takes its value where the signature is used. *)
    | Closed of Creation_site.t list
        (** The captured variables hold the values of the closures of the
            lambda in the table of the analysis or signature that holds the
            shape, one per creation site of the list, joined. *)

  and closure = captured IL.NameMap.t
  (** The captured variables of a closure, by variable. *)

  and captured = {
    value : cell;  (** The value of the variable. *)
    location : Taint.lval option;
        (** The input of the function that defines the closure that the
            variable holds, when it holds one: a write to the variable in a
            call of the closure is a write to it. *)
  }

  val equal_cell : cell -> cell -> bool

  val equal_cell_with_guards : cell -> cell -> bool
  (** Like [equal_cell] but a difference only in a taint's guard counts as
      a difference, recursively ([Fun] shapes compare via
      [Signature.equal_with_guards]). For the dataflow fixpoint's stability
      test; identity and fusion keying keep using [equal_cell]. *)

  val equal_shape : shape -> shape -> bool
  val compare_shape : shape -> shape -> int

  val compare_shape_structure : shape -> shape -> int
  (** Like [compare_shape] but blind to the taints of the cells of an object:
      two shapes with the same fields, parameters and functions compare as 0. *)

  val union_same_structure : shape -> shape -> shape
  (** [union_same_structure s1 s2], where [compare_shape_structure s1 s2 = 0],
      unites the taints of corresponding cells. *)

  val equal_closure : closure -> closure -> bool

  val equal_closure_with_guards : closure -> closure -> bool
  (** Like [equal_closure] but over [equal_cell_with_guards]. *)

  val compare_closure : closure -> closure -> int
  val show_closure : closure -> string
  val show_cell : cell -> string
  val show_shape : shape -> string
  val show_obj : obj -> string
end = struct
  type shape =
    | Bot
    | Obj of obj
    | Arg of T.formal * T.offset list list
    | Fun of Signature.t
    | Fun_ref of Function_id.t * Signature.t Lazy.t * closure_kind
    | Funs of shape list
  and closure_kind = Open | Closed of Creation_site.t list
  and cell = Cell of Xtaint.t * shape
  and obj = cell Fields.t
  and closure = captured IL.NameMap.t
  and captured = {
    value : cell;
    location :
      (T.lval[@equal T.equal_lval])
      option;
  }
  [@@deriving eq]

  (*************************************)
  (* Equality *)
  (*************************************)
  (* TODO: Should we just define these in terms of `compare_*` ? *)

  (* Depth-limited equality to prevent infinite recursion and force convergence
   * for pathological patterns like obj[key] = [obj[key], item] that create
   * unbounded recursive structures. If both shapes exceed MAX_SHAPE_DEPTH,
   * we consider them equal (widening approximation). *)
  let rec equal_cell_depth ~(equal_xtaint : Xtaint.t -> Xtaint.t -> bool)
      ~(equal_sig : Signature.t -> Signature.t -> bool) depth cell1 cell2 =
    if phys_equal cell1 cell2 || depth > Limits_semgrep.taint_MAX_SHAPE_DEPTH then true
    else
      let (Cell (taints1, shape1)) = cell1 in
      let (Cell (taints2, shape2)) = cell2 in
      equal_xtaint taints1 taints2
      && equal_shape_depth ~equal_xtaint ~equal_sig depth shape1 shape2

  and equal_shape_depth ~equal_xtaint ~equal_sig depth shape1 shape2 =
    if depth > Limits_semgrep.taint_MAX_SHAPE_DEPTH then true
    else
      match (shape1, shape2) with
      | Bot, Bot -> true
      | Obj obj1, Obj obj2 ->
          Fields.equal (equal_cell_depth ~equal_xtaint ~equal_sig (depth + 1)) obj1 obj2
      | Arg (arg1, offsets1), Arg (arg2, offsets2) ->
          T.equal_formal arg1 arg2
          && Int.equal
               (List.compare (List.compare T.compare_offset)
                  offsets1 offsets2)
               0
      | Fun sig1, Fun sig2 -> equal_sig sig1 sig2
      | Fun_ref (id1, _, kind1), Fun_ref (id2, _, kind2) ->
          (* A reference refers to a function: its signature is that
             function's, so the copy it carries does not distinguish it. *)
          Function_id.equal id1 id2 && equal_closure_kind kind1 kind2
      | Funs funs1, Funs funs2 ->
          List.equal (equal_shape_depth ~equal_xtaint ~equal_sig depth) funs1 funs2
      | Bot, _
      | Obj _, _
      | Arg _, _
      | Fun _, _
      | Fun_ref _, _
      | Funs _, _ ->
          false

  (* Public API uses depth 0 *)
  let equal_cell cell1 cell2 =
    equal_cell_depth ~equal_xtaint:Xtaint.equal ~equal_sig:Signature.equal 0 cell1 cell2

  (* Guard-aware twin of [equal_cell]: cell taints compare via
   * [Xtaint.equal_with_guards] and [Fun] shapes via
   * [Signature.equal_with_guards]. *)
  let equal_cell_with_guards cell1 cell2 =
    equal_cell_depth ~equal_xtaint:Xtaint.equal_with_guards
      ~equal_sig:Signature.equal_with_guards 0 cell1 cell2

  let equal_closure_with ~(equal_cell : cell -> cell -> bool) (closure1 : closure)
      (closure2 : closure) : bool =
    IL.NameMap.equal
      (fun (c1 : captured) (c2 : captured) ->
        equal_cell c1.value c2.value && Option.equal T.equal_lval c1.location c2.location)
      closure1 closure2

  let equal_closure = equal_closure_with ~equal_cell
  let equal_closure_with_guards = equal_closure_with ~equal_cell:equal_cell_with_guards

  (*************************************)
  (* Comparison *)
  (*************************************)

  let rec compare_cell cell1 cell2 =
    let (Cell (taints1, shape1)) = cell1 in
    let (Cell (taints2, shape2)) = cell2 in
    match Xtaint.compare taints1 taints2 with
    | 0 -> compare_shape shape1 shape2
    | other -> other

  and compare_shape shape1 shape2 =
    match (shape1, shape2) with
    | Bot, Bot -> 0
    | Obj obj1, Obj obj2 -> compare_obj obj1 obj2
    | Arg (arg1, offsets1), Arg (arg2, offsets2) -> (
        match T.compare_formal arg1 arg2 with
        | 0 -> List.compare (List.compare T.compare_offset) offsets1 offsets2
        | other -> other)
    | Fun sig1, Fun sig2 -> Signature.compare sig1 sig2
    | Fun_ref (id1, _, kind1), Fun_ref (id2, _, kind2) -> (
        match Function_id.compare id1 id2 with
        | 0 -> compare_closure_kind kind1 kind2
        | other -> other)
    | Funs funs1, Funs funs2 -> List.compare compare_shape funs1 funs2
    | Bot, (Obj _ | Arg _ | Fun _ | Fun_ref _ | Funs _)
    | Obj _, (Arg _ | Fun _ | Fun_ref _ | Funs _)
    | Arg _, (Fun _ | Fun_ref _ | Funs _)
    | Fun _, (Fun_ref _ | Funs _)
    | Fun_ref _, Funs _ ->
        -1
    | Obj _, Bot
    | Arg _, (Bot | Obj _)
    | Fun _, (Bot | Obj _ | Arg _)
    | Fun_ref _, (Bot | Obj _ | Arg _ | Fun _)
    | Funs _, (Bot | Obj _ | Arg _ | Fun _ | Fun_ref _) ->
        1

  and compare_obj obj1 obj2 = Fields.compare compare_cell obj1 obj2

  and compare_closure_kind (kind1 : closure_kind) (kind2 : closure_kind) : int =
    match (kind1, kind2) with
    | Open, Open -> 0
    | Closed sites1, Closed sites2 ->
        List.compare Creation_site.compare sites1 sites2
    | Open, Closed _ -> -1
    | Closed _, Open -> 1

  and compare_closure (closure1 : closure) (closure2 : closure) : int =
    IL.NameMap.compare
      (fun (c1 : captured) (c2 : captured) ->
        match compare_cell c1.value c2.value with
        | 0 -> Option.compare T.compare_lval c1.location c2.location
        | other -> other)
      closure1 closure2

  let rec compare_cell_structure (Cell (_, shape1) : cell)
      (Cell (_, shape2) : cell) : int =
    compare_shape_structure shape1 shape2

  and compare_shape_structure (shape1 : shape) (shape2 : shape) : int =
    match (shape1, shape2) with
    | Obj obj1, Obj obj2 -> Fields.compare compare_cell_structure obj1 obj2
    | (Bot | Obj _ | Arg _ | Fun _ | Fun_ref _ | Funs _), _ -> compare_shape shape1 shape2

  (* Both cells have the same structure, so neither is [`Clean] unless both
     shapes are [Bot], and the union keeps INVARIANT(cell).2. *)
  let rec union_cell_same_structure (Cell (xtaint1, shape1) : cell)
      (Cell (xtaint2, shape2) : cell) : cell =
    Cell (Xtaint.union xtaint1 xtaint2, union_same_structure shape1 shape2)

  and union_same_structure (shape1 : shape) (shape2 : shape) : shape =
    match (shape1, shape2) with
    | Obj obj1, Obj obj2 ->
        Obj
          (Fields.union
             (fun (_ : T.offset) (cell1 : cell) (cell2 : cell) ->
               Some (union_cell_same_structure cell1 cell2))
             obj1 obj2)
    | (Bot | Obj _ | Arg _ | Fun _ | Fun_ref _ | Funs _), _ -> shape1

  (*************************************)
  (* Pretty-printing *)
  (*************************************)

  let rec show_cell cell =
    let (Cell (xtaint, shape)) = cell in
    spf "cell<%s>(%s)" (Xtaint.show xtaint) (show_shape shape)

  and show_shape = function
    | Bot -> "_|_"
    | Obj obj -> spf "obj {|%s|}" (show_obj obj)
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
    | Fun fsig -> Signature.show fsig
    | Fun_ref (id, _, kind) -> (
        "ref:" ^ Function_id.show id
        ^
        match kind with
        | Open -> ""
        | Closed _ -> "[closed]")
    | Funs funs -> "funs{" ^ String.concat " | " (List.map show_shape funs) ^ "}"

  and show_closure (closure : closure) : string =
    "["
    ^ (IL.NameMap.bindings closure
      |> List.map (fun ((var : IL.name), (captured : captured)) ->
             IL.str_of_name var ^ ":" ^ show_cell captured.value)
      |> String.concat ", ")
    ^ "]"

  and show_obj obj =
    obj |> Fields.to_seq
    |> Seq.map (fun (o, o_cell) ->
           spf "%s: %s" (T.show_offset o) (show_cell o_cell))
    |> List.of_seq |> String.concat "; "
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
    control_taints : Taint.taints;
        (** The taints propagated via the control flow (cf., `control: true`
            sources) * used for reachability queries. *)
    return_tok : AST_generic.tok;
    guards : Effect_guard.t;
        (** See note on [taints_to_sink.guards]. *)
  }

  type taints_to_lval = {
    taints : Taint.taints;
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
            (** The formal parameter corresponding to the function shape, this
                is what we instantiate at a specific call site. *)
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
            the 'ToSink' effects of a function call where the function is not *
            yet known (the function is an argument to be instantiated at call *
            site). * * TODO: Handle 'ToReturn' (probably easy) and 'ToLval' (may
            be trickier). *)

  val compare : t -> t -> int
  (** Guard-excluding: two effects equal up to their [guards] compare as 0,
      so an [Effects] set holds one element per guard-less effect identity
      and fuses guards on insertion (see [fuse_guards]). *)

  val union_sink_items : taint_to_sink_item list -> taint_to_sink_item list
  (** One item per taint, the guards of the items of one taint fused. *)

  val compare_target : t -> t -> int
  (** The order of effects by target: the sink, precondition and bindings of
      a [ToSink]; the l-value of a [ToLval]; the callee, parameter, offset and
      structure of the argument shapes of a [ToSinkInCall]
      ([Shape.compare_shape_structure]). On a [ToReturn] it is [compare].
      [compare] refines it, so effects with the same target are adjacent in
      an [Effects] set. *)

  val join_same_target : t -> t -> t
  (** [join_same_target e1 e2], where [compare_target e1 e2 = 0] and
      [joins_by_target e1], unites the taints of the two effects. *)

  val joins_by_target : t -> bool
  (** Whether an [Effects] set unites the taints of this effect with those of
      an effect of the same target and guard: true of [ToLval],
      [ToSinkInCall] and a [ToSink] without precondition, whose instantiation
      maps their taints one by one. *)

  val fuse_guards : t -> t -> t
  (** [fuse_guards e1 e2], where [compare e1 e2 = 0], is [e1] with every
      guard-bearing payload fused disjunctively: the effect-level guard, the
      per-item [ToSink] guards, and the bundle guards inside the [Taints.t]
      payloads. The fused effect applies iff either of the two would. *)

  val guards_equal : t -> t -> bool
  (** Whether two identity-equal effects carry the same guards in every
      guard-bearing payload. Insertion no-op checks and fixpoint stability
      tests must use this; effect identity ([compare]) is guard-blind. *)

  val show : ?truncate_guards:bool -> t -> string

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
    control_taints : Taint.taints;
    return_tok : AST_generic.tok;
    guards : Effect_guard.t;
  }

  type taints_to_lval = {
    taints : T.taints;
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

  (* One item per taint, in order; the sort is stable, so of two items of
   * one taint the first is kept, with the two guards fused. *)
  let union_sink_items (items : taint_to_sink_item list) : taint_to_sink_item list =
    List.stable_sort compare_taint_to_sink_item items
    |> List.fold_left
         (fun (acc : taint_to_sink_item list) (item : taint_to_sink_item) ->
           match acc with
           | prev :: rest when Int.equal (compare_taint_to_sink_item prev item) 0 ->
               { prev with guard = Effect_guard.compose_or prev.guard item.guard } :: rest
           | _ -> item :: acc)
         []
    |> List.rev

  (* A sink effect without its taints: the sink, the precondition and the
   * metavariable bindings. *)
  let compare_sink_target sink1 pre1 env1 sink2 pre2 env2 =
    match compare_sink sink1 sink2 with
    | 0 -> (
        match R.compare_precondition pre1 pre2 with
        | 0 -> T.compare_metavar_env env1 env2
        | other -> other)
    | other -> other

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
    match compare_sink_target sink1 pre1 env1 sink2 pre2 env2 with
    | 0 -> List.compare compare_taint_to_sink_item ttsis1 ttsis2
    | other -> other

  let compare_taints_to_return
      {
        data_taints = data_taints1;
        data_shape = data_shape1;
        control_taints = control_taints1;
        return_tok = _;
        guards = _;
      }
      {
        data_taints = data_taints2;
        data_shape = data_shape2;
        control_taints = control_taints2;
        return_tok = _;
        guards = _;
      } =
    match Taints.compare data_taints1 data_taints2 with
    | 0 -> (
        match Shape.compare_shape data_shape1 data_shape2 with
        | 0 -> Taints.compare control_taints1 control_taints2
        | other -> other)
    | other -> other

  let compare_taints_to_lval
      { taints = ts1; lval = lv1; guards = _ }
      { taints = ts2; lval = lv2; guards = _ } =
    match T.compare_lval lv1 lv2 with
    | 0 -> Taints.compare ts1 ts2
    | other -> other

  (* The argument without its taints: its name and shape. *)
  let compare_arg_shape ~(compare_shape : Shape.shape -> Shape.shape -> int)
      (arg1 : _ IL.argument) (arg2 : _ IL.argument) =
    match (arg1, arg2) with
    | Unnamed (_, shape1), Unnamed (_, shape2) -> compare_shape shape1 shape2
    | Named ((name1, _), (_, shape1)), Named ((name2, _), (_, shape2)) -> (
        (* Arguments of one name from different calls are one argument. *)
        match String.compare name1 name2 with
        | 0 -> compare_shape shape1 shape2
        | other -> other)
    | Unnamed _, Named _ -> -1
    | Named _, Unnamed _ -> 1

  (* The taints in the cells of an argument shape grow from one round of a
     recursive component to the next; a target that included them would
     never be met again, and the effects of the earlier rounds would stay
     beside the joined one. *)
  let compare_arg_target =
    compare_arg_shape ~compare_shape:Shape.compare_shape_structure

  let compare_arg_taints (arg1 : _ IL.argument) (arg2 : _ IL.argument) =
    match (arg1, arg2) with
    | Unnamed (taints1, _), Unnamed (taints2, _)
    | Named (_, (taints1, _)), Named (_, (taints2, _)) ->
        Taints.compare taints1 taints2
    | Unnamed _, Named _ -> -1
    | Named _, Unnamed _ -> 1

  let compare_sink_in_call_target (fexp1 : IL.exp) fvar1 foff1 args_taints1
      (fexp2 : IL.exp) fvar2 foff2 args_taints2 =
    (* Comparing "fvar"s is cheap so better to do it first. *)
    match T.compare_formal fvar1 fvar2 with
    | 0 -> (
        match List.compare T.compare_offset foff1 foff2 with
        | 0 -> (
            match IL.compare_orig fexp1.eorig fexp2.eorig with
            | 0 -> List.compare compare_arg_target args_taints1 args_taints2
            | other -> other)
        | other -> other)
    | other -> other

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
        match
          compare_sink_in_call_target fexp1 fvar1 foff1 args_taints1 fexp2
            fvar2 foff2 args_taints2
        with
        | 0 -> (
            match
              List.compare
                (compare_arg_shape ~compare_shape:Shape.compare_shape)
                args_taints1 args_taints2
            with
            | 0 -> List.compare compare_arg_taints args_taints1 args_taints2
            | other -> other)
        | other -> other)
    | ToSink _, (ToReturn _ | ToLval _ | ToSinkInCall _) -> -1
    | ToReturn _, (ToLval _ | ToSinkInCall _) -> -1
    | ToLval _, ToSinkInCall _ -> -1
    | ToReturn _, ToSink _ -> 1
    | ToLval _, (ToSink _ | ToReturn _) -> 1
    | ToSinkInCall _, (ToSink _ | ToReturn _ | ToLval _) -> 1

  let compare_target r1 r2 =
    match (r1, r2) with
    | ToSink tts1, ToSink tts2 ->
        compare_sink_target tts1.sink (snd tts1.taints_with_precondition)
          tts1.merged_env tts2.sink (snd tts2.taints_with_precondition)
          tts2.merged_env
    | ToLval ttl1, ToLval ttl2 -> T.compare_lval ttl1.lval ttl2.lval
    | ToSinkInCall c1, ToSinkInCall c2 ->
        compare_sink_in_call_target c1.callee c1.arg c1.arg_offset
          c1.args_taints c2.callee c2.arg c2.arg_offset c2.args_taints
    | _ -> compare r1 r2

  (* A sink effect with a precondition is not joined: the precondition is
   * solved over the taints of one effect, and over a union it could hold
   * where it holds for neither. *)
  let joins_by_target = function
    | ToLval _ | ToSinkInCall _ -> true
    | ToSink { taints_with_precondition = _, R.PBool true; _ } -> true
    | ToSink _ | ToReturn _ -> false

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

  let show ?(truncate_guards = true) = function
    | ToSink tts -> show_taints_to_sink ~truncate_guards tts
    | ToReturn ttr -> show_taints_to_return ~truncate_guards ttr
    | ToLval { taints; lval; guards; _ } ->
        Printf.sprintf "%s%s ----> %s" (T.show_taints ~truncate_guards taints)
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
  let fuse_guards e1 e2 =
    match (e1, e2) with
    | ToSink tts1, ToSink tts2 ->
        (* [compare e1 e2 = 0] gives pairwise identity-equal items in the same
         * order, so the per-item guards fuse positionally. *)
        let items1, pre = tts1.taints_with_precondition in
        let items2, _ = tts2.taints_with_precondition in
        let items =
          List.map2
            (fun (i1 : taint_to_sink_item) (i2 : taint_to_sink_item) ->
              { i1 with guard = Effect_guard.compose_or i1.guard i2.guard })
            items1 items2
        in
        ToSink
          { tts1 with
            taints_with_precondition = (items, pre);
            guards = Effect_guard.compose_or tts1.guards tts2.guards }
    | ToReturn ttr1, ToReturn ttr2 ->
        (* [Taints.union] fuses identity-equal bundles' guards via
         * [compose_or]; on identity-equal sets that is exactly per-bundle
         * guard fusion. *)
        ToReturn
          { ttr1 with
            data_taints = Taints.union ttr1.data_taints ttr2.data_taints;
            control_taints =
              Taints.union ttr1.control_taints ttr2.control_taints;
            guards = Effect_guard.compose_or ttr1.guards ttr2.guards }
    | ToLval ttl1, ToLval ttl2 ->
        ToLval
          { ttl1 with
            taints = Taints.union ttl1.taints ttl2.taints;
            guards = Effect_guard.compose_or ttl1.guards ttl2.guards }
    | ToSinkInCall c1, ToSinkInCall c2 ->
        let fuse_arg (a1 : (Taints.t * Shape.shape) IL.argument)
            (a2 : (Taints.t * Shape.shape) IL.argument) :
            (Taints.t * Shape.shape) IL.argument =
          (* The shapes have the same structure and their cells' taints are
           * united; guard refinement inside [Fun] shapes is a known gap of
           * the guard-blind [Signature] equality. *)
          match (a1, a2) with
          | IL.Unnamed (t1, s1), IL.Unnamed (t2, s2) ->
              IL.Unnamed (Taints.union t1 t2, Shape.union_same_structure s1 s2)
          | IL.Named (id1, (t1, s1)), IL.Named (_, (t2, s2)) ->
              IL.Named
                (id1, (Taints.union t1 t2, Shape.union_same_structure s1 s2))
          | (IL.Unnamed _ | IL.Named _), _ ->
              a1 (* unreachable: target-equal args have the same structure *)
        in
        ToSinkInCall
          { c1 with
            args_taints = List.map2 fuse_arg c1.args_taints c2.args_taints;
            guards = Effect_guard.compose_or c1.guards c2.guards }
    | _ -> e1 (* unreachable: differing constructors compare non-zero *)

  (* Precondition: [compare_target e1 e2 = 0] and [joins_by_target e1]. The
   * union of the taints of two effects of one target; a taint in both keeps
   * the item of [e1] with the two guards fused. *)
  let join_same_target e1 e2 =
    match (e1, e2) with
    | ToSink tts1, ToSink tts2 ->
        let items1, pre = tts1.taints_with_precondition in
        let items2, _ = tts2.taints_with_precondition in
        let items = union_sink_items (items1 @ items2) in
        ToSink
          { tts1 with
            taints_with_precondition = (items, pre);
            guards = Effect_guard.compose_or tts1.guards tts2.guards }
    | _ -> fuse_guards e1 e2

  (* Whether two identity-equal effects ([compare] = 0) carry the same
   * guards, including every guard-bearing payload: [ToSink] item guards
   * and the bundle guards inside the [Taints.t] payloads (effect
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
end

and Effects : sig
  include Set.S with type elt = Effect.t

  val equal_with_guards : t -> t -> bool
  (** Like [equal] but also requires the guards of identity-equal elements to
      be equal ([Effect_guard.equal]). [Effect.compare] ignores guards, so
      plain [equal] treats a pass that only refines an effect's guard (fusing
      in a new disjunct) as unchanged; a fixpoint stability check using it
      would stop with the narrower guard and drop effects the refined guard
      keeps. Use this for stability checks. *)

  val show : ?truncate_guards:bool -> t -> string
  val add_list : Effect.t list -> t -> t
  val union_list : t list -> t
end = struct
  include Set.Make (struct
    type t = Effect.t

    let compare effect1 effect2 =
      if phys_equal effect1 effect2 then 0 else Effect.compare effect1 effect2
  end)

  (* [Effect.compare] ignores guards, so the set holds one element per
   * guard-less effect identity. Every inserting operation below fuses the
   * guards of colliding elements via [Effect.fuse_guards]. Letting [Set]
   * keep one variant and discard the other's guard would be unsound: at
   * instantiation an effect whose guard folds to false is dropped, so at a
   * call site where the kept variant's guard folds to false the effect
   * would be dropped even though the discarded variant's guard would have
   * kept it. The [Effect.guards_equal] check makes re-adding an
   * already-fused effect a no-op (disjunction is idempotent under the
   * clause-set dedup), so the dataflow fixpoint still reaches a fixed
   * point; it compares every guard-bearing payload, not just the
   * effect-level guard — an item- or bundle-guard-only refinement must
   * not be discarded. *)
  let add_elt = add

  (* An effect of the same target and effect-level guard as [eff], for an
   * effect that joins by target. Such effects are adjacent in the set. *)
  let find_same_target eff set =
    if not (Effect.joins_by_target eff) then None
    else
      match find_first_opt (fun e -> Effect.compare_target e eff >= 0) set with
      | None -> None
      | Some first ->
          to_seq_from first set
          |> Seq.take_while (fun e -> Int.equal (Effect.compare_target e eff) 0)
          |> Seq.find (fun e ->
                 Effect_guard.equal (Effect.guards_of e) (Effect.guards_of eff))

  (* An effect that joins by target is united with the effect of the same
   * target and guard: the set holds one such effect per target and guard. *)
  let rec add eff set =
    match find_opt eff set with
    | Some existing ->
        let fused = Effect.fuse_guards existing eff in
        if Effect.guards_equal fused existing then set
        else add fused (remove existing set)
    | None -> (
        match find_same_target eff set with
        | None -> add_elt eff set
        | Some existing ->
            let fused = Effect.join_same_target existing eff in
            if
              Int.equal (Effect.compare fused existing) 0
              && Effect.guards_equal fused existing
            then set
            else add fused (remove existing set))

  let union s1 s2 =
    (* Fold the smaller set into the larger one. *)
    if cardinal s1 >= cardinal s2 then fold add s2 s1 else fold add s1 s2

  let of_list elts = List.fold_left (fun set e -> add e set) empty elts
  let map f s = fold (fun e acc -> add (f e) acc) s empty

  let filter_map f s =
    fold
      (fun e acc ->
        match f e with
        | Some e' -> add e' acc
        | None -> acc)
      s empty

  (* [equal] gives identity-equal element pairs at the same position of both
   * sorted element lists, so a parallel walk pairs each element with its
   * counterpart. [Effect.guards_equal] covers every guard-bearing payload,
   * not just the effect-level guard. *)
  let equal_with_guards s1 s2 =
    equal s1 s2
    && List.for_all2 Effect.guards_equal (elements s1) (elements s2)

  let show ?(truncate_guards = true) s =
    s |> to_seq |> List.of_seq
    |> List_.map (Effect.show ~truncate_guards)
    |> String.concat "; "

  let add_list elts t = List.fold_left (fun set e -> add e set) t elts
  let union_list ts = List.fold_left union empty ts
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
  (** A simplified version of 'AST_generic.parameter', we use 'Other' to
      represent parameter kinds that we do not support yet. We don't want to
      just remove those unsupported parameters because we rely on the position
      of a parameter to represent taint variables, see 'Taint.arg'. *)
  type param =
    | P of string
    | POpt of string
    | PRest of string
    | PKwd of string
    | Other
  [@@deriving eq, ord, show]

  type params = param list [@@deriving eq, ord]

  type t = {
    params : params;
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
    effects : Effects.t;
    closures : Shape.closure ClosureMap.t;
        (** The closures that the [Closed] lambda references in [effects]
            refer to, by lambda: their captured variables, as values of the
            inputs of the function. *)
  }
  (** * The 'params' act like an universal quantifier, we need them to later *
      instantiate the accompanying signature. *)

  val equal : t -> t -> bool

  val equal_with_guards : t -> t -> bool
  (** Like [equal] but a difference only in a guard counts as a difference
      ([Effects.equal_with_guards]). For the dataflow fixpoint's stability
      test, via [Shape.equal_cell_with_guards] on [Fun] shapes. *)

  val compare : t -> t -> int
  val of_IL_params : IL.param list -> params
  val show_params : params -> string
  val show : ?truncate_guards:bool -> t -> string
end = struct
  (*************************************)
  (* Param(eter)s *)
  (*************************************)

  (* TODO: Now with HOFs we run the risk of shadowing... *)
  type param =
    | P of string
    | POpt of string (* a parameter that has a default *)
    | PRest of string
    | PKwd of string (* takes a named argument only: Ruby 'sep:' *)
    | Other [@@deriving eq, ord, show]
  type params = param list

  let show_param = function
    | P s -> s
    | POpt s -> s ^ "=_"
    | PRest s -> "*" ^ s (* Python syntax for "rest" params *)
    | PKwd s -> s ^ ":"
    | Other -> "_?"

  let equal_params params1 params2 = List.equal equal_param params1 params2

  let compare_params params1 params2 =
    List.compare compare_param params1 params2

  let show_params params = params |> List_.map show_param |> String.concat ", "

  let of_IL_params il_params =
    il_params
    |> List.filter (function
         | IL.ParamReceiver _ -> false
         | _ -> true)
    |> List_.map (function
         | IL.Param { pname = { ident = s, _; _ }; pdefault = Some _; _ } -> POpt s
         | IL.Param { pname = { ident = s, _; _ }; pdefault = None; _ } -> P s
         (* function signatures don't look into the shape of the argument. *)
         | IL.ParamRest { pname = { ident = s, _; _ }; _ } -> PRest s
         | IL.ParamKwd { pname = { ident = s, _; _ }; _ } -> PKwd s
         | IL.ParamPattern ({ pname = { ident = s, _; _ }; _ }, pat) -> (
             match pat with
             | AST_generic.PatId (name, _) -> P (fst name)
             | AST_generic.PatTyped (AST_generic.PatId (name, _), _) ->
                 P (fst name)
             | _ -> P s)
         | IL.ParamReceiver _ -> Other (* filtered above *)
         | IL.ParamFixme -> Other)

  (*************************************)
  (* Signatures *)
  (*************************************)

  type t = {
    params : params;
    params_il : IL.param list;
    effects : Effects.t;
    closures : Shape.closure ClosureMap.t;
  }

  (* [params_il] is instantiation metadata; identity is determined by
     [params], [effects] and [closures] alone. *)
  let equal { params = params1; params_il = _; effects = effects1; closures = closures1 }
      { params = params2; params_il = _; effects = effects2; closures = closures2 } =
    equal_params params1 params2 && Effects.equal effects1 effects2
    && ClosureMap.equal Shape.equal_closure closures1 closures2

  let equal_with_guards
      { params = params1; params_il = _; effects = effects1; closures = closures1 }
      { params = params2; params_il = _; effects = effects2; closures = closures2 } =
    equal_params params1 params2
    && Effects.equal_with_guards effects1 effects2
    && ClosureMap.equal Shape.equal_closure_with_guards closures1 closures2

  let compare { params = params1; params_il = _; effects = effects1; closures = closures1 }
      { params = params2; params_il = _; effects = effects2; closures = closures2 } =
    match compare_params params1 params2 with
    | 0 -> (
        match Effects.compare effects1 effects2 with
        | 0 -> ClosureMap.compare Shape.compare_closure closures1 closures2
        | other -> other)
    | other -> other

  let show ?(truncate_guards = true) { params; params_il = _; effects; closures } =
    spf "%s => {%s}%s" (show_params params)
      (Effects.show ~truncate_guards effects)
      (if ClosureMap.is_empty closures then ""
       else
         " closures {"
         ^ (ClosureMap.bindings closures
           |> List.map (fun (((id, _) : Closure_key.t), (closure : Shape.closure)) ->
                  Function_id.show id ^ Shape.show_closure closure)
           |> String.concat "; ")
         ^ "}")
end

(*****************************************************************************)
(* Signature Database *)
(*****************************************************************************)

(* Function key for the signature database - uses just the function name (last element of fn_id).
   This matches the graph vertex type in Call_graph.ml. *)
type func_key = Function_id.t

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

type signature_database = {
  signatures : SignatureSet.t FunctionMap.t;
  lambda_signatures : Signature.t Lazy.t FunctionMap.t;
      (** The signatures of the lambdas of the function under analysis,
          extracted when first read. A lambda has one signature. *)
  getters : Function_id.t Member_map.t;
      (** The methods that a read of the attribute of an instance of the
          class calls ([Lang_config.getter_decorators]). *)
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
let find_by_arity (sigs : SignatureSet.t) (arity : int) : Signature.t option =
  if Int.equal (SignatureSet.cardinal sigs) 1 then
    Some (SignatureSet.choose sigs).sig_
  else
    let exact =
      SignatureSet.filter
        (fun (x : extended_sig) -> equal_sig_arity x.arity (Arity_exact arity))
        sigs
    in
    if Int.equal (SignatureSet.cardinal exact) 1 then
      Some (SignatureSet.choose exact).sig_
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
      |> Option.map (fun (x : extended_sig) -> x.sig_)

let lookup_builtin_signature (db : builtin_signature_database)
    (func_name : string) (arity : int) : Signature.t option =
  match BuiltinMap.find_opt func_name db with
  | Some sigs when not (SignatureSet.is_empty sigs) ->
    (* NOTE: We do not use [find_by_arity sigs arity] because for built-ins we require an exact
       arity match. *)
      let filtered_sigs =
        SignatureSet.filter (fun x -> equal_sig_arity x.arity (Arity_exact arity)) sigs
      in
      let signatures_card = SignatureSet.cardinal filtered_sigs in
      if Int.equal signatures_card 1 then
        Some (SignatureSet.choose filtered_sigs).sig_
      else None
  | _ -> None

let show_name (name_opt : IL.name option) =
  match name_opt with
  | Some name -> IL.show_ident name.ident
  | None -> ""

let empty_signature_database () : signature_database =
  {
    signatures = FunctionMap.empty;
    lambda_signatures = FunctionMap.empty;
    getters = Member_map.empty;
  }

let add_getter (db : signature_database) ~(class_name : string) ~(attribute : string)
    (getter : Function_id.t) : signature_database =
  { db with getters = Member_map.add (class_name, attribute) getter db.getters }

let find_getter (db : signature_database) ~(class_name : string) ~(attribute : string) :
    Function_id.t option =
  Member_map.find_opt (class_name, attribute) db.getters

let lookup_signature (db : signature_database) (name : Function_id.t)
    (arity : int) : Signature.t option =
  match FunctionMap.find_opt name db.signatures with
  | Some sigs when not (SignatureSet.is_empty sigs) ->
      find_by_arity sigs arity
  | _ ->
      (* [find_by_arity] gives the single signature of a name at any arity. *)
      FunctionMap.find_opt name db.lambda_signatures |> Option.map Lazy.force

let has_signature (db : signature_database) (name : Function_id.t) : bool =
  FunctionMap.mem name db.signatures || FunctionMap.mem name db.lambda_signatures

let lambda_signature (db : signature_database) (name : Function_id.t) :
    Signature.t Lazy.t option =
  FunctionMap.find_opt name db.lambda_signatures

let add_lambda_signature (db : signature_database) (name : Function_id.t)
    (signature : Signature.t Lazy.t) : signature_database =
  { db with lambda_signatures = FunctionMap.add name signature db.lambda_signatures }

let lookup_all_signatures (db : signature_database) (name : Function_id.t)
    : extended_sig list =
  match FunctionMap.find_opt name db.signatures with
  | Some sigs -> SignatureSet.elements sigs
  | None -> []

(* The latest extraction replaces the signature of its arity: a function has
   one signature per arity ([find_by_arity] cannot choose between two), and in
   a fixpoint the newest is the one to keep. *)
let add_signature (db : signature_database) (name : Function_id.t)
    (signature : extended_sig) : signature_database =
  let signatures =
    FunctionMap.update name
      (fun existing_sigs ->
        match existing_sigs with
        | Some sigs ->
            Some
              (SignatureSet.add signature
                 (SignatureSet.filter
                    (fun (x : extended_sig) ->
                      not (equal_sig_arity x.arity signature.arity))
                    sigs))
        | None -> Some (SignatureSet.singleton signature))
      db.signatures
  in
  { db with signatures }

(* Unlike add_signature, discards any prior entries for [name]. *)
let replace_signature (db : signature_database) (name : Function_id.t)
    (signature : extended_sig) : signature_database =
  let signatures =
    FunctionMap.add name (SignatureSet.singleton signature) db.signatures
  in
  { db with signatures }

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
