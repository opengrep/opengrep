(* Iago Abal
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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

module G = AST_generic
module PM = Core_match
module R = Rule
module EG = Effect_guard
module LabelSet = Set.Make (String)
module Log = Log_tainting.Log
open Common

(* TODO: Consider renaming the `show_xyz` functions to `pretty_xyz` and
 * generate `show_xyz` ones via `deriving show`. *)

(* NOTE "on compare functions":
 *
 * We should be careful about using ppx_deriving. The problem is having "automagic"
 * comparisons that silently change as you change data types. Automagic comparisons
 * are very convenient but sometimes the comparison is not what you want. Initially
 * I just thought that if you carefully considered whether that automagic comparison
 * is OK for each data type then you are fine... So we used `Stdlib.compare` in several
 * places, until one day `Taint.arg` evolved, and we added an `offset` to it, and we
 * forgot to revisit whether `Stdlib.compare` was still a good option (it wasn't)...
 * and this led to duplicates which led to an explosion in the size of taint sets and
 * to big perf problems. I think the only way to get warned about this is to write these
 * comparisons manually, no matter how painful it is, so the typechecker will force
 * you to revisit the comparison functions if the types change. It's safe to use
 * ppx_deriving when all the types are primitive, but if any is not then it can cause
 * these problems.
 *
 * Besides, we now favor the use of pattern matching over record field access, e.g.
 * ```ocaml
 * let compare_sink { pm = pm1; rule_sink = sink1 } { pm = pm2; rule_sink = sink2 }
 *     =
 *   Stdlib.compare
 *     (sink1.Rule.sink_id, pm1.rule_id, pm1.range_loc, pm1.env)
 *     (sink2.Rule.sink_id, pm2.rule_id, pm2.range_loc, pm2.env)
 * ```
 * instead of
 * ```ocaml
 * let compare_sink sink1 sink2
 *      =
 *   Stdlib.compare
 *     (sink1.rule_sink.Rule.sink_id, sink1.pm.rule_id, ...)
 *     (sink2.rule_sink.Rule.sink_id, sink2.pm.rule_id, ...)
 * ```
 * If we pattern-match, and later we add new fields to the record, the typechecker
 * will let us know that we need to revisit `compare_sink`---this does not happen
 * when you use dot accesses.
 *)

(*****************************************************************************)
(* Call traces *)
(*****************************************************************************)

type tainted_token = G.tok [@@deriving show]
type tainted_tokens = tainted_token list [@@deriving show]
(* TODO: Given that the analysis is path-insensitive, the trace should capture
 * all potential paths. So a set of tokens seems more appropriate than a list.
 * TODO: May have to annotate each tainted token with a `call_trace` that explains
 * how it got tainted, so it may help triaging. For example, if we got
 * `x = f(tainted)`, it may be interesting to see how is `f` propagating the
 * taint from its input.
 *)

type 'a call_trace =
  | PM of PM.t * 'a
  | Call of G.expr * tainted_tokens * 'a call_trace

let length_of_call_trace ct =
  let rec loop acc = function
    | PM _ -> acc
    | Call (_, _, ct') -> loop (acc + 1) ct'
  in
  loop 0 ct

let compare_metavar_env env1 env2 =
  (* Returns 0 only for bindings that [Metavariable.equal_bindings] treats
     as equal. Otherwise, there will be many duplicates. *)
  Metavariable.compare_bindings env1 env2

(* The byte offsets of the matches are compared first: the rule and the file,
   compared as strings, are almost always the same for two taints of one
   analysis. *)
let compare_matches pm1 pm2 =
  if phys_equal pm1 pm2 then 0
  else
    let (start1, end1), (start2, end2) = (pm1.PM.range_loc, pm2.PM.range_loc) in
    match Int.compare start1.Tok.pos.bytepos start2.Tok.pos.bytepos with
    | 0 -> (
        match Int.compare end1.Tok.pos.bytepos end2.Tok.pos.bytepos with
        | 0 -> (
            match Rule_ID.compare pm1.PM.rule_id.id pm2.PM.rule_id.id with
            | 0 -> (
                match Core_match.compare_range_loc pm1.range_loc pm2.range_loc with
                | 0 -> compare_metavar_env pm1.env pm2.env
                | other -> other)
            | other -> other)
        | other -> other)
    | other -> other

let rec pm_of_trace = function
  | PM (pm, x) -> (pm, x)
  | Call (_, _, trace) -> pm_of_trace trace

let trace_of_pm (pm, x) = PM (pm, x)

let rec show_call_trace show_thing = function
  | PM (pm, x) ->
      let matched_str =
        let tok1, tok2 = pm.range_loc in
        let r = Range.range_of_token_locations tok1 tok2 in
        Range.content_at_range pm.path.internal_path_to_content r
      in
      let matched_line =
        let loc1, _ = pm.range_loc in
        loc1.Tok.pos.line
      in
      Printf.sprintf "%s at l.%d [%s]" matched_str matched_line (show_thing x)
  | Call (_e, _, trace) ->
      Printf.sprintf "call to %s -> ... %s"
        (Pretty_print_AST.expr_to_string Lang.Java _e)
        (show_call_trace show_thing trace)

(*****************************************************************************)
(* Taint arguments ("variables", kind of) *)
(*****************************************************************************)

type arg = { name : string; index : int } [@@deriving eq, ord]
type base = BGlob of IL.name | BThis | BArg of arg | BCapture of IL.name
[@@deriving ord]

(* [Oslice n] mirrors [IL.Slice n]: the trailing-rest of a list/tuple
 * scrutinee starting at index [n]. Reading element [k] of a slice
 * resolves to the source's index [n + k]; the engine collapses
 * [Oint k :: Oslice n :: rest] to [Oint (n+k) :: rest] and
 * [Oslice b :: Oslice a :: rest] to [Oslice (a+b) :: rest]. *)
type offset =
  | Ofld of IL.name
  | Oint of int
  | Ostr of string
  | Oslice of int
  | Oany
  | Ofld_any
[@@deriving eq, ord]

type lval = { base : base; offset : offset list }

let compare_lval { base = base1; offset = offset1 }
    { base = base2; offset = offset2 } =
  match compare_base base1 base2 with
  | 0 -> List.compare compare_offset offset1 offset2
  | other -> other

let equal_lval (lval1 : lval) (lval2 : lval) : bool =
  Int.equal (compare_lval lval1 lval2) 0

module Base_map = Map.Make (struct
  type t = base

  let compare = compare_base
end)

(* The paths of an input below it. *)
module Path_set = Set.Make (struct
  type t = offset list

  let compare = List.compare compare_offset
end)

module Lval_map = Map.Make (struct
  type t = lval

  let compare = compare_lval
end)

let show_arg { name = s; index = i } = Printf.sprintf "arg(%s#%d)" s i

let show_base base =
  match base with
  | BGlob name -> fst name.ident
  | BThis -> "this"
  | BArg arg -> show_arg arg
  | BCapture name -> "capture(" ^ fst name.ident ^ ")"

type formal = Param of arg | Captured of IL.name [@@deriving ord]

let equal_formal (formal1 : formal) (formal2 : formal) : bool =
  Int.equal (compare_formal formal1 formal2) 0

let base_of_formal (formal : formal) : base =
  match formal with
  | Param arg -> BArg arg
  | Captured var -> BCapture var

let show_formal (formal : formal) : string = show_base (base_of_formal formal)

let show_offset offset =
  match offset with
  | Ofld n -> "." ^ fst n.IL.ident
  | Oint i -> Printf.sprintf "[%d]" i
  | Ostr s -> Printf.sprintf "[%s]" s
  | Oslice n -> Printf.sprintf "[%d..]" n
  | Oany -> "[*]"
  | Ofld_any -> ".*"

let show_offset_list offset =
  offset |> List_.map show_offset |> String.concat ""

let show_lval { base; offset } = show_base base ^ show_offset_list offset

let offset_of_IL lang (o : IL.offset) =
match o.o with
| IL.Dot n -> Ofld n
| IL.Index { e = IL.Literal (Int pi); _ } -> (
    match Parsed_int.to_int_opt pi with
    | Some i -> Oint i
    | None -> Oany)
(* Several frontends emit an integral index as a Float rather than an Int:
 * JS/TS have no integer literals at all, and Solidity, Lua and R map every
 * decimal numeric literal to a Float. Such an index would degrade to the
 * Oany wildcard, which makes every constant index alias every other one.
 * Accept a float that denotes a representable integer.
 *
 * The bound is strict: [Float.of_int Int.max_int] rounds *up* to 2^62,
 * which is one past [max_int], so [<=] would admit 2^62 and
 * [int_of_float] would wrap it to a negative offset. *)
| IL.Index { e = IL.Literal (Float (Some f, _)); _ }
  when Float.is_integer f && Float.abs f < Float.of_int Int.max_int ->
    Oint (int_of_float f)
(* In JS/TS `o[0]` and `o["0"]` are the same property: an integer key is
 * canonicalized to its decimal string form. Map a string index that
 * is in such a canonical form to the [Oint] the numeric form takes, so
 * both sides unify. Non-canonical forms (`"00"`, `"-0"`, `"0x10"`)
 * name distinct properties in JS too and stay [Ostr]. Restricted to
 * integers that survive the float round-trip: past 2^53 the numeric key
 * collapses to a neighbouring integer while the string form remains
 * its own property. Every other language keeps the distinction — in
 * Python `d[0]` and `d["0"]` really are different dict keys. *)
| IL.Index { e = IL.Literal (String (_, (s, _), _)); _ }
  when Lang.is_js lang -> (
    match int_of_string_opt s with
    | Some i when string_of_int i = s && int_of_float (float_of_int i) =|= i
      ->
        Oint i
    | _ -> Ostr s)
| IL.Index { e = IL.Literal (String (_, (s, _), _)); _ } -> Ostr s
| IL.Index { e = IL.Literal (Atom (_, (s, _))); _ } -> Ostr s
| IL.Index _ -> Oany
| IL.DotDynamic _ -> Ofld_any
| IL.Slice n -> Oslice n

let offset_of_rev_IL_offset lang ~rev_offset =
  List.rev_map (offset_of_IL lang) rev_offset

let rev_IL_offset_of_offset offset =
  let os =
    offset
    |> List_.map (function
         | Ofld x -> Some IL.{ o = Dot x; oorig = NoOrig }
         | Oint i ->
             Some
               {
                 o =
                   Index
                     {
                       e = Literal (G.Int (Parsed_int.of_int i));
                       eorig = NoOrig;
                     };
                 oorig = NoOrig;
               }
         | Ostr s ->
             Some
               {
                 o =
                   Index
                     {
                       e =
                         Literal
                           (G.String
                              (Tok.unsafe_fake_bracket
                                 (s, Tok.unsafe_fake_tok s)));
                       eorig = NoOrig;
                     };
                 oorig = NoOrig;
               }
         | Oslice n -> Some IL.{ o = Slice n; oorig = NoOrig }
         | Oany
         | Ofld_any ->
             None)
  in
  os
  |> List.fold_left
       (fun acc opt_o ->
         match (acc, opt_o) with
         | Some acc, Some o -> Some (o :: acc)
         | _, None
         | None, _ ->
             None)
       (Some [])

let lval_of_arg arg = { base = BArg arg; offset = [] }

(*****************************************************************************)
(* Taint *)
(*****************************************************************************)

type source = {
  call_trace : R.taint_source call_trace;
  origin : PM.t * R.taint_source;
      (* [pm_of_trace call_trace]: a call pushed onto the trace keeps it. *)
  label : string;
      (* This is needed because we may change the label of a taint,
         from the original source that it came from.
         This happens from propagators which change the label of the taint.
         We don't put it under `taint`, though, because Arg and Control taints
         are polymorphic in label.
      *)
  precondition : (taint list * R.precondition) option;
  group : Metavariable.bindings option;
      (* [Taint.mli]. *)
}

and source_grouping = By_message | By_bindings | Each_source

and orig =
  | Src of source
  | Var of lval
  | Shape_var of lval
  | Call_result of call_result
  | Control

and call_result = { callee : lval; args : taint list IL.argument list }
(* THINK(iago): We could group 'Var', 'Shape_var' and 'Control' into a single
 * 'Var' with a "kind" parameter. But we need to be careful about perf when
 * adding an extra level of indirection here (due to memory allocations). *)

and taint = { orig : orig; tokens : tainted_tokens }

let compare_precondition (_ts1, f1) (_ts2, f2) =
  (* We don't consider the "incoming" taints here, assuming both
     preconditions come from otherwise the same source.
     See 'pick_taint' below for details. *)
  R.compare_precondition f1 f2

let compare_source
    { origin = pm1, ts1; label = label1; precondition = precondition1; _ }
    { origin = pm2, ts2; label = label2; precondition = precondition2; _ } =
  (* Comparing metavariable environments this way is not robust, e.g.:
   * [("$A",e1);("$B",e2)] is not considered equal to [("$B",e2);("$A",e1)].
   * For our purposes, this is OK.
   *)
  match compare_matches pm1 pm2 with
  | 0 -> (
      match
        match String.compare label1 label2 with
        | 0 -> String.compare ts1.R.label ts2.R.label
        | other -> other
      with
      | 0 -> (
          (* It's important that we include preconditions as a distinguishing factor
             between two taints.

             Otherwise, suppose that we had a taint with label A with precondition `false`
             and one with precondition `true`. Obviously, only one actually exists. But
             if we pick the wrong one, we might fallaciously say a taint label finding does
             not actually occur.
          *)
          match (precondition1, precondition2) with
          | None, _
          | _, None ->
              (* 'None' here is the same as 'true', although the `requires` of both taints
                 * may not be the same, in this specific case we consider them "the same",
                 * see 'pick_best_taint'. *)
              0
          | Some pre1, Some pre2 -> compare_precondition pre1 pre2)
      | other -> other)
  | other -> other

let rec compare_orig orig1 orig2 =
  match (orig1, orig2) with
  | Src p, Src q -> compare_source p q
  | Var lv1, Var lv2 -> compare_lval lv1 lv2
  | Shape_var lv1, Shape_var lv2 -> compare_lval lv1 lv2
  | Call_result c1, Call_result c2 -> (
      match compare_lval c1.callee c2.callee with
      | 0 -> List.compare (IL.compare_argument_by_name (List.compare compare_taint)) c1.args c2.args
      | other -> other)
  | Control, Control -> 0
  (* constructor "smaller than" ... *)
  | Src _, (Var _ | Shape_var _ | Call_result _ | Control) -> -1
  | Var _, (Shape_var _ | Call_result _ | Control) -> -1
  | Shape_var _, (Call_result _ | Control) -> -1
  | Call_result _, Control -> -1
  (* constructor "greater than" ... *)
  | Var _, Src _ -> 1
  | Shape_var _, (Var _ | Src _) -> 1
  | Call_result _, (Shape_var _ | Var _ | Src _) -> 1
  | Control, (Call_result _ | Shape_var _ | Var _ | Src _) -> 1

and compare_taint taint1 taint2 =
  (* THINK: Right now we disregard the trace because we just want to keep one
   * potential path. *)
  if phys_equal taint1 taint2 then 0 else compare_orig taint1.orig taint2.orig

let rec show_precondition = function
  | R.PLabel str -> str
  | R.PVariable var -> "var:" ^ var
  | R.PBool b -> Bool.to_string b
  | R.PNot p -> Printf.sprintf "not %s" (show_precondition p)
  | R.PAnd [ p1; p2 ] ->
      Printf.sprintf "(%s and %s)" (show_precondition p1) (show_precondition p2)
  | R.PAnd _ -> "(and ...)"
  | R.POr _ -> "(or ...)"

let rec show_source { call_trace; label; precondition; _ } =
  (* We want to show the actual label, not the originating label.
     This may change, for instance, if we have ever propagated this taint to
     a different label.
  *)
  let pm, ts = pm_of_trace call_trace in
  let matched_str =
    let tok1, tok2 = pm.range_loc in
    let r = Range.range_of_token_locations tok1 tok2 in
    Range.content_at_range pm.path.internal_path_to_content r
  in
  let matched_line =
    let loc1, _ = pm.range_loc in
    loc1.Tok.pos.line
  in
  let num_calls = length_of_call_trace call_trace in
  let num_calls_str =
    if num_calls =|= 0 then "" else Printf.sprintf "%d> " num_calls
  in
  let label_str =
    if label = R.default_source_label then ""
    else if label = ts.label then Printf.sprintf " :%s" label
    else Printf.sprintf " :%s->%s" ts.label label
  in
  let precondition_str = show_taints_with_precondition precondition in
  Printf.sprintf "[%s%s :l.%d%s%s]" num_calls_str matched_str matched_line
    label_str precondition_str

and show_taints_with_precondition precondition =
  match precondition with
  | None -> ""
  | Some (ts, pre) ->
      Common.spf "{/PRE|%s|if %s/}"
        (List_.map show_taint ts |> String.concat " + ")
        (show_precondition pre)

and show_taint taint =
  match taint.orig with
  | Src src -> show_source src
  | Var lval -> "'" ^ show_lval lval
  | Shape_var lval -> "'<" ^ show_lval lval ^ ">"
  | Call_result { callee; args } ->
      Printf.sprintf "'%s(%s)" (show_lval callee)
        (args
        |> List_.map (fun (arg : taint list IL.argument) ->
               let show (taints : taint list) = List_.map show_taint taints |> String.concat " + " in
               match arg with
               | IL.Unnamed taints -> show taints
               | IL.Named ((name, _), taints) -> name ^ "=" ^ show taints)
        |> String.concat ", ")
  | Control -> "'<control>"

(*****************************************************************************)
(* Taint sets *)
(*****************************************************************************)

type guarded_taint = { taint : taint; guard : EG.t }

let lift_taint (t : taint) : guarded_taint = { taint = t; guard = EG.top }

let with_guard (g : EG.t) (gt : guarded_taint) : guarded_taint =
  { gt with guard = EG.compose_and gt.guard g }

(* A label, a guard and the bindings of the metavariables that a rule's
   message refers to: sources of one class give the same findings. *)
module Source_class_map = Map.Make (struct
  type t = string * EG.t * Metavariable.bindings

  let compare ((label1, guard1, env1) : t) ((label2, guard2, env2) : t) : int =
    match String.compare label1 label2 with
    | 0 -> (
        match EG.compare guard1 guard2 with
        | 0 -> compare_metavar_env env1 env2
        | other -> other)
    | other -> other
end)

module Taint_set = struct
  (* NOTE "Taint sets"
   *
   * Why not just 'Set.Make(...)' using `compare_taint`? If we did this, then
   * given two taints that we consider "the same", we would pick one arbitrarily.
   * While we consider them _essentially_ "the same", there are details such as
   * the call trace or the precondition (for labeled taint) that may differ, and
   * we want to pick "the best". This is what this data structure is for, the
   * key functions are 'add' and 'pick_best_taint'.
   *
   * Each element is a [guarded_taint] carrying the taint plus the
   * [Effect_guard.t] under which that taint is live. Identity is the
   * taint only ([compare_taint]), so when a bundle with the same taint
   * identity but a different guard is added, [add] fuses guards via
   * [EG.compose_or] and picks the best taint via [pick_best_taint].
   *
   * Represented as a [Map] from taint identity to bundle rather than a
   * [Set] of bundles: the merge-on-collision access pattern is then one
   * [Taints.update] descent (instead of find + remove + add), and
   * [union] is the hedge [Map.union], which links non-overlapping
   * subtrees wholesale instead of re-inserting every element — the
   * dominant cost on big env joins and instantiation unions, where the
   * two sides share most of their structure. The key is a
   * representative taint of the equivalence class; merging may store a
   * "better" taint in the bundle without rekeying (both compare equal
   * by [compare_taint]). *)
  module Taints = Map.Make (struct
    type t = taint

    let compare = compare_taint
  end)

  type t = guarded_taint Taints.t

  let empty = Taints.empty
  let is_empty set = Taints.is_empty set
  let cardinal set = Taints.cardinal set

  (* Equality/order on the taint identities (keys) only, matching the
     previous [Set.Make] over taint-only bundle compare: guards and
     trace details do not participate. *)
  let equal set1 set2 = phys_equal set1 set2 || Taints.equal (fun _ _ -> true) set1 set2

  let compare set1 set2 =
    if phys_equal set1 set2 then 0 else Taints.compare (fun _ _ -> 0) set1 set2
  let to_seq set = Taints.to_seq set |> Seq.map snd
  let elements set = set |> to_seq |> List.of_seq

  (* Like [equal] but also requires the guards of identity-equal taints
   * to be equal (the map keys are taint identities only ([compare_taint]),
   * so plain [equal] treats a guard-only refinement of a taint as
   * unchanged). Stability and insertion no-op checks over guard-bearing
   * sets must use this, or a fused wider guard is discarded for the
   * narrower one. *)
  let equal_with_guards set1 set2 =
    equal set1 set2
    && List.for_all2
         (fun (b1 : guarded_taint) (b2 : guarded_taint) ->
           EG.equal b1.guard b2.guard)
         (elements set1) (elements set2)

  (* If two taints are "the same", we still want to pick "the best", e.g.
   * the one with the shortest trace.
   *
   * This also helps avoiding infinite loops, which can happen when inferring
   * taint signatures for functions like this:
   *
   *     f(tainted) {
   *         while (true) {
   *             x = g(tainted, f(tainted));
   *             if (true) return x;
   *         }
   *     }
   *
   * Intuitively `f` propagates taint from its input to its output, and with every
   * iteration we have a "new" taint source made by the tainted input passing N
   * times through `f`, and so the fixpoint computation diverges. This is actually
   * rather tricky and removing the `if (true)` or the `g` breaks the infinite loop,
   * but this has not been investigated in detail. (TODO)
   *
   * THINK: We could do more clever things like checking whether a trace is an
   *   extension of another trace and such. This could also be dealt with in the
   *   taint-signatures themselves. But for now this solution is good.
   *
   * coupling: If this changes, make sure to update docs for the `Taint.signature` type.
   *)
  let rec add alt_bundle set =
    Taints.update alt_bundle.taint
      (function
        | None -> Some alt_bundle
        | Some curr_bundle -> Some (merge_bundles alt_bundle curr_bundle))
      set

  (* Merge two bundles with the same taint identity: best taint by the
   * shortest-trace rule, guards fused disjunctively. Returns
   * [curr_bundle] physically when nothing changes, so [Taints.update]
   * returns the map unchanged. (For [Src] taints [pick_best_taint]
   * rebuilds the source to merge preconditions, so [best_taint] is never
   * physically [curr_bundle.taint] and this shortcut only fires for
   * [Var]/[Shape_var]/[Control].) *)
  and merge_bundles alt_bundle curr_bundle =
    let best_taint = pick_best_taint alt_bundle.taint curr_bundle.taint in
    let merged_guard = EG.compose_or alt_bundle.guard curr_bundle.guard in
    if
      Common.phys_equal best_taint curr_bundle.taint
      && EG.equal merged_guard curr_bundle.guard
    then curr_bundle
    else { taint = best_taint; guard = merged_guard }

  (* Hedge union: non-overlapping subtrees are linked without visiting
   * their elements; colliding keys merge like [add] (set1 is the
   * incoming side, matching the previous [fold add set1 set2]). *)
  and union set1 set2 = union_unbounded set1 set2 |> bound

  and union_unbounded set1 set2 =
    Taints.union (fun _taint b1 b2 -> Some (merge_bundles b1 b2)) set1 set2

  (* Each bound looks at the whole set only, so the bounds give one set
     whether they are applied after every union or once after several. *)
  and bound (set : t) : t =
    set |> bound_sources |> bound_placeholders |> bound_call_results

  (* A set keeps the taint variables ([Var], [Shape_var]) of at most
     [Limits_semgrep.taint_MAX_PLACEHOLDERS_PER_INPUT] paths of each input,
     the two variables of a path counting once; past it, those of the input become
     the variables of the input itself, [Var] for the taint of its value and
     [Shape_var] for every taint below it, which hold them all at a coarser
     path ([Sig_inst.instantiate_taint_var]). A set that has [Shape_var] of
     the input itself keeps no other variable of the input at a non-empty
     path. *)
  and bound_placeholders (set : t) : t =
    (* The taint variables that count towards the bound of their input: all
       but [Var] of the input itself, with whether it is [Shape_var] of the
       input itself. *)
    let counted (key : taint) : (base * offset list) option =
      match key.orig with
      | Var { offset = []; _ }
      | Src _
      | Call_result _
      | Control ->
          None
      | Var { base; offset }
      | Shape_var { base; offset } ->
          Some (base, offset)
    in
    (* Taint variables come after the sources and before the other taints
       in the order of the set ([compare_orig]). *)
    match
      Taints.find_first_opt
        (fun (key : taint) ->
          match key.orig with
          | Src _ -> false
          | Var _
          | Shape_var _
          | Call_result _
          | Control ->
              true)
        set
    with
    | None -> set
    | Some (first, _) ->
        (* By input, the paths of its counted variables and their guards;
           the empty path is that of [Shape_var] of the input itself. *)
        let counts : (Path_set.t * EG.t) Base_map.t =
          Taints.to_seq_from first set
          |> Seq.take_while (fun ((key, _) : taint * guarded_taint) ->
                 match key.orig with
                 | Var _
                 | Shape_var _ ->
                     true
                 | Src _
                 | Call_result _
                 | Control ->
                     false)
          |> Seq.fold_left
               (fun (counts : (Path_set.t * EG.t) Base_map.t)
                    ((key, b) : taint * guarded_taint) ->
                 match counted key with
                 | None -> counts
                 | Some (base, path) ->
                     Base_map.update base
                       (function
                         | None -> Some (Path_set.singleton path, b.guard)
                         | Some (paths, guard) ->
                             Some (Path_set.add path paths, EG.compose_or guard b.guard))
                       counts)
               Base_map.empty
        in
        (* The variable of the whole value of an input holds every other
           variable of the input, so a set that has it keeps no other. *)
        let collapsed =
          Base_map.filter
            (fun (_ : base) ((paths, _) : Path_set.t * EG.t) ->
              let n = Path_set.cardinal paths in
              n > Limits_semgrep.taint_MAX_PLACEHOLDERS_PER_INPUT
              || (Path_set.mem [] paths && n > 1))
            counts
        in
        if Base_map.is_empty collapsed then set
        else
          Base_map.fold
            (fun (base : base) ((_, guard) : Path_set.t * EG.t) (acc : t) ->
              acc
              |> add { taint = { orig = Var { base; offset = [] }; tokens = [] }; guard }
              |> add { taint = { orig = Shape_var { base; offset = [] }; tokens = [] }; guard })
            collapsed
            (Taints.filter
               (fun (key : taint) (_ : guarded_taint) ->
                 match counted key with
                 | Some (base, _) -> not (Base_map.mem base collapsed)
                 | None -> true)
               set)

  (* A set keeps at most [Limits_semgrep.taint_MAX_SOURCES_PER_LABEL] sources
     without a precondition of each label, guard and group, the least in the
     order of the set: a sink needs one source of a label under a guard, and
     sources of different groups give different findings, so the others only
     add alternative traces. A source that gives a finding of its own (no
     group) is kept. *)
  and bound_sources (set : t) : t =
    (* A set of at most [Limits_semgrep.taint_MAX_SOURCES_PER_LABEL] sources is within the bound
       whatever their labels; the walk stops at the first source past it. *)
    let rec more_sources_than (n : int) (seq : (taint * guarded_taint) Seq.t) : bool =
      match seq () with
      | Seq.Nil -> false
      | Seq.Cons ((_, (b : guarded_taint)), rest) -> (
          match b.taint.orig with
          | Src _ -> n <= 0 || more_sources_than (n - 1) rest
          | Var _
          | Shape_var _
          | Call_result _
          | Control ->
              false)
    in
    if not (more_sources_than Limits_semgrep.taint_MAX_SOURCES_PER_LABEL (Taints.to_seq set)) then set
    else
      (* By label, guard and group, the number of sources without a
         precondition kept. *)
      Taints.to_seq set
      |> Seq.take_while (fun ((key, _) : taint * guarded_taint) ->
             match key.orig with
             | Src _ -> true
             | Var _
             | Shape_var _
             | Call_result _
             | Control ->
                 false)
      |> Seq.fold_left
           (fun ((counts, kept) : int Source_class_map.t * t)
                ((key, (b : guarded_taint)) : taint * guarded_taint) ->
             match key.orig with
             (* Which of the sources with a precondition holds is known only
                where the precondition is solved, so they are kept. *)
             | Src { precondition = Some _; _ }
             | Src { group = None; _ } ->
                 (counts, kept)
             | Src ({ precondition = None; group = Some group; _ } as src) ->
                 let source_class = (src.label, b.guard, group) in
                 let n = Source_class_map.find_opt source_class counts |> Option.value ~default:0 in
                 if n >= Limits_semgrep.taint_MAX_SOURCES_PER_LABEL then
                   (counts, Taints.remove key kept)
                 else (Source_class_map.add source_class (n + 1) counts, kept)
             | Var _
             | Shape_var _
             | Call_result _
             | Control ->
                 (counts, kept))
           (Source_class_map.empty, set)
      |> snd

  (* A set keeps one [Call_result] of each callee: the result of a call grows
     with its arguments, so one result whose arguments are those of the others,
     united position by position, holds them all. *)
  and bound_call_results (set : t) : t =
    (* The results come after the taint variables in the order of the set
       ([compare_orig]). *)
    match
      Taints.find_first_opt
        (fun (key : taint) ->
          match key.orig with
          | Src _
          | Var _
          | Shape_var _ ->
              false
          | Call_result _
          | Control ->
              true)
        set
    with
    | None -> set
    | Some (first, _) ->
        (* Positional arguments of one position unite, and named arguments
           of one name, as a call matches them to the parameters
           ([Sig_inst.find_pos_in_actual_args]); an argument of one side only
           is kept. *)
        let union_args (args1 : taint list IL.argument list)
            (args2 : taint list IL.argument list) : taint list IL.argument list =
          let unite (a : taint list) (b : taint list) : taint list =
            List.sort_uniq compare_taint (a @ b)
          in
          let split (args : taint list IL.argument list) :
              taint list list * (IL.ident * taint list) list =
            List.partition_map
              (function
                | IL.Unnamed taints -> Either.Left taints
                | IL.Named (id, taints) -> Either.Right (id, taints))
              args
          in
          let positional1, named1 = split args1 in
          let positional2, named2 = split args2 in
          let rec unite_positional (p1 : taint list list) (p2 : taint list list) :
              taint list list =
            match (p1, p2) with
            | [], rest
            | rest, [] ->
                rest
            | a :: rest1, b :: rest2 -> unite a b :: unite_positional rest1 rest2
          in
          let named =
            List.fold_left
              (fun (acc : (IL.ident * taint list) list) ((id, taints) : IL.ident * taint list) ->
                match List.partition (fun ((id', _) : IL.ident * taint list) ->
                          String.equal (fst id') (fst id)) acc with
                | [ (id', taints') ], rest -> (id', unite taints' taints) :: rest
                | _ -> (id, taints) :: acc)
              named1 named2
            |> List.sort (fun ((id1, _) : IL.ident * taint list) ((id2, _) : IL.ident * taint list) ->
                   String.compare (fst id1) (fst id2))
          in
          List_.map (fun (taints : taint list) -> IL.Unnamed taints)
            (unite_positional positional1 positional2)
          @ List_.map (fun ((id, taints) : IL.ident * taint list) -> IL.Named (id, taints)) named
        in
        (* By callee, its results, their arguments united position by
           position, and their guards. *)
        let by_callee : (taint list * taint list IL.argument list * EG.t) Lval_map.t =
          Taints.to_seq_from first set
          |> Seq.fold_left
               (fun (acc : (taint list * taint list IL.argument list * EG.t) Lval_map.t)
                    ((key, b) : taint * guarded_taint) ->
                 match key.orig with
                 | Call_result { callee; args } ->
                     Lval_map.update callee
                       (function
                         | None -> Some ([ key ], args, b.guard)
                         | Some (keys, args', guard) ->
                             Some (key :: keys, union_args args' args, EG.compose_or guard b.guard))
                       acc
                 | Src _
                 | Var _
                 | Shape_var _
                 | Control ->
                     acc)
               Lval_map.empty
        in
        Lval_map.fold
          (fun (callee : lval) ((keys, args, guard) : taint list * taint list IL.argument list * EG.t)
               (acc : t) ->
            match keys with
            | []
            | [ _ ] ->
                acc
            | _ :: _ :: _ ->
                add
                  { taint = { orig = Call_result { callee; args }; tokens = [] }; guard }
                  (List.fold_left (fun (acc : t) (key : taint) -> Taints.remove key acc) acc keys))
          by_callee set

  and of_list bundles =
    List.fold_left (fun set b -> add b set) Taints.empty bundles

  and pick_best_taint taint1 taint2 =
    (* Here we assume that 'compare taint1 taint2 = 0' so we could keep any
       * of them, but we want "the best" one, e.g. the one with the shortest trace. *)
    if phys_equal taint1 taint2 then taint2
    else
    match (taint1.orig, taint2.orig) with
    | Var _, Var _
    | Shape_var _, Shape_var _
    | Call_result _, Call_result _
    | Control, Control ->
        (* Polymorphic taint should only be intraprocedural so the call-trace is irrelevant. *)
        if List.compare_lengths taint1.tokens taint2.tokens < 0 then taint1
        else taint2
    | Src src1, Src src2 ->
        let precondition =
          (* We don't pick a precondition, but we merge them! *)
          match (src1.precondition, src2.precondition) with
          | None, _
          | _, None ->
              (* We could have 'None, Some_' or 'Some _, None' here. That could mean
               * that there are two sources for the same label with different 'requires',
               * or perhaps the same 'requires' but one was trivially true in the context?
               *
               * In any case, if this happens, we just assume that no precondition
               * is needed to generate the label. *)
              None
          | Some (ts1, p1), Some (ts2, _p2) ->
              (* p1 = p2 given compare_precondition
               * Then we merge both lists of "incoming" taints, this also removes
               * from each individual taint list.
               *
               * Otherwise we end up with taint sets where most of the taints are
               * essentially the same. This is probably due to
               * [see note "Taint-tracking via ranges" in Match_tainting_mode],
               * and not having "Best_sources" [see note "Best matches" in 'Taint_spec_match'].
               * TOOD: Revisit ^^^ now we have `exact: true` sources.
               *)
              let ts1' = of_list (List_.map lift_taint ts1) in
              let ts2' = of_list (List_.map lift_taint ts2) in
              if equal ts1' ts2' then
                (* Optimization: prefer sharing. *)
                Some (ts1, p1)
              else
                let ts =
                  union ts1' ts2' |> elements
                  |> List_.map (fun (b : guarded_taint) -> b.taint)
                in
                Some (ts, p1)
        in
        let taint1 = { taint1 with orig = Src { src1 with precondition } } in
        let taint2 = { taint2 with orig = Src { src2 with precondition } } in
        let call_trace_cmp =
          Int.compare
            (length_of_call_trace src1.call_trace)
            (length_of_call_trace src2.call_trace)
        in
        if call_trace_cmp < 0 then taint1
        else if call_trace_cmp > 0 then taint2
        else if
          (* same length *)
          List.compare_lengths taint1.tokens taint2.tokens < 0
        then taint1
        else taint2
    | (Src _ | Var _ | Shape_var _ | Call_result _ | Control), _ ->
        Log.err (fun m ->
            m "Taint_set.pick_taint: Ooops, the impossible happened!");
        taint2

  (* Keep set1's bindings whose key is absent in set2. [Taints.filter]
     shares the subtrees it retains, where [Taints.merge] rebuilds the
     whole result. *)
  let diff set1 set2 =
    Taints.filter (fun taint _ -> not (Taints.mem taint set2)) set1

  let singleton (t : taint) : t = add (lift_taint t) empty

  (* Map over the bundles. The fast path leaves keys untouched, which is
     only sound while [f] preserves the taint identity ([orig]) of every
     bundle — true for the hot mappers ([with_guard] and the token-only
     [map_taint] uses). Some [map_taint] callers DO change identity
     (offset rewriting in [Taint_shape], label propagation in
     [Dataflow_tainting]); when we detect that, rebuild the map with
     proper keys and merging instead of leaving stale keys behind.
     Returns the input set itself when [f] changes nothing. *)
  let map f set =
    let changed = ref false in
    let rekey = ref false in
    let r =
      Taints.map
        (fun b ->
          let b' = f b in
          if not (Common.phys_equal b' b) then (
            changed := true;
            (* [b.taint] compares equal to its key by construction, so
               comparing against it detects key staleness; the physical
               check skips the structural compare when [f] kept the taint
               (e.g. [with_guard], which only changes the guard). *)
            if
              (not (Common.phys_equal b'.taint b.taint))
              && compare_taint b'.taint b.taint <> 0
            then rekey := true);
          b')
        set
    in
    if !rekey then Taints.fold (fun _ b acc -> add b acc) r Taints.empty
    else if !changed then r
    else set

  (* Union the per-element results: identity-equal taints from different
     [f] outputs fuse guards ([compose_or]) and keep the best taint
     ([pick_best_taint]), as in [add]/[union]. The bounds are applied once,
     to the whole result. *)
  let bind set f =
    Taints.fold (fun _ b acc -> union_unbounded (f b) acc) set empty |> bound
  let iter f set = Taints.iter (fun _ b -> f b) set
  let fold f set acc = Taints.fold (fun _ b acc -> f b acc) set acc
  let filter f set = Taints.filter (fun _ b -> f b) set

  (* Strip per-taint guards: yields the bare taint identities. Used at
   * boundaries that don't (yet) consume guards. *)
  let to_taint_list (set : t) : taint list =
    set |> elements |> List_.map (fun (b : guarded_taint) -> b.taint)

  (* Lift a list of bare taints with [EG.top] guards. *)
  let of_taint_list (taints : taint list) : t =
    of_list (List_.map lift_taint taints)

  (* Conjoin [g] into every bundle's guard. *)
  let conjoin_guard (g : EG.t) (set : t) : t =
    set |> map (with_guard g)

  (* The disjunction of the per-bundle guards: the condition under which at
   * least one taint in the set is live. Used to derive the effect-level
   * guard of a [ToLval] synthesised at function exit from the guards its
   * written taints carry. [empty] yields [top] (no constraint); callers
   * pass non-empty sets. *)
  let guards_disjunction (set : t) : EG.t =
    match elements set with
    | [] -> EG.top
    | b :: bs ->
        List.fold_left
          (fun acc (x : guarded_taint) -> EG.compose_or acc x.guard)
          b.guard bs

  (* Convenience: add a bare taint (with [EG.top] guard) to a set. *)
  let add_taint (t : taint) (set : t) : t = add (lift_taint t) set

  (* Convenience: add a taint with a specific guard. *)
  let add_taint_with_guard (t : taint) (g : EG.t) (set : t) : t =
    add { taint = t; guard = g } set

  (* Map the inner [taint] of every bundle, leaving guards untouched. *)
  let map_taint (f : taint -> taint) (set : t) : t =
    set |> map (fun (b : guarded_taint) -> { b with taint = f b.taint })
end

(* A call of [callee] in an argument of a call of [callee] is replaced by its
   own arguments: the return of a signature is distributive over the union of
   the argument taints, so [callee(a + callee(b))] returns the taints of
   [callee(a + b)]. Nesting is then bounded by the number of callees. *)
let call_result (callee : lval) (args : taint list IL.argument list) : orig =
  (* An argument holds one taint per origin, as a taint set does. *)
  let canonical (taints : taint list) : taint list =
    taints |> Taint_set.of_taint_list |> Taint_set.to_taint_list
  in
  let rec without_calls_of (taints : taint list) : taint list =
    taints
    |> List.concat_map (fun (taint : taint) ->
           match taint.orig with
           | Call_result call when equal_lval call.callee callee ->
               List.concat_map
                 (fun (arg : taint list IL.argument) -> without_calls_of (IL.value_of_argument arg))
                 call.args
           | Call_result call ->
               [
                 {
                   taint with
                   orig =
                     Call_result
                       {
                         call with
                         args =
                           List.map
                             (IL.map_argument (fun (arg : taint list) ->
                                  canonical (without_calls_of arg)))
                             call.args;
                       };
                 };
               ]
           | Src _
           | Var _
           | Shape_var _
           | Control ->
               [ taint ])
  in
  Call_result
    {
      callee;
      args =
        args
        |> List.map
             (IL.map_argument (fun (arg : taint list) -> canonical (without_calls_of arg)));
    }

type taints = Taint_set.t

let single_var (taints : taints) : lval option =
  match
    taints |> Taint_set.to_taint_list
    |> List_.filter_map (fun (taint : taint) ->
           match taint.orig with
           | Var lval -> Some lval
           | Src _
           | Shape_var _
           | Call_result _
           | Control ->
               None)
  with
  | [ lval ] -> Some lval
  | []
  | _ :: _ :: _ ->
      None

let show_taints ?(truncate_guards = true) taints =
  taints |> Taint_set.elements
  |> List_.map (fun (b : guarded_taint) ->
         let guard_str = EG.show_in_brackets ~truncate_guards b.guard in
         show_taint b.taint ^ guard_str)
  |> String.concat ", "
  |> fun str -> "{ " ^ str ^ " }"

(*****************************************************************************)
(* Taint labels *)
(*****************************************************************************)

let labels_in_precondition pre =
  let rec loop = function
    | R.PBool _ -> LabelSet.empty
    | R.PLabel l -> LabelSet.singleton l
    | R.PVariable _ -> LabelSet.empty
    | R.PAnd pres
    | R.POr pres ->
        pres |> List_.map loop |> List.fold_left LabelSet.union LabelSet.empty
    | R.PNot pre -> loop pre
  in
  loop pre

(* We need to be able to solve preconditions even with partial information, so we can
 * simplify away trivially true/false preconditions, and remove taint labels that we
 * know can't be present.
 *
 * Polymorphic taint is tricky, let's say we have:
 *
 *     def foo(y):
 *       x = "taint:A"
 *       sink(x, y)
 *
 * and the 'requires' of `sink` is `A and not B`. We don't know if 'B' could be
 * passed via the arguemnt `y`. Should we report a finding here? If we choose
 * 'ignore_poly_taint' then we will consider that 'B' is not present, and we would
 * solve the precondition as being true (i.e. 'Some true'); otherwise we will consider
 * that 'B' could be present, and the precondition would be considered unsolvable
 * (i.e. 'None').
 *
 * TODO: Perhaps we should "simplify" the precondition _and_ the incoming taints,
 *   so e.g. if we got taint labels 'A' and '?1' (poly label), and the precondition
 *   is `A and not B`, we can reduce that to having taint '?1' and precondition
 *   `not B`.
 *)
let rec solve_precondition ~ignore_poly_taint ~taints pre : bool option =
  let open Common in
  let sure_labels, maybe_labels, has_poly_taint = labels_in_taints taints in
  let rec loop = function
    | R.PBool b -> Some b
    | R.PLabel l ->
        if LabelSet.mem l sure_labels then Some true
        else if
          not (LabelSet.mem l maybe_labels)
          && (not has_poly_taint || ignore_poly_taint)
        then Some false
        else None
    | R.PVariable _var_name ->
        (* Check if any taint in the filtered set has source origins *)
        let has_source_taint =
          taints
          |> Taint_set.to_taint_list
          |> List.exists (fun (taint : taint) ->
                 match taint.orig with
                 | Src _ -> true (* Actual source taint *)
                 | _ -> false (* Variable initialization doesn't count *))
        in
        if has_source_taint then Some true
        else Some false
    | R.PNot p -> loop p |> Option.map not
    | R.PAnd ps ->
        ps
        |> List.fold_left
             (fun acc p ->
               let* v1 = acc in
               let* v2 = loop p in
               Some (v1 && v2))
             (Some true)
    | R.POr ps ->
        ps
        |> List.fold_left
             (fun acc p ->
               let* v1 = acc in
               let* v2 = loop p in
               Some (v1 || v2))
             (Some false)
  in
  loop pre

and labels_in_taints taints =
  let has_poly_taint = ref false in
  let sure_labels = ref LabelSet.empty in
  let maybe_labels = ref LabelSet.empty in
  taints
  |> Taint_set.iter (fun (b : guarded_taint) ->
         match b.taint.orig with
         | Var _
         | Shape_var _
         | Call_result _
         | Control ->
             has_poly_taint := true
         | Src { label; precondition = None; _ } ->
             sure_labels := LabelSet.add label !sure_labels
         | Src { label; precondition = Some (incoming, pre); _ } -> (
             match
               solve_precondition ~ignore_poly_taint:false
                 ~taints:(Taint_set.of_taint_list incoming)
                 pre
             with
             | Some true -> sure_labels := LabelSet.add label !sure_labels
             | Some false -> ()
             | None -> maybe_labels := LabelSet.add label !maybe_labels));
  (!sure_labels, !maybe_labels, !has_poly_taint)

let taints_satisfy_requires taints pre =
  (* This is used when deciding whether to report a finding, so if we had:
   *
   *     def foo(y):
   *       x = "taint:A"
   *       sink(x, y)
   *
   * with the 'requires' of `sink` being `A and not B`, we don't know whether
   * 'B' may be coming through the argument `y`. For now, to keep it simple, we
   * choose to ignore the polymorphic taint and report a finding. This is
   * because `not B` is typically used to encode a sanitizer, and it is
   * weird that the actual taint 'A' is present in `foo` but the sanitizer
   * label comes from an argument.
   *
   * TODO: We should check whether `foo` is being called from somewhere else.
   *   If there is no call to `foo` then the safest is to produce the finding,
   *   but if there are calls to `foo` such as `foo("safe")` then we could
   *   wait and not report any finding here.
   *)
  match
    solve_precondition ~ignore_poly_taint:true
      ~taints:(Taint_set.of_taint_list taints) pre
  with
  | Some b -> b
  | None ->
      (* If we set 'ignore_poly_taint' then we expect to be able to solve
       * the precondition! *)
      Log.err (fun m -> m "Could not solve taint label precondition");
      false

let filter_relevant_taints requires taints =
  let labels = labels_in_precondition requires in
  (* If the precondition is 'A' we don't care about taint with label 'B' or 'C'. *)
  taints
  |> Taint_set.filter (fun (b : guarded_taint) ->
         match b.taint.orig with
         | Src src -> LabelSet.mem src.label labels
         | Var _
         | Shape_var _
         | Call_result _
         | Control ->
             true)

(* Just a straightforward bottom-up map on preconditions. *)
let rec map_preconditions f taint =
  match taint.orig with
  | Var _
  | Shape_var _
  | Call_result _
  | Control ->
      Some taint
  | Src { precondition = None; _ } -> Some taint
  | Src ({ precondition = Some (incoming, expr); _ } as src) -> (
      let new_incoming =
        incoming
        |> List_.filter_map (map_preconditions f)
        |> f |> Taint_set.of_taint_list
      in
      let new_incoming = filter_relevant_taints expr new_incoming in
      match
        solve_precondition ~ignore_poly_taint:false ~taints:new_incoming expr
      with
      | Some false -> None
      | Some true ->
          Some { taint with orig = Src { src with precondition = None } }
      | None ->
          let new_incoming = new_incoming |> Taint_set.to_taint_list in
          let new_precondition = Some (new_incoming, expr) in
          Some
            {
              taint with
              orig = Src { src with precondition = new_precondition };
            })

(*****************************************************************************)
(* New taints *)
(*****************************************************************************)

let src_of_pm ~(grouping : source_grouping) ~incoming (pm, (source : Rule.taint_source)) =
  let group =
    match grouping with
    | By_message ->
        Some
          (List.filter
             (fun ((mvar, _) : string * Metavariable.mvalue) ->
               String_.contains ~term:mvar pm.Core_match.rule_id.message)
             pm.env)
    | By_bindings -> Some pm.env
    | Each_source -> None
  in
  let pc = R.get_source_precondition source in
  let relevant_incoming = filter_relevant_taints pc incoming in
  (* We don't expect to be able to solve preconditions here, but we need to try
   * in order to simplify away the trivial cases. Otherwise if we had e.g. a pattern
   * source like `pattern: $X` that matches tons of things, with label 'B' and a
   * 'requires' like `A`, we could be generating lots of 'B's in places where we know
   * for sure that we don't have any 'A'!
   *)
  match
    solve_precondition ~ignore_poly_taint:false ~taints:relevant_incoming pc
  with
  | Some false -> None
  | Some true ->
      Some
        (Src
           {
             call_trace = PM (pm, source);
             origin = (pm, source);
             label = source.label;
             precondition = None;
             group;
           })
  | None ->
      let taints_list = Taint_set.to_taint_list relevant_incoming in
      let precondition =
        match pc with
        | Rule.PBool true -> None
        | other -> Some (taints_list, other)
      in
      Some
        (Src
           {
             call_trace = PM (pm, source);
             origin = (pm, source);
             label = source.label;
             precondition;
             group;
           })

let taint_of_pm ~grouping ~incoming pm =
  match src_of_pm ~grouping ~incoming pm with
  | Some orig -> Some { orig; tokens = [] }
  | None -> None

let taints_of_pms ~grouping ~incoming pms =
  let max_ITERS =
    (* Just in case, we set a limit. *)
    3
  in
  (* Since labels can have a 'requires' constraint, we need to try adding
   * more labels until we reach a fixpoint. E.g., we could have a PM adding
   * label 'A', and another PM adding label 'B' but requiring 'A', so until
   * we add 'A' to the taint set we cannot add 'B'.
   * alt: Top-sort and then a fold ? *)
  let rec go i taints pms_i =
    if i >= max_ITERS then taints
    else
      let incoming = taints |> Taint_set.union incoming in
      let new_taint_list, pms_left =
        pms_i |> Common2.fpartition (taint_of_pm ~grouping ~incoming)
      in
      match new_taint_list with
      | [] -> taints
      | _ :: _ ->
          let taints' =
            Taint_set.of_taint_list new_taint_list
            |> Taint_set.union taints
          in
          go (i + 1) taints' pms_left
  in
  go 0 Taint_set.empty pms
