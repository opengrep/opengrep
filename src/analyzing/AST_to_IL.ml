(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc, 2025 Opengrep.
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
open IL
module Log = Log_analyzing.Log
module G = AST_generic
module H = AST_generic_helpers
module CLJ_ME1 = Macro_expand_clojure

[@@@warning "-40-42"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST generic to IL translation.
 *
 * todo:
 *  - a lot ...
 *)
let locate ?tok s : string =
  let opt_loc =
    try Option.map Tok.stringpos_of_tok tok with
    | Tok.NoTokenLocation _ -> None
  in
  match opt_loc with
  | Some loc -> spf "%s: %s" loc s
  | None -> s

let log_debug ?tok msg : unit = Log.debug (fun m -> m "%s" (locate ?tok msg))
let log_warning ?tok msg : unit = Log.warn (fun m -> m "%s" (locate ?tok msg))
let log_error ?tok msg : unit = Log.err (fun m -> m "%s" (locate ?tok msg))

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type stmts = stmt list

type rec_point_lvals =
  | Loop_rec_point of lval list (* loop binding lvals *)
  | Fn_rec_point of lval (* one parameter, destructured in body. *)

type env = {
  lang : Lang.t;
  (* When entering a loop, we create two labels, one to jump to if a Continue stmt is found
     and another to jump to if a Break stmt is found. Since PHP supports breaking an arbitrary
     number of loops up, we keep a stack of break labels instead of just one
  *)
  break_labels : label list;
  cont_label : label option;
  rec_point_label : label option;
  rec_point_lvals : rec_point_lvals option;
  inside_function : bool;
}

let empty_env (lang : Lang.t) : env =
  { break_labels = [];
    cont_label = None;
    rec_point_label = None;
    rec_point_lvals = None;
    inside_function = false;
    lang }

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception Fixme of fixme_kind * G.any

let sgrep_construct any_generic : 'a =
  raise (Fixme (Sgrep_construct, any_generic))

let todo any_generic : 'a = raise (Fixme (ToDo, any_generic))

let impossible any_generic : 'a =
  raise (Fixme (Impossible, any_generic))

let log_fixme kind gany : unit =
  let toks = AST_generic_helpers.ii_of_any gany in
  let tok = Common2.hd_opt toks in
  match kind with
  | ToDo ->
      log_warning ?tok
        "Unsupported construct(s) may affect the accuracy of dataflow analyses"
  | Sgrep_construct ->
      log_error ?tok "Cannot translate Semgrep construct(s) into IL"
  | Impossible ->
      log_error ?tok "Impossible happened during AST-to-IL translation"

let fixme_exp ?partial kind gany eorig : exp =
  log_fixme kind (any_of_orig eorig);
  { e = FixmeExp (kind, gany, partial); eorig }

let fixme_instr kind gany eorig : instr =
  log_fixme kind (any_of_orig eorig);
  { i = FixmeInstr (kind, gany); iorig = eorig }

let fixme_stmt kind gany : stmts =
  log_fixme kind gany;
  [ { s = FixmeStmt (kind, gany) } ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fresh_var ?(str = "_tmp") tok : name =
  let tok =
    (* We don't want "fake" auxiliary variables to have non-fake tokens, otherwise
       we confuse ourselves! E.g. during taint-tracking we don't want to add these
       variables to the taint trace. *)
    if Tok.is_fake tok then tok else Tok.fake_tok tok str
  in
  let i = G.SId.mk () in
  { ident = (str, tok); sid = i; id_info = G.empty_id_info () }

let fresh_label ?(label = "_label") tok : label =
  let i = G.SId.mk () in
  ((label, tok), i)

let fresh_lval ?str tok : lval =
  let var = fresh_var ?str tok in
  { base = Var var; rev_offset = [] }

let var_of_id_info id id_info : name =
  let sid =
    match !(id_info.G.id_resolved) with
    | Some (_resolved, sid) -> sid
    | None ->
        let id_str, id_tok = id in
        let msg = spf "the ident '%s' is not resolved" id_str in
        log_debug ~tok:id_tok msg;
        G.SId.unsafe_default
  in
  { ident = id; sid; id_info }

let var_of_name name : name =
  match name with
  | G.Id (id, id_info) -> var_of_id_info id id_info
  | G.IdQualified { G.name_last = id, _typeargsTODO; name_info = id_info; _ } ->
      var_of_id_info id id_info

let lval_of_id_info id id_info : lval =
  let var = var_of_id_info id id_info in
  { base = Var var; rev_offset = [] }

(* TODO: use also qualifiers? *)
let lval_of_id_qualified
    { G.name_last = id, _typeargsTODO; name_info = id_info; _ } : lval =
  lval_of_id_info id id_info

let lval_of_base base : lval = { base; rev_offset = [] }

(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label lbl : label = (lbl, G.SId.unsafe_default)
let lookup_label lbl : label = (lbl, G.SId.unsafe_default)
let mk_e e eorig : exp = { e; eorig }
let mk_i i iorig : instr = { i; iorig }
let mk_s s : stmt = { s }

let mk_unit tok eorig : exp =
  let unit = G.Unit tok in
  mk_e (Literal unit) eorig

(* Create an auxiliary variable for an expression.
 *
 * If 'force' is 'false' and the expression itself is already a variable then
 * it will not create an auxiliary variable but just return that. *)
let aux_var ?(force = false) ?str _env tok exp : stmts * name * lval =
  match exp.e with
  | Fetch ({ base = Var var; rev_offset = []; _ } as lval) when not force ->
      ([], var, lval)
  | __else__ ->
      let var = fresh_var ?str tok in
      let lval = lval_of_base (Var var) in
      ([mk_s (Instr (mk_i (Assign (lval, exp)) NoOrig))], var, lval)

let call_instr tok eorig ~void mk_call : stmts * exp =
  if void then
    ([mk_s (Instr (mk_i (mk_call None) eorig))], mk_unit tok NoOrig)
  else
    let lval = fresh_lval tok in
    ([mk_s (Instr (mk_i (mk_call (Some lval)) eorig))], mk_e (Fetch lval) NoOrig)

let ident_of_entity_opt ent : (G.ident * G.id_info) option =
  match ent.G.name with
  | G.EN (G.Id (i, pinfo)) -> Some (i, pinfo)
  (* TODO: use name_middle? name_top? *)
  | G.EN (G.IdQualified { name_last = i, _topt; name_info = pinfo; _ }) ->
      Some (i, pinfo)
  | G.EDynamic _ -> None
  (* TODO *)
  | G.EPattern _
  | G.OtherEntity _ ->
      None

let name_of_entity ent : name option =
  match ident_of_entity_opt ent with
  | Some (i, pinfo) ->
      let name = var_of_id_info i pinfo in
      Some name
  | _else_ -> None

let composite_of_container ~g_expr :
    G.container_operator -> IL.composite_kind =
 fun cont ->
  match cont with
  | Array -> CArray
  | List -> CList
  | Tuple -> CTuple
  | Set -> CSet
  | Dict -> impossible (E g_expr)

let mk_unnamed_args (exps : IL.exp list) : exp argument list = List_.map (fun x -> Unnamed x) exps

let is_hcl lang : bool =
  match lang with
  | Lang.Terraform -> true
  | _ -> false

(* Extract the class name from a G.New type to build the constructor
   reference in the IL New instruction. Only called from class_construction,
   which only runs for G.New nodes (always constructor calls).

   The TyExpr case exists because the JS parser produces TyExpr where
   it should produce TyN.

   Previously this had a guard `when Option.is_some !(cons_id_info.id_resolved)`
   which meant it only worked when the pro engine's naming pass had run.
   In OSS mode id_resolved is never set, so the IL New always got
   constructor=None, falling through to all_args_taints (accidental leak).
   The guard was removed because G.New is always a constructor call and
   the fallback behavior is unchanged when no signature is found. *)
let mk_class_constructor_name (ty : G.type_) cons_id_info : G.name option =
  match ty with
  | { t = TyN (G.Id (id, _)); _ }
  | { t = TyExpr { e = G.N (G.Id (id, _)); _ }; _ } ->
      Some (G.Id (id, cons_id_info))
  | __else__ -> None


let def_expr_evaluates_to_value (lang : Lang.t) : bool =
  match lang with
  | Elixir (* | Clojure *) -> true
  | _else_ -> false

let is_constructor env ret_ty id_info : bool =
  match id_info.G.id_resolved.contents with
  | Some (G.GlobalName (ls, _), _) -> (
      env.lang =*= Lang.Python
      && List.length ls >= 3 (* Module + Class + __init__ *)
      && (match List_.last_opt ls with
         | Some "__init__" -> true
         | _ -> false)
      &&
      match ret_ty with
      (* It would be nice if we can check that this type actually
         corresponds to a class, but I am uncertain if this is
         possible. Instead we just check if it is a nominal typed.
         TODO could we somehow guarentee this type is a class? *)
      | { G.t = G.TyN _; _ } -> true
      | _ -> false)
  | _ -> false

(*****************************************************************************)
(* Pattern helpers *)
(*****************************************************************************)

(* Detect a list-shaped case pattern and return the arity check that should be
 * used as the Switch discriminator.
 *
 * A [PatList [p1; ...; pN]] with no trailing [&] matches iff the scrutinee is a
 * list of length exactly [N] — the condition is [length(scrutinee) = N].
 *
 * A [PatList [p1; ...; pK; PatConstructor("&", _)]] with a trailing variadic
 * slot matches iff the scrutinee has at least [K] elements — the condition is
 * [length(scrutinee) >= K].
 *
 * Any other shape (tuple, literal, constructor, value pattern) returns [None]
 * and the caller falls back to a [fixme] condition — which leaves dataflow to
 * conservatively take every branch, as today. *)
let rec list_pat_arity (pat : G.pattern) : (G.operator * int) option =
  match pat with
  | G.PatWhen (inner, _guard) -> list_pat_arity inner
  | G.PatList (_, slots, _) ->
      let n = List.length slots in
      (match List.rev slots with
       | G.PatConstructor (G.Id (("&", _), _), _) :: _ ->
           Some (G.GtE, n - 1)
       | _ -> Some (G.Eq, n))
  | _ -> None

(*****************************************************************************)
(* Implicit-return helper *)
(*****************************************************************************)

(* Clojure wraps function bodies and macro-expanded forms such as [->>] in
 * [OtherExpr("ExprBlock", ...)] / [OtherExpr("->>", ...)] wrappers whose last
 * sub-expression carries the value. [mark_first_instr_ancestor] sets
 * [is_implicit_return] on the inner value-producing G.expr (e.g. the Call), but
 * the lowering sites check the flag on the outer ExprStmt's eorig, which is
 * the wrapper, not the inner expression. This helper treats a wrapper as
 * implicitly-returned when its last sub-expression is. *)
let effective_implicit_return env (eorig : G.expr) : bool =
  if eorig.is_implicit_return then true
  else
    match eorig.e with
    | G.OtherExpr ((kind, _), exprs)
      when env.lang =*= Lang.Clojure
           && CLJ_ME1.expands_as_block kind -> (
        match List.rev exprs with
        | G.E { is_implicit_return = true; _ } :: _ -> true
        | _ -> false)
    | _ -> false

(*****************************************************************************)
(* Map-pair pattern helpers *)
(*****************************************************************************)

(* A [PatList] / [PatConstructor] holds "map pairs" when every element is a
 * [PatKeyVal] in the canonical [(lookup_key, binding_pattern)] order,
 * possibly wrapped by exactly one [OtherPat("MapPairKeyword" |
 * "MapPairArrow", …)]. That tag is emitted by the Elixir parser to
 * preserve the source-level [:] vs [=>] distinction for rule matching;
 * no other parser uses it, and no parser nests it. *)
let is_map_pair_pattern (p : G.pattern) : bool =
  match p with
  | G.PatKeyVal (_, _) -> true
  | G.OtherPat
      ( (("MapPairKeyword" | "MapPairArrow"), _),
        [ G.P (G.PatKeyVal (_, _)) ] ) ->
      true
  | _ -> false

let unwrap_map_pair_pattern (p : G.pattern) : G.pattern =
  match p with
  | G.OtherPat
      ( (("MapPairKeyword" | "MapPairArrow"), _),
        [ G.P (G.PatKeyVal (_, _) as inner) ] ) ->
      inner
  | _ -> p

(* Build a field [Dot] offset from a map-pair key pattern. Returns [None]
 * for opaque keys — callers fall back to binding the value without a key
 * projection, losing field-sensitivity for that leaf.
 *
 * The [OtherPat((":"|"::"), …)] branches below recognise the Clojure
 * parser's encoding of atom keys; no other parser emits that tag.
 * Clojure distinguishes [:body] (plain keyword) from [::body]
 * (auto-resolved namespaced keyword) — they denote different runtime
 * atoms and must produce different offsets. [G.Atom] cannot currently
 * carry that distinction, so we reconstruct the surface form
 * [":body"] / [":body"] from the tag prefix. If [G.Atom] is later
 * extended with a "kind" field, this handling can collapse into the
 * [PatLiteral (G.Atom …)] branch — option (3) in the design
 * discussion. *)
let key_offset_of_pattern (key_pat : G.pattern) : offset option =
  let mk_dot (s, tok) =
    {
      o =
        Dot
          {
            ident = (s, tok);
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          };
      oorig = NoOrig;
    }
  in
  let stripped (s, tok) = (String_.strip_wrapping_char ':' s, tok) in
  match key_pat with
  | G.PatId (id, _) -> Some (mk_dot (stripped id))
  | G.PatLiteral (G.String (_, id, _)) -> Some (mk_dot (stripped id))
  | G.PatLiteral (G.Atom (_, id)) -> Some (mk_dot (stripped id))
  | G.OtherPat ((((":" | "::") as prefix), _), [ G.Name (G.Id (id, _)) ]) ->
      let s, tok = id in
      Some (mk_dot (prefix ^ String_.strip_wrapping_char ':' s, tok))
  | G.OtherPat
      ( (((":" | "::") as prefix), _),
        [ G.Name (G.IdQualified { name_last = (id, _); _ }) ] ) ->
      let s, tok = id in
      Some (mk_dot (prefix ^ String_.strip_wrapping_char ':' s, tok))
  | _ -> None

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)

let rec lval env eorig : stmts * lval =
  match eorig.G.e with
  | G.N n -> ([], name n)
  | G.IdSpecial (G.This, tok) -> ([], lval_of_base (VarSpecial (This, tok)))
  | G.DotAccess (e1orig, tok, field) ->
      let ss_off, offset' =
        match field with
        | G.FN (G.Id (id, idinfo)) -> ([], Dot (var_of_id_info id idinfo))
        | G.FN name ->
            let ss, attr = expr env (G.N name |> G.e) in
            (ss, Index attr)
        | G.FDynamic e2orig ->
            let ss, attr = expr env e2orig in
            (ss, Index attr)
      in
      let offset' = { o = offset'; oorig = SameAs eorig } in
      let ss_lv, lv1 = nested_lval env tok e1orig in
      (ss_lv @ ss_off, { lv1 with rev_offset = offset' :: lv1.rev_offset })
  | G.ArrayAccess (e1orig, (_, e2orig, _)) ->
      let tok = G.fake "[]" in
      let ss_lv, lv1 = nested_lval env tok e1orig in
      let ss_e2, e2 = expr env e2orig in
      let offset' = { o = Index e2; oorig = SameAs eorig } in
      (ss_lv @ ss_e2, { lv1 with rev_offset = offset' :: lv1.rev_offset })
  | G.DeRef (_, e1orig) ->
      let ss, e1 = expr env e1orig in
      (ss, lval_of_base (Mem e1))
  | _ -> ([], todo (G.E eorig))

and nested_lval env tok e_gen : stmts * lval =
  match expr env e_gen with
  | ss, { e = Fetch lval; _ } -> (ss, lval)
  | ss, rhs ->
      let fresh = fresh_lval tok in
      let instr = mk_s (Instr (mk_i (Assign (fresh, rhs)) (related_exp e_gen))) in
      (ss @ [instr], fresh)

and name : G.name -> lval = function
  | G.Id (("_", tok), _) ->
      (* wildcard *)
      fresh_lval tok
  | G.Id (id, id_info) ->
      let lval = lval_of_id_info id id_info in
      lval
  | G.IdQualified qualified_info ->
      let lval = lval_of_id_qualified qualified_info in
      lval

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)

(* TODO: This code is very similar to that of `assign`. Actually, we should not
 * be dealing with patterns in the LHS of `Assign`, those are supposed to be
 * `LetPattern`s. *)
(* TODO: PatDisj, but it's abused in a lot of places so it's not clear.
 * Normally, assuming that both patterns have the same identifiers, one
 * could just recurse on one of them and ignore the other. *)
and pattern env pat : stmts * lval * stmts =
  match pat with
  | G.PatWildcard tok ->
      let lval = fresh_lval tok in
      ([], lval, [])
  | G.PatLiteral _ ->
      let lval = fresh_lval (Tok.unsafe_fake_tok "_patlit") in
      ([], lval, [])
  | G.PatId (id, id_info) ->
      let lval = lval_of_id_info id id_info in
      ([], lval, [])
  | G.PatAs (pat_inner, (id, id_info)) ->
    let tok = snd id in
    (* Create tmp to hold the whole matched value *)
    let tmp = fresh_var tok in
    let tmp_lval = lval_of_base (Var tmp) in
    (* Alias lval for 'id' *)
    let alias_lval = lval_of_id_info id id_info in
    let tmp_fetch_e = mk_e (Fetch tmp_lval) (Related (G.P pat_inner)) in
    let alias_assign_stmt =
      mk_s (Instr (mk_i (Assign (alias_lval, tmp_fetch_e)) (related_tok tok)))
    in
    let inner_ss =
      pattern_assign_statements
        env ~eorig:(Related (G.P pat_inner)) tmp_fetch_e pat_inner
    in
    (* NOTE: Order of statements determines scope in cases like: `[x; y] as x`
     * here we will see the whole value as `x`. Not important. *)
    ([], tmp_lval, inner_ss @ [ alias_assign_stmt ])
  | G.PatList (_tok1, pats, tok2)
  | G.PatTuple (_tok1, pats, tok2) ->
      let tmp = fresh_var tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      let ss =
        if pats <> [] && List.for_all is_map_pair_pattern pats then
          (* Map/dict destructure: every element is a [PatKeyVal] (possibly
           * tagged). Each binding fetches [tmp.key], not [tmp[i]]. *)
          List.concat_map (map_pair_assign_stmts env tmp) pats
        else
          (* Positional tuple/list destructure: [pat_i = tmp[i]]. *)
          List.concat_map
            (fun (pat_i, i) ->
              let eorig = Related (G.P pat_i) in
              let index_i = Literal (G.Int (Parsed_int.of_int i)) in
              let offset_i =
                { o = Index { e = index_i; eorig }; oorig = NoOrig }
              in
              let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
              pattern_assign_statements env
                (mk_e (Fetch lval_i) eorig)
                ~eorig pat_i)
            (List_.index_list pats)
      in
      ([], tmp_lval, ss)
  | G.PatTyped (pat1, ty) ->
      let pre_ss, _ = type_ env ty in
      let inner_pre_ss, lval, post_ss = pattern env pat1 in
      (pre_ss @ inner_pre_ss, lval, post_ss)
  | G.PatConstructor (G.Id ((_s, tok), _id_info), pats)
    when pats <> [] && List.for_all is_map_pair_pattern pats ->
      (* Clojure [:keys] / [Assoc] map destructure: the constructor wraps
       * a flat list of [PatKeyVal]s over the incoming map. Lower as a
       * map destructure rather than a positional tuple. *)
      pattern env (G.PatList (G.fake "[", pats, tok))
  | G.PatConstructor (G.Id ((_s, tok), _id_info), pats) ->
      pattern env (G.PatTuple (G.fake "(", pats, tok))
  | G.PatKeyVal (key_pat, val_pat) ->
      (* Standalone [PatKeyVal] outside a [PatList]/[PatConstructor]
       * container. Rare; lower by projecting a fresh [tmp] by the key.
       * The common case (multiple pairs inside a [PatList]) takes the
       * map-pair branch of the [PatList] handler above. *)
      let tok =
        match key_offset_of_pattern key_pat with
        | Some { o = Dot { ident = _, t; _ }; _ } -> t
        | _ -> Tok.unsafe_fake_tok "_patkv"
      in
      let tmp = fresh_var tok in
      let tmp_lval = lval_of_base (Var tmp) in
      let eorig = Related (G.P val_pat) in
      let ss =
        match key_offset_of_pattern key_pat with
        | Some offset_i ->
            let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
            pattern_assign_statements env
              (mk_e (Fetch lval_i) eorig)
              ~eorig val_pat
        | None ->
            pattern_assign_statements env
              (mk_e (Fetch tmp_lval) eorig)
              ~eorig val_pat
      in
      ([], tmp_lval, ss)
  | G.PatRecord (_tok1, fields, tok2) ->
    (* Each field is (dotted_ident, pattern). The dotted_ident carries only
     * the field label (string * tok) with no id_info; we synthesise an
     * IL.name with empty id_info and the default sid. That matches the
     * shape of caller-side field access (via [var_of_id_info] on an
     * unresolved id_info) and of record-literal field keys, which also
     * land with the default sid. The inner pattern [pat_i] comes from the
     * source AST and has already been processed by Naming_AST, so its
     * bound variable's id_info (if any) is already resolved.
     *
     * For qualified field names (e.g. [M.field] in OCaml), we use the last
     * segment as the field label; the qualifier is dropped. *)
    let tmp = fresh_var tok2 in
    let tmp_lval = lval_of_base (Var tmp) in
    let synth_name ident =
      { ident; sid = G.SId.unsafe_default; id_info = G.empty_id_info () }
    in
    let last_ident_of dot_ident =
      match List.rev dot_ident with
      | last :: _ -> last
      | [] -> ("", tok2)
    in
    let ss =
      List.concat_map
        (fun (dot_ident, pat_i) ->
          let eorig = Related (G.P pat_i) in
          let field_name = synth_name (last_ident_of dot_ident) in
          let offset_i = { o = Dot field_name; oorig = NoOrig } in
          let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
          pattern_assign_statements env
            (mk_e (Fetch lval_i) eorig)
            ~eorig pat_i)
        fields
    in
    ([], tmp_lval, ss)
  | G.PatWhen (pat_inner, when_expr) ->
      let pre_ss, lval, pat_stmts = pattern env pat_inner in
      let guard_stmts, _e_guard = expr env when_expr in
      (* TODO: Handle fallthrough which is now true by default for
       * this kind of pattern. But I wonder if it would be pointless
       * to bother with it. *)
      (pre_ss, lval, pat_stmts @ guard_stmts)
  | G.DisjPat (pat1, _pat2) ->
    (* XXX: Assume same bound variables on lhs and rhs, as is imposed on most
     * languages. Hence we only recurse on one side. Seems good enough for now. *)
    pattern env pat1
  | G.OtherPat ((("MapPairArrow" | "MapPairKeyword"), _), [ G.P inner ])
    when env.lang =*= Lang.Elixir ->
    pattern env inner
  | G.OtherPat (("ExprToPattern", tok), [ G.E e ]) ->
    (* expr_to_pattern fallback: the expression couldn't be statically
     * converted to a known pattern. Evaluate the expression so that
     * side-effects and taint flow are captured, then bind a fresh tmp. *)
    let pre_ss, _e' = expr env e in
    let tmp = fresh_lval tok in
    (pre_ss, tmp, [])
  | G.PatEllipsis _ -> sgrep_construct (G.P pat)
  | _ -> todo (G.P pat)

(* Lower one map-pair element of a [PatList]/[PatConstructor] destructure.
 * Unwraps the [OtherPat("MapPair*", …)] tag if present and emits
 * [binding = tmp.key]. If the key form is opaque (no literal name we
 * can turn into a [Dot] offset) or the element is not a [PatKeyVal]
 * after unwrapping, emit a [fixme_stmt] so the gap is visible in the IL
 * rather than silently dropping the binding. *)
and map_pair_assign_stmts env tmp (pat : G.pattern) : stmt list =
  match unwrap_map_pair_pattern pat with
  | G.PatKeyVal (key_pat, val_pat) -> (
      match key_offset_of_pattern key_pat with
      | Some offset_i ->
          let eorig = Related (G.P pat) in
          let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
          let exp = mk_e (Fetch lval_i) eorig in
          pattern_assign_statements env exp ~eorig val_pat
      | None -> fixme_stmt ToDo (G.P pat))
  | _ -> fixme_stmt ToDo (G.P pat)

and _catch_exn env exn : stmts * lval * stmts =
  match exn with
  | G.CatchPattern pat -> pattern env pat
  | G.CatchParam { pname = Some id; pinfo = id_info; _ } ->
      let lval = lval_of_id_info id id_info in
      ([], lval, [])
  | _ -> todo (G.Ce exn)

and pattern_assign_statements env ?(eorig = NoOrig) exp pat : stmt list =
  try
    let pre_ss, lval, post_ss = pattern env pat in
    pre_ss @ [ mk_s (Instr (mk_i (Assign (lval, exp)) eorig)) ] @ post_ss
  with
  | Fixme (kind, any_generic) ->
      fixme_stmt kind any_generic

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
and try_catch_else_finally env ~try_st ~catches ~opt_else ~opt_finally : stmts =
  let try_stmt = stmt env try_st in
  let catches_stmt_rev =
    List.map
      (fun (ctok, exn, catch_st) ->
        (* TODO: Handle exn properly. *)
        let name = fresh_var ctok in
        let todo_pattern = fixme_stmt ToDo (G.Ce exn) in
        let catch_stmt = stmt env catch_st in
        (name, todo_pattern @ catch_stmt))
      catches
  in
  let else_stmt =
    match opt_else with
    | None -> []
    | Some (_tok, else_st) -> stmt env else_st
  in
  let finally_stmt =
    match opt_finally with
    | None -> []
    | Some (_tok, finally_st) -> stmt env finally_st
  in
  [ mk_s (Try (try_stmt, catches_stmt_rev, else_stmt, finally_stmt)) ]

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)
and assign env ~g_expr lhs tok rhs_exp : stmts * exp =
  let eorig = SameAs g_expr in
  match lhs.G.e with
  | G.N _
  | G.DotAccess _
  | G.ArrayAccess _
  | G.DeRef _ -> (
      try
        let ss_lv, lval = lval env lhs in
        let instr = mk_s (Instr (mk_i (Assign (lval, rhs_exp)) eorig)) in
        (ss_lv @ [instr], mk_e (Fetch lval) (SameAs lhs))
      with
      | Fixme (kind, any_generic) ->
          (* lval translation failed, we use a fresh lval instead *)
          let fixme_lval = fresh_lval ~str:"_FIXME" tok in
          let instr = mk_s (Instr (mk_i (Assign (fixme_lval, rhs_exp)) eorig)) in
          ([instr], fixme_exp kind any_generic (related_exp g_expr)))
  | G.Container (((G.Tuple | G.List | G.Array) as ckind), (tok1, lhss, tok2)) ->
      (* TODO: handle cases like [a, b, ...rest] = e *)
      (* E1, ..., En = RHS *)
      (* tmp = RHS*)
      let tmp = fresh_var tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      let tmp_assign = mk_s (Instr (mk_i (Assign (tmp_lval, rhs_exp)) eorig)) in
      (* Ei = tmp[i] *)
      let tup_results =
        List.map
          (fun (lhs_i, i) ->
            let index_i = Literal (G.Int (Parsed_int.of_int i)) in
            let offset_i =
              {
                o = Index { e = index_i; eorig = related_exp lhs_i };
                oorig = NoOrig;
              }
            in
            let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
            let ss, expr =
              assign env ~g_expr lhs_i tok1
                { e = Fetch lval_i; eorig = related_exp lhs_i }
            in
            (ss, expr))
          (List_.index_list lhss)
      in
      let tup_ss = List.concat_map fst tup_results in
      let tup_elems = List.map snd tup_results in
      (* (E1, ..., En) *)
      ( tmp_assign :: tup_ss,
        mk_e
          (Composite
             ( composite_of_container ~g_expr ckind,
               (tok1, tup_elems, tok2) ))
          (related_exp lhs) )
  | G.Record (tok1, fields, tok2) ->
      assign_to_record env (tok1, fields, tok2) rhs_exp (related_exp lhs)
  | _ ->
      let instr = mk_s (Instr (fixme_instr ToDo (G.E g_expr) (related_exp g_expr))) in
      ([instr], fixme_exp ToDo (G.E g_expr) (related_exp lhs))

and assign_to_record env (tok1, fields, tok2) rhs_exp lhs_orig : stmts * exp =
  (* Assignments of the form
   *
   *     {x1: p1, ..., xN: pN} = RHS
   *
   * where `xi` are field names, and `pi` are patterns.
   *
   * In the simplest case, where the patterns are variables
   * v1, ..., VN, this becomes:
   *
   *     tmp = RHS
   *     v1 = tmp.x1
   *     ...
   *     vN = tmp.xN
   *)
  let aux_ss, tmp, _tmp_lval = aux_var env tok1 rhs_exp in
  let rec do_fields acc_rev_offsets fs =
    let results = List.map (fun x -> do_field acc_rev_offsets x) fs in
    let ss = List.concat_map fst results in
    let fields = List.map snd results in
    (ss, fields)
  and do_field acc_rev_offsets f =
    match f with
    | G.F
        {
          s =
            G.DefStmt
              ( { name = EN (G.Id (id1, ii1)); _ },
                G.FieldDefColon
                  { vinit = Some { e = G.N (G.Id (id2, ii2)); _ }; _ } );
          _;
        } ->
        (* fld = var ----> var := tmp. ... <accumulated offsets> ... .fld *)
        let tok = snd id1 in
        let fldi = var_of_id_info id1 ii1 in
        let offset = { o = Dot fldi; oorig = NoOrig } in
        let vari = var_of_id_info id2 ii2 in
        let vari_lval = lval_of_base (Var vari) in
        let ei =
          mk_e
            (Fetch { base = Var tmp; rev_offset = offset :: acc_rev_offsets })
            (related_tok tok)
        in
        let instr = mk_s (Instr (mk_i (Assign (vari_lval, ei)) (related_tok tok))) in
        ([instr], Field (fldi, mk_e (Fetch vari_lval) (related_tok tok)))
    | G.F
        {
          s =
            G.DefStmt
              ( { name = EN (G.Id (id1, ii1)); _ },
                G.FieldDefColon
                  { vinit = Some { e = G.Record (_, fields, _); _ }; _ } );
          _;
        } ->
        (* fld = { ... }, nested record pattern, we recurse. *)
        let tok = snd id1 in
        let fldi = var_of_id_info id1 ii1 in
        let offset = { o = Dot fldi; oorig = NoOrig } in
        let ss, fields = do_fields (offset :: acc_rev_offsets) fields in
        (ss, Field (fldi, mk_e (RecordOrDict fields) (related_tok tok)))
    | field ->
        (* TODO: What other patterns could be nested ? *)
        (* __FIXME_AST_to_IL__: FixmeExp ToDo *)
        let xi = ("__FIXME_AST_to_IL_assign_to_record__", tok1) in
        let xn =
          {
            ident = xi;
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          }
        in
        let ei = fixme_exp ToDo (G.Fld field) (related_tok tok1) in
        let tmpi = fresh_var tok2 in
        let tmpi_lval = lval_of_base (Var tmpi) in
        let instr = mk_s (Instr (mk_i (Assign (tmpi_lval, ei)) (related_tok tok1))) in
        ([instr], Field (xn, mk_e (Fetch tmpi_lval) (Related (G.Fld field))))
  in
  let fields_ss, fields = do_fields [] fields in
  (* {x1: E1, ..., xN: En} *)
  (aux_ss @ fields_ss, mk_e (RecordOrDict fields) lhs_orig)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(* less: we could pass in an optional lval that we know the caller want
 * to assign into, which would avoid creating useless fresh_var intermediates.
 *)
(* We set `void` to `true` when the value of the expression is being discarded, in
 * which case, for certain expressions and in certain languages, we assume that the
 * expression has side-effects. See translation of operators below. *)
and expr_aux env ?(void = false) g_expr : stmts * exp =
  let eorig = SameAs g_expr in
  match g_expr.G.e with
  | G.Call
      ( { e = G.IdSpecial (G.Op ((G.And | G.Or) as op), tok); _ },
        (_, arg0 :: args, _) )
    when not void || env.lang =*= Lang.Ruby ->
      expr_lazy_op env op tok arg0 args eorig
  | G.Call ({ e = G.IdSpecial (G.Op op, tok); _ }, args) -> (
      match op with
      | G.Elvis when env.lang =*= Lang.Kotlin || env.lang =*= Lang.Csharp -> (
          (* This implements the logic:
           * result = lhs
           * if (result == null) { result = rhs; }
           * This ensures the lhs expression is evaluated exactly once.
           *)
          match Tok.unbracket args with
          | [ G.Arg lhs_gen; G.Arg rhs_gen ] -> begin
              let result_lval = fresh_lval tok in
              (* Evaluate lhs and assign its value to a temp var ('result = lhs;') *)
              let ss_for_lhs, lhs_exp = expr env lhs_gen in
              let lhs_assign = mk_s (Instr (mk_i (Assign (result_lval, lhs_exp)) NoOrig)) in
              let result_val_exp = mk_e (Fetch result_lval) (related_tok tok) in
              (* Create the condition 'result == null' *)
              let null_literal =
                mk_e (Literal (G.Null tok)) (related_tok tok)
              in
              let condition_exp =
                mk_e
                  (Operator
                     ( (G.Eq, tok),
                       [ Unnamed result_val_exp; Unnamed null_literal ] ))
                  (related_tok tok)
              in
              (* Define the 'then' branch, which evaluates rhs and updates the temp var. *)
              let ss_for_rhs, rhs_exp = expr env rhs_gen in
              let then_branch =
                ss_for_rhs
                @ [ mk_s (Instr (mk_i (Assign (result_lval, rhs_exp)) NoOrig)) ]
              in
              let if_stmt = mk_s (If (tok, condition_exp, then_branch, [])) in
              (ss_for_lhs @ [lhs_assign; if_stmt], mk_e (Fetch result_lval) eorig)
            end
          (* TODO: simply getting rid of the elvis here is semantically not correct, *)
          (* but should not affect the analysis in practical cases. The proper implememntation *)
          (* requires more gymnastics with IL, for which we first need a deeper refactoring *)
          (* of AST_to_IL *)
          | [ G.Arg arg ] when env.lang =*= Lang.Csharp ->
              expr_aux env ~void arg
          | _ -> impossible (G.E g_expr))
      | _ -> (
          (* All other operators *)
          let ss_args, args = arguments env (Tok.unbracket args) in
          if not void then (ss_args, mk_e (Operator ((op, tok), args)) eorig)
          else
            (* The operation's result is not being used, so it may have side-effects.
             * We then assume this is just syntax sugar for a method call. E.g. in
             * Ruby `s << "hello"` is syntax sugar for `s.<<("hello")` and it mutates
             * the string `s` appending "hello" to it. *)
            match args with
            | [] -> impossible (G.E g_expr)
            | obj :: args' ->
                let aux_ss, obj_var, _obj_lval =
                  aux_var env tok (IL_helpers.exp_of_arg obj)
                in
                let method_name =
                  fresh_var tok ~str:(Tok.content_of_tok tok)
                in
                let offset = { o = Dot method_name; oorig = NoOrig } in
                let method_lval =
                  { base = Var obj_var; rev_offset = [ offset ] }
                in
                let method_ =
                  { e = Fetch method_lval; eorig = related_tok tok }
                in
                let call_ss, call_exp = call_instr tok eorig ~void (fun res ->
                    Call (res, method_, args')) in
                (ss_args @ aux_ss @ call_ss, call_exp)))
  | G.Call
      ( ({ e = G.IdSpecial ((G.This | G.Super | G.Self | G.Parent), tok); _ } as
         e),
        args ) ->
      call_generic env ~void tok eorig e args
  | G.Call
      ({ e = G.IdSpecial (G.IncrDecr (incdec, _prepostIGNORE), tok); _ }, args)
    -> (
      (* in theory in expr() we should return each time a list of pre-instr
       * and a list of post-instrs to execute before and after the use
       * of the expression. However this complicates the interface of 'expr()'.
       * Right now, for the pre-instr we agglomerate them instead in env
       * and use them in 'expr_with_pre_instr()' below, but for the post
       * we dont. Anyway, for our static analysis purpose it should not matter.
       * We don't do fancy path-sensitive-evaluation-order-sensitive analysis.
       *)
      match Tok.unbracket args with
      | [ G.Arg e ] ->
          let ss_lv, lval = lval env e in
          (* TODO: This `lval` should have a new svalue ref given that we
           * are translating `lval++` as `lval = lval + 1`. *)
          let lvalexp = mk_e (Fetch lval) (related_exp e) in
          let op =
            ( (match incdec with
              | G.Incr -> G.Plus
              | G.Decr -> G.Minus),
              tok )
          in
          let one = G.Int (Parsed_int.of_int 1) in
          let one_exp = mk_e (Literal one) (related_tok tok) in
          let opexp =
            mk_e
              (Operator (op, [ Unnamed lvalexp; Unnamed one_exp ]))
              (related_tok tok)
          in
          let instr = mk_s (Instr (mk_i (Assign (lval, opexp)) eorig)) in
          (ss_lv @ [instr], lvalexp)
      | _ -> impossible (G.E g_expr))
  | G.Call
      ( {
          e =
            G.DotAccess
              ( obj,
                tok,
                G.FN
                  (G.Id
                     (("concat", _), { G.id_resolved = { contents = None }; _ }))
              );
          _;
        },
        args ) ->
      (* obj.concat(args) *)
      (* NOTE: Often this will be string concatenation but not necessarily! *)
      let ss_obj, obj_expr = expr env obj in
      let obj_arg' = Unnamed obj_expr in
      let ss_args, args' = arguments env (Tok.unbracket args) in
      let ss_res, res =
        match env.lang with
        (* Ruby's concat method is side-effectful and updates the object. *)
        (* TODO: The lval in the LHs should have a differnt svalue than the
         * one in the RHS. *)
        | Lang.Ruby -> (
            try
              let ss_lv, lv = lval env obj in
              (ss_lv, lv)
            with
            | Fixme _ ->
                ([], fresh_lval ~str:"Fixme" tok))
        | _ -> ([], fresh_lval tok)
      in
      let instr = mk_s (Instr (mk_i (CallSpecial (Some res, (Concat, tok), obj_arg' :: args')) eorig)) in
      (ss_obj @ ss_args @ ss_res @ [instr], mk_e (Fetch res) eorig)
  (* todo: if the xxx_to_generic forgot to generate Eval *)
  | G.Call
      ( {
          e =
            G.N
              (G.Id (("eval", tok), { G.id_resolved = { contents = None }; _ }));
          _;
        },
        args ) ->
      let lval = fresh_lval tok in
      let special = (Eval, tok) in
      let ss_args, args = arguments env (Tok.unbracket args) in
      let instr = mk_s (Instr (mk_i (CallSpecial (Some lval, special, args)) eorig)) in
      (ss_args @ [instr], mk_e (Fetch lval) (related_tok tok))
  | G.Call
      ({ e = G.IdSpecial (G.InterpolatedElement, _); _ }, (_, [ G.Arg e ], _))
    ->
      (* G.InterpolatedElement is useful for matching certain patterns against
       * interpolated strings, but we do not have an use for it yet during
       * semantic analysis, so in the IL we just unwrap the expression. *)
      expr env e
  | G.New (tok, ty, _cons_id_info, args) ->
      (* HACK: Fall-through case where we don't know to what variable the allocated
       * object is being assigned to. See HACK(new), we expect to intercept `New`
       * already in 'stmt_aux'.
       *)
      let lval = fresh_lval tok in
      let ss_args, args = arguments env (Tok.unbracket args) in
      let ss_ty, t = type_ env ty in
      let instr = mk_s (Instr (mk_i (New (lval, t, None, args)) eorig)) in
      (ss_args @ ss_ty @ [instr], mk_e (Fetch lval) NoOrig)
  | G.Call ({ e = G.IdSpecial spec; _ }, args) -> (
      let tok = snd spec in
      let ss_args, args = arguments env (Tok.unbracket args) in
      try
        let special = call_special env spec in
        let call_ss, call_exp = call_instr tok eorig ~void (fun res ->
            CallSpecial (res, special, args)) in
        (ss_args @ call_ss, call_exp)
      with
      | Fixme (kind, any_generic) ->
          let fixme = fixme_exp kind any_generic (related_exp g_expr) in
          let call_ss, call_exp = call_instr tok eorig ~void (fun res -> Call (res, fixme, args)) in
          (ss_args @ call_ss, call_exp))
  (* Clojure keyword-as-function [(:body m)]: a single-argument map
   * lookup by the atom [:body]. Lower as a field access
   * [Fetch (m, [Dot :body])] so the shape layer projects the caller's
   * [Obj] by the matching [Ostr] offset — symmetric with the
   * map-literal key lowering in [dict]. Macro-expanded chains like
   * [(-> m :body :nested)] arrive as nested [Call] forms, so the
   * recursive [nested_lval] on the argument handles the threaded
   * case. *)
  | G.Call
      ( { e = G.OtherExpr
              ( ("Atom", _),
                [ G.Name (G.IdQualified {
                    name_last = (s, atom_tok), _;
                    name_middle = Some (G.QDots [ ((prefix, _), _) ]);
                    _ }) ] ); _ } as callee,
        ( _, [ G.Arg map_expr ], _ ) )
    when env.lang =*= Lang.Clojure
         && (String.equal prefix ":" || String.equal prefix "::") ->
      let bare = String_.strip_wrapping_char ':' s in
      let field_name =
        { ident = (prefix ^ bare, atom_tok);
          sid = G.SId.unsafe_default;
          id_info = G.empty_id_info () }
      in
      let ss_map, map_lval = nested_lval env atom_tok map_expr in
      let offset = { o = Dot field_name; oorig = SameAs callee } in
      let field_lval =
        { map_lval with rev_offset = offset :: map_lval.rev_offset }
      in
      (ss_map, mk_e (Fetch field_lval) eorig)
  (* Clojure [(:body m default)]: two-argument form with a default
   * returned when the key is absent. Lower as [tmp = m.:body; if tmp
   * is falsy: tmp = default] so both the field-access and default
   * paths contribute to the result via [Lval_env]'s branch union. *)
  | G.Call
      ( { e = G.OtherExpr
              ( ("Atom", _),
                [ G.Name (G.IdQualified {
                    name_last = (s, atom_tok), _;
                    name_middle = Some (G.QDots [ ((prefix, _), _) ]);
                    _ }) ] ); _ } as callee,
        ( _, [ G.Arg map_expr; G.Arg default_expr ], _ ) )
    when env.lang =*= Lang.Clojure
         && (String.equal prefix ":" || String.equal prefix "::") ->
      let bare = String_.strip_wrapping_char ':' s in
      let field_name =
        { ident = (prefix ^ bare, atom_tok);
          sid = G.SId.unsafe_default;
          id_info = G.empty_id_info () }
      in
      let ss_map, map_lval = nested_lval env atom_tok map_expr in
      let offset = { o = Dot field_name; oorig = SameAs callee } in
      let field_lval =
        { map_lval with rev_offset = offset :: map_lval.rev_offset }
      in
      let tmp = fresh_var atom_tok in
      let tmp_lval = lval_of_base (Var tmp) in
      let assign_field =
        mk_s
          (Instr (mk_i (Assign (tmp_lval, mk_e (Fetch field_lval) eorig)) eorig))
      in
      (* Clojure is strict: [default] is evaluated eagerly at the call
       * site regardless of whether [m.:body] is nil. Lift its side
       * effects out of the [if] so they always run. *)
      let ss_default, default_exp = expr env default_expr in
      let null_lit = mk_e (Literal (G.Null atom_tok)) NoOrig in
      let cond_exp =
        mk_e
          (Operator
             ( (G.Eq, atom_tok),
               [ Unnamed (mk_e (Fetch tmp_lval) NoOrig); Unnamed null_lit ] ))
          NoOrig
      in
      let then_branch =
        [ mk_s (Instr (mk_i (Assign (tmp_lval, default_exp)) eorig)) ]
      in
      let if_stmt = mk_s (If (atom_tok, cond_exp, then_branch, [])) in
      ( ss_map @ [ assign_field ] @ ss_default @ [ if_stmt ],
        mk_e (Fetch tmp_lval) eorig )
  | G.Call (e, args) when env.lang =*= Lang.Clojure ->
      let tok = G.fake "call" in
      let arg_list = Tok.unbracket args in
      let arg_list_unwrapped =
        List_.map (function
            | G.Arg expr -> expr
            | _ -> failwith "Expected G.Arg")
          arg_list
      in
      let arg_container =
            [ G.Arg
                (G.Container
                   (G.List, Tok.unsafe_fake_bracket arg_list_unwrapped)
                 |> G.e) ]
      in
      call_generic env ~void tok eorig e (Tok.unsafe_fake_bracket arg_container)
  (* Ruby do-block flattening: `f(args) do |x| ... end` is parsed as
     Call(Call(f, args), [Lambda]) but the block is semantically an argument
     to f, not to its return value. Flatten into Call(f, args @ [Lambda]). *)
  | G.Call ({ e = G.Call (callee, inner_args); _ },
            (_, ([ G.Arg { G.e = G.Lambda _; _ } ] as outer_arg), _ ))
    when env.lang =*= Lang.Ruby ->
      let merged_args =
        Tok.unsafe_fake_bracket
          (Tok.unbracket inner_args @ outer_arg)
      in
      expr_aux env ~void (G.Call (callee, merged_args) |> G.e)
  (* Ruby library-call field-access idioms: [h.fetch(:k)],
   * [h.fetch(:k, default)], [obj.send(:k)], [obj.public_send(:k)],
   * [h.dig(:a, :b, …)]. Lower as a [Fetch] with a [Dot] offset (and
   * a conditional default-assign for the two-argument [fetch] form)
   * so the shape layer projects the caller's [Obj] by the matching
   * [Ostr] offset — symmetric with bracket access [h[:k]] and with
   * the Clojure keyword-as-function lowering above. [ruby_field_
   * access_decode] returns [None] for dynamic keys or unrecognised
   * shapes; in that case we fall through to the generic call
   * lowering rather than mis-rewriting. *)
  | G.Call
      ( { e = G.DotAccess (_, _, G.FN (G.Id ((method_name, _), _))); _ } as
          callee,
        args )
    when env.lang =*= Lang.Ruby
         && (match method_name with
            | "fetch"
            | "send"
            | "public_send"
            | "dig" ->
                true
            | _ -> false) -> (
      match (callee.G.e, ruby_field_access_decode method_name args) with
      | ( G.DotAccess (receiver, _, dig_name),
          Some (field_idents, tail_args, default_opt) ) ->
          let first_tok = snd (List.hd field_idents) in
          let ss_recv, recv_lval = nested_lval env first_tok receiver in
          let offsets =
            List.map
              (fun id ->
                let field_name =
                  {
                    ident = id;
                    sid = G.SId.unsafe_default;
                    id_info = G.empty_id_info ();
                  }
                in
                { o = Dot field_name; oorig = SameAs callee })
              field_idents
          in
          let field_lval =
            {
              recv_lval with
              rev_offset = List.rev offsets @ recv_lval.rev_offset;
            }
          in
          let field_exp = mk_e (Fetch field_lval) eorig in
          (match (default_opt, tail_args) with
          | None, [] -> (ss_recv, field_exp)
          | Some default_expr, [] ->
              (* [h.fetch(:k, default)]: Ruby is strict, evaluate
               * [default] eagerly before the conditional and emit
               * [tmp = h.k; if tmp == nil then tmp = default]. *)
              let tmp = fresh_var first_tok in
              let tmp_lval = lval_of_base (Var tmp) in
              let assign_field =
                mk_s
                  (Instr (mk_i (Assign (tmp_lval, field_exp)) eorig))
              in
              let ss_default, default_exp = expr env default_expr in
              let null_lit = mk_e (Literal (G.Null first_tok)) NoOrig in
              let cond_exp =
                mk_e
                  (Operator
                     ( (G.Eq, first_tok),
                       [
                         Unnamed (mk_e (Fetch tmp_lval) NoOrig);
                         Unnamed null_lit;
                       ] ))
                  NoOrig
              in
              let then_branch =
                [
                  mk_s
                    (Instr (mk_i (Assign (tmp_lval, default_exp)) eorig));
                ]
              in
              let if_stmt =
                mk_s (If (first_tok, cond_exp, then_branch, []))
              in
              ( ss_recv @ [ assign_field ] @ ss_default @ [ if_stmt ],
                mk_e (Fetch tmp_lval) eorig )
          | None, _ :: _ ->
              (* Hybrid [h.dig(:a, :b, x, …)]: the literal prefix
               * [:a; :b] is precise; the tail starts with a dynamic
               * key so we hand it back to the generic call as
               * [prefix_tmp.dig(tail_args)]. Emit [prefix_tmp =
               * h.a.b] first, then reconstruct a synthetic [G.Call
               * (G.DotAccess (prefix_tmp_as_expr, _, dig), tail_args)]
               * and feed it through [expr_aux]. *)
              let prefix_tmp = fresh_var first_tok in
              let prefix_tmp_lval = lval_of_base (Var prefix_tmp) in
              let assign_prefix =
                mk_s
                  (Instr (mk_i (Assign (prefix_tmp_lval, field_exp)) eorig))
              in
              let prefix_tmp_id = prefix_tmp.ident in
              let prefix_tmp_id_info = prefix_tmp.id_info in
              let prefix_tmp_g_expr =
                G.N (G.Id (prefix_tmp_id, prefix_tmp_id_info)) |> G.e
              in
              let dig_dot_tok = first_tok in
              let tail_call_gexpr =
                G.Call
                  ( G.DotAccess
                      ( prefix_tmp_g_expr,
                        dig_dot_tok,
                        G.FN (G.Id (("dig", first_tok), G.empty_id_info ())) )
                    |> G.e,
                    Tok.unsafe_fake_bracket tail_args )
                |> G.e
              in
              let _ = dig_name in
              let ss_call, call_exp = expr env tail_call_gexpr in
              (ss_recv @ [ assign_prefix ] @ ss_call, call_exp)
          | Some _, _ :: _ ->
              (* Unreachable: [fetch] is a 1- or 2-arg form, [dig]
               * never carries a default. Fall through defensively
               * rather than assert. *)
              Log.warn (fun m ->
                  m
                    "Ruby field-access decode produced an unexpected \
                     (default + tail) combination; falling through");
              let tok = G.fake "call" in
              call_generic env ~void tok eorig callee args)
      | _ ->
          let tok = G.fake "call" in
          call_generic env ~void tok eorig callee args)
  | G.Call (e, args) ->
      let tok = G.fake "call" in
      call_generic env ~void tok eorig e args
  | G.L lit -> ([], mk_e (Literal lit) eorig)
  | G.DotAccess ({ e = N (Id (("var", _), _)); _ }, _, FN (Id ((s, t), id_info)))
    when is_hcl env.lang ->
      (* We need to change all uses of a variable, which looks like a DotAccess, to a name which
         reads the same. This is so that our parameters to our function can properly be recognized
         as tainted by the taint engine.
      *)
      expr_aux env (G.N (Id (("var." ^ s, t), id_info)) |> G.e)
  | G.N _
  | G.DotAccess (_, _, _)
  | G.ArrayAccess (_, _)
  | G.DeRef (_, _) ->
      let ss_lv, lval = lval env g_expr in
      (ss_lv, mk_e (Fetch lval) eorig)
  (* x = ClassName(args ...) in Python *)
  (* ClassName has been resolved to __init__ by the pro engine. *)
  (* Identified and treated as x = New ClassName(args ...) to support
     field sensitivity. See HACK(new) *)
  | G.Assign
      ( ({
           e =
             G.N
               (G.Id ((_, _), { id_type = { contents = Some ret_ty }; _ }) as
                obj);
           _;
         } as obj_e),
        _,
        ({
           e =
             G.Call
               ( {
                   e =
                     ( G.N (Id (_, id_info))
                     (* Module paths are currently parsed into
                        dotaccess so m.ClassName() is completely
                        valid. *)
                     | G.DotAccess (_, _, FN (Id (_, id_info))) );
                   _;
                 },
                 args );
           _;
         } as origin_exp) )
    when is_constructor env ret_ty id_info ->
      let obj' = var_of_name obj in
      let lval, ss =
        class_construction env obj' origin_exp ret_ty id_info args
      in
      (ss, mk_e (Fetch lval) (SameAs obj_e))
  | G.Assign (e1, tok, e2) ->
      let ss_e2, exp = expr env e2 in
      let ss_assign, result = assign env ~g_expr e1 tok exp in
      (ss_e2 @ ss_assign, result)
  | G.AssignOp (e1, (G.Eq, tok), e2) ->
      (* AsssignOp(Eq) is used to represent plain assignment in some languages,
       * e.g. Go's `:=` is represented as `AssignOp(Eq)`, and C#'s assignments
       * are all represented this way too. *)
      let ss_e2, exp = expr env e2 in
      let ss_assign, result = assign env ~g_expr e1 tok exp in
      (ss_e2 @ ss_assign, result)
  | G.AssignOp (e1, op, e2) ->
      let ss_e2, exp = expr env e2 in
      let ss_lv, lval = lval env e1 in
      let lvalexp = mk_e (Fetch lval) (SameAs e1) in
      let opexp =
        mk_e
          (Operator (op, [ Unnamed lvalexp; Unnamed exp ]))
          (related_tok (snd op))
      in
      let instr = mk_s (Instr (mk_i (Assign (lval, opexp)) eorig)) in
      (ss_e2 @ ss_lv @ [instr], lvalexp)
  | G.LetPattern (pat, e) ->
      let ss_e, exp = expr env e in
      let new_stmts = pattern_assign_statements env ~eorig exp pat in
      (ss_e @ new_stmts, mk_unit (G.fake "()") NoOrig)
  (* TODO: Use instead of ExprBlock? But we also have scope for block. *)
  | G.Seq xs -> (
      match List.rev xs with
      | [] -> impossible (G.E g_expr)
      | last :: xs ->
          let xs = List.rev xs in
          let ss_list = List.map (fun x ->
              let ss, _e = expr env x in ss) xs
          in
          let ss_last, e_last = expr env last in
          (List.concat ss_list @ ss_last, e_last))
  | G.Record fields ->
      let ss, e = record env fields in
      (ss, e)
  | G.Container (G.Dict, (l, entries, r))
    when AST_modifications.is_lua_array_table env.lang entries ->
      (* Lua array-like table: {1, 2, 3} parsed as Dict with NextArrayIndex keys *)
      let values = AST_modifications.extract_lua_array_values entries in
      let results = List.map (fun v ->
        let ss, e = expr env v in (ss, e)) values in
      let ss = List.concat_map fst results in
      let vs = List.map snd results in
      (ss, mk_e (Composite (CList, (l, vs, r))) eorig)
  | G.Container (G.Dict, xs) ->
      let ss, e = dict env xs g_expr in
      (ss, e)
  | G.Container (kind, xs) ->
      let l, xs, r = xs in
      let results = List.map (fun x ->
        let ss, e = expr env x in (ss, e)) xs in
      let ss = List.concat_map fst results in
      let xs = List.map snd results in
      let kind = composite_kind ~g_expr kind in
      (ss, mk_e (Composite (kind, (l, xs, r))) eorig)
  | G.Comprehension (_op, (_l, (er, clauses), _r)) ->
      comprehension env er clauses
  | G.Lambda fdef ->
      let lval = fresh_lval ~str:"_tmp_lambda" (snd fdef.fkind) in
      let final_fdef =
        (* NOTE: Reset control-flow labels so that break/continue/recur from
         * the enclosing scope don't bleed into the lambda body. *)
        function_definition
          { env with cont_label = None;
                     break_labels = [];
                     rec_point_label = None;
                     rec_point_lvals = None }
          fdef
      in
      let instr = mk_s (Instr (mk_i (AssignAnon (lval, Lambda final_fdef)) eorig)) in
      ([instr], mk_e (Fetch lval) eorig)
  | G.AnonClass def ->
      (* TODO: should use def.ckind *)
      let tok = Common2.fst3 def.G.cbody in
      let lval = fresh_lval tok in
      let instr = mk_s (Instr (mk_i (AssignAnon (lval, AnonClass def)) eorig)) in
      ([instr], mk_e (Fetch lval) eorig)
  | G.IdSpecial (spec, tok) -> (
      let opt_var_special =
        match spec with
        | G.This -> Some This
        | G.Super -> Some Super
        | G.Self -> Some Self
        | G.Parent -> Some Parent
        | _ -> None
      in
      match opt_var_special with
      | Some var_special ->
          let lval = lval_of_base (VarSpecial (var_special, tok)) in
          ([], mk_e (Fetch lval) eorig)
      | None -> impossible (G.E g_expr))
  | G.SliceAccess (_, _) -> todo (G.E g_expr)
  (* e1 ? e2 : e3 ==>
   *  pre: lval = e1;
   *       if(lval) { lval = e2 } else { lval = e3 }
   *  exp: lval
   *)
  | G.Conditional (e1_gen, e2_gen, e3_gen) ->
      let tok = G.fake "conditional" in
      let lval = fresh_lval tok in

      let ss_for_e1, e1 = expr env e1_gen in
      let ss_for_e2, e2 = expr env e2_gen in
      let ss_for_e3, e3 = expr env e3_gen in

      let if_stmt =
        mk_s
          (If
             ( tok,
               e1,
               ss_for_e2 @ [ mk_s (Instr (mk_i (Assign (lval, e2)) NoOrig)) ],
               ss_for_e3 @ [ mk_s (Instr (mk_i (Assign (lval, e3)) NoOrig)) ]
             ))
      in
      (ss_for_e1 @ [if_stmt], mk_e (Fetch lval) eorig)
  | G.Await (tok, e1orig) ->
      let ss_e1, e1 = expr env e1orig in
      let tmp = fresh_lval tok in
      let instr = mk_s (Instr (mk_i (CallSpecial (Some tmp, (Await, tok), [ Unnamed e1 ])) eorig)) in
      (ss_e1 @ [instr], mk_e (Fetch tmp) NoOrig)
  | G.Yield (tok, e1orig_opt, _) ->
      let ss_yield, yield_args =
        match e1orig_opt with
        | None -> ([], [])
        | Some e1orig ->
            let ss, y = expr env e1orig in
            (ss, [ y ])
      in
      let instr = mk_s (Instr (mk_i (CallSpecial (None, (Yield, tok), mk_unnamed_args yield_args)) eorig)) in
      (ss_yield @ [instr], mk_unit tok NoOrig)
  | G.Ref (tok, e1orig) ->
      let ss_e1, e1 = expr env e1orig in
      let tmp = fresh_lval tok in
      let instr = mk_s (Instr (mk_i (CallSpecial (Some tmp, (Ref, tok), [ Unnamed e1 ])) eorig)) in
      (ss_e1 @ [instr], mk_e (Fetch tmp) NoOrig)
  (* Rust struct literal [S { f1: v1; f2: v2 }]: each element in
   * [esorig] arrives as [G.Assign (N (Id f), _, v)] — a field-init.
   * Lower as a [RecordOrDict] of [Entry (L (String f), v)] so the
   * shape layer builds per-field [Ostr] cells and caller-side
   * field-sensitivity survives the struct construction — the default
   * [Composite (Constructor, …)] is positional and collapses fields. *)
  | G.Constructor (_cname, (_tok1, esorig, _tok2))
    when env.lang =*= Lang.Rust && esorig <> []
         && List.for_all
              (function
                | { G.e = G.Assign ({ G.e = G.N (G.Id _); _ }, _, _); _ } ->
                    true
                | _ -> false)
              esorig ->
      (* Each element is guaranteed by the [for_all] guard to be a
       * field-init [Assign (N (Id …), _, _)]; any non-matching shape
       * would indicate a parser change, in which case we skip that
       * element rather than assert — a missing [Entry] is a precision
       * loss, not a crash — and emit a warning so the drift is
       * traceable. *)
      let entries_ss, entries =
        esorig
        |> List.filter_map (fun eiorig ->
               match eiorig.G.e with
               | G.Assign
                   ( { G.e = G.N (G.Id ((field_name, field_tok), _)); _ },
                     _,
                     value_expr ) ->
                   let ss_v, ve = expr env value_expr in
                   let key =
                     mk_e
                       (Literal
                          (G.String
                             (Tok.unsafe_fake_bracket (field_name, field_tok))))
                       (related_tok field_tok)
                   in
                   Some (ss_v, Entry (key, ve))
               | _ ->
                   Log.warn (fun m ->
                       m
                         "Rust struct-literal field init had unexpected \
                          shape; skipping (element: %s)"
                         (G.show_expr_kind eiorig.G.e));
                   None)
        |> List.split
      in
      let ss = List.concat entries_ss in
      (ss, mk_e (RecordOrDict entries) eorig)
  | G.Constructor (cname, (tok1, esorig, tok2)) ->
      let cname = var_of_name cname in
      let results = List.map (fun eiorig ->
          let ss, e = expr env eiorig in (ss, e)) esorig
      in
      let ss = List.concat_map fst results in
      let es = List.map snd results in
      (ss, mk_e (Composite (Constructor cname, (tok1, es, tok2))) eorig)
  | G.RegexpTemplate ((l, e, r), _opt) ->
      let ss_e, e = expr env e in
      (ss_e, mk_e (Composite (Regexp, (l, [ e ], r))) NoOrig)
  | G.Xml xml -> xml_expr env ~void eorig xml
  | G.Cast (typ, _, e) ->
      let ss_e, e = expr env e in
      (ss_e, mk_e (Cast (typ, e)) eorig)
  | G.Alias (_alias, e) -> expr env e
  | G.LocalImportAll (_module, _tk, e) ->
      (* TODO: what can we do with _module? *)
      expr env e
  | G.Ellipsis _
  | G.TypedMetavar (_, _, _)
  | G.DisjExpr (_, _)
  | G.DeepEllipsis _
  | G.DotAccessEllipsis _ ->
      sgrep_construct (G.E g_expr)
  | G.StmtExpr st -> stmt_expr env ~g_expr st
  | G.OtherExpr (("ShortLambda", _), _) when env.lang =*= Lang.Elixir ->
      let lambda_expr = AST_modifications.convert_elixir_short_lambda g_expr in
      expr env lambda_expr
  (* Elixir pipe: OtherExpr("PipelineCall", [E call]) is a desugared
   * x |> f(a) => f(x, a). The tag preserves search distinction; for
   * IL/taint we evaluate the inner call transparently. *)
  | G.OtherExpr (("PipelineCall", _tk), [ G.E inner ])
    when env.lang =*= Lang.Elixir ->
      expr env inner
  (* The idea here is that this is like a block, and we only
   * really care about the last expression. *)
  (* TODO: What if a statement creeps in? E.g. an If, `fn`..?
   * What we really must avoid is Block. *)
  | G.OtherExpr
      (* Other cases are macroexpanded to these. TODO: Confirm which. *)
      ((todo_kind,
        tok), any_exprs)
    when env.lang =*= Lang.Clojure && CLJ_ME1.expands_as_block todo_kind ->
    let results =
      List.map
        (fun any_expr ->
          match any_expr with
          | G.E exp ->
              let ss, e = expr env exp in (ss, e)
          | _else_ -> ([], fixme_exp ToDo any_expr (related_tok tok)))
        any_exprs
    in
    let all_ss = List.concat_map fst results in
    let exprs = List.map snd results in
    begin match List.rev exprs with
    | [] -> (all_ss, mk_unit (G.fake "()") NoOrig)
    | e_last :: _e_rest -> (all_ss, e_last)
    end
  (* Clojure loop. *)
  | G.OtherExpr (("Loop", tok),
                 G.E { e = G.OtherExpr
                           (("LoopPatternBindings", lpbs_tok),
                            bindings); _ }
                 :: body_exprs)
    when env.lang =*= Lang.Clojure ->
    let env =
      {env with rec_point_lvals = None; rec_point_label = None}
    in
    let (binding_ss_rev, lvals) =
      List.fold_left
        (fun (ss_acc, lvals) any_expr ->
          match any_expr with
          | G.E {e = G.LetPattern (pat, e); _} ->
            let ss_e, exp = expr env e in
            let lval, new_stmts =
              try
                let pre_ss, lval, post_ss = pattern env pat in
                (Some lval,
                 pre_ss @ [ mk_s (Instr (mk_i (Assign (lval, exp)) eorig)) ] @ post_ss)
              with
              | Fixme (kind, any_generic) ->
                  (None, fixme_stmt kind any_generic)
            in
            begin match lval with
              | Some lval -> (ss_e @ new_stmts) :: ss_acc, lval :: lvals
              | None -> (ss_e @ new_stmts) :: ss_acc, lvals
            end
          | _else_ -> (* This should not happen in our translation *) ss_acc, lvals)
        ([], []) bindings
    in
    let binding_ss = List.concat (List.rev binding_ss_rev) in
    let env =
      { env with rec_point_lvals = Loop_rec_point (List.rev lvals) |> Option.some }
    in
    let _rec_point_label, rec_point_label_stmts, rec_point_env =
      recursion_point_label env lpbs_tok
    in
    let env = rec_point_env in
    let body_results =
      List.map
        (fun any_expr ->
          match any_expr with
          | G.E exp ->
              let ss, e = expr env exp in (ss, e)
          | _else_ -> ([], fixme_exp ToDo any_expr (related_tok tok)))
        body_exprs
    in
    let body_ss = List.concat_map fst body_results in
    let exprs = List.map snd body_results in
    begin match List.rev exprs with
    | [] -> (binding_ss @ rec_point_label_stmts @ body_ss, mk_unit (G.fake "()") NoOrig)
    | e_last :: _e_rest -> (binding_ss @ rec_point_label_stmts @ body_ss, e_last)
    end
  (* Clojure recur. *)
  | G.OtherExpr (("Recur", tok), args)
    when env.lang =*= Lang.Clojure ->
    let recur_ss =
      match env.rec_point_lvals with
      | Some (Loop_rec_point rec_point_lvals) ->
        let rec_point_lvals, args =
        match (List.length rec_point_lvals) - (List.length args) with
        | 0 -> (rec_point_lvals, args)
        (* User did not pass all values for bindings, add nil bindings. *)
        | n when n > 0 ->
          (rec_point_lvals,
           args @ (List.init n (fun _ ->
               G.E (G.L (G.Null (G.fake "nil")) |> G.e))))
        (* User passed more values than expected for bindings; ignore
         * the extra ones. *)
        | n (* < 0 *) -> (List.take n rec_point_lvals, args)
      in
      (* Assign bindings to recursion point lvals. *)
      List.fold_left2
        (fun ss_acc lval arg_expr ->
           let arg = match arg_expr with
             | G.E e -> e
             | _else_ -> impossible (G.E g_expr)
           in
          let ss_arg, arg_exp = expr env arg in
          let instr = mk_s (Instr (mk_i (Assign (lval, arg_exp)) NoOrig)) in
          ss_acc @ ss_arg @ [instr])
        [] rec_point_lvals args
      (* In this case there is one lval, which is where the unique
       * parameter is stored in clojure, ie, before destructuring.
       * TODO: How about short lambdas? *)
      | Some (Fn_rec_point lval) ->
        let args = List_.map (function
            | G.E e -> e
            | _else_ -> impossible (G.E g_expr))
          args
        in
        let ss_cont, arg_container_exp =
          G.Container (G.List, Tok.unsafe_fake_bracket args) |> G.e
          |> expr env
        in
        let instr = mk_s (Instr (mk_i (Assign (lval, arg_container_exp)) NoOrig)) in
        ss_cont @ [instr]
      | _ ->
        impossible (G.E g_expr)
    in
    begin match env.rec_point_label with
      | None ->
        impossible (G.Tk tok)
      | Some lbl ->
        (recur_ss @ [ mk_s (Goto (tok, lbl)) ], mk_unit tok NoOrig)
    end
  (* OtherExpr("Apply", Call) ~> Call(concat [a_1 .. n_k] a_k+1).
   * So the difference is in how we construct the arguments vector. *)
  | G.OtherExpr (("Apply", _tok), [ G.E ({e = Call(e, args); _} as call_exp) ])
    when env.lang =*= Lang.Clojure ->
      let tok = G.fake "call" in
      let arg_list = Tok.unbracket args in
      let arg_list_unwrapped =
        List_.map (function
            | G.Arg expr -> expr
            | _ -> failwith "Expected G.Arg")
          arg_list
      in
      begin match e.G.e, List.rev arg_list_unwrapped with
        | G.IdSpecial _, _ | _, [] ->
          (* No arguments or special, fallback to standard call.
           * TODO: For idSpecial with args, use Call IdSpecial Spread
           * to at least show the correct semantics. *)
          expr env call_exp
        | _, last_arg :: rest_args_rev ->
          let rest_args_container =
                (G.Container
                   (G.List, Tok.unsafe_fake_bracket (List.rev rest_args_rev))
                 |> G.e)
          in
          let apply_arg =
            [ G.Arg (G.opcall (G.Concat, G.fake "concat")
                       [ rest_args_container; last_arg ]) ]
          in
          call_generic env ~void tok eorig e (Tok.unsafe_fake_bracket apply_arg)
      end
  (* Clojure: a kind of macroexpansion (macroexpand-1). *)
  | G.OtherExpr ((todo_kind, tok), _ :: _)
    when env.lang =*= Lang.Clojure && CLJ_ME1.is_macroexpandable todo_kind ->
    (try let macro_expanded = CLJ_ME1.macro_expand_1 (* env? *) g_expr in
    expr env macro_expanded
    with
    | CLJ_ME1.Macroexpansion_error (_msg, any_expr) ->
      ([], fixme_exp ToDo any_expr (related_tok tok))
    | exn -> raise exn)
  (* Default. *)
  | G.OtherExpr ((str, tok), xs) ->
      let results =
        List.map
          (fun x ->
            match x with
            | G.E e1orig ->
                let ss, e = expr env e1orig in (ss, e)
            | __else__ -> ([], fixme_exp ToDo x (related_tok tok)))
          xs
      in
      let ss = List.concat_map fst results in
      let es = List.map snd results in
      let other_expr = mk_e (Composite (CTuple, (tok, es, tok))) eorig in
      let aux_ss, _, tmp = aux_var ~str env tok other_expr in
      let partial = mk_e (Fetch tmp) (related_tok tok) in
      (ss @ aux_ss, fixme_exp ToDo (G.E g_expr) (related_tok tok) ~partial)
  | G.RawExpr _ -> todo (G.E g_expr)

and expr env ?void e_gen : stmts * exp =
  try expr_aux env ?void e_gen with
  | Fixme (kind, any_generic) ->
      ([], fixme_exp kind any_generic (related_exp e_gen))

and expr_opt env tok : G.expr option -> stmts * exp = function
  | None ->
      let void = G.Unit tok in
      ([], mk_e (Literal void) (related_tok tok))
  | Some e -> expr env e

and expr_lazy_op env op tok arg0 args eorig : stmts * exp =
  let ss0, arg0' = argument env arg0 in
  let (acc_ss, _), args' =
    (* Consider A && B && C, side-effects in B must only take effect `if A`,
     * and side-effects in C must only take effect `if A && B`. *)
    args
    |> List.fold_left_map
         (fun (acc_ss, cond) argi ->
           let ssi, argi' = argument env argi in
           let if_ss =
             if ssi <> [] then [mk_s @@ If (tok, cond, ssi, [])]
             else []
           in
           let condi =
             mk_e (Operator ((op, tok), [ Unnamed cond; argi' ])) eorig
           in
           ((acc_ss @ if_ss, condi), argi'))
         ([], IL_helpers.exp_of_arg arg0')
  in
  (ss0 @ acc_ss, mk_e (Operator ((op, tok), arg0' :: args')) eorig)

and call_generic env ?(void = false) tok eorig e args : stmts * exp =
  let ss_e, e = expr env e in
  let ss_args, args = arguments env (Tok.unbracket args) in
  let call_ss, call_exp = call_instr tok eorig ~void (fun res -> Call (res, e, args)) in
  (ss_e @ ss_args @ call_ss, call_exp)

and call_special _env (x, tok) : call_special * Tok.t =
  ( (match x with
    | G.Op _
    | G.IncrDecr _
    | G.This
    | G.Super
    | G.Self
    | G.Parent
    | G.InterpolatedElement ->
        impossible (G.E (G.IdSpecial (x, tok) |> G.e))
    (* should be intercepted before *)
    | G.Eval -> Eval
    | G.Typeof -> Typeof
    | G.Instanceof -> Instanceof
    | G.Sizeof -> Sizeof
    | G.ConcatString _kindopt -> Concat
    | G.Spread -> SpreadFn
    | G.Require -> Require
    | G.EncodedString _
    | G.Defined
    | G.HashSplat
    | G.ForOf
    | G.NextArrayIndex ->
        todo (G.E (G.IdSpecial (x, tok) |> G.e))),
    tok )

and composite_kind ~g_expr : G.container_operator -> IL.composite_kind = function
  | G.Array -> CArray
  | G.List -> CList
  | G.Dict -> impossible (E g_expr)
  | G.Set -> CSet
  | G.Tuple -> CTuple

(* TODO: dependency of order between arguments for instr? *)
and arguments env xs : stmts * exp argument list =
  let results = List.map (argument env) xs in
  let ss = List.concat_map fst results in
  let args = List.map snd results in
  (ss, args)

and argument env arg : stmts * exp argument =
  match arg with
  | G.Arg e ->
      let ss, arg = expr env e in
      (ss, Unnamed arg)
  | G.ArgKwd (id, e)
  | G.ArgKwdOptional (id, e) ->
      let ss, arg = expr env e in
      (ss, Named (id, arg))
  | G.ArgType { t = TyExpr e; _ } ->
      let ss, arg = expr env e in
      (ss, Unnamed arg)
  | __else__ ->
      let any = G.Ar arg in
      ([], Unnamed (fixme_exp ToDo any (Related any)))

and record env ((_tok, origfields, _) as record_def) : stmts * exp =
  let e_gen = G.Record record_def |> G.e in
  let results =
    List.map
      (fun x ->
        match x with
        | G.F
            {
              s =
                G.DefStmt
                  ( { G.name = G.EN (G.Id (id, id_info)); tparams = None; _ },
                    def_kind );
              _;
            } as forig ->
            let field_name = var_of_id_info id id_info in
            let ss, field_def =
              match def_kind with
              (* TODO: Consider what to do with vtype. *)
              | G.VarDef { G.vinit = Some fdeforig; _ }
              | G.FieldDefColon { G.vinit = Some fdeforig; _ } ->
                  expr env fdeforig
              (* Some languages such as javascript allow function
                 definitions in object literal syntax. *)
              | G.FuncDef fdef ->
                  let lval = fresh_lval ~str:"_tmp_lambda" (snd fdef.fkind) in
                  (* See NOTE about resetting control-flow labels for lambdas. *)
                  let fdef =
                    function_definition
                      { env with cont_label = None;
                                 rec_point_label = None;
                                 break_labels = [];
                                 rec_point_lvals = None }
                      fdef
                  in
                  let forig = Related (G.Fld forig) in
                  let instr = mk_s (Instr (mk_i (AssignAnon (lval, Lambda fdef)) forig)) in
                  ([instr], mk_e (Fetch lval) forig)
              | ___else___ -> todo (G.E e_gen)
            in
            (ss, Some (Field (field_name, field_def)))
        | G.F
            {
              s =
                G.ExprStmt
                  ( {
                      e =
                        Call
                          ({ e = IdSpecial (Spread, _); _ }, (_, [ Arg e ], _));
                      _;
                    },
                    _ );
              _;
            } ->
            let ss, expression = expr env e in
            (ss, Some (Spread expression))
        | G.F
            {
              s =
                G.ExprStmt
                  ( ({
                       e =
                         Call
                           ( { e = N (Id (id, id_info)); _ },
                             (_, [ Arg { e = Record fields; _ } ], _) );
                       _;
                     } as prior_expr),
                    _ );
              _;
            }
          when is_hcl env.lang ->
            let field_name = var_of_id_info id id_info in
            let ss, field_expr = record env fields in
            (ss, Some
              (Field
                 (field_name, { field_expr with eorig = SameAs prior_expr })))
        | _ when is_hcl env.lang ->
            (* For HCL constructs such as `lifecycle` blocks within a module call, the
                IL translation engine will brick the whole record if it is encountered.
                To avoid this, we will just ignore any unrecognized fields for HCL specifically.
             *)
            log_warning "Skipping HCL record field during IL translation";
            ([], None)
        | G.F _ -> todo (G.E e_gen))
      origfields
  in
  let all_ss = List.concat_map fst results in
  let fields = List.filter_map snd results in
  (all_ss, mk_e (RecordOrDict fields) (SameAs e_gen))

(* Extract a literal field-key identifier from an expression used as
 * an index or an atom/string library-call key. Used by the per-
 * language library-call field-access recognisers (Ruby, future
 * Python/Java/…) to decide when to lower [x.get("k")] / [x.fetch(:k)]
 * / similar idioms as a [Fetch] with a [Dot] offset. Returns [None]
 * for dynamic keys so callers can fall through to the generic call
 * lowering without losing correctness. *)
and literal_field_ident (e : G.expr) : G.ident option =
  match e.G.e with
  | G.L (G.Atom (_, id)) -> Some id
  | G.L (G.String (_, id, _)) -> Some id
  | _ -> None

(* Walk a [G.argument] list and return the longest prefix of literal
 * field keys together with whatever follows (possibly empty). Used
 * to give partial field-sensitivity to mixed-key accessors like
 * Ruby's [h.dig(:a, :b, x, :c)]: the [:a; :b] prefix is a precise
 * [Fetch] chain, while [x, :c] fall back to a generic call on the
 * intermediate result. *)
and longest_literal_key_prefix (args : G.argument list) :
    G.ident list * G.argument list =
  let rec go acc = function
    | G.Arg e :: rest -> (
        match literal_field_ident e with
        | Some id -> go (id :: acc) rest
        | None -> (List.rev acc, G.Arg e :: rest))
    | other -> (List.rev acc, other)
  in
  go [] args

(* Decode a Ruby library-call field-access. Returns [None] for
 * dynamic keys or unrecognised shapes so the caller can fall back
 * to the generic call lowering. Covers:
 * - [h.fetch(:k)]             → ([:k], [], None)
 * - [h.fetch(:k, default)]    → ([:k], [], Some default)
 * - [obj.send(:m)]            → ([:m], [], None)
 * - [obj.public_send(:m)]     → ([:m], [], None)
 * - [h.dig(:a, :b, …)]        → ([:a; :b; …], [tail_args], None)
 * The [tail_args] list is empty when every [dig] key is literal;
 * when non-empty, the literal prefix is precise and the tail is
 * handed to the generic call as [prefix_lval.dig(tail_args)]. *)
and ruby_field_access_decode (method_name : string) (args : G.arguments) :
    (G.ident list * G.argument list * G.expr option) option =
  let arg_list = Tok.unbracket args in
  match (method_name, arg_list) with
  | ("fetch" | "send" | "public_send"), [ G.Arg key_expr ] -> (
      match literal_field_ident key_expr with
      | Some id -> Some ([ id ], [], None)
      | None -> None)
  | "fetch", [ G.Arg key_expr; G.Arg default_expr ] -> (
      match literal_field_ident key_expr with
      | Some id -> Some ([ id ], [], Some default_expr)
      | None -> None)
  | "dig", (_ :: _ as key_args) ->
      let prefix, tail = longest_literal_key_prefix key_args in
      if List.compare_length_with prefix 0 > 0 then Some (prefix, tail, None)
      else None
  | _ -> None

and clojure_atom_key_literal (e : G.expr) : exp option =
  (* When [env.lang] is Clojure, a map-literal key like [:body] or
   * [::body] arrives here as [OtherExpr("Atom", [Name IdQualified
   * {name_middle = Some (QDots [(prefix, _)]); name_last = (s, tok)}])],
   * where [prefix] is [":" / "::"] — the auto-resolved-namespace
   * marker — and [s] carries the atom's surface text (still prefixed
   * with a [:]). We need [:body] and [::body] to produce distinct
   * [Ostr] offsets downstream so field-sensitive taint can tell them
   * apart; emit a [String] literal whose text is [prefix ^ bare] so
   * that [:body] stays [":body"] and [::body] stays ["::body"].
   * [G.Atom] cannot currently carry the [:]/[::] distinction, hence
   * the bespoke path here — see the comment on
   * [key_offset_of_pattern] for the matching pattern-side. *)
  match e.G.e with
  | G.OtherExpr
      ( ("Atom", _),
        [
          G.Name
            (G.IdQualified
              {
                name_last = (s, tok), _;
                name_middle = Some (G.QDots [ ((prefix, _), _) ]);
                _;
              });
        ] )
    when String.equal prefix ":" || String.equal prefix "::" ->
      let bare = String_.strip_wrapping_char ':' s in
      Some
        (mk_e
           (Literal (G.String (Tok.unsafe_fake_bracket (prefix ^ bare, tok))))
           (SameAs e))
  | _ -> None

and dict env (_, orig_entries, _) orig : stmts * exp =
  let entry_of_kv korig vorig =
    let ss_v, ve = expr env vorig in
    match
      if env.lang =*= Lang.Clojure then clojure_atom_key_literal korig else None
    with
    | Some ke -> (ss_v, Entry (ke, ve))
    | None ->
        let ss_k, ke = expr env korig in
        (ss_k @ ss_v, Entry (ke, ve))
  in
  let results =
    List.map
      (fun orig_entry ->
        match orig_entry.G.e with
        | G.Container (G.Tuple, (_, [ korig; vorig ], _)) ->
            entry_of_kv korig vorig
        | G.OtherExpr ((("MapPairArrow" | "MapPairKeyword"), _), [ G.E inner ])
          when env.lang =*= Lang.Elixir ->
            (match inner.G.e with
            | G.Container (G.Tuple, (_, [ korig; vorig ], _)) ->
                entry_of_kv korig vorig
            | _ -> todo (G.E orig))
        | __else__ -> todo (G.E orig))
      orig_entries
  in
  let ss = List.concat_map fst results in
  let entries = List.map snd results in
  (ss, mk_e (RecordOrDict entries) (SameAs orig))

and xml_expr env ~void eorig xml : stmts * exp =
  let tok, jsx_name =
    match xml.G.xml_kind with
    | G.XmlClassic (tok, name, _, _)
    | G.XmlSingleton (tok, name, _) ->
        (tok, Some name)
    | G.XmlFragment (tok, _) -> (tok, None)
  in
  let body_results =
    List.map
      (fun x ->
        match x with
        | G.XmlExpr (tok, Some eorig, _) ->
            let ss_e, exp = expr env eorig in
            let aux_ss, _, lval = aux_var env tok exp in
            (ss_e @ aux_ss, Some (mk_e (Fetch lval) (SameAs eorig)))
        | G.XmlXml xml' ->
            let eorig' = SameAs (G.Xml xml' |> G.e) in
            let ss, xml_e = xml_expr env ~void:false eorig' xml' in
            (ss, Some xml_e)
        | G.XmlExpr (_, None, _)
        | G.XmlText _ ->
            ([], None))
      xml.G.xml_body
  in
  let body_ss = List.concat_map fst body_results in
  let filtered_body = List.filter_map snd body_results in
  match jsx_name with
  | Some jsx_name when Lang.is_js env.lang ->
      (* Model `<Foo x={y}>{bar}</Foo>` as `Foo({x: y, children: [bar])`
       *
       * Technically, this should be modeled as `React.createElement(Foo, {x:
       * y}, bar)`, and we should then correctly track taint through the call to
       * `React.createElement`. But realistically we can shortcut that and just
       * model it as a direct call.
       *
       * This works for functional components, which are standard practice these
       * days. In order to correctly model older kinds of React components,
       * we'll need to do more work. *)
      let name_eorig = SameAs (G.N jsx_name |> G.e) in
      let name_lval = name jsx_name in
      let e = mk_e (Fetch name_lval) name_eorig in
      let attr_results =
        List.map
          (fun x ->
            match x with
            | G.XmlAttr (id, tok, eorig) ->
                (* e.g. <Foo x={y}/> *)
                let attr_name =
                  {
                    ident = id;
                    sid = G.SId.unsafe_default;
                    id_info = G.empty_id_info ();
                  }
                in
                let ss_e, e = expr env eorig in
                let aux_ss, _, lval = aux_var env tok e in
                let e = mk_e (Fetch lval) (SameAs eorig) in
                (ss_e @ aux_ss, Some (Field (attr_name, e)))
            | G.XmlAttrExpr (_l, eorig, _r) ->
                let ss, e = expr env eorig in
                (ss, Some (Spread e))
            | G.XmlEllipsis _ ->
                (* Should never encounter this in a target *)
                ([], None))
          xml.G.xml_attrs
      in
      let attrs_ss = List.concat_map fst attr_results in
      let body_exp =
        mk_e
          (Composite (CArray, Tok.unsafe_fake_bracket filtered_body))
          (Related (G.Xmls xml.G.xml_body))
      in
      let children_field_name =
        {
          ident = ("children", G.fake "children");
          sid = G.SId.unsafe_default;
          id_info = G.empty_id_info ();
        }
      in
      let filtered_fields = List.filter_map snd attr_results in
      let fields = Field (children_field_name, body_exp) :: filtered_fields in
      let fields_orig =
        let attrs = xml.G.xml_attrs |> List_.map (fun attr -> G.XmlAt attr) in
        let body = G.Xmls xml.G.xml_body in
        Related (G.Anys (body :: attrs))
      in
      let record = mk_e (RecordOrDict fields) fields_orig in
      let args = [ Unnamed record ] in
      let call_ss, call_exp = call_instr tok eorig ~void (fun res -> Call (res, e, args)) in
      (body_ss @ attrs_ss @ call_ss, call_exp)
  | Some _
  | None ->
      let attr_results =
        List.map
          (fun x ->
            match x with
            | G.XmlAttr (_, tok, eorig)
            | G.XmlAttrExpr (tok, eorig, _) ->
                let ss_e, exp = expr env eorig in
                let aux_ss, _, lval = aux_var env tok exp in
                (ss_e @ aux_ss, Some (mk_e (Fetch lval) (SameAs eorig)))
            | _ -> ([], None))
          xml.G.xml_attrs
      in
      let attrs_ss = List.concat_map fst attr_results in
      let filtered_attrs = List.filter_map snd attr_results in
      ( body_ss @ attrs_ss,
        mk_e
          (Composite
             (CTuple, (tok, List.rev_append filtered_attrs filtered_body, tok)))
          (Related (G.Xmls xml.G.xml_body)) )

(* Build a single foreach loop around [inner_body] IL stmts.
 * Adjusted from for_each; used by [comprehension] below. *)
and comprehension_loop env tok_for (pat : G.pattern) (tok_in : tok)
    (collection_expr : G.expr) (inner_body : stmts) : stmts =
  let cont_label_s, break_label_s, env = break_continue_labels env tok_for in
  let ss, e' = expr env collection_expr in
  let next_lval = fresh_lval tok_in in
  let hasnext_lval = fresh_lval tok_in in
  let hasnext_call =
    mk_s
      (Instr
         (mk_i
            (CallSpecial
               (Some hasnext_lval, (ForeachHasNext, tok_in), [ Unnamed e' ]))
            (related_tok tok_in)))
  in
  let next_call =
    mk_s
      (Instr
         (mk_i
            (CallSpecial (Some next_lval, (ForeachNext, tok_in), [ Unnamed e' ]))
            (related_tok tok_in)))
  in
  let assign_st =
    pattern_assign_statements env
      (mk_e (Fetch next_lval) (related_tok tok_in))
      ~eorig:(related_tok tok_in) pat
  in
  let cond = mk_e (Fetch hasnext_lval) (related_tok tok_in) in
  ss @
  [ hasnext_call;
    mk_s
        (Loop
           ( tok_in,
             cond,
             next_call :: assign_st @ inner_body @ cont_label_s
             @ [ hasnext_call ] ));
  ]
  @ break_label_s

(* Recursively build nested loops/guards from comprehension clauses,
 * wrapping [inner_body] at the innermost level.
 * Mirrors the MultiForEach nesting in [stmt]. *)
and comprehension_clauses env (clauses : G.for_or_if_comp list)
    (inner_body : stmts) : stmts =
  match clauses with
  | [] -> inner_body
  | G.CompFor (tok_for, pat, tok_in, collection_expr) :: rest ->
      let body = comprehension_clauses env rest inner_body in
      comprehension_loop env tok_for pat tok_in collection_expr body
  | G.CompIf (tok_if, guard_expr) :: rest ->
      let body = comprehension_clauses env rest inner_body in
      let ss, e' = expr env guard_expr in
      ss @ [ mk_s (If (tok_if, e', body, [])) ]

(* Compile a comprehension: create an accumulator, build nested
 * loops from the clause list, append each result to the accumulator. *)
and comprehension env (result_expr : G.expr)
    (clauses : G.for_or_if_comp list) : stmts * exp =
  let tok = G.fake "comprehension" in
  let tmp = fresh_lval ~str:"_comprehension_tmp" tok in
  let ss_res, e_eres = expr env ~void:false result_expr in
  let e_plus = mk_e (Operator ((G.Plus, Tok.unsafe_fake_tok "+="), [Unnamed e_eres])) NoOrig in
  let append_st = mk_s (Instr (mk_i (Assign (tmp, e_plus)) NoOrig)) in
  let inner_body = ss_res @ [ append_st ] in
  let loop_stmts = comprehension_clauses env clauses inner_body in
  (loop_stmts, mk_e (Fetch tmp) NoOrig)
  
and stmt_expr env ?g_expr st : stmts * exp =
  let todo_here () =
    match g_expr with
    | None -> todo (G.E (G.e (G.StmtExpr st)))
    | Some e_gen -> todo (G.E e_gen)
  in
  match st.G.s with
  | G.ExprStmt (eorig, tok) ->
      let ss, e = expr env eorig in
      if effective_implicit_return env eorig then
        let ret_stmt = mk_s (Return (tok, e)) in
        let ss_unit, unit_e = expr_opt env tok None in
        (ss @ [ret_stmt] @ ss_unit, unit_e)
      else (ss, e)
  | G.OtherStmt
      ( OS_Delete,
        ( [ (G.Tk tok as atok); G.E eorig ]
        | [ (G.Tk tok as atok); G.Tk _; G.Tk _; G.E eorig ] (* delete[] *) ) )
    ->
      let ss_e, e = expr env eorig in
      let special = (Delete, tok) in
      let instr = mk_s (Instr (mk_i (CallSpecial (None, special, [ Unnamed e ])) (Related atok))) in
      (ss_e @ [instr], mk_unit tok (Related atok))
  | G.If (tok, cond_e, st1, opt_st2) ->
      (* if cond then e1 else e2
       * -->
       * if cond {
       *   tmp = e1;
       * }
       * else {
       *   tmp = e2;
       * }
       * tmp
       *
       * TODO: Look at RIL (used by Diamondblack Ruby) for insiration,
       *       see https://www.cs.umd.edu/~mwh/papers/ril.pdf.
       *)
      let ss, e' = cond env cond_e in
      let pre_a1, e1 = stmt_expr env st1 in
      let pre_a2, e2 =
        match opt_st2 with
        | Some st2 -> stmt_expr env st2
        | None ->
            (* Coming from OCaml-land we would not expect this to happen... but
             * we got some Ruby examples from r2c's SR team where there is an `if`
             * expression without an `else`... anyways, if it happens we translate
             * what we can, and we fill-in the `else` with a "fixme" node. *)
            ([], fixme_exp ToDo (G.Tk tok) (Related (G.S st)))
      in
      let fresh = fresh_lval tok in
      let a1 = mk_s (Instr (mk_i (Assign (fresh, e1)) (related_tok tok))) in
      let a2 = mk_s (Instr (mk_i (Assign (fresh, e2)) (related_tok tok))) in
      let if_stmt = mk_s (If (tok, e', pre_a1 @ [ a1 ], pre_a2 @ [ a2 ])) in
      let eorig =
        match g_expr with
        | None -> related_exp (G.e (G.StmtExpr st))
        | Some e_gen -> SameAs e_gen
      in
      (ss @ [if_stmt], mk_e (Fetch fresh) eorig)
  | G.Block (_, block, _) -> (
      (* See 'AST_generic.stmt_to_expr' *)
      match List.rev block with
      | st :: rev_sts ->
          let list_of_lists = List.map (stmt env) (List.rev rev_sts) in
          let prefix_ss = List_.flatten list_of_lists in
          let last_ss, last_e = stmt_expr env st in
          (prefix_ss @ last_ss, last_e)
      | __else__ -> todo_here ())
  | G.Return (t, eorig, _) ->
      let ss_e, expression = expr_opt env t eorig in
      let ret_stmt = mk_s (Return (t, expression)) in
      let ss_unit, unit_e = expr_opt env t None in
      (ss_e @ [ret_stmt] @ ss_unit, unit_e)
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
    when def_expr_evaluates_to_value env.lang ->
      let ss_ty, () = type_opt env opt_ty in
      (* We may end up here due to Elixir_to_elixir's parsing. Other languages
       * such as Ruby, Julia, and C seem to result in Assignments, not DefStmts.
       *)
      let ss_e, e = expr env e in
      let ss_lv, lv = lval_of_ent env ent in
      let instr = mk_s (Instr (mk_i (Assign (lv, e)) (Related (G.S st)))) in
      (ss_ty @ ss_e @ ss_lv @ [instr], mk_e (Fetch lv) (related_exp (G.e (G.StmtExpr st))))
  | G.Switch (tok, switch_expr_opt, cases_and_bodies) ->
      (* Switch used as an expression (e.g. Elixir `case`).
       * Mirror the stmt-context Switch handler but lower each case body with
       * stmt_expr so the branch value is captured into a fresh variable. *)
      let ss, translate_cases, switch_expr_opt' =
        match switch_expr_opt with
        | Some switch_expr ->
            let ss, switch_expr' = cond env switch_expr in
            ( ss,
              switch_expr_and_cases_to_exp tok
                (H.cond_to_expr switch_expr)
                switch_expr',
              Some switch_expr' )
        | None -> ([], cases_to_exp tok, None)
      in
      let break_label, break_label_s, switch_env =
        switch_break_label env tok
      in
      let fresh = fresh_lval tok in
      let lower_body body =
        let pre_ss, e_val = stmt_expr switch_env body in
        let assign =
          mk_s (Instr (mk_i (Assign (fresh, e_val)) (related_tok tok)))
        in
        pre_ss @ [ assign ]
      in
      let jumps, bodies =
        cases_and_bodies_to_stmts switch_env switch_expr_opt' tok break_label
          translate_cases lower_body cases_and_bodies
      in
      let eorig =
        match g_expr with
        | None -> related_exp (G.e (G.StmtExpr st))
        | Some e_gen -> SameAs e_gen
      in
      (ss @ jumps @ bodies @ break_label_s, mk_e (Fetch fresh) eorig)
  | __else__ ->
      (* In any case, let's make sure the statement is in the IL translation
       * so that e.g. taint can do its job. *)
      let new_stmts = stmt env st in
      (* Do not call todo_here() here: it raises and would lose new_stmts.
       * Instead, produce the fixme expression directly. *)
      let gany = match g_expr with
        | None -> G.E (G.e (G.StmtExpr st))
        | Some e_gen -> G.E e_gen
      in
      let eorig_for_fixme = related_exp (match g_expr with
        | None -> G.e (G.StmtExpr st)
        | Some e_gen -> e_gen)
      in
      (new_stmts, fixme_exp ToDo gany eorig_for_fixme)

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)
and lval_of_ent env ent : stmts * lval =
  match ent.G.name with
  | G.EN (G.Id (id, idinfo)) -> ([], lval_of_id_info id idinfo)
  | G.EN name -> lval env (G.N name |> G.e)
  | G.EDynamic eorig -> lval env eorig
  | G.EPattern (PatId (id, id_info)) -> lval env (G.N (Id (id, id_info)) |> G.e)
  (* Why not more here, if we are to support more patterns? *)
  | G.EPattern _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = AST_generic_helpers.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> ([], fresh_lval x))
  | G.OtherEntity _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = AST_generic_helpers.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> ([], fresh_lval x))

(* alt: could use H.cond_to_expr and reuse expr *)
and cond env cond_e : stmts * exp =
  match cond_e with
  | G.Cond e -> expr env e
  | G.OtherCond
      ( todok,
        [
          (Def (ent, VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
           as def);
        ] ) ->
      let ss_ty, _ = type_opt env opt_ty in
      (* e.g. C/C++: `if (const char *tainted_or_null = source("PATH"))` *)
      let ss_e, e' = expr env e in
      let ss_lv, lv = lval_of_ent env ent in
      let instr = mk_s (Instr (mk_i (Assign (lv, e')) (Related def))) in
      (ss_ty @ ss_e @ ss_lv @ [instr], mk_e (Fetch lv) (Related (G.TodoK todok)))
  | G.OtherCond (categ, xs) ->
      let e = G.OtherExpr (categ, xs) |> G.e in
      log_fixme ToDo (G.E e);
      expr env e

and for_var_or_expr_list env xs : stmts =
  let list_of_lists =
    List.map
      (fun x ->
        match x with
        | G.ForInitExpr e ->
            let ss, _eIGNORE = expr env e in
            ss
        | G.ForInitVar (ent, vardef) -> (
            (* copy paste of VarDef case in stmt *)
            match vardef with
            | { G.vinit = Some e; vtype = opt_ty; vtok = _ } ->
                let ss1, e' = expr env e in
                let ss2, () = type_opt env opt_ty in
                let ss_lv, lv = lval_of_ent env ent in
                ss1 @ ss2 @ ss_lv
                  @ [
                      mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.En ent))));
                    ]
            | _ -> []))
      xs
  in
  List.concat list_of_lists (*TODO this is not tail recursive!!!!*)

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)
and parameters params : param list =
  params |> Tok.unbracket
  |> List_.mapi (fun idx gparam ->
       match gparam with
       | G.Param { pname = Some i; pinfo; pdefault; _ } ->
           let pname = var_of_id_info i pinfo in
           (* Clojure/Elixir/OCaml encode multi-clause functions with a
              single synthetic !!_implicit_param! that already receives the
              CList of actual arguments (the call site wraps them). Keep it
              as a plain positional Param so the signature layer binds the
              CList directly instead of re-wrapping it. *)
           Param { pname; pdefault }
       | G.ParamRest (_, { pname = Some i; pinfo; pdefault; _ }) ->
           ParamRest { pname = var_of_id_info i pinfo; pdefault }
       | G.ParamPattern (pat, { pname = Some i; pinfo; pdefault; _ }) ->
           (* The synthetic [!!_implicit_param!] binder from
            * [implicit_param_classic] becomes the IL name_param. Rename
            * it to [!!_implicit_param!_idx] so multiple destructuring
            * params in the same function get distinguishable Arg entries
            * in the taint signature layer; the [is_implicit_param] prefix
            * check still recognises the indexed form. The inner pattern
            * is lowered separately by [function_definition] as a
            * [pattern_assign_statements] prelude over this name. *)
           let _, tk = i in
           let i = G.implicit_param_id_indexed idx tk in
           let pname = var_of_id_info i pinfo in
           ParamPattern ({ pname; pdefault }, pat)
       | G.ParamReceiver _param ->
           (* TODO: Treat receiver as this parameter *)
           ParamFixme (* TODO *)
       (* Ruby/PHP block parameter: &callback -> OtherParam("Ref", [Pa(Param(...))]) *)
       | G.OtherParam (("Ref", _), [ G.Pa (G.Param { pname = Some i; pinfo; pdefault; _ }) ])
         ->
           Param { pname = var_of_id_info i pinfo; pdefault }
       | G.Param { pname = None; _ }
       | G.ParamRest (_, _)
       | G.ParamHashSplat (_, _)
       | G.ParamEllipsis _
       | G.OtherParam (_, _) ->
           ParamFixme (* TODO *)
       | G.ParamPattern (_, { pname = None; _ }) ->
           (* Every [G.ParamPattern] is built via [implicit_param_classic]
            * which always sets [pname = Some ...]. This case cannot be
            * reached from any current parser. *)
           ParamFixme)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

and type_ env (ty : G.type_) : stmts * G.type_ =
  (* Expressions inside types also need to be analyzed.
   *
   * E.g., in C we need to be able to do const prop here:
   *
   *     int x = 3;
   *     int arr[x]; // should match 'int arr[3]'
   *)
  let ss_exps, exps =
    match ty.t with
    | G.TyArray ((_, Some e, _), _)
    | G.TyExpr e ->
        let ss, expression = expr env e in
        (ss, [ expression ])
    | __TODO__ -> ([], [])
  in
  let tok = G.fake "type" in
  let aux_ss_list =
    List.map
      (fun e ->
        let aux_ss, x, y = aux_var ~force:true ~str:"_type" env tok e in
        (aux_ss, (x, y)))
      exps
  in
  let aux_ss = List.concat_map fst aux_ss_list in
  (ss_exps @ aux_ss, ty)

and type_opt env opt_ty : stmts * unit =
  match opt_ty with
  | None -> ([], ())
  | Some ty ->
      let ss, _ = type_ env ty in
      (ss, ())

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

and no_switch_fallthrough : Lang.t -> bool = function
  | Go
  | Ruby
  | Rust
  | Clojure
  | Elixir ->
      true
  | _ -> false

and break_continue_labels env tok : stmts * stmts * env =
  let cont_label = fresh_label ~label:"__loop_continue" tok in
  let break_label = fresh_label ~label:"__loop_break" tok in
  let st_env =
    {
      env with
      break_labels = break_label :: env.break_labels;
      cont_label = Some cont_label;
    }
  in
  let cont_label_s = [ mk_s (Label cont_label) ] in
  let break_label_s = [ mk_s (Label break_label) ] in
  (cont_label_s, break_label_s, st_env)

and switch_break_label env tok : label * stmts * env =
  let break_label = fresh_label ~label:"__switch_break" tok in
  let switch_env =
    { env with break_labels = break_label :: env.break_labels }
  in
  (break_label, [ mk_s (Label break_label) ], switch_env)

and recursion_point_label env tok : label * stmts * env =
  let rec_point_label = fresh_label ~label:"__rec_point" tok in
  let rec_point_env =
    { env with rec_point_label = Some rec_point_label }
  in
  (rec_point_label, [ mk_s (Label rec_point_label) ], rec_point_env)

and implicit_return env eorig tok : stmts =
  (* We always expect a value from an expression that is implicitly
   * returned, so void is set to false here.
   *)
  let ss, e = expr env ~void:false eorig in
  let ret = mk_s (Return (tok, e)) in
  ss @ [ ret ]

and expr_stmt env (eorig : G.expr) tok : IL.stmt list =
  (* optimize? pass context to expr when no need for return value? *)
  let ss, e = expr env ~void:true eorig in

  (* Some expressions may return unit, and if we call aux_var below, not only
   * is it extraneous, but it also interferes with implicit return analysis.
   *
   * For example,
   *   call f()
   *   tmp = unit
   * interferes with implicit return analysis, because the analysis walks
   * backwards from the exit node to mark the first instr node it sees on each
   * path.
   *
   * If we have
   *   call f()
   *   tmp = unit
   * then `unit` will be marked as a returning expression when we actually
   * want to mark `f()`, so we must avoid creating `tmp = unit` following
   * a function call that doesn't expect results.
   *)
  let aux_ss =
    match e.e with
    | Literal (G.Unit _) -> []
    | _else_ ->
        let aux_ss, _, _ = aux_var env tok e in
        aux_ss
  in

  match ss @ aux_ss with
  | [] ->
      (* This case may happen when we have a function like
       *
       *   function some_function(some_var) {
       *     some_var
       *   }
       *
       * the `some_var` will not show up in the CFG. Neither expr
       * nor aux_var will cause nodes to be created.
       *
       * This is typically OK, because it doesn't make sense to write
       * `some_var` for side-effects.
       *
       * The issue is that for some languages
       * when `some_var` is the last evaluated expression in the function,
       * `some_var` is also implicitly returned from the function. In this case
       * `some_var` actually means `return some_var`, so there should be a return
       * node in the CFG.
       *
       * We'd like to always create an IL node here as a fake "no-op" assignment
       *   tmp = some_var
       * because we'd like to mark some_var's eorig as an implicit return node
       * so later we can convert
       *   some_var
       * to
       *   return some_var
       * when some_var is marked as an implicit return node.
       *
       * If some_var isn't a returning expression, we have created an unneeded node
       * but it doesn't affect correctness.
       *)
      let var = fresh_var tok in
      let lval = lval_of_base (Var var) in
      let fake_i = mk_i (Assign (lval, e)) NoOrig in
      [ mk_s (Instr fake_i) ]
  | ss'' -> ss''

and class_construction env obj origin_exp ty cons_id_info args :
    lval * stmt list =
  (* We encode `obj = new T(args)` as `obj = new obj.T(args)` so that taint
     analysis knows that the reciever when calling `T` is the variable
     `obj`. It's kinda hacky but works for now. *)
  let lval = lval_of_base (Var obj) in
  let ss1, args' = arguments env (Tok.unbracket args) in
  let opt_cons =
    let* cons = mk_class_constructor_name ty cons_id_info in
    let cons' = var_of_name cons in
    let cons_exp =
      mk_e
        (Fetch { lval with rev_offset = [ { o = Dot cons'; oorig = NoOrig } ] })
        (SameAs (G.N cons |> G.e))
      (* THINK: ^^^^^ We need to construct a `SameAs` eorig here because Pro
       * looks at the eorig, but maybe it shouldn't? *)
    in
    Some cons_exp
  in
  let ss2, ty = type_ env ty in
  ( lval,
    ss1 @ ss2
    @ [
        mk_s
          (Instr (mk_i (New (lval, ty, opt_cons, args')) (SameAs origin_exp)));
      ] )

and stmt_aux env st : stmts =
  match st.G.s with
  | G.ExprStmt (eorig, tok) -> (
      match eorig with
      | _ when effective_implicit_return env eorig ->
          implicit_return env eorig tok
      (* Python's yield statement functions similarly to a return
         statement but with the added capability of saving the
         function's state. While this analogy isn't entirely precise,
         we currently treat it as a return statement for simplicity's
         sake. *)
      | { e = Yield (_, Some e, _); _ } when env.lang =*= Lang.Python ->
          implicit_return env e tok
      | _ -> expr_stmt env eorig tok)
  | G.DefStmt
      ( { name = EN obj; _ },
        G.VarDef
          {
            G.vinit =
              Some ({ e = G.New (_tok, ty, cons_id_info, args); _ } as new_exp);
            _;
          } ) ->
      (* x = new T(args) *)
      (* HACK(new): Because of field-sensitivity hacks, we need to know to which
       * variable are we assigning the `new` object, so we intercept the assignment. *)
      let obj' = var_of_name obj in
      let _, new_stmts =
        class_construction env obj' new_exp ty cons_id_info args
      in
      new_stmts
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ }) ->
      let ss1, e' = expr env e in
      let ss_lv, lv = lval_of_ent env ent in
      let ss2, () = type_opt env opt_ty in
      ss1 @ ss_lv @ ss2 @ [ mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.S st)))) ]
  | G.DefStmt (ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ })
    when env.lang =*= Lang.Cpp ->
      (* Handle C++ constructor calls like: User user(taintedInput) *)
      (match ty.t with
      | G.TyFun (params, return_ty) ->
          (match ent.name, return_ty.t with
          | G.EN (G.Id (var_name, var_info)), G.TyN (G.Id (_, class_info)) ->
              (* This is a C++ constructor: ClassName varName(args) *)
              let obj' = var_of_name (G.Id (var_name, var_info)) in
              (* Convert params to argument expressions *)
              let args = List.map (fun param ->
                match param with
                | G.Param { ptype; _ } ->
                    (match ptype with
                    | Some { t = G.TyN (G.Id (arg_name, arg_info)); _ } ->
                        G.Arg (G.N (G.Id (arg_name, arg_info)) |> G.e)
                    | _ ->
                        (* Fallback for complex parameter types *)
                        G.Arg (G.N (G.Id (("_unknown", Tok.unsafe_fake_tok ""), G.empty_id_info ())) |> G.e)
                    )
                | _ ->
                    (* Fallback for non-Param parameter types *)
                    G.Arg (G.N (G.Id (("_unknown", Tok.unsafe_fake_tok ""), G.empty_id_info ())) |> G.e)
              ) params in
              (* Create fake New expression for class_construction *)
              let fake_new_exp = G.New (Tok.unsafe_fake_tok "", return_ty, class_info, (Tok.unsafe_fake_tok "", args, Tok.unsafe_fake_tok "")) |> G.e in
              let _, new_stmts =
                class_construction env obj' fake_new_exp return_ty class_info (Tok.unsafe_fake_tok "", args, Tok.unsafe_fake_tok "")
              in
              new_stmts
          | _ ->
              (* Not a constructor pattern, fall back to type analysis *)
              let ss, _ = type_ env ty in
              ss
          )
      | _ ->
          (* Not TyFun, fall back to type analysis *)
          let ss, _ = type_ env ty in
          ss
      )
  | G.DefStmt (_ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ }) ->
      (* We want to analyze any expressions in 'ty'. *)
      let ss, _ = type_ env ty in
      ss
  | G.DefStmt (ent, G.FuncDef fdef) when env.inside_function ->
      (* Translate nested function declarations as lambda assignments so that
       * the CFG builder extracts them into lambdas_cfgs, enabling the taint
       * engine to propagate closure-captured variables through them. *)
      let ss_lv, lv = lval_of_ent env ent in
      let il_fdef =
        (* See NOTE about resetting control-flow labels for lambdas. *)
        function_definition
          { env with cont_label = None;
                     break_labels = [];
                     rec_point_label = None;
                     rec_point_lvals = None }
          fdef
      in
      ss_lv @ [ mk_s (Instr (mk_i (AssignAnon (lv, Lambda il_fdef)) (Related (G.S st)))) ]
  | G.DefStmt def -> [ mk_s (MiscStmt (DefStmt def)) ]
  | G.DirectiveStmt dir -> [ mk_s (MiscStmt (DirectiveStmt dir)) ]
  | G.Block xs ->
      let xs = xs |> Tok.unbracket in
      List.concat_map (stmt env) xs
  (* Rust: if let Some(x) = some_x { ... } etc. *)
  (* TODO: Handle LetChain too, see Parse_rust_tree_sitter. *)
  | G.If (tok, G.OtherCond (("LetCond", _tk), [G.P pat; G.E e]), st1, st2)
    when env.lang =*= Lang.Rust ->
    (* Convert to switch(e) { pat -> if_branch, _ -> else_branch }. *)
    let cond_opt = Some (G.Cond e) in
    let if_case_and_body =
      G.CasesAndBody ([ G.Case (G.fake "case", pat) ], st1)
    in
    let cases_and_bodies =
      match st2 with
      | Some st2 ->
          [
            if_case_and_body;
            G.CasesAndBody ([ G.Case (G.fake "case", G.PatWildcard (G.fake "_")) ], st2);
          ]
      | None -> [ if_case_and_body ]
    in
    let switch =
      G.Switch (tok, cond_opt, cases_and_bodies) |> G.s
    in
    stmt env switch
  | G.If (tok, cond_e, st1, st2) ->
      let ss, e' = cond env cond_e in
      let st1 = stmt env st1 in
      let st2 = List.concat_map (stmt env) (Option.to_list st2) in
      ss @ [ mk_s (If (tok, e', st1, st2)) ]
  | G.Switch (tok, switch_expr_opt, cases_and_bodies) ->
      let ss, translate_cases, switch_expr_opt' =
        match switch_expr_opt with
        | Some switch_expr ->
            let ss, switch_expr' = cond env switch_expr in
            ( ss,
              switch_expr_and_cases_to_exp tok
                (H.cond_to_expr switch_expr)
                switch_expr',
              Some switch_expr' )
        | None -> ([], cases_to_exp tok, None)
      in
      let break_label, break_label_s, switch_env =
        switch_break_label env tok
      in

      let jumps, bodies =
        cases_and_bodies_to_stmts switch_env switch_expr_opt' tok break_label translate_cases
          (stmt switch_env) cases_and_bodies
      in
      ss @ jumps @ bodies @ break_label_s
  | G.While (tok, e, st) ->
      let cont_label_s, break_label_s, st_env =
        break_continue_labels env tok
      in
      let ss, e' = cond env e in
      let st = stmt st_env st in
      ss @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ] @ break_label_s
  | G.DoWhile (tok, st, e) ->
      let cont_label_s, break_label_s, st_env =
        break_continue_labels env tok
      in
      let st = stmt st_env st in
      let ss, e' = expr env e in
      st @ ss
        @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ]
        @ break_label_s
  | G.For (tok, G.ForEach (pat, tok2, e), st) ->
      for_each env tok (pat, tok2, e) st
  | G.For (_, G.MultiForEach [], st) -> stmt env st
  | G.For (_, G.MultiForEach (FEllipsis _ :: _), _) ->
      sgrep_construct (G.S st)
  | G.For (tok, G.MultiForEach (FECond (fr, tok2, e) :: for_eachs), st) ->
      let loop = G.For (tok, G.MultiForEach for_eachs, st) |> G.s in
      let st = G.If (tok2, Cond e, loop, None) |> G.s in
      for_each env tok fr st
  | G.For (tok, G.MultiForEach (FE fr :: for_eachs), st) ->
      for_each env tok fr (G.For (tok, G.MultiForEach for_eachs, st) |> G.s)
  | G.For (tok, G.ForClassic (xs, eopt1, eopt2), st) ->
      let cont_label_s, break_label_s, st_env =
        break_continue_labels env tok
      in
      let ss1 = for_var_or_expr_list env xs in
      let st = stmt st_env st in
      let ss2, cond_e =
        match eopt1 with
        | None ->
            let vtrue = G.Bool (true, tok) in
            ([], mk_e (Literal vtrue) (related_tok tok))
        | Some e -> expr env e
      in
      let next =
        match eopt2 with
        | None -> []
        | Some e ->
            let ss, _eIGNORE = expr env e in
            ss
      in
      ss1 @ ss2
        @ [ mk_s (Loop (tok, cond_e, st @ cont_label_s @ next @ ss2)) ]
        @ break_label_s
  | G.For (_, G.ForEllipsis _, _) -> sgrep_construct (G.S st)
  (* TODO: repeat env work of controlflow_build.ml *)
  | G.Continue (tok, lbl_ident, _) -> (
      match lbl_ident with
      | G.LNone -> (
          match env.cont_label with
          | None -> impossible (G.Tk tok)
          | Some lbl -> [ mk_s (Goto (tok, lbl)) ])
      | G.LId lbl -> [ mk_s (Goto (tok, label_of_label lbl)) ]
      | G.LInt _
      | G.LDynamic _ ->
          todo (G.S st))
  | G.Break (tok, lbl_ident, _) -> (
      match lbl_ident with
      | G.LNone -> (
          match env.break_labels with
          | [] -> impossible (G.Tk tok)
          | lbl :: _ -> [ mk_s (Goto (tok, lbl)) ])
      | G.LId lbl -> [ mk_s (Goto (tok, label_of_label lbl)) ]
      | G.LInt (i, _) -> (
          match List.nth_opt env.break_labels i with
          | None -> impossible (G.Tk tok)
          | Some lbl -> [ mk_s (Goto (tok, lbl)) ])
      | G.LDynamic _ -> impossible (G.Tk tok))
  | G.Label (lbl, st) ->
      let lbl = label_of_label lbl in
      let st = stmt env st in
      [ mk_s (Label lbl) ] @ st
  | G.Goto (tok, lbl, _sc) ->
      let lbl = lookup_label lbl in
      [ mk_s (Goto (tok, lbl)) ]
  | G.Return (tok, eopt, _) ->
      let ss, e = expr_opt env tok eopt in
      ss @ [ mk_s (Return (tok, e)) ]
  | G.Assert (tok, args, _) ->
      let ss, args = arguments env (Tok.unbracket args) in
      let special = (Assert, tok) in
      (* less: wrong e? would not be able to match on Assert, or
       * need add sorig:
       *)
      ss
      @ [
          mk_s
            (Instr
               (mk_i (CallSpecial (None, special, args)) (Related (G.S st))));
        ]
  | G.Throw (tok, e, _) ->
      let ss, e = expr env e in
      ss @ [ mk_s (Throw (tok, e)) ]
  | G.OtherStmt (G.OS_Go, [G.E call]) ->
      expr_stmt env call G.sc
  | G.OtherStmt (G.OS_ThrowNothing, [ G.Tk tok ]) ->
      (* Python's `raise` without arguments *)
      let eorig = related_tok tok in
      let todo_exp = fixme_exp ToDo (G.Tk tok) eorig in
      [ mk_s (Throw (tok, todo_exp)) ]
  | G.OtherStmt
      (G.OS_ThrowFrom, [ G.E from; G.S ({ s = G.Throw _; _ } as throw_stmt) ])
    ->
      (* Python's `raise E1 from E2` *)
      let todo_stmt = fixme_stmt ToDo (G.E from) in
      let rest = stmt_aux env throw_stmt in

      todo_stmt @ rest
  | G.Try (_tok, try_st, catches, opt_else, opt_finally) ->
      try_catch_else_finally env ~try_st ~catches ~opt_else ~opt_finally
  | G.WithUsingResource (_, stmt1, stmt2) ->
      let stmt1 = List.concat_map (stmt env) stmt1 in
      let stmt2 = stmt env stmt2 in
      stmt1 @ stmt2
  | G.DisjStmt _ -> sgrep_construct (G.S st)
  | G.OtherStmtWithStmt (G.OSWS_With, [ G.E manager_as_pat ], body) ->
      let opt_pat, manager =
        (* Extract <manager> and <pat> from `with <manager> as <pat>`;
         * <manager> is an expression that evaluates to a context manager,
         * <pat> is optional. *)
        match manager_as_pat.G.e with
        | G.LetPattern (pat, manager) -> (Some pat, manager)
        | _ -> (None, manager_as_pat)
      in
      python_with_stmt env manager opt_pat body
  (* Java: synchronized (E) S *)
  | G.OtherStmtWithStmt (G.OSWS_Block _, [ G.E objorig ], stmt1) ->
      (* TODO: Restrict this to a syncrhonized block ? *)
      let ss, _TODO_obj = expr env objorig in
      let new_stmts = stmt env stmt1 in
      ss @ new_stmts
  (* Rust: unsafe block *)
  | G.OtherStmtWithStmt (G.OSWS_Block ("Unsafe", tok), [], stmt1) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("unsafe_block", tok)) in
      let new_stmts = stmt env stmt1 in
      todo_stmt @ new_stmts
  | G.OtherStmt (OS_Async, [ G.S stmt1 ]) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("async", G.fake "async")) in
      let new_stmts = stmt env stmt1 in
      todo_stmt @ new_stmts
  | G.OtherStmt _
  | G.OtherStmtWithStmt _ ->
      todo (G.S st)
  | G.RawStmt _ -> todo (G.S st)

and for_each env tok (pat, tok2, e) st : stmts =
  let cont_label_s, break_label_s, st_env = break_continue_labels env tok in
  let ss, e' = expr env e in
  let st = stmt st_env st in
  let next_lval = fresh_lval tok2 in
  let hasnext_lval = fresh_lval tok2 in
  let hasnext_call =
    mk_s
      (Instr
         (mk_i
            (CallSpecial
               (Some hasnext_lval, (ForeachHasNext, tok2), [ Unnamed e' ]))
            (related_tok tok2)))
  in
  let next_call =
    mk_s
      (Instr
         (mk_i
            (CallSpecial (Some next_lval, (ForeachNext, tok2), [ Unnamed e' ]))
            (related_tok tok2)))
  in
  (* same semantic? or need to take Ref? or pass lval
   * directly in next_call instead of using intermediate next_lval?
   *)
  let assign_st =
    pattern_assign_statements env
      (mk_e (Fetch next_lval) (related_tok tok2))
      ~eorig:(related_tok tok2) pat
  in
  let cond = mk_e (Fetch hasnext_lval) (related_tok tok2) in

  (ss @ [ hasnext_call ])
  @ [
      mk_s
        (Loop
           ( tok,
             cond,
             [ next_call ] @ assign_st @ st @ cont_label_s
             @ [ (* ss @ ?*) hasnext_call ] ));
    ]
  @ break_label_s

(* TODO: Maybe this and the following function could be merged *)
and switch_expr_and_cases_to_exp tok switch_expr_orig switch_expr env cases : stmts * exp =
  (* If there is a scrutinee, the cases are expressions we need to check for equality with the scrutinee  *)
  let ss, es =
    List.fold_left
      (fun (ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            ( ss,
              {
                e =
                  Operator
                    ( (G.Eq, tok),
                      [
                        Unnamed { e = Literal l; eorig = related_tok tok };
                        Unnamed switch_expr;
                      ] );
                eorig = related_tok tok;
              }
              :: es )
        | G.Case (tok, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (tok, c) ->
            (* TODO: PatWhen should use something along these lines... *)
            let c_ss, c' = expr env c in
            ( ss @ c_ss,
              {
                e = Operator ((G.Eq, tok), [ Unnamed c'; Unnamed switch_expr ]);
                eorig = related_tok tok;
              }
              :: es )
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible (G.Tk tok)
        | G.Case (tok, pat) -> (
            (* For list-shaped patterns we encode the arity check as a
             * condition on the scrutinee's length. Clojure multi-arity
             * functions lower as a Switch over a single list-valued implicit
             * parameter; without a real condition here the effects of every
             * leg would merge at the caller's signature instantiation. *)
            match list_pat_arity pat with
            | None -> (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es)
            | Some (op, n) ->
                let length_exp =
                  {
                    e = Operator ((G.Length, tok), [ Unnamed switch_expr ]);
                    eorig = related_tok tok;
                  }
                in
                let n_exp =
                  {
                    e = Literal (G.Int (Parsed_int.of_int n));
                    eorig = related_tok tok;
                  }
                in
                ( ss,
                  {
                    e =
                      Operator ((op, tok), [ Unnamed length_exp; Unnamed n_exp ]);
                    eorig = related_tok tok;
                  }
                  :: es ))
        | G.OtherCase ((_todo_categ, tok), _any) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es))
      ([], []) cases
  in
  (* A [CasesAndBody] with a single case does not need the [Or] wrapper —
   * unwrapping it here keeps downstream consumers (e.g. the arity-guard
   * recogniser in the taint transfer) from having to peel it off. *)
  ( ss,
    match es with
    | [ single ] -> single
    | _ ->
        {
          e = Operator ((Or, tok), mk_unnamed_args es);
          eorig = SameAs switch_expr_orig;
        } )

and cases_to_exp tok env cases : stmts * exp =
  (* If we have no scrutinee, the cases are boolean expressions, so we Or them together *)
  let ss, es =
    List.fold_left
      (fun (ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            (ss, { e = Literal l; eorig = related_tok tok } :: es)
        | G.Case (_, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (_, c) ->
            let c_ss, c' = expr env c in
            (ss @ c_ss, c' :: es)
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible (G.Tk tok)
        (* TODO: Other patterns? Maybe not worth it. *)
        | G.Case (tok, _) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es)
        | G.OtherCase ((_, tok), _) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es))
      ([], []) cases
  in
  ( ss,
    match es with
    | [ single ] -> single
    | _ ->
        { e = Operator ((Or, tok), mk_unnamed_args es); eorig = related_tok tok }
  )

and cases_and_bodies_to_stmts env switch_expr_opt tok break_label translate_cases
    lower_body : G.case_and_body list -> stmts * stmts = function
  | [] -> ([ mk_s (Goto (tok, break_label)) ], [])
  | G.CaseEllipsis tok :: _ -> sgrep_construct (G.Tk tok)
  | [ G.CasesAndBody ([ G.Default dtok ], body) ] ->
      let label = fresh_label ~label:"__switch_default" tok in

      let new_stmts = lower_body body in
      ([ mk_s (Goto (dtok, label)) ], mk_s (Label label) :: new_stmts)
  | G.CasesAndBody (cases, body) :: xs ->
      let jumps, bodies =
        cases_and_bodies_to_stmts env switch_expr_opt tok break_label translate_cases
          lower_body xs (* TODO this is not tail recursive *)
      in
      let label = fresh_label ~label:"__switch_case" tok in
      let case_ss, case = translate_cases env cases in
      let jump =
        mk_s (IL.If (tok, case, [ mk_s (Goto (tok, label)) ], jumps))
      in

      (* Here we add bindings for the switch pattern, for the common case:
       * cases is one case for the branch. This makes Switch branches behave
       * like LetPattern.
       * Note that we only bind patterns when the switch condition is not None,
       * even though it still does not always make sense...
       * On the other hand, if the pattern has for example PatId, what is the
       * intention if not to create a variable?
       *)
      let pat_stmts =
        match switch_expr_opt, cases with
        | Some cond, [ G.Case (_tok, pat) ] ->
            (* TODO: Need break_label here, if we are to handle PatWhen.
             * See comments below. *)
            pattern_assign_statements env ~eorig:(Related (G.P pat)) cond pat
        | _ -> []
      in

      let new_stmts = lower_body body in

      let body = [ mk_s (Label label) ] @ pat_stmts @ new_stmts in
      (* Maybe lang has no_fallthrough in general but here we have PatWhen
       * with guard. Not sure any of that makes a true difference though! *)
      let is_guarded_pat =
        match cases with
        | [ G.Case (_, G.PatWhen _) ] -> true
        | _ -> false
      in
      let break_if_no_fallthrough =
        if no_switch_fallthrough env.lang && not is_guarded_pat then
          (* TODO: Now, this instruction must be emitted conditionally
           * in the translation of PatWhen, in the true branch of the If. *)
          [ mk_s (Goto (tok, break_label)) ]
        else []
      in
      (case_ss @ [ jump ], body @ break_if_no_fallthrough @ bodies)

and stmt env st : stmt list =
  try stmt_aux env st with
  | Fixme (kind, any_generic) -> fixme_stmt kind any_generic

(* We keep it really simple, very far from what would be the proper translation
 * (see https://www.python.org/dev/peps/pep-0343/):
 *
 *     with MANAGER as PAT:
 *         BODY
 *
 * ~>
 *
 *     PAT = MANAGER
 *     BODY
 *
 * Previously we used this more accurate (yet not 100% accurate) translation:
 *
 *     mgr = MANAGER
 *     value = type(mgr).__enter__(mgr)
 *     try:
 *         PAT = value
 *         BODY
 *     finally:
 *         type(mgr).__exit__(mgr)
 *
 * but to be honest we had no use for all that extra complexity, and this
 * translated prevented symbolic propagation to match e.g.
 * `Session(...).execute(...)` against:
 *
 *   with Session(engine) as s:
 *       s.execute("<query>")
 *)
and python_with_stmt env manager opt_pat body : stmts =
  (* mgr = MANAGER *)
  let mgr = fresh_lval G.sc in
  let ss_mk_mgr, manager' = expr env manager in
  let ss_def_mgr = ss_mk_mgr @ [ mk_s (Instr (mk_i (Assign (mgr, manager')) NoOrig)) ] in
  (* PAT = mgr *)
  let ss_def_pat =
    match opt_pat with
    | None -> []
    | Some pat ->
        pattern_assign_statements env (mk_e (Fetch mgr) NoOrig) ~eorig:NoOrig
          pat
  in
  let new_stmts = stmt env body in
  ss_def_mgr @ ss_def_pat @ new_stmts

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

and function_body env fbody : stmts =
  let body_stmt = H.funcbody_to_stmt fbody in
  stmt env body_stmt

and function_definition env fdef : function_definition =
  let fparams = parameters fdef.G.fparams in
  let env, rec_point_label_stmts = match env.lang, fparams with
    (* NOTE: Clojure functions are translated to have one formal parameter,
     * which is then destructured in a Switch (this is how multi-arity works). *)
    | Lang.Clojure, [ Param { pname; _ } ] ->
      let lval = IL_helpers.lval_of_var pname in
      let _rec_point_label, rec_point_label_stmts, rec_point_env =
        recursion_point_label env (G.fake "fun_rec_point")
      in
      {rec_point_env with rec_point_lvals = Some (Fn_rec_point lval)},
      rec_point_label_stmts
    | _ -> env, []
  in
  let env = { env with inside_function = true } in
  (* ParamPattern destructuring is expressed declaratively: the
   * parameter_classic's implicit binder is the signature's Arg slot,
   * and each leaf of the pattern is pre-seeded at taint-env setup
   * time with shape [Arg (taint_arg, offset_path)]. See
   * [Taint_signature_extractor.mk_param_assumptions]. No IL
   * instructions are needed here — the shape system handles the
   * projection from caller's argument to leaf at call-site
   * signature instantiation. This avoids the destructive overwrite
   * that an IL [Assign] would cause on [mk_fun_input_env]'s pre-
   * seeded source taints, and avoids introducing origs that would
   * falsely match enclosing-sink ranges via
   * [Match_taint_spec.any_is_in_matches_OSS]. *)
  let fbody = function_body env fdef.G.fbody in
  let fbody = rec_point_label_stmts @ fbody in
  { fkind = fdef.fkind; fparams; frettype = fdef.G.frettype; fbody }

(****************************************************************************)
(* Entry points *)
(****************************************************************************)

let function_definition lang fdef : function_definition =
  let env = empty_env lang in
  function_definition env fdef

let stmt lang st : stmts =
  let env = empty_env lang in
  stmt env st

let expr lang e : exp =
  let env = empty_env lang in
  let _stmts, e = expr env e in
  e

let lval lang e : lval =
  let env = empty_env lang in
  let _stmts, lv = lval env e in
  lv
