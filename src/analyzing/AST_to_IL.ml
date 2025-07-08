(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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

[@@@warning "-40-42"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST generic to IL translation.
 *
 * todo:
 *  - a lot ...
 *)
let locate ?tok s =
  let opt_loc =
    try Option.map Tok.stringpos_of_tok tok with
    | Tok.NoTokenLocation _ -> None
  in
  match opt_loc with
  | Some loc -> spf "%s: %s" loc s
  | None -> s

let log_debug ?tok msg = Log.debug (fun m -> m "%s" (locate ?tok msg))
let log_warning ?tok msg = Log.warn (fun m -> m "%s" (locate ?tok msg))
let log_error ?tok msg = Log.err (fun m -> m "%s" (locate ?tok msg))

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
module IdentSet = Set.Make (String)

type ctx = { entity_names : IdentSet.t }
type stmts = stmt list

type env = {
  lang : Lang.t;
  (* stmts hidden inside expressions that we want to move out of 'exp',
   * usually simple Instr, but can be also If when handling Conditional expr.
   *)
  (* When entering a loop, we create two labels, one to jump to if a Continue stmt is found
     and another to jump to if a Break stmt is found. Since PHP supports breaking an arbitrary
     number of loops up, we keep a stack of break labels instead of just one
  *)
  break_labels : label list;
  cont_label : label option;
  ctx : ctx;
}

let empty_ctx = { entity_names = IdentSet.empty }

let empty_env (lang : Lang.t) : env =
  { break_labels = []; cont_label = None; ctx = empty_ctx; lang }

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception Fixme of stmts * fixme_kind * G.any

let sgrep_construct stmts any_generic =
  raise (Fixme (stmts, Sgrep_construct, any_generic))

let todo stmts any_generic = raise (Fixme (stmts, ToDo, any_generic))

let impossible stmts any_generic =
  raise (Fixme (stmts, Impossible, any_generic))

let log_fixme kind gany =
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

let fixme_exp ?partial kind gany eorig =
  log_fixme kind (any_of_orig eorig);
  { e = FixmeExp (kind, gany, partial); eorig }

let fixme_instr kind gany eorig =
  log_fixme kind (any_of_orig eorig);
  { i = FixmeInstr (kind, gany); iorig = eorig }

let fixme_stmt kind gany =
  log_fixme kind gany;
  [ { s = FixmeStmt (kind, gany) } ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fresh_var ?(str = "_tmp") (_env, _stmts) tok =
  let tok =
    (* We don't want "fake" auxiliary variables to have non-fake tokens, otherwise
       we confuse ourselves! E.g. during taint-tracking we don't want to add these
       variables to the taint trace. *)
    if Tok.is_fake tok then tok else Tok.fake_tok tok str
  in
  let i = G.SId.mk () in
  { ident = (str, tok); sid = i; id_info = G.empty_id_info () }

let fresh_label ?(label = "_label") (_env, _stmts) tok =
  let i = G.SId.mk () in
  ((label, tok), i)

let fresh_lval ?str (env, stmts) tok =
  let var = fresh_var ?str (env, stmts) tok in
  { base = Var var; rev_offset = [] }

let var_of_id_info id id_info =
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

let var_of_name name =
  match name with
  | G.Id (id, id_info) -> var_of_id_info id id_info
  | G.IdQualified { G.name_last = id, _typeargsTODO; name_info = id_info; _ } ->
      var_of_id_info id id_info

let lval_of_id_info (_env, _stmts) id id_info =
  let var = var_of_id_info id id_info in
  { base = Var var; rev_offset = [] }

(* TODO: use also qualifiers? *)
let lval_of_id_qualified (env, stmts)
    { G.name_last = id, _typeargsTODO; name_info = id_info; _ } =
  lval_of_id_info (env, stmts) id id_info

let lval_of_base base = { base; rev_offset = [] }

(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label (_env, _stmts) lbl = (lbl, G.SId.unsafe_default)
let lookup_label (_env, _stmts) lbl = (lbl, G.SId.unsafe_default)
let mk_e e eorig = { e; eorig }
let mk_i i iorig = { i; iorig }
let mk_s s = { s }

let mk_unit tok eorig =
  let unit = G.Unit tok in
  mk_e (Literal unit) eorig

let add_instr (env, stmts) instr = (env, mk_s (Instr instr) :: stmts)

(* Create an auxiliary variable for an expression.
 *
 * If 'force' is 'false' and the expression itself is already a variable then
 * it will not create an auxiliary variable but just return that. *)
let mk_aux_var ?(force = false) ?str (env, stmts) tok exp =
  match exp.e with
  | Fetch ({ base = Var var; rev_offset = []; _ } as lval) when not force ->
      ((env, stmts), var, lval)
  | __else__ ->
      let var = fresh_var ?str (env, stmts) tok in
      let lval = lval_of_base (Var var) in
      let env, stmts =
        add_instr (env, stmts) (mk_i (Assign (lval, exp)) NoOrig)
      in
      ((env, stmts), var, lval)

let add_call (env, stmts) tok eorig ~void mk_call =
  if void then
    let env, stmts = add_instr (env, stmts) (mk_i (mk_call None) eorig) in
    ((env, stmts), mk_unit tok NoOrig)
  else
    let lval = fresh_lval (env, stmts) tok in
    let env, stmts =
      add_instr (env, stmts) (mk_i (mk_call (Some lval)) eorig)
    in
    ((env, stmts), mk_e (Fetch lval) NoOrig)

let add_stmt (env, stmts) st = (env, st :: stmts)
let add_stmts (env, stmts) xs = (env, List.rev xs @ stmts)

let pop_stmts (env, stmts) =
  let xs = List.rev stmts in
  ((env, []), xs)

let with_pre_stmts (env, stmts) f =
  let (env, stmts1), r = f (env, []) in

  ((env, stmts), List.rev stmts1, r)

let ident_of_entity_opt ent =
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

let name_of_entity ent =
  match ident_of_entity_opt ent with
  | Some (i, pinfo) ->
      let name = var_of_id_info i pinfo in
      Some name
  | _____else_____ -> None

let composite_of_container ~g_expr :
    G.container_operator -> stmts -> IL.composite_kind =
 fun cont stmts ->
  match cont with
  | Array -> CArray
  | List -> CList
  | Tuple -> CTuple
  | Set -> CSet
  | Dict -> impossible stmts (E g_expr)

let mk_unnamed_args (exps : IL.exp list) = List_.map (fun x -> Unnamed x) exps

let is_hcl lang =
  match lang with
  | Lang.Terraform -> true
  | _ -> false

let mk_class_constructor_name (ty : G.type_) cons_id_info =
  match ty with
  | { t = TyN (G.Id (id, _)); _ }
  | { t = TyExpr { e = G.N (G.Id (id, _)); _ }; _ }
  (* FIXME: JS parser produces this ^ although it should be parsed as a 'TyN'. *)
    when Option.is_some !(cons_id_info.G.id_resolved) ->
      Some (G.Id (id, cons_id_info))
  | __else__ -> None

let add_entity_name ctx ident =
  { entity_names = IdentSet.add (H.str_of_ident ident) ctx.entity_names }

let def_expr_evaluates_to_value (lang : Lang.t) =
  match lang with
  | Elixir -> true
  | _else_ -> false

let is_constructor (env, _stmts) ret_ty id_info =
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
         possible. Istead we just check if it is a nominal typed.
         TODO could we somehow guarentee this type is a class? *)
      | { G.t = G.TyN _; _ } -> true
      | _ -> false)
  | _ -> false

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)

let rec lval (env, stmts) eorig =
  match eorig.G.e with
  | G.N n -> ((env, stmts), name (env, stmts) n)
  | G.IdSpecial (G.This, tok) ->
      ((env, stmts), lval_of_base (VarSpecial (This, tok)))
  | G.DotAccess (e1orig, tok, field) ->
      let (env, stmts), offset' =
        match field with
        | G.FN (G.Id (id, idinfo)) ->
            ((env, stmts), Dot (var_of_id_info id idinfo))
        | G.FN name ->
            let (env, stmts), attr = expr (env, stmts) (G.N name |> G.e) in
            ((env, stmts), Index attr)
        | G.FDynamic e2orig ->
            let (env, stmts), attr = expr (env, stmts) e2orig in
            ((env, stmts), Index attr)
      in
      let offset' = { o = offset'; oorig = SameAs eorig } in
      let (env, stmts), lv1 = nested_lval (env, stmts) tok e1orig in
      ((env, stmts), { lv1 with rev_offset = offset' :: lv1.rev_offset })
  | G.ArrayAccess (e1orig, (_, e2orig, _)) ->
      let tok = G.fake "[]" in
      let (env, stmts), lv1 = nested_lval (env, stmts) tok e1orig in
      let (env, stmts), e2 = expr (env, stmts) e2orig in
      let offset' = { o = Index e2; oorig = SameAs eorig } in
      ((env, stmts), { lv1 with rev_offset = offset' :: lv1.rev_offset })
  | G.DeRef (_, e1orig) ->
      let (env, stmts), e1 = expr (env, stmts) e1orig in
      ((env, stmts), lval_of_base (Mem e1))
  | _ -> ((env, stmts), todo stmts (G.E eorig))

and nested_lval (env, stmts) tok e_gen : (env * stmts) * lval =
  match expr (env, stmts) e_gen with
  | (env, stmts), { e = Fetch lval; _ } -> ((env, stmts), lval)
  | (env, stmts), rhs ->
      let fresh = fresh_lval (env, stmts) tok in
      let env, stmts =
        add_instr (env, stmts) (mk_i (Assign (fresh, rhs)) (related_exp e_gen))
      in
      ((env, stmts), fresh)

and name (env, stmts) = function
  | G.Id (("_", tok), _) ->
      (* wildcard *)
      fresh_lval (env, stmts) tok
  | G.Id (id, id_info) ->
      let lval = lval_of_id_info (env, stmts) id id_info in
      lval
  | G.IdQualified qualified_info ->
      let lval = lval_of_id_qualified (env, stmts) qualified_info in
      lval

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)

(* TODO: This code is very similar to that of `assign`. Actually, we should not
 * be dealing with patterns in the LHS of `Assign`, those are supposed to be
 * `LetPattern`s. *)
and pattern (env, stmts) pat =
  match pat with
  | G.PatWildcard tok ->
      let lval = fresh_lval (env, stmts) tok in
      ((env, stmts), lval, [])
  | G.PatId (id, id_info) ->
      let lval = lval_of_id_info (env, stmts) id id_info in
      ((env, stmts), lval, [])
  | G.PatList (_tok1, pats, tok2)
  | G.PatTuple (_tok1, pats, tok2) ->
      (* P1, ..., Pn *)
      let tmp = fresh_var (env, stmts) tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      (* Pi = tmp[i] *)
      let (env, stmts), ss =
        List.fold_left_map
          (fun (env, stmts) (pat_i, i) ->
            let eorig = Related (G.P pat_i) in
            let index_i = Literal (G.Int (Parsed_int.of_int i)) in
            let offset_i =
              { o = Index { e = index_i; eorig }; oorig = NoOrig }
            in
            let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
            pattern_assign_statements (env, stmts)
              (mk_e (Fetch lval_i) eorig)
              ~eorig pat_i)
          (env, stmts) (List_.index_list pats)
      in
      let flatten_ss = List_.flatten ss in
      ((env, stmts), tmp_lval, flatten_ss)
  | G.PatTyped (pat1, ty) ->
      let (env, stmts), _ = type_ (env, stmts) ty in
      pattern (env, stmts) pat1
  | _ -> todo stmts (G.P pat)

and _catch_exn (env, stmts) exn =
  match exn with
  | G.CatchPattern pat -> pattern (env, stmts) pat
  | G.CatchParam { pname = Some id; pinfo = id_info; _ } ->
      let lval = lval_of_id_info (env, stmts) id id_info in
      ((env, stmts), lval, [])
  | _ -> todo stmts (G.Ce exn)

and pattern_assign_statements (env, stmts) ?(eorig = NoOrig) exp pat :
    (env * stmts) * stmt list =
  try
    let (env, stmts), lval, ss = pattern (env, stmts) pat in
    ((env, stmts), [ mk_s (Instr (mk_i (Assign (lval, exp)) eorig)) ] @ ss)
  with
  | Fixme (stmts, kind, any_generic) ->
      ((env, stmts), fixme_stmt kind any_generic)

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
and try_catch_else_finally (env, stmts) ~try_st ~catches ~opt_else ~opt_finally
    =
  let (env, stmts), try_stmt = stmt (env, stmts) try_st in
  let (env, stmts), catches_stmt_rev =
    List.fold_left_map
      (fun (env, stmts) (ctok, exn, catch_st) ->
        (* TODO: Handle exn properly. *)
        let name = fresh_var (env, stmts) ctok in
        let todo_pattern = fixme_stmt ToDo (G.Ce exn) in
        let (env, stmts), catch_stmt = stmt (env, stmts) catch_st in
        ((env, stmts), (name, todo_pattern @ catch_stmt)))
      (env, stmts) catches
  in
  let (env, stmts), else_stmt =
    match opt_else with
    | None -> ((env, stmts), [])
    | Some (_tok, else_st) -> stmt (env, stmts) else_st
  in
  let (env, stmts), finally_stmt =
    match opt_finally with
    | None -> ((env, stmts), [])
    | Some (_tok, finally_st) -> stmt (env, stmts) finally_st
  in
  ( (env, stmts),
    [ mk_s (Try (try_stmt, catches_stmt_rev, else_stmt, finally_stmt)) ] )

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)
and assign (env, stmts) ~g_expr lhs tok rhs_exp =
  let eorig = SameAs g_expr in
  match lhs.G.e with
  | G.N _
  | G.DotAccess _
  | G.ArrayAccess _
  | G.DeRef _ -> (
      try
        let (env, stmts), lval = lval (env, stmts) lhs in
        let env, stmts =
          add_instr (env, stmts) (mk_i (Assign (lval, rhs_exp)) eorig)
        in
        ((env, stmts), mk_e (Fetch lval) (SameAs lhs))
      with
      | Fixme (stmts, kind, any_generic) ->
          (* lval translation failed, we use a fresh lval instead *)
          let fixme_lval = fresh_lval ~str:"_FIXME" (env, stmts) tok in
          let env, stmts =
            add_instr (env, stmts) (mk_i (Assign (fixme_lval, rhs_exp)) eorig)
          in
          ((env, stmts), fixme_exp kind any_generic (related_exp g_expr)))
  | G.Container (((G.Tuple | G.List | G.Array) as ckind), (tok1, lhss, tok2)) ->
      (* TODO: handle cases like [a, b, ...rest] = e *)
      (* E1, ..., En = RHS *)
      (* tmp = RHS*)
      let tmp = fresh_var (env, stmts) tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      let env, stmts =
        add_instr (env, stmts) (mk_i (Assign (tmp_lval, rhs_exp)) eorig)
      in
      (* Ei = tmp[i] *)
      let (env, stmts), tup_elems =
        List.fold_left_map
          (fun (env, stmts) (lhs_i, i) ->
            let index_i = Literal (G.Int (Parsed_int.of_int i)) in
            let offset_i =
              {
                o = Index { e = index_i; eorig = related_exp lhs_i };
                oorig = NoOrig;
              }
            in
            let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
            let (env, stmts), expr =
              assign (env, stmts) ~g_expr lhs_i tok1
                { e = Fetch lval_i; eorig = related_exp lhs_i }
            in
            ((env, stmts), expr))
          (env, stmts) (List_.index_list lhss)
      in
      (* (E1, ..., En) *)
      ( (env, stmts),
        mk_e
          (Composite
             ( composite_of_container ~g_expr ckind stmts,
               (tok1, tup_elems, tok2) ))
          (related_exp lhs) )
  | G.Record (tok1, fields, tok2) ->
      assign_to_record (env, stmts) (tok1, fields, tok2) rhs_exp
        (related_exp lhs)
  | _ ->
      let env, stmts =
        add_instr (env, stmts)
          (fixme_instr ToDo (G.E g_expr) (related_exp g_expr))
      in
      ((env, stmts), fixme_exp ToDo (G.E g_expr) (related_exp lhs))

and assign_to_record (env, stmts) (tok1, fields, tok2) rhs_exp lhs_orig =
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
  let (env, stmts), tmp, _tmp_lval = mk_aux_var (env, stmts) tok1 rhs_exp in
  let rec do_fields (env, stmts) acc_rev_offsets fs =
    let (env, stmts), fields =
      List.fold_left_map
        (fun (env, stmts) x ->
          let (env, stmts), f = do_field (env, stmts) acc_rev_offsets x in
          ((env, stmts), f))
        (env, stmts) fs
    in
    ((env, stmts), fields)
  and do_field (env, stmts) acc_rev_offsets f =
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
        let env, stmts =
          add_instr (env, stmts)
            (mk_i (Assign (vari_lval, ei)) (related_tok tok))
        in
        ((env, stmts), Field (fldi, mk_e (Fetch vari_lval) (related_tok tok)))
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
        let (env, stmts), fields =
          do_fields (env, stmts) (offset :: acc_rev_offsets) fields
        in
        ( (env, stmts),
          Field (fldi, mk_e (RecordOrDict fields) (related_tok tok)) )
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
        let tmpi = fresh_var (env, stmts) tok2 in
        let tmpi_lval = lval_of_base (Var tmpi) in
        let env, stmts =
          add_instr (env, stmts)
            (mk_i (Assign (tmpi_lval, ei)) (related_tok tok1))
        in
        ( (env, stmts),
          Field (xn, mk_e (Fetch tmpi_lval) (Related (G.Fld field))) )
  in
  let (env, stmts), fields = do_fields (env, stmts) [] fields in
  (* {x1: E1, ..., xN: En} *)
  ((env, stmts), mk_e (RecordOrDict fields) lhs_orig)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(* less: we could pass in an optional lval that we know the caller want
 * to assign into, which would avoid creating useless fresh_var intermediates.
 *)
(* We set `void` to `true` when the value of the expression is being discarded, in
 * which case, for certain expressions and in certain languages, we assume that the
 * expression has side-effects. See translation of operators below. *)
and expr_aux (env, stmts) ?(void = false) g_expr =
  let eorig = SameAs g_expr in
  match g_expr.G.e with
  | G.Call
      ( { e = G.IdSpecial (G.Op ((G.And | G.Or) as op), tok); _ },
        (_, arg0 :: args, _) )
    when not void ->
      expr_lazy_op (env, stmts) op tok arg0 args eorig
  (* args_with_pre_stmts *)
  | G.Call ({ e = G.IdSpecial (G.Op op, tok); _ }, args) -> (
      let (env, stmts), args = arguments (env, stmts) (Tok.unbracket args) in
      if not void then ((env, stmts), mk_e (Operator ((op, tok), args)) eorig)
      else
        (* The operation's result is not being used, so it may have side-effects.
         * We then assume this is just syntax sugar for a method call. E.g. in
         * Ruby `s << "hello"` is syntax sugar for `s.<<("hello")` and it mutates
         * the string `s` appending "hello" to it. *)
        match args with
        | [] -> impossible stmts (G.E g_expr)
        | obj :: args' ->
            let (env, stmts), obj_var, _obj_lval =
              mk_aux_var (env, stmts) tok (IL_helpers.exp_of_arg obj)
            in
            let method_name =
              fresh_var (env, stmts) tok ~str:(Tok.content_of_tok tok)
            in
            let offset = { o = Dot method_name; oorig = NoOrig } in
            let method_lval = { base = Var obj_var; rev_offset = [ offset ] } in
            let method_ = { e = Fetch method_lval; eorig = related_tok tok } in
            add_call (env, stmts) tok eorig ~void (fun res ->
                Call (res, method_, args')))
  | G.Call
      ( ({ e = G.IdSpecial ((G.This | G.Super | G.Self | G.Parent), tok); _ } as
         e),
        args ) ->
      call_generic (env, stmts) ~void tok eorig e args
  | G.Call
      ({ e = G.IdSpecial (G.IncrDecr (incdec, _prepostIGNORE), tok); _ }, args)
    -> (
      (* in theory in expr() we should return each time a list of pre-instr
       * and a list of post-instrs to execute before and after the use
       * of the expression. However this complicates the interface of 'expr()'.
       * Right now, for the pre-instr we agglomerate them instead in (env, stmts)
       * and use them in 'expr_with_pre_instr()' below, but for the post
       * we dont. Anyway, for our static analysis purpose it should not matter.
       * We don't do fancy path-sensitive-evaluation-order-sensitive analysis.
       *)
      match Tok.unbracket args with
      | [ G.Arg e ] ->
          let (env, stmts), lval = lval (env, stmts) e in
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
          let env, stmts =
            add_instr (env, stmts) (mk_i (Assign (lval, opexp)) eorig)
          in
          ((env, stmts), lvalexp)
      | _ -> impossible stmts (G.E g_expr))
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
      let (env, stmts), obj_expr = expr (env, stmts) obj in
      let obj_arg' = Unnamed obj_expr in
      let (env, stmts), args' = arguments (env, stmts) (Tok.unbracket args) in
      let (env, stmts), res =
        match env.lang with
        (* Ruby's concat method is side-effectful and updates the object. *)
        (* TODO: The lval in the LHs should have a differnt svalue than the
         * one in the RHS. *)
        | Lang.Ruby -> (
            try lval (env, stmts) obj with
            | Fixme (stmts, _, _) ->
                ((env, stmts), fresh_lval ~str:"Fixme" (env, stmts) tok))
        | _ -> ((env, stmts), fresh_lval (env, stmts) tok)
      in
      let env, stmts =
        add_instr (env, stmts)
          (mk_i
             (CallSpecial (Some res, (Concat, tok), obj_arg' :: args'))
             eorig)
      in
      ((env, stmts), mk_e (Fetch res) eorig)
  (* todo: if the xxx_to_generic forgot to generate Eval *)
  | G.Call
      ( {
          e =
            G.N
              (G.Id (("eval", tok), { G.id_resolved = { contents = None }; _ }));
          _;
        },
        args ) ->
      let lval = fresh_lval (env, stmts) tok in
      let special = (Eval, tok) in
      let (env, stmts), args = arguments (env, stmts) (Tok.unbracket args) in
      let env, stmts =
        add_instr (env, stmts)
          (mk_i (CallSpecial (Some lval, special, args)) eorig)
      in
      ((env, stmts), mk_e (Fetch lval) (related_tok tok))
  | G.Call
      ({ e = G.IdSpecial (G.InterpolatedElement, _); _ }, (_, [ G.Arg e ], _))
    ->
      (* G.InterpolatedElement is useful for matching certain patterns against
       * interpolated strings, but we do not have an use for it yet during
       * semantic analysis, so in the IL we just unwrap the expression. *)
      expr (env, stmts) e
  | G.New (tok, ty, _cons_id_info, args) ->
      (* HACK: Fall-through case where we don't know to what variable the allocated
       * object is being assigned to. See HACK(new), we expect to intercept `New`
       * already in 'stmt_aux'.
       *)
      let lval = fresh_lval (env, stmts) tok in
      let (env, stmts), args = arguments (env, stmts) (Tok.unbracket args) in
      let (env, stmts), t = type_ (env, stmts) ty in
      let env, stmts =
        add_instr (env, stmts) (mk_i (New (lval, t, None, args)) eorig)
      in
      ((env, stmts), mk_e (Fetch lval) NoOrig)
  | G.Call ({ e = G.IdSpecial spec; _ }, args) -> (
      let tok = snd spec in
      let (env, stmts), args = arguments (env, stmts) (Tok.unbracket args) in
      try
        let special = call_special (env, stmts) spec in
        add_call (env, stmts) tok eorig ~void (fun res ->
            CallSpecial (res, special, args))
      with
      | Fixme (stmts, kind, any_generic) ->
          let fixme = fixme_exp kind any_generic (related_exp g_expr) in
          add_call (env, stmts) tok eorig ~void (fun res ->
              Call (res, fixme, args)))
  | G.Call (e, args) ->
      let tok = G.fake "call" in
      call_generic (env, stmts) ~void tok eorig e args
  | G.L lit -> ((env, stmts), mk_e (Literal lit) eorig)
  | G.DotAccess ({ e = N (Id (("var", _), _)); _ }, _, FN (Id ((s, t), id_info)))
    when is_hcl env.lang ->
      (* We need to change all uses of a variable, which looks like a DotAccess, to a name which
         reads the same. This is so that our parameters to our function can properly be recognized
         as tainted by the taint engine.
      *)
      expr_aux (env, stmts) (G.N (Id (("var." ^ s, t), id_info)) |> G.e)
  | G.N _
  | G.DotAccess (_, _, _)
  | G.ArrayAccess (_, _)
  | G.DeRef (_, _) ->
      let (env, stmts), lval = lval (env, stmts) g_expr in
      let exp = mk_e (Fetch lval) eorig in
      let ident_function_call_hack exp =
        (* Taking into account Ruby's ability to allow function calls without
         * parameters or parentheses, we are conducting a check to determine
         * if a function with the same name as the identifier exists, specifically
         * for Ruby. *)
        match lval with
        | { base = Var { ident; id_info; _ }; _ }
          when env.lang =*= Lang.Ruby
               && Option.is_none !(id_info.id_resolved)
               && IdentSet.mem (H.str_of_ident ident) env.ctx.entity_names ->
            let tok = G.fake "call" in
            add_call (env, stmts) tok eorig ~void (fun res ->
                Call (res, exp, []))
        | _ -> ((env, stmts), exp)
      in
      ident_function_call_hack exp
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
    when is_constructor (env, stmts) ret_ty id_info ->
      let obj' = var_of_name obj in
      let (env, stmts), lval, ss =
        mk_class_construction (env, stmts) obj' origin_exp ret_ty id_info args
      in
      let env, stmts = add_stmts (env, stmts) ss in
      ((env, stmts), mk_e (Fetch lval) (SameAs obj_e))
  | G.Assign (e1, tok, e2) ->
      let (env, stmts), exp = expr (env, stmts) e2 in
      assign (env, stmts) ~g_expr e1 tok exp
  | G.AssignOp (e1, (G.Eq, tok), e2) ->
      (* AsssignOp(Eq) is used to represent plain assignment in some languages,
       * e.g. Go's `:=` is represented as `AssignOp(Eq)`, and C#'s assignments
       * are all represented this way too. *)
      let (env, stmts), exp = expr (env, stmts) e2 in
      assign (env, stmts) ~g_expr e1 tok exp
  | G.AssignOp (e1, op, e2) ->
      let (env, stmts), exp = expr (env, stmts) e2 in
      let (env, stmts), lval = lval (env, stmts) e1 in
      let lvalexp = mk_e (Fetch lval) (SameAs e1) in
      let opexp =
        mk_e
          (Operator (op, [ Unnamed lvalexp; Unnamed exp ]))
          (related_tok (snd op))
      in
      let env, stmts =
        add_instr (env, stmts) (mk_i (Assign (lval, opexp)) eorig)
      in
      ((env, stmts), lvalexp)
  | G.LetPattern (pat, e) ->
      let (env, stmts), exp = expr (env, stmts) e in
      let (env, stmts), new_stmts =
        pattern_assign_statements (env, stmts) ~eorig exp pat
      in
      let env, stmts = add_stmts (env, stmts) new_stmts in
      ((env, stmts), mk_unit (G.fake "()") NoOrig)
  | G.Seq xs -> (
      match List.rev xs with
      | [] -> impossible stmts (G.E g_expr)
      | last :: xs ->
          let xs = List.rev xs in
          let (env, stmts), _eIGNORE =
            List.fold_left_map
              (fun (env, stmts) x -> expr (env, stmts) x)
              (env, stmts) xs
          in
          expr (env, stmts) last)
  | G.Record fields -> record (env, stmts) fields
  | G.Container (G.Dict, xs) -> dict (env, stmts) xs g_expr
  | G.Container (kind, xs) ->
      let l, xs, r = xs in
      let (env, stmts), xs =
        List.fold_left_map
          (fun (env, stmts) x -> expr (env, stmts) x)
          (env, stmts) xs
      in
      let kind = composite_kind ~g_expr kind in
      ((env, stmts), mk_e (Composite (kind, (l, xs, r))) eorig)
  | G.Comprehension _ -> todo stmts (G.E g_expr)
  | G.Lambda fdef ->
      let lval = fresh_lval (env, stmts) (snd fdef.fkind) in
      let (env, stmts), fdef =
        (* NOTE(config.stmts): This is a recursive call to
         * `function_definition` and we need to pass it a fresh
         * `stmts` ref list. If we reuse the same `stmts` ref list,
         * then whatever `stmts` we have accumulated so far, will
         * "magically" appear in the body of this lambda in the final
         * IL representation. This can happen e.g. when translating
         * `foo(bar(), (x) => { ... })`, because the instruction added
         * to `stmts` by the translation of `bar()` is still present
         * when traslating `(x) => { ... }`. *)
        function_definition (env, []) fdef
      in
      let env, stmts =
        add_instr (env, stmts) (mk_i (AssignAnon (lval, Lambda fdef)) eorig)
      in
      ((env, stmts), mk_e (Fetch lval) eorig)
  | G.AnonClass def ->
      (* TODO: should use def.ckind *)
      let tok = Common2.fst3 def.G.cbody in
      let lval = fresh_lval (env, stmts) tok in
      let env, stmts =
        add_instr (env, stmts) (mk_i (AssignAnon (lval, AnonClass def)) eorig)
      in
      ((env, stmts), mk_e (Fetch lval) eorig)
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
          ((env, stmts), mk_e (Fetch lval) eorig)
      | None -> impossible stmts (G.E g_expr))
  | G.SliceAccess (_, _) -> todo stmts (G.E g_expr)
  (* e1 ? e2 : e3 ==>
   *  pre: lval = e1;
   *       if(lval) { lval = e2 } else { lval = e3 }
   *  exp: lval
   *)
  | G.Conditional (e1_gen, e2_gen, e3_gen) ->
      let tok = G.fake "conditional" in
      let lval = fresh_lval (env, stmts) tok in

      (* not sure this is correct *)
      let (env, stmts1), e1 = expr (env, []) e1_gen in
      let ss_for_e1 = List.rev stmts1 in
      let (env, stmts2), e2 = expr (env, []) e2_gen in
      let ss_for_e2 = List.rev stmts2 in
      let (env, stmts3), e3 = expr (env, []) e3_gen in
      let ss_for_e3 = List.rev stmts3 in

      let env, stmts = add_stmts (env, stmts) ss_for_e1 in
      let env, stmts =
        add_stmt (env, stmts)
          (mk_s
             (If
                ( tok,
                  e1,
                  ss_for_e2 @ [ mk_s (Instr (mk_i (Assign (lval, e2)) NoOrig)) ],
                  ss_for_e3 @ [ mk_s (Instr (mk_i (Assign (lval, e3)) NoOrig)) ]
                )))
      in
      ((env, stmts), mk_e (Fetch lval) eorig)
  | G.Await (tok, e1orig) ->
      let (env, stmts), e1 = expr (env, stmts) e1orig in
      let tmp = fresh_lval (env, stmts) tok in
      let env, stmts =
        add_instr (env, stmts)
          (mk_i (CallSpecial (Some tmp, (Await, tok), [ Unnamed e1 ])) eorig)
      in
      ((env, stmts), mk_e (Fetch tmp) NoOrig)
  | G.Yield (tok, e1orig_opt, _) ->
      let (env, stmts), yield_args =
        match e1orig_opt with
        | None -> ((env, stmts), [])
        | Some e1orig ->
            let (env, stmts), y = expr (env, stmts) e1orig in
            ((env, stmts), [ y ])
      in
      let env, stmts =
        add_instr (env, stmts)
          (mk_i
             (CallSpecial (None, (Yield, tok), mk_unnamed_args yield_args))
             eorig)
      in
      ((env, stmts), mk_unit tok NoOrig)
  | G.Ref (tok, e1orig) ->
      let (env, stmts), e1 = expr (env, stmts) e1orig in
      let tmp = fresh_lval (env, stmts) tok in
      let env, stmts =
        add_instr (env, stmts)
          (mk_i (CallSpecial (Some tmp, (Ref, tok), [ Unnamed e1 ])) eorig)
      in
      ((env, stmts), mk_e (Fetch tmp) NoOrig)
  | G.Constructor (cname, (tok1, esorig, tok2)) ->
      let cname = var_of_name cname in
      let (env, stmts), es =
        List.fold_left_map
          (fun (env, stmts) eiorig -> expr (env, stmts) eiorig)
          (env, stmts) esorig
      in
      ( (env, stmts),
        mk_e (Composite (Constructor cname, (tok1, es, tok2))) eorig )
  | G.RegexpTemplate ((l, e, r), _opt) ->
      let (env, stmts), e = expr (env, stmts) e in
      ((env, stmts), mk_e (Composite (Regexp, (l, [ e ], r))) NoOrig)
  | G.Xml xml -> xml_expr (env, stmts) ~void eorig xml
  | G.Cast (typ, _, e) ->
      let (env, stmts), e = expr (env, stmts) e in
      ((env, stmts), mk_e (Cast (typ, e)) eorig)
  | G.Alias (_alias, e) -> expr (env, stmts) e
  | G.LocalImportAll (_module, _tk, e) ->
      (* TODO: what can we do with _module? *)
      expr (env, stmts) e
  | G.Ellipsis _
  | G.TypedMetavar (_, _, _)
  | G.DisjExpr (_, _)
  | G.DeepEllipsis _
  | G.DotAccessEllipsis _ ->
      sgrep_construct stmts (G.E g_expr)
  | G.StmtExpr st -> stmt_expr (env, stmts) ~g_expr st
  | G.OtherExpr ((str, tok), xs) ->
      let (env, stmts), es =
        List.fold_left_map
          (fun (env, stmts) x ->
            match x with
            | G.E e1orig -> expr (env, stmts) e1orig
            | __else__ -> ((env, stmts), fixme_exp ToDo x (related_tok tok)))
          (env, stmts) xs
      in
      let other_expr = mk_e (Composite (CTuple, (tok, es, tok))) eorig in
      let (env, stmts), _, tmp = mk_aux_var ~str (env, stmts) tok other_expr in
      let partial = mk_e (Fetch tmp) (related_tok tok) in
      ((env, stmts), fixme_exp ToDo (G.E g_expr) (related_tok tok) ~partial)
  | G.RawExpr _ -> todo stmts (G.E g_expr)

and expr (env, stmts) ?void e_gen : (env * stmts) * exp =
  try expr_aux (env, stmts) ?void e_gen with
  | Fixme (stmts, kind, any_generic) ->
      ((env, stmts), fixme_exp kind any_generic (related_exp e_gen))

and expr_opt (env, stmts) tok = function
  | None ->
      let void = G.Unit tok in
      ((env, stmts), mk_e (Literal void) (related_tok tok))
  | Some e -> expr (env, stmts) e

and expr_lazy_op (env, stmts) op tok arg0 args eorig =
  let (env, stmts), arg0' = argument (env, stmts) arg0 in
  let ((env, stmts), _), args' =
    (* Consider A && B && C, side-effects in B must only take effect `if A`,
     * and side-effects in C must only take effect `if A && B`. *)
    args
    |> List.fold_left_map
         (fun ((env, stmts), cond) argi ->
           let (env, stmts), ssi, argi' =
             arg_with_pre_stmts (env, stmts) argi
           in
           let env, stmts =
             if ssi <> [] then
               add_stmt (env, stmts) (mk_s @@ If (tok, cond, ssi, []))
             else (env, stmts)
           in
           let condi =
             mk_e (Operator ((op, tok), [ Unnamed cond; argi' ])) eorig
           in
           (((env, stmts), condi), argi'))
         ((env, stmts), IL_helpers.exp_of_arg arg0')
  in
  ((env, stmts), mk_e (Operator ((op, tok), arg0' :: args')) eorig)

and call_generic (env, stmts) ?(void = false) tok eorig e args =
  let (env, stmts), e = expr (env, stmts) e in
  (* In theory, instrs in args could have side effect on the value in 'e',
   * but we will agglomerate all those instrs in the (env, stmts)ironment and
   * the caller will call them in sequence (see expr_with_pre_instr).
   * In theory, we should not execute those instrs before getting the
   * value in 'e' in the caller, but for our static analysis purpose
   * we should not care about those edge cases. That would require
   * to return in expr multiple arguments and thread things around; Not
   * worth it.
   *)
  let (env, stmts), args = arguments (env, stmts) (Tok.unbracket args) in
  add_call (env, stmts) tok eorig ~void (fun res -> Call (res, e, args))

and call_special (_env, stmts) (x, tok) =
  ( (match x with
    | G.Op _
    | G.IncrDecr _
    | G.This
    | G.Super
    | G.Self
    | G.Parent
    | G.InterpolatedElement ->
        impossible stmts (G.E (G.IdSpecial (x, tok) |> G.e))
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
        todo stmts (G.E (G.IdSpecial (x, tok) |> G.e))),
    tok )

and composite_kind ~g_expr = function
  | G.Array -> CArray
  | G.List -> CList
  | G.Dict -> impossible [] (E g_expr)
  | G.Set -> CSet
  | G.Tuple -> CTuple

(* TODO: dependency of order between arguments for instr? *)
and arguments (env, stmts) xs = List.fold_left_map argument (env, stmts) xs

and argument (env, stmts) arg =
  match arg with
  | G.Arg e ->
      let (env, stmts), arg = expr (env, stmts) e in
      ((env, stmts), Unnamed arg)
  | G.ArgKwd (id, e)
  | G.ArgKwdOptional (id, e) ->
      let (env, stmts), arg = expr (env, stmts) e in
      ((env, stmts), Named (id, arg))
  | G.ArgType { t = TyExpr e; _ } ->
      let (env, stmts), arg = expr (env, stmts) e in
      ((env, stmts), Unnamed arg)
  | __else__ ->
      let any = G.Ar arg in
      ((env, stmts), Unnamed (fixme_exp ToDo any (Related any)))

and record (env, stmts) ((_tok, origfields, _) as record_def) =
  let e_gen = G.Record record_def |> G.e in
  let (env, stmts), fields =
    List.fold_left_map
      (fun (env, stmts) x ->
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
            let (env, stmts), field_def =
              match def_kind with
              (* TODO: Consider what to do with vtype. *)
              | G.VarDef { G.vinit = Some fdeforig; _ }
              | G.FieldDefColon { G.vinit = Some fdeforig; _ } ->
                  expr (env, stmts) fdeforig
              (* Some languages such as javascript allow function
                  definitions in object literal syntax. *)
              | G.FuncDef fdef ->
                  let lval = fresh_lval (env, stmts) (snd fdef.fkind) in
                  (* See NOTE(config.stmts)! *)
                  let _, fdef = function_definition (env, []) fdef in
                  let forig = Related (G.Fld forig) in
                  let env, stmts =
                    add_instr (env, stmts)
                      (mk_i (AssignAnon (lval, Lambda fdef)) forig)
                  in
                  ((env, stmts), mk_e (Fetch lval) forig)
              | ___else___ -> ((env, stmts), todo stmts (G.E e_gen))
            in
            ((env, stmts), Some (Field (field_name, field_def)))
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
            let (env, stmts), expression = expr (env, stmts) e in
            ((env, stmts), Some (Spread expression))
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
            (* This is an inner block of the form
                someblockhere {
                  s {
                    <args>
                  }
                }

                We want this to be understood as a record of { <args> } being bound to
                the name `s`.

                So we just translate it to a field defining `s = <record>`.

                We don't actually really care for it to be specifically defining the name `s`.
                we just want it in there at all so that we can use it as a sink.
             *)
            let field_name = var_of_id_info id id_info in
            let (env, stmts), field_expr = record (env, stmts) fields in
            (* We need to use the entire `prior_expr` here, or the range won't be quite
                right (we'll leave out the identifier)
             *)
            ( (env, stmts),
              Some
                (Field
                   (field_name, { field_expr with eorig = SameAs prior_expr }))
            )
        | _ when is_hcl env.lang ->
            (* For HCL constructs such as `lifecycle` blocks within a module call, the
                IL translation engine will brick the whole record if it is encountered.
                To avoid this, we will just ignore any unrecognized fields for HCL specifically.
             *)
            log_warning "Skipping HCL record field during IL translation";
            ((env, stmts), None)
        | G.F _ -> ((env, stmts), todo stmts (G.E e_gen)))
      (env, stmts) origfields
  in
  let filtered_fields = List.filter_map Fun.id fields in
  ((env, stmts), mk_e (RecordOrDict filtered_fields) (SameAs e_gen))

and dict (env, stmts) (_, orig_entries, _) orig =
  let (env, stmts), entries =
    List.fold_left_map
      (fun (env, stmts) orig_entry ->
        match orig_entry.G.e with
        | G.Container (G.Tuple, (_, [ korig; vorig ], _)) ->
            let (env, stmts), ke = expr (env, stmts) korig in
            let (env, stmts), ve = expr (env, stmts) vorig in
            ((env, stmts), Entry (ke, ve))
        | __else__ -> todo stmts (G.E orig))
      (env, stmts) orig_entries
  in
  ((env, stmts), mk_e (RecordOrDict entries) (SameAs orig))

and xml_expr (env, stmts) ~void eorig xml =
  let tok, jsx_name =
    match xml.G.xml_kind with
    | G.XmlClassic (tok, name, _, _)
    | G.XmlSingleton (tok, name, _) ->
        (tok, Some name)
    | G.XmlFragment (tok, _) -> (tok, None)
  in
  let (env, stmts), body =
    List.fold_left_map
      (fun (env, stmts) x ->
        match x with
        | G.XmlExpr (tok, Some eorig, _) ->
            let (env, stmts), exp = expr (env, stmts) eorig in
            let (env, stmts), _, lval = mk_aux_var (env, stmts) tok exp in
            ((env, stmts), Some (mk_e (Fetch lval) (SameAs eorig)))
        | G.XmlXml xml' ->
            let eorig' = SameAs (G.Xml xml' |> G.e) in
            let (env, stmts), xml =
              xml_expr (env, stmts) ~void:false eorig' xml'
            in
            ((env, stmts), Some xml)
        | G.XmlExpr (_, None, _)
        | G.XmlText _ ->
            ((env, stmts), None))
      (env, stmts) xml.G.xml_body
  in
  let filtered_body = List.filter_map Fun.id body in
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
      let name_lval = name (env, stmts) jsx_name in
      let e = mk_e (Fetch name_lval) name_eorig in
      let (env, stmts), fields =
        List.fold_left_map
          (fun (env, stmts) x ->
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
                let (env, stmts), e = expr (env, stmts) eorig in
                let (env, stmts), _, lval = mk_aux_var (env, stmts) tok e in
                let e = mk_e (Fetch lval) (SameAs eorig) in
                ((env, stmts), Some (Field (attr_name, e)))
            | G.XmlAttrExpr (_l, eorig, _r) ->
                let (env, stmts), e = expr (env, stmts) eorig in
                ((env, stmts), Some (Spread e))
            | G.XmlEllipsis _ ->
                (* Should never encounter this in a target *)
                ((env, stmts), None))
          (env, stmts) xml.G.xml_attrs
      in
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
      let filtered_fields = List.filter_map Fun.id fields in
      let fields = Field (children_field_name, body_exp) :: filtered_fields in
      let fields_orig =
        let attrs = xml.G.xml_attrs |> List_.map (fun attr -> G.XmlAt attr) in
        let body = G.Xmls xml.G.xml_body in
        Related (G.Anys (body :: attrs))
      in
      let record = mk_e (RecordOrDict fields) fields_orig in
      let args = [ Unnamed record ] in
      add_call (env, stmts) tok eorig ~void (fun res -> Call (res, e, args))
  | Some _
  | None ->
      let (env, stmts), attrs =
        List.fold_left_map
          (fun (env, stmts) x ->
            match x with
            | G.XmlAttr (_, tok, eorig)
            | G.XmlAttrExpr (tok, eorig, _) ->
                let (env, stmts), exp = expr (env, stmts) eorig in
                let (env, stmts), _, lval = mk_aux_var (env, stmts) tok exp in
                ((env, stmts), Some (mk_e (Fetch lval) (SameAs eorig)))
            | _ -> ((env, stmts), None))
          (env, stmts) xml.G.xml_attrs
      in
      let filtered_attrs = List.filter_map Fun.id attrs in
      ( (env, stmts),
        mk_e
          (Composite
             (CTuple, (tok, List.rev_append filtered_attrs filtered_body, tok)))
          (Related (G.Xmls xml.G.xml_body)) )

and stmt_expr (env, stmts) ?g_expr st =
  let todo stmts =
    match g_expr with
    | None -> todo stmts (G.E (G.e (G.StmtExpr st)))
    | Some e_gen -> todo stmts (G.E e_gen)
  in
  match st.G.s with
  | G.ExprStmt (eorig, tok) ->
      let (env, stmts), e = expr (env, stmts) eorig in
      if eorig.is_implicit_return then
        let expr = mk_s (Return (tok, e)) in
        let env, stmts = add_stmt (env, stmts) expr in
        expr_opt (env, stmts) tok None
      else ((env, stmts), e)
  | G.OtherStmt
      ( OS_Delete,
        ( [ (G.Tk tok as atok); G.E eorig ]
        | [ (G.Tk tok as atok); G.Tk _; G.Tk _; G.E eorig ] (* delete[] *) ) )
    ->
      let (env, stmts), e = expr (env, stmts) eorig in
      let special = (Delete, tok) in
      let env, stmts =
        add_instr (env, stmts)
          (mk_i (CallSpecial (None, special, [ Unnamed e ])) (Related atok))
      in
      ((env, stmts), mk_unit tok (Related atok))
  | G.If (tok, cond, st1, opt_st2) ->
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
      let (env, stmts), ss, e' = cond_with_pre_stmts (env, stmts) cond in
      let (env, stmts), pre_a1, e1 =
        stmt_expr_with_pre_stmts (env, stmts) st1
      in
      let (env, stmts), pre_a2, e2 =
        match opt_st2 with
        | Some st2 -> stmt_expr_with_pre_stmts (env, stmts) st2
        | None ->
            (* Coming from OCaml-land we would not expect this to happen... but
             * we got some Ruby examples from r2c's SR team where there is an `if`
             * expression without an `else`... anyways, if it happens we translate
             * what we can, and we fill-in the `else` with a "fixme" node. *)
            ((env, stmts), [], fixme_exp ToDo (G.Tk tok) (Related (G.S st)))
      in
      let fresh = fresh_lval (env, stmts) tok in
      let a1 = mk_s (Instr (mk_i (Assign (fresh, e1)) (related_tok tok))) in
      let a2 = mk_s (Instr (mk_i (Assign (fresh, e2)) (related_tok tok))) in
      let env, stmts =
        add_stmts (env, stmts)
          (ss @ [ mk_s (If (tok, e', pre_a1 @ [ a1 ], pre_a2 @ [ a2 ])) ])
      in
      let eorig =
        match g_expr with
        | None -> related_exp (G.e (G.StmtExpr st))
        | Some e_gen -> SameAs e_gen
      in
      ((env, stmts), mk_e (Fetch fresh) eorig)
  | G.Block (_, block, _) -> (
      (* See 'AST_generic.stmt_to_expr' *)
      match List.rev block with
      | st :: rev_sts ->
          let (env, stmts), new_stmts =
            List.fold_left_map stmt (env, stmts) (List.rev rev_sts)
          in
          let env, stmts = add_stmts (env, stmts) (List_.flatten new_stmts) in
          stmt_expr (env, stmts) st
      | __else__ -> todo stmts)
  | G.Return (t, eorig, _) ->
      let (env, stmts), expression = expr_opt (env, stmts) t eorig in
      let env, stmts = mk_s (Return (t, expression)) |> add_stmt (env, stmts) in
      expr_opt (env, stmts) t None
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
    when def_expr_evaluates_to_value env.lang ->
      let (env, stmts), () = type_opt (env, stmts) opt_ty in
      (* We may end up here due to Elixir_to_elixir's parsing. Other languages
       * such as Ruby, Julia, and C seem to result in Assignments, not DefStmts.
       *)
      let (env, stmts), e = expr (env, stmts) e in
      let (env, stmts), lv = lval_of_ent (env, stmts) ent in
      let env, stmts =
        mk_i (Assign (lv, e)) (Related (G.S st)) |> add_instr (env, stmts)
      in
      ((env, stmts), mk_e (Fetch lv) (related_exp (G.e (G.StmtExpr st))))
  | __else__ ->
      (* In any case, let's make sure the statement is in the IL translation
       * so that e.g. taint can do its job. *)
      let (env, stmts), new_stmts = stmt (env, stmts) st in
      let env, stmts = add_stmts (env, stmts) new_stmts in
      ((env, stmts), todo stmts)

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)
and lval_of_ent (env, stmts) ent =
  match ent.G.name with
  | G.EN (G.Id (id, idinfo)) ->
      ((env, stmts), lval_of_id_info (env, stmts) id idinfo)
  | G.EN name -> lval (env, stmts) (G.N name |> G.e)
  | G.EDynamic eorig -> lval (env, stmts) eorig
  | G.EPattern (PatId (id, id_info)) ->
      lval (env, stmts) (G.N (Id (id, id_info)) |> G.e)
  | G.EPattern _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = AST_generic_helpers.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> ((env, stmts), fresh_lval (env, stmts) x))
  | G.OtherEntity _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = AST_generic_helpers.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> ((env, stmts), fresh_lval (env, stmts) x))

and expr_with_pre_stmts (env, stmts) ?void e =
  with_pre_stmts (env, stmts) (fun (env, stmts) -> expr (env, stmts) ?void e)

and stmt_expr_with_pre_stmts (env, stmts) st =
  with_pre_stmts (env, stmts) (fun (env, stmts) -> stmt_expr (env, stmts) st)

(* alt: could use H.cond_to_expr and reuse expr_with_pre_stmts *)
and cond_with_pre_stmts (env, stmts) cond =
  with_pre_stmts (env, stmts) (fun (env, stmts) ->
      match cond with
      | G.Cond e -> expr (env, stmts) e
      | G.OtherCond
          ( todok,
            [
              (Def (ent, VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
               as def);
            ] ) ->
          let (env, stmts), _ = type_opt (env, stmts) opt_ty in
          (* e.g. C/C++: `if (const char *tainted_or_null = source("PATH"))` *)
          let (env, stmts), e' = expr (env, stmts) e in
          let (env, stmts), lv = lval_of_ent (env, stmts) ent in
          let env, stmts =
            add_instr (env, stmts) (mk_i (Assign (lv, e')) (Related def))
          in
          ((env, stmts), mk_e (Fetch lv) (Related (G.TodoK todok)))
      | G.OtherCond (categ, xs) ->
          let e = G.OtherExpr (categ, xs) |> G.e in
          log_fixme ToDo (G.E e);
          expr (env, stmts) e)

and arg_with_pre_stmts (env, stmts) arg =
  with_pre_stmts (env, stmts) (fun (env, stmts) -> argument (env, stmts) arg)

and args_with_pre_stmts (env, stmts) args =
  with_pre_stmts (env, stmts) (fun (env, stmts) -> arguments (env, stmts) args)

and expr_with_pre_stmts_opt (env, stmts) tok eopt =
  match eopt with
  | None ->
      let (env, stmts), expression = expr_opt (env, stmts) tok None in
      ((env, stmts), [], expression)
  | Some e -> expr_with_pre_stmts (env, stmts) e

and for_var_or_expr_list (env, stmts) xs =
  let (env, stmts), list_of_lists =
    List.fold_left_map
      (fun (env, stmts) x ->
        match x with
        | G.ForInitExpr e ->
            let (env, stmts), ss, _eIGNORE =
              expr_with_pre_stmts (env, stmts) e
            in
            ((env, stmts), ss)
        | G.ForInitVar (ent, vardef) -> (
            (* copy paste of VarDef case in stmt *)
            match vardef with
            | { G.vinit = Some e; vtype = opt_ty; vtok = _ } ->
                let (env, stmts), ss1, e' =
                  expr_with_pre_stmts (env, stmts) e
                in
                let (env, stmts), ss2 =
                  type_opt_with_pre_stmts (env, stmts) opt_ty
                in
                let (env, stmts), lv = lval_of_ent (env, stmts) ent in
                ( (env, stmts),
                  ss1 @ ss2
                  @ [
                      mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.En ent))));
                    ] )
            | _ -> ((env, stmts), [])))
      (env, stmts) xs
  in
  ( (env, stmts),
    List.concat list_of_lists (*TODO this is not tail recursive!!!!*) )

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)
and parameters (_env, _stmts) params : param list =
  params |> Tok.unbracket
  |> List_.map (function
       | G.Param { pname = Some i; pinfo; pdefault; _ } ->
           Param { pname = var_of_id_info i pinfo; pdefault }
       | G.ParamPattern pat -> PatternParam pat
       | G.Param { pname = None; _ }
       | G.ParamRest (_, _)
       | G.ParamHashSplat (_, _)
       | G.ParamEllipsis _
       | G.ParamReceiver _
       | G.OtherParam (_, _) ->
           FixmeParam (* TODO *))

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

and type_ (env, stmts) (ty : G.type_) : (env * stmts) * G.type_ =
  (* Expressions inside types also need to be analyzed.
   *
   * E.g., in C we need to be able to do const prop here:
   *
   *     int x = 3;
   *     int arr[x]; // should match 'int arr[3]'
   *)
  let (env, stmts), exps =
    match ty.t with
    | G.TyArray ((_, Some e, _), _)
    | G.TyExpr e ->
        let (env, stmts), expression = expr (env, stmts) e in
        ((env, stmts), [ expression ])
    | __TODO__ -> ((env, stmts), [])
  in
  let tok = G.fake "type" in
  let (env, stmts), _ =
    List.fold_left_map
      (fun (env, stmts) e ->
        let (env, stmts), x, y =
          mk_aux_var ~force:true ~str:"_type" (env, stmts) tok e
        in
        ((env, stmts), (x, y)))
      (env, stmts) exps
  in
  ((env, stmts), ty)

and type_with_pre_stmts (env, stmts) ty =
  with_pre_stmts (env, stmts) (fun (env, stmts) -> type_ (env, stmts) ty)

and type_opt (env, stmts) opt_ty : (env * stmts) * unit =
  Option.fold
    ~none:((env, stmts), ())
    ~some:(fun ty ->
      let (env, stmts), _ = type_ (env, stmts) ty in
      ((env, stmts), ()))
    opt_ty

and type_opt_with_pre_stmts (env, stmts) opt_ty =
  let (env, stmts), ss, () =
    with_pre_stmts (env, stmts) (fun (env, stmts) ->
        type_opt (env, stmts) opt_ty)
  in
  ((env, stmts), ss)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* NOTE: There should not be direct calls to 'expr' from here on, instead
 * use 'expr_with_pre_stmts' or other '*_pre_stmts*' functions. Just so that
 * we don't forget about '(env, stmts).stmts'! *)

and no_switch_fallthrough : Lang.t -> bool = function
  | Go
  | Ruby
  | Rust ->
      true
  | _ -> false

and mk_break_continue_labels (env, stmts) tok =
  let cont_label = fresh_label ~label:"__loop_continue" (env, stmts) tok in
  let break_label = fresh_label ~label:"__loop_break" (env, stmts) tok in
  let st_env =
    {
      env with
      break_labels = break_label :: env.break_labels;
      cont_label = Some cont_label;
    }
  in
  let cont_label_s = [ mk_s (Label cont_label) ] in
  let break_label_s = [ mk_s (Label break_label) ] in
  (cont_label_s, break_label_s, (st_env, stmts))

and mk_switch_break_label (env, stmts) tok =
  let break_label = fresh_label ~label:"__switch_break" (env, stmts) tok in
  let switch_env =
    { env with break_labels = break_label :: env.break_labels }
  in
  (break_label, [ mk_s (Label break_label) ], (switch_env, stmts))

and implicit_return (env, stmts) eorig tok =
  (* We always expect a value from an expression that is implicitly
   * returned, so void is set to false here.
   *)
  let (env, stmts), ss, e =
    expr_with_pre_stmts ~void:false (env, stmts) eorig
  in
  let ret = mk_s (Return (tok, e)) in
  ((env, stmts), ss @ [ ret ])

and expr_stmt (env, stmts) (eorig : G.expr) tok : (env * stmts) * IL.stmt list =
  (* optimize? pass context to expr when no need for return value? *)
  let (env, stmts), ss, e = expr_with_pre_stmts ~void:true (env, stmts) eorig in

  (* Some expressions may return unit, and if we call mk_aux_var below, not only
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
  let env, stmts =
    match e.e with
    | Literal (G.Unit _) -> (env, stmts)
    | _else_ ->
        let (env, stmts), _, _ = mk_aux_var (env, stmts) tok e in
        (env, stmts)
  in

  let (env, stmts), ss' = pop_stmts (env, stmts) in

  match ss @ ss' with
  | [] ->
      (* This case may happen when we have a function like
       *
       *   function some_function(some_var) {
       *     some_var
       *   }
       *
       * the `some_var` will not show up in the CFG. Neither expr_with_pre_stmts
       * nor mk_aux_var will cause nodes to be created.
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
      let var = fresh_var (env, stmts) tok in
      let lval = lval_of_base (Var var) in
      let fake_i = mk_i (Assign (lval, e)) NoOrig in
      ((env, stmts), [ mk_s (Instr fake_i) ])
  | ss'' -> ((env, stmts), ss'')

and mk_class_construction (env, stmts) obj origin_exp ty cons_id_info args :
    (env * stmts) * lval * stmt list =
  (* We encode `obj = new T(args)` as `obj = new obj.T(args)` so that taint
     analysis knows that the reciever when calling `T` is the variable
     `obj`. It's kinda hacky but works for now. *)
  let lval = lval_of_base (Var obj) in
  let (env, stmts), ss1, args' =
    args_with_pre_stmts (env, stmts) (Tok.unbracket args)
  in
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
  let (env, stmts), ss2, ty = type_with_pre_stmts (env, stmts) ty in
  ( (env, stmts),
    lval,
    ss1 @ ss2
    @ [
        mk_s
          (Instr (mk_i (New (lval, ty, opt_cons, args')) (SameAs origin_exp)));
      ] )

and stmt_aux (env, stmts) st =
  match st.G.s with
  | G.ExprStmt (eorig, tok) -> (
      match eorig with
      | { is_implicit_return = true; _ } ->
          implicit_return (env, stmts) eorig tok
      (* Python's yield statement functions similarly to a return
         statement but with the added capability of saving the
         function's state. While this analogy isn't entirely precise,
         we currently treat it as a return statement for simplicity's
         sake. *)
      | { e = Yield (_, Some e, _); _ } when env.lang =*= Lang.Python ->
          implicit_return (env, stmts) e tok
      | _ -> expr_stmt (env, stmts) eorig tok)
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
      let (env, stmts), _, new_stmts =
        mk_class_construction (env, stmts) obj' new_exp ty cons_id_info args
      in
      ((env, stmts), new_stmts)
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ }) ->
      let (env, stmts), ss1, e' = expr_with_pre_stmts (env, stmts) e in
      let (env, stmts), lv = lval_of_ent (env, stmts) ent in
      let (env, stmts), ss2 = type_opt_with_pre_stmts (env, stmts) opt_ty in
      ( (env, stmts),
        ss1 @ ss2 @ [ mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.S st)))) ]
      )
  | G.DefStmt (_ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ }) ->
      (* We want to analyze any expressions in 'ty'. *)
      let (env, stmts), ss, _ = type_with_pre_stmts (env, stmts) ty in
      ((env, stmts), ss)
  | G.DefStmt def -> ((env, stmts), [ mk_s (MiscStmt (DefStmt def)) ])
  | G.DirectiveStmt dir ->
      ((env, stmts), [ mk_s (MiscStmt (DirectiveStmt dir)) ])
  | G.Block xs ->
      let xs = xs |> Tok.unbracket in
      let (env, stmts), list_of_lists =
        List.fold_left_map stmt (env, stmts) xs
      in
      ((env, stmts), List.concat list_of_lists)
  | G.If (tok, cond, st1, st2) ->
      let (env, stmts), ss, e' = cond_with_pre_stmts (env, stmts) cond in
      let (env, stmts), st1 = stmt (env, stmts) st1 in
      let (env, stmts), list_of_lists =
        List.fold_left_map stmt (env, stmts) (Option.to_list st2)
      in
      let st2 = List.concat list_of_lists in
      ((env, stmts), ss @ [ mk_s (If (tok, e', st1, st2)) ])
  | G.Switch (tok, switch_expr_opt, cases_and_bodies) ->
      let (env, stmts), ss, translate_cases =
        match switch_expr_opt with
        | Some switch_expr ->
            let (env, stmts), ss, switch_expr' =
              cond_with_pre_stmts (env, stmts) switch_expr
            in
            ( (env, stmts),
              ss,
              switch_expr_and_cases_to_exp tok
                (H.cond_to_expr switch_expr)
                switch_expr' )
        | None -> ((env, stmts), [], cases_to_exp tok)
      in
      let break_label, break_label_s, (switch_env, switch_stmts) =
        mk_switch_break_label (env, stmts) tok
      in

      let (env, stmts), jumps, bodies =
        cases_and_bodies_to_stmts (switch_env, switch_stmts) tok break_label
          translate_cases cases_and_bodies
      in

      ((env, stmts), ss @ jumps @ bodies @ break_label_s)
  | G.While (tok, e, st) ->
      let cont_label_s, break_label_s, (st_env, st_stmts) =
        mk_break_continue_labels (env, stmts) tok
      in
      let (env, stmts), ss, e' = cond_with_pre_stmts (env, stmts) e in
      let _, st = stmt (st_env, st_stmts) st in
      ( (env, stmts),
        ss @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ] @ break_label_s
      )
  | G.DoWhile (tok, st, e) ->
      let cont_label_s, break_label_s, (st_env, st_stmts) =
        mk_break_continue_labels (env, stmts) tok
      in
      let (env, stmts), st = stmt (st_env, st_stmts) st in
      let (env, stmts), ss, e' = expr_with_pre_stmts (env, stmts) e in
      ( (env, stmts),
        st @ ss
        @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ]
        @ break_label_s )
  | G.For (tok, G.ForEach (pat, tok2, e), st) ->
      for_each (env, stmts) tok (pat, tok2, e) st
  | G.For (_, G.MultiForEach [], st) -> stmt (env, stmts) st
  | G.For (_, G.MultiForEach (FEllipsis _ :: _), _) ->
      sgrep_construct stmts (G.S st)
  | G.For (tok, G.MultiForEach (FECond (fr, tok2, e) :: for_eachs), st) ->
      let loop = G.For (tok, G.MultiForEach for_eachs, st) |> G.s in
      let st = G.If (tok2, Cond e, loop, None) |> G.s in
      for_each (env, stmts) tok fr st
  | G.For (tok, G.MultiForEach (FE fr :: for_eachs), st) ->
      for_each (env, stmts) tok fr
        (G.For (tok, G.MultiForEach for_eachs, st) |> G.s)
  | G.For (tok, G.ForClassic (xs, eopt1, eopt2), st) ->
      let cont_label_s, break_label_s, (st_env, st_stmts) =
        mk_break_continue_labels (env, stmts) tok
      in
      let (env, stmts), ss1 = for_var_or_expr_list (env, stmts) xs in
      let _, st = stmt (st_env, st_stmts) st in
      let (env, stmts), ss2, cond =
        match eopt1 with
        | None ->
            let vtrue = G.Bool (true, tok) in
            ((env, stmts), [], mk_e (Literal vtrue) (related_tok tok))
        | Some e -> expr_with_pre_stmts (env, stmts) e
      in
      let (env, stmts), next =
        match eopt2 with
        | None -> ((env, stmts), [])
        | Some e ->
            let (env, stmts), ss, _eIGNORE =
              expr_with_pre_stmts (env, stmts) e
            in
            ((env, stmts), ss)
      in
      ( (env, stmts),
        ss1 @ ss2
        @ [ mk_s (Loop (tok, cond, st @ cont_label_s @ next @ ss2)) ]
        @ break_label_s )
  | G.For (_, G.ForEllipsis _, _) -> sgrep_construct stmts (G.S st)
  (* TODO: repeat (env, stmts) work of controlflow_build.ml *)
  | G.Continue (tok, lbl_ident, _) -> (
      match lbl_ident with
      | G.LNone -> (
          match env.cont_label with
          | None -> impossible stmts (G.Tk tok)
          | Some lbl -> ((env, stmts), [ mk_s (Goto (tok, lbl)) ]))
      | G.LId lbl ->
          ((env, stmts), [ mk_s (Goto (tok, label_of_label (env, stmts) lbl)) ])
      | G.LInt _
      | G.LDynamic _ ->
          todo stmts (G.S st))
  | G.Break (tok, lbl_ident, _) -> (
      match lbl_ident with
      | G.LNone -> (
          match env.break_labels with
          | [] -> impossible stmts (G.Tk tok)
          | lbl :: _ -> ((env, stmts), [ mk_s (Goto (tok, lbl)) ]))
      | G.LId lbl ->
          ((env, stmts), [ mk_s (Goto (tok, label_of_label (env, stmts) lbl)) ])
      | G.LInt (i, _) -> (
          match List.nth_opt env.break_labels i with
          | None -> impossible stmts (G.Tk tok)
          | Some lbl -> ((env, stmts), [ mk_s (Goto (tok, lbl)) ]))
      | G.LDynamic _ -> impossible stmts (G.Tk tok))
  | G.Label (lbl, st) ->
      let lbl = label_of_label (env, stmts) lbl in
      let (env, stmts), st = stmt (env, stmts) st in
      ((env, stmts), [ mk_s (Label lbl) ] @ st)
  | G.Goto (tok, lbl, _sc) ->
      let lbl = lookup_label (env, stmts) lbl in
      ((env, stmts), [ mk_s (Goto (tok, lbl)) ])
  | G.Return (tok, eopt, _) ->
      let (env, stmts), ss, e = expr_with_pre_stmts_opt (env, stmts) tok eopt in
      ((env, stmts), ss @ [ mk_s (Return (tok, e)) ])
  | G.Assert (tok, args, _) ->
      let (env, stmts), ss, args =
        args_with_pre_stmts (env, stmts) (Tok.unbracket args)
      in
      let special = (Assert, tok) in
      (* less: wrong e? would not be able to match on Assert, or
       * need add sorig:
       *)
      ( (env, stmts),
        ss
        @ [
            mk_s
              (Instr
                 (mk_i (CallSpecial (None, special, args)) (Related (G.S st))));
          ] )
  | G.Throw (tok, e, _) ->
      let (env, stmts), ss, e = expr_with_pre_stmts (env, stmts) e in
      ((env, stmts), ss @ [ mk_s (Throw (tok, e)) ])
  | G.OtherStmt (G.OS_ThrowNothing, [ G.Tk tok ]) ->
      (* Python's `raise` without arguments *)
      let eorig = related_tok tok in
      let todo_exp = fixme_exp ToDo (G.Tk tok) eorig in
      ((env, stmts), [ mk_s (Throw (tok, todo_exp)) ])
  | G.OtherStmt
      (G.OS_ThrowFrom, [ G.E from; G.S ({ s = G.Throw _; _ } as throw_stmt) ])
    ->
      (* Python's `raise E1 from E2` *)
      let todo_stmt = fixme_stmt ToDo (G.E from) in
      let (env, stmts), rest = stmt_aux (env, stmts) throw_stmt in

      ((env, stmts), todo_stmt @ rest)
  | G.Try (_tok, try_st, catches, opt_else, opt_finally) ->
      try_catch_else_finally (env, stmts) ~try_st ~catches ~opt_else
        ~opt_finally
  | G.WithUsingResource (_, stmt1, stmt2) ->
      let (env, stmts), stmt1 = List.fold_left_map stmt (env, stmts) stmt1 in
      let stmt1 = List.concat stmt1 in
      let (env, stmts), stmt2 = stmt (env, stmts) stmt2 in
      ((env, stmts), stmt1 @ stmt2)
  | G.DisjStmt _ -> sgrep_construct stmts (G.S st)
  | G.OtherStmtWithStmt (G.OSWS_With, [ G.E manager_as_pat ], body) ->
      let opt_pat, manager =
        (* Extract <manager> and <pat> from `with <manager> as <pat>`;
         * <manager> is an expression that evaluates to a context manager,
         * <pat> is optional. *)
        match manager_as_pat.G.e with
        | G.LetPattern (pat, manager) -> (Some pat, manager)
        | _ -> (None, manager_as_pat)
      in
      python_with_stmt (env, stmts) manager opt_pat body
  (* Java: synchronized (E) S *)
  | G.OtherStmtWithStmt (G.OSWS_Block _, [ G.E objorig ], stmt1) ->
      (* TODO: Restrict this to a syncrhonized block ? *)
      let (env, stmts), ss, _TODO_obj =
        expr_with_pre_stmts (env, stmts) objorig
      in
      let (env, stmts), new_stmts = stmt (env, stmts) stmt1 in
      ((env, stmts), ss @ new_stmts)
  (* Rust: unsafe block *)
  | G.OtherStmtWithStmt (G.OSWS_Block ("Unsafe", tok), [], stmt1) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("unsafe_block", tok)) in
      let (env, stmts), new_stmts = stmt (env, stmts) stmt1 in
      ((env, stmts), todo_stmt @ new_stmts)
  | G.OtherStmt (OS_Async, [ G.S stmt1 ]) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("async", G.fake "async")) in
      let (env, stmts), new_stmts = stmt (env, stmts) stmt1 in
      ((env, stmts), todo_stmt @ new_stmts)
  | G.OtherStmt _
  | G.OtherStmtWithStmt _ ->
      todo stmts (G.S st)
  | G.RawStmt _ -> todo stmts (G.S st)

and for_each (env, stmts) tok (pat, tok2, e) st =
  let cont_label_s, break_label_s, (st_env, st_stmts) =
    mk_break_continue_labels (env, stmts) tok
  in
  let (env, stmts), ss, e' = expr_with_pre_stmts (env, stmts) e in
  let _, st = stmt (st_env, st_stmts) st in

  let next_lval = fresh_lval (env, stmts) tok2 in
  let hasnext_lval = fresh_lval (env, stmts) tok2 in
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
  let (env, stmts), assign_st =
    pattern_assign_statements (env, stmts)
      (mk_e (Fetch next_lval) (related_tok tok2))
      ~eorig:(related_tok tok2) pat
  in
  let cond = mk_e (Fetch hasnext_lval) (related_tok tok2) in

  ( (env, stmts),
    (ss @ [ hasnext_call ])
    @ [
        mk_s
          (Loop
             ( tok,
               cond,
               [ next_call ] @ assign_st @ st @ cont_label_s
               @ [ (* ss @ ?*) hasnext_call ] ));
      ]
    @ break_label_s )

(* TODO: Maybe this and the following function could be merged *)
and switch_expr_and_cases_to_exp tok switch_expr_orig switch_expr (env, stmts)
    cases =
  (* If there is a scrutinee, the cases are expressions we need to check for equality with the scrutinee  *)
  let (env, stmts), ss, es =
    List.fold_left
      (fun ((env, stmts), ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            ( (env, stmts),
              ss,
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
            let (env, stmts), c_ss, c' = expr_with_pre_stmts (env, stmts) c in
            ( (env, stmts),
              ss @ c_ss,
              {
                e = Operator ((G.Eq, tok), [ Unnamed c'; Unnamed switch_expr ]);
                eorig = related_tok tok;
              }
              :: es )
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible stmts (G.Tk tok)
        | G.Case (tok, _) ->
            ((env, stmts), ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es)
        | G.OtherCase ((_todo_categ, tok), _any) ->
            ((env, stmts), ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es))
      ((env, stmts), [], [])
      cases
  in
  ( (env, stmts),
    ss,
    {
      e = Operator ((Or, tok), mk_unnamed_args es);
      eorig = SameAs switch_expr_orig;
    } )

and cases_to_exp tok (env, stmts) cases =
  (* If we have no scrutinee, the cases are boolean expressions, so we Or them together *)
  let (env, stmts), ss, es =
    List.fold_left
      (fun ((env, stmts), ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            ((env, stmts), ss, { e = Literal l; eorig = related_tok tok } :: es)
        | G.Case (_, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (_, c) ->
            let (env, stmts), c_ss, c' = expr_with_pre_stmts (env, stmts) c in
            ((env, stmts), ss @ c_ss, c' :: es)
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible stmts (G.Tk tok)
        | G.Case (tok, _) ->
            ((env, stmts), ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es)
        | G.OtherCase ((_, tok), _) ->
            ((env, stmts), ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es))
      ((env, stmts), [], [])
      cases
  in
  ( (env, stmts),
    ss,
    { e = Operator ((Or, tok), mk_unnamed_args es); eorig = related_tok tok } )

and cases_and_bodies_to_stmts (env, stmts) tok break_label translate_cases =
  function
  | [] -> ((env, stmts), [ mk_s (Goto (tok, break_label)) ], [])
  | G.CaseEllipsis tok :: _ -> sgrep_construct stmts (G.Tk tok)
  | [ G.CasesAndBody ([ G.Default dtok ], body) ] ->
      let label = fresh_label ~label:"__switch_default" (env, stmts) tok in

      let (env, stmts), new_stmts = stmt (env, stmts) body in
      ( (env, stmts),
        [ mk_s (Goto (dtok, label)) ],
        mk_s (Label label) :: new_stmts )
  | G.CasesAndBody (cases, body) :: xs ->
      let (env, stmts), jumps, bodies =
        cases_and_bodies_to_stmts (env, stmts) tok break_label translate_cases
          xs (* TODO this is not tail recursive *)
      in
      let label = fresh_label ~label:"__switch_case" (env, stmts) tok in
      let (env, stmts), case_ss, case = translate_cases (env, stmts) cases in
      let jump =
        mk_s (IL.If (tok, case, [ mk_s (Goto (tok, label)) ], jumps))
      in
      let (env, stmts), new_stmts = stmt (env, stmts) body in

      let body = mk_s (Label label) :: new_stmts in
      let break_if_no_fallthrough =
        if no_switch_fallthrough env.lang then
          [ mk_s (Goto (tok, break_label)) ]
        else []
      in
      ((env, stmts), case_ss @ [ jump ], body @ break_if_no_fallthrough @ bodies)

and stmt (env, stmts) st : (env * stmts) * stmt list =
  try stmt_aux (env, stmts) st with
  | Fixme (stmts, kind, any_generic) ->
      ((env, stmts), fixme_stmt kind any_generic)

and function_body (env, stmts) fbody =
  let body_stmt = H.funcbody_to_stmt fbody in
  stmt (env, stmts) body_stmt

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
and python_with_stmt (env, stmts) manager opt_pat body =
  (* mgr = MANAGER *)
  let mgr = fresh_lval (env, stmts) G.sc in
  let (env, stmts), ss_def_mgr =
    let (env, stmts), ss_mk_mgr, manager' =
      expr_with_pre_stmts (env, stmts) manager
    in
    ( (env, stmts),
      ss_mk_mgr @ [ mk_s (Instr (mk_i (Assign (mgr, manager')) NoOrig)) ] )
  in
  (* PAT = mgr *)
  let (env, stmts), ss_def_pat =
    match opt_pat with
    | None -> ((env, stmts), [])
    | Some pat ->
        pattern_assign_statements (env, stmts) (mk_e (Fetch mgr) NoOrig)
          ~eorig:NoOrig pat
  in
  let (env, stmts), new_stmts = stmt (env, stmts) body in
  ((env, stmts), ss_def_mgr @ ss_def_pat @ new_stmts)
(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

and function_definition (env, stmts) fdef =
  let fparams = parameters (env, stmts) fdef.G.fparams in
  let (env, stmts), fbody = function_body (env, stmts) fdef.G.fbody in
  ( (env, stmts),
    { fkind = fdef.fkind; fparams; frettype = fdef.G.frettype; fbody } )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let function_definition lang ?ctx fdef =
  let env, stmts = ({ (empty_env lang) with ctx = ctx ||| empty_ctx }, []) in
  let _, fd = function_definition (env, stmts) fdef in
  fd

let stmt lang st =
  let env, stmts = (empty_env lang, []) in
  let _, stmts = stmt (env, stmts) st in
  stmts

let expr lang e =
  let env, stmts = (empty_env lang, []) in
  let _, e = expr (env, stmts) e in
  e
