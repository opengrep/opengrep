(* Yoann Padioleau
 *
 * Copyright (c) 2022-2023 Semgrep Inc.
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
open Either_
open AST_elixir
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_elixir to generic AST conversion
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = Program | Pattern

(* TODO: use this behavior also in AST_generic.exprstmt? breaking tests? *)
let exprstmt (e : G.expr) : G.stmt =
  match e.e with
  | StmtExpr st -> st
  | _else -> G.exprstmt e

let fb = Tok.unsafe_fake_bracket
let map_string _env x = x
let map_char _env x = x
let map_list f env xs = List_.map (f env) xs
let map_option f env x = Option.map (f env) x

let either_to_either3 = function
  | Left x -> Left3 x
  (* Use Middle3 (not Right3) so that G.interpolated emits the expression
   * directly as Arg(e) rather than wrapping it in Call(InterpolatedElement,...).
   * This keeps the ConcatString arg list flat: literal string parts and
   * interpolated expressions appear at the same level, which is what both
   * pattern matching (m_arguments_concat) and taint analysis expect.
   * Note: AST_to_IL already unwraps InterpolatedElement anyway. *)
  | Right (_l, x, _r) -> Middle3 x

exception UnhandledIdEllipsis

type quoted_generic = (string G.wrap, G.expr G.bracket) Either_.t list G.bracket
type keywords_generic =
  (((G.ident, quoted_generic) Either_.t * G.expr), G.expr (* ... *)) Either_.t list

type stab_clause_generic =
  (G.argument list * (Tok.t * G.expr) option) * Tok.t * G.stmt list

type body_or_clauses_generic = (G.stmt list, stab_clause_generic list) Either_.t

type do_block_generic =
  (body_or_clauses_generic
  * (exn_clause_kind wrap * body_or_clauses_generic) list)
  bracket

(* Merge consecutive Left (literal) entries so that escape sequences like \'
 * don't fragment a single text segment into multiple concat args.
 * e.g. [Left("foo=", t); Left("\\'", t)] → [Left("foo=\\'", t)] *)
let merge_adjacent_lefts xs =
  let rev_merged =
    List.fold_left
      (fun acc x ->
        match (x, acc) with
        | Left3 (s2, _), Left3 (s1, tok1) :: rest -> Left3 (s1 ^ s2, tok1) :: rest
        | _ -> x :: acc)
      [] xs
  in
  List.rev rev_merged

let expr_of_quoted (quoted : quoted_generic) : G.expr =
  let l, xs, r = quoted in
  G.interpolated (l, xs |> List_.map either_to_either3 |> merge_adjacent_lefts, r)

let keyval_of_pair p : G.expr =
  match p with
  | Left (kwd, e) ->
    let key =
      match kwd with
      | Left id -> G.N (H.name_of_id id) |> G.e
      | Right (quoted : quoted_generic) -> expr_of_quoted quoted
    in
    G.keyval key (G.fake "=>") e
  (* TODO: How about ellipsis-metavariables `$...ARG`? *)
  | Right expr -> expr (* ... *)

let argument_of_pair p =
  match p with
  | Left (kwd, e) ->
    begin match kwd with
      | Left id -> G.ArgKwd (id, e)
      | Right (quoted : quoted_generic) ->
          let l, _, _ = quoted in
          let e = expr_of_quoted quoted in
          OtherArg (("ArgKwdQuoted", l), [ G.E e ])
    end
  (* TODO: How about ellipsis-metavariables `$...ARG`? *)
  | Right expr -> G.Arg expr (* ... *)

let list_container_of_kwds (kwds : keywords_generic) : G.expr =
  G.Container (G.List, fb (kwds |> List_.map keyval_of_pair)) |> G.e

let expr_of_expr_or_kwds (x : (G.expr, keywords_generic) Either_.t) : G.expr =
  match x with
  | Left e -> e
  | Right kwds -> list_container_of_kwds kwds

(* This is a modified version of Ast_generic_helpers.expr_to_pattern *)
let rec expr_to_pattern (e : G.expr) : G.pattern =
  match e.e with
  | G.N (G.Id (id, info)) -> G.PatId (id, info)
  | G.Container (G.Tuple, (t1, xs, t2)) ->
      G.PatTuple (t1, List_.map expr_to_pattern xs, t2)
  | G.L l -> G.PatLiteral l
  | G.Container ((List | Dict), (t1, xs, t2)) ->
      G.PatList (t1, List_.map expr_to_pattern xs, t2)
  | G.Constructor (n, (_, args, _)) ->
      G.PatConstructor (n, List_.map expr_to_pattern args)
  | G.Ellipsis t -> G.PatEllipsis t
  | G.OtherExpr (tag, [ G.E e ]) -> G.OtherPat (tag, [ G.P (expr_to_pattern e) ])
  | G.Cast (ty, _tok, expr) -> G.PatTyped (expr_to_pattern expr, ty)
  | G.LetPattern (p, {e = G.N (G.Id (i, info)); _} ) -> G.PatAs (p, (i, info))
  | G.Call (f, args) ->
      begin match f.e, Tok.unbracket args with
      | G.N (G.Id (("<>", _), _) as n),
        [ G.Arg ({ e = G.L (G.String _); _ } as l);
          G.Arg ({ e = G.N _; _ } as r) ] ->
          G.PatConstructor (n, [ expr_to_pattern l; expr_to_pattern r ])
      | G.N (G.Id (("^", _), _)),
        [ G.Arg ({ e = G.N _; _ } as rhs) ] ->
          let tmp = "__tmp", Tok.unsafe_fake_tok "__tmp" in
          let tmp_info = G.empty_id_info ~hidden:true () in
          let lhs = G.N (G.Id (tmp, tmp_info)) |> G.e in
          let op = G.IdSpecial (G.Op G.Eq, Tok.unsafe_fake_tok "==") |> G.e in
          let cmp = G.Call (op, Tok.unsafe_fake_bracket [ G.Arg lhs; G.Arg rhs ]) |> G.e in
          G.PatWhen (G.PatId (tmp, tmp_info), cmp)
      | _ -> OtherPat (("ExprToPattern", Tok.unsafe_fake_tok ""), [ G.E e ])
      end
  (* TODO: PatKeyVal and more *)
  | _ -> OtherPat (("ExprToPattern", Tok.unsafe_fake_tok ""), [ G.E e ])

let pats_of_args (args : G.argument list) : G.pattern list =
  List_.map
    (function
      | G.OtherArg (("ArgKwdQuoted", _), [ G.E e ]) -> expr_to_pattern e
      | arg -> H.argument_to_expr arg |> expr_to_pattern)
    args

let wrap_when (when_opt : (Tok.t * G.expr) option) (pat : G.pattern) : G.pattern =
  match when_opt with
  | None -> pat
  | Some (_tok, e) -> G.PatWhen (pat, e)

(* TODO: lots of work here to detect when args is really a single
 * pattern, or tuples *)
let pat_of_args_and_when (args, when_opt) : G.pattern =
  (* let rest =
       match when_opt with
       | None -> []
       | Some (_tok, e) -> [ G.E e ]
     in *)
  let pat =
    match pats_of_args args with
    | [] -> G.PatLiteral (G.Null (G.fake "no_arg")) (* invalid syntax anyway. *)
    | pats -> G.PatTuple (fb pats)
  in
  (* G.OtherPat (("ArgsAndWhenOpt", G.fake ""), G.Args args :: rest) |> G.p *)
  wrap_when when_opt pat

let case_and_body_of_stab_clause (x : stab_clause_generic) : G.case_and_body =
  (* body can be empty *)
  let args_and_when, _tarrow, stmts = x in
  let pat = pat_of_args_and_when args_and_when in
  let stmt = G.stmt1 stmts in
  G.case_of_pat_and_stmt (pat, stmt)

let case_and_body_of_case_clause (x : stab_clause_generic) : G.case_and_body =
  let (args, when_opt), _tarrow, stmts = x in
  let pat =
    match pats_of_args args with
    | [] -> G.PatLiteral (G.Null (G.fake "no_arg"))
    | [ single ] -> single
    | pats -> G.PatTuple (fb pats)
  in
  let pat = wrap_when when_opt pat in
  G.case_of_pat_and_stmt (pat, G.stmt1 stmts)

(* TODO: if the list contains just one element, can be a simple lambda
 * as in 'fn (x, y) -> x + y end'. Otherwise it can be a multiple-cases
 * switch/match.
 * The first tk parameter corresponds to 'fn' for lambdas and 'do' when
 * used in a do_block.
 *)
let stab_clauses_to_function_definition tk (xs : stab_clause_generic list) :
    G.function_definition =
  (* mostly a copy-paste of code to handle Function in ml_to_generic *)
  let xs = xs |> List_.map case_and_body_of_stab_clause in
  let id = G.implicit_param_id tk in
  let params = [ G.Param (G.param_of_id id) ] in
  let body_stmt =
    G.Switch (tk, Some (G.Cond (G.N (H.name_of_id id) |> G.e)), xs) |> G.s
  in
  {
    G.fparams = fb params;
    frettype = None;
    fkind = (G.Function, tk);
    fbody = G.FBStmt body_stmt;
  }

let expr_of_body_or_clauses tk (x : body_or_clauses_generic) : G.expr =
  match x with
  | Left stmts ->
      (* less: use G.stmt1 instead? or get rid of fake_bracket here
       * passed down from caller? *)
      let block = G.Block (fb stmts) |> G.s in
      G.stmt_to_expr block
  | Right clauses ->
      let fdef = stab_clauses_to_function_definition tk clauses in
      G.Lambda fdef |> G.e

(* following Elixir semantic (unsugaring do/end block in keywords) *)
let kwds_of_do_block (bl : do_block_generic) : keywords_generic =
  let tdo, (body_or_clauses, extras), _tend = bl in
  (* In theory we should unsugar as "do:", and below for the
   * other kwds as "rescue:" for example, which is Elixir unsugaring semantic,
   * but then this does not play well with Analyze_pattern.ml
   * so simpler for now to unsugar as the actual keyword string without
   * the ':' suffix.
   *)
  let dokwd = Left ("do", tdo) in
  let e = expr_of_body_or_clauses tdo body_or_clauses in
  let pair1 = Left (dokwd, e) in
  let rest =
    extras
    |> List_.map (fun ((kind, t), body_or_clauses) ->
           let s = string_of_exn_kind kind in
           let kwd = Left (s, t) in
           let e = expr_of_body_or_clauses t body_or_clauses in
           Left (kwd, e))
  in
  pair1 :: rest

let args_of_do_block_opt (blopt : do_block_generic option) : G.argument list =
  match blopt with
  | None -> []
  | Some bl ->
      let kwds = kwds_of_do_block bl in
      List_.map argument_of_pair kwds

(*****************************************************************************)
(* Short Lambda / Capture helpers *)
(*****************************************************************************)

(** Find the maximum placeholder number in a ShortLambda body.
    E.g., in &(foo(&1, &3)), returns 3. Recursively searches all subexpressions. *)
let find_max_placeholder (e : G.expr) : int =
  let max_found = ref 0 in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_expr env expr =
        (match expr.G.e with
        | G.OtherExpr
            (("PlaceHolder", _), [ G.E { e = G.L (G.Int (Some n, _)); _ } ]) ->
            max_found := max !max_found (Int64.to_int n)
        | _ -> super#visit_expr env expr)
    end
  in
  visitor#visit_expr () e;
  !max_found

(** Replace placeholder references with parameter names.
     Recursively replaces in all subexpressions. *)
let replace_placeholders (e : G.expr) : G.expr =
  let mapper =
    object
      inherit [_] AST_generic.map as super

      method! visit_expr env expr =
        match expr.G.e with
        | G.OtherExpr
            (("PlaceHolder", _), [ G.E { e = G.L (G.Int (Some n, tk)); _ } ]) ->
            let param_name = Printf.sprintf "&%Ld" n in
            let param_id = (param_name, tk) in
            G.N (G.Id (param_id, G.empty_id_info ())) |> G.e
        | _ -> super#visit_expr env expr
    end
  in
  mapper#visit_expr () e

(** Convert a ShortLambda/Capture body to OtherExpr with params and body.
    Creates params &1, &2, ... based on placeholders.
    Structure: OtherExpr("ShortLambda", [Params [...]; S body_stmt])
    This allows Naming_AST to create proper scope for the params. *)
let convert_short_lambda (tok : Tok.t) (body_expr : G.expr) : G.expr =
  let max_placeholder = find_max_placeholder body_expr in
  let replaced_body = replace_placeholders body_expr in
  let params =
    List.init max_placeholder (fun i ->
        let param_name = Printf.sprintf "&%d" (i + 1) in
        let param_id = (param_name, tok) in
        G.Param (G.param_of_id param_id))
  in
  let body_stmt = G.ExprStmt (replaced_body, G.sc) |> G.s in
  (* Structure that Naming_AST can recognize for scope handling *)
  G.OtherExpr (("ShortLambda", tok), [ G.Params params; G.S body_stmt ]) |> G.e

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

let map_wrap f env (v1, v2) = (f env v1, v2)
let map_bracket f env (v1, v2, v3) = (v1, f env v2, v3)

let map_ident_or_metavar_only_exn_on_ellipsis env v : G.ident =
  match v with
  | Id v ->
      let id = (map_wrap map_string) env v in
      id
  | IdEllipsis _ -> raise UnhandledIdEllipsis
  | IdMetavar v ->
      let id = (map_wrap map_string) env v in
      id

(* you should not use it because it's likely that if you
 * are in a pattern context, you want to interpret
 * IdEllipsis
 *)
let map_ident_should_not_use env v : G.ident =
  match v with
  | IdEllipsis v -> ("...", v)
  | Id _
  | IdMetavar _ ->
      map_ident_or_metavar_only_exn_on_ellipsis env v

(* TODO: this should really return a dotted_ident *)
let map_alias env v : G.ident = (map_wrap map_string) env v

let map_wrap_operator env (op, tk) =
  match op with
  | OPin
  | ODot
  | OMatch
  | OCapture
  | OType
  | OStrictAnd
  | OStrictOr
  | OStrictNot
  | OPipeline
  | OModuleAttr
  | OLeftArrow
  | ODefault
  | ORightArrow
  | OCons
  | OWhen ->
      Right (Tok.content_of_tok tk, tk)
  | O op -> Left (op, tk)
  | OOther v ->
      let v = map_string env v in
      Right (v, tk)

let map_wrap_operator_ident env v : G.ident =
  match map_wrap_operator env v with
  | Left (_op, tk) -> (Tok.content_of_tok tk, tk)
  | Right (s, tk) -> (s, tk)

(* start of big mutually recursive functions *)

let rec map_atom env (tcolon, v2) : G.expr =
  let v2 = (map_or_quoted1 (map_wrap map_string)) env v2 in
  match v2 with
  | Left x -> G.L (G.Atom (tcolon, x)) |> G.e
  | Right quoted ->
      let e = expr_of_quoted quoted in
      G.OtherExpr (("AtomExpr", tcolon), [ E e ]) |> G.e

(* TODO: maybe need a 'a. for all type *)
and map_or_quoted1 f env v =
  match v with
  | X1 v ->
      let v = f env v in
      Left (f env v)
  | Quoted1 v -> Right (map_quoted env v)

and map_keyword env (v1, _tcolon) = map_or_quoted1 (map_wrap map_string) env v1

and map_quoted env v : quoted_generic =
  map_bracket
    (map_list (fun env x ->
         match x with
         | Left x -> Left (map_wrap map_string env x)
         | Right x -> Right (map_bracket map_expr env x)))
    env v

and map_arguments env (v1, v2) : G.argument list =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  List_.map G.arg v1 @ List_.map argument_of_pair v2

and map_items env (v1, v2) : G.expr list =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  v1 @ List_.map keyval_of_pair v2

(* Like map_items but wraps each pair in OtherExpr to distinguish
 * arrow syntax (%{"k" => v}) from keyword syntax (%{k: v}). *)
and map_map_items env (v1, v2) : G.expr list =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  let wrap_arrow (e : G.expr) : G.expr =
    match e.G.e with
    | G.Ellipsis _ -> e
    | _ -> G.OtherExpr (("MapPairArrow", G.fake "=>"), [ G.E e ]) |> G.e
  in
  let wrap_keyword p : G.expr =
    match p with
    | Right e -> e (* ellipsis or metavar, pass through unwrapped *)
    | Left _ ->
        let e = keyval_of_pair p in
        G.OtherExpr (("MapPairKeyword", G.fake ":"), [ G.E e ]) |> G.e
  in
  List_.map wrap_arrow v1 @ List_.map wrap_keyword v2

and map_keywords env v = (map_list map_pair) env v

and map_pair_kw_expr env (v1, v2) =
  let v1 = map_keyword env v1 in
  let v2 = map_expr env v2 in
  Left (v1, v2)

and map_pair env p =
  match p with
  | Kw_expr (v1, v2) -> map_pair_kw_expr env (v1, v2)
  | Semg_ellipsis tok -> Right (map_expr env (I (IdEllipsis tok))) 

and map_expr_or_kwds env v : (G.expr, keywords_generic) Either_.t =
  match v with
  | E v ->
      let v = map_expr env v in
      Left v
  | Kwds v ->
      let v = map_keywords env v in
      Right v

and map_stmt env (v : stmt) : G.stmt =
  match v with
  | If (tif, cond, tdo, then_, elseopt, tend) ->
      let e = map_expr env cond in
      let then_ = map_stmts env then_ in
      let elseopt, tthenend =
        match elseopt with
        | None -> (None, tend)
        | Some (telse, else_) ->
            let else_ = map_stmts env else_ in
            let st2 = G.Block (telse, else_, tend) |> G.s in
            (* TODO? reusing telse is good here? better generate fake? *)
            (Some st2, telse)
      in
      let st1 = G.Block (tdo, then_, tthenend) |> G.s in
      G.If (tif, G.Cond e, st1, elseopt) |> G.s
  | Throw (tthrow, e) ->
      let e = map_expr env e in
      G.Throw (tthrow, e, G.sc) |> G.s
  | Case (tcase, e, (_tdo, clauses, _tend)) ->
      let e = map_expr env e in
      let xs = map_clauses env clauses in
      let cases = xs |> List_.map case_and_body_of_case_clause in
      G.Switch (tcase, Some (G.Cond e), cases) |> G.s
  | For (tfor, clauses, (tdo, body, tend)) ->
      let comp_clauses = List_.map (fun (clause : for_clause) ->
        match clause with
        | ForGenerator (pat, tarrow, collection) ->
            let pat = map_expr env pat |> expr_to_pattern in
            let collection = map_expr env collection in
            G.CompFor (tfor, pat, tarrow, collection)
        | ForFilter e ->
            let e = map_expr env e in
            G.CompIf (G.fake "if", e)
      ) clauses in
      let body_stmts = map_stmts env body in
      let body_expr =
        match body_stmts with
        | [ { G.s = G.ExprStmt (e, _sc); _ } ] -> e
        | _ -> G.stmt_to_expr (G.Block (tdo, body_stmts, tend) |> G.s)
      in
      let comp = G.Comprehension (G.List, (tdo, (body_expr, comp_clauses), tend)) in
      G.ExprStmt (comp |> G.e, G.sc) |> G.s
  | Try (ttry, (tdo, (boc, extras), tend)) ->
      let body_stmts =
        match boc with
        | Body stmts -> map_stmts env stmts
        | Clauses _ -> (* unreachable: try body is always Body stmts *) []
      in
      let body_stmt = G.Block (tdo, body_stmts, tend) |> G.s in
      wrap_with_rescue env ttry body_stmt extras
  | D def ->
      let d = map_definition env def in
      G.DefStmt d |> G.s

and map_param_as_arg env (p : parameter) : G.argument =
  match p with
  | P { pname; pdefault = None } ->
      let e =
        match pname with
        | IdEllipsis tok -> G.Ellipsis tok |> G.e
        | Id _ | IdMetavar _ ->
            let id = map_ident_should_not_use env pname in
            G.N (H.name_of_id id) |> G.e
      in
      G.Arg e
  | P { pname; pdefault = Some _ } ->
      let id = map_ident_should_not_use env pname in
      G.Arg (G.N (H.name_of_id id) |> G.e)
  | OtherParamExpr e -> G.Arg (map_expr env e)
  | OtherParamPair (kwd, e) ->
      let kwd = map_keyword env kwd in
      let e = map_expr env e in
      argument_of_pair (Left (kwd, e))

and map_param_to_gparam env (p : parameter) : G.parameter =
  match p with
  | P { pname; pdefault } -> (
      match (pname, pdefault, env) with
      | IdEllipsis t, None, Pattern -> G.ParamEllipsis t
      | _else_ ->
          let id = map_ident_should_not_use env pname in
          let pdefault =
            match pdefault with
            | None -> None
            | Some (_tk, e) -> Some (map_expr env e)
          in
          G.Param (G.param_of_id ?pdefault id))
  | OtherParamExpr e ->
      let e = map_expr env e in
      G.ParamPattern (expr_to_pattern e)
  | OtherParamPair (kwd, e) ->
      let kwd = map_keyword env kwd in
      let e = map_expr env e in
      let e = keyval_of_pair (Left (kwd, e)) in
      G.ParamPattern (expr_to_pattern e)

(* Convert one rescue/catch stab clause to a G.catch arm.
 * Each stab has a list of exception-type expressions and a handler body. *)
and map_rescue_stab_to_catch env tok (stab : stab_clause) : G.catch =
  let ((args, _kwargs), guard_opt), _tarrow, body_stmts = stab in
  let catch_pat =
    match args with
    | [] -> G.PatEllipsis tok
    | [arg] ->
        let e = map_expr env arg in
        expr_to_pattern e
    | args ->
        let pats = List_.map (fun a -> expr_to_pattern (map_expr env a)) args in
        let pat =
          List.fold_right (fun p acc -> G.DisjPat (p, acc))
            (List.tl pats) (List.hd pats)
        in
        pat
  in
  let catch_pat = 
    match guard_opt with
    | Some (_tok, guard) -> G.PatWhen (catch_pat, map_expr env guard)
    | None -> catch_pat
  in
  let catch_exn = G.CatchPattern catch_pat in
  let body = map_stmts env body_stmts in
  (tok, catch_exn, G.Block (Tok.unsafe_fake_bracket body) |> G.s)

(* Wrap a body statement in a G.Try when there are rescue/catch/after clauses.
 * `tok` is used as the try token. *)
and wrap_with_rescue env tok body_stmt rescue =
  match rescue with
  | [] -> body_stmt
  | extras ->
      let catches, try_else, finally =
        List.fold_left
          (fun (catches, try_else, finally) ((kind, t), boc) ->
            match (kind : exn_clause_kind) with
            | Rescue | Catch ->
                let s =
                  match boc with
                  | Clauses stabs ->
                      let cs = List_.map (map_rescue_stab_to_catch env t) stabs in
                      catches @ cs
                  | Body _ -> catches
                in
                (s,
                 try_else,
                 finally)
            | Else ->
                let s =
                  match boc with
                  | Clauses stabs ->
                      stabs |> List.concat_map (fun (_, _, b) -> map_stmts env b)
                  | Body _ -> []
                in
                (catches,
                 Some (t, G.Block (Tok.unsafe_fake_bracket s) |> G.s),
                 finally)
            | After ->
                let s =
                  match boc with
                  | Clauses stabs ->
                      stabs |> List.concat_map (fun (_, _, b) -> map_stmts env b)
                  | Body stmts -> map_stmts env stmts
                in
                (catches,
                 try_else,
                 Some (t, G.Block (Tok.unsafe_fake_bracket s) |> G.s)))
          ([], None, None) extras
      in
      G.Try (tok, body_stmt, catches, try_else, finally) |> G.s

and map_func_clause_to_stab env (clause : function_definition) :
    stab_clause_generic =
  let _, params, _ = clause.f_params in
  let args = List_.map (map_param_as_arg env) params in
  let guard_opt =
    match clause.f_guard with
    | None -> None
    | Some guard -> Some (G.fake "when", map_expr env guard)
  in
  let body = map_body env (let _, b, _ = clause.f_body in b) in
  ((args, guard_opt), G.fake "->", body)

and map_definition env (v : definition) : G.definition =
  match v with
  | FuncDef [ { f_def; f_name; f_params; f_guard = None; f_body; f_rescue; f_is_private } ] ->
      (* Single clause, no guard: use direct param names (original behavior).
       * This preserves the simple fparams=[x,y,...] representation so that
       * taint analysis can match call arguments to parameters by position
       * without any call-site wrapping. *)
      let id = map_ident_should_not_use env f_name in
      let ent =
        G.basic_entity
          ~attrs:
            (if f_is_private then
               [ G.KeywordAttr (G.Private, Tok.fake_tok f_def "") ]
             else [])
          id
      in
      let fparams = map_bracket (map_list map_param_to_gparam) env f_params in
      let l, body, r = f_body in
      let body_stmts = map_stmts env body in
      let body_stmt =
        wrap_with_rescue env l (G.Block (l, body_stmts, r) |> G.s) f_rescue
      in
      let fdef =
        {
          G.fkind = (G.Function, f_def);
          fparams;
          frettype = None;
          fbody = G.FBStmt body_stmt;
        }
      in
      (ent, G.FuncDef fdef)
  | FuncDef (clause :: _ as clauses) ->
      (* Multi-clause or guarded: N synthetic params + Switch.
       * We build Container(Tuple, [__p0__, ..., __pN__]) as the switch
       * discriminant so that PatTuple case patterns can destructure it.
       * Calls remain as N-argument calls (no wrapping needed), and
       * find_pos_in_actual_args maps each call arg to __pI__ by position. *)
      let id = map_ident_should_not_use env clause.f_name in
      let ent =
        G.basic_entity
          ~attrs:
            (if clause.f_is_private then
               [ G.KeywordAttr (G.Private, Tok.fake_tok clause.f_def "") ]
             else [])
          id
      in
      let tk = clause.f_def in
      let _, first_params, _ = clause.f_params in
      let n = List.length first_params in
      let param_ids = List.init n (fun i -> (Printf.sprintf "__p%d__" i, tk)) in
      (* The __pN__ names are synthesised for the multi-clause lowering
         and never appear in Elixir source; mark hidden so the prefilter
         regex doesn't require them. *)
      let synthetic_fparams =
        Tok.unsafe_fake_bracket
          (List_.map (fun pid -> G.Param (G.param_of_id ~hidden:true pid))
             param_ids)
      in
      let switch_cond =
        match param_ids with
        | [] -> G.L (G.Null tk) |> G.e
        | ids ->
            let refs =
              List_.map (fun pid ->
                  G.N (H.name_of_id ~hidden:true pid) |> G.e)
                ids
            in
            G.Container (G.Tuple, Tok.unsafe_fake_bracket refs) |> G.e
      in
      let cases =
        clauses
        |> List_.map (fun c ->
               let stab = map_func_clause_to_stab env c in
               case_and_body_of_stab_clause stab)
      in
      let switch_stmt = G.Switch (tk, Some (G.Cond switch_cond), cases) |> G.s in
      (* Aggregate rescue/catch/after clauses from all function clauses *)
      let all_rescue = List.concat_map (fun c -> c.f_rescue) clauses in
      let body_stmt = wrap_with_rescue env tk switch_stmt all_rescue in
      let fdef =
        {
          G.fkind = (G.Function, tk);
          fparams = synthetic_fparams;
          frettype = None;
          fbody = G.FBStmt body_stmt;
        }
      in
      (ent, G.FuncDef fdef)
  | FuncDef [] ->
      let fake_tok = G.fake "def" in
      let ent = G.basic_entity ("?", fake_tok) in
      let fdef =
        {
          G.fkind = (G.Function, fake_tok);
          fparams = Tok.unsafe_fake_bracket [];
          frettype = None;
          fbody = G.FBStmt (G.Block (Tok.unsafe_fake_bracket []) |> G.s);
        }
      in
      (ent, G.FuncDef fdef)
  | ModuleDef { m_defmodule = _; m_name; m_body = _tdo, xs, _tend } ->
      (* TODO: split alias in components, and then use name_of_ids *)
      let v = map_alias env m_name in
      let n = H.name_of_id v in
      let ent = { G.name = G.EN n; attrs = []; tparams = None } in
      let items = map_stmts env xs in
      (* alt: we could also generate a Package directive instead *)
      let def = { G.mbody = G.ModuleStruct (None, items) } in
      (ent, G.ModuleDef def)

and map_stmts env (xs : stmts) : G.stmt list =
  xs
  |> List_.map (function
       | S x -> map_stmt env x
       | e ->
           let e = map_expr env e in
           exprstmt e)

and map_vardef env v1 v2 =
  (* We don't expect the IdEllipsis case to enter this function. *)
  let id = map_ident_or_metavar_only_exn_on_ellipsis env v1 in
  let ent = G.basic_entity id in
  let e2 = map_expr env v2 in
  let def_stmt =
    G.DefStmt (ent, VarDef { vinit = Some e2; vtype = None; vtok = G.no_sc }) |> G.s
  in
  G.StmtExpr def_stmt |> G.e

(* TODO: Elixir also has these patterns:
 *   ^x = 0 meaning x cannot be re-assigned later, and
 *   [x|y] = [0, 1, 2] where x maps to 0, and y maps to the rest
 * and expr_to_pattern doesn't cover these cases.
 *)
and map_letpattern env v1 v2 =
  let e1 = expr_to_pattern (map_expr env v1) in
  let e2 = map_expr env v2 in
  G.LetPattern (e1, e2) |> G.e

and map_binary_op env v1 v2 v3 =
  let e1 = map_expr env v1 in
  let op = map_wrap_operator env v2 in
  let e2 = map_expr env v3 in
  match op with
  | Left (op, tk) -> G.opcall (op, tk) [ e1; e2 ]
  | Right (("=>", tk) as _id) ->
      G.keyval e1 tk e2
  | Right id ->
      let n = G.N (H.name_of_id id) |> G.e in
      G.Call (n, fb ([ e1; e2 ] |> List_.map G.arg)) |> G.e

(* Desugar pipe: x |> f(a, b) becomes f(x, a, b), tagged with
 * OtherExpr("PipelineCall", ...) so search patterns can distinguish
 * piped calls from direct calls. *)
and map_pipeline env (v1 : expr) (tk : tok) (v3 : expr) : G.expr =
  let desugared =
    match v3 with
    | Call (fn, (l, (args, kwds), r), blk) ->
        (* Insert v1 as first argument at the Elixir AST level;
         * map_call will call map_expr on v1, handling nested pipes. *)
        map_call env (fn, (l, (v1 :: args, kwds), r), blk)
    | _ ->
        (* Bare function reference: x |> f  becomes  f(x) *)
        let e1 = map_expr env v1 in
        let e2 = map_expr env v3 in
        G.Call (e2, fb [ G.Arg e1 ]) |> G.e
  in
  G.OtherExpr (("PipelineCall", tk), [ G.E desugared ]) |> G.e

and map_match env v1 v2 =
  (* Single LHS names are VarDefs.
   * Otherwise, including ellipsis, we consider them LetPatterns.
   *)
  match v1 with
  | I id -> (
      match id with
      | Id _
      | IdMetavar _ ->
          map_vardef env id v2
      | IdEllipsis _ -> map_letpattern env v1 v2)
  | _else_ -> map_letpattern env v1 v2

and map_expr env v : G.expr =
  match v with
  | S x ->
      let st = map_stmt env x in
      G.stmt_to_expr st
  | I v -> (
      match v with
      | IdEllipsis v -> G.Ellipsis v |> G.e
      | Id _
      | IdMetavar _ ->
          let id = map_ident_or_metavar_only_exn_on_ellipsis env v in
          G.N (H.name_of_id id) |> G.e)
  | L lit -> G.L lit |> G.e
  | A v -> map_atom env v
  | String v ->
      let v = map_quoted env v in
      expr_of_quoted v
  | Charlist v ->
      let v = map_quoted env v in
      let e = expr_of_quoted v in
      let l, _, _ = v in
      G.OtherExpr (("Charlist", l), [ G.E e ]) |> G.e
  | Sigil (ttilde, v2, v3) ->
      let v2 = map_sigil_kind env v2 in
      let v3 = (map_option (map_wrap map_string)) env v3 in
      G.OtherExpr
        ( ("Sigil", ttilde),
          v2
          @
          match v3 with
          | None -> []
          | Some id -> [ G.I id ] )
      |> G.e
  | List v ->
      let v = (map_bracket map_items) env v in
      G.Container (G.List, v) |> G.e
  | Tuple v ->
      (* NOTE: Getting Tuple (..., ...) instead of ... for the last occurrence of:
       * "%{..., some_item: $V, ...}", but the pattern "%{..., some_item: $V}"
       * works for most purposes... *)
      let v = (map_bracket map_items) env v in
      G.Container (G.Tuple, v) |> G.e
  | Bits v ->
      let l, xs, r = (map_bracket map_items) env v in
      G.OtherExpr (("Bits", l), List_.map (fun e -> G.E e) xs @ [ G.Tk r ])
      |> G.e
  | Map (v1, v2, v3) -> (
      let v2 = (map_option map_astruct) env v2 in
      let l, xs, r = (map_bracket map_map_items) env v3 in
      let dict_l = Tok.combine_toks v1 [ l ] in
      let dict_body = G.Container (G.Dict, (dict_l, xs, r)) |> G.e in
      match v2 with
      | None -> dict_body
      | Some name_expr -> (
          match H.name_of_dot_access name_expr with
          | Some n -> G.Constructor (n, fb [ dict_body ]) |> G.e
          | None ->
              (* Fallback for dynamic struct names that can't be statically resolved *)
              G.Call (name_expr, fb [ G.arg dict_body ]) |> G.e))
  | Alias v ->
      (* TODO: split alias in components, and then use name_of_ids *)
      let v = map_alias env v in
      G.N (H.name_of_id v) |> G.e
  | Block v ->
      let l, body_or_clauses, _r = map_block env v in

      (* TODO: could pass a 'body_or_clauses bracket' to
       * expr_of_body_or_clauses to avoid the fake_bracket above
       *)
      expr_of_body_or_clauses l body_or_clauses
  | DotAlias (v1, tdot, v3) ->
      let e = map_expr env v1 in
      (* TODO: split alias in components, and then use name_of_ids *)
      let id = map_alias env v3 in
      G.DotAccess (e, tdot, G.FN (H.name_of_id id)) |> G.e
  | DotTuple (v1, tdot, v3) ->
      let e = map_expr env v1 in
      let items = (map_bracket map_items) env v3 in
      let tuple = G.Container (G.Tuple, items) |> G.e in
      G.DotAccess (e, tdot, G.FDynamic tuple) |> G.e
  (* only inside a Call *)
  | DotAnon (v1, tdot) ->
      let e = map_expr env v1 in
      G.OtherExpr (("DotAnon", tdot), [ G.E e ]) |> G.e
  (* only inside a Call *)
  | DotRemote v -> map_remote_dot env v
  (* Elixir field access: `foo.bar` (no parens). Translate to a plain
   * DotAccess so downstream analysis treats it as field access, not a
   * zero-arity function call. *)
  | FieldAccess v -> map_remote_dot env v
  | ModuleVarAccess (tat, v2) ->
      let e = map_expr env v2 in
      G.OtherExpr (("AttrExpr", tat), [ G.E e ]) |> G.e
  | ArrayAccess (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_bracket map_expr) env v2 in
      G.ArrayAccess (v1, v2) |> G.e
  | Call v -> map_call env v
  | UnaryOp (v1, v2) -> (
      let v1 = map_wrap_operator env v1 in
      let e = map_expr env v2 in
      match v1 with
      | Left (op, tk) -> G.opcall (op, tk) [ e ]
      | Right id ->
          let n = G.N (H.name_of_id id) |> G.e in
          G.Call (n, fb [ G.Arg e ]) |> G.e)
  | BinaryOp (v1, v2, v3) -> (
      match v2 with
      | OMatch, _tk -> map_match env v1 v3
      | OPipeline, tk -> map_pipeline env v1 tk v3
      | _else_ -> map_binary_op env v1 v2 v3)
  | OpArity (v1, tslash, pi) ->
      let id = map_wrap_operator_ident env v1 in
      let lit = G.L (G.Int pi) |> G.e in
      G.OtherExpr (("OpArity", tslash), [ G.I id; G.E lit ]) |> G.e
  | When (v1, twhen, v3) ->
      let e1 = map_expr env v1 in
      let v3 = map_expr_or_kwds env v3 in
      let e3 = expr_of_expr_or_kwds v3 in
      G.OtherExpr (("When", twhen), [ G.E e1; G.E e3 ]) |> G.e
  | Join (v1, tbar, v3) ->
      let e1 = map_expr env v1 in
      let v3 = map_expr_or_kwds env v3 in
      let e3 = expr_of_expr_or_kwds v3 in
      G.Constructor (H.name_of_id ("|", tbar), fb [ e1; e3 ]) |> G.e
  | Lambda (tfn, v2, _tend) ->
      let xs = map_clauses env v2 in
      let fdef = stab_clauses_to_function_definition tfn xs in
      let fdef = { fdef with fkind = (G.LambdaKind, tfn) } in
      G.Lambda fdef |> G.e
  | Capture (tamp, v2) ->
      (* Convert capture with placeholders to lambda wrapped in ShortLambda *)
      let e = map_expr env v2 in
      convert_short_lambda tamp e
  | ShortLambda (tamp, (l, v2, r)) ->
      (* Convert short lambda with placeholders to lambda wrapped in ShortLambda *)
      let e = map_expr env v2 in
      H.set_e_range l r e;
      convert_short_lambda tamp e
  | PlaceHolder (tamp, x) ->
      let lit = G.L (G.Int x) |> G.e in
      G.OtherExpr (("PlaceHolder", tamp), [ G.E lit ]) |> G.e
  | DeepEllipsis v ->
      let v = (map_bracket map_expr) env v in
      G.DeepEllipsis v |> G.e

and map_astruct env v = map_expr env v

and map_sigil_kind env v : G.any list =
  match v with
  | Lower (v1, v2) ->
      let c, tk = (map_wrap map_char) env v1 in
      let quoted = map_quoted env v2 in
      let id = (spf "%c" c, tk) in
      [ G.I id; G.E (expr_of_quoted quoted) ]
  | Upper (v1, v2) ->
      let c, tk = (map_wrap map_char) env v1 in
      let l, x, r = (map_bracket (map_wrap map_string)) env v2 in
      let id = (spf "%c" c, tk) in
      [ G.I id; G.E (G.L (G.String (l, x, r)) |> G.e) ]

and map_body env v : G.stmt list =
  let xs = (map_list map_expr) env v in
  xs |> List_.map exprstmt

and map_call env (v1, v2, v3) : G.expr =
  (* Special handling for DotAnon - extract the inner expression to use as callee *)
  let e =
    match v1 with
    | DotAnon (inner_expr, _tdot) -> map_expr env inner_expr
    | _ -> map_expr env v1
  in
  let l, args, r = (map_bracket map_arguments) env v2 in
  let v3 = (map_option map_do_block) env v3 in
  let args' = args_of_do_block_opt v3 in
  G.Call (e, (l, args @ args', r)) |> G.e

and map_remote_dot env (v1, tdot, v3) : G.expr =
  let e = map_expr env v1 in
  let v3 =
    match v3 with
    | X2 id_or_op ->
        let id =
          match id_or_op with
          | Left id -> map_ident_should_not_use env id
          | Right op -> map_wrap_operator_ident env op
        in
        X1 id
    | Quoted2 x -> Quoted1 x
  in
  let v3 = map_or_quoted1 (map_wrap map_string) env v3 in
  let fld =
    match v3 with
    | Left id -> G.FN (H.name_of_id id)
    | Right (quoted : quoted_generic) -> G.FDynamic (expr_of_quoted quoted)
  in
  G.DotAccess (e, tdot, fld) |> G.e

and map_stab_clause env (v : stab_clause) : stab_clause_generic =
  let (vargs, vwhenopt), tarrow, vbody = v in
  let args = map_arguments env vargs in
  let map_when env (twhen, v2) =
    let e = map_expr env v2 in
    (twhen, e)
  in
  let whenopt = (map_option map_when) env vwhenopt in
  let stmts = map_body env vbody in
  ((args, whenopt), tarrow, stmts)

and map_clauses env v = (map_list map_stab_clause) env v

and map_body_or_clauses env v =
  match v with
  | Body v ->
      let v = map_body env v in
      Left v
  | Clauses v ->
      let v = map_clauses env v in
      Right v

and map_do_block env (v : do_block) : do_block_generic =
  let map_tuple1 env (v1, v2) =
    let v1 = map_body_or_clauses env v1 in
    let map_tuple2 env (kind, v2) =
      let v2 = map_body_or_clauses env v2 in
      (kind, v2)
    in
    let v2 = (map_list map_tuple2) env v2 in
    (v1, v2)
  in
  (map_bracket map_tuple1) env v

and map_block env v = (map_bracket map_body_or_clauses) env v

let map_program env v : G.program = map_body env v

let map_any env v =
  match v with
  | Pr v ->
      let v = map_program env v in
      G.Pr v

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program x =
  let env = Program in
  let x = Elixir_to_elixir.map_program x in
  map_program env x

let any x =
  let env = Pattern in
  let x = Elixir_to_elixir.map_any x in
  map_any env x
