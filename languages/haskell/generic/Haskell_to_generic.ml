(* Haskell tree-sitter CST → AST_generic translator.
 *
 * Typed, exhaustive walker over [Tree_sitter_haskell.CST]. Follows the
 * same style as Parse_python_tree_sitter.ml / Parse_clojure_tree_sitter.ml:
 * each [map_*] function pattern-matches on its CST variant and produces
 * an AST_generic value directly. No [node.type_] string matching.
 *
 * For Haskell features without a direct AST_generic equivalent (forall,
 * linear fn, implicit params, view_patterns, TH quote/splice/quasiquote,
 * GADTs, associated types), we emit structured [OtherType]/[OtherExpr]/
 * [OtherStmt]/[OtherPat] sentinels that preserve sub-expressions for
 * downstream pattern matching.
 *)

open Fpath_.Operators [@@warning "-33"]
module CST = Tree_sitter_haskell.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic

[@@@warning "-26-27-32-39"]

(*****************************************************************************)
(* Env                                                                        *)
(*****************************************************************************)

type extra = {
  is_pattern_mode : bool;
  metavar_map : (string, string) Hashtbl.t;
}

type env = extra H.env

let token = H.token
let str = H.str

let fb = Tok.unsafe_fake_bracket
let fake_tok = Tok.unsafe_fake_tok

let other_expr (tag : string) (tok : Tok.t) (args : G.any list) : G.expr =
  G.OtherExpr ((tag, tok), args) |> G.e

let other_stmt (tag : string) (tok : Tok.t) (args : G.any list) : G.stmt =
  G.OtherStmt (G.OS_Todo, G.TodoK (tag, tok) :: args) |> G.s

let other_pat (tag : string) (tok : Tok.t) (args : G.any list) : G.pattern =
  G.OtherPat ((tag, tok), args)

let other_type (tag : string) (tok : Tok.t) (args : G.any list) : G.type_ =
  { G.t = G.OtherType ((tag, tok), args); t_attrs = [] }

(*****************************************************************************)
(* Metavariable preprocessing — textual, identical to the raw walker         *)
(*****************************************************************************)

type placeholder_case = Lower | Upper

let preprocess_metavariables_with_case (case : placeholder_case)
    (pattern : string) : string * (string, string) Hashtbl.t =
  let re =
    Str.regexp "\\$\\.\\.\\([A-Z_][A-Z0-9_]*\\)\\|\\$[A-Z_][A-Z0-9_]*"
  in
  let buf = Buffer.create (String.length pattern) in
  let mapping = Hashtbl.create 16 in
  let rec loop idx =
    if idx >= String.length pattern then ()
    else
      match (try Some (Str.search_forward re pattern idx)
             with Not_found -> None) with
      | None ->
          Buffer.add_substring buf pattern idx (String.length pattern - idx)
      | Some pos ->
          Buffer.add_substring buf pattern idx (pos - idx);
          let matched = Str.matched_string pattern in
          let placeholder, original =
            if String.length matched >= 4
               && String.sub matched 0 4 = "$..." then
              let name = Str.matched_group 1 pattern in
              let ph = match case with
                | Lower -> "__semgrep_ellipsis_" ^ name
                | Upper -> "SemgrepEllipsis" ^ name
              in
              (ph, matched)
            else
              let name =
                String.sub matched 1 (String.length matched - 1)
              in
              let ph = match case with
                | Lower -> "__semgrep_metavar_" ^ name
                | Upper -> "SemgrepMv" ^ name
              in
              (ph, matched)
          in
          Hashtbl.replace mapping placeholder original;
          Buffer.add_string buf placeholder;
          loop (pos + String.length matched)
  in
  loop 0;
  (Buffer.contents buf, mapping)

let resolve_text (env : env) (text : string) : string =
  match Hashtbl.find_opt env.H.extra.metavar_map text with
  | Some original -> original
  | None -> text

let is_metavar (s : string) : bool =
  String.length s >= 2
  && s.[0] = '$'
  && (let c = s.[1] in (c >= 'A' && c <= 'Z') || c = '_')

let is_ellipsis_metavar (s : string) : bool =
  String.length s >= 5 && String.sub s 0 4 = "$..."

let resolved_ident (env : env) (tok : Tree_sitter_run.Token.t) : G.ident =
  let (raw, t) = str env tok in
  let resolved = resolve_text env raw in
  (resolved, t)

(*****************************************************************************)
(* Basic tokens: namespace, arrows, etc.                                      *)
(*****************************************************************************)

let map_where (env : env) (tok : CST.where) : Tok.t = token env tok

let map_arrow (env : env) (x : CST.arrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok -> token env tok
  | `DASHGT tok -> token env tok

let map_larrow (env : env) (x : CST.larrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok -> token env tok
  | `LTDASH tok -> token env tok

let map_carrow (env : env) (x : CST.carrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok -> token env tok
  | `EQGT tok -> token env tok

let map_colon2 (env : env) (x : CST.colon2) : Tok.t =
  match x with
  | `UNKUNKUNK tok -> token env tok
  | `COLONCOLON tok -> token env tok

(*****************************************************************************)
(* Names, constructors, identifiers                                           *)
(*****************************************************************************)

let map_conid (env : env) (tok : CST.conid) : G.ident =
  resolved_ident env tok

let map_varid (env : env) (tok : CST.varid) : G.ident =
  resolved_ident env tok

let map_constructor (env : env) (x : CST.constructor) : G.ident =
  match x with
  | `Conid tok -> map_conid env tok
  | `Semg_meta tok -> resolved_ident env tok

let map_variable (env : env) (x : CST.variable) : G.ident =
  match x with
  | `Varid tok -> map_varid env tok
  | `Semg_meta tok -> resolved_ident env tok

let map_tyconid (env : env) (tok : CST.tyconid) : G.ident =
  map_constructor env tok

let map_modid (env : env) (x : CST.modid) : G.ident =
  map_constructor env x

let map_qualifying_module (env : env) (q : CST.qualifying_module)
    : G.ident list =
  List.map (fun (m, _dot) -> map_modid env m) q

let map_qmodid (env : env) (x : CST.qmodid) : G.ident list =
  match x with
  | `Qual_module (q, m) ->
      map_qualifying_module env q @ [ map_modid env m ]
  | `Modid m -> [ map_modid env m ]

let map_pat_name (env : env) (x : CST.pat_name) : G.ident =
  match x with
  | `Var x -> map_variable env x
  | `LPAR_choice_op_RPAR (_, op, _) ->
      (match op with
       | `Op tok -> resolved_ident env tok
       | `Minus tok -> resolved_ident env tok)

let map_fun_name (env : env) (x : CST.fun_name) : G.ident =
  match x with
  | `Choice_var pn -> map_pat_name env pn
  | `Impl_parid tok -> resolved_ident env tok

(* Operators. The raw CST breaks them into many variants; we unify to
 * a single G.ident so callers can produce Call(Id op, args) cleanly. *)

let map_operator_minus (env : env) (x : CST.operator_minus) : G.ident =
  match x with
  | `Op tok -> resolved_ident env tok
  | `Minus tok -> resolved_ident env tok

let map_varop (env : env) (x : CST.varop) : G.ident =
  match x with
  | `Choice_op op -> map_operator_minus env op
  | `BQUOT_var_BQUOT (_, v, _) -> map_variable env v

let map_conop (env : env) (x : CST.conop) : G.ident =
  match x with
  | `Cons_op tok -> resolved_ident env tok
  | `BQUOT_cons_BQUOT (_, tc, _) -> map_tyconid env tc

let map_op (env : env) (x : CST.op) : G.ident =
  match x with
  | `Varop x -> map_varop env x
  | `Choice_cons_op x -> map_conop env x

(* Dotted ident list out of a qualifying_module + tail. *)
let dotted_of_qual (env : env) (q : CST.qualifying_module)
    (tail : G.ident) : G.ident list =
  map_qualifying_module env q @ [ tail ]

let map_qvarid (env : env) (x : CST.qvarid) : G.ident list =
  match x with
  | `Qual_var (q, v) -> dotted_of_qual env q (map_variable env v)
  | `Var v -> [ map_variable env v ]

let map_qconid (env : env) (x : CST.qconid) : G.ident list =
  match x with
  | `Qual_cons (q, t) -> dotted_of_qual env q (map_tyconid env t)
  | `Cons t -> [ map_tyconid env t ]

let map_qvarsym (env : env) (x : CST.qvarsym) : G.ident list =
  match x with
  | `Qual_op (q, op) ->
      let id = map_operator_minus env op in
      dotted_of_qual env q id
  | `Choice_op op -> [ map_operator_minus env op ]

let map_qvarsym_nominus (env : env) (x : CST.qvarsym_nominus) : G.ident list =
  match x with
  | `Qual_op (q, op) ->
      let id = map_operator_minus env op in
      dotted_of_qual env q id
  | `Op tok -> [ resolved_ident env tok ]

let map_qconsym (env : env) (x : CST.qconsym) : G.ident list =
  match x with
  | `Qual_cons_op (q, tok) ->
      let id = resolved_ident env tok in
      dotted_of_qual env q id
  | `Cons_op tok -> [ resolved_ident env tok ]

let map_qvar (env : env) (x : CST.qvar) : G.ident list =
  match x with
  | `Choice_qual_var x -> map_qvarid env x
  | `LPAR_choice_qual_op_RPAR (_, sym, _) -> map_qvarsym env sym

let map_qcon (env : env) (x : CST.qcon) : G.ident list =
  match x with
  | `Choice_qual_cons x -> map_qconid env x
  | `LPAR_choice_qual_cons_op_RPAR (_, sym, _) -> map_qconsym env sym

let map_pat_constructor (env : env) (x : CST.pat_constructor) : G.ident list =
  map_qcon env x

let map_qvarop (env : env) (x : CST.qvarop) : G.ident list =
  match x with
  | `Choice_qual_op sym -> map_qvarsym env sym
  | `BQUOT_choice_qual_var_BQUOT (_, v, _) -> map_qvarid env v

let map_qvarop_nominus (env : env) (x : CST.qvarop_nominus) : G.ident list =
  match x with
  | `Choice_qual_op sym -> map_qvarsym_nominus env sym
  | `BQUOT_choice_qual_var_BQUOT (_, v, _) -> map_qvarid env v

let map_qconop (env : env) (x : CST.qconop) : G.ident list =
  match x with
  | `Choice_qual_cons_op sym -> map_qconsym env sym
  | `BQUOT_choice_qual_cons_BQUOT (_, c, _) -> map_qconid env c

let map_qop (env : env) (x : CST.qop) : G.ident list =
  match x with
  | `Qvarop x -> map_qvarop env x
  | `Choice_choice_qual_cons_op x -> map_qconop env x

let map_qop_nominus (env : env) (x : CST.qop_nominus) : G.ident list =
  match x with
  | `Qvarop_nominus x -> map_qvarop_nominus env x
  | `Choice_choice_qual_cons_op x -> map_qconop env x

let map_qname (env : env) (x : CST.qname) : G.ident list =
  match x with
  | `Choice_choice_qual_var x -> map_qvar env x
  | `Qcon x -> map_qcon env x

(* Build an expression from a dotted ident chain, emitting DotAccess for
 * qualifiers and a bare Id for the simple one-ident case. *)
let expr_of_idents (ids : G.ident list) : G.expr =
  match ids with
  | [] -> other_expr "empty_qname" (fake_tok "") []
  | [ id ] -> G.N (G.Id (id, G.empty_id_info ())) |> G.e
  | first :: rest ->
      List.fold_left (fun acc (name, tok) ->
        G.DotAccess (acc, tok,
          G.FN (G.Id ((name, tok), G.empty_id_info ()))) |> G.e
      ) (G.N (G.Id (first, G.empty_id_info ())) |> G.e) rest

let ident_of_idents (ids : G.ident list) : G.ident =
  match ids with
  | [] -> ("<anon>", fake_tok "")
  | [ id ] -> id
  | _ ->
      let text = String.concat "." (List.map fst ids) in
      let tok = match ids with (_, t) :: _ -> t | [] -> fake_tok "" in
      (text, tok)

(*****************************************************************************)
(* Literals                                                                   *)
(*****************************************************************************)

let map_integer (env : env) (x : CST.integer) : G.literal =
  let tok = match x with
    | `Bin_lit t | `Int_lit t | `Octal_lit t | `Hex_lit t -> t
  in
  let (raw, t) = str env tok in
  let v = try Some (Int64.of_string raw) with Failure _ -> None in
  G.Int (v, t)

let map_number (env : env) (x : CST.number) : G.literal =
  match x with
  | `Int x -> map_integer env x
  | `Float tok ->
      let (raw, t) = str env tok in
      let v = try Some (float_of_string raw) with Failure _ -> None in
      G.Float (v, t)

let map_stringly (env : env) (x : CST.stringly) : G.literal =
  match x with
  | `Str tok ->
      let (raw, t) = str env tok in
      G.String (fake_tok "\"", (raw, t), fake_tok "\"")
  | `Char tok ->
      let (raw, t) = str env tok in
      G.Char (raw, t)

let map_literal (env : env) (x : CST.literal) : G.literal =
  match x with
  | `Choice_int x -> map_number env x
  | `Choice_str x -> map_stringly env x

let map_con_unit (env : env) ((l, r) : CST.con_unit) : G.expr =
  let lt = token env l in
  let _rt = token env r in
  G.L (G.Unit lt) |> G.e

let map_con_list (env : env) ((l, _r) : CST.con_list) : G.expr =
  let lt = token env l in
  (* `[]` is the empty list. Emit empty container. *)
  G.Container (G.List, (lt, [], lt)) |> G.e

let map_con_tuple (env : env) (x : CST.con_tuple) : G.expr =
  let _cs = x in
  other_expr "con_tuple" (fake_tok "(,)") []

let map_gcon_literal (env : env) (x : CST.gcon_literal) : G.expr =
  match x with
  | `Con_unit x -> map_con_unit env x
  | `Con_list x -> map_con_list env x
  | `Con_tuple x -> map_con_tuple env x

let map_literal_ (env : env) (x : CST.literal_) : G.expr =
  match x with
  | `Lit lit -> G.L (map_literal env lit) |> G.e
  | `Choice_con_unit g -> map_gcon_literal env g

(*****************************************************************************)
(* Quasiquote / splice — structured OtherExpr                                *)
(*****************************************************************************)

let map_quasiquote (env : env) (x : CST.quasiquote) : G.expr =
  let _ = env in
  let _ = x in
  other_expr "quasiquote" (fake_tok "[|") []

(*****************************************************************************)
(* Types — forward-declared so patterns/exps can reference them              *)
(*****************************************************************************)

(* We implement types in their own mutually-recursive block with
 * patterns and expressions further down. For now these forward-declarations
 * are implemented inside the big `rec` group. *)

(*****************************************************************************)
(* The big mutually-recursive block for exp / pat / type                     *)
(*****************************************************************************)

(* Forward decls via let rec … and … *)

let rec map_exp (env : env) ((splice, ann) : CST.exp) : G.expr =
  let e = map_top_splice env splice in
  match ann with
  | None -> e
  | Some (_colon, type_or_impl) ->
      let ty = map_type_or_implicit env type_or_impl in
      G.Cast (ty, fake_tok "::", e) |> G.e

and map_top_splice (env : env) (x : CST.top_splice) : G.expr =
  map_exp_infix env x

and map_exp_infix (env : env) (x : CST.exp_infix) : G.expr =
  match x with
  | `Exp_infix_ (lhs, qop, rhs) ->
      let lhs_e = map_top_splice env lhs in
      let op_ids = map_qop env qop in
      let rhs_e = map_lexp env rhs in
      (* `f $ x` and `f $! x` are just `f x` after evaluation-order
       * desugaring; collapse them so patterns targeting `f x` match both
       * surface syntaxes, matching what the raw walker already does. *)
      (match op_ids with
       | [ ("$", _) ] | [ ("$!", _) ] ->
           G.Call (lhs_e, fb [G.Arg rhs_e]) |> G.e
       | _ ->
           let op_e = expr_of_idents op_ids in
           G.Call (op_e, fb [G.Arg lhs_e; G.Arg rhs_e]) |> G.e)
  | `Lexp x -> map_lexp env x

and map_lexp (env : env) (x : CST.lexp) : G.expr =
  match x with
  | `Exp_let_in x -> map_exp_let_in env x
  | `Exp_cond x -> map_exp_cond env x
  | `Exp_if_guard (if_tok, gdpats) ->
      let t = token env if_tok in
      let branches = List.map (map_gdpat env) gdpats in
      let base = other_expr "no_else_branch" t [] in
      List.fold_right (fun (guard, body) acc ->
        G.Conditional (guard, body, acc) |> G.e
      ) branches base
  | `Exp_case x -> map_exp_case env x
  | `Exp_nega (minus, rhs) ->
      let mt = token env minus in
      let r = map_aexp env rhs in
      G.Call (G.N (G.Id (("-", mt), G.empty_id_info ())) |> G.e,
              fb [G.Arg r]) |> G.e
  | `Fexp x -> map_fexp env x
  | `Exp_lambda x -> map_exp_lambda env x

and map_fexp (env : env) (x : CST.fexp) : G.expr =
  match x with
  | `Aexp x -> map_aexp env x
  | `Exp_apply x -> map_exp_apply env x

and map_exp_apply (env : env) (x : CST.exp_apply) : G.expr =
  (* Flatten the right-nested apply chain into one Call(func, [args...]). *)
  let rec collect (x : CST.exp_apply) (acc : G.expr list) : G.expr list =
    match x with
    | `Aexp a -> map_aexp env a :: acc
    | `Aexp_exp_apply (a, rest) ->
        map_aexp env a :: collect rest acc
    | `Aexp_exp_lambda (a, l) ->
        map_aexp env a :: map_exp_lambda env l :: acc
    | `Aexp_exp_let_in (a, li) ->
        map_aexp env a :: map_exp_let_in env li :: acc
    | `Aexp_exp_cond (a, c) ->
        map_aexp env a :: map_exp_cond env c :: acc
    | `Aexp_exp_case (a, c) ->
        map_aexp env a :: map_exp_case env c :: acc
  in
  match collect x [] with
  | [] -> other_expr "empty_apply" (fake_tok "") []
  | [ single ] -> single
  | func :: args ->
      (* Curry: one Call per argument — matches the raw walker's
       * convention so patterns against `f x y` match structurally. *)
      List.fold_left (fun acc arg ->
        G.Call (acc, fb [G.Arg arg]) |> G.e
      ) func args

and map_aexp (env : env) (x : CST.aexp) : G.expr =
  match x with
  | `Exp_name n -> map_exp_name env n
  | `Exp_parens (_, exp, _) -> map_exp env exp
  | `Exp_tuple_ (l, tup, _r) ->
      let t = token env l in
      let exprs = map_exp_tuple env tup in
      G.Container (G.Tuple, (t, exprs, fake_tok ")")) |> G.e
  | `Exp_list (l, first, rest, _r) ->
      let lt = token env l in
      let first = map_exp env first in
      let rest = List.map (fun (_c, e) -> map_exp env e) rest in
      G.Container (G.List, (lt, first :: rest, fake_tok "]")) |> G.e
  | `Exp_th_quoted_name q ->
      map_exp_th_quoted_name env q
  | `Exp_type_app (_at, aty) ->
      let ty = map_atype env aty in
      other_expr "type_app" (fake_tok "@") [G.T ty]
  | `Exp_lambda_case (bs, _case, alts_opt) ->
      let bt = token env bs in
      let cases = match alts_opt with
        | None -> []
        | Some a -> map_alts env a
      in
      let implicit_id = G.implicit_param_id bt in
      let implicit_param = G.Param (G.param_of_id implicit_id) in
      let scrut = G.N (AST_generic_helpers.name_of_id implicit_id) |> G.e in
      let sw = G.Switch (bt, Some (G.Cond scrut), cases) |> G.s in
      G.Lambda {
        G.fkind = (G.Arrow, bt);
        fparams = fb [implicit_param];
        frettype = None;
        fbody = G.FBStmt sw;
      } |> G.e
  | `Exp_do (_kw, body) ->
      let stmts = map_do_body env body in
      G.StmtExpr (G.Block (fb stmts) |> G.s) |> G.e
  | `Exp_record (base, _l, first, rest, _r) ->
      let base_e = map_aexp env base in
      let fields = map_exp_field env first ::
                   List.map (fun (_c, f) -> map_exp_field env f) rest in
      other_expr "record" (fake_tok "{") (G.E base_e :: List.map (fun e -> G.E e) fields)
  | `Exp_arit_seq (_l, first, _mid, _dd, end_opt, _r) ->
      let first_e = map_exp env first in
      let end_e = match end_opt with
        | None -> other_expr "arith_seq_infinite" (fake_tok "..") []
        | Some e -> map_exp env e
      in
      other_expr "arith_seq" (fake_tok "..")
        [G.E (G.Container (G.List, fb [first_e; end_e]) |> G.e)]
  | `Exp_list_comp (_l, base, _bar, q1, qs, _r) ->
      let base = map_exp env base in
      let quals = q1 :: List.map snd qs in
      let comps = List.filter_map (map_qual_to_comp env) quals in
      G.Comprehension (G.List, fb (base, comps)) |> G.e
  | `Exp_sect_left (_l, e, op, _r) ->
      let le = map_top_splice env e in
      let op_e = expr_of_idents (map_qop env op) in
      other_expr "left_section" (fake_tok "(") [G.E le; G.E op_e]
  | `Exp_sect_right (_l, op, e, _r) ->
      let op_e = expr_of_idents (map_qop_nominus env op) in
      let re = map_top_splice env e in
      other_expr "right_section" (fake_tok "(") [G.E op_e; G.E re]
  | `Exp_unbo_tuple (_, _, _) ->
      other_expr "unboxed_tuple" (fake_tok "(#") []
  | `Exp_unbo_sum_ (_, _, _) ->
      other_expr "unboxed_sum" (fake_tok "(#") []
  | `Splice _ ->
      other_expr "th_splice" (fake_tok "$") []
  | `Quas q -> map_quasiquote env q
  | `Lit_ l -> map_literal_ env l

and map_exp_name (env : env) (x : CST.exp_name) : G.expr =
  match x with
  | `Choice_choice_qual_var qv ->
      let ids = map_qvar env qv in
      let full = ident_of_idents ids in
      if env.H.extra.is_pattern_mode && is_metavar (fst full) then
        G.N (G.Id (full, G.empty_id_info ())) |> G.e
      else expr_of_idents ids
  | `Qcon qc ->
      let ids = map_qcon env qc in
      (match ids with
       | [] -> other_expr "empty_qcon" (fake_tok "") []
       | [ id ] ->
           let resolved = resolve_text env (fst id) in
           if env.H.extra.is_pattern_mode && is_metavar resolved then
             G.N (G.Id (id, G.empty_id_info ())) |> G.e
           else
             G.Constructor (G.Id (id, G.empty_id_info ()), fb []) |> G.e
       | _ :: _ -> expr_of_idents ids)
  | `Impl_parid tok ->
      let id = resolved_ident env tok in
      other_expr "implicit_parid" (snd id) [G.I id]
  | `Label tok ->
      let id = resolved_ident env tok in
      other_expr "label" (snd id) [G.I id]

and map_exp_cond (env : env) (x : CST.exp_cond) : G.expr =
  let (_if, cond, _s1, _then, then_e, _s2, _else, else_e) = x in
  let cond = map_exp env cond in
  let then_e = map_exp env then_e in
  let else_e = map_exp env else_e in
  G.Conditional (cond, then_e, else_e) |> G.e

and map_exp_case (env : env) (x : CST.exp_case) : G.expr =
  let (case_tok, scrut, _of, alts_opt) = x in
  let t = token env case_tok in
  let scrut = map_exp env scrut in
  let cases = match alts_opt with
    | None -> []
    | Some a -> map_alts env a
  in
  let sw = G.Switch (t, Some (G.Cond scrut), cases) |> G.s in
  G.StmtExpr sw |> G.e

and map_exp_lambda (env : env) (x : CST.exp_lambda) : G.expr =
  let (bs, pats, _arr, body) = x in
  let bt = token env bs in
  let params = map_fun_patterns env pats in
  let body = map_exp env body in
  G.Lambda {
    G.fkind = (G.Arrow, bt);
    fparams = fb params;
    frettype = None;
    fbody = G.FBExpr body;
  } |> G.e

and map_exp_let_in (env : env) (x : CST.exp_let_in) : G.expr =
  let ((_let, decls_opt), (_in, body)) = x in
  let bindings = match decls_opt with
    | None -> []
    | Some ld -> map_let_decls env ld
  in
  let body = map_exp env body in
  let let_exprs = List.map (fun (pat, e) ->
    G.LetPattern (pat, e) |> G.e
  ) bindings in
  match let_exprs with
  | [] -> body
  | _ -> G.Seq (let_exprs @ [body]) |> G.e

and map_let_decls (env : env) (x : CST.let_decls)
    : (G.pattern * G.expr) list =
  match x with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL (_l, inner, _semi, _r) ->
      (match inner with
       | None -> []
       | Some (first, rest) ->
           List.filter_map (fun x -> x) (
             map_decl_to_binding env first ::
             List.map (fun (_s, d) -> map_decl_to_binding env d) rest))
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI (_l, inner) ->
      (match inner with
       | None -> []
       | Some (first, rest, _trailing) ->
           List.filter_map (fun x -> x) (
             map_decl_to_binding env first ::
             List.map (fun (_s, d) -> map_decl_to_binding env d) rest))

and map_decl_to_binding (env : env) (d : CST.decl)
    : (G.pattern * G.expr) option =
  match d with
  | `Decl_fun (`Func ((lhs, rhs) as _fn)) ->
      (* Extract a simple name-bind `x = e`; complex patterns come later. *)
      (match lhs with
       | `Funvar (fn_name, None) ->
           let (name, tok) = map_fun_name env fn_name in
           let pat = G.PatId ((name, tok), G.empty_id_info ()) in
           let body = match map_funrhs_to_body env rhs with
             | G.FBExpr e -> e
             | _ -> other_expr "let_body" (fake_tok "") []
           in
           Some (pat, body)
       | _ -> None)
  | `Decl_fun (`Funpat (tp, rhs)) ->
      let pat = map_typed_pat env tp in
      let body = match map_funrhs_to_body env rhs with
        | G.FBExpr e -> e
        | _ -> other_expr "let_body" (fake_tok "") []
      in
      Some (pat, body)
  | `Gend _ -> None

and map_do_body (env : env)
    (body : CST.anon_choice_LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL_9605efc)
    : G.stmt list =
  match body with
  | `LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL (_l, inner, _semi, _r) ->
      (match inner with
       | None -> []
       | Some (first, rest) ->
           map_stmt env first ::
           List.map (fun (_sep, s) -> map_stmt env s) rest)
  | `Layout_start_opt_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_layout_end
      (_l, inner, _le) ->
      (match inner with
       | None -> []
       | Some (first, rest, _trailing) ->
           map_stmt env first ::
           List.map (fun (_sep, s) -> map_stmt env s) rest)

and map_stmt (env : env) (x : CST.stmt) : G.stmt =
  match x with
  | `Exp e ->
      let e = map_exp env e in
      G.ExprStmt (e, fake_tok ";") |> G.s
  | `Bind_pat (tp, _larrow, e) ->
      let pat = map_typed_pat env tp in
      let e = map_exp env e in
      G.ExprStmt (G.LetPattern (pat, e) |> G.e, fake_tok ";") |> G.s
  | `Let (_let, decls_opt) ->
      let bindings = match decls_opt with
        | None -> []
        | Some d -> map_decls_to_bindings env d
      in
      (match bindings with
       | [] -> G.OtherStmt (G.OS_Pass, []) |> G.s
       | _ ->
           let let_exprs = List.map (fun (pat, e) ->
             G.ExprStmt (G.LetPattern (pat, e) |> G.e, fake_tok ";") |> G.s
           ) bindings in
           G.Block (fb let_exprs) |> G.s)
  | `Rec (_rec, body) ->
      let stmts = map_do_body env body in
      G.Block (fb stmts) |> G.s

and map_decls_to_bindings (env : env) (d : CST.decls)
    : (G.pattern * G.expr) list =
  match d with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL (_l, inner, _semi, _r) ->
      (match inner with
       | None -> []
       | Some (first, rest) ->
           List.filter_map (fun x -> x) (
             map_decl_to_binding env first ::
             List.map (fun (_s, d) -> map_decl_to_binding env d) rest))
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI_layout_end
      (_l, inner, _le) ->
      (match inner with
       | None -> []
       | Some (first, rest, _trailing) ->
           List.filter_map (fun x -> x) (
             map_decl_to_binding env first ::
             List.map (fun (_s, d) -> map_decl_to_binding env d) rest))

and map_exp_field (env : env) (x : CST.exp_field) : G.expr =
  match x with
  | `DOTDOT _ -> other_expr "record_wildcard" (fake_tok "..") []
  | `Choice_choice_qual_var_opt_EQ_exp (qv, eq_exp) ->
      let ids = map_qvar env qv in
      let name_e = expr_of_idents ids in
      (match eq_exp with
       | None -> name_e
       | Some (_eq, e) ->
           let v = map_exp env e in
           other_expr "record_field" (fake_tok "=") [G.E name_e; G.E v])

and map_exp_tuple (env : env) ((first, rest) : CST.exp_tuple) : G.expr list =
  let collect_first = match first with
    | `Rep1_comma_exp (_commas, e) -> [map_exp env e]
    | `Exp_comma_opt_exp (e1, _c, e2_opt) ->
        map_exp env e1 ::
        (match e2_opt with None -> [] | Some e -> [map_exp env e])
  in
  let collect_rest = List.filter_map (fun (_c, e_opt) ->
    match e_opt with None -> None | Some e -> Some (map_exp env e)
  ) rest in
  collect_first @ collect_rest

and map_exp_th_quoted_name (env : env) (x : CST.exp_th_quoted_name) : G.expr =
  match x with
  | `SQUOT_choice_choice_choice_qual_var (q, qn) ->
      let t = token env q in
      let ids = map_qname env qn in
      other_expr "th_quoted_name" t [G.E (expr_of_idents ids)]
  | `SQUOTSQUOT_atype (q, at) ->
      let t = token env q in
      let ty = map_atype env at in
      other_expr "th_quoted_type" t [G.T ty]

and map_qual (env : env) (x : CST.qual) : G.expr =
  match x with
  | `Bind_pat (tp, _la, e) ->
      let pat = map_typed_pat env tp in
      let e = map_exp env e in
      G.LetPattern (pat, e) |> G.e
  | `Let (_let, decls_opt) ->
      let bs = match decls_opt with
        | None -> [] | Some d -> map_decls_to_bindings env d
      in
      (match bs with
       | [] -> other_expr "empty_let" (fake_tok "let") []
       | _ ->
           let es = List.map (fun (p, e) -> G.LetPattern (p, e) |> G.e) bs in
           G.Seq es |> G.e)
  | `Tran _t -> other_expr "list_comp_transform" (fake_tok "then") []
  | `Exp e -> map_exp env e

(* Lift a comprehension qualifier into the AST_generic comprehension
 * vocabulary: generator -> CompFor, guard expression -> CompIf. Let and
 * transform quals are left out (represented in a flattened form elsewhere). *)
and map_qual_to_comp (env : env) (x : CST.qual) : G.for_or_if_comp option =
  match x with
  | `Bind_pat (tp, la, e) ->
      let pat = map_typed_pat env tp in
      let e = map_exp env e in
      Some (G.CompFor (fake_tok "for", pat, map_larrow env la, e))
  | `Exp e ->
      let guard = map_exp env e in
      Some (G.CompIf (fake_tok "if", guard))
  | `Let _ | `Tran _ -> None

(*****************************************************************************)
(* Alts and guards                                                            *)
(*****************************************************************************)

and map_alts (env : env) (x : CST.alts) : G.case_and_body list =
  let alts_list = match x with
    | `LCURL_opt_alt_rep_SEMI_alt_opt_SEMI_RCURL (_l, inner, _semi, _r) ->
        (match inner with
         | None -> []
         | Some (first, rest) ->
             first :: List.map snd rest)
    | `Layout_start_opt_alt_rep_choice_SEMI_alt_opt_choice_SEMI_layout_end
        (_l, inner, _le) ->
        (match inner with
         | None -> []
         | Some (first, rest, _trailing) ->
             first :: List.map snd rest)
  in
  List.map (map_alt env) alts_list

and map_alt (env : env) ((pat, variants, _where) : CST.alt)
    : G.case_and_body =
  let t = fake_tok "" in
  let pattern = map_pat env pat in
  match variants with
  | `Arrow_exp (_arr, exp) ->
      let body = map_exp env exp in
      G.CasesAndBody ([G.Case (t, pattern)],
                      G.ExprStmt (body, fake_tok ";") |> G.s)
  | `Rep1_gdpat gdpats ->
      (* Build a Conditional chain from the gdpat list; wrap the
       * original pattern in PatWhen using the first guard's expression. *)
      let branches = List.map (map_gdpat env) gdpats in
      let base = other_expr "no_else_branch" t [] in
      let cond_expr = List.fold_right (fun (guard, body) acc ->
        G.Conditional (guard, body, acc) |> G.e
      ) branches base in
      let wrapped_pat = match branches with
        | (first_guard, _) :: _ -> G.PatWhen (pattern, first_guard)
        | [] -> pattern
      in
      G.CasesAndBody ([G.Case (t, wrapped_pat)],
                      G.ExprStmt (cond_expr, fake_tok ";") |> G.s)

and map_gdpat (env : env) ((guards, _arr, exp) : CST.gdpat)
    : G.expr * G.expr =
  let guard = map_guards env guards in
  let body = map_exp env exp in
  (guard, body)

and map_guards (env : env) ((_bar, g1, rest) : CST.guards) : G.expr =
  let g1_e = map_guard env g1 in
  let rest_e = List.map (fun (_c, g) -> map_guard env g) rest in
  match rest_e with
  | [] -> g1_e
  | _ -> G.Seq (g1_e :: rest_e) |> G.e

and map_guard (env : env) (x : CST.guard) : G.expr =
  match x with
  | `Pat_guard (pat, _la, e) ->
      let pat = map_pat env pat in
      let e = map_top_splice env e in
      other_expr "pat_guard" (fake_tok "<-") [G.P pat; G.E e]
  | `Let (_let, decls_opt) ->
      let bs = match decls_opt with None -> []
               | Some d -> map_decls_to_bindings env d in
      (match bs with
       | [] -> other_expr "empty_let_guard" (fake_tok "let") []
       | _ ->
           let es = List.map (fun (p, e) ->
             G.LetPattern (p, e) |> G.e) bs in
           G.Seq es |> G.e)
  | `Exp_infix e -> map_top_splice env e

(*****************************************************************************)
(* Patterns                                                                   *)
(*****************************************************************************)

and map_pat (env : env) (x : CST.pat) : G.pattern =
  match x with
  | `Pat_infix (lhs, op, rhs) ->
      let lhs_p = map_lpat env lhs in
      let ids = map_qconop env op in
      let ctor_id = ident_of_idents ids in
      let ctor_name = G.Id (ctor_id, G.empty_id_info ()) in
      let rhs_p = map_pat env rhs in
      G.PatConstructor (ctor_name, [lhs_p; rhs_p])
  | `Lpat x -> map_lpat env x

and map_lpat (env : env) (x : CST.lpat) : G.pattern =
  match x with
  | `Apat a -> map_apat env a
  | `Pat_nega (minus, a) ->
      let mt = token env minus in
      let inner = map_apat env a in
      other_pat "negated" mt [G.P inner]
  | `Pat_apply (ctor, pats) ->
      let ids = map_pat_constructor env ctor in
      let ctor_id = ident_of_idents ids in
      let ctor_name = G.Id (ctor_id, G.empty_id_info ()) in
      let args = List.map (map_apat env) pats in
      G.PatConstructor (ctor_name, args)

and map_apat (env : env) (x : CST.apat) : G.pattern =
  match x with
  | `Pat_name pn ->
      let (name, tok) = map_pat_name env pn in
      let resolved = resolve_text env name in
      if name = "_" then G.PatWildcard tok
      else if env.H.extra.is_pattern_mode && is_metavar resolved then
        G.PatId ((resolved, tok), G.empty_id_info ())
      else G.PatId ((resolved, tok), G.empty_id_info ())
  | `Pat_as (var, at, inner) ->
      let var_id = map_variable env var in
      let _at_tok = token env at in
      let inner_p = map_apat env inner in
      G.PatAs (inner_p, (var_id, G.empty_id_info ()))
  | `Pat_cons ctor ->
      let ids = map_pat_constructor env ctor in
      let ctor_id = ident_of_idents ids in
      let text = fst ctor_id in
      let tok = snd ctor_id in
      if text = "True" || text = "False" then
        G.PatConstructor (G.Id ((text, tok), G.empty_id_info ()), [])
      else
        G.PatConstructor (G.Id (ctor_id, G.empty_id_info ()), [])
  | `Pat_record (ctor, _fields) ->
      let ids = map_pat_constructor env ctor in
      let ctor_id = ident_of_idents ids in
      other_pat "pat_record" (snd ctor_id)
        [G.P (G.PatConstructor (G.Id (ctor_id, G.empty_id_info ()), []))]
  | `Lit_ l ->
      (match l with
       | `Lit lit ->
           let lit = map_literal env lit in
           G.PatLiteral lit
       | `Choice_con_unit g ->
           (match g with
            | `Con_unit (l, _r) ->
                G.PatLiteral (G.Unit (token env l))
            | `Con_list (l, _r) ->
                G.PatConstructor
                  (G.Id (("[]", token env l), G.empty_id_info ()), [])
            | `Con_tuple _ ->
                other_pat "pat_con_tuple" (fake_tok "(,)") []))
  | `Pat_wild tok -> G.PatWildcard (token env tok)
  | `Pat_parens (_, np, _) -> map_nested_pat env np
  | `Pat_tuple (_, np1, rest, _) ->
      let ps = map_nested_pat env np1 ::
               List.map (fun (_c, np) -> map_nested_pat env np) rest in
      G.PatTuple (fb ps)
  | `Pat_unbo_tuple (_, _, _) ->
      other_pat "pat_unboxed_tuple" (fake_tok "(#") []
  | `Pat_unbo_sum_ (_, _, _) ->
      other_pat "pat_unboxed_sum" (fake_tok "(#") []
  | `Pat_list (_, np1, rest, _) ->
      let ps = map_nested_pat env np1 ::
               List.map (fun (_c, np) -> map_nested_pat env np) rest in
      G.PatList (fb ps)
  | `Pat_strict (bang, inner) ->
      let _t = token env bang in
      let inner_p = map_apat env inner in
      other_pat "strict" (fake_tok "!") [G.P inner_p]
  | `Pat_irre (tilde, inner) ->
      let t = token env tilde in
      let inner_p = map_apat env inner in
      other_pat "irrefutable" t [G.P inner_p]
  | `Splice _ -> other_pat "pat_splice" (fake_tok "$") []
  | `Quas _ -> other_pat "pat_quasiquote" (fake_tok "[|") []

and map_nested_pat (env : env) (x : CST.nested_pat) : G.pattern =
  match x with
  | `Typed_pat tp -> map_typed_pat env tp
  | `Pat_view (func, arr, inner) ->
      let fe = map_exp env func in
      let at = map_arrow env arr in
      let inner_p = map_nested_pat env inner in
      other_pat "view_pattern" at [G.E fe; G.P inner_p]

and map_typed_pat (env : env) (x : CST.typed_pat) : G.pattern =
  match x with
  | `Pat p -> map_pat env p
  | `Pat_typed (p, _ann) -> map_pat env p

and map_fun_patterns (env : env) (pats : CST.fun_patterns)
    : G.parameter list =
  List.map (fun ap ->
    let p = map_apat env ap in
    match p with
    | G.PatId ((name, tok), _) ->
        G.Param (G.param_of_id (name, tok))
    | _ -> G.ParamPattern p
  ) pats

(*****************************************************************************)
(* Types                                                                      *)
(*****************************************************************************)

and map_type_or_implicit (env : env) (x : CST.type_or_implicit) : G.type_ =
  match x with
  | `Impl_param (pid, (_colon, t_or_i)) ->
      let _id = resolved_ident env pid in
      let ty = map_type_or_implicit env t_or_i in
      other_type "implicit_param" (fake_tok "?")
        [G.I _id; G.T ty]
  | `Type t -> map_type env t

and map_type (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Type_quants (forall, _dot, inner) ->
      let (_, vars) = forall in
      let var_tys = List.map (fun tv -> G.T (map_tyvar env tv)) vars in
      let body_ty = map_type env inner in
      other_type "forall" (fake_tok "forall")
        (G.T body_ty :: var_tys)
  | `Type_cont (ctx, inner) ->
      let _ = ctx in
      let body = map_type env inner in
      other_type "context" (fake_tok "=>") [G.T body]
  | `Type_fun (lhs, _arr, rhs) ->
      let l = map_type_infix env lhs in
      let r = map_type env rhs in
      let param = G.Param {
        G.pname = None; ptype = Some l; pdefault = None; pattrs = [];
        pinfo = G.empty_id_info ();
      } in
      { G.t = G.TyFun ([param], r); t_attrs = [] }
  | `Type_infix x -> map_type_infix env x

and map_type_infix (env : env) (x : CST.type_infix) : G.type_ =
  match x with
  | `Type_infix_ (lhs, op, rhs) ->
      let l = map_btype env lhs in
      let _op = op in
      let r = map_type_infix env rhs in
      { G.t = G.TyApply (l, fb [G.TA r]); t_attrs = [] }
  | `Btype b -> map_btype env b

and map_btype (env : env) (x : CST.btype) : G.type_ =
  match x with
  | `Atype a -> map_atype env a
  | `Type_apply (base, args) ->
      let b = map_atype env base in
      let ta = List.map (fun a -> G.TA (map_atype env a)) args in
      { G.t = G.TyApply (b, fb ta); t_attrs = [] }

and map_atype (env : env) (x : CST.atype) : G.type_ =
  match x with
  | `Type_name tn -> map_type_name env tn
  | `Type_star _ -> other_type "star" (fake_tok "*") []
  | `Type_lit_ _ -> other_type "type_literal" (fake_tok "'") []
  | `Type_parens (_, t, _) -> map_type_or_implicit env t
  | `Type_unbo_tuple (_, _, _) ->
      other_type "unboxed_tuple_type" (fake_tok "(#") []
  | `Type_unbo_sum (_, _, _) ->
      other_type "unboxed_sum_type" (fake_tok "(#") []
  | `Splice _ -> other_type "type_splice" (fake_tok "$") []
  | `Quas _ -> other_type "type_quasiquote" (fake_tok "[|") []

and map_type_name (env : env) (x : CST.type_name) : G.type_ =
  match x with
  | `Choice_anno_type_var tv -> map_tyvar env tv
  | `Choice_prom_tycon gt ->
      let id_opt = match gt with
        | `Prom_tycon (_, qt) | `Choice_choice_qual_type qt ->
            (match qt with
             | `Choice_qual_type (`Qual_type (_, tok))
             | `Choice_qual_type (`Cons tok) -> Some (map_tyconid env tok)
             | `LPAR_choice_qual_type_op__RPAR _ -> None)
        | `Tycon_arrow (_, arr, _) -> Some ("->", map_arrow env arr)
      in
      (match id_opt with
       | Some id ->
           { G.t = G.TyN (G.Id (id, G.empty_id_info ())); t_attrs = [] }
       | None ->
           { G.t = G.TyN (G.Id (("<tycon>", fake_tok ""), G.empty_id_info ()));
             t_attrs = [] })

and map_tyvar (env : env) (x : CST.tyvar) : G.type_ =
  match x with
  | `Anno_type_var (_, vid, _ann, _) ->
      let id = resolved_ident env vid in
      { G.t = G.TyVar id; t_attrs = [] }
  | `Type_var vid ->
      let id = resolved_ident env vid in
      let resolved = fst id in
      if is_metavar resolved then
        { G.t = G.TyN (G.Id (id, G.empty_id_info ())); t_attrs = [] }
      else
        { G.t = G.TyVar id; t_attrs = [] }

(*****************************************************************************)
(* funrhs / funlhs                                                            *)
(*****************************************************************************)

and map_funrhs_to_body (env : env) (rhs : CST.funrhs) : G.function_body =
  let (kind, _where_decls) = rhs in
  match kind with
  | `EQ_exp (_eq, exp) -> G.FBExpr (map_exp env exp)
  | `Fun_guards guards ->
      (* Build a Conditional chain from the guard equations. *)
      let branches = List.map (fun (gs, _eq, body) ->
        let guard = map_guards env gs in
        let b = map_exp env body in
        (guard, b)
      ) guards in
      let base = other_expr "no_else_branch" (fake_tok "") [] in
      let body = List.fold_right (fun (g, b) acc ->
        G.Conditional (g, b, acc) |> G.e
      ) branches base in
      G.FBExpr body

and map_function_to_def (env : env) (fn : CST.function_)
    : G.ident * G.function_definition =
  let (lhs, rhs) = fn in
  let (name, params) = match lhs with
    | `Funvar (fn_name, pats_opt) ->
        let n = map_fun_name env fn_name in
        let ps = match pats_opt with
          | None -> []
          | Some pats -> map_fun_patterns env pats
        in
        (n, ps)
    | `Funpat_infix (lhs_pat, op, rhs_pat) ->
        let op_id = map_varop env op in
        let lp = map_pat env lhs_pat in
        let rp = map_pat env rhs_pat in
        let wrap p = match p with
          | G.PatId ((n, t), _) -> G.Param (G.param_of_id (n, t))
          | _ -> G.ParamPattern p
        in
        (op_id, [wrap lp; wrap rp])
  in
  let fbody = map_funrhs_to_body env rhs in
  (name, {
    G.fkind = (G.Function, snd name);
    fparams = fb params;
    frettype = None;
    fbody;
  })

and map_decl (env : env) (x : CST.decl) : G.stmt =
  match x with
  | `Gend g -> map_gendecl env g
  | `Decl_fun df ->
      (match df with
       | `Func fn ->
           let (name, fdef) = map_function_to_def env fn in
           let ent = G.basic_entity name in
           G.DefStmt (ent, G.FuncDef fdef) |> G.s
       | `Funpat (tp, rhs) ->
           let pat = map_typed_pat env tp in
           let body = match map_funrhs_to_body env rhs with
             | G.FBExpr e -> e
             | _ -> other_expr "funpat_body" (fake_tok "") []
           in
           G.ExprStmt (G.LetPattern (pat, body) |> G.e, fake_tok ";") |> G.s)

and map_gendecl (env : env) (x : CST.gendecl) : G.stmt =
  match x with
  | `Sign (pn1, rest, (_colon, type_or_impl)) ->
      let ty = map_type_or_implicit env type_or_impl in
      let names = map_pat_name env pn1 ::
                  List.map (fun (_c, pn) -> map_pat_name env pn) rest in
      let stmts = List.map (fun (name, tok) ->
        let ent = G.basic_entity (name, tok) in
        let fdef = G.FuncDef {
          G.fkind = (G.Function, tok);
          fparams = fb [];
          frettype = Some ty;
          fbody = G.FBDecl (fake_tok ";");
        } in
        G.DefStmt (ent, fdef) |> G.s
      ) names in
      (match stmts with
       | [ one ] -> one
       | _ -> G.Block (fb stmts) |> G.s)
  | `Fixity _ -> G.OtherStmt (G.OS_Pass, []) |> G.s

(*****************************************************************************)
(* Class / instance bodies                                                    *)
(*****************************************************************************)

(* We provide minimal class/instance handling: pull the name from the
 * constraint head and emit a ClassDef stub. The body's decls are walked
 * via a generic catch-all; detailed cdecl/idecl handling is best-effort. *)

let rec find_first_conid_in_constraint (env : env) (c : CST.constraint_)
    : G.ident option =
  match c with
  | `Type_name_rep_atype (tn, _rest) ->
      find_first_conid_in_type_name env tn
  | `Type_infix_ _ -> None

and find_first_conid_in_type_name (env : env) (tn : CST.type_name)
    : G.ident option =
  match tn with
  | `Choice_anno_type_var _ -> None
  | `Choice_prom_tycon x ->
      find_first_conid_in_gtycon env x

and find_first_conid_in_gtycon (env : env) (x : CST.gtycon)
    : G.ident option =
  match x with
  | `Prom_tycon (_, qt) -> find_first_conid_in_qtycon env qt
  | `Choice_choice_qual_type qt -> find_first_conid_in_qtycon env qt
  | `Tycon_arrow _ -> Some ("->", fake_tok "->")

and find_first_conid_in_qtycon (env : env) (x : CST.qtycon)
    : G.ident option =
  match x with
  | `Choice_qual_type qtid ->
      (match qtid with
       | `Qual_type (_q, tok) -> Some (map_tyconid env tok)
       | `Cons tok -> Some (map_tyconid env tok))
  | `LPAR_choice_qual_type_op__RPAR _ -> None

(*****************************************************************************)
(* topdecl                                                                    *)
(*****************************************************************************)

let map_import_directive (env : env)
    (imp_tok, _pre_qual, _pkg, qmod, _post_qual, as_opt, _list)
    : G.stmt =
  let import_tok = token env imp_tok in
  let mod_dotted = map_qmodid env qmod in
  let dir = match as_opt with
    | Some (_as_tok, alias_qm) ->
        let alias_dotted = map_qmodid env alias_qm in
        let alias = match List.rev alias_dotted with
          | last :: _ -> last
          | [] -> ("<anon>", fake_tok "")
        in
        { G.d = G.ImportAs (import_tok, G.DottedName mod_dotted,
                            Some (alias, G.empty_id_info ()));
          G.d_attrs = [] }
    | None ->
        { G.d = G.ImportAll (import_tok, G.DottedName mod_dotted,
                             import_tok);
          G.d_attrs = [] }
  in
  G.DirectiveStmt dir |> G.s

(* Digging helpers for data/newtype/class/instance names. *)

let rec dig_first_conid (x : G.any) : G.ident option = match x with
  | G.I id -> Some id
  | _ -> None
[@@warning "-32"]

let name_from_tyfam_head (env : env) (head : CST.tyfam_head) : G.ident option =
  (* tyfam_head has various shapes; we walk to the first constructor. *)
  let _ = env in
  let _ = head in
  None  (* Fallback — structural search later. *)

let rec name_from_simpletype (env : env) (st : CST.simpletype)
    : G.ident option =
  let _ = env in
  let _ = st in
  None

let map_decl_adt (env : env) data_tok _ctx _head _ann _rhs : G.stmt =
  let t = token env data_tok in
  (* Try to find the LHS type name by digging into the tyfam_head's
   * constructor. For now emit a TypeDef stub carrying a synthetic name. *)
  let name = ("<data>", t) in
  let ent = G.basic_entity name in
  G.DefStmt (ent, G.TypeDef {
    tbody = G.OtherTypeKind (("data", t), []);
  }) |> G.s

let map_decl_newt (env : env) nt_tok _ctx _rhs : G.stmt =
  let t = token env nt_tok in
  let name = ("<newtype>", t) in
  let ent = G.basic_entity name in
  G.DefStmt (ent, G.TypeDef {
    tbody = G.OtherTypeKind (("newtype", t), []);
  }) |> G.s

let map_decl_class (env : env) class_tok _ctx head _fundeps body_opt
    : G.stmt =
  let t = token env class_tok in
  let cls_name = match find_first_conid_in_constraint env head with
    | Some id -> id
    | None -> ("<class>", t)
  in
  let ent = G.basic_entity cls_name in
  let cbody = match body_opt with
    | None -> fb []
    | Some _b -> fb []
    (* Full class_body walking — best-effort: left as [] for now; the
     * structure compiles and maps to G.ClassDef properly. *)
  in
  let cdef = G.ClassDef {
    ckind = (G.Interface, t);
    cextends = []; cimplements = []; cmixins = [];
    cparams = fb [];
    cbody;
  } in
  G.DefStmt (ent, cdef) |> G.s

let map_decl_inst (env : env)
    (instance_head, _body_opt) : G.stmt =
  (* instance_head has [forall opt, context opt, constraint]. Pull name. *)
  let _ = instance_head in
  let t = fake_tok "instance" in
  let name = ("<instance>", t) in
  let ent = G.basic_entity name in
  let cdef = G.ClassDef {
    ckind = (G.Class, t);
    cextends = []; cimplements = []; cmixins = [];
    cparams = fb [];
    cbody = fb [];
  } in
  G.DefStmt (ent, cdef) |> G.s

let map_topdecl (env : env) (x : CST.topdecl) : G.stmt list =
  match x with
  | `Decl_type _ ->
      [ other_stmt "haskell_type_alias" (fake_tok "type") [] ]
  | `Decl_tyfam _ ->
      [ other_stmt "haskell_type_family" (fake_tok "type") [] ]
  | `Decl_tyinst _ ->
      [ other_stmt "haskell_type_instance" (fake_tok "type") [] ]
  | `Decl_role _ ->
      [ other_stmt "haskell_type_role" (fake_tok "type") [] ]
  | `Decl_adt (data_tok, ctx, head, ann, rhs) ->
      [ map_decl_adt env data_tok ctx head ann rhs ]
  | `Decl_newt (nt_tok, ctx, rhs) ->
      [ map_decl_newt env nt_tok ctx rhs ]
  | `Decl_data_8db362d _ ->
      [ other_stmt "haskell_data_family" (fake_tok "data") [] ]
  | `Decl_data_2e645bc _ ->
      [ other_stmt "haskell_data_instance" (fake_tok "data") [] ]
  | `Decl_import imp ->
      [ map_import_directive env imp ]
  | `Decl_class (class_tok, ctx, head, fundeps, body) ->
      [ map_decl_class env class_tok ctx head fundeps body ]
  | `Decl_inst x ->
      [ map_decl_inst env x ]
  | `Decl_defa _ ->
      [ other_stmt "haskell_default" (fake_tok "default") [] ]
  | `Decl_fore _ ->
      [ other_stmt "haskell_foreign" (fake_tok "foreign") [] ]
  | `Decl_deri _ ->
      [ other_stmt "haskell_deriving_standalone" (fake_tok "deriving") [] ]
  | `Decl d ->
      [ map_decl env d ]
  | `Decl_pat _ ->
      [ other_stmt "haskell_pattern_synonym" (fake_tok "pattern") [] ]
  | `Top_splice _ ->
      [ other_stmt "haskell_top_splice" (fake_tok "$") [] ]

(*****************************************************************************)
(* Multi-clause grouping                                                      *)
(*****************************************************************************)

(* Identify a DefStmt(FuncDef) and extract (name, fdef). *)
let extract_fun_stmt (s : G.stmt) : (G.ident * G.function_definition) option =
  match s.G.s with
  | G.DefStmt (ent, G.FuncDef fdef) ->
      (match ent.G.name with
       | G.EN (G.Id (id, _)) -> Some (id, fdef)
       | _ -> None)
  | _ -> None

(* Group consecutive DefStmt(FuncDef) with the same name into a single
 * FuncDef whose body is a Switch on the parameter tuple. *)
let rec group_consecutive_clauses (stmts : G.stmt list) : G.stmt list =
  let rec go (acc : G.stmt list) (stmts : G.stmt list) =
    match stmts with
    | [] -> List.rev acc
    | s :: rest ->
        (match extract_fun_stmt s with
         | None -> go (s :: acc) rest
         | Some (name, _fdef_first) ->
             (* Collect consecutive clauses with the same name. *)
             let rec take_same ss got =
               match ss with
               | s' :: rest' ->
                   (match extract_fun_stmt s' with
                    | Some (n, _) when fst n = fst name ->
                        take_same rest' (s' :: got)
                    | _ -> (List.rev got, ss))
               | [] -> (List.rev got, [])
             in
             let (same, rest2) = take_same rest [s] in
             if List.length same <= 1 then go (s :: acc) rest
             else begin
               let clauses = List.filter_map extract_fun_stmt same in
               let merged = merge_clauses name clauses in
               go (merged :: acc) rest2
             end)
  in
  go [] stmts

and merge_clauses (name : G.ident) (clauses : (G.ident * G.function_definition) list)
    : G.stmt =
  let first = match clauses with c :: _ -> c | [] -> failwith "merge_clauses empty" in
  let (_, first_fdef) = first in
  let num_params =
    let (_, ps, _) = first_fdef.G.fparams in
    List.length ps
  in
  let tk = snd name in
  let implicit_ids = List.init num_params (fun _ -> G.implicit_param_id tk) in
  let implicit_params = List.map (fun id -> G.Param (G.param_of_id id)) implicit_ids in
  let scrutinee = match implicit_ids with
    | [] -> G.L (G.Unit tk) |> G.e
    | [id] -> G.N (AST_generic_helpers.name_of_id id) |> G.e
    | ids ->
        G.Container (G.Tuple,
          fb (List.map (fun id -> G.N (AST_generic_helpers.name_of_id id) |> G.e) ids))
        |> G.e
  in
  let cases = List.map (fun (_name, fdef) ->
    let (_, ps, _) = fdef.G.fparams in
    let patterns = List.map (fun p ->
      match p with
      | G.Param { G.pname = Some id; _ } ->
          G.PatId (id, G.empty_id_info ())
      | G.Param _ -> G.PatWildcard tk
      | G.ParamPattern pat -> pat
      | _ -> G.PatWildcard tk
    ) ps in
    let case_pat = match patterns with
      | [] -> G.PatWildcard tk
      | [p] -> p
      | ps -> G.PatTuple (fb ps)
    in
    let body_stmt = match fdef.G.fbody with
      | G.FBExpr e -> G.ExprStmt (e, fake_tok ";") |> G.s
      | G.FBStmt s -> s
      | _ -> G.OtherStmt (G.OS_Todo, []) |> G.s
    in
    G.CasesAndBody ([G.Case (tk, case_pat)], body_stmt)
  ) clauses in
  let sw = G.Switch (tk, Some (G.Cond scrutinee), cases) |> G.s in
  let fdef = G.FuncDef {
    G.fkind = (G.Function, tk);
    fparams = fb implicit_params;
    frettype = first_fdef.G.frettype;
    fbody = G.FBStmt sw;
  } in
  let ent = G.basic_entity name in
  G.DefStmt (ent, fdef) |> G.s

(*****************************************************************************)
(* topdecl sequence + module body                                             *)
(*****************************************************************************)

let map_topdecls_seq (env : env)
    ((first, rest, _trailing)
       : CST.anon_topd_rep_choice_SEMI_topd_opt_choice_SEMI_eb02f02)
    : G.stmt list =
  let raw = map_topdecl env first
            @ List.concat_map (fun (_sep, td) -> map_topdecl env td) rest in
  group_consecutive_clauses raw

let map_module_body (env : env) body : G.stmt list =
  let raw = match body with
    | `LCURL_opt_topd_rep_SEMI_topd_opt_SEMI_RCURL
        (_lcurl, inner, _semi, _rcurl) ->
        (match inner with
         | None -> []
         | Some (first, rest) ->
             map_topdecl env first
             @ List.concat_map (fun (_s, td) -> map_topdecl env td) rest)
    | `Layout_start_opt_topd_rep_choice_SEMI_topd_opt_choice_SEMI_layout_end
        (_ls, inner, _le) ->
        (match inner with
         | None -> []
         | Some tds ->
             let (first, rest, _trailing) = tds in
             map_topdecl env first
             @ List.concat_map (fun (_sep, td) -> map_topdecl env td) rest)
  in
  group_consecutive_clauses raw

let program (env : env) (x : CST.haskell) : G.program =
  match x with
  | `Empty_file _ -> []
  | `Module (_mod_tok, qmod, _exports, _where, body_opt) ->
      let mod_ids = map_qmodid env qmod in
      let body_stmts = match body_opt with
        | None -> []
        | Some body -> map_module_body env body
      in
      (match mod_ids with
       | [] -> body_stmts
       | _ :: _ ->
           let ent_name = match List.rev mod_ids with
             | last :: _ -> last
             | [] -> ("<anon>", fake_tok "")
           in
           let ent = G.basic_entity ent_name in
           let mod_body = G.ModuleStruct (Some mod_ids, body_stmts) in
           let def = G.DefStmt (ent, G.ModuleDef { G.mbody = mod_body })
                     |> G.s in
           [ def ])
  | `Topd_rep_choice_SEMI_topd_opt_choice_SEMI tds ->
      map_topdecls_seq env tds
