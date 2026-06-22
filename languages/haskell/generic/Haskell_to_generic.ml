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

[@@@warning "-27-39"]

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

(* Lift a [G.function_body] back into an [G.expr] for contexts (let/in,
 * comprehension qualifiers, function-pattern bindings) that demand an
 * expression. We cover the four function_body cases explicitly so we
 * never fall through to a tagged sentinel. *)
let expr_of_funbody (b : G.function_body) : G.expr =
  match b with
  | G.FBExpr e -> e
  | G.FBStmt s -> G.StmtExpr s |> G.e
  | G.FBDecl _sc -> G.L (G.Unit (fake_tok "()")) |> G.e
  | G.FBNothing -> G.L (G.Null (fake_tok "")) |> G.e

(* In pattern mode, an absent body for a stmt-level construct like
 * `module $M where`, `class $C a where` or `instance $C $T where` should
 * mean "match any body". We model that by injecting a single ExprStmt
 * Ellipsis into the body — m_stmts_deep / m_fields treat it as a wildcard
 * over the target list (lines 2520/3404 in Generic_vs_generic.ml). *)
let ellipsis_body_stmt () : G.stmt =
  let ell = G.Ellipsis (fake_tok "...") |> G.e in
  G.ExprStmt (ell, fake_tok ";") |> G.s

let pattern_body_or_ellipsis (env : env) (stmts : G.stmt list) : G.stmt list =
  if env.H.extra.is_pattern_mode && stmts = [] then
    [ ellipsis_body_stmt () ]
  else stmts

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

let resolved_ident (env : env) (tok : Tree_sitter_run.Token.t) : G.ident =
  let (raw, t) = str env tok in
  let resolved = resolve_text env raw in
  (resolved, t)

(*****************************************************************************)
(* Basic tokens: namespace, arrows, etc.                                      *)
(*****************************************************************************)

let map_arrow (env : env) (x : CST.arrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok -> token env tok
  | `DASHGT tok -> token env tok

let map_unboxed_open (env : env) (x : CST.unboxed_open) : Tok.t =
  match x with
  | `LPARHASHSPACE tok -> token env tok
  | `LPARHASHLF tok -> token env tok

let map_forall_kw (env : env) (x : CST.forall_kw) : Tok.t =
  match x with
  | `Forall tok -> token env tok
  | `UNKUNKUNK tok -> token env tok

let map_larrow (env : env) (x : CST.larrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok -> token env tok
  | `LTDASH tok -> token env tok

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
  | [] -> raise Common.Impossible
  | [ id ] -> G.N (G.Id (id, G.empty_id_info ())) |> G.e
  | first :: rest ->
      List.fold_left (fun acc (name, tok) ->
        G.DotAccess (acc, tok,
          G.FN (G.Id ((name, tok), G.empty_id_info ()))) |> G.e
      ) (G.N (G.Id (first, G.empty_id_info ())) |> G.e) rest

let ident_of_idents (ids : G.ident list) : G.ident =
  match ids with
  | [] -> raise Common.Impossible
  | [ id ] -> id
  | (_, tok) :: _ ->
      let text = String.concat "." (List.map fst ids) in
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

(* Strip the surrounding double quotes from a Haskell string literal
 * token. Tree-sitter delivers the raw `"hello"` (including quotes) so
 * we trim them to match the convention used by Python and friends:
 * G.String contents do not include the quotes. *)
let strip_string_quotes (raw : string) : string =
  let n = String.length raw in
  if n >= 2 && raw.[0] = '"' && raw.[n - 1] = '"' then
    String.sub raw 1 (n - 2)
  else raw

(* Strip the surrounding single quotes from a char literal `'a'` -> `a`,
 * matching the quote-free convention used for strings. *)
let strip_char_quotes (raw : string) : string =
  let n = String.length raw in
  if n >= 2 && raw.[0] = '\'' && raw.[n - 1] = '\'' then
    String.sub raw 1 (n - 2)
  else raw

let map_stringly (env : env) (x : CST.stringly) : G.literal =
  match x with
  | `Str tok ->
      let (raw, t) = str env tok in
      let content = strip_string_quotes raw in
      G.String (fake_tok "\"", (content, t), fake_tok "\"")
  | `Char tok ->
      let (raw, t) = str env tok in
      G.Char (strip_char_quotes raw, t)

let map_literal (env : env) (x : CST.literal) : G.literal =
  match x with
  | `Choice_int x -> map_number env x
  | `Choice_str x -> map_stringly env x

let map_con_unit (env : env) ((l, _r) : CST.con_unit) : G.expr =
  G.L (G.Unit (token env l)) |> G.e

let map_con_list (env : env) ((l, r) : CST.con_list) : G.expr =
  let lt = token env l in
  let rt = token env r in
  G.Container (G.List, (lt, [], rt)) |> G.e

let map_con_tuple (env : env) (x : CST.con_tuple) : G.expr =
  let (lparen, commas, _rparen) = x in
  let lpar_tok = token env lparen in
  (* Build the textual tag from the comma count: 1 comma => "(,)",
   * 2 commas => "(,,)", etc. The arity of the tuple constructor is
   * (#commas + 1). Expose it via a single ident payload so patterns
   * can match e.g. `(,,)`. *)
  let n = List.length commas in
  let tag = "(" ^ String.make n ',' ^ ")" in
  other_expr "con_tuple" lpar_tok [G.I (tag, lpar_tok)]

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
  let (header, body_opt, _close) = x in
  let (start_tok, quoter_ids) = match header with
    | `UNKUNKUNK tok -> (token env tok, [])
    | `Quas_start_opt_choice_qual_var_quas_bar (open_tok, q_opt, _bar) ->
        let toks = token env open_tok in
        let qids = match q_opt with
          | None -> []
          | Some q -> map_qvarid env q
        in
        (toks, qids)
  in
  let args = (List.map (fun id -> G.I id) quoter_ids) in
  let args = match body_opt with
    | None -> args
    | Some body_tok -> args @ [ G.Tk (token env body_tok) ]
  in
  other_expr "quasiquote" start_tok args

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

(* Standard Haskell Prelude fixity table: (precedence 0..9, associativity).
 * Used to re-associate infix chains since tree-sitter-haskell ignores
 * fixities. User-declared operators default to `infixl 9`, matching GHC. *)
let haskell_op_fixity (name : string)
    : int * [ `Left | `Right | `Non ] =
  match name with
  | "$" | "$!" | "seq" -> (0, `Right)
  | ">>" | ">>=" -> (1, `Left)
  | "=<<" -> (1, `Right)
  | "||" -> (2, `Right)
  | "&&" -> (3, `Right)
  | "==" | "/=" | "<" | "<=" | ">=" | ">"
  | "elem" | "notElem" -> (4, `Non)
  | ":" | "++" -> (5, `Right)
  | "+" | "-" -> (6, `Left)
  | "*" | "/" | "div" | "mod" | "rem" | "quot" -> (7, `Left)
  | "^" | "^^" | "**" -> (8, `Right)
  | "." -> (9, `Right)
  | "!!" -> (9, `Left)
  | _ -> (9, `Left)

(* The textual name of an operator from its (possibly qualified) ident
 * list, e.g. [("Data",_); ("List",_); ("++",_)] -> "++". *)
let op_name (op_ids : G.ident list) : string =
  match List.rev op_ids with
  | (n, _) :: _ -> n
  | [] -> ""

(* Map the standard Prelude operators to AST_generic's `operator` enum so
 * they are emitted as Call(IdSpecial(Op _)), like Python/JS/Java/Go. This
 * unlocks the matcher's operator equivalences (associative-commutative
 * matching for && / || / ++, symmetric equality, etc.) which only fire on
 * IdSpecial(Op _). User-defined operators (<>, >>=, $, ., ...) have no Op
 * counterpart and stay Call(N(Id)). *)
let haskell_op_to_special (name : string) : G.operator option =
  match name with
  | "+" -> Some G.Plus
  | "-" -> Some G.Minus
  | "*" -> Some G.Mult
  | "/" -> Some G.Div
  | "div" -> Some G.FloorDiv
  | "mod" | "rem" -> Some G.Mod
  | "^" | "^^" | "**" -> Some G.Pow
  | "==" -> Some G.Eq
  | "/=" -> Some G.NotEq
  | "<" -> Some G.Lt
  | "<=" -> Some G.LtE
  | ">" -> Some G.Gt
  | ">=" -> Some G.GtE
  | "&&" -> Some G.And
  | "||" -> Some G.Or
  | "++" -> Some G.Concat
  | _ -> None

let rec map_exp (env : env) ((splice, ann) : CST.exp) : G.expr =
  let e = map_top_splice env splice in
  match ann with
  | None -> e
  | Some (_colon, type_or_impl) ->
      let ty = map_type_or_implicit env type_or_impl in
      G.Cast (ty, fake_tok "::", e) |> G.e

and map_top_splice (env : env) (x : CST.top_splice) : G.expr =
  map_exp_infix env x

(* tree-sitter-haskell does not know operator fixities (they are
 * user-declared in Haskell), so it parses every infix chain
 * left-associatively. We re-associate the flattened chain using the
 * standard Prelude fixity table via precedence climbing, so
 * `a + b * c` becomes `a + (b * c)` and `f $ x ++ y` becomes
 * `f $ (x ++ y)`. *)
and map_exp_infix (env : env) (x : CST.exp_infix) : G.expr =
  match x with
  | `Lexp x -> map_lexp env x
  | `Exp_infix_ _ ->
      (* Flatten the left-nested chain into a leftmost operand plus a list
       * of (operator, right-operand) pairs in source order. *)
      let rec flatten (x : CST.exp_infix) acc =
        match x with
        | `Exp_infix_ (lhs, qop, rhs) ->
            flatten lhs ((map_qop env qop, rhs) :: acc)
        | `Lexp l -> (l, acc)
      in
      let (first, pairs) = flatten x [] in
      let operands =
        Array.of_list (map_lexp env first
                       :: List.map (fun (_op, r) -> map_lexp env r) pairs) in
      let ops = Array.of_list (List.map (fun (op, _r) -> op) pairs) in
      let combine op_ids lhs rhs =
        (* `f $ x` / `f $! x` desugar to application `f x`. *)
        match op_ids with
        | [ ("$", _) ] | [ ("$!", _) ] -> G.Call (lhs, fb [ G.Arg rhs ]) |> G.e
        | _ ->
            (match haskell_op_to_special (op_name op_ids) with
             | Some op ->
                 let tok = (match op_ids with (_, t) :: _ -> t
                            | [] -> fake_tok (op_name op_ids)) in
                 G.Call (G.IdSpecial (G.Op op, tok) |> G.e,
                         fb [ G.Arg lhs; G.Arg rhs ]) |> G.e
             | None ->
                 G.Call (expr_of_idents op_ids, fb [ G.Arg lhs; G.Arg rhs ]) |> G.e)
      in
      let pos = ref 0 in
      let n_ops = Array.length ops in
      let rec climb min_prec =
        let result = ref operands.(!pos) in
        let continue = ref true in
        while !continue && !pos < n_ops
              && (let (p, _) = haskell_op_fixity (op_name ops.(!pos)) in
                  p >= min_prec) do
          let op = ops.(!pos) in
          let (prec, assoc) = haskell_op_fixity (op_name op) in
          incr pos;
          let next_min = match assoc with
            | `Right -> prec
            | `Left | `Non -> prec + 1
          in
          let rhs = climb next_min in
          result := combine op !result rhs
        done;
        !result
      in
      climb 0

and map_lexp (env : env) (x : CST.lexp) : G.expr =
  match x with
  | `Exp_let_in x -> map_exp_let_in env x
  | `Exp_cond x -> map_exp_cond env x
  | `Exp_if_guard (if_tok, gdpats) ->
      let t = token env if_tok in
      let branches = List.map (map_gdpat env) gdpats in
      (* Inert sentinel as the fold base: G.Null doesn't match anything
       * in the matcher, which is exactly what we want for an absent
       * else branch. *)
      let base = G.L (G.Null t) |> G.e in
      List.fold_right (fun (guard, body) acc ->
        G.Conditional (guard, body, acc) |> G.e
      ) branches base
  | `Exp_case x -> map_exp_case env x
  | `Exp_nega (minus, rhs) ->
      let mt = token env minus in
      let r = map_aexp env rhs in
      (* Keep unary negation as a plain Id "-" call: emitting
       * IdSpecial(Op Minus) triggers the matcher's `-n` <-> negative-literal
       * equivalence, which makes a `-$X` pattern over-match positive
       * literals. *)
      G.Call (G.N (G.Id (("-", mt), G.empty_id_info ())) |> G.e,
              fb [ G.Arg r ]) |> G.e
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
  | [] -> raise Common.Impossible
  | [ single ] -> single
  | func :: args ->
      (match func.G.e, args with
       (* `<... e ...>` deep ellipsis was rewritten to the marker
        * `__semgrep_deep__ (e)` at preprocessing time. *)
       | G.N (G.Id (("<...>", t), _)), [ e ] ->
           G.DeepEllipsis (t, e, t) |> G.e
       | _ ->
           (* Curry: one Call per argument — matches the raw walker's
            * convention so patterns against `f x y` match structurally. *)
           List.fold_left (fun acc arg ->
             G.Call (acc, fb [G.Arg arg]) |> G.e
           ) func args)

and map_aexp (env : env) (x : CST.aexp) : G.expr =
  match x with
  | `Exp_name n -> map_exp_name env n
  | `Exp_parens (_, exp, _) -> map_exp env exp
  | `Exp_tuple_ (l, tup, r) ->
      let (exprs, has_hole) = map_exp_tuple env tup in
      let tuple =
        G.Container (G.Tuple, (token env l, exprs, token env r)) |> G.e in
      (* A tuple with a hole is a TupleSections section (a function),
       * not a tuple value — tag it so it is not confused with one. *)
      if has_hole then other_expr "tuple_section" (token env l) [ G.E tuple ]
      else tuple
  | `Exp_list (l, first, rest, r) ->
      let first = map_exp env first in
      let rest = List.map (fun (_c, e) -> map_exp env e) rest in
      G.Container (G.List, (token env l, first :: rest, token env r)) |> G.e
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
  | `Exp_record (base, l, first, rest, r) ->
      (* Both record construction `Foo { a = 1 }` and record update
       * `r { a = 2 }` are lowered uniformly to `Call(base, [ArgKwd ...])`.
       * Keeping one shape lets a pattern with a metavariable base such as
       * `$C { $F = $V }` match either form (the base metavar binds the
       * constructor or the updated expression indifferently), and each
       * field becomes a keyword argument so `Foo { password = $X }` matches
       * structurally. *)
      let base_e = map_aexp env base in
      let lb = token env l in
      let rb = token env r in
      let args = map_exp_field_arg env first ::
                 List.map (fun (_c, f) -> map_exp_field_arg env f) rest in
      G.Call (base_e, (lb, args, rb)) |> G.e
  | `Exp_arit_seq (_l, first, mid, _dd, end_opt, _r) ->
      let first_e = map_exp env first in
      (* `[1,3..9]` carries a step element (the `3`). Keep it. *)
      let step_es = match mid with
        | None -> []
        | Some (_c, e) -> [ map_exp env e ]
      in
      let end_e = match end_opt with
        | None -> other_expr "arith_seq_infinite" (fake_tok "..") []
        | Some e -> map_exp env e
      in
      other_expr "arith_seq" (fake_tok "..")
        [G.E (G.Container (G.List, fb (first_e :: step_es @ [end_e])) |> G.e)]
  | `Exp_list_comp (_l, base, _bar, q1, qs, _r) ->
      let base = map_exp env base in
      let quals = q1 :: List.map snd qs in
      let comps = List.filter_map (map_qual_to_comp env) quals in
      G.Comprehension (G.List, fb (base, comps)) |> G.e
  | `Exp_sect_left (l, e, op, _r) ->
      let le = map_top_splice env e in
      let op_e = expr_of_idents (map_qop env op) in
      other_expr "left_section" (token env l) [G.E le; G.E op_e]
  | `Exp_sect_right (l, op, e, _r) ->
      let op_e = expr_of_idents (map_qop_nominus env op) in
      let re = map_top_splice env e in
      other_expr "right_section" (token env l) [G.E op_e; G.E re]
  | `Exp_unbo_tuple (l, inner_opt, _) ->
      let exprs = map_unboxed_tuple_exp_args env inner_opt in
      other_expr "unboxed_tuple" (map_unboxed_open env l)
        (List.map (fun e -> G.E e) exprs)
  | `Exp_unbo_sum_ (l, sum, _) ->
      let exprs = map_exp_unboxed_sum env sum in
      other_expr "unboxed_sum" (map_unboxed_open env l)
        (List.map (fun e -> G.E e) exprs)
  | `Splice sp ->
      let t = tok_of_splice env sp in
      let inner = map_splice_payload env sp in
      other_expr "th_splice" t [G.E inner]
  | `Quas q -> map_quasiquote env q
  | `Lit_ l -> map_literal_ env l

and map_exp_name (env : env) (x : CST.exp_name) : G.expr =
  match x with
  | `Choice_choice_qual_var qv ->
      let ids = map_qvar env qv in
      let full = ident_of_idents ids in
      (* A bare `...` ellipsis was rewritten to a placeholder identifier at
       * preprocessing time; turn it back into G.Ellipsis here so it works
       * in argument / list / tuple / do-statement positions. *)
      if fst full = "..." then G.Ellipsis (snd full) |> G.e
      else if env.H.extra.is_pattern_mode && is_metavar (fst full) then
        G.N (G.Id (full, G.empty_id_info ())) |> G.e
      else expr_of_idents ids
  | `Qcon qc ->
      let ids = map_qcon env qc in
      (match ids with
       | [] -> raise Common.Impossible
       | [ id ] ->
           let resolved = resolve_text env (fst id) in
           if env.H.extra.is_pattern_mode && is_metavar resolved then
             G.N (G.Id (id, G.empty_id_info ())) |> G.e
           else if fst id = "True" || fst id = "False" then
             (* Lower the Bool data constructors to literals, like every
              * other language, so boolean constant propagation works. *)
             G.L (G.Bool (fst id = "True", snd id)) |> G.e
           else
             G.Constructor (G.Id (id, G.empty_id_info ()), fb []) |> G.e
       | _ :: _ -> expr_of_idents ids)
  | `Impl_parid tok ->
      let id = resolved_ident env tok in
      other_expr "implicit_parid" (snd id) [G.I id]
  | `Label tok ->
      let id = resolved_ident env tok in
      other_expr "label" (snd id) [G.I id]

(* Walk the inside of a Template Haskell splice `$x` or `$(...)`.
 * The splice is (dollar_tok, splice_exp) where splice_exp = Exp_name | Exp_parens. *)
and map_splice_payload (env : env) ((_dollar, sexp) : CST.splice) : G.expr =
  match sexp with
  | `Exp_name n -> map_exp_name env n
  | `Exp_parens (_, exp, _) -> map_exp env exp

and tok_of_splice (env : env) ((dollar, _sexp) : CST.splice) : Tok.t =
  token env dollar

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
           let body = expr_of_funbody (map_funrhs_to_body env rhs) in
           Some (pat, body)
       | _ -> None)
  | `Decl_fun (`Funpat (tp, rhs)) ->
      let pat = map_typed_pat env tp in
      let body = expr_of_funbody (map_funrhs_to_body env rhs) in
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

and map_exp_field_arg (env : env) (x : CST.exp_field) : G.argument =
  match x with
  | `DOTDOT dd_tok ->
      (* RecordWildCards `Foo {..}`. Preserve the real `..` token so
       * locations stay accurate. *)
      let t = token env dd_tok in
      G.OtherArg (("record_wildcard", t), [])
  | `Choice_choice_qual_var_opt_EQ_exp (qv, eq_exp) ->
      let ids = map_qvar env qv in
      let field_id =
        match List.rev ids with id :: _ -> id | [] -> ("", fake_tok "")
      in
      (match eq_exp with
       | Some (_eq, e) ->
           let v = map_exp env e in
           G.ArgKwd (field_id, v)
       | None ->
           (* NamedFieldPuns `Foo { a }` desugars to `a = a`. *)
           G.ArgKwd (field_id, expr_of_idents ids))

(* Returns the tuple elements and whether any position is a hole. A hole
 * is a TupleSections gap, e.g. `(,5)` or `(1,)`; we keep a placeholder so
 * the arity is preserved and the caller can tag it as a section. *)
and map_exp_tuple (env : env) ((first, rest) : CST.exp_tuple)
    : G.expr list * bool =
  let has_hole = ref false in
  let hole () =
    has_hole := true;
    other_expr "tuple_section_hole" (fake_tok ",") []
  in
  let collect_first = match first with
    | `Rep1_comma_exp (commas, e) ->
        (* N leading commas == N holes before `e`. *)
        List.map (fun _ -> hole ()) commas @ [ map_exp env e ]
    | `Exp_comma_opt_exp (e1, _c, e2_opt) ->
        map_exp env e1 ::
        [ (match e2_opt with None -> hole () | Some e -> map_exp env e) ]
  in
  let collect_rest = List.map (fun (_c, e_opt) ->
    match e_opt with None -> hole () | Some e -> map_exp env e
  ) rest in
  (collect_first @ collect_rest, !has_hole)

(* Helpers for unboxed tuple / sum walks. An absent slot in the CST
 * (e.g. `(# x | #)` has a None slot for the right alternative) is
 * surfaced as an Ellipsis expression / wildcard pattern so the payload
 * is never empty and downstream tooling can still see structure. *)
and map_exp_opt (env : env) (x : CST.exp option) : G.expr =
  match x with
  | Some e -> map_exp env e
  | None -> G.Ellipsis (fake_tok "...") |> G.e

and map_pat_opt (env : env) (x : CST.nested_pat option) : G.pattern =
  match x with
  | Some p -> map_nested_pat env p
  | None -> G.PatWildcard (fake_tok "_")

and map_unboxed_tuple_exp_args (env : env)
    (x : (CST.exp option * CST.anon_rep_comma_opt_exp_fc8072d) option)
    : G.expr list =
  match x with
  | None -> []
  | Some (first_opt, rest) ->
      map_exp_opt env first_opt ::
      List.map (fun (_c, e_opt) -> map_exp_opt env e_opt) rest

and map_exp_unboxed_sum (env : env) ((first_opt, rest) : CST.exp_unboxed_sum)
    : G.expr list =
  map_exp_opt env first_opt ::
  List.map (fun (_pipe, e_opt) -> map_exp_opt env e_opt) rest

and map_unboxed_tuple_pat_args (env : env)
    (x : (CST.nested_pat * (CST.comma * CST.nested_pat) list) option)
    : G.pattern list =
  match x with
  | None -> []
  | Some (first, rest) ->
      map_nested_pat env first ::
      List.map (fun (_c, np) -> map_nested_pat env np) rest

and map_pat_unboxed_sum (env : env) ((first_opt, rest) : CST.pat_unboxed_sum)
    : G.pattern list =
  map_pat_opt env first_opt ::
  List.map (fun (_pipe, np_opt) -> map_pat_opt env np_opt) rest

and map_unboxed_tuple_type_args (env : env)
    (x : (CST.type_or_implicit
          * (CST.comma * CST.type_or_implicit) list) option)
    : G.type_ list =
  match x with
  | None -> []
  | Some (first, rest) ->
      map_type_or_implicit env first ::
      List.map (fun (_c, t) -> map_type_or_implicit env t) rest

and map_type_unboxed_sum (env : env) ((first, rest) : CST.type_sum)
    : G.type_ list =
  map_type_or_implicit env first ::
  List.map (fun (_pipe, t) -> map_type_or_implicit env t) rest

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

(* Walk a TransformListComp clause. The CST has 3 variants:
 *   `then group by <exp> using <exp>` -> Then_exp_using_exp
 *   `then group using <exp>`           -> Then_exp_9b4d8a6
 *   `then <exp>`                       -> Then_exp_8bf9922
 * We expose the keyword token plus the sub-expressions as G.any so a
 * pattern targeting either function is reachable. *)
and map_transform (env : env) (t : CST.transform) : G.any list =
  match t with
  | `Then_exp_using_exp (kw, e1, using_kw, e2) ->
      let kw_tok = token env kw in
      let using_tok = token env using_kw in
      let e1_e = map_exp env e1 in
      let e2_e = map_exp env e2 in
      [G.Tk kw_tok; G.E e1_e; G.Tk using_tok; G.E e2_e]
  | `Then_exp_9b4d8a6 (kw, e) ->
      let kw_tok = token env kw in
      [G.Tk kw_tok; G.E (map_exp env e)]
  | `Then_exp_8bf9922 (kw, e) ->
      let kw_tok = token env kw in
      [G.Tk kw_tok; G.E (map_exp env e)]

and tok_of_transform (env : env) (t : CST.transform) : Tok.t =
  match t with
  | `Then_exp_using_exp (kw, _, _, _)
  | `Then_exp_9b4d8a6 (kw, _)
  | `Then_exp_8bf9922 (kw, _) -> token env kw

(* Lift a comprehension qualifier into the AST_generic comprehension
 * vocabulary: generator -> CompFor, guard expression -> CompIf,
 * TransformListComp `then ...` and `let` bindings -> CompIf carrying a
 * structured expression so their sub-expressions stay matchable. *)
and map_qual_to_comp (env : env) (x : CST.qual) : G.for_or_if_comp option =
  match x with
  | `Bind_pat (tp, la, e) ->
      let pat = map_typed_pat env tp in
      let e = map_exp env e in
      Some (G.CompFor (fake_tok "for", pat, map_larrow env la, e))
  | `Exp e ->
      let guard = map_exp env e in
      Some (G.CompIf (fake_tok "if", guard))
  | `Tran t ->
      let kw_tok = tok_of_transform env t in
      let payload = map_transform env t in
      Some (G.CompIf (kw_tok, other_expr "list_comp_transform" kw_tok payload))
  | `Let (let_tok, decls_opt) ->
      (* `[ ... | ..., let z = e, ... ]`: surface the bindings as a
       * CompIf wrapping a Seq of LetPatterns so `z` and `e` stay in the
       * AST (no for_or_if_comp slot exists for let). *)
      let bs = match decls_opt with
        | None -> [] | Some d -> map_decls_to_bindings env d in
      let es = List.map (fun (p, e) -> G.LetPattern (p, e) |> G.e) bs in
      let lt = token env let_tok in
      Some (G.CompIf (lt, G.Seq es |> G.e))

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

and map_alt (env : env) ((pat, variants, where) : CST.alt)
    : G.case_and_body =
  let t = fake_tok "" in
  let pattern = map_pat env pat in
  (* A case alternative may have its own `where` clause; prepend those
   * local definitions to the alternative's body. *)
  let body_of body_expr =
    match where_clause_stmts env where with
    | [] -> G.ExprStmt (body_expr, fake_tok ";") |> G.s
    | wstmts ->
        let body_stmt = G.ExprStmt (body_expr, fake_tok ";") |> G.s in
        G.Block (fb (wstmts @ [ body_stmt ])) |> G.s
  in
  match variants with
  | `Arrow_exp (_arr, exp) ->
      let body = map_exp env exp in
      G.CasesAndBody ([G.Case (t, pattern)], body_of body)
  | `Rep1_gdpat gdpats ->
      (* Build a Conditional chain from the gdpat list; wrap the
       * original pattern in PatWhen using the first guard's expression. *)
      let branches = List.map (map_gdpat env) gdpats in
      (* Inert sentinel base — G.Null is matcher-neutral. *)
      let base = G.L (G.Null t) |> G.e in
      let cond_expr = List.fold_right (fun (guard, body) acc ->
        G.Conditional (guard, body, acc) |> G.e
      ) branches base in
      let wrapped_pat = match branches with
        | (first_guard, _) :: _ -> G.PatWhen (pattern, first_guard)
        | [] -> pattern
      in
      G.CasesAndBody ([G.Case (t, wrapped_pat)], body_of cond_expr)

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
      let es = List.map (fun (p, e) ->
        G.LetPattern (p, e) |> G.e) bs in
      G.Seq es |> G.e
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
      if env.H.extra.is_pattern_mode && is_metavar text then
        (* In pattern mode, an uppercase metavariable that landed in a
         * Pat_cons position is still a metavariable — preserve it as
         * PatId so it binds to the corresponding pattern element. *)
        G.PatId (ctor_id, G.empty_id_info ())
      else if text = "True" || text = "False" then
        (* Match the expression-side lowering of Bool to a literal. *)
        G.PatLiteral (G.Bool (text = "True", tok))
      else
        G.PatConstructor (G.Id (ctor_id, G.empty_id_info ()), [])
  | `Pat_record (ctor, (lc, fields_opt, rc)) ->
      let ids = map_pat_constructor env ctor in
      let ctor_id = ident_of_idents ids in
      let lc_tok = token env lc in
      let rc_tok = token env rc in
      let fields = match fields_opt with
        | None -> []
        | Some (first, rest) ->
            map_pat_field env first
            :: List.map (fun (_c, f) -> map_pat_field env f) rest
      in
      let named =
        List.filter_map (function `Field fp -> Some fp | `Wild _ -> None) fields
      in
      let wild_toks =
        List.filter_map (function `Wild t -> Some t | `Field _ -> None) fields
      in
      (* Keep the constructor (so `Person{..}` differs from `Company{..}`) and
       * make the fields structurally matchable via PatRecord. A RecordWildCards
       * `..` becomes a trailing PatEllipsis over the remaining fields. *)
      let record_pat = G.PatRecord (lc_tok, named, rc_tok) in
      let args =
        record_pat
        :: (match wild_toks with t :: _ -> [ G.PatEllipsis t ] | [] -> [])
      in
      G.PatConstructor (G.Id (ctor_id, G.empty_id_info ()), args)
  | `Lit_ l ->
      (match l with
       | `Lit lit ->
           let lit = map_literal env lit in
           G.PatLiteral lit
       | `Choice_con_unit g ->
           (match g with
            | `Con_unit (l, _r) ->
                G.PatLiteral (G.Unit (token env l))
            | `Con_list (l, r) ->
                (* `[]` in pattern position is the empty list, like
                 * `[a, b]` -> PatList; keep them consistent. *)
                G.PatList (token env l, [], token env r)
            | `Con_tuple _ ->
                other_pat "pat_con_tuple" (fake_tok "(,)") []))
  | `Pat_wild tok -> G.PatWildcard (token env tok)
  | `Pat_parens (_, np, _) -> map_nested_pat env np
  | `Pat_tuple (_, np1, rest, _) ->
      let ps = map_nested_pat env np1 ::
               List.map (fun (_c, np) -> map_nested_pat env np) rest in
      G.PatTuple (fb ps)
  | `Pat_unbo_tuple (l, inner_opt, _) ->
      let pats = map_unboxed_tuple_pat_args env inner_opt in
      other_pat "pat_unboxed_tuple" (map_unboxed_open env l)
        (List.map (fun p -> G.P p) pats)
  | `Pat_unbo_sum_ (l, sum, _) ->
      let pats = map_pat_unboxed_sum env sum in
      other_pat "pat_unboxed_sum" (map_unboxed_open env l)
        (List.map (fun p -> G.P p) pats)
  | `Pat_list (_, np1, rest, _) ->
      let ps = map_nested_pat env np1 ::
               List.map (fun (_c, np) -> map_nested_pat env np) rest in
      G.PatList (fb ps)
  | `Pat_strict (bang, inner) ->
      let bang_tok = token env bang in
      let inner_p = map_apat env inner in
      other_pat "strict" bang_tok [G.P inner_p]
  | `Pat_irre (tilde, inner) ->
      let t = token env tilde in
      let inner_p = map_apat env inner in
      other_pat "irrefutable" t [G.P inner_p]
  | `Splice sp ->
      let t = tok_of_splice env sp in
      let inner = map_splice_payload env sp in
      other_pat "pat_splice" t [G.E inner]
  | `Quas q ->
      let qe = map_quasiquote env q in
      other_pat "pat_quasiquote" (fake_tok "[|") [G.E qe]

(* One field in a record pattern `Person{ name, age = a, .. }`:
 * - `{..}`        -> a record-wildcard marker
 * - `{ name }`    -> a field pun, binds the variable `name`
 * - `{ f = pat }` -> binds `pat`, tagged with the field name *)
and map_pat_field (env : env) (f : CST.pat_field)
    : [ `Field of G.dotted_ident * G.pattern | `Wild of Tok.t ] =
  match f with
  | `DOTDOT tok -> `Wild (token env tok)
  | `Choice_choice_qual_var_opt_EQ_nested_pat (qv, eq_opt) ->
      let fname = ident_of_idents (map_qvar env qv) in
      (match eq_opt with
       | None ->
           (* NamedFieldPuns `Person{ name }` binds the variable `name`. *)
           `Field ([ fname ], G.PatId (fname, G.empty_id_info ()))
       | Some (_eq, np) ->
           let p = map_nested_pat env np in
           `Field ([ fname ], p))

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
  | `Pat_typed (p, (_colon, ann)) ->
      let pat = map_pat env p in
      let ty = map_type_or_implicit env ann in
      G.PatTyped (pat, ty)

and map_fun_patterns (env : env) (pats : CST.fun_patterns)
    : G.parameter list =
  List.map (fun ap ->
    let p = map_apat env ap in
    match p with
    | G.PatId ((name, tok), _) ->
        G.Param (G.param_of_id (name, tok))
    | _ ->
        let tk =
          match AST_generic_helpers.ii_of_any (G.P p) with
          | t :: _ -> t
          | [] -> fake_tok ""
        in
        G.ParamPattern (p, G.implicit_param_classic tk)
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
      let (forall_tok, vars) = forall in
      let var_tys = List.map (fun tv -> G.T (map_tyvar env tv)) vars in
      let body_ty = map_type env inner in
      other_type "forall" (map_forall_kw env forall_tok)
        (G.T body_ty :: var_tys)
  | `Type_cont (ctx, inner) ->
      (* `(C a, D b) => T`: surface both the constraint set and the body,
       * with the real `=>` token. *)
      let (constraints, carrow) = ctx in
      let carrow_tok = match carrow with
        | `UNKUNKUNK tok -> token env tok
        | `EQGT tok -> token env tok
      in
      let constraint_tys = context_constraints_to_types env constraints in
      let body = map_type env inner in
      other_type "context" carrow_tok
        (G.T body :: List.map (fun t -> G.T t) constraint_tys)
  | `Type_fun (lhs, _arr, rhs) ->
      let l = map_type_infix env lhs in
      let r = map_type env rhs in
      let param = G.Param {
        G.pname = None; ptype = Some l; pdefault = None; pattrs = [];
        pinfo = G.empty_id_info ();
      } in
      { G.t = G.TyFun ([param], r); t_attrs = [] }
  | `Type_infix x -> map_type_infix env x

(* Walk a single type-class constraint `C a b` into a type. *)
and map_constraint_as_type (env : env) (c : CST.constraint_) : G.type_ =
  match c with
  | `Type_name_rep_atype (tn, atypes) ->
      let head = map_type_name env tn in
      (match atypes with
       | [] -> head
       | _ ->
           let args = List.map (fun a -> G.TA (map_atype env a)) atypes in
           { G.t = G.TyApply (head, fb args); t_attrs = [] })
  | `Type_infix_ (lhs, _op, rhs) ->
      let l = map_btype env lhs in
      let r = map_type_infix env rhs in
      { G.t = G.TyApply (l, fb [ G.TA r ]); t_attrs = [] }

(* `constraint__` wraps a constraint_ through optional forall / context /
 * parens. Descend to the inner constraint_. *)
and constraint__to_type (env : env) (c : CST.constraint__) : G.type_ =
  match c with
  | `Quan_cons (_, _, inner) -> constraint__to_type env inner
  | `Cons_cont (_, inner) -> constraint__to_type env inner
  | `LPAR_cons__RPAR (_, inner, _) -> constraint__to_type env inner
  | `Cons c -> map_constraint_as_type env c

(* Walk a context's constraint set into a list of types. *)
and context_constraints_to_types (env : env) (cc : CST.context_constraints)
    : G.type_ list =
  match cc with
  | `Cons c -> [ map_constraint_as_type env c ]
  | `LPAR_opt_choice_cons__rep_comma_choice_cons__RPAR (_, inner_opt, _) ->
      (match inner_opt with
       | None -> []
       | Some (first, rest) ->
           let walk ch = match ch with
             | `Cons_ c -> Some (constraint__to_type env c)
             | `Impl_param _ -> None
           in
           List.filter_map walk (first :: List.map (fun (_c, x) -> x) rest))

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
  | `Type_star ts ->
      let tok = match ts with
        | `STAR t -> token env t
        | `UNKUNKUNK t -> token env t
      in
      other_type "star" tok [G.I ("*", tok)]
  | `Type_lit_ lit ->
      (match lit with
       | `Type_prom_lit_c26b94c (tick, plit) ->
           (* DataKinds promoted literal: `'[a]`, `'(a, b)`, `'Con`. *)
           let tok = token env tick in
           other_type "type_literal" tok [G.E (map_type_promotable_literal env plit)]
       | `Type_prom_lit_af79c83 plit ->
           (* No tick: an ordinary list type `[a]` or tuple type `(a, b)`. *)
           map_promotable_literal_as_type env plit)
  | `Type_parens (_, t, _) -> map_type_or_implicit env t
  | `Type_unbo_tuple (l, inner_opt, _) ->
      let tys = map_unboxed_tuple_type_args env inner_opt in
      other_type "unboxed_tuple_type" (map_unboxed_open env l)
        (List.map (fun t -> G.T t) tys)
  | `Type_unbo_sum (l, sum, _) ->
      let tys = map_type_unboxed_sum env sum in
      other_type "unboxed_sum_type" (map_unboxed_open env l)
        (List.map (fun t -> G.T t) tys)
  | `Splice sp ->
      let t = tok_of_splice env sp in
      let inner = map_splice_payload env sp in
      other_type "type_splice" t [G.E inner]
  | `Quas q ->
      let qe = map_quasiquote env q in
      other_type "type_quasiquote" (fake_tok "[|") [G.E qe]

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

(* DataKinds-style promoted literals: `'Foo`, `'(...)`, `'[...]`, `'5`, etc.
 * Exposed as expressions so they live in the OtherType payload. *)
and map_type_promotable_literal (env : env)
    (x : CST.type_promotable_literal) : G.expr =
  match x with
  | `Type_lit lit ->
      (match lit with
       | `Lit l -> G.L (map_literal env l) |> G.e
       | `Con_unit cu -> map_con_unit env cu
       | `Con_list cl -> map_con_list env cl
       | `Con_tuple ct -> map_con_tuple env ct)
  | `Type_tuple_ (lt, (first, rest), rt) ->
      let lt = token env lt in
      let to_expr ty = other_expr "type_in_tuple" lt [G.T ty] in
      let f = to_expr (map_type_or_implicit env first) in
      let r = List.map (fun (_c, t) -> to_expr (map_type_or_implicit env t))
        rest in
      G.Container (G.Tuple, (lt, f :: r, token env rt)) |> G.e
  | `Type_list (lt, first, rest, rt) ->
      let lt = token env lt in
      let to_expr ty = other_expr "type_in_list" lt [G.T ty] in
      let f = to_expr (map_type_or_implicit env first) in
      let r = List.map (fun (_c, t) -> to_expr (map_type_or_implicit env t))
        rest in
      G.Container (G.List, (lt, f :: r, token env rt)) |> G.e

(* Tick-less `[a]` / `(a, b)` are ordinary list / tuple TYPES, not
 * DataKinds promoted literals. Represent them with TyArray / TyTuple. *)
and map_promotable_literal_as_type (env : env)
    (x : CST.type_promotable_literal) : G.type_ =
  match x with
  | `Type_tuple_ (lt, (first, rest), rt) ->
      let tys = map_type_or_implicit env first
                :: List.map (fun (_c, t) -> map_type_or_implicit env t) rest in
      { G.t = G.TyTuple (token env lt, tys, token env rt); t_attrs = [] }
  | `Type_list (lt, first, _rest, rt) ->
      (* A list type `[a]` has exactly one element type. *)
      let elem = map_type_or_implicit env first in
      { G.t = G.TyArray ((token env lt, None, token env rt), elem);
        t_attrs = [] }
  | `Type_lit lit ->
      (* `()`, `[]`, `(,)`, or a type-level literal. Surface as a type
       * expression. *)
      let e = match lit with
        | `Lit l -> G.L (map_literal env l) |> G.e
        | `Con_unit cu -> map_con_unit env cu
        | `Con_list cl -> map_con_list env cl
        | `Con_tuple ct -> map_con_tuple env ct
      in
      { G.t = G.TyExpr e; t_attrs = [] }

(*****************************************************************************)
(* funrhs / funlhs                                                            *)
(*****************************************************************************)

and map_funrhs_to_body (env : env) (rhs : CST.funrhs) : G.function_body =
  let (kind, where_opt) = rhs in
  let body_expr = match kind with
    | `EQ_exp (_eq, exp) -> map_exp env exp
    | `Fun_guards guards ->
        (* Build a Conditional chain from the guard equations. *)
        let branches = List.map (fun (gs, _eq, body) ->
          let guard = map_guards env gs in
          let b = map_exp env body in
          (guard, b)
        ) guards in
        (* Inert sentinel base — G.Null is matcher-neutral. *)
        let base = G.L (G.Null (fake_tok "")) |> G.e in
        List.fold_right (fun (g, b) acc ->
          G.Conditional (g, b, acc) |> G.e
        ) branches base
  in
  (* A `where` clause binds local definitions visible in the body. Wrap
   * the body in a Block: the where definitions become local DefStmts
   * (so functions keep their params), followed by the body expression. *)
  match where_clause_stmts env where_opt with
  | [] -> G.FBExpr body_expr
  | wstmts ->
      let body_stmt = G.ExprStmt (body_expr, fake_tok ";") |> G.s in
      G.FBStmt (G.Block (fb (wstmts @ [ body_stmt ])) |> G.s)

(* Walk an optional `where { decls }` into a list of statements. *)
and where_clause_stmts (env : env)
    (w : CST.anon_opt_where_opt_decls_4a349ec) : G.stmt list =
  match w with
  | None -> []
  | Some (_where, None) -> []
  | Some (_where, Some decls) -> map_decls_to_stmts env decls

(* Walk a `decls` block into a list of statements (each decl -> DefStmt),
 * unlike map_decls_to_bindings which produces (pattern, expr) pairs and
 * loses function parameters. *)
and map_decls_to_stmts (env : env) (d : CST.decls) : G.stmt list =
  match d with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL (_l, inner, _semi, _r) ->
      (match inner with
       | None -> []
       | Some (first, rest) ->
           map_decl env first
           :: List.map (fun (_s, d) -> map_decl env d) rest)
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI_layout_end
      (_l, inner, _le) ->
      (match inner with
       | None -> []
       | Some (first, rest, _trailing) ->
           map_decl env first
           :: List.map (fun (_s, d) -> map_decl env d) rest)

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
          | _ ->
              let tk =
                match AST_generic_helpers.ii_of_any (G.P p) with
                | t :: _ -> t
                | [] -> fake_tok ""
              in
              G.ParamPattern (p, G.implicit_param_classic tk)
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
           (* Pure value bindings (`foo = 42`, `key = "secret"`) are
            * emitted as VarDef instead of FuncDef so that the global
            * Naming_AST + Constant_propagation passes propagate their
            * svalue to use sites. Haskell top-level bindings are
            * immutable by nature, so we tag them with the Const
            * keyword attribute — that triggers
            * Constant_propagation.propagate_basic for any language
            * (see src/analyzing/Constant_propagation.ml:471). *)
           let (_, params, _) = fdef.G.fparams in
           (match params, fdef.G.fbody with
            | [], G.FBExpr e ->
                let const_attr = G.KeywordAttr (G.Const, snd name) in
                let ent = { (G.basic_entity name) with
                            G.attrs = [ const_attr ] } in
                let vdef = G.VarDef {
                  G.vinit = Some e;
                  vtype = fdef.G.frettype;
                  vtok = G.no_sc;
                } in
                G.DefStmt (ent, vdef) |> G.s
            | _ ->
                let ent = G.basic_entity name in
                G.DefStmt (ent, G.FuncDef fdef) |> G.s)
       | `Funpat (tp, rhs) ->
           let pat = map_typed_pat env tp in
           let body = expr_of_funbody (map_funrhs_to_body env rhs) in
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
  | `Fixity (assoc, prec_opt, op1, rest) ->
      let (kw, kw_tok) = match assoc with
        | `Infixl t -> ("infixl", token env t)
        | `Infixr t -> ("infixr", token env t)
        | `Infix t -> ("infix", token env t)
      in
      let assoc_id : G.ident = (kw, kw_tok) in
      let prec_args : G.any list = match prec_opt with
        | None -> []
        | Some n ->
            let lit = map_integer env n in
            [ G.E (G.L lit |> G.e) ]
      in
      let op_ids : G.ident list =
        map_op env op1 :: List.map (fun (_c, o) -> map_op env o) rest
      in
      let op_args : G.any list = List.map (fun id -> G.I id) op_ids in
      let payload = G.I assoc_id :: prec_args @ op_args in
      other_stmt "haskell_fixity" kw_tok payload

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

(* One name in an import list: `foo`, `Map`, `(<+>)`, etc. *)
let map_import_item (env : env) ((_ns, choice) : CST.import_item) : G.ident =
  match choice with
  | `Choice_var pn -> map_pat_name env pn
  | `Choice_cons_opt_import_con_names (sty, _con_names) ->
      (match sty with
       | `Cons tok -> map_tyconid env tok
       | `LPAR_type_op_RPAR (_, op, _) ->
           (match op with
            | `Tyco tok -> str env tok
            | `Cons_op tok -> str env tok))

(* Classify an import list. `import M (a, b)` is `Selective`; `import M
 * hiding (a, b)` is `Hiding` (import-all-except, which has no first-class
 * node so it is modelled as ImportAll plus the hidden names). Returns None
 * for an empty list. *)
let import_list_classify (env : env) (il : CST.import_list)
    : [ `Selective of G.ident list | `Hiding of G.ident list ] option =
  let (hiding_opt, _lpar, items_opt, _rpar) = il in
  let idents = match items_opt with
    | None -> []
    | Some (first, rest, _trailing) ->
        map_import_item env first
        :: List.map (fun (_c, it) -> map_import_item env it) rest
  in
  match hiding_opt with
  | Some _ -> Some (`Hiding idents)
  | None -> (match items_opt with None -> None | Some _ -> Some (`Selective idents))

let map_import_directive (env : env)
    (imp_tok, _pre_qual, _pkg, qmod, _post_qual, as_opt, list_opt)
    : G.stmt =
  let import_tok = token env imp_tok in
  let mod_dotted = map_qmodid env qmod in
  let modname = G.DottedName mod_dotted in
  let selected = match list_opt with
    | Some il -> import_list_classify env il
    | None -> None
  in
  let dir, d_attrs = match as_opt, selected with
    | Some (_as_tok, alias_qm), _ ->
        (* `import qualified M as N [(...)]`: the alias is what matters
         * for qualified-name matching; keep ImportAs. *)
        let alias_dotted = map_qmodid env alias_qm in
        let alias = match List.rev alias_dotted with
          | last :: _ -> last
          | [] -> raise Common.Impossible
        in
        (G.ImportAs (import_tok, modname, Some (alias, G.empty_id_info ())), [])
    | None, Some (`Selective idents) ->
        (* `import M (a, b, c)`: selective import. *)
        let named = List.map (fun id -> (id, None)) idents in
        (G.ImportFrom (import_tok, modname, named), [])
    | None, Some (`Hiding hidden) ->
        (* `import M hiding (a, b)`: the module is still imported wholesale,
         * minus a few names. There is no "all except" node, so keep
         * ImportAll but record the hidden names as an attribute so it stays
         * distinguishable from a plain `import M`. *)
        let anys = List.map (fun id -> G.I id) hidden in
        (G.ImportAll (import_tok, modname, import_tok),
         [ G.OtherAttribute (("import_hiding", import_tok), anys) ])
    | None, None ->
        (G.ImportAll (import_tok, modname, import_tok), [])
  in
  G.DirectiveStmt { G.d = dir; G.d_attrs } |> G.s

(* Digging helpers for data/newtype/class/instance names. *)

(* Pull a type-constructor name out of a tyfam_head (= simpletype).
 * Walks through the parens / infix / Choice_cons_rep_choice variants
 * and returns the first tyconid found. *)
let name_from_tyfam_head (env : env) (head : CST.tyfam_head) : G.ident option =
  let rec aux (st : CST.simpletype) =
    match st with
    | `LPAR_simp_RPAR (_, inner, _) -> aux inner
    | `Simp_infix (_lhs, op, _rhs) ->
        (match op with
         | `Ticked_tycon (_, tc, _) -> Some (map_tyconid env tc)
         | `Type_op _ -> None)
    | `Choice_cons_rep_choice_anno_type_var (head, _vars) ->
        (match head with
         | `Cons tok -> Some (map_tyconid env tok)
         | `LPAR_type_op_RPAR _ -> None)
  in
  aux head

let name_from_decl_newtype_head (env : env) (st : CST.simpletype)
    : G.ident option =
  name_from_tyfam_head env st

(* Walk a `!T` strict-type field, exposing the inner atype as a
 * structured argument so patterns over the underlying type still work. *)
let map_strict_type (env : env) ((bang, at) : CST.strict_type) : G.type_ =
  let bang_tok = token env bang in
  other_type "strict_type" bang_tok [G.T (map_atype env at)]

(* Extract the list of bound type variables from a tyfam_head / simpletype,
 * walking through parens. For infix shapes, return both lhs and rhs vars. *)
let vars_from_tyfam_head (env : env) (head : CST.tyfam_head) : G.type_ list =
  let rec aux (st : CST.simpletype) =
    match st with
    | `LPAR_simp_RPAR (_, inner, _) -> aux inner
    | `Simp_infix (lhs, _op, rhs) ->
        [ map_tyvar env lhs; map_tyvar env rhs ]
    | `Choice_cons_rep_choice_anno_type_var (_head, vars) ->
        List.map (fun tv -> map_tyvar env tv) vars
  in
  aux head

(* The type parameters of a `data`/`newtype`/`type` head as entity
 * tparams: `data Map k v` -> [k; v]. Mirrors what class declarations do. *)
let tparams_from_tyfam_head (env : env) (head : CST.tyfam_head)
    : G.type_parameter list =
  List.filter_map (fun ty ->
    match ty.G.t with
    | G.TyVar id -> Some (G.tparam_of_id id)
    | _ -> None
  ) (vars_from_tyfam_head env head)

(* Attach tparams to an entity, leaving it untouched when there are none. *)
let entity_with_tparams (base_ent : G.entity)
    (tparams : G.type_parameter list) : G.entity =
  match tparams with
  | [] -> base_ent
  | _ -> { base_ent with G.tparams = Some (Tok.unsafe_fake_bracket tparams) }

(* Map a Haskell role keyword to a G.ident with its real token. *)
let map_type_role (env : env) (x : CST.type_role) : G.ident =
  match x with
  | `Repr tok -> ("representational", token env tok)
  | `Nomi tok -> ("nominal", token env tok)
  | `Phan tok -> ("phantom", token env tok)
  | `X__ tok -> ("_", token env tok)

(* Build a left-hand-side TyApply for a type/data instance:
 *   F t1 t2 ...   -->   TyApply(TyN F, [t1; t2; ...])
 * If `atypes` is empty, just return TyN. *)
let inst_atypes_to_lhs (env : env) (name : G.ident)
    (atypes : CST.atype list) : G.type_ =
  let head_ty = { G.t = G.TyN (G.Id (name, G.empty_id_info ()));
                  t_attrs = [] } in
  match atypes with
  | [] -> head_ty
  | _ ->
      let args = List.map (fun a -> G.TA (map_atype env a)) atypes in
      { G.t = G.TyApply (head_ty, fb args); t_attrs = [] }

(* Walk a record-style field group `{ x, y :: T }`, returning each field
 * as an OtherType("record_field:NAME", [T inner_ty]) so patterns can
 * match field names + types structurally. *)
let record_fields_to_arg_types (env : env) (rf : CST.record_fields)
    : G.type_ list =
  let (_lc, first_field, rest_fields, _rc) = rf in
  let walk_field (vars1, varsN, _colon, ty_choice) =
    let ty = match ty_choice with
      | `Strict_type st -> map_strict_type env st
      | `Type t -> map_type env t
    in
    let var_names =
      map_variable env vars1
      :: List.map (fun (_c, v) -> map_variable env v) varsN
    in
    List.map (fun n -> (n, ty)) var_names
  in
  let all_fields =
    walk_field first_field
    @ List.concat_map (fun (_c, f) -> walk_field f) rest_fields
  in
  List.map (fun (n, ty) ->
    other_type ("record_field:" ^ fst n) (snd n) [G.T ty]
  ) all_fields

(* Walk a `data` constructor variant: yields the constructor ident and
 * the list of arg types. *)
let map_data_constructor (env : env) (x : CST.anon_choice_data_cons_3ed9ff3)
    : G.or_type_element =
  match x with
  | `Data_cons (tycon, args) ->
      let name = map_tyconid env tycon in
      let arg_types = List.map (fun a ->
        match a with
        | `Strict_type st -> map_strict_type env st
        | `Atype at -> map_atype env at
      ) args in
      G.OrConstructor (name, arg_types)
  | `Data_cons_infix (_lhs, op, _rhs) ->
      let op_id = match op with
        | `Cons_op tok -> str env tok
        | `BQUOT_cons_BQUOT (_, tc, _) -> map_tyconid env tc
      in
      G.OrConstructor (op_id, [])
  | `Data_cons_record (tycon, rec_fields) ->
      let name = map_tyconid env tycon in
      G.OrConstructor (name, record_fields_to_arg_types env rec_fields)

(* Walk an `adt_rhs = (=, constructors, deriving list)` and return the
 * list of OR-type variants. *)
let map_constructors (env : env) (cs : CST.constructors)
    : G.or_type_element list =
  (* The leading forall/context are existential quantification
   * (`forall a. Show a => ...`). OrConstructor has no slot for them, so
   * they are not represented; the constructor name + field types are. *)
  let (_existential_forall, _existential_ctx, first, rest) = cs in
  map_data_constructor env first
  :: List.map (fun (_pipe, _f, _c, c) -> map_data_constructor env c) rest

(* Walk a single deriving clause, returning the deriving keyword token
 * and the list of class names it mentions (e.g. `deriving (Show, Eq)`
 * -> [Show; Eq]). The token is reused as the NamedAttr position. *)
let map_deriving_class (env : env) (d : CST.deriving)
    : Tok.t * G.type_ option * G.ident list =
  (* `_strategy_opt` is the stock/newtype/anyclass keyword — metadata we
   * drop. `via_opt` carries the `via T` type for DerivingVia. *)
  let (deriving_tok, _strategy_opt, choice, via_opt) = d in
  let dt = token env deriving_tok in
  let via_ty = match via_opt with
    | None -> None
    | Some (_via_tok, ty) -> Some (map_type env ty)
  in
  let cons_to_id (qt : CST.qtyconid) : G.ident =
    match qt with
    | `Qual_type (_, tok) -> map_tyconid env tok
    | `Cons tok -> map_tyconid env tok
  in
  let rec constraint_to_id (c : CST.constraint__) : G.ident option =
    match c with
    | `Quan_cons (_, _, inner) -> constraint_to_id inner
    | `Cons_cont (_, inner) -> constraint_to_id inner
    | `LPAR_cons__RPAR (_, inner, _) -> constraint_to_id inner
    | `Cons c -> find_first_conid_in_constraint env c
  in
  let class_ids = match choice with
    | `Choice_qual_type qt -> [ cons_to_id qt ]
    | `LPAR_opt_cons__rep_comma_cons__RPAR (_, inner_opt, _) ->
        (match inner_opt with
         | None -> []
         | Some (first, rest) ->
             let acc = match constraint_to_id first with
               | Some id -> [ id ]
               | None -> []
             in
             let rest_ids = List.filter_map (fun (_c, x) ->
               constraint_to_id x
             ) rest in
             acc @ rest_ids)
  in
  (dt, via_ty, class_ids)

(* Convert a list of derivings into NamedAttr (e.g. @deriving Show)
 * for the entity's attrs slot. The NamedAttr token is the real
 * `deriving` keyword token; a `via T` type rides along as an ArgType. *)
let derivings_to_attrs (env : env) (derivings : CST.deriving list) : G.attribute list =
  List.concat_map (fun d ->
    let (dt, via_ty, class_ids) = map_deriving_class env d in
    let args = match via_ty with
      | None -> fb []
      | Some ty -> fb [ G.ArgType ty ]
    in
    List.map (fun id ->
      G.NamedAttr (dt, G.Id (id, G.empty_id_info ()), args)
    ) class_ids
  ) derivings

let map_adt_rhs (env : env) ((_eq, cs, _derivings) : CST.adt_rhs)
    : G.or_type_element list =
  map_constructors env cs

(* Walk a single GADT constructor: (con, gadt_constr_type). We emit one
 * OrConstructor with the constructor name and best-effort arg types
 * pulled from the signature (left-hand-side of arrows in `gadt_sig`). *)
let map_gadt_con_name (env : env) (c : CST.con) : G.ident =
  match c with
  | `Cons tok -> map_tyconid env tok
  | `LPAR_cons_op_RPAR (_, sym, _) -> str env sym

let rec gadt_sig_arg_types (env : env) (gs : CST.gadt_sig) : G.type_ list =
  match gs with
  | `Gadt_fun (lhs, _arrow, rest) ->
      let lhs_ty = match lhs with
        | `Strict_type (_bang, at) -> map_atype env at
        | `Type_infix ti -> map_type_infix env ti
      in
      lhs_ty :: gadt_sig_arg_types env rest
  | `Choice_strict_type _ ->
      (* Final return type — not an arg; drop. *)
      []

let map_gadt_constructor (env : env)
    ((con, (_dcolon, _forall, _ctx, sig_or_rec)) : CST.gadt_constructor)
    : G.or_type_element =
  let name = map_gadt_con_name env con in
  let arg_types = match sig_or_rec with
    | `Gadt_sig gs -> gadt_sig_arg_types env gs
    | `Record_fields_arrow_gadt_sig (rf, _arrow, _ret_sig) ->
        (* GADT record-syntax: { x :: A, y :: B } -> R.
         * Expose the record fields; the return type is implicit in
         * the parent data declaration. *)
        record_fields_to_arg_types env rf
  in
  G.OrConstructor (name, arg_types)

let map_gadt_rhs_constructors (env : env) (rhs : CST.gadt_rhs)
    : G.or_type_element list =
  let (_where, body_opt) = rhs in
  match body_opt with
  | None -> []
  | Some body ->
      let entries = match body with
        | `LCURL_opt_choice_gadt_cons_rep_SEMI_choice_gadt_cons_opt_SEMI_RCURL
            (_, inner_opt, _, _) ->
            (match inner_opt with
             | None -> []
             | Some (first, rest) ->
                 first :: List.map (fun (_sep, x) -> x) rest)
        | `Layout_start_opt_choice_gadt_cons_rep_choice_SEMI_choice_gadt_cons_opt_choice_SEMI_layout_end
            (_, inner_opt, _) ->
            (match inner_opt with
             | None -> []
             | Some (first, rest, _trail) ->
                 first :: List.map (fun (_sep, x) -> x) rest)
      in
      List.filter_map (fun e ->
        match e with
        | `Gadt_cons gc -> Some (map_gadt_constructor env gc)
        | `Deri _ -> None
      ) entries

let map_decl_adt (env : env) data_tok _ctx head _ann rhs : G.stmt =
  let t = token env data_tok in
  let name = match name_from_tyfam_head env head with
    | Some id -> id
    | None -> ("<data>", t)
  in
  let (tbody, derivings) = match rhs with
    | Some (`Adt (`Adt_rhs ((_, _, ds) as adt_rhs))) ->
        let variants = map_adt_rhs env adt_rhs in
        (G.OrType variants, ds)
    | Some (`Adt (`Gadt_rhs gadt_rhs)) ->
        let variants = map_gadt_rhs_constructors env gadt_rhs in
        (G.OrType variants, [])
    | Some (`Rep_deri ds) ->
        (G.OrType [], ds)
    | None ->
        (G.OrType [], [])
  in
  let attrs = derivings_to_attrs env derivings in
  let tparams = tparams_from_tyfam_head env head in
  let base_ent = entity_with_tparams (G.basic_entity name) tparams in
  let ent = { base_ent with G.attrs } in
  G.DefStmt (ent, G.TypeDef { tbody }) |> G.s

(* Walk a single record `field` (one or more variables sharing a type
 * annotation) and return one OtherType per variable. *)
let single_record_field_to_arg_types (env : env)
    ((vars1, varsN, _colon, ty_choice) : CST.field) : G.type_ list =
  let ty = match ty_choice with
    | `Strict_type st -> map_strict_type env st
    | `Type t -> map_type env t
  in
  let var_names =
    map_variable env vars1
    :: List.map (fun (_c, v) -> map_variable env v) varsN
  in
  List.map (fun n ->
    other_type ("record_field:" ^ fst n) (snd n) [G.T ty]
  ) var_names

(* Walk a newtype constructor: (tyconid, atype | record_field).
 * Returns one OrConstructor with the constructor name + arg type. *)
let map_newtype_constructor (env : env)
    ((tycon, body) : CST.newtype_constructor) : G.or_type_element =
  let name = map_tyconid env tycon in
  let arg_types = match body with
    | `Atype at -> [ map_atype env at ]
    | `Record_field (_lc, field, _rc) ->
        single_record_field_to_arg_types env field
  in
  G.OrConstructor (name, arg_types)

let map_decl_newt (env : env) nt_tok ()
    (ctx_newtype : CST.context_newtype) (rhs : _) : G.stmt =
  let t = token env nt_tok in
  (* context_newtype = `Cont__simp (context, tyfam_head) | `Simp tyfam_head *)
  let head = match ctx_newtype with
    | `Cont__simp (_ctx, st) -> st
    | `Simp st -> st
  in
  let name = match name_from_decl_newtype_head env head with
    | Some id -> id
    | None -> ("<newtype>", t)
  in
  let (tbody, derivings) = match rhs with
    | `Newt (_eq, ctor, ds) ->
        (G.OrType [ map_newtype_constructor env ctor ], ds)
    | `Opt_type_anno_gadt_rhs (_ann_opt, gadt_rhs) ->
        let variants = map_gadt_rhs_constructors env gadt_rhs in
        (G.OrType variants, [])
  in
  let attrs = derivings_to_attrs env derivings in
  let tparams = tparams_from_tyfam_head env head in
  let base_ent = entity_with_tparams (G.basic_entity name) tparams in
  let ent = { base_ent with G.attrs } in
  G.DefStmt (ent, G.TypeDef { tbody }) |> G.s

(* ---- Class / instance body walking ---- *)

(* Extract a list of statements from a `class_body` / instance body
 * wrapper. Both wrappers have the same shape (a `where` token followed
 * by either a brace-enclosed or layout-delimited list of decls), so we
 * dispatch via a small `map_decl` callback. *)
let rec map_class_body (env : env) ((_w, inner_opt) : CST.class_body) : G.stmt list =
  match inner_opt with
  | None -> []
  | Some inner ->
      (match inner with
       | `LCURL_opt_cdecl_rep_SEMI_cdecl_opt_SEMI_RCURL
           (_, decls_opt, _, _) ->
           (match decls_opt with
            | None -> []
            | Some (first, rest) ->
                map_cdecl env first
                @ List.concat_map (fun (_s, d) -> map_cdecl env d) rest)
       | `Layout_start_opt_cdecl_rep_choice_SEMI_cdecl_opt_choice_SEMI_layout_end
           (_, decls_opt, _) ->
           (match decls_opt with
            | None -> []
            | Some (first, rest, _trailing) ->
                map_cdecl env first
                @ List.concat_map (fun (_s, d) -> map_cdecl env d) rest))

and map_cdecl (env : env) (x : CST.cdecl) : G.stmt list =
  match x with
  | `Gend g -> [ map_gendecl env g ]
  | `Defa_sign (_default_tok, sig_) ->
      [ map_gendecl env (`Sign sig_) ]
  | `Func fn ->
      let (name, fdef) = map_function_to_def env fn in
      let ent = G.basic_entity name in
      [ G.DefStmt (ent, G.FuncDef fdef) |> G.s ]
  | `Class_tyfam (type_tok, _family_opt, head, ann_opt) ->
      let t = token env type_tok in
      let name = match name_from_tyfam_head env head with
        | Some id -> id
        | None -> ("<assoc_type>", t)
      in
      let vars = vars_from_tyfam_head env head in
      let kind_args = match ann_opt with
        | Some (_colon, ty) -> [ G.T (map_type_or_implicit env ty) ]
        | None -> []
      in
      let payload =
        G.I name
        :: List.map (fun ty -> G.T ty) vars
        @ kind_args
      in
      [ other_stmt "class_associated_type" t payload ]
  | `Inst_tyinst (type_tok, _inst_opt, atypes, _eq, rhs) ->
      let t = token env type_tok in
      (* The lhs is `F t1 t2 ...` but inside a class body the head name
       * is implicit (the class's associated type). We expose only the
       * applied atypes plus the rhs type. *)
      let lhs_args = List.map (fun a -> G.T (map_atype env a)) atypes in
      let rhs_ty = map_type env rhs in
      [ other_stmt "class_associated_type_instance" t
          (lhs_args @ [G.T rhs_ty]) ]
  | `Class_data (data_tok, _family_opt, head, ann_opt) ->
      let t = token env data_tok in
      let name = match name_from_tyfam_head env head with
        | Some id -> id
        | None -> ("<assoc_data>", t)
      in
      let vars = vars_from_tyfam_head env head in
      let kind_args = match ann_opt with
        | Some (_colon, ty) -> [ G.T (map_type_or_implicit env ty) ]
        | None -> []
      in
      let payload =
        G.I name
        :: List.map (fun ty -> G.T ty) vars
        @ kind_args
      in
      [ other_stmt "class_associated_data" t payload ]

(* ---- Helpers for class/instance entity construction ---- *)

(* Walk a constraint head and return the trailing atype arguments.
 * For `instance MyClass Int where`, the head is
 * `Type_name_rep_atype(MyClass, [Int])` and we want `[TyN Int]`. *)
let extract_instance_types (env : env) (c : CST.constraint_) : G.type_ list =
  match c with
  | `Type_name_rep_atype (_tn, atypes) ->
      List.map (map_atype env) atypes
  | `Type_infix_ _ -> []

(* Extract the type parameters from a class head `C a b` -> [a; b].
 * Functional dependencies (`| a -> b`) have no AST_generic slot and are
 * not represented. *)
let class_params_from_head (env : env) (head : CST.constraint_)
    : G.type_parameter list =
  match head with
  | `Type_name_rep_atype (_tn, atypes) ->
      List.filter_map (fun a ->
        match (map_atype env a).G.t with
        | G.TyVar id -> Some (G.tparam_of_id id)
        | _ -> None
      ) atypes
  | `Type_infix_ _ -> []

let map_decl_class (env : env) class_tok ctx head _fundeps body_opt
    : G.stmt =
  let t = token env class_tok in
  let cls_name = match find_first_conid_in_constraint env head with
    | Some id -> id
    | None -> ("<class>", t)
  in
  (* Superclass context `class Eq a => Ord a`: keep `Eq a` as a parent, the
   * same way the instance path records its head types in `cextends`. *)
  let superclass_tys = match ctx with
    | None -> []
    | Some (constraints, _carrow) ->
        context_constraints_to_types env constraints
  in
  let tparams = class_params_from_head env head in
  let base_ent = G.basic_entity cls_name in
  let ent = match tparams with
    | [] -> base_ent
    | _ -> { base_ent with G.tparams = Some (fb tparams) }
  in
  let body_stmts = match body_opt with
    | None -> []
    | Some b -> map_class_body env b
  in
  let body_stmts =
    if env.H.extra.is_pattern_mode && body_stmts = [] then
      [ ellipsis_body_stmt () ]
    else body_stmts
  in
  let cbody = fb (List.map (fun s -> G.F s) body_stmts) in
  let cdef = G.ClassDef {
    ckind = (G.Interface, t);
    cextends = List.map (fun ty -> (ty, None)) superclass_tys;
    cimplements = []; cmixins = [];
    cparams = fb [];
    cbody;
  } in
  G.DefStmt (ent, cdef) |> G.s

(* idecl reuses map_cdecl shapes for Func / Sign / type-instance decls. *)
let map_idecl (env : env) (x : CST.idecl) : G.stmt list =
  match x with
  | `Func fn ->
      let (name, fdef) = map_function_to_def env fn in
      let ent = G.basic_entity name in
      [ G.DefStmt (ent, G.FuncDef fdef) |> G.s ]
  | `Sign sig_ ->
      [ map_gendecl env (`Sign sig_) ]
  | `Inst_data idi ->
      let (kw_tok, _inst_opt, lhs_ty, ctors_anys) = match idi with
        | `Data_opt_inst_data_opt_adt (dt, inst_opt, datainst, adt_opt) ->
            let (_fa, _ctx, ti, _ann) = datainst in
            let lhs = map_type_infix env ti in
            let ctors = match adt_opt with
              | Some (`Adt_rhs adt_rhs) ->
                  let variants = map_adt_rhs env adt_rhs in
                  List.filter_map (fun v -> match v with
                    | G.OrConstructor (id, _) ->
                        Some (G.E (G.N (G.Id (id, G.empty_id_info ())) |> G.e))
                    | _ -> None
                  ) variants
              | Some (`Gadt_rhs _) | None -> []
            in
            (token env dt, inst_opt, lhs, ctors)
        | `Newt_opt_inst_data_newt (nt, inst_opt, datainst, (_, ctor, _)) ->
            let (_fa, _ctx, ti, _ann) = datainst in
            let lhs = map_type_infix env ti in
            let (cn, _body) = ctor in
            let cid = map_tyconid env cn in
            ([G.E (G.N (G.Id (cid, G.empty_id_info ())) |> G.e)]
             |> fun ctors -> (token env nt, inst_opt, lhs, ctors))
      in
      [ other_stmt "instance_associated_data" kw_tok
          (G.T lhs_ty :: ctors_anys) ]
  | `Inst_tyinst (type_tok, _inst_opt, atypes, _eq, rhs) ->
      let t = token env type_tok in
      let lhs_args = List.map (fun a -> G.T (map_atype env a)) atypes in
      let rhs_ty = map_type env rhs in
      [ other_stmt "instance_associated_type" t
          (lhs_args @ [G.T rhs_ty]) ]

let map_instance_body (env : env) (_w, inner) : G.stmt list =
  match inner with
  | `LCURL_opt_idecl_rep_SEMI_idecl_opt_SEMI_RCURL (_, decls_opt, _, _) ->
      (match decls_opt with
       | None -> []
       | Some (first, rest) ->
           map_idecl env first
           @ List.concat_map (fun (_s, d) -> map_idecl env d) rest)
  | `Layout_start_opt_idecl_rep_choice_SEMI_idecl_opt_choice_SEMI_layout_end
      (_, decls_opt, _) ->
      (match decls_opt with
       | None -> []
       | Some (first, rest, _trailing) ->
           map_idecl env first
           @ List.concat_map (fun (_s, d) -> map_idecl env d) rest)

let map_decl_inst (env : env) (full_inst) : G.stmt =
  let (instance_head, body_opt) = full_inst in
  (* instance is (instance_tok, forall_ option, context_ option, constraint_).
   * The class name is the first conid we can dig out of the constraint;
   * the trailing atype args are the types being instantiated. *)
  let (instance_tok, _forall, _ctx, head) = instance_head in
  let t = token env instance_tok in
  let name = match find_first_conid_in_constraint env head with
    | Some id -> id
    | None -> ("<instance>", t)
  in
  let ent = G.basic_entity name in
  let cextends = extract_instance_types env head in
  (* body_opt is `(where * variant_option) option`. Walk if Some. *)
  let body_stmts = match body_opt with
    | None -> []
    | Some (_w, None) -> []
    | Some (_w, Some inner) ->
        let inner_pair : CST.where * _ = (_w, inner) in
        map_instance_body env inner_pair
  in
  let body_stmts =
    if env.H.extra.is_pattern_mode && body_stmts = [] then
      [ ellipsis_body_stmt () ]
    else body_stmts
  in
  let cbody = fb (List.map (fun s -> G.F s) body_stmts) in
  let cdef = G.ClassDef {
    ckind = (G.Class, t);
    cextends = List.map (fun ty -> (ty, None)) cextends;
    cimplements = []; cmixins = [];
    cparams = fb [];
    cbody;
  } in
  G.DefStmt (ent, cdef) |> G.s

(* Extract the constructor name from a pattern synonym LHS.
 * Typical shapes:
 *   pattern Foo        -> Pat_cons
 *   pattern Foo a b    -> Pat_apply (ctor, [args...])
 *   pattern a `Foo` b  -> Pat_infix (..., op, ...)
 * If we cannot find a name, fall back to "<pattern_synonym>".
 *)
let pattern_synonym_name_of_pat (env : env) (p : CST.pat) (default_tok : Tok.t)
    : G.ident =
  let from_pat_constructor (pc : CST.pat_constructor) : G.ident =
    let ids = map_pat_constructor env pc in
    ident_of_idents ids
  in
  match p with
  | `Lpat (`Apat (`Pat_cons pc)) -> from_pat_constructor pc
  | `Lpat (`Pat_apply (pc, _args)) -> from_pat_constructor pc
  | `Pat_infix (_, op, _) ->
      let ids = map_qconop env op in
      ident_of_idents ids
  | _ -> ("<pattern_synonym>", default_tok)

let map_decl_pattern (env : env) (pattern_tok : Tok.t)
    (v : [ `Pat_type of CST.pattern_type
         | `Pat_equals of CST.pattern_equals
         | `Pat_arrow of CST.pattern_arrow ])
    : G.stmt =
  let kind_id : G.ident = ("pattern_synonym", pattern_tok) in
  match v with
  | `Pat_type (con, (_colon, type_or_impl)) ->
      let name = map_gadt_con_name env con in
      let ty = map_type_or_implicit env type_or_impl in
      let ent = G.basic_entity name in
      let payload = [ G.T ty ] in
      G.DefStmt (ent, G.OtherDef (kind_id, payload)) |> G.s
  | `Pat_equals (lhs, eq_tok, rhs) ->
      let name = pattern_synonym_name_of_pat env lhs (token env eq_tok) in
      let lhs_p = map_pat env lhs in
      let rhs_p = map_pat env rhs in
      let ent = G.basic_entity name in
      let payload = [ G.P lhs_p; G.Tk (token env eq_tok); G.P rhs_p ] in
      G.DefStmt (ent, G.OtherDef (kind_id, payload)) |> G.s
  | `Pat_arrow (lhs, arr, rhs, _where_opt) ->
      let arr_tok = map_larrow env arr in
      let name = pattern_synonym_name_of_pat env lhs arr_tok in
      let lhs_p = map_pat env lhs in
      let rhs_p = map_pat env rhs in
      let ent = G.basic_entity name in
      let payload = [ G.P lhs_p; G.Tk arr_tok; G.P rhs_p ] in
      G.DefStmt (ent, G.OtherDef (kind_id, payload)) |> G.s

let map_topdecl (env : env) (x : CST.topdecl) : G.stmt list =
  match x with
  | `Decl_type (type_tok, head, rhs) ->
      let t = token env type_tok in
      let name = match name_from_tyfam_head env head with
        | Some id -> id
        | None -> ("<type>", t)
      in
      let tparams = tparams_from_tyfam_head env head in
      let ent = entity_with_tparams (G.basic_entity name) tparams in
      let tbody = match rhs with
        | `EQ_type_or_impl (_eq, ty_or_impl) ->
            let ty = map_type_or_implicit env ty_or_impl in
            G.AliasType ty
        | `Type_anno (_colon, kind) ->
            let kind_ty = map_type_or_implicit env kind in
            G.OtherTypeKind (("type_kind_signature", t), [G.T kind_ty])
      in
      [ G.DefStmt (ent, G.TypeDef { tbody }) |> G.s ]
  | `Decl_tyfam (type_tok, _family, head, ann_opt, _eqs_opt) ->
      let t = token env type_tok in
      let name = match name_from_tyfam_head env head with
        | Some id -> id
        | None -> ("<type_family>", t)
      in
      let vars = vars_from_tyfam_head env head in
      let kind_args = match ann_opt with
        | Some (`Type_anno (_colon, ty)) ->
            [ G.T (map_type_or_implicit env ty) ]
        | Some (`Tyfam_inj _) | None -> []
      in
      let payload =
        G.I name
        :: List.map (fun ty -> G.T ty) vars
        @ kind_args
      in
      let ent = G.basic_entity name in
      [ G.DefStmt (ent, G.TypeDef {
          tbody = G.OtherTypeKind (("type_family", t), payload);
        }) |> G.s ]
  | `Decl_tyinst (type_tok, _inst, atypes, _eq, rhs) ->
      let t = token env type_tok in
      (* `type instance F t1 t2 = U` — the head name `F` appears as the
       * first atype. Try to dig it out for a richer payload; otherwise
       * fall back to a synthetic name. *)
      let (head_name, rest_atypes) = match atypes with
        | first :: rest ->
            (match first with
             | `Type_name (`Choice_prom_tycon gt) ->
                 let id_opt = match gt with
                   | `Prom_tycon (_, qt) | `Choice_choice_qual_type qt ->
                       (match qt with
                        | `Choice_qual_type (`Qual_type (_, tok))
                        | `Choice_qual_type (`Cons tok) ->
                            Some (map_tyconid env tok)
                        | `LPAR_choice_qual_type_op__RPAR _ -> None)
                   | `Tycon_arrow _ -> None
                 in
                 (match id_opt with
                  | Some id -> (id, rest)
                  | None -> (("<type_instance>", t), atypes))
             | _ -> (("<type_instance>", t), atypes))
        | [] -> (("<type_instance>", t), [])
      in
      let lhs_ty = inst_atypes_to_lhs env head_name rest_atypes in
      let rhs_ty = map_type_or_implicit env rhs in
      [ other_stmt "haskell_type_instance" t [G.T lhs_ty; G.T rhs_ty] ]
  | `Decl_role (type_tok, _role_kw, qtc, roles) ->
      let t = token env type_tok in
      let name = match find_first_conid_in_qtycon env qtc with
        | Some id -> id
        | None -> ("<role_target>", t)
      in
      let role_anys = List.map (fun r -> G.I (map_type_role env r)) roles in
      [ other_stmt "haskell_type_role" t (G.I name :: role_anys) ]
  | `Decl_adt (data_tok, ctx, head, ann, rhs) ->
      [ map_decl_adt env data_tok ctx head ann rhs ]
  | `Decl_newt (nt_tok, ctx_newtype, rhs) ->
      [ map_decl_newt env nt_tok () ctx_newtype rhs ]
  | `Decl_data_8db362d (data_tok, _family, head, ann_opt) ->
      let t = token env data_tok in
      let name = match name_from_tyfam_head env head with
        | Some id -> id
        | None -> ("<data_family>", t)
      in
      let vars = vars_from_tyfam_head env head in
      let kind_args = match ann_opt with
        | Some (_colon, ty) -> [ G.T (map_type_or_implicit env ty) ]
        | None -> []
      in
      let payload =
        G.I name
        :: List.map (fun ty -> G.T ty) vars
        @ kind_args
      in
      let ent = G.basic_entity name in
      [ G.DefStmt (ent, G.TypeDef {
          tbody = G.OtherTypeKind (("data_family", t), payload);
        }) |> G.s ]
  | `Decl_data_2e645bc didi ->
      let (kw_tok, lhs_ty, ctors_anys) = match didi with
        | `Data_inst_data_opt_adt (dt, _inst, datainst, adt_opt) ->
            let (_fa, _ctx, ti, _ann) = datainst in
            let lhs = map_type_infix env ti in
            let ctors = match adt_opt with
              | Some (`Adt_rhs adt_rhs) ->
                  let variants = map_adt_rhs env adt_rhs in
                  List.filter_map (fun v -> match v with
                    | G.OrConstructor (id, _) ->
                        Some (G.E (G.N (G.Id (id, G.empty_id_info ())) |> G.e))
                    | _ -> None
                  ) variants
              | Some (`Gadt_rhs _) | None -> []
            in
            (token env dt, lhs, ctors)
        | `Newt_inst_data_newt (nt, _inst, datainst, (_, ctor, _)) ->
            let (_fa, _ctx, ti, _ann) = datainst in
            let lhs = map_type_infix env ti in
            let (cn, _body) = ctor in
            let cid = map_tyconid env cn in
            (token env nt, lhs,
             [G.E (G.N (G.Id (cid, G.empty_id_info ())) |> G.e)])
      in
      [ other_stmt "haskell_data_instance" kw_tok
          (G.T lhs_ty :: ctors_anys) ]
  | `Decl_import imp ->
      [ map_import_directive env imp ]
  | `Decl_class (class_tok, ctx, head, fundeps, body) ->
      [ map_decl_class env class_tok ctx head fundeps body ]
  | `Decl_inst x ->
      [ map_decl_inst env x ]
  | `Decl_defa (default_tok, _lp, types_opt, _rp) ->
      let t = token env default_tok in
      let types = match types_opt with
        | None -> []
        | Some (ti1, rest) ->
            map_type_infix env ti1 ::
            List.map (fun (_c, ti) -> map_type_infix env ti) rest
      in
      let payload = List.map (fun ty -> G.T ty) types in
      [ other_stmt "haskell_default" t payload ]
  | `Decl_fore df ->
      (* foreign import / foreign export: extract the signature so the
       * declared name + type appear in the AST as a regular FuncDef
       * with frettype + FBDecl. *)
      let (foreign_tok, kind_tok, _pre, _str_opt, sig_) = match df with
        | `Decl_fore_import (a, b, c, d, e) -> (a, b, c, d, e)
        | `Decl_fore_export (a, b, c, d, e) -> (a, b, c, d, e)
      in
      let kind = match df with
        | `Decl_fore_import _ -> "import"
        | `Decl_fore_export _ -> "export"
      in
      let _ = foreign_tok in
      let _ = kind_tok in
      let _ = kind in
      [ map_gendecl env (`Sign sig_) ]
  | `Decl_deri (deriv_tok, _strat_opt, (_inst_tok, _forall, _ctx, head)) ->
      let t = token env deriv_tok in
      let class_payload : G.any list = match find_first_conid_in_constraint env head with
        | Some id -> [ G.I id ]
        | None -> []
      in
      let inst_types = extract_instance_types env head in
      let type_payload = List.map (fun ty -> G.T ty) inst_types in
      let payload = class_payload @ type_payload in
      [ other_stmt "haskell_deriving_standalone" t payload ]
  | `Decl d ->
      [ map_decl env d ]
  | `Decl_pat (pattern_tok, variant) ->
      let t = token env pattern_tok in
      [ map_decl_pattern env t variant ]
  | `Top_splice ts ->
      (* Emit as a regular ExprStmt so the inner expression is visible to
       * expression patterns (m_stmts_deep extracts exprs from ExprStmt). *)
      let e = map_top_splice env ts in
      [ G.ExprStmt (e, fake_tok ";") |> G.s ]

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
let rec group_consecutive_clauses ?(in_pattern=false) (stmts : G.stmt list)
    : G.stmt list =
  (* In pattern mode we skip clause merging so that
   * `$A +++ $B = $E` parses as a single FuncDef and matches each clause
   * of the target individually — same convention as Python's
   * `def $X(...): ...` matching every def site. *)
  if in_pattern then stmts
  else
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
  (* A type signature `f :: T` is walked as a FuncDef with an FBDecl body
   * (see map_gendecl `Sign). It is not a pattern-matching clause: it only
   * contributes the return type. Separate signatures from real clauses so
   * a signature + single clause stays a clean FuncDef instead of becoming
   * a spurious Switch. *)
  let is_sig (_, (fdef : G.function_definition)) =
    match fdef.G.fbody with G.FBDecl _ -> true | _ -> false
  in
  let sigs, reals = List.partition is_sig clauses in
  (* Return type: prefer the signature's, else the first real clause's. *)
  let frettype =
    match sigs with
    | (_, sfdef) :: _ -> sfdef.G.frettype
    | [] -> (match reals with (_, f) :: _ -> f.G.frettype | [] -> None)
  in
  match reals with
  | [] ->
      (* Only signature(s): keep the signature declaration as-is. *)
      let (_, sfdef) = List.hd clauses in
      G.DefStmt (G.basic_entity name, G.FuncDef sfdef) |> G.s
  | [ (_, only) ] ->
      (* One real clause (+ optional signature): clean FuncDef, just
       * inject the signature's return type. *)
      let fdef = { only with G.frettype } in
      G.DefStmt (G.basic_entity name, G.FuncDef fdef) |> G.s
  | _ ->
  let clauses = reals in
  let first = List.hd clauses in
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
      | G.ParamPattern (pat, _) -> pat
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
      (* A signature-only clause (FBDecl) and an absent body (FBNothing)
       * are inert vis-à-vis the matcher: emit OS_Pass so we don't carry
       * a "Todo" tag through grouped clauses. *)
      | G.FBDecl _ | G.FBNothing ->
          G.OtherStmt (G.OS_Pass, []) |> G.s
    in
    G.CasesAndBody ([G.Case (tk, case_pat)], body_stmt)
  ) clauses in
  let sw = G.Switch (tk, Some (G.Cond scrutinee), cases) |> G.s in
  let fdef = G.FuncDef {
    G.fkind = (G.Function, tk);
    fparams = fb implicit_params;
    frettype;
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
  group_consecutive_clauses ~in_pattern:env.H.extra.is_pattern_mode raw

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
  group_consecutive_clauses ~in_pattern:env.H.extra.is_pattern_mode raw

(* One export entry yields the exported name(s): a value `foo`, a type
 * `Bar(..)`/`Qux(A,B)` (the type-constructor name), or a module re-export
 * `module M`. *)
let map_export_item (env : env) (e : CST.export) : G.ident list =
  match e with
  | `Choice_choice_qual_var qv -> map_qvar env qv
  | `Opt_name_choice_choice_qual_type_opt_export_names (_ns, qtycon, _names) ->
      (match find_first_conid_in_qtycon env qtycon with
       | Some id -> [ id ]
       | None -> [])
  | `Module_qmodid (_mod_tok, qmod) -> map_qmodid env qmod

(* The module export list `( foo, Bar(..) )` is surfaced as an
 * OtherDirective carrying the exported idents, so rules can match on
 * what a module exposes. *)
let map_exports_directive (env : env) (exports : CST.exports) : G.stmt option =
  let (lpar, items_opt, _trailing, _rpar) = exports in
  match items_opt with
  | None -> None
  | Some (first, rest) ->
      let all =
        map_export_item env first
        @ List.concat_map (fun (_c, e) -> map_export_item env e) rest in
      let anys = List.map (fun id -> G.I id) all in
      let dir = { G.d = G.OtherDirective (("haskell_exports", token env lpar), anys);
                  G.d_attrs = [] } in
      Some (G.DirectiveStmt dir |> G.s)

let program (env : env) (x : CST.haskell) : G.program =
  match x with
  | `Empty_file _ -> []
  | `Module (_mod_tok, qmod, exports_opt, _where, body_opt) ->
      let mod_ids = map_qmodid env qmod in
      let body_stmts = match body_opt with
        | None -> []
        | Some body -> map_module_body env body
      in
      (* Prepend the export list directive (if any) so exports are
       * visible in the module body. *)
      let body_stmts = match exports_opt with
        | None -> body_stmts
        | Some exports ->
            (match map_exports_directive env exports with
             | None -> body_stmts
             | Some d -> d :: body_stmts)
      in
      let body_stmts = pattern_body_or_ellipsis env body_stmts in
      (match mod_ids with
       | [] -> body_stmts
       | _ :: _ ->
           let ent_name = match List.rev mod_ids with
             | last :: _ -> last
             | [] -> raise Common.Impossible
           in
           let ent = G.basic_entity ent_name in
           (* Match the convention used by OCaml / Elixir / VB.NET:
            * the dotted name lives in the entity, and ModuleStruct's
            * own name slot stays None. Including it twice (once as
            * the entity ident, once as the ModuleStruct dotted name)
            * causes the matcher to bind the same metavariable as
            * MV.Id and MV.N, which then conflict on the consistency
            * check. *)
           let mod_body = G.ModuleStruct (None, body_stmts) in
           let def = G.DefStmt (ent, G.ModuleDef { G.mbody = mod_body })
                     |> G.s in
           [ def ])
  | `Topd_rep_choice_SEMI_topd_opt_choice_SEMI tds ->
      map_topdecls_seq env tds
