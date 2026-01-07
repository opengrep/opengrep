(* Yoann Padioleau
 *
 * Copyright (C) 2023 r2c, 2025 Opengrep
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
open Fpath_.Operators
module CST = Tree_sitter_clojure.CST
module R = Raw_tree
open AST_generic
module G = AST_generic
module GH = AST_generic_helpers
module H = Parse_tree_sitter_helpers

(* TODO: Remove after testing, this helps move faster. *)
(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39-34-37"]

(*****************************************************************************)
(* Prelude                                                                   *)
(*****************************************************************************)

(*
 * Many of the common macros are expanded.
 *
 * TODO: Update with latest grammar?
 * https://github.com/sogaiu/tree-sitter-clojure/blob/master/grammar.js
 * This is, by the way, the grammar used here -- but we don't have the
 * latest version.
 *
 * TODO: Handle imports better, it makes rules more robust and with less
 * patterns needed. Especially module aliasing.
 *
 * TODO: Handle pre: and post: maps, since the could determine if parameters
 * are sanitised. Add as attributes (OtherAtribute)?
 *)

(*****************************************************************************)
(* Types, Exceptions and Helpers                                             *)
(*****************************************************************************)

(* TODO: Catch and be lenient, but log in debug mode. *)
exception Parse_error of {msg: string; related_ast: G.any option}
let raise_parse_error ?related_ast msg =
  raise (Parse_error { msg; related_ast })

(* We use this to control when ellipsis is allowed and how it should be handled. *)
type parse_kind =
  | Program
  | Pattern

type syntax_mode =
  | Normal
  | Quoted
  | Syntax_quoted

type extra = {
  kind: parse_kind;
  mode: syntax_mode;
  ns: string option; (* alt: or in split form? [clojure; core] etc; or G.name? *)
  ns_aliases: (string * string) list;
   (* (ns my.ns (:require [other.ns :as my-alias])) *)
} 

type env = extra H.env

let in_pattern (env : env) : bool =
  match env.extra.kind with
  | Pattern -> true
  | _ -> false

type insert_pos =
  | Insert_first
  | Insert_last

(*****************************************************************************)
(* Anon function helpers                                                     *)
(*****************************************************************************)

class ['self] find_max_placeholder_short_lambda_visitor =
  object (_self : 'self)
    inherit [_] AST_generic.iter_no_id_info as super

    method! visit_expr ((max_found, has_shorthand_param_1, has_rest) as env) expr =
      (match expr.G.e with
      | G.N (G.Id (((s, _tk) as param_id), _id_info_unresolved)) ->
          begin match s with
            | "%&" ->
                has_rest := true
            | "%" ->
                (* XXX: We can't generate only %1, so we create PatAs (%1, %)
                 * to enable the extra binding. *)
                (* implicit param % == %1 *)
                has_shorthand_param_1 := true;
                max_found := max !max_found 1
            | _ when String.starts_with ~prefix:"%" s ->
                let n_str = String.sub s 1 (String.length s - 1) in
                begin
                  try
                    let n = int_of_string n_str in
                    max_found := max !max_found n
                  with Failure _ ->
                    ()
                end
            | _ -> ()
          end
      (* This is not really needed for valid code; but we should not nest into
       * another anonymous function to avoid strange results in invalid code. *)
      | G.OtherExpr (("ShortLambda", _tk), _params_and_body) -> ()
      | _ -> super#visit_expr env expr);
  end

let find_max_placeholder_short_lambda_visitor_instance =
  new find_max_placeholder_short_lambda_visitor

(* Find the maximum placeholder number and if it has and shorthand % (%1) and
 * rest params in a ShortLambda body.
 * E.g., in #(sink % %& %2), returns (3, true, true).
 * Recursively searches all subexpressions. Nesting such lambdas is not allowed. *)
let find_max_placeholder_short_lambda (e : G.expr) : int * bool * bool =
  let max_found = ref 0 in
  let has_shorthand_param_1 = ref false in
  let has_rest = ref false in
  find_max_placeholder_short_lambda_visitor_instance#visit_expr
    (max_found, has_shorthand_param_1, has_rest) e;
  !max_found, !has_shorthand_param_1, !has_rest

(* Convert a ShortLambda body to OtherExpr with params and body.
 * Creates params %1, %2, ... based on the occurrences in the body.
 * Structure: OtherExpr("ShortLambda", [Params [...]; E body_expr]).
 * This allows Naming_AST to create proper scope for the params.
 *
 * Examples: 
 * (macroexpand-1 '#(+ 1 %)) => (fn* [x] (+ 1 x))
 * (macroexpand-1 '#(+ 1 % %&)) => (fn* [x & rest] (+ 1 x rest))
 *
 * So we create for the above example:
 * G.OtherExpr (("ShortLambda", _), [ G.Params [x]; G.E body_translation ]) 
 *)
let encode_short_lambda (env : env) (tok (* # *) : Tok.t) (body_expr : G.expr) : G.expr =
  let max_placeholder, has_shorthand, has_rest =
    find_max_placeholder_short_lambda body_expr
  in
  let params =
    List.init max_placeholder (fun i ->
        let param_name = Printf.sprintf "%%%d" (i + 1) in
        let param_id = (param_name, tok) in
        match i + 1, has_shorthand with
        | 1, true ->
          (* implicit param % == %1 *)
          let id_info = G.empty_id_info () in
          G.PatAs (* the alias ensures both % and %1 are resolved. *)
            (G.PatId (param_id, id_info), (("%", tok), id_info))
        | _ ->
          G.PatId (param_id, G.empty_id_info ()))
  in
  (* What is the number of parameters in a pattern like #($_ ... %2 ...)?
   * Some i >= 2. So what we do is add ... (PatEllipsis) to allow for more
   * parameters in matching. *)
  let params =
    match env.extra.kind, params with
    | Pattern, params -> params @ [ G.PatEllipsis tok ]
    | _ -> params
  in 
  let params_with_rest =
    if has_rest then
      let pat_constructor =
        G.PatConstructor ((Id (("&", tok), G.empty_id_info ())),
                          [G.PatId (("%&", tok), G.empty_id_info ())])
      in
      params @ [ pat_constructor ]
    else
      params
  in
  let final_param =
    G.ParamPattern (G.PatList (Tok.unsafe_fake_bracket params_with_rest))
  in
  (* Structure that Naming_AST can recognize for scope handling *)
  G.OtherExpr (("ShortLambda", tok),
               [ G.Params [final_param]; G.E body_expr ])
  |> G.e

(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)

(* Not excited about adding this, but it's used to parse sequential destructuring
 * and it works. *)
type list_binding_form_state =
  | Normal_list of G.pattern list
  | At_ampersand_list of G.pattern list * Tok.t (* the & *)
  | After_ampersand_list of G.pattern list
  | At_as_list of G.pattern list * Tok.t (* the :as *)
  | After_as_list of G.pattern list * G.ident

type try_catch_finally_state =
  | Try_exprs of Tok.t (* try *) * G.expr list
  | At_catch_clause of (Tok.t (* catch *) * G.catch_exn * G.expr list) list
  | At_finally_clause of Tok.t (* finally *) * G.expr list

let expr_to_stmt e =
  match e.e with
    | StmtExpr st -> st
    | _else_ -> s (ExprStmt (e, sc))

let exprblock = function
  | [ ({e = G.OtherExpr(("ExprBlock",_ ), _); _} as exp) ] -> exp
  | exprs ->
    let exs_any = List_.map (fun e -> G.E e) exprs in
    G.OtherExpr (("ExprBlock", (G.fake "expr_block")), exs_any)
    |> G.e

let with_mode env mode =
  H.{env with extra = {env.extra with mode}}

let with_ns env ns =
  H.{env with extra = {env.extra with ns}}

let int_regex =
  Pcre2_.regexp "^[+-]?[0-9]+N?$"

let float_regex =
  Pcre2_.regexp
    {|^[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?M?$|}

let ratio_regex =
  Pcre2_.regexp "^[+-]?[0-9]+/[0-9]+$"

let octal_regex =
  Pcre2_.regexp {|^0[0-7]+$|}

let hex_regex =
  Pcre2_.regexp {|^0[xX][0-9A-Fa-f]+$|}

(* XXX: radix is 2-36 in reality. *)
let radix_regex =
  Pcre2_.regexp {|^[1-9][0-9]?[Rr][0-9a-zA-Z]+[Nn]?$|}

let qualified_name_regex_str = "^\\(.+\\)/\\(.+\\)$"

let fake_variable_ident = "G__1111"
let implicit_param_ident = G.implicit_param

let todo (_env : env) _ = raise_parse_error "Not implemented."

let token = H.token
let raw_token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token (H.str env tok)

let char_str_of_octal (oct_with_prefix : string) : string =
  (* Remove clojure's \o prefix. *)
  let oct =
    String.sub oct_with_prefix 2 (String.length oct_with_prefix - 2)
  in
  (* Parse octal string into an int (0–255 allowed in Clojure) *)
  let code =
    try int_of_string ("0o" ^ oct)
    with _ -> raise_parse_error ("Invalid octal char literal: " ^ oct) 
  in
  (* Restrict to 0–255 (Clojure uses Java char: 0–65535, but octal max is 377) *)
  if code < 0 || code > 0xFF then
    raise_parse_error ("Octal char literal out of range: " ^ oct);

  (* Convert to a single UTF-8 character string *)
  let uchar = Uchar.of_int code in
  (* Extract final UTF-8 string *)
  let buf = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buf uchar;
  Buffer.contents buf

let char_str_of_unicode (hex_with_prefix : string) : string =
  (* Remove clojure's \u prefix. *)
  let hex =
    String.sub hex_with_prefix 2 (String.length hex_with_prefix - 2)
  in
  (* Parse hex codepoint e.g. "03A9" *)
  let code =
    try int_of_string ("0x" ^ hex)
    with _ -> raise_parse_error ("Invalid unicode char literal: \\u" ^ hex)
  in
  (* Check valid BMP range (Clojure uses Java char = UTF-16 unit) *)
  if code < 0 || code > 0xFFFF then
    raise_parse_error ("Unicode char literal out of range: \\u" ^ hex);

  (* Convert to UTF-8 string *)
  let uchar = Uchar.of_int code in
  let buf = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buf uchar;
  Buffer.contents buf

(*****************************************************************************)
(* Names and identifiers                                                     *)
(*****************************************************************************)

(* NOTE: for symbols in patterns / function args / definitions,
 * we cannot have qualified ids, so clojure.core/x is not expected...
 * What is expected is simple symbol "variables".
 * Actually in def we can have qualified var but it must exist... *)
(* NOTE: There is also %1, %&  etc in #(vector %1 %&). These are symbols. *)
let name_from_ident ?(is_auto_resolved = false) ?(is_kwd = false)
    (env : env) (id : G.ident) : G.name =
  let s, t = id in
  if s =~ qualified_name_regex_str then
      let before, after = Common.matched2 s in
      let t_before, t_after =
        Tok.split_tok_at_bytepos (String.length before) t
      in
      let _t_slash, t_after =
        Tok.split_tok_at_bytepos 1 t_after
      in
      (* Example: obtain idents for 'clojure' and 'core' from 'clojure.core'. *)
      let ids_before =
        snd @@ 
        List.fold_left_map
          (fun t_acc id ->
             let t_id, t_rest =
               Tok.split_tok_at_bytepos (String.length id) t_acc
             in t_rest, (id, t_id))
          t_before
          (String.split_on_char '.' before)
          (* NOTE: We don't assume well formed, 'clojure..core'
           * won't throw but will contain empty idents in the list. *)
      in
      let should_add_colon =
        is_kwd && not (in_pattern env && G.is_metavar_name after)
      in
      let colon = (* if is_auto_resolved then "::" else *) ":" in
      let after = if should_add_colon then colon ^ after else after in
      let name_top, ids_before =
        match ids_before, is_kwd, is_auto_resolved with
        |  (s, tk) :: ids_rest , true, true ->
          (* if in_pattern env && G.is_metavar_name s then
               ids_before
             else *)
            Some (G.fake "::"), ("::", (G.fake "::")) :: (s, tk) :: ids_rest
        | _, true, false -> Some (G.fake ":"), (":", (G.fake ":")) :: ids_before
        | _ -> None, ids_before
      in
      let id_after = (after, t_after) in
      GH.name_of_ids ?name_top (ids_before @ [ id_after ])
  else
    let should_add_colon =
      is_kwd && not (in_pattern env && G.is_metavar_name s)
    in
    let colon = (* if is_auto_resolved then "::" else *) ":" in
    let after = if should_add_colon then colon ^ s else s in
    let s = if should_add_colon then colon ^ s else s in
    match is_kwd, is_auto_resolved with
    | true, true ->
        GH.name_of_ids ~name_top:(G.fake "::") [ ("::", (G.fake "::")); (s, t) ]
    | true, false ->
        GH.name_of_ids ~name_top:(G.fake ":") [ (":", (G.fake ":")); (s, t) ]
    | _ ->
        let s' =
          match s with "..." when in_pattern env -> "$_" | _ -> s
        in
        Id ((s', t), empty_id_info ())

let name (env : env) (tok : Tree_sitter_run.Token.t) : G.name =
  name_from_ident env (H.str env tok)

let extract_unqualified_ident (env : env) (tok : Tree_sitter_run.Token.t) : G.ident =
  let s, t = H.str env tok
  in
  if s =~ qualified_name_regex_str then
      let before, after = Common.matched2 s in
      let _t_before, t_after =
        Tok.split_tok_at_bytepos (String.length before + 1 (* / *)) t
      in
      (after, t_after)
  else
    match s with
    | "..." -> raise_parse_error ~related_ast:(G.I (s, t))
                 "Ellipsis not allowed here."
    | _ -> (s, t)

let ident_or_ellipsis_pat
    (env : env) (tok : Tree_sitter_run.Token.t) : G.pattern =
  let _, s = tok
  in
  match env.extra.kind, s with
  | Pattern, "..." ->
    PatEllipsis (H.token env tok)
  | _ ->
    (* XXX: We throw way the x in x/y. *)
    let id = extract_unqualified_ident env tok in
    PatId (id, empty_id_info ())

let simple_ident_or_ellipsis_pat
    (env : env) (tok : Tree_sitter_run.Token.t) : G.pattern =
  let _, s = tok
  in
  match env.extra.kind, s with
  | Pattern, "..." ->
    PatEllipsis (H.token env tok)
  | _ when not (s =~ qualified_name_regex_str) ->
    let id = H.str env tok in
    PatId (id, empty_id_info ())
  | _ ->
    raise_parse_error ~related_ast:(G.I (H.str env tok)) 
      "Expected simple symbol"

let sym_to_name_or_ellipsis_expr (env : env) (tok : Tree_sitter_run.Token.t) : G.expr =
  let _, s = tok
  in
  match env.extra.kind, s with
  | Pattern, "..." ->
    Ellipsis (H.token env tok) |> G.e
  | _ ->
    let id = name env tok in
    N id |> G.e 

(*****************************************************************************)
(* Ground values                                                             *)
(*****************************************************************************)

let map_boolean (env : env) (tok : Tree_sitter_run.Token.t) : bool wrap =
  let _, s = tok in
  let b =
    match s with
    | "true" -> true
    | "false" -> false
    (* TODO: Better in tree-sitter? In fact it's impossible to fail!
     * This is because we already got a bool literal from parsing. *)
    | _ -> raise Impossible
  in
  (b, token env tok)

let map_num (env : env) (tok : Tree_sitter_run.Token.t) : G.expr =
  let is_match = function Ok x -> x | _ -> false in
  let s, t = H.str env tok in
  if is_match @@ Pcre2_.pmatch ~rex:octal_regex s then
    begin
      let i = String.sub s 1 (String.length s - 1) in
      L (G.Int (Parsed_int.parse ("0o" ^ i, t)))
      |> G.e 
    end
  else if is_match @@ Pcre2_.pmatch ~rex:int_regex s then
    begin
      (* NOTE: 1234567890123456789012345678901234567890N becomes
       * None, so we keep as BigInt. But then it won't be found
       * as an integer... *)
      let i =
        if String.ends_with ~suffix:"N" s then
          String.sub s 0 (String.length s - 1)
        else s
      in
      match Common2.int64_of_string_opt i with
      | Some _ as n -> L (G.Int (n, t)) |> G.e
      | None -> 
          G.OtherExpr (("BigInt", G.fake "BigInt"),
                       [G.Str (Tok.unsafe_fake_bracket (s, t))])
          |> G.e
    end
  else if is_match @@ Pcre2_.pmatch ~rex:float_regex s then
    begin
      let f = if String.ends_with ~suffix:"M" s then
          String.sub s 0 (String.length s - 1)
          else s
      in
      L (G.Float (float_of_string_opt f, t))
      |> G.e 
    end
  else if is_match @@ Pcre2_.pmatch ~rex:hex_regex s then
    begin
      L (G.Int (Parsed_int.parse (s, t)))
      |> G.e 
    end
  else if is_match @@ Pcre2_.pmatch ~rex:ratio_regex s then
    L (G.Ratio (s, t))
    |> G.e 
  else if is_match @@ Pcre2_.pmatch ~rex:radix_regex s then
    let base_and_digits =
      String.split_on_char 'r' (String.lowercase_ascii s)
    in
    match base_and_digits with
    | [base; digits] ->
      begin
        (* Because this is not covered by Common2.int_of_base. *)
        if String.equal base "16" then
          L (G.Int (Parsed_int.parse ("0x" ^ digits, t))) |> G.e 
        else
        try
          let n = Common2.int_of_base digits (int_of_string base) in
          L (G.Int (Parsed_int.parse (string_of_int n, t))) |> G.e
        with
          (* This is because of the limitations of Common.int_of_base,
           * and the fact we don't care so much for these edge cases. *)
          Failure _ ->
          G.OtherExpr (("RadixNumber", G.fake "RadixNumber"),
                       [G.Str (Tok.unsafe_fake_bracket (s, t))])
          |> G.e
      end
    | _ -> assert false
  else
    (* Because why fail? But this should not happen. *)
    G.OtherExpr (("UnexpectedNumber", G.fake "UnexpectedNumber"),
                 [G.Str (Tok.unsafe_fake_bracket (s, t))])
    |> G.e

let map_char (env : env) (tok : Tree_sitter_run.Token.t) : G.expr =
  let s, t = H.str env tok in 
  let s_char =
    match s with
    | "\\backspace" -> "\\b"
    | "\\formfeed" -> "\\f"
    | "\\newline" -> "\\n"
    | "\\return" -> "\\r"
    | "\\space" -> " "
    | "\\tab" -> "\\t"
    | s when String.starts_with ~prefix:"\\o" s ->
      (* octal *) char_str_of_octal s
    | s when String.starts_with ~prefix:"\\u" s ->
      (* unicode *) char_str_of_unicode s
    | _ -> String.sub s 1 (String.length s - 1)
  in
  L (G.Char (s_char, t))
  |> G.e 

(*****************************************************************************)
(* Translation                                                               *)
(*****************************************************************************)

let rec _helps_other_functions_move_around () = () 

(* Get first element of source list to see if it's a special form. *)
and get_forms_and_discriminant (env : env)
    ((meta, (_lp, src, _rp)) : CST.list_lit) : string option * CST.form list =
  let _meta = R.List (List_.map (map_metadata_lit env) meta) in
  let forms = forms_in_source env src in
  match forms with
  | `Sym_lit (_meta, (_loc, sym_name)) :: _rest -> Some sym_name, forms
  | _ -> None, forms

and map_list_form
    (env : env) (((meta, (lp, _src, rp)) as list_lit) : CST.list_lit)
  : G.expr =
  let discriminant, forms = get_forms_and_discriminant env list_lit
  in
  (* TODO:
   * - if not env says quoted;
   * - if not under defmacro. *)
  match discriminant with
    (* TODO: Check if in Quoted state! In that case just symbols list. *)
    | Some ("def" | "defonce") -> map_def_form env forms
    | Some "fn" -> map_fn_form env forms
    | Some ("defn" | "defn-") -> map_defn_form env forms
    | Some "let" -> map_let_form env forms
    | Some "letfn" -> map_letfn_form env forms
    | Some "if" -> map_if_form env forms
    | Some "if-not" -> map_if_not_form env forms
    | Some ("if-let" | "if-some") -> map_if_let_some_form env forms
    | Some "when" -> map_when_form env forms
    | Some "when-not" -> map_when_not_form env forms
    | Some "when-let" -> map_when_let_form env forms
    | Some "when-some" -> map_when_some_form env forms
    | Some "when-first" -> map_when_first_form env forms
    | Some "do" -> map_do_form env forms
    | Some ("->" | "->>") -> map_thread_first_last_form env forms
    | Some ("cond->" | "cond->>") -> map_cond_thread_first_last_form env forms
    | Some ("some->" | "some->>") -> map_some_thread_first_last_form env forms
    | Some "try" -> map_try_catch_finally_form env forms
    | Some "throw" -> map_throw_form env forms
    | Some "as->" -> map_as_thread_form env forms
    | Some "comment" -> map_comment_form env forms
    (* | Some "doto" -> map_doto_form env forms *) (* XXX: Treat as normal function. *)
    (* | Some "format" -> map_format_form env forms *) (* XXX: Treat as normal function. *)
    (* | Some ".." -> map_dotdot_form env forms *) (* TODO: It's a threading macro for methods. *)
    | Some "loop" -> map_loop_form env forms
    | Some "recur" -> map_recur_form env forms
    | Some ("defmacro" | "definline") -> map_defmacro_form env forms
    | Some "ns" -> map_ns_form env forms
    | Some "require" -> map_require_form env forms
    | Some "use" -> map_use_form env forms
    | Some ("in-ns" (* TODO: replace env.extra.ns *)) ->
      (* TODO: Do not ignore. but for now we don't want this to be
       * treated like function application. *)
      G.(L (Null (fake "")) |> e)
    (* Here we assume this is not a special form, and it's interpreted
     * as function application. *)
    (* TODO: How about (:user ...)? We should use G.DotAccess with FDynamic
     * Atom :user. *)
    | Some "..." when in_pattern env -> map_ellipsis_list_form env forms
    | Some "apply" -> map_apply_form env forms
    | _ -> map_call_form env forms

and map_form (env : env) (x : CST.form) : G.expr =
  match x with
  | `Semg_deep_exp (lt, _v2_gap, expr_form, _v4_gap, gt) ->
    begin
    match env.extra.kind with
    | Pattern ->
      let v1 = (* "<..." *) token env lt in
      let v3 = map_form env expr_form in
      let v5 = (* "...>" *) token env gt in
      DeepEllipsis (v1, v3, v5) |> G.e
    | _ ->
      raise_parse_error
        "Deep ellipsis <... ...> is only allowed in Pattern mode."
    end
  | `Num_lit tok -> map_num env tok
  | `Kwd_lit tok ->
    (* let lit = map_kwd_lit env tok in L lit |> G.e  *)
    map_kwd_expr env tok
  | `Str_lit tok ->
    (* TODO: Make interpolated, in reality only used in patterns in
     * order to easily match "SELECT ... FROM ..." etc within strings,
     * without a regex. *)
    let s, t = H.str env tok in 
    let s_no_quotes = String.sub s 1 (String.length s - 2) in
    L (G.String (Tok.unsafe_fake_bracket (s_no_quotes, t)))
    |> G.e 
  | `Char_lit tok -> map_char env tok
  | `Nil_lit tok -> L (Null (token env tok)) |> G.e 
  | `Bool_lit tok -> let b = map_boolean env tok in L (G.Bool b) |> G.e 
  | `Sym_lit x -> map_sym_lit env x
  | `List_lit x -> map_list_form env x
  | `Map_lit x -> map_map_form env x
  | `Vec_lit x -> map_vec_form env x
  | `Set_lit x -> map_set_form env x
  | `Anon_fn_lit (meta_list, v_tok (* # *), bare_list) ->
    map_anon_func_form env meta_list v_tok bare_list
  | `Sym_val_lit (v1, v2, v3) ->
    let _v1 = (* "##" *) raw_token env v1 in
    let _v2 = List_.map (map_gap env) v2 in
    map_sym_val_lit env v3
  | `Regex_lit (v1, v2) ->
    (* TODO: See ruby, what about #... which should be Ellipsis?
     * But should be only in Pattern mode... ruby does not do that. *)
     let _v1 = (* "#" *) token env v1 in
     let s, t = H.str env v2 in
     let s_no_quotes = String.sub s 1 (String.length s - 2) in
     (* begin match env.extra.kind, s_no_quotes with
        | Program, "..." ->
            Ellipsis t |> G.e |> raw_of_expr
        | _else_ -> *)
       G.L (G.Regexp (Tok.unsafe_fake_bracket (s_no_quotes, t), None))
       |> G.e 
     (* end *)

  (* TODO: Start of remaining RAW translation. *)

  | `Read_cond_lit x ->
    R.Case ("Read_cond_lit", map_read_cond_lit env x)
    |> expr_of_raw
  | `Spli_read_cond_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Spli_read_cond_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#?@" *) raw_token env v2 in
          let v3 = R.List (List_.map (raw_token env (* ws *)) v3) in
          let v4 = map_bare_list_lit env v4 in
          R.Tuple [ v2; v3; v4 ] )
    |> expr_of_raw
  (* TODO: Handle this. *)
  | `Ns_map_lit (v1, v2, v3, v4, v5) ->
    (*
     #:person{:first "Han" ::second "Solo"}
     => {:person/first "Han", :opengrep.clj/second "Solo"}
      *)
      R.Case
        ( "Ns_map_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#" *) raw_token env v2 in
          let v3 =
            match v3 with
            | `Auto_res_mark tok ->
                R.Case ("Auto_res_mark", (* auto_res_mark *) raw_token env tok)
            | `Kwd_lit tok -> R.Case ("Kwd_lit", (* kwd_lit *) raw_token env tok)
          in
          let _v4 = List_.map (map_gap env) v4 in
          let v5 = map_bare_map_lit env v5 in
          R.Tuple [ v2; v3; v5 ] )
    |> expr_of_raw
  | `Var_quot_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Var_quot_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#'" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in (* This #'x => (var x) *)
          R.Tuple [ v2; raw_of_expr v4 ] )
    |> expr_of_raw
  | `Eval_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Eval_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#=" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 =
            match v4 with
            | `List_lit x -> R.Case ("List_lit", map_list_lit env x)
            | `Read_cond_lit x ->
                R.Case ("Read_cond_lit", map_read_cond_lit env x)
            | `Sym_lit x -> R.Case ("Sym_lit", map_sym_lit env x |> raw_of_expr)
          in
          R.Tuple [ v2; v4 ] )
    |> expr_of_raw
  | `Tagged_or_ctor_lit (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "Tagged_or_ctor_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_sym_lit env v4 in
          let _v5 = List_.map (map_gap env) v5 in
          let v6 = map_form env v6 in
          R.Tuple [ v2; raw_of_expr v4; raw_of_expr v6 ] )
    |> expr_of_raw
  | `Dere_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Dere_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "@" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; raw_of_expr v4 ] )
    |> expr_of_raw
  (* TODO: Handle this. *)
  | `Quot_lit (v1, v2, v3, v4) ->
     (* TODO: function (quote ...) behaves the same. *)
      R.Case
        ( "Quot_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "'" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form (with_mode env Quoted) v4 in
          R.Tuple [ v2; raw_of_expr v4 ] )
    |> expr_of_raw
  (* TODO: Handle this. *)
  | `Syn_quot_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Syn_quot_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "`" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form (with_mode env Syntax_quoted) v4 in
          R.Tuple [ v2; raw_of_expr v4 ] )
    |> expr_of_raw
  | `Unqu_spli_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Unqu_spli_lit",
          let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "~@" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form (with_mode env Normal) v4 in
          R.Tuple [ v2; raw_of_expr v4 ] )
    |> expr_of_raw
  | `Unqu_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Unqu_lit",
          let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "~" *) raw_token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form (with_mode env Normal) v4 in
          R.Tuple [ v2; raw_of_expr v4 ] )
    |> expr_of_raw

(* TODO: Instead of Call, make list container, works for rest forms
 * inductively because env should say it's a quoted context.
 * TODO: For symbols not in unquoted context, we should wrap them
 * in something that does not resolve them in Naming. That is, we
 * will need to intentionally skip visiting the node in Naming in
 * order for such symbols to remain unresolved.
 *)
and _UNUSED_map_quoted_list_form (env : env) (forms : CST.form list) =
  todo env ()

(* TODO: Like defn but the definition is MacroDef with idents.
 * The parameters are not evaluated. For now we ignore macros,
 * we don't want to fail parsing a target because of this. *)
and map_defmacro_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta, ((_loc, ("defmacro" | "definline")) as tk)) :: _ ->
    L (Null (H.token env tk)) |> G.e
  | _ -> assert false (* We invoked when the symbol was one of the above. *)

(*
 * (doto x & forms)
 *
 * Evaluates x then calls all of the methods and functions with the
 * value of x supplied at the front of the given arguments.  The forms
 * are evaluated in order.  Returns x.
 *  (doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
 *
 * opengrep.clj=> (macroexpand '(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2)))
 * (let* [g (new java.util.HashMap)] (.put g "a" 1) (.put g "b" 2) g) 
 *
 * TODO: This one requires a fresh symbol for x, and then we need
 * a letpattern and to thread the symbol in the forms.
 *
 * TODO: For search mode, we are better off not translating this; but for
 * tainting, it's probably better to translate. The issue is that most
 * people would naturally write patterns like `(.put "a" 1)` which won't
 * match once `doto` is expanded.
 *
 * NOTE: At them moment this is interpeted as a normal function call,
 * because it can become confusing for users if it's expanded in search mode.
 *)
and _UNUSED_map_doto_form (env : env) (forms : CST.form list) : G.expr =
  todo env ()

(* TODO: (.. object method1 method2 method3) ~> (.method3 (.method2 (.method1 object))) *)
and _UNUSED_map_dotdot_form (env : env) (forms : CST.form list) : G.expr =
  todo env ()

(* TODO: some-> / some->> are similar to -> / ->> but need a let bindings
 * translation similar to cond-> / cond->>.
 *
 * opengrep.clj=> (macroexpand '(some->> x (-> sink) func))
 * (let* [g x
 *        g (if (clojure.core/nil? g) nil (clojure.core/->> g (-> sink)))]
 *  (if (clojure.core/nil? g) nil (clojure.core/->> g func)))
 *)
and map_some_thread_first_last_form (env : env) (forms : CST.form list) : G.expr =
  (* XXX: Temporary solution. *)
  map_thread_first_last_form env forms

(* (format fmt & args) *)
and _UNUSED_map_format_form (env : env) (forms : CST.form list) : G.expr =
  (* TODO: String interpolation. *)
  todo env ()

and map_kwd_expr (env : env) (tok : CST.kwd_lit) : G.expr =
  let _atom_kind, tok_colon, atom_name =
    map_kwd_expr_aux env tok
  in
  G.OtherExpr (("Atom", tok_colon), [G.Name atom_name])
  |> G.e

and map_kwd_expr_aux (env : env) (tok : CST.kwd_lit)
  : string * Tok.t * G.name =
  (* TODO: svalue can be set here if env contains ns information,
   * which it currently does not.
   *  
   * There are a few cases to consider:
   * - ::a which requires to expand current namespace in svalue.
   * - :my-ns/a which is explicitly namespaced keyword. 
   * - ::my-alias/hi which is alias from:
   *   (ns my.ns (:require [other.ns :as my-alias]))
   *   This should have svalue: :other.ns/hi.
   *
   * In patterns ::$A/a, :$A/a, $A/$B, ::$A and :$A match as expected.
   * *)
  let s, t = H.str env tok in
  (* Cases for svalue:
   * ::a => :env.ns/a
   * ::my-alias/a => :other.ns/a *)
  let atom_kind, s_name =
    if String.starts_with ~prefix:"::" s then
      "::", String.sub s 2 (String.length s - 2)
    else
      ":", String.sub s 1 (String.length s - 1)
  in
  (* Separate colon from rest of name. *)
  let tok_colon, tok_name =
    Tok.split_tok_at_bytepos (String.length atom_kind) t
  in
  let id_info = G.empty_id_info () in
  let atom_name =
    name_from_ident ~is_auto_resolved:(atom_kind =*= "::")
      ~is_kwd:true env (s_name, tok_name)
  in
  (* TODO: id_info.s_value := expanded if not set *)
  atom_kind, tok_colon, atom_name

(* (def symbol doc-string? init?) noting that metadata are parsed
 * together with the symbol. *)
and map_def_form (env : env) (forms : CST.form list) =
  match forms with
  | `Sym_lit (_meta, ((_loc, ("def" | "defonce" as sym)) as def_tk))
    :: `Sym_lit x :: rest ->
    (* XXX: in fact, the symbol is interned, if it does not exist,
     * in the current namespace *ns*. *)
    (* XXX: In patterns this could be Ellipsis? No need though. *)
    let symbol_ident : G.name = map_name env x in
    let init_val = 
      match rest with
      | [ init ] | [ _ (* doc string (TODO: match this?) *); init ] ->
        (* initial value *)
        map_form env init
      | [] | _ :: _ :: _ ->
        (* nil, needs fake token since it's not there; or reuse. *)
        (* also: covers the case of invalid syntax, with more elements
         * than expected.
         * XXX: Why not keep init as above in such cases,
         * dropping the rest, when the list is non empty? *)
        let (_meta, tk) = x (* piggybacking on the symbol token *) in
        L (Null (H.token env tk (* alt: G.fake "nil" *))) |> G.e
    in
    (* TODO: Unfortunately the 'def' token is not used, so matches
     * will be strange and autofix won't work... do something? *)
    let attrs = match sym with
      | "defonce" ->
        [G.KeywordAttr (G.Const, token env def_tk)]
      | _ -> []
    in
    let entity = 
      {
        G.name = EN symbol_ident;
        G.attrs;
        G.tparams = None;
      }
    in
    let vardef = 
      {
        G.vinit = Some init_val;
        G.vtype = None; (* TODO: Use metadata of the symbol, if present. *)
        G.vtok = None;
      }
    in
    let st = G.DefStmt (entity, G.VarDef vardef) |> G.s in
    (* TODO: Some forms should just return statement. Or at least
     * unpack the statement from the expression. *)
    StmtExpr st |> G.e
  | `Sym_lit (_meta, ((_loc, "def") as tk)) :: _rest ->
    (* Invalid syntax, nothing after 'def', no symbol after 'def', etc. *)
    L (Null (H.token env tk (* alt: G.fake "nil" *))) |> G.e
  | _ ->
    (* A precondition of this function is that we have the symbol
     * 'def' at the head of the list. *)
    raise Impossible

and map_binding_form (env : env) (form : CST.form) : G.pattern =
  match form with
  | `Sym_lit ((_meta, (_loc, s)) as x) when not String.(equal s "&") ->
    map_sym_lit_pat env x

  | `Vec_lit vec_lit -> map_binding_form_vec_lit env vec_lit

  | `Map_lit x -> map_binding_form_map_lit env x

  | _ ->
    raise_parse_error "Invalid binding form."

(* Optional & rest, and only after this if defined, optional :as id for the
 * whole result. After & rest we cannot have more list elements.
 * In top-level parameter lists, we cannot have :as at all. *)
and map_binding_form_vec_lit
    ?(allow_as = true) (env : env) ((_meta, (lb, srcs, rb)) : CST.vec_lit)
  : G.pattern =
  let lb, rb = token env lb, token env rb in
  let forms = forms_in_source env srcs in
  let state = 
    List.fold_left
      (fun state form ->
       match state, form with
       | Normal_list acc_pat, `Sym_lit (_meta, ((_loc, s) as tk))
         when String.(equal s "&") ->
         At_ampersand_list (acc_pat, token env tk)
       | At_ampersand_list (acc_pat, tk_amb), form ->
         let pat_constructor =
           (* XXX: How about index sensitivity? *)
           G.PatConstructor ((Id (("&", tk_amb), G.empty_id_info ())),
                             [map_binding_form env form])
         in
         After_ampersand_list (pat_constructor :: acc_pat)
       | (Normal_list acc_pat | After_ampersand_list acc_pat),
         `Kwd_lit ((_loc, s) as tk)
         when allow_as && String.(equal s ":as") ->
         At_as_list (acc_pat, (token env tk))
         (* if allow_as then At_as_list (acc_pat, (token env tk))
            else G.error (token env tk) ":as not allowed here." *)
       | Normal_list acc_pat, form ->
         (* This matches also :as in form, when allow_as is false,
          * and it fails as expected. *)
         Normal_list (map_binding_form env form :: acc_pat)
       | At_as_list (acc_pat, _tk_as), `Sym_lit (_meta, ((_, s) as tk))
         (* The symbol after :as must be a simple symbol, and not ellipsis. *)
         when not (String.equal s "..." || (s =~ qualified_name_regex_str)) ->
         let ident = H.str env tk in
         After_as_list (acc_pat, ident)
       | _ -> raise_parse_error "Invalid sequential destructuring."
      )
      (Normal_list [])
      forms
    in
    match state with
    | (Normal_list acc_pat | After_ampersand_list acc_pat) ->
      G.PatList (lb, List.rev acc_pat, rb)
    | After_as_list (acc_pat, ident) ->
      G.PatAs (G.PatList (lb, List.rev acc_pat, rb), (ident, empty_id_info ()))
    | (At_ampersand_list (_, tk) | At_as_list (_, tk)) ->
      raise_parse_error ~related_ast:(Tk tk)
        "Invalid sequential destructuring."

(* 
 * Examples:
 *  
 * - {a :a, b :b, [c1 c2] :c, :as m :or {a (+ 1 1) b 3}}
 * - {:keys [a b c]}
 * - {:keys [x/a y/b]}
 * - {:keys [::x]}
 * - {:domain/keys [a b]} 
 * - There are similar :strs and :syms directives for matching string
 *   and symbol keys.
 *
 * (let [{:keys [x/a y/b]} {:x/a 1 :y/b 2}] [a b])  ;; => [1 2]
 *
 * (let [{:keys [::x]} {:my.ns/x 42}] x)  ;; => 42 
 *
 * ;; Equivalent to {:keys [domain/a domain/b]} 
 * (let [{:domain/keys [a b]} {:domain/a 1 :domain/b 2}] [a b])  ;; => [1 2] 
 *  
 * (let [{:strs [x y]} {"x" 1 "y" 2}] [x y]) ;; x=1, y=2
 * 
 * TODO: careful with naming 'x! keep as string?
 * (let [{:syms [foo bar]} {'foo 1 'bar 2}]
 *   ;; foo=1, bar=2
 *   )
 *
 * (let [{:keys [a b] :or {a 0 b 0}} {:a 5}]
 *   ;; a=5, b=0 (default)
 *   ) 
 * *)
and map_binding_form_map_lit (env : env) ((_meta, (lb, srcs, rb)) : CST.map_lit)
  : G.pattern =
  let lb, rb = token env lb, token env rb in
  let forms = forms_in_source env srcs in
  let make_pat constr_name tk pats =
      G.PatConstructor ((Id ((constr_name, tk), G.empty_id_info ())),
                        pats)
  in 
  let rec with_or_as s_pat tk_pat pats rest =
    match rest with
    | [ `Kwd_lit ((_loc, ":or") as tk_or); `Map_lit default_val ] ->
       make_pat s_pat tk_pat @@
       pats @
       (* TODO: This won't trace (source...) in default_val to a sink in the
        * body, we need to create instantiation code that relates to the variable
        * to which the default value may be assigned. An If-Not-Nil-Assign will
        * suffice, for each symbol in the default_val (which is a map of symbols
        * to values). The function must return G.pattern * expr list where expr
        * is a map of default values; we can have several. Careful with ordering
        * to capture any shadowing that may occur (should not happen in correct
        * code). *)
       [ G.OtherPat (("OrValuePat", (token env tk_or)),
                     [G.E (map_map_form env default_val)]) ]

    | [ `Kwd_lit ((_loc, ":as") as _kwd_tk);
        `Sym_lit (_meta, ((_, s_sym) as sym_tk)) ]
      when 
        (* The symbol after :as must be a simple symbol, and not ellipsis. *)
        not (String.equal s_sym "..."
             || (s_sym =~ qualified_name_regex_str)) ->
      let ident = H.str env sym_tk in
      G.PatAs (make_pat s_pat tk_pat pats, (ident, empty_id_info ()))

    | ((`Kwd_lit ((_loc_or, s_or) as kwd_or)
        :: `Map_lit map_or
        :: `Kwd_lit ((_loc_as, s_as) as kwd_as)
        :: `Sym_lit ((_meta, ((_, s_sym) as sym_tk)) as sym_as) :: []
       | `Kwd_lit ((_loc_as, s_as) as kwd_as)
         :: `Sym_lit ((_meta, ((_, s_sym) as sym_tk)) as sym_as)
         :: `Kwd_lit ((_loc_or, s_or) as kwd_or)
         :: `Map_lit map_or :: []))
      when String.equal s_or ":or" ->
      let pats' =
        pats @ [ G.OtherPat (("OrValuePat", (token env kwd_or)),
                             [G.E (map_map_form env map_or)]) ]
        in 
        with_or_as s_pat tk_pat pats' [ `Kwd_lit kwd_as; `Sym_lit sym_as ]

    | [] -> make_pat s_pat tk_pat pats

    | _ -> raise_parse_error "invalid :or or :and in map binding form."
  in
  match forms with

  (* Special shorthand binding form, eg, {:keys [x y]}. *)
  | `Kwd_lit ((_loc, s) as tk) (* TODO: This also allows :keyz, restrict! *)
    :: `Vec_lit ((_meta, (lb, vec_srcs, rb)) : CST.map_lit) :: rest ->
    (* Example: {:keys [z x/a ::x]} *)
    let pats = List_.map
        (function
          | `Sym_lit ((_meta, (loc, s)) as x) ->
            let key = map_sym_lit_pat_qualified env x in
            let atom_kind, tok_colon, atom_name =
              map_kwd_expr_aux env (loc, ":" ^ s) 
            in
            let value =
              G.OtherPat ((atom_kind, tok_colon), [G.Name atom_name])
            in
            G.PatKeyVal (key, value)
          | `Kwd_lit ((_loc, s) as kwd)
            when String.starts_with ~prefix:"::" s ->
             let s, t = H.str env kwd in
             (* Remove prefix "::" *)
             (* TODO: We can use env namespace here, probably should. *)
             let s_no_colon = String.sub s 2 (String.length s - 2) in
             let _tok_colon, tok_name =
               Tok.split_tok_at_bytepos 2 t
             in
             let key =
               G.PatId ((s_no_colon, tok_name), G.empty_id_info ())
             in
            let atom_kind, tok_colon, atom_name =
              map_kwd_expr_aux env kwd 
            in
            let value =
              G.OtherPat ((atom_kind, tok_colon), [G.Name atom_name])
            in
            G.PatKeyVal (key, value)
          | _ -> raise_parse_error ~related_ast:(G.Tk (token env tk))
                   "Invalid associative binding form"
        )
        (forms_in_source env vec_srcs)
    in
    with_or_as s (token env tk) pats rest

  (* Standard map binding, eg, {x :a, [y z] :b}. *)
  | _bind_form :: `Kwd_lit _  :: _ ->
    let rec keyval_and_rest acc = function
      | bind_form :: `Kwd_lit kwd_lit  :: rest_forms  ->
        let key = map_binding_form env bind_form in
        (* TODO: PatRecord of (dotted_ident * pattern) list bracket *)
        let atom_kind, tok_colon, atom_name =
          map_kwd_expr_aux env kwd_lit
        in
        let value = G.OtherPat ((atom_kind, tok_colon), [G.Name atom_name]) in
        (* let value = G.PatLiteral (map_kwd_lit env kwd_lit) in *)
        keyval_and_rest
          (G.PatKeyVal (key, value) :: acc)
          rest_forms
      | (`Kwd_lit _or_as  :: _  as rest_forms)-> List.rev acc, rest_forms
      | _ -> raise_parse_error "Invalid map binding form."
    in
    let pats, rest = keyval_and_rest [] forms in
    with_or_as "Assoc" (Tok.unsafe_fake_tok "assoc") pats rest

  | _ ->
    raise_parse_error "Invalid map binding form."

and map_fn_params (env : env) (form : CST.form) : G.pattern (* list bracket *) =
  match form with
  (* We relax patterns do allow (defn ... ... ...). *)
  | `Sym_lit (_, (_, "...")) when in_pattern env ->
    let ellipsis = map_binding_form env form in
    G.PatList (Tok.unsafe_fake_tok "[",
               [ellipsis],
               Tok.unsafe_fake_tok "]")
  | `Vec_lit vec_lit -> map_binding_form_vec_lit ~allow_as:false env vec_lit
  | _ -> raise_parse_error "Invalid fn params."

(* TODO:
 *  
 * The condition-map is like pre-post map in defn. 
 * The expressions are wrapped in an implicit do block.
 * Special case: single (do) block, no need to wrap again.
 *
 * If there are no expr, the map is the body, not a prepost
 * condition map.
 *
 * If name? is given, then it allows recursion via that name
 * in the body. Therefore such calls must be resolved to
 * the function itself.
 *
 * But is that needed? What if there is a global var with that
 * name? How about intrafile?
 * Maybe generate a fresh internal name for the function, and
 * replace all calls to name with that fresh name, using a visitor? 
 * The same applies to letfn, defn.
 * Example:
 *  (defn f ....)
 *  ((fn f [x y z] (... f x1 y1 z1)) v1 v2 v3)
 * The signature found for f inside fn will be the one from defn,
 * which is the wrong one!
 * In fact do we need to do the work in Naming_AST?
 * For now we could rebind to nil in the body? Better to find
 * nothing rather than the wrong definition. 
*)
(* (fn name? [param* ] condition-map? expr* )
 * forms is: [param* ] condition-map? expr* *)
and map_fn_single_arity (env : env) (forms : CST.form list) :
  G.pattern (* list bracket *) * G.expr list =
  let params, rest_forms =
    match forms with
    | params :: rest_forms -> map_fn_params env params, rest_forms
    | [] -> assert false (* precondition: we called when form matches. *)
  in
  let body_forms =
    List_.map (map_form env) @@
    match rest_forms with
    (* XXX: Do something with this? It can contain FVs.
     * Add as a kind of attribute? *)
    | `Map_lit _pre_post_map :: (_ :: _ as rest) -> rest
    | _ -> rest_forms
  in
  params, body_forms

and map_fn_multi_arity (env : env) (forms : CST.form list) :
  (G.pattern (* list bracket *) * G.expr list) list =
  List_.map
    (fun form ->
       match form with
       | `List_lit (_meta, (_lp, srcs, _rp) ) ->
         let forms = forms_in_source env srcs in
         map_fn_single_arity env forms
       (* TODO: Be lenient, just drop non List_lit forms? *)
       | _ -> raise_parse_error "Invalid fn multi-arity form.")
    forms

(*
 * (fn name? [param* ] condition-map? expr* )
 *  Sym_lit Sym_lit? Bare_vec_lit Map_lit? expr*
 *
 * (fn name? ([param* ] condition-map? expr* )+)
 *  Sym_lit Sym_lit? (Bare_vec_lit Map_lit? expr* )+
 *
 * params ⇒ positional-param* , or positional-param* & rest-param
 * positional-param ⇒ binding-form
 * rest-param ⇒ binding-form
 * name ⇒ symbol
 *
 * NOTE: name? binds to the fn itself when it's present, and can
 * be used for recursion. 
 * Can we declare_var in naming? This will force it to be distinct from
 * any equal name in scope.
 *
 * NOTE: Passing letfn_sym means we won't require 'fn' :: rest. 
 *)
and map_fn_form
    (* TODO: pass name of function, eg, name in (letfn [name f] e),
     * since it's the closest reasonable token? *)
    ?(letfn_sym:CST.sym_lit option) (env : env) (forms : CST.form list) =
  let fn_sym, forms = 
    match letfn_sym, forms with
    | Some sym, _ -> sym, forms (* letfn case, no 'fn'. *)
    | None, `Sym_lit (_meta, (_loc, "fn") as fn_sym) :: name_or_params
      -> fn_sym, name_or_params
    | None, _ -> raise_parse_error "Invalid fn form."
  in
  match fn_sym, forms with
  | (_meta, ((_loc, fn_or_letfn) as fn_tk)), _ :: _
    when ("fn" =*= fn_or_letfn
          || ("letfn" =*= fn_or_letfn && Option.is_some letfn_sym)) ->
    let name_opt, rest_forms =
      match forms with
      | `Sym_lit x :: rest ->
         let fn_name = map_name env x in
         Some fn_name, rest
      | _ ->
         None, forms
    in
    (* If in letfn, the name must be present; but we don't care. *)
    let patterns_and_bodies =
      match rest_forms with
      | `Vec_lit _ :: _ -> [map_fn_single_arity env rest_forms]
      | `List_lit _ :: _ -> map_fn_multi_arity env rest_forms
      | _ -> raise_parse_error "Invalid fn form."
    in
    let cases =
      List_.map
        (fun (pat, exs) ->
           let expr_block = exprblock exs in
           G.case_of_pat_and_expr (pat, expr_block)
           (* Why commented out? We don't want block under let. Messes
            * up order of bindings. *)
           (* match exs with
              | [] -> raise_parse_error "Function body cannot be empty."
              | [ e ] ->
                G.case_of_pat_and_expr (pat, e)
              | _ ->
                let stmts = List_.map (fun e -> G.exprstmt e) exs in
                let body_block =
                  G.Block (Tok.unsafe_fake_bracket stmts) |> G.s
                in
                G.case_of_pat_and_stmt (pat, body_block) *)
        )
        patterns_and_bodies
    in
    (* TODO: Rebind name as needed with Assign? *)
    let fn_tok = token env fn_tk in
    let param_id = (implicit_param_ident, fn_tok) in
    let param_implicit =
      [G.Param (G.param_of_id param_id)]
    in
    let name_of_id id = Id (id, empty_id_info ())
    in
    let body_stmt =
      G.Switch
        (fn_tok,
         Some (G.Cond (G.N (name_of_id param_id) |> G.e)),
         cases)
      |> G.s
    in
    G.Lambda
      {
        G.fparams = Tok.unsafe_fake_bracket param_implicit;
        frettype = None;
        fkind = (G.LambdaKind, fn_tok);
        fbody = G.FBStmt body_stmt;
      }
    |> G.e
  | _ -> raise_parse_error "Invalid fn form."

(*
 * Usage:
 * 
 * (defn name doc-string? attr-map? [params*] prepost-map? body)
 *  Sym_lit Str_lit? Map_lit? Bare_vec_lit Map_lit? body
 *
 * (defn name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?)
 *  Sym_lit Str_lit? Map_lit? (Bare_vec_lit Map_lit? body)+ Map_lit? 
 * 
 * Same as:
 *  (def name (fn [params* ] exprs* )) or
 *  (def name (fn ([params* ] exprs* )+))
 * with any doc-string or attrs added to the var metadata. prepost-map defines
 * a map with optional keys :pre and :post that contain collections of pre or
 * post conditions.
 *  
 * TODO: Add doc-string to attributes so it can be matched?
 *)
and map_defn_form (env : env) (forms : CST.form list) =
  match forms with
  | `Sym_lit (_meta, ((_loc, ("defn" | "defn-" as sym)) as defn_tk))
    :: `Sym_lit n :: rest_forms ->
    let name_of_defn = map_name env n in
    let doc_string_opt, rest_forms =
      begin match rest_forms with
        | `Str_lit ((_meta, ds) as ds_tok) :: rest ->
            Some (H.str env ds_tok), rest
        | _ -> None, rest_forms
      end
    in
    let attr_map_opt, rest_forms =
      begin match rest_forms with
        | (`Map_lit m as attr_map) :: rest ->
            Some (map_form env attr_map), rest
        | _ -> None, rest_forms
      end
    in
    let patterns_and_bodies =
      match rest_forms with
      | `Sym_lit (_, (_, "...")) :: _
      | `Vec_lit _ :: _ ->
        [map_fn_single_arity env rest_forms]
      | `List_lit _ :: _ ->
        let rest_forms, final_attr_map_opt =
          begin match List.rev rest_forms with
            | (`Map_lit m as attr_map) :: rev_rest ->
                List.rev rev_rest, Some (map_form env attr_map)
            | _ -> rest_forms, None
          end
        in
        map_fn_multi_arity env rest_forms
      | _ -> raise_parse_error "Invalid defn form."
    in
    let cases =
      List_.map
        (fun (pat, exs) ->
           let expr_block = exprblock exs in
           G.case_of_pat_and_expr (pat, expr_block))
        patterns_and_bodies
    in
    (* TODO: Rebind name as needed with Assign? *)
    let defn_tok = token env defn_tk in
    let param_id = (implicit_param_ident, defn_tok) in
    let param_implicit =
      [G.Param (G.param_of_id param_id)]
    in
    let name_of_id id = Id (id, empty_id_info ())
    in
    let body_stmt =
      G.Switch
        (defn_tok,
         Some (G.Cond (G.N (name_of_id param_id) |> G.e)),
         cases)
      |> G.s
    in
    let attrs = match sym with
      | "defn-" ->
        [G.KeywordAttr (G.Private, token env defn_tk)]
      (* | "defmacro" ->
           [G.OtherAttribute (("Macro", token env defn_tk), [])] *)
      | _ -> []
    in
    let entity = 
      {
        G.name = EN name_of_defn;
        G.attrs;
        G.tparams = None;
      }
    in
    let def =
      G.FuncDef
        {
          G.fparams = Tok.unsafe_fake_bracket param_implicit;
          frettype = None;
          fkind = (G.Function, defn_tok);
          fbody = G.FBStmt body_stmt;
        }
    in
    let st = G.DefStmt (entity, def) |> G.s in
    StmtExpr st |> G.e
  | _ ->
    raise_parse_error "Invalid defn form."

(* (let [ binding* ] expr* )
 *  
 * binding => binding-form init-expr
 * The exprs are contained in an implicit do.
 *)
and map_let_binding_forms (env : env) (forms : CST.form) =
  match forms with
  | `Vec_lit (_meta_vec, (_lb, binding_srcs, _rb)) ->
     (* let lb, rb = token env lb, token env rb in *)
     let binding_forms = forms_in_source env binding_srcs in
     let rec aux acc = function
         | [] -> List.rev acc
         (* TODO: when env.extra.kind = Pattern? *)
         (* NOTE: we can do (let [... x $E ...] ...) *)
         | `Sym_lit (_, ((_loc, "...") as ellipsis_tk)) :: rest ->
           let ellipsis_expr =
             G.E (G.Ellipsis (token env ellipsis_tk) |> G.e)
           in
           aux (ellipsis_expr :: acc) rest
         (* What if init_form is ellipsis? *)
         | bind_form :: init_form :: rest ->
           let pat = map_binding_form env bind_form in
           let init_expr = map_form env init_form in
           aux
             (G.E (G.(LetPattern (pat, init_expr) |> e)) :: acc)
             rest
         | _ ->
           raise_parse_error "Invalid let binding forms."
     in
     aux [] binding_forms
  | _ ->
     raise_parse_error "Invalid let binding forms."

and map_let_form (env : env) (forms : CST.form list) =
  match forms with
    | `Sym_lit (_meta_let, ((_loc, "let") as let_tk)) ::
      (`Vec_lit (_meta_vec, (_lb, binding_srcs, _rb)) as bindings_form) ::
      body_forms ->
        let bindings = map_let_binding_forms env bindings_form in
        let body_exprs =
          List_.map (fun form -> G.E (map_form env form))
            body_forms
        in
        (* We need MultiLetPatternBindings to be able to match the bindings
         * with ..., specifically to avoid this (let [$X $V] ...) matching
         * multiple bindings. The IL translation remains flat. *)
        (* TODO:
         * - Use ExprBlock here too? But it creates scope!
         *   And by definition we want same scope as below body!
         * - Use wrapper OtherExpr to 'tag' the AST we produce in order
         *   to distinguish constructs (because now everything will look
         *   the same.) *)
        let bindings_other_expr =
            G.OtherExpr (("MultiLetPatternBindings", token env let_tk),
                         bindings)
            |> G.e
        in
        G.OtherExpr (("ExprBlock", (token env let_tk)),
                     (G.E bindings_other_expr) :: body_exprs)
        |> G.e
    | _ ->
      raise_parse_error "Invalid let form."

(* TODO: Do we really want this? And how about (... e) or (... e ...)?
 * For the later we can write without parentheses, and it will be combined
 * in a block for us. We can also write (do ...) which is the same as (...).
 * Currently if we have a pattern (... e1 e2) it will be interpreted as a
 * block, not as a call. *)
(* (...) more naturally maps to ExprBlock(...) rather than Call(..., []). *)
and map_ellipsis_list_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta, ((_loc, "...") as ellipsis_tk)) :: _ ->
    exprblock (List_.map (map_form env) forms)
  | _ ->
    (* We only call this function when the forms start with ... . *)
    assert false

and map_apply_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta, ((_loc, "apply") as apply_tk)) :: (_func :: _args as rest) ->
    (* The last parameter of the function is unpacked as collection and spread
     * out as N arguments. For this reason we tag it, and AST_to_IL can detect
     * that and translate to the correct semantics. *)
    let call = map_call_form env rest in
    G.OtherExpr (("Apply", token env apply_tk), [G.E call]) |> G.e
  | _ ->
    raise_parse_error "Invalid apply form."

and map_call_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | (`Sym_lit (_meta, ((_loc, sym_name) as sym_tk)) as sym) :: rest ->
    let op, arg_forms =
      match sym_name with
      | "+" -> Some G.Plus, rest
      | "-" -> Some G.Minus, rest
      | "*" -> Some G.Mult, rest
      | "/" -> Some G.Div, rest
      | "mod" -> Some G.Mod, rest
      | "Math/pow" -> Some G.Pow, rest
      | "quot" -> Some G.FloorDiv, rest
      | "bit-shift-left" -> Some G.LSL, rest
      | "unsigned-bit-shift-right" -> Some G.LSR, rest
      | "bit-shift-right" -> Some G.ASR, rest
      | "bit-or" -> Some G.BitOr, rest
      | "bit-xor" -> Some G.BitXor, rest
      | "bit-and" -> Some G.BitAnd, rest
      | "bit-not" -> Some G.BitNot, rest
      | "bit-and-not" -> Some G.BitClear, rest
      | "and" -> Some G.And, rest
      | "or" -> Some G.Or, rest
      | "=" -> Some G.Eq, rest
      | "not=" -> Some G.NotEq, rest
      | "identical?" -> Some G.PhysEq, rest
      | "<" -> Some G.Lt, rest
      | "<=" -> Some G.LtE, rest
      | ">" -> Some G.Gt, rest
      | ">=" -> Some G.GtE, rest
      | "compare" -> Some G.Cmp, rest
      | "str" -> Some G.Concat, rest
      | "conj" -> Some G.Append, rest
      | "re-find" -> Some G.RegexpMatch, rest
      | "range" -> Some G.Range, rest
      | "count" -> Some G.Length, rest
      | "contains?" -> Some G.In, rest
      | "instance?" -> Some G.Is, rest
      | "not" -> begin
          match rest with
          | `Sym_lit (_, (_, "identical?")) :: rest -> Some G.NotPhysEq, rest
          | `Sym_lit (_, (_, "contains?")) :: rest -> Some G.NotIn, rest
          | `Sym_lit (_, (_, "instance?")) :: rest -> Some G.NotIs, rest
          | _ -> Some G.Not, rest
        end
      | _ -> None, rest (* Because first is the function symbol. *)
    in
    let args =
      List_.map (map_form env) arg_forms
    in
    begin match op with
      | Some op ->
        G.opcall (op, token env sym_tk) args
      | _ ->
        G.Call (map_form env sym,
                Tok.unsafe_fake_bracket
                  (List_.map (fun e -> G.Arg e)
                     args))
        |> G.e
    end
  (* XXX: Do we want / need this? We won't be able to match such
   * fragments with ($F ...) since this pattern becomes a call.
   * But (:$K ...) and (::$K ...) still work. On the other hand,
   * :kwd is also a function!
   *
   * TODO How about:
   *
   * (:kwd arg default-value)
   *
   * (get arg :kwd)
   * (get arg :kwd default-value)
   *
   * Leaving the code below commented out in case we want to properly
   * support DotAccess later. 
   *)
  (* | (`Kwd_lit ((_loc, kwd_name) as kwd_tk) as kwd) :: [ arg_form ] ->
       begin match map_form env kwd with
       | {e = G.OtherExpr (("Atom", atom_tk), [G.Name n]); _} ->
         G.DotAccess (map_form env arg_form, atom_tk, G.FN n) |> G.e
       | _ ->
         raise_parse_error ~related_ast:(Tk (token env kwd_tk))
           "Invalid keyword call form."
       end *)
  | fn_expr :: arg_forms ->
    let fn_expr_mapped = map_form env fn_expr in
    let arg_exprs_mapped =
      List_.map (fun arg -> G.Arg (map_form env arg)) arg_forms
    in
    (* This works, but then `(sink $S)` matches multiple parameters
     * since they are simply a list container. So we do this in AST_to_IL
     * now. *)
    (* let arg_exprs_mapped =
         List_.map (map_form env) arg_forms
         |> (fun args ->
             [ G.Arg 
                 (G.Container
                    (G.List, Tok.unsafe_fake_bracket args)
                  |> G.e) ])
       in *)
    G.Call (fn_expr_mapped, Tok.unsafe_fake_bracket arg_exprs_mapped)
    |> G.e
  | [] ->
    (* Invalid syntax, nothing to call. *)
    raise_parse_error "Invalid call form."

(* (loop [binding* ] expr* ) *)
and map_loop_form (env : env) (forms : CST.form list) : G.expr =
  (* XXX: temporarily handle as function application.
   * This is not correct but will avoid failing to parse for now. *)
  match forms with
    | `Sym_lit (_meta_let, ((_loc, "loop") as loop_tk)) ::
      (`Vec_lit (_meta_vec, (_lb, binding_srcs, _rb)) as bindings_form) ::
      body_forms ->
        let bindings = map_let_binding_forms env bindings_form in
        let body_exprs =
          List_.map (fun form -> G.E (map_form env form))
            body_forms
        in
        let bindings_other_expr =
            G.OtherExpr (("LoopPatternBindings", token env loop_tk),
                         bindings)
            |> G.e
        in
        G.OtherExpr (("Loop", (token env loop_tk)),
                     (G.E bindings_other_expr) :: body_exprs)
        |> G.e
    | _ ->
      raise_parse_error "Invalid loop form."

and map_recur_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_let, ((_loc, "recur") as recur_tk)) ::
      arg_forms ->
        let arg_exprs =
          List_.map (fun form -> G.E (map_form env form))
            arg_forms
        in
        G.OtherExpr (("Recur", (token env recur_tk)),
                     arg_exprs)
        |> G.e
    | _ ->
      raise_parse_error "Invalid recur form."

(*
  letfn
  special form
  Usage: (letfn [fnspecs*] exprs* )
  fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)

  Takes a vector of function specs and a body, and generates a set of
  bindings of functions to their names. All of the names are available
  in all of the definitions of the functions, as well as the body.
  Added in Clojure version 1.0

  TODO: Naming requires 2 passes, all names are available in all functions.
    It doesn't work well as it is: in
      (letfn [(f ...) (g ...)] ())
    the body of f does not contain a resolved g. 
*)
and map_letfn_form (env : env) (forms : CST.form list) =
  match forms with
    | `Sym_lit (_meta_letfn, ((_loc, "letfn") as letfn_tk) as letfn_sym) ::
      `Vec_lit (_meta_vec, (_lb, binding_srcs, _rb)) :: body_forms ->
        let binding_forms = forms_in_source env binding_srcs in
        let bindings =
          let rec aux acc = function
              | [] -> List.rev acc
              (* TODO: when env.extra.kind = Pattern? *)
              | `Sym_lit (_, ((_loc, "...") as ellipsis_tk)) :: rest ->
                let ellipsis_expr =
                  G.E (G.Ellipsis (token env ellipsis_tk) |> G.e)
                in
                aux (ellipsis_expr :: acc) rest
              | `List_lit (_meta, (lp, init_srcs, rp)) :: rest ->
                let lb, rb = token env lp, token env rp in
                (* Sym_lit x :: _forms as forms *)
                let init_forms = forms_in_source env init_srcs in
                (* The binding form is the actual name of the function,
                 * which must exist. *)
                let bind_form =
                  match init_forms with
                  | (`Sym_lit _ as bind_form) :: _rest -> bind_form
                  | _ -> raise_parse_error "Invalid letfn binding form."
                in
                let pat = map_binding_form env bind_form in
                let init_expr = map_fn_form ~letfn_sym env init_forms in
                aux
                  (* TODO: Simple names only, so no letpattern needed,
                   * but works. *)
                  (G.E (G.(LetPattern (pat, init_expr) |> e)) :: acc)
                  rest
              | _ ->
                raise_parse_error "Invalid letfn binding forms."
          in
          aux [] binding_forms
        in
        let body_exprs =
          List_.map (fun form -> G.E (map_form env form))
            body_forms
        in
        let bindings_other_expr =
            G.OtherExpr (("MultiLetFnBindings", (token env letfn_tk)), bindings)
            |> G.e
        in
        G.OtherExpr (("ExprBlock", (token env letfn_tk)),
                     (G.E bindings_other_expr)
                     :: body_exprs)
        |> G.e 
    | _ ->
      raise_parse_error "Invalid letfn form."

(* If *)

(* The expr variation a la ternary. *)
(* and map_if_form (env : env) (forms : CST.form list) =
     match forms with
     | _if :: test_form :: if_branch :: else_branch_opt ->
       let test_expr = map_form env test_form in
       let if_expr = map_form env if_branch in
       let else_expr_opt =
         match else_branch_opt with
         | [ else_branch ] -> map_form env else_branch
         | [] -> L (Null (G.fake "nil")) |> G.e 
         | _ -> raise_parse_error "Invalid if form."
       in
       (\* TODO: Missing token makes range not good: missing 'if'. *\)
       G.Conditional (test_expr, if_expr, else_expr_opt)
       |> G.e
     | _ ->
       raise_parse_error "Invalid if form." *)

(* The stmt variation. *)
(* What about `(if ...)`? Should this be something we can match?
 * If so we have partial if. *)
and map_if_form (env : env) (forms : CST.form list) =
  match forms with
  | `Sym_lit (_meta_if, ((_loc, "if") as if_tk))
    :: test_form :: if_branch :: else_branch_opt ->
    let test_expr = map_form env test_form in
    let if_expr = map_form env if_branch |> G.exprstmt in
    let else_expr_opt =
      match else_branch_opt with
      | [ else_branch ] -> Some (map_form env else_branch |> G.exprstmt)
      | [] -> None
      | _ -> raise_parse_error "Invalid if form."
    in
    G.If ((token env if_tk), (G.Cond test_expr), if_expr, else_expr_opt)
    |> G.s |> G.stmt_to_expr
  (* We need the case below because of things like:
   * `(-> test (if if_branch else_branch))`
   * `(-> test (if if_branch))`
   * or even:
   * `(->> else_branch (if test if_branch))`
   * `(->> if_branch (if test))`
   * Of course these should be fairly uncommon... *)
  | `Sym_lit (_meta_if, ((_loc, "if") as if_tk))
    :: (_ :: _ as rest) when List.length rest < 3 ->
    let tok = token env if_tk in
    let if_exprs =
      List_.map (fun re -> G.E (map_form env re)) rest
    in 
    G.OtherExpr (("PartialIf", token env if_tk), if_exprs) |> G.e
  | _ ->
    (* Here the form should be logged? *)
    raise_parse_error "Invalid if form."

and map_if_not_form (env : env) (forms : CST.form list) =
  match forms with
  | `Sym_lit (_meta_if, ((_loc, "if-not") as if_not_tk))
    :: test_form :: if_branch :: else_branch_opt ->
    G.OtherExpr
      (("IfNot", token env if_not_tk),
       [
         G.E (map_form env test_form);
         G.E (map_form env if_branch);
         (match else_branch_opt with
          | [ else_branch ] -> G.E (map_form env else_branch)
          | [] -> G.E (L (Null (G.fake "nil")) |> G.e)
          | _ -> raise_parse_error "Invalid if-not form.")
       ])
    |> G.e
  | _ ->
    raise_parse_error "Invalid if-not form."

(*
 * (do expr* )
 * Evaluates the expressions exprs in order and returns the value of the last.
 * If no expressions are supplied, returns nil.
 *)
and map_do_form (env : env) (forms: CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_do, ((_loc, "do") as do_tk)) :: expr_forms ->
    let do_exprs = List_.map
        (fun e_form -> G.E (map_form env e_form))
        expr_forms
    in
    let do_exprs = match do_exprs with
      | [] -> [G.E (L (Null (G.fake "nil")) |> G.e)]
      | _ -> do_exprs
    in
    G.OtherExpr (("ExprBlock", (token env do_tk)), do_exprs)
    |> G.e
  | _ ->
    raise_parse_error "Invalid do form."

(*
 * Usage: (-> x forms* )
 * Threads the expr through the forms. Inserts x as the
 * second item in the first form, making a list of it is not a
 * list already. If there are more forms, inserts the first form
 * as the second item in second form, etc.
 *)
and insert_threaded
    (env: env) (insert_pos: insert_pos) (value_form: CST.form) (target_form: CST.form)
  : CST.form =
  let insert_last x xs = List.rev (x :: (List.rev xs)) in
  match target_form with
  | `List_lit (meta, (lp, srcs, rp)) ->
    let src_forms = forms_in_source env srcs in
    begin match src_forms with
    | hd_form :: src_forms_rest -> 
      let srcs' = match insert_pos with
        | Insert_first -> hd_form :: value_form :: src_forms_rest
        | Insert_last -> insert_last value_form src_forms
      in
      `List_lit (meta, (lp, List_.map (fun src -> `Form src) srcs', rp))
    | _ ->
      raise_parse_error "Invalid thread target form: list is empty."
    end
  | _ ->
    (* Create fake tokens for brackets since they will (should) be thrown away anyway. *)
    let module T = Tree_sitter_run.Token in
    let module L = Tree_sitter_run.Loc in
    let pos = L.{row = -1; column = -1} in
    let loc = L.{start = pos; end_ = pos} in
    match target_form with
    | `Sym_lit (_meta, ((_loc, "...") as ellipsis_tk)) when in_pattern env ->
      `Semg_deep_exp ((loc, "<..."), [], value_form, [], (loc, "...>"))
    | _ ->
      `List_lit ([], ((loc, "("), [`Form target_form; `Form value_form], (loc, ")")))

(* How about (-> e e1 ... e2) ie ellipsis. Direct translation makes it:
 * (e2 (... (e1 e))) and what will it match? Only call. Users would expect it to
 * match any sequence of e in the pipeline. But this cannot be macroexpanded properly.
 * So we convert to deep ellipsis. This way we should get (e2 (<... (e1 e) ...>)) which
 * is closer to the intention of the user. This has now been implemented (see above
 * function). *)
and map_thread_first_last_form
    (env : env) (forms: CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_thread,
                ((_loc, (("->" | "->>" | "some->" | "some->>") as first_or_last))
                 as thread_tk))
      :: (v_form :: rest_forms) ->
        let insert_pos =
          match first_or_last with
          | ("->" | "some->") -> Insert_first
          | _ (* "->>" *) -> Insert_last
        in
        let expanded =
          List.fold_left (insert_threaded env insert_pos) v_form rest_forms
        in
        let body = map_form env expanded in
        G.OtherExpr ((first_or_last, (token env thread_tk)), [ G.E body ])
        |> G.e
    | _ ->
        raise_parse_error "Invalid thread-first / thread_last form."

and ensure_form_is_list (env : env) (form: CST.form) : CST.form =
  match form with
  | `List_lit _ -> form
  | _ ->
    let module T = Tree_sitter_run.Token in
    let module L = Tree_sitter_run.Loc in
    let pos = L.{row = -1; column = -1} in
    let loc = L.{start = pos; end_ = pos} in
    `List_lit ([], ((loc, "("), [`Form form], (loc, ")")))

(*
 * (let [g (gensym)
 *         steps (map (fn [[test step]] `(if ~test (-> ~g ~step) ~g))
 *                    (partition 2 claues))]
 *     `(let [~g ~expr
 *            ~@(interleave (repeat g) (butlast steps))]
 *        ~(if (empty? steps)
 *           g
 *           (last steps)))))
 *
 * opengrep.clj=> (macroexpand-1 '(cond-> 1 true (+ 1) false (- 1)))
 * replacing a gensym such as G__2174 with g: 
 * (let [g 1
 *       g (if true (-> g (+ 1)) g)]
 *  (if false (-> g (- 1)) g)) 
 *)
and map_cond_thread_first_last_form (env : env) (forms: CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_thread,
                ((loc, (("cond->" | "cond->>") as first_or_last)) as thread_tk))
      :: (v_expr :: clauses as rest_forms) ->

      (* Note that we piggyback on the location of "cond->" for the new
       * variable we are introducing. *)
      let fake_let_var_sym_lit = ([], ((loc, fake_variable_ident))) in
      let fake_let_var_sym = `Sym_lit fake_let_var_sym_lit in
      let pos =
        match first_or_last with
        | "cond->" -> Insert_first
        | _ (* cond->> *) -> Insert_last
      in

      let test_expr_pairs =
        let rec aux acc = function
          | test_expr_form :: body_expr_form :: rest ->
            aux ((test_expr_form, body_expr_form) :: acc) rest
          | (`Sym_lit (_, ((_, "..."))) as ellipsis) :: [] when in_pattern env ->
            List.rev ((ellipsis, ellipsis) :: acc)
          | [] -> List.rev acc
          | _ ->
            raise_parse_error
              "Expected even number of test and body exprs"
        in
        aux [] clauses
       in
       let clauses_threaded =
         List_.map
           (fun (test_expr_form, body_expr_form) ->
              (* We keep ... ... as is in patterns, so that
               * we can convert to a single ellipsis in between
               * the synthetic let bindings created below.
               * This allows to match with patterns like:
               * (cond-> e ... ... $TEST $FORM). *)
              match test_expr_form, body_expr_form with
              | `Sym_lit (_, ((_, "..."))), `Sym_lit (_, ((_, "...")))
                when in_pattern env ->
                (test_expr_form, body_expr_form)
              | _ ->
                (test_expr_form,
                 insert_threaded env pos fake_let_var_sym body_expr_form))
           test_expr_pairs
       in
       (* Now we have clauses (test, body') where body' is (-> g body),
        * resp. for ->>. *)
       begin match clauses_threaded with
         | [] ->
           let e = map_form env v_expr (* no need for binding at all *) in 
           G.OtherExpr ((first_or_last, (token env thread_tk)), [ G.E e ])
           |> G.e
         | _ ->
           let last_test, last_expr, rest_test_exprs =
             begin match List.rev clauses_threaded with
             | (last_test_form, last_expr_form) :: rest_rev_forms ->
               (map_form env last_test_form,
                map_form env last_expr_form,
                (List.rev
                  (List_.map (fun (tst_form, expr_form) ->
                    (map_form env tst_form, map_form env expr_form))
                     rest_rev_forms)))
             | _ -> assert false
             end
           in
           (* TODO: Maybe use Tok.ExpandedTok? No, won't work, no ranges
            * and wrong results. But would be good to distinguish since
            * for example we cannot show the intermediate variable. *)
           let fake_g = H.str env (snd fake_let_var_sym_lit) in
           let make_tmp () =
             let id_info = G.empty_id_info ~hidden:true () in
             G.PatId (fake_g, id_info)
           in
           let make_if_thread_else previous_ident pos test_expr body_expr =
             let ident_expr =
               G.N previous_ident |> G.e
             in
             (* TODO: Make condition more specific. *)
             G.If (G.fake "if-cond", (G.Cond test_expr),
                   body_expr |> G.exprstmt,
                   Some (ident_expr |> G.exprstmt))
             |> G.s |> G.stmt_to_expr
           in
           let make_let new_pat expr = 
             G.LetPattern (new_pat, expr) |> G.e
           in
           let init_pat_id = make_tmp () in
           let init_exr = 
             make_let init_pat_id (map_form env v_expr)
           in
           let last_binding, intermediate_binding_exprs =
             List.fold_left_map
               (fun last_binding (test_expr, body_expr) ->
                  begin match last_binding with
                 | G.PatId (prev_ident, id_info) ->
                     begin match test_expr.G.e, body_expr.G.e with
                       (* So we can match using: (cond-> e ... ... $TEST $FORM). *)
                       | G.Ellipsis tk, G.Ellipsis _ when in_pattern env ->
                         last_binding, test_expr
                       | _ -> 
                         let new_tmp = make_tmp () in
                         let if_expr =
                             make_if_thread_else
                               (G.Id (prev_ident, id_info))
                               pos
                               test_expr
                               body_expr
                         in
                         (new_tmp, (make_let new_tmp if_expr))
                     end
                 | _ -> assert false
                  end
               )
               init_pat_id
               rest_test_exprs
           in
           let last_id = match last_binding with
             | G.PatId (last_ident, id_info) -> G.Id (last_ident, id_info)
             | _ -> assert false
           in
           let last_if_expr =
             begin match last_test.G.e, last_expr.G.e with
               (* So we can match using: (cond-> e ... ...). *)
               | G.Ellipsis tk, G.Ellipsis _ when in_pattern env ->
                 last_test
               | _ -> make_if_thread_else last_id pos last_test last_expr
             end
           in
           let body_exprs =
             List_.map (fun e -> G.E e)
               (init_exr :: intermediate_binding_exprs @ [last_if_expr])
           in
           G.OtherExpr ((first_or_last, (token env thread_tk)), body_exprs)
           |> G.e
         end
    | _ ->
        raise_parse_error "Invalid cond-> / cond->> form."

(*
 * as->
 * Usage: (as-> expr name & forms)
 * Binds name to expr, evaluates the first form in the lexical context
 * of that binding, then binds name to that result, repeating for each
 * successive form, returning the result of the last form. 
 *
 * Expansion: 
 * opengrep.clj=> (macroexpand-1 '(as-> 5 x (+ x x) (- x x)))
 * (clojure.core/let [x 5 x (+ x x)] (- x x))
 *
 * This is also allowed in the repl (but seems not to be the intention): 
 * opengrep.clj=> (macroexpand-1 '(as-> 5 [x1 x2] (+ x1 x1) (- x1 x1) (str x2)))
 * (clojure.core/let [[x1 x2] 5 [x1 x2] (+ x1 x1) [x1 x2] (- x1 x1)] (str x2))
 *
 * as-> v p f1 .... fn
 * becomes:
 * (E v) (P p) - (E f1) (P p) - (E f2) (P p)  - ... - (E fn-1) (P p) - E Efn 
 *
 * If there is not rest forms, then body becomes p.
 *)
and map_as_thread_form (env : env) (forms: CST.form list) : G.expr =
  let map_pat_and_rest pat_and_rest_forms =
    match pat_and_rest_forms with
      | pat_form :: rest_forms ->
        let pat = map_binding_form env pat_form in
        let rest_exprs = List_.map (map_form env) rest_forms in
        let rec interleave_pat pat exprs =
          match exprs, env.extra.kind with
          | [], _ -> [] 
          | [ last_expr ], _ -> G.E last_expr :: []
          | ({e = G.Ellipsis _; _} as expr) :: rest_exprs, Pattern ->
            (* Special case: an ellipsis should abstract over P, E pairs. *)
            (* But not for v_expr; in that case we want (as-> ... p exprs),
             * hence v_expr is dealt with separately. *)
            G.E expr :: interleave_pat pat rest_exprs
          | expr :: rest_exprs, _ ->
            (* XXX: Need to have fresh id_info, force simple name (PatId) and
             * replace id_info, or use visitor, or live with this trick below
             * (create new pattern). For now this trick is sufficient. *)
            G.E expr :: G.P (map_binding_form env pat_form) (* pat *)
            :: interleave_pat pat rest_exprs
        in
        G.P pat :: (interleave_pat pat rest_exprs)
      | _ ->
        raise_parse_error "Invalid as-> form."
  in
  match forms with
  | `Sym_lit (_meta_thread, ((_loc, "as->") as thread_tk))
    :: v_expr_form :: ( pat_form :: rest_forms as pat_and_rest ) ->
    let pat_and_rest = map_pat_and_rest pat_and_rest in
    let v_expr = map_form env v_expr_form in
    let interleaved_expr_pat =
      G.E v_expr :: pat_and_rest
    in
    G.OtherExpr (("as->", (token env thread_tk)), interleaved_expr_pat)
    |> G.e
  | _ ->
      raise_parse_error "Invalid as-> form."

(*
 * Usage: (when test & body)
 * Evaluates test. If logical true, evaluates body in an implicit do.
 *  
 * TODO: We can ignore the If part... just test and do body?
 * Not very bulletproof though for future improvements in the engine.
 *)
and map_when_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_when, ((_loc, "when") as when_tk))
    :: (_test_form :: _rest_forms as test_and_rest_forms) ->
    let when_forms =
      List_.map (fun when_form -> G.E (map_form env when_form))
        test_and_rest_forms
    in
    G.OtherExpr (("When", token env when_tk), when_forms) |> G.e
  | _ ->
    raise_parse_error "Invalid when form."

and map_when_not_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_when, ((_loc, "when-not") as when_not_tk))
    :: (_test_form :: _rest_forms as test_and_rest_forms) ->
    let when_forms =
      List_.map (fun when_form -> G.E (map_form env when_form))
        test_and_rest_forms
    in
    G.OtherExpr (("WhenNot", token env when_not_tk), when_forms) |> G.e
  | _ ->
    raise_parse_error "Invalid when-not form."

(*
 *  when-let
 *  Usage: (when-let bindings & body)
 *  bindings => binding-form test
 *  
 *  When test is true, evaluates body with binding-form bound to the value
 *  of test.
 *)
and map_when_let_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_when, ((_loc, "when-let") as when_let_tk)) ::
    (`Vec_lit (_meta_vec, (_lb, _binding_srcs, _rb)) as bindings_form) ::
    body_forms ->
    (* NOTE: Valid syntax requires one binding only, of course we
     * can also have (when-let [...] body). We don't check for now. *)
    let bindings = map_let_binding_forms env bindings_form in
    let bindings_other_expr =
        G.OtherExpr (("WhenLetPatternBindings", token env when_let_tk),
                     bindings)
        |> G.e
    in
    let body_forms =
      List_.map (fun when_form -> G.E (map_form env when_form))
        body_forms
    in
    G.OtherExpr (("WhenLet", token env when_let_tk),
                 G.E bindings_other_expr :: body_forms)
    |> G.e
  | _ ->
    raise_parse_error "Invalid when-let form."

(*
 * if-let
 * Usage: (if-let bindings then)
 *        (if-let bindings then else & oldform)
 * bindings => binding-form test
 * 
 * If test is true, evaluates then with binding-form bound to the value of 
 * test, if not, yields else
 *)
and map_if_let_some_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_let, ((_loc, ( "if-let" | "if-some" as if_let_some))
                            as let_tk)) ::
      (`Vec_lit (_meta_vec, (_lb, binding_srcs, _rb)) as bindings_form) ::
      body_forms ->

        let bindings = map_let_binding_forms env bindings_form in
        if List.length bindings > 1 then
          raise_parse_error "if-let/some requires exactly one binding to a form."
        else
          ();

        let then_, else_opt =
          match
            List_.map
              (fun form -> G.E (map_form env form))
              body_forms
          with
          | [ then_expr ] -> then_expr, None
          | then_expr :: else_expr :: [] ->
            then_expr, Some else_expr
          | _ ->
            raise_parse_error (spf "Invalid %s form" if_let_some)
        in
        (* We need to be able to match the bindings with ..., specifically
         * to avoid this (if-let [$X $V] ...) matching multiple bindings. *)
        let if_let_or_if_some =
            match if_let_some with
            | "if-let" -> "IfLet"
            | "if-some" -> "IfSome"
            | _ -> assert false
        in
        let bindings_other_expr =
            G.OtherExpr (((spf "%sPatternBindings" if_let_or_if_some),
                          token env let_tk),
                         bindings)
            |> G.e
        in
        let bindings_and_then_branch =
          (* Scope of bindings is limited to this block. *)
          G.OtherExpr (("ExprBlock", (token env let_tk)),
                       (G.E bindings_other_expr) :: [ then_ ])
          |> G.e
          |> (fun e -> G.E e)
        in
        let else_branch =
          match else_opt with
          | Some else_expr -> [ else_expr ]
          | None -> []
        in
        G.OtherExpr ((if_let_or_if_some, (token env let_tk)),
                     bindings_and_then_branch :: else_branch)
        |> G.e
    | _ ->
      raise_parse_error "Invalid if-let/some form."

(*
 * when-some
 * Usage: (when-some bindings & body)
 * bindings => binding-form test
 * 
 * When test is not nil, evaluates body with binding-form bound to the
 * value of test.
 *)
and map_when_some_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_when, ((_loc, "when-some") as when_some_tk)) ::
    (`Vec_lit (_meta_vec, (_lb, _binding_srcs, _rb)) as bindings_form) ::
    body_forms ->
    (* NOTE: Valid syntax requires one form only, of course we
     * can also have (when-some [...] body). We don't check for now. *)
    let bindings = map_let_binding_forms env bindings_form in
    let bindings_other_expr =
        G.OtherExpr (("WhenSomePatternBindings", token env when_some_tk),
                     bindings)
        |> G.e
    in
    let body_forms =
      List_.map (fun when_form -> G.E (map_form env when_form))
        body_forms
    in
    G.OtherExpr (("WhenSome", token env when_some_tk),
                 G.E bindings_other_expr :: body_forms)
    |> G.e
  | _ ->
    raise_parse_error "Invalid when-some form."

(*
 * when-first
 * Usage: (when-first bindings & body)
 * bindings => x xs
 * 
 * Roughly the same as (when (seq xs) (let [x (first xs)] body))
 * but xs is evaluated only once.
 *
 * TODO: In fact the pattern binding is on the first element of the
 * rhs expression, needs different encoding.
 *  
 * opengrep.clj=> (when-first [[y1 y2] [[1 2] 3 4]] y1)
 * 1
 *
 * TODO: Wrap bindings in PatList (bindings, ...) ?
 * This will give the correct semantics except for the nil check. 
 *)
and map_when_first_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_when, ((_loc, "when-first") as when_some_tk)) ::
    (`Vec_lit (_meta_vec, (_lb, _binding_srcs, _rb)) as bindings_form) ::
    body_forms ->
    (* NOTE: Valid syntax requires one form only, of course we
     * can also have (when-first [...] body). We don't check for now. *)
    let bindings = map_let_binding_forms env bindings_form in
    let bindings_other_expr =
        G.OtherExpr (("WhenFirstPatternBindings", token env when_some_tk),
                     bindings)
        |> G.e
    in
    let body_forms =
      List_.map (fun when_form -> G.E (map_form env when_form))
        body_forms
    in
    G.OtherExpr (("WhenFirst", token env when_some_tk),
                 G.E bindings_other_expr :: body_forms)
    |> G.e
  | _ ->
    raise_parse_error "Invalid when-first form."

and map_anon_func_form
    (env : env)
    (meta_list : CST.metadata_lit list)
    (v_tok : Tree_sitter_run.Token.t)
    (((lp, srcs, rp) as bare_list) : CST.bare_list_lit)
  : G.expr =
  let _v1TODO = List_.map (map_metadata_lit env) meta_list in
  let tok = token env v_tok in
  let body_expr = map_list_form env (meta_list, bare_list) in
  encode_short_lambda env tok body_expr

and map_comment_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta_let, ((_loc, "comment") as comment_tk)) :: _rest -> 
    L (Null (H.token env comment_tk)) |> G.e
  | _ -> assert false

and map_ns_form (env : env) (forms : CST.form list) : G.expr =
  (* TODO: DirectiveStmt with imported modules (directive_kind).
   * But for now a quick hack is probably enough.
   * TODO: However, we should treat as quoted. *)
  match forms with
    | `Sym_lit (_meta_ns, ((_loc, "ns") as ns_tk)) :: body_forms ->
      let as_exprs = List_.map
          (fun form -> G.E (map_form env form))
          body_forms
      in
      let directive = G.OtherDirective
          (("NsDirective", token env ns_tk), as_exprs)
      in
      G.DirectiveStmt (G.d directive) |> G.s |> G.stmt_to_expr
    | _ ->
      raise_parse_error "Invalid ns form."

and map_require_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_ns, ((_loc, "require") as req_tk)) :: body_forms ->
      let as_exprs = List_.map
          (fun form -> G.E (map_form env form))
          body_forms
      in
      let directive = G.OtherDirective
          (("RequireDirective", token env req_tk), as_exprs)
      in
      G.DirectiveStmt (G.d directive) |> G.s |> G.stmt_to_expr
    | _ ->
      raise_parse_error "Invalid require form."

and map_use_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_ns, ((_loc, "use") as req_tk)) :: body_forms ->
      let as_exprs = List_.map
          (fun form -> G.E (map_form env form))
          body_forms
      in
      let directive = G.OtherDirective
          (("UseDirective", token env req_tk), as_exprs)
      in
      G.DirectiveStmt (G.d directive) |> G.s |> G.stmt_to_expr
    | _ ->
      raise_parse_error "Invalid use form."

(*
 * (throw expr)
 * The expr is evaluated and thrown, therefore it should yield an instance of some
 * derivee of Throwable. 
 *)
and map_throw_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
    | `Sym_lit (_meta_throw, ((_loc, "throw") as throw_tk)) :: body_form :: [] ->
      G.Throw (token env throw_tk, map_form env body_form, G.fake "")
      |> G.s
      |> G.stmt_to_expr
    | _ ->
      raise_parse_error "Invalid throw form."

(*
 * (try expr* catch-clause* finally-clause?)
 *
 * catch-clause → (catch classname name expr* )
 * finally-clause → (finally expr* )
 *
 * The exprs are evaluated and, if no exceptions occur, the value of the
 * last expression is returned. If an exception occurs and catch-clauses
 * are provided, each is examined in turn and the first for which the thrown
 * exception is an instance of the classname is considered a matching catch-clause.
 * If there is a matching catch-clause, its exprs are evaluated in a context in
 * which name is bound to the thrown exception, and the value of the last is the
 * return value of the function. If there is no matching catch-clause, the exception
 * propagates out of the function. Before returning, normally or abnormally, any
 * finally-clause exprs will be evaluated for their side effects.
 *)
and map_try_catch_finally_form (env : env) (forms : CST.form list) : G.expr =
  match forms with
  | `Sym_lit (_meta, ((_loc, ("try")) as try_tk)) :: rest_forms ->
    let state =
      List.fold_left
        (fun acc form ->
           match form with
           | `List_lit (_meta, (lp, srcs, rp)) ->
             let list_forms = forms_in_source env srcs in
             begin match acc, list_forms with
               | ( Try_exprs _ | At_catch_clause _ ) :: acc',
                 `Sym_lit (_meta, ((_loc, ("catch")) as catch_tk))
                 :: `Sym_lit class_name :: `Sym_lit ex_name
                 :: catch_exprs ->
                 let class_name_expr = G.Name (map_name env class_name) in
                 let ex_name_ident = G.P (map_sym_lit_pat env ex_name) in
                 let exprs =
                   List_.map (map_form env) catch_exprs
                 in
                 begin match acc with
                   | Try_exprs _ :: acc' ->
                     let tk = H.str env catch_tk in
                     At_catch_clause
                       [ snd tk,
                         G.OtherCatch (tk,
                                       [class_name_expr; ex_name_ident]),
                         exprs ] :: acc
                   | At_catch_clause catch_state :: acc' ->
                     let tk = H.str env catch_tk in
                     At_catch_clause
                       ((snd tk,
                         G.OtherCatch (tk,
                                       [class_name_expr; ex_name_ident]),
                         exprs) :: catch_state)
                     :: acc'
                   | _ -> assert false
                 end
               | ( Try_exprs _ | At_catch_clause _ ) :: acc',
                 `Sym_lit (_meta, ((_loc, ("finally")) as finally_tk))
                 :: finally_exprs ->
                 let tk = token env finally_tk in
                 At_finally_clause
                   (tk, List_.map (map_form env) finally_exprs)
                 :: acc
               (* At this point we know that we don't have a catch or finally
                * in the form head. *)
               | Try_exprs (tk, try_expr_list) :: acc', _ ->
                 let expr = map_form env form in
                 Try_exprs (tk, expr :: try_expr_list) :: acc'
               | _ -> raise_parse_error "Invalid try-catch-finally form."
             end
           | _ -> raise_parse_error "Invalid try-catch-finally form."
        )
        [Try_exprs (token env try_tk, [])]
        rest_forms
    in
    let state = List.rev state
    in
    begin match state with
      (* Try *)
      | [ Try_exprs (try_tk, try_exprs) ] ->
        G.Try (try_tk,
               exprblock (List.rev try_exprs) |> expr_to_stmt,
               [],
               None,
               None)
        |> G.s |> stmt_to_expr
      (* Try-Finally *)
      | [ Try_exprs (try_tk, try_exprs);
            At_finally_clause (finally_tk, finally_exprs) ] ->
            G.Try (try_tk,
                 exprblock (List.rev try_exprs) |> expr_to_stmt,
                 [],
                 None,
                 Some (finally_tk,
                         exprblock (List.rev finally_exprs ) |> expr_to_stmt))
            |> G.s |> stmt_to_expr
      (* Try-Catch *)
      | [ Try_exprs (try_tk, try_exprs);
          At_catch_clause catch_clauses ] ->
        let catch_clauses =
          List.rev_map
            (fun (tk, catch_kind, exprs) ->
               (tk,
                catch_kind,
                exprblock (List.rev exprs) |> expr_to_stmt))
            catch_clauses
        in
        G.Try (try_tk,
               exprblock (List.rev try_exprs) |> expr_to_stmt,
               catch_clauses,
               None,
               None)
        |> G.s |> stmt_to_expr
      (* Try-Catch-Finally *)
      | [ Try_exprs (try_tk, try_exprs);
          At_catch_clause catch_clauses;
          At_finally_clause (finally_tk, finally_exprs) ] ->
        let catch_clauses =
          List.rev_map
            (fun (tk, catch_kind, exprs) ->
               (tk,
                catch_kind,
                exprblock (List.rev exprs) |> expr_to_stmt))
            catch_clauses
        in
        G.Try (try_tk,
               exprblock (List.rev try_exprs) |> expr_to_stmt,
               catch_clauses,
               None,
               Some (finally_tk,
                     exprblock (List.rev finally_exprs) |> expr_to_stmt))
        |> G.s |> stmt_to_expr
      | _ -> raise_parse_error "Invalid try-catch-finally form."
    end
  | _ -> raise_parse_error "Invalid try-catch-finally form."

and map_bare_set_form (env : env) ((hash_sym, lb, src, rb) : CST.bare_set_lit) =
  let hash_tk = (* "#" *) token env hash_sym in
  let lb_tk = (* "{" *) token env lb in
  let rb_tk = (* "}" *) token env rb in
  let forms = forms_in_source env src in
  let exprs = List_.map (map_form env) forms in
  G.Container (G.Set, (lb_tk, exprs, rb_tk)) |> G.e

and map_set_form (env : env) ((meta, bare_set) : CST.set_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) meta) in
  map_bare_set_form env bare_set

and map_bare_vec_form (env : env) ((lb, src, rb) : CST.bare_vec_lit) =
  let lb_tk = (* "[" *) token env lb in
  let rb_tk = (* "]" *) token env rb in
  let forms = forms_in_source env src in
  let exprs = List_.map (map_form env) forms in
  G.Container (G.Array, (lb_tk, exprs, rb_tk)) |> G.e

and map_vec_form (env : env) ((v1, v2) : CST.vec_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  map_bare_vec_form env v2

and map_bare_map_form (env : env) ((lb, src, rb) : CST.bare_map_lit) =
  let lb_tk = (* "{" *) token env lb in
  let rb_tk = (* "}" *) token env rb in
  let kv_pairs = forms_in_source env src in
  let rec aux acc = function
    | [] -> List.rev acc
    | `Sym_lit (_, ((_loc, "...") as ellipsis_tk)) :: rest ->
      let ellipsis_expr = G.Ellipsis (token env ellipsis_tk) |> G.e in
      aux (ellipsis_expr :: acc) rest
    | key_form :: val_form :: rest ->
      let k_expr = map_form env key_form in
      let v_expr = map_form env val_form in
      let kv_tuple =
        G.keyval k_expr (G.fake "") v_expr
      in
      aux (kv_tuple :: acc) rest
    | _ -> raise_parse_error "Invalid map kv pairs."
  in
  G.Container (G.Dict, (lb_tk, aux [] kv_pairs, rb_tk)) |> G.e

and map_map_form (env : env) ((v1, v2) : CST.map_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  map_bare_map_form env v2

and map_sym_lit (env : env) ((v1, v2) : CST.sym_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = sym_to_name_or_ellipsis_expr env v2 in
  v2

and map_sym_lit_pat (env : env) ((v1, v2) : CST.sym_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = simple_ident_or_ellipsis_pat env v2 in
  v2

and map_sym_lit_pat_qualified (env : env) ((v1, v2) : CST.sym_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = ident_or_ellipsis_pat env v2 in
  v2

and map_sym_val_lit (env : env) ((v1, v2) : CST.sym_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let _, s = v2 in
  match s with
  | ( "Inf" | "-Inf" | "NaN" ) ->
    L (G.Float (float_of_string_opt s, token env v2)) |> G.e 
  | _ ->
    (* TODO: Use OtherExpr... who cares if it makes sense? *)
    raise_parse_error
      "Invalid syntax: ##<symbol> only supports ##Inf, ##-Inf, ##NaN."

and map_name (env : env) ((v1, v2) : CST.sym_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = name env v2 in
  v2

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-clojure/.../Boilerplate.ml *)

 and map_anon_choice_read_cond_lit_137feb9 (env : env)
    (x : CST.anon_choice_read_cond_lit_137feb9) =
  match x with
  | `Read_cond_lit x -> R.Case ("Read_cond_lit", map_read_cond_lit env x)
  | `Map_lit x -> R.Case ("Map_lit", map_map_lit env x)
  | `Str_lit tok -> R.Case ("Str_lit", (* str_lit *) raw_token env tok)
  | `Kwd_lit tok -> R.Case ("Kwd_lit", (* kwd_lit *) raw_token env tok)
  | `Sym_lit x -> R.Case ("Sym_lit", map_sym_lit env x |> raw_of_expr)

and map_bare_list_lit (env : env) ((v1, v2, v3) : CST.bare_list_lit) =
  let v1 = (* "(" *) raw_token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* ")" *) raw_token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_bare_map_lit (env : env) ((v1, v2, v3) : CST.bare_map_lit) =
  let v1 = (* "{" *) raw_token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "}" *) raw_token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_bare_set_lit (env : env) ((v1, v2, v3, v4) : CST.bare_set_lit) =
  let v1 = (* "#" *) raw_token env v1 in
  let v2 = (* "{" *) raw_token env v2 in
  let v3 = map_source env v3 in
  let v4 = (* "}" *) raw_token env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

and map_bare_vec_lit (env : env) ((v1, v2, v3) : CST.bare_vec_lit) =
  let v1 = (* "[" *) raw_token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "]" *) raw_token env v3 in
  R.Tuple [ v1; v2; v3 ]

(* whitespace/comment *)
and map_gap (_env : env) (x : CST.gap) : unit =
  match x with
  | `Ws _tok -> ()
  | `Comm _tok -> ()
  (* ?? *)
  | `Dis_expr (_v1, _v2, _v3formTODO) -> ()

(* (meta, (lp, list, rp)) *)
(* (meta, (lp, `[Form (`Kw_lit ":a"); ...], rp)) *)
and map_list_lit (env : env) ((v1, v2) : CST.list_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = map_bare_list_lit env v2 in
  v2

and map_vec_lit (env : env) ((v1, v2) : CST.vec_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = map_bare_vec_lit env v2 in
  v2

and map_map_lit (env : env) ((v1, v2) : CST.map_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = map_bare_map_lit env v2 in
  v2

and map_meta_lit (env : env) ((v1, v2, v3) : CST.meta_lit) =
  let v1 = (* "^" *) raw_token env v1 in
  let _v2 = List_.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  R.Tuple [ v1; v3 ]

and map_metadata_lit (env : env) ((v1, v2) : CST.metadata_lit) =
  let v1 =
    match v1 with
    | `Meta_lit x -> R.Case ("Meta_lit", map_meta_lit env x)
    | `Old_meta_lit x -> R.Case ("Old_meta_lit", map_old_meta_lit env x)
  in
  let _v2 =
    match v2 with
    | Some xs ->
        let _ = List_.map (map_gap env) xs in
        ()
    | None -> ()
  in
  R.Tuple [ v1 ]

and map_old_meta_lit (env : env) ((v1, v2, v3) : CST.old_meta_lit) =
  let v1 = (* "#^" *) raw_token env v1 in
  let _v2 = List_.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  R.Tuple [ v1; v3 ]

and map_read_cond_lit (env : env) ((v1, v2, v3, v4) : CST.read_cond_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = (* "#?" *) raw_token env v2 in
  let v3 = R.List (List_.map (raw_token env (* ws *)) v3) in
  let v4 = map_bare_list_lit env v4 in
  R.Tuple [ v2; v3; v4 ]

(*****************************************************************************)
(* Forms processing                                                          *)
(*****************************************************************************)

(* TODO: To detect (ns ...) we need to fold so that env.extra.ns
 * is modified... But map_form must return the env too! *)
and map_forms_in_source :
  type res.
  (env -> CST.form -> res) ->
  env ->
  CST.source ->
  res list
  =
  fun f env xs ->
    List_.filter_map
      (function
        | `Form x ->
          (try Some (f env x) with
           (* TODO: Do something with errors, collect? Log? *)
           | Parse_error pe -> failwith pe.msg)
        | `Gap _ -> None)
      xs

(* TODO: Work with CST.form list after filtering out gaps. *)
and fold_forms_in_source :
  type res.
  (env -> CST.form -> env * res) ->
  env ->
  CST.source ->
  env * res list
  =
  fun f env xs ->
  List.fold_left_map
    (fun env x ->
       match x with
       | `Form form ->
         let env', res = f env form in
         env', Some res
       | `Gap _ -> env, None)
    env
    xs
  |> fun (acc, res_opt_list) ->
  let res_list = List_.filter_map (fun x -> x) res_opt_list in
  acc, res_list

(* TODO: This should return a proper list, so we can use in the constructs.
 * But it depends on what we are looking for!
 * In fact this should not be used, instead specialised versions per construct
 * should be created. *)
(* NOTE: This is not used except in remaining raw forms. *)
and map_source (env : env) (xs : CST.source) =
  R.List
    (map_forms_in_source
       (fun env x ->
          let (*_env, *) x = map_form env x in
          R.Case ("Form", raw_of_expr x))
       env
       xs)

and forms_in_source (env : env) (xs : CST.source) =
  map_forms_in_source (fun _env x -> x) env xs

(* This is for top-level stuff, used below in [parse]. *)
and map_program (env : env) (xs : CST.source) : G.program =
  map_forms_in_source
    (fun env x ->
       let x = map_form env x in
       (* In reality stuff like `def`, `defn` etc will already be
        * statements. So these can return ExprStmt and se can unpack it.
        * Else we wrap into Stmt at this top level only. That is, a file
        * is a list of statements, but inside we work with expressions
        * (modulo If etc which exist as statements only). *)
       expr_to_stmt x)
    env
    xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_clojure.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file;
                  conv = H.line_col_to_pos file;
                  extra = {kind = Program; mode = Normal;
                           ns = None; ns_aliases = []} }
      in
      map_program env cst)

let ss_is_expr_stmt_seq stmts = 
  List.for_all
    (function
      | {s = G.ExprStmt _; _ } -> true
      | _ -> false)
    stmts

let with_expr_stmt_seq_to_block = function
  (* More than one statement and all statements are expressions. *)
  | ( _stmt :: _ :: _ as stmts ) when ss_is_expr_stmt_seq stmts ->
    let exprs =
      List_.map
        (function
          | {s = G.ExprStmt (e, _sc); _ } -> G.E e
          | _ -> assert false (* Because: ss_is_expr_stmt_seq stmts *))
        stmts
    in
    G.OtherExpr (("ExprBlock", (G.fake "expr_block")), exprs)
    |> G.e |> expr_to_stmt |> (fun e -> [e])
  | ss -> ss

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_clojure.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file;
                  conv = H.line_col_to_pos_pattern str;
                  (* But, the implied ns in a pattern, for example
                   * when the pattern contains syntax quoted code,
                   * will not match the implied ns in a file which
                   * may have an actual declaration (ns ...). So
                   * maybe in patterns the implied ns should be a
                   * metavar, perhaps anonymous, so it can be matched.
                   * *)
                  extra = {kind = Pattern; mode = Normal;
                           ns = None; ns_aliases = []} }
      in
      (* XXX: Need sequence of ExprStmt to one ExprStmt with ExprBlock inside.
       * See dots_stmts.{clj,sgrep} and metavar_equality_var.{clj,sgrep}.
       * Avoids having to introduce strange patterns with 'do' around
       * expressions. *)
      let e = map_program env cst |> with_expr_stmt_seq_to_block in
      Pr e)
