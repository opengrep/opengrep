(* Sjoerd Langkemper
 *
 * Copyright (c) 2021, 2024 Semgrep Inc.
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
open Fpath_.Operators
module CST = Tree_sitter_c_sharp.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Csharp parser using tree-sitter-lang/semgrep-charp and converting
 * directly to AST_generic.ml
 *
 * TODO:
 *  - lots of TODO in this file ... it parses correctly (no exn raised),
 *    but lots of constructs are not converted correctly to the generic AST
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* less: we should check we consume all constraints *)
let type_parameters_with_constraints (tparams : type_parameters option)
    constraints : type_parameters option =
  match tparams with
  (* TODO: possible to have constraints without tparams? *)
  | None -> None
  | Some (lt, tparams, gt) ->
      let tparams' =
        tparams
        |> List_.map (function
             (* we do not generate those semgrep constructs for now in
              * semgrep-java, we parse Java patterns in the Java pfff parser *)
             | TParamEllipsis _ -> raise Impossible
             | OtherTypeParam (x, anys) ->
                 (* TODO: add constraints *)
                 OtherTypeParam (x, anys)
             | TP tparam -> (
                 let with_constraints =
                   constraints
                   |> List.find_opt (fun (id, _xs) -> fst id = fst tparam.tp_id)
                 in
                 match with_constraints with
                 | Some (_id, xs) ->
                     let _more_constraintsTODO, more_bounds =
                       xs |> Either_.partition (fun x -> x)
                     in
                     TP
                       {
                         tparam with
                         tp_bounds = more_bounds @ tparam.tp_bounds;
                       }
                 | None -> TP tparam))
      in
      Some (lt, tparams', gt)

let var_def_stmt (attrs : attribute list)
    (decls : (entity * variable_definition) list) (sc : Tok.t) : G.stmt =
  let stmts =
    decls
    |> List_.map (fun (ent, def) ->
           let ent = { ent with attrs = ent.attrs @ attrs } in
           (ent, def))
    |> H2.add_semicolon_to_last_var_def_and_convert_to_stmts sc
  in
  G.stmt1 stmts

(* TODO? integrate in AST_generic at some point? or extend
 * AST_generic.comprehension to support also the Group/Into/Join/OrderBy?
 *)
type linq_query_part =
  (* comprehension/SQL like *)
  | From of tok * (type_ option * ident) * expr
  | Select of tok * expr
  | Where of tok * expr
  (* PL like *)
  | Let of tok * ident * expr
  | Into of tok * ident * linq_query_part list
  (* SQL like *)
  | Group of tok * expr * expr
  | Join of tok * (type_ option * ident) * expr * expr * expr * ident option
  | OrderBy of tok * (expr * direction) list

and direction = Ascending | Descending

let param_from_lambda_params lambda_params =
  match lambda_params with
  | [] -> failwith "empty lambda_params"
  | [ id ] ->
      Param
        {
          pname = Some id;
          ptype = None;
          pdefault = None;
          pattrs = [];
          pinfo = empty_id_info ();
        }
  | ids ->
      let tk =
        match ids with
        | (_, t) :: _ -> t
        | [] -> Tok.unsafe_fake_tok ""
      in
      let ids = List_.map (fun id -> PatId (id, empty_id_info ())) ids in
      ParamPattern (PatTuple (fb ids), implicit_param_classic tk)

(* create lambda lambda_params -> expr *)
let create_lambda lambda_params expr =
  let fparams =
    Tok.unsafe_fake_bracket [ param_from_lambda_params lambda_params ]
  in
  Lambda
    {
      fkind = (Arrow, fake "=>");
      fparams;
      frettype = None;
      fbody = FBExpr expr;
    }
  |> G.e

(* create lambda (lambda_params, ident) -> (lambda_params..., ident) *)
let create_join_result_lambda lambda_params ident =
  let p1 = param_from_lambda_params lambda_params in
  let p2 = Param (param_of_id ident) in
  let fparams = fb [ p1; p2 ] in
  let ids =
    lambda_params @ [ ident ]
    |> List_.map (fun id -> N (Id (id, empty_id_info ())) |> G.e)
  in
  let expr = G.Container (G.Tuple, fb ids) |> G.e in
  Lambda
    {
      fkind = (Arrow, fake "=>");
      fparams;
      frettype = None;
      fbody = FBExpr expr;
    }
  |> G.e

(* create a new lambda in the form
 * base_expr.funcname(lambda_params => expr)
 *)
let call_lambda base_expr funcname tok funcs =
  (* let funcs = exprs |> List.map (fun expr -> create_lambda lambda_params expr) in *)
  let args = funcs |> List_.map (fun func -> Arg func) in
  (* We use hidden:true because the funcname is a fake identifier
   * that actually does not occur in the target code.
   *)
  let idinfo = empty_id_info ~hidden:true () in
  let method_ =
    DotAccess (base_expr, tok, FN (Id ((funcname, tok), idinfo))) |> G.e
  in
  Call (method_, fb args) |> G.e

let rec call_orderby base_expr lambda_params tok orderings =
  match orderings with
  | [] -> base_expr
  | ht :: tl ->
      let expr, dir = ht in
      let funcname =
        match dir with
        | Ascending -> "OrderBy"
        | Descending -> "OrderByDescending"
      in
      let func = create_lambda lambda_params expr in
      let base_expr = call_lambda base_expr funcname tok [ func ] in
      call_orderby base_expr lambda_params tok tl

let rec linq_remainder_to_expr (query : linq_query_part list) (base_expr : expr)
    (lambda_params : ident list) =
  match query with
  | [] -> base_expr
  | ht :: tl -> (
      match ht with
      | Select (tok, expr) ->
          let func = create_lambda lambda_params expr in
          let base_expr = call_lambda base_expr "Select" tok [ func ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Where (tok, expr) ->
          let func = create_lambda lambda_params expr in
          let base_expr = call_lambda base_expr "Where" tok [ func ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Let (tok, ident, expr) ->
          (* base_expr.Select(lambda_params -> (lambda_params..., expr))
           * and add ident to lambda_params
           *)
          let ids =
            List_.map
              (fun id -> N (Id (id, empty_id_info ())) |> G.e)
              lambda_params
          in
          let expr = Container (Tuple, fb (ids @ [ expr ])) |> G.e in
          let func = create_lambda lambda_params expr in
          let base_expr = call_lambda base_expr "Select" tok [ func ] in
          let lambda_params = lambda_params @ [ ident ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Group (tok, vals, key) ->
          (* base_expr.GroupBy(lambda_params -> key, lambda_params -> vals)
           * and clear lambda_params
           *)
          let key_func = create_lambda lambda_params key in
          let val_func = create_lambda lambda_params vals in
          let base_expr =
            call_lambda base_expr "GroupBy" tok [ key_func; val_func ]
          in
          linq_remainder_to_expr tl base_expr []
      | Into (_tok, ident, remainder) ->
          (* TODO can we throw away tl? *)
          linq_remainder_to_expr remainder base_expr [ ident ]
      | From (tok, (_type, ident), expr) ->
          (* base_expr.SelectMany(lambda_params -> expr, (lambda_params, col) -> (lambda_params, col))
           * and add ident to lambda_params
           *)
          let sel_func = create_lambda lambda_params expr in
          let res_func = create_join_result_lambda lambda_params ident in
          let base_expr =
            call_lambda base_expr "SelectMany" tok [ sel_func; res_func ]
          in
          let lambda_params = lambda_params @ [ ident ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | OrderBy (tok, orderings) ->
          let base_expr = call_orderby base_expr lambda_params tok orderings in
          linq_remainder_to_expr tl base_expr lambda_params
      | Join (tok, (_type, ident), enum, left, right, None) ->
          (* base_expr.Join(enum, lambda_params -> left, ident -> right, (lambda_params, ident) -> (lambda_params, ident))
           * and add ident to lambda_params
           *)
          let left_func = create_lambda lambda_params left in
          let right_func = create_lambda [ ident ] right in
          let res_func = create_join_result_lambda lambda_params ident in
          let base_expr =
            call_lambda base_expr "Join" tok
              [ enum; left_func; right_func; res_func ]
          in
          let lambda_params = lambda_params @ [ ident ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Join (tok, (_type, ident), enum, left, right, Some into) ->
          (* base_expr.GroupJoin(enum, lambda_params -> left, ident -> right, (lambda_params, into) -> (lambda_params, into))
                * and add into to lambda_params
          *)
          let left_func = create_lambda lambda_params left in
          let right_func = create_lambda [ ident ] right in
          let res_func = create_join_result_lambda lambda_params into in
          let base_expr =
            call_lambda base_expr "GroupJoin" tok
              [ enum; left_func; right_func; res_func ]
          in
          let lambda_params = lambda_params @ [ into ] in
          linq_remainder_to_expr tl base_expr lambda_params)

let linq_to_expr (from : linq_query_part) (body : linq_query_part list) =
  match from with
  | From (_, (_type, id), collection) ->
      linq_remainder_to_expr body collection [ id ]
  | _ -> raise Impossible

let new_index_from_end tok expr =
  (* `arr[^N]` is desugared to `new System.Index(N, true)`. Neither
     `System` nor `Index` appears in the source `^N` syntax, so mark
     the synthesised qualified name hidden so the prefilter regex
     doesn't demand them. *)
  let name =
    H2.name_of_ids ~hidden:true
      [ ("System", fake "System"); ("Index", fake "Index") ]
  in
  let index = TyN name |> G.t in
  New
    ( tok,
      index,
      empty_id_info (),
      fb [ Arg expr; Arg (L (Bool (true, fake "true")) |> G.e) ] )
  |> G.e

module List = struct
  include List

  (* not available in 4.09 *)
  let concat_map f xs = List_.map f xs |> List_.flatten
end

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-java/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the csharp CST
   to another type of tree.
*)


let opt_semi (env : env) (v1 : CST.optional_semi) =
  (* The external scanner emits optional_semi either empty or as a
     semicolon, so the token's text tells which. *)
  match snd v1 with
  | ";" -> Some (token env v1)
  | __else__ -> None

let map_anon_choice_static_d4628a0 (env : env)
    (x : CST.anon_choice_static_d4628a0) =
  match x with
  | `Async tok -> KeywordAttr (Async, token env tok)
  | `Static tok -> KeywordAttr (Static, token env tok)

let escape_sequence (env : env) (tok : CST.escape_sequence) =
  let s = str env tok in
  (* escape_sequence *)
  String (fb s)

let assignment_operator (env : env)
    (x :
      [ `EQ of Tree_sitter_run.Token.t
      | `PLUSEQ of Tree_sitter_run.Token.t
      | `DASHEQ of Tree_sitter_run.Token.t
      | `STAREQ of Tree_sitter_run.Token.t
      | `SLASHEQ of Tree_sitter_run.Token.t
      | `PERCEQ of Tree_sitter_run.Token.t
      | `AMPEQ of Tree_sitter_run.Token.t
      | `HATEQ of Tree_sitter_run.Token.t
      | `BAREQ of Tree_sitter_run.Token.t
      | `LTLTEQ of Tree_sitter_run.Token.t
      | `GTGTEQ of Tree_sitter_run.Token.t
      | `GTGTGTEQ of Tree_sitter_run.Token.t
      | `QMARKQMARKEQ of Tree_sitter_run.Token.t ]) : operator wrap =
  match x with
  | `EQ tok -> (Eq, token env tok) (* "=" *)
  | `PLUSEQ tok -> (Plus, token env tok) (* "+=" *)
  | `DASHEQ tok -> (Minus, token env tok) (* "-=" *)
  | `STAREQ tok -> (Mult, token env tok) (* "*=" *)
  | `SLASHEQ tok -> (Div, token env tok) (* "/=" *)
  | `PERCEQ tok -> (Mod, token env tok) (* "%=" *)
  | `AMPEQ tok -> (BitAnd, token env tok) (* "&=" *)
  | `HATEQ tok -> (BitXor, token env tok) (* "^=" *)
  | `BAREQ tok -> (BitOr, token env tok) (* "|=" *)
  (* https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/bitwise-and-shift-operators#right-shift-operator-
     https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/bitwise-and-shift-operators#unsigned-right-shift-operator-
  *)
  | `LTLTEQ tok -> (LSL, token env tok) (* "<<=" *)
  | `GTGTEQ tok -> (ASR, token env tok) (* ">>=" *)
  | `GTGTGTEQ tok -> (LSR, token env tok) (* ">>>=" *)
  | `QMARKQMARKEQ tok -> (Nullish, token env tok)

(* "??=" *)

let boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok) (* "true" *)
  | `False tok -> Bool (false, token env tok)

(* "false" *)

let predefined_type (env : env) (tok : CST.predefined_type) =
  G.ty_builtin (str env tok)
(*
let verbatim_string_literal (env : env) (tok : CST.verbatim_string_literal) =
  G.String (fb (str env tok))
*)
(* verbatim_string_literal *)

let attribute_target_specifier (env : env)
    ((v1, v2) : CST.attribute_target_specifier) =
  let _v2 = token env v2 (* ":" *) in
  match v1 with
  | `Field tok -> token env tok (* "field" *)
  | `Event tok -> token env tok (* "event" *)
  | `Meth tok -> token env tok (* "method" *)
  | `Param tok -> token env tok (* "param" *)
  | `Prop tok -> token env tok (* "property" *)
  | `Ret tok -> token env tok (* "return" *)
  | `Type_599dcce tok -> token env tok (* "type" *)
  | `Type_2a87ba5 tok -> token env tok (* "typevar" *)

(* note that there's no octal literal in C# so no need for
 * H.int_of_string_c_octal_opt
 *)
let integer_literal (env : env) (tok : CST.integer_literal) =
  let s, t = str env tok in
  (* integer_literal *)
  G.Int (Parsed_int.parse (s, t))

let overloadable_operator (env : env)
    (x :
      [ `BANG of Tree_sitter_run.Token.t
      | `TILDE of Tree_sitter_run.Token.t
      | `PLUSPLUS of Tree_sitter_run.Token.t
      | `DASHDASH of Tree_sitter_run.Token.t
      | `True of Tree_sitter_run.Token.t
      | `False of Tree_sitter_run.Token.t
      | `PLUS of Tree_sitter_run.Token.t
      | `DASH of Tree_sitter_run.Token.t
      | `STAR of Tree_sitter_run.Token.t
      | `SLASH of Tree_sitter_run.Token.t
      | `PERC of Tree_sitter_run.Token.t
      | `HAT of Tree_sitter_run.Token.t
      | `BAR of Tree_sitter_run.Token.t
      | `AMP of Tree_sitter_run.Token.t
      | `LTLT of Tree_sitter_run.Token.t
      | `GTGT of Tree_sitter_run.Token.t
      | `GTGTGT of Tree_sitter_run.Token.t
      | `EQEQ of Tree_sitter_run.Token.t
      | `BANGEQ of Tree_sitter_run.Token.t
      | `GT of Tree_sitter_run.Token.t
      | `LT of Tree_sitter_run.Token.t
      | `GTEQ of Tree_sitter_run.Token.t
      | `LTEQ of Tree_sitter_run.Token.t
      | `PLUSEQ of Tree_sitter_run.Token.t
      | `DASHEQ of Tree_sitter_run.Token.t
      | `STAREQ of Tree_sitter_run.Token.t
      | `SLASHEQ of Tree_sitter_run.Token.t
      | `PERCEQ of Tree_sitter_run.Token.t
      | `HATEQ of Tree_sitter_run.Token.t
      | `BAREQ of Tree_sitter_run.Token.t
      | `AMPEQ of Tree_sitter_run.Token.t
      | `LTLTEQ of Tree_sitter_run.Token.t
      | `GTGTEQ of Tree_sitter_run.Token.t
      | `GTGTGTEQ of Tree_sitter_run.Token.t ]) =
  match x with
  | `BANG tok -> str env tok (* "!" *)
  | `TILDE tok -> str env tok (* "~" *)
  | `PLUSPLUS tok -> str env tok (* "++" *)
  | `DASHDASH tok -> str env tok (* "--" *)
  | `True tok -> str env tok (* "true" *)
  | `False tok -> str env tok (* "false" *)
  | `PLUS tok -> str env tok (* "+" *)
  | `DASH tok -> str env tok (* "-" *)
  | `STAR tok -> str env tok (* "*" *)
  | `SLASH tok -> str env tok (* "/" *)
  | `PERC tok -> str env tok (* "%" *)
  | `HAT tok -> str env tok (* "^" *)
  | `BAR tok -> str env tok (* "|" *)
  | `AMP tok -> str env tok (* "&" *)
  | `LTLT tok -> str env tok (* "<<" *)
  | `GTGT tok -> str env tok (* ">>" *)
  | `GTGTGT tok -> str env tok (* ">>>" *)
  | `EQEQ tok -> str env tok (* "==" *)
  | `BANGEQ tok -> str env tok (* "!=" *)
  | `GT tok -> str env tok (* ">" *)
  | `LT tok -> str env tok (* "<" *)
  | `GTEQ tok -> str env tok (* ">=" *)
  | `LTEQ tok -> str env tok (* "<=" *)
  (* C# 14 compound assignment operators *)
  | `PLUSEQ tok
  | `DASHEQ tok
  | `STAREQ tok
  | `SLASHEQ tok
  | `PERCEQ tok
  | `HATEQ tok
  | `BAREQ tok
  | `AMPEQ tok
  | `LTLTEQ tok
  | `GTGTEQ tok
  | `GTGTGTEQ tok ->
      str env tok

let modifier (env : env) (x : CST.modifier) =
  (* TODO these should all be KeywordAttr, but pfff doesn't know about all keywords *)
  match x with
  | `Abst tok -> KeywordAttr (Abstract, token env tok) (* "abstract" *)
  | `Async tok -> KeywordAttr (Async, token env tok) (* "async" *)
  | `Const tok -> KeywordAttr (Const, token env tok) (* "const" *)
  | `Extern tok -> KeywordAttr (Extern, token env tok) (* "extern" *)
  | `Fixed tok -> unhandled_keywordattr (str env tok)
  | `Inte tok -> unhandled_keywordattr (str env tok)
  | `New tok -> unhandled_keywordattr (str env tok)
  | `Over tok -> unhandled_keywordattr (str env tok)
  | `Part tok -> unhandled_keywordattr (str env tok)
  | `Priv tok -> KeywordAttr (Private, token env tok) (* "private" *)
  | `Prot tok -> KeywordAttr (Protected, token env tok) (* "protected" *)
  | `Public tok -> KeywordAttr (Public, token env tok) (* "public" *)
  | `Read tok -> KeywordAttr (Const, token env tok) (* "readonly" *)
  | `Requ tok -> unhandled_keywordattr (str env tok) (* "required" *)
  | `File tok -> unhandled_keywordattr (str env tok) (* "file" *)
  | `Sealed tok ->
      (* TODO we map Sealed to Final here, is that OK? *)
      KeywordAttr (Final, token env tok)
      (* "sealed" *)
  | `Static tok -> KeywordAttr (Static, token env tok) (* "static" *)
  | `Unsafe tok -> unhandled_keywordattr (str env tok)
  | `Virt tok -> unhandled_keywordattr (str env tok)
  | `Vola tok -> KeywordAttr (Volatile, token env tok)

(* "volatile" *)

let interpolation_format_clause (env : env)
    ((v1, v2) : CST.interpolation_format_clause) =
  let _v1 = token env v1 (* ":" *) in
  let v2 = token env v2 (* pattern "[^}\"]+" *) in
  v2

(* The text between interpolations, as the external scanner delimits it, in
 * a regular, verbatim or raw interpolated string. *)
let interpolation_string_content (env : env)
    (tok : CST.interpolation_string_content) =
  String (fb (str env tok))

let real_literal (env : env) (tok : CST.real_literal) =
  let s, t = str env tok (* real_literal *) in
  G.Float (float_of_string_opt s, t)

let reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Alias x (* "alias" *)
  | `Asce x (* "ascending" *)
  | `By x (* "by" *)
  | `Desc x (* "descending" *)
  | `Equals x (* "equals" *)
  | `Field x (* "field" *)
  | `File x (* "file" *)
  | `From x (* "from" *)
  | `Global x (* "global" *)
  | `Group x (* "group" *)
  | `Into x (* "into" *)
  | `Join x (* "join" *)
  | `Let x (* "let" *)
  | `Meth x (* "method" *)
  | `Notn x (* "notnull" *)
  | `On x (* "on" *)
  | `Orde x (* "orderby" *)
  | `Param x (* "param" *)
  | `Prop x (* "property" *)
  | `Scoped x (* "scoped" *)
  | `Select x (* "select" *)
  | `Type_599dcce x (* "type" *)
  | `Type_2a87ba5 x (* "typevar" *)
  | `Unma x (* "unmanaged" *)
  | `Var x (* "var" *)
  | `When x (* "when" *)
  | `Where x (* "where" *)
  | `Yield x (* "yield" *) ->
      str env x

let identifier (env : env) (tok : CST.identifier) : ident =
  match tok with
  | `Choice_id_tok (`Id_tok tok) -> str env tok
  | `Choice_id_tok (`Rese_id kw) -> reserved_identifier env kw
  | `Semg_meta tok -> str env tok

let rec variable_designation (env : env) (x : CST.variable_designation) =
  match x with
  | `Disc tok -> PatWildcard (token env tok) (* "_" *)
  | `Paren_var_desi (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = variable_designation env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = variable_designation env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 = token env v3 (* ")" *) in
      PatTuple (v1, v2, v3)
  | `Id tok -> PatId (identifier env tok, empty_id_info ())

(* identifier *)

let join_into_clause (env : env) ((v1, v2) : CST.join_into_clause) =
  let _v1 = token env v1 (* "into" *) in
  let v2 = identifier env v2 (* identifier *) in
  v2

let rec tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) =
  let v1 = token env v1 (* "(" *) in
  let v2 = anon_choice_id_c036834 env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = anon_choice_id_c036834 env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  PatTuple (v1, v2 :: v3, v4)

and anon_choice_id_c036834 (env : env) (x : CST.anon_choice_id_c036834) =
  match x with
  | `Id tok ->
      let id = identifier env tok (* identifier *) in
      PatId (id, empty_id_info ())
  | `Disc tok ->
      let tok = token env tok (* "_" *) in
      PatWildcard tok
  | `Tuple_pat x -> tuple_pattern env x

(* A verbatim string literal is one token, `@"..."` with an optional
 * encoding suffix. It is split into its opening quote, its content and its
 * closing quote, which converts it like a regular string literal. The
 * closing quote and the suffix are on the token's last line, so the closing
 * quote is located from the token's end. *)
let verbatim_string_parts ((loc, s) : Tree_sitter_run.Token.t) :
    Tree_sitter_run.Token.t
    * Tree_sitter_run.Token.t option
    * Tree_sitter_run.Token.t =
  let shift (p : Tree_sitter_run.Loc.pos) (n : int) : Tree_sitter_run.Loc.pos =
    { p with column = p.column + n }
  in
  let start = loc.Tree_sitter_run.Loc.start in
  let open_len = 2 (* "@\"" *) in
  let close_index = String.rindex s '"' in
  let close_start = shift loc.Tree_sitter_run.Loc.end_ (close_index - String.length s) in
  let lquote =
    ({ Tree_sitter_run.Loc.start; end_ = shift start open_len }, "@\"")
  in
  let rquote =
    ( { Tree_sitter_run.Loc.start = close_start; end_ = shift close_start 1 },
      "\"" )
  in
  let content =
    match String.sub s open_len (close_index - open_len) with
    | "" -> None
    | content ->
        Some
          ( { Tree_sitter_run.Loc.start = shift start open_len; end_ = close_start },
            content )
  in
  (lquote, content, rquote)

let literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Null_lit tok -> G.Null (token env tok) (* "null" *)
  | `Bool_lit x -> boolean_literal env x
  | `Char_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let s, t =
        match v2 with
        | `Char_lit_content tok -> str env tok (* pattern "[^'\\\\]" *)
        | `Esc_seq tok -> str env tok
        (* escape_sequence *)
      in
      let v3 = token env v3 (* "'" *) in
      Char (s, Tok.combine_toks v1 [ t; v3 ])
  | `Real_lit tok -> real_literal env tok (* real_literal *)
  | `Int_lit tok -> integer_literal env tok (* integer_literal *)
  | `Str_lit (v1, v2, v3, _v4TODO) ->
      (* v4 is the "string literal encoding" *)
      let l = token env v1 (* "\"" *) in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_lit_content tok -> str env tok (* pattern "[^\"\\\\\\n]+" *)
            | `Esc_seq tok -> str env tok (* escape_sequence *))
          v2
      in
      let r = token env v3 in
      G.String (G.string_ (l, xs, r))
  | `Verb_str_lit tok ->
      let l, content, r = verbatim_string_parts tok in
      let xs = content |> Option.to_list |> List_.map (str env) in
      G.String (G.string_ (token env l, xs, token env r))
  | `Raw_str_lit (v1, v2, v3, _v4TODO) ->
      (* v4 is the "string literal encoding" *)
      let l = token env v1 (* "\"\"\""\"" *) in
      let xs =
        match v2 with
        | _, "" -> []
        | content -> [ str env content ]
      in
      let r = token env v3 in
      G.String (G.string_ (l, xs, r))

(* "void" *)
let rec type_pattern (env : env) (x : CST.type_pattern) = type_ env x

and variable_declaration (env : env) ((v1, v2, v3) : CST.variable_declaration) :
    (entity * variable_definition) list =
  let v1 = local_variable_type env v1 in
  let v2 = variable_declarator env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = variable_declarator env v2 in
        v2)
      v3
  in
  let decls = v2 :: v3 in
  List_.map
    (fun (ent, vardef) ->
      (ent, { vinit = vardef.vinit; vtype = v1; vtok = G.no_sc }))
    decls

and interpolation_alignment_clause (env : env)
    ((v1, v2) : CST.interpolation_alignment_clause) =
  let _v1 = token env v1 (* "," *) in
  let v2 = expression env v2 in
  v2

and adjust_range_of_parenthesized_expr (env : env)
    (l : Tree_sitter_run.Token.t) (r : Tree_sitter_run.Token.t) (e : G.expr) : G.expr =
  let lloc = Tok.unsafe_loc_of_tok (token env l) in
  let rloc = Tok.unsafe_loc_of_tok (token env r) in
  e.e_range <- Some (lloc, rloc);
  e

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  non_lvalue_expression env v2
  |> adjust_range_of_parenthesized_expr env v1 v3

and postfix_unary_expression (env : env)
    ((v1, v2) : CST.postfix_unary_expression) =
  let v1 = expression env v1 in
  match v2 with
  | `PLUSPLUS tok ->
      let v2 = token env tok (* "++" *) in
      Call (IdSpecial (IncrDecr (Incr, Postfix), v2) |> G.e, fb [ Arg v1 ])
      |> G.e
  | `DASHDASH tok ->
      let v2 = token env tok (* "--" *) in
      Call (IdSpecial (IncrDecr (Decr, Postfix), v2) |> G.e, fb [ Arg v1 ])
      |> G.e
  | `BANG tok ->
      let v2 = token env tok (* "!" *) in
      Call (IdSpecial (Op NotNullPostfix, v2) |> G.e, fb [ Arg v1 ]) |> G.e

and when_clause (env : env) ((v1, v2) : CST.when_clause) =
  let _v1 = token env v1 (* "when" *) in
  let v2 = expression env v2 in
  v2

(* Each `into` continuation of a query body nests the rest of the query. *)
and query_continuations (env : env)
    (xs :
      (Tree_sitter_run.Token.t
      * CST.identifier
      * CST.query_clause list
      * CST.select_or_group_clause)
      list) : linq_query_part list =
  match xs with
  | [] -> []
  | (v1, v2, v3, v4) :: rest ->
      let v1 = token env v1 (* "into" *) in
      let v2 = identifier env v2 (* identifier *) in
      let v3 = List_.map (query_clause env) v3 in
      let v4 = select_or_group_clause env v4 in
      [ Into (v1, v2, v3 @ [ v4 ] @ query_continuations env rest) ]

(* A relational pattern such as `< 5` only provides the RHS operand; the
 * subject being matched (e.g. `x` in `x is < 5`) is left implicit here, the
 * same way Kotlin's `when` conditions elide their subject. We record the
 * operator (as the todo_kind string) and the operand expression only.
 * [AST_to_IL.pattern_match_cond] reconstructs the real comparison
 * `scrutinee <op> rhs` against the already-materialised scrutinee, so the
 * scrutinee is neither shared in the AST nor evaluated twice in the IL. *)
and relational_pattern (env : env) (x : CST.relational_pattern) =
  let mk op_str tok_v e_v =
    let optok = token env tok_v in
    let operand = expression env e_v in
    G.OtherPat ((op_str, optok), [ G.E operand ])
  in
  match x with
  | `LT_exp (v1, v2) -> mk "<" v1 v2
  | `LTEQ_exp (v1, v2) -> mk "<=" v1 v2
  | `GT_exp (v1, v2) -> mk ">" v1 v2
  | `GTEQ_exp (v1, v2) -> mk ">=" v1 v2

and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op And, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Or, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op ASR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSL, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitAnd, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitXor, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitOr, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Plus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Minus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mult, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Div, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mod, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Lt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Eq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op GtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Gt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Nullish, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e

and block (env : env) ((v1, v2, v3) : CST.block) : stmt =
  let v1 = token env v1 (* "{" *) in
  let v2 = List_.map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  G.Block (v1, v2, v3) |> G.s

and variable_declarator (env : env) ((v1, v2, v3) : CST.variable_declarator) =
  let v1, pattern =
    match v1 with
    | `Id tok -> (identifier env tok, None) (* identifier *)
    | `Tuple_pat x ->
        let tok, _, _, _ = x in
        let id = (G.special_multivardef_pattern, token env tok) in
        let pat = Some (tuple_pattern env x) in
        (id, pat)
  in
  let _v2TODO = Option.map (element_binding_expression env) v2 in
  let v3 = Option.map (equals_value_clause env) v3 in
  let vinit =
    match (pattern, v3) with
    | Some pat, Some init -> Some (LetPattern (pat, init) |> G.e)
    | _ -> v3
  in
  let ent = basic_entity v1 in
  let vardef = { vinit; vtype = None; vtok = G.no_sc } in
  (ent, vardef)

and with_body (env : env) ((v1, v2, v3, v4) : CST.with_body) :
    G.field list G.bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = with_initializer env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = with_initializer env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = Option.map (token env) v3 (* "," *) in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and prefix_unary_expression (env : env)
    ((v1, v2) : CST.prefix_unary_expression) =
  let v2 = expression env v2 in
  match v1 with
  | `BANG tok ->
      let v1 = token env tok (* "!" *) in
      Call (IdSpecial (Op Not, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `PLUS tok ->
      let v1 = token env tok (* "+" *) in
      Call (IdSpecial (Op Plus, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `PLUSPLUS tok ->
      let v1 = token env tok (* "++" *) in
      Call (IdSpecial (IncrDecr (Incr, Prefix), v1) |> G.e, fb [ Arg v2 ])
      |> G.e
  | `DASH tok ->
      let v1 = token env tok (* "-" *) in
      Call (IdSpecial (Op Minus, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `DASHDASH tok ->
      let v1 = token env tok (* "--" *) in
      Call (IdSpecial (IncrDecr (Decr, Prefix), v1) |> G.e, fb [ Arg v2 ])
      |> G.e
  | `HAT tok ->
      let v1 = token env tok (* "^" *) in
      new_index_from_end v1 v2
  | `TILDE tok ->
      let v1 = token env tok (* "~" *) in
      Call (IdSpecial (Op BitNot, v1) |> G.e, fb [ Arg v2 ]) |> G.e

and name (env : env) (x : CST.name) : G.name =
  match x with
  | `Alias_qual_name (v1, v2, v3) ->
      (* old: was QTop, but simpler to just return an ident for H2.name_of_ids *)
      let v1 = identifier env v1 in
      let _v2 = token env v2 (* "::" *) in
      let v3 = simple_name env v3 in
      H2.name_of_ids_with_opt_typeargs [ (v1, None); v3 ]
  | `Qual_name (v1, v2, v3) ->
      let v1 = name env v1 in
      let _v2 = token env v2 (* "." *) in
      let v3 = simple_name env v3 in
      H2.add_id_opt_type_args_to_name v1 v3
  | `Simple_name x -> H2.name_of_ids_with_opt_typeargs [ simple_name env x ]

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) :
    G.type_parameter =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 =
    match v2 with
    | Some (`In tok) -> Some (Contravariant, token env tok) (* "in" *)
    | Some (`Out tok) -> Some (Covariant, token env tok) (* "out" *)
    | None -> None
  in
  let v3 = identifier env v3 (* identifier *) in
  G.tparam_of_id v3 ~tp_attrs:v1 ?tp_variance:v2

and element_binding_expression (env : env) (x : CST.bracketed_argument_list) =
  let open_br, args, close_br = bracketed_argument_list env x in
  let exprs = List_.map H2.argument_to_expr args in
  (open_br, exprs, close_br)

and nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let t = nullable_base_type env v1 in
  let tquestion = (* "?" *) token env v2 in
  TyQuestion (t, tquestion) |> G.t

and nullable_base_type (env : env) (x : CST.nullable_base_type) : G.type_ =
  match x with
  | `Array_type x -> array_type env x
  | `Name x ->
      let n = name env x in
      TyN n |> G.t
  (*
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  *)
  | `Pred_type tok -> predefined_type env tok
  | `Tuple_type x -> tuple_type env x

and array_type (env : env) ((v1, v2) : CST.array_type) =
  let v1 = array_base_type env v1 in
  let v2 = array_rank_specifier env v2 in
  let open_br, exps, close_br = v2 in
  let rec jag exps t =
    match exps with
    | [] -> TyArray ((open_br, None, close_br), t) |> G.t
    | [ e ] -> TyArray ((open_br, e, close_br), t) |> G.t
    | e :: tl -> jag tl (TyArray ((open_br, e, close_br), t) |> G.t)
  in
  (* TODO correct order? *)
  jag exps v1

and array_base_type (env : env) (x : CST.array_base_type) : G.type_ =
  match x with
  | `Array_type x -> array_type env x
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

and interpolated_verbatim_string_content (env : env)
    (x : CST.interpolated_verbatim_string_content) =
  match x with
  | `Interp_str_content tok -> L (interpolation_string_content env tok) |> G.e
  | `Interp x -> Tok.unbracket (interpolation env x)

and interpolated_raw_string_content (env : env)
    (x : CST.interpolated_raw_string_content) =
  match x with
  | `Interp_str_content tok -> L (interpolation_string_content env tok) |> G.e
  | `Interp x -> Tok.unbracket (interpolation env x)

and array_rank_specifier (env : env) ((v1, v2, v3) : CST.array_rank_specifier) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = Option.map (expression env) v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = Option.map (expression env) v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* "]" *) in
  (* TODO we could give each expression brackets, instead of using the same brackets for all expressions *)
  (v1, v2, v3)

and argument (env : env) (x : CST.argument) : G.argument =
  match x with
  | `Opt_id_COLON_opt_choice_ref_choice_exp (v1, v2, v3) -> (
      let v1 =
        Option.map
          (fun (v1, v2) ->
            let _v2 = token env v2 (* ":" *) in
            identifier env v1)
          v1
      in
      let _v2TODO =
        match v2 with
        | Some x -> (
            match x with
            | `Ref tok -> Some (token env tok) (* "ref" *)
            | `Out tok -> Some (token env tok) (* "out" *)
            | `In tok -> Some (token env tok) (* "in" *))
        | None -> None
      in
      let v3 =
        match v3 with
        | `Exp x -> expression env x
        | `Decl_exp x -> declaration_expression env x
      in
      match v1 with
      | None -> G.Arg v3
      | Some id -> G.ArgKwd (id, v3))
  | `Semg_vari_meta v1 ->
      let id = str env v1 in
      G.Arg (N (H2.name_of_id id) |> G.e)

and initializer_expression (env : env)
    ((v1, v2, v3, v4) : CST.initializer_expression) : expr list G.bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 = anon_opt_exp_elem_rep_interp_alig_clause_48c2617 env v2 in
  let _v3 = Option.map (token env) v3 (* "," *) in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and switch_expression_arm (env : env)
    ((v1, v2, v3, v4) : CST.switch_expression_arm) =
  let v1 = pattern env v1 in
  let v2 =
    match v2 with
    | Some x -> PatWhen (v1, when_clause env x)
    | None -> v1
  in
  let _v3 = token env v3 (* "=>" *) in
  let v4 = expression env v4 in
  G.case_of_pat_and_expr (v2, v4)

and tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = argument env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = argument env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  let exprs = List_.map H2.argument_to_expr (v2 :: v3) in
  Container (Tuple, (v1, exprs, v4)) |> G.e

and query_body (env : env) ((v1, v2, v3) : CST.query_body) =
  let v1 = List_.map (query_clause env) v1 in
  let v2 = select_or_group_clause env v2 in
  let v3 = query_continuations env v3 in
  v1 @ [ v2 ] @ v3

and catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let exn =
    List.fold_left
      (fun exn x ->
        match x with
        | `Catch_decl x -> catch_declaration env x
        | `Catch_filter_clause x ->
            let _filterTODO = catch_filter_clause env x in
            exn)
      (CatchPattern (PatWildcard (fake "_")))
      v2
  in
  let v3 = block env v3 in
  (v1, exn, v3)

and ordering (env : env) ((v1, v2) : CST.ordering) =
  let v1 = expression env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Asce _tok -> Ascending (* "ascending" *)
        | `Desc _tok -> Descending (* "descending" *))
    | None -> Ascending
  in
  (v1, v2)

and interpolated_string_content (env : env)
    (x : CST.interpolated_string_content) =
  match x with
  | `Interp_str_content tok -> L (interpolation_string_content env tok) |> G.e
  | `Esc_seq tok -> L (escape_sequence env tok) |> G.e
  | `Interp x -> Tok.unbracket (interpolation env x)

and checked_expression (env : env)
    ((v1, v2, v3, v4) : CST.checked_expression) =
  let _v2 = token env v2 (* "(" *) in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* ")" *) in
  match v1 with
  | `Chec tok ->
      let v1 = token env tok (* "checked" *) in
      OtherExpr (("Checked", v1), [ E v3 ]) |> G.e
  | `Unch tok ->
      let v1 = token env tok (* "unchecked" *) in
      OtherExpr (("Unchecked", v1), [ E v3 ]) |> G.e

and pred_type env x =
  (* e.g. `int` in `int.maxValue` *)
  let id = str env x in
  N (Id (id, empty_id_info ())) |> G.e

and default_expression env (v1, v2) =
  let v1 = token env v1 (* "default" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = ArgType (type_pattern env v2) in
        let v3 = token env v3 (* ")" *) in
        (v1, [ v2 ], v3)
    | None -> fb []
  in
  (* old: was a New *)
  let e = G.OtherExpr (("Default", v1), []) |> G.e in
  Call (e, v2) |> G.e

and size_of_expression env (v1, v2, v3, v4) =
  let v1 = token env v1 (* "sizeof" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = type_pattern env v3 in
  let v4 = token env v4 (* ")" *) in
  Call (IdSpecial (Sizeof, v1) |> G.e, (v2, [ ArgType v3 ], v4)) |> G.e

and type_of_expression env (v1, v2, v3, v4) =
  let v1 = token env v1 (* "typeof" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = type_pattern env v3 in
  let v4 = token env v4 (* ")" *) in
  Call (IdSpecial (Typeof, v1) |> G.e, (v2, [ ArgType v3 ], v4)) |> G.e

and member_access_expression (env : env)
    ((v1, v2, v3) : CST.member_access_expression) =
  let v1 =
    match v1 with
    | `Exp x -> expression env x
    | `Pred_type x -> pred_type env x
    | `Name x -> N (name env x) |> G.e
  in
  match v2 with
  | `DOT tok (* "." *)
  | `DASHGT tok (* "->" *) ->
      let v2 = token env tok in
      let v3 = simple_name env v3 in
      let n = H2.name_of_ids_with_opt_typeargs [ v3 ] in
      G.DotAccess (v1, v2, G.FN n) |> G.e

(* `e?.m` and `e?[i]`: the receiver is wrapped in an Elvis call, which marks
 * the access as conditional on a non-null receiver. *)
and conditional_access_expression (env : env)
    ((v1, v2, v3) : CST.conditional_access_expression) =
  (* The receiver is a primary expression. *)
  let v1 =
    match v1 with
    | (`This _ | `Member_access_exp _ | `Simple_name _ | `Elem_access_exp _
      | `Tuple_exp _ | `Paren_lvalue_exp _ | `Cond_access_exp _ ) as x ->
        lvalue_expression env x
    | (`Invo_exp _ | `Post_un_exp _ | `Obj_crea_exp _ | `Paren_exp _) as x ->
        expression_statement_expression env x
    | ( `Base _ | `Lit _ | `Inte_str_exp _ | `Impl_obj_crea_exp _
      | `Anon_obj_crea_exp _ | `Array_crea_exp _ | `Impl_array_crea_exp _
      | `Typeof_exp _ | `Sizeof_exp _ | `Chec_exp _ | `Defa_exp _ ) as x ->
        non_lvalue_expression env x
  in
  let v2 = token env v2 (* "?" *) in
  let inner =
    G.Call (G.IdSpecial (G.Op G.Elvis, v2) |> G.e, fb [ G.Arg v1 ]) |> G.e
  in
  match v3 with
  | `Brac_arg_list x ->
      let x = element_binding_expression env x in
      let open_br, exprs, close_br = x in
      (* Mirror the unwrapping in [element_access_expression], see the
       * comment there. *)
      let index_expr =
        match exprs with
        | [ e ] -> e
        | _ -> Container (Tuple, x) |> G.e
      in
      ArrayAccess (inner, (open_br, index_expr, close_br)) |> G.e
  | `Member_bind_exp (x1, x2) ->
      let x1 = token env x1 (* "." *) in
      let x2 = simple_name env x2 in
      let n = H2.name_of_ids_with_opt_typeargs [ x2 ] in
      DotAccess (inner, x1, FN n) |> G.e

and element_access_expression (env : env)
    ((v1, v2) : CST.element_access_expression) =
  let v1 = expression env v1 in
  let open_br, exprs, close_br = element_binding_expression env v2 in
  (* TODO we map multidim arrays as jagged arrays when creating arrays, with as tuples here. Does that work? Should we map this as multiple nested ArrayAccess? *)
  (* Single-argument indexer [d[k]] must not be wrapped in a Tuple
   * container: downstream [offset_of_IL] matches [Index { e =
   * Literal (String _) }] to build field-sensitive [Ostr] offsets,
   * and a [Composite (CTuple, [String])] wrapper defeats that and
   * collapses every key into [Oany]. Multi-argument indexers (rare)
   * keep the Tuple. *)
  let index_expr =
    match exprs with
    | [ e ] -> e
    | _ -> Container (Tuple, (open_br, exprs, close_br)) |> G.e
  in
  ArrayAccess (v1, (open_br, index_expr, close_br)) |> G.e

and invocation_expression env (v1, v2) =
  let v1 = expression env v1 in
  let v2 = argument_list env v2 in
  G.Call (v1, v2) |> G.e

and cast_expression env (v1, v2, v3, v4) =
  let v1 = token env v1 (* "(" *) in
  let v2 = type_pattern env v2 in
  let _v3 = token env v3 (* ")" *) in
  let v4 = expression env v4 in
  Cast (v2, v1, v4) |> G.e

and simple_name_expression env x =
  N (H2.name_of_ids_with_opt_typeargs [ simple_name env x ]) |> G.e

and deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = token env v1 in
  let v2 = expression env v2 in
  let v3 = token env v3 in
  DeepEllipsis (v1, v2, v3) |> G.e

and member_access_ellipsis (env : env)
    ((v1, v2, v3) : CST.member_access_ellipsis_expression) =
  let e =
    match v1 with
    | `Exp x -> expression env x
    | `Pred_type x -> pred_type env x
    | `Name x -> N (name env x) |> G.e
  in
  let _tdot =
    match v2 with
    | `DOT tok -> token env tok (* "." *)
    | `DASHGT tok -> token env tok
    (* "->" *)
  in
  let tdots = token env v3 in
  DotAccessEllipsis (e, tdots) |> G.e

and typed_metavariable (env : env) ((v1, v2, v3, v4) : CST.typed_metavariable) =
  let lp = (* "(" *) token env v1 in
  let ty = type_pattern env v2 in
  let id = (* semgrep_metavariable *) str env v3 in
  let _rp = (* ")" *) token env v4 in
  TypedMetavar (id, lp, ty) |> G.e

and expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Non_lvalue_exp v1 -> non_lvalue_expression env v1
  | `Lvalue_exp v1 -> lvalue_expression env v1
  (* semgrep: *)
  | `Ellips v1 -> Ellipsis (token env v1) |> G.e
  | `Deep_ellips v1 -> deep_ellipsis env v1
  | `Member_access_ellips_exp v1 -> member_access_ellipsis env v1
  | `Typed_meta v1 -> typed_metavariable env v1

and lvalue_expression (env : env) (x : CST.lvalue_expression) : G.expr =
  match x with
  | `This tok ->
      let t = token env tok (* "this" *) in
      IdSpecial (This, t) |> G.e
  | `Member_access_exp x -> member_access_expression env x
  | `Tuple_exp x -> tuple_expression env x
  | `Simple_name x -> simple_name_expression env x
  | `Elem_access_exp x -> element_access_expression env x
  | `Brac_arg_list x ->
      Container (Tuple, element_binding_expression env x) |> G.e
  | `Poin_indi_exp (v1, v2) ->
      DeRef (token env v1, expression env v2) |> G.e
  | `Paren_lvalue_exp (_v1, v2, _v3) -> lvalue_expression env v2
  | `Cond_access_exp x -> conditional_access_expression env x

and expression_statement_expression (env : env)
    (x : CST.expression_statement_expression) =
  match x with
  | `Assign_exp (v1, v2, v3) ->
      let v1 = lvalue_expression env v1 in
      let v2 = assignment_operator env v2 in
      let v3 = expression env v3 in
      AssignOp (v1, v2, v3) |> G.e
  | `Invo_exp x -> invocation_expression env x
  | `Post_un_exp x -> postfix_unary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = expression env v2 in
      Await (v1, v2) |> G.e
  | `Obj_crea_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = type_pattern env v2 in
      let v3 =
        match v3 with
        | Some x -> argument_list env x
        | None -> fb []
      in
      let v4 =
        match v4 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lp, v3', rp = v3 in
      let args = (lp, v3' @ [ Arg (Container (Tuple, v4) |> G.e) ], rp) in
      New (v1, v2, empty_id_info (), args) |> G.e
  | `Paren_exp x -> parenthesized_expression env x

and non_lvalue_expression (env : env) (x : CST.non_lvalue_expression) : G.expr =
  match x with
  | `Anon_meth_exp (v1, v2, v3, v4) ->
      let _v1TODO = List_.map (map_anon_choice_static_d4628a0 env) v1 in
      let tdelegate = token env v2 (* "delegate" *) in
      let fparams =
        match v3 with
        | Some x -> parameter_list env x
        | None -> fb []
      in
      let v4 = block env v4 in
      Lambda
        {
          fkind = (LambdaKind, tdelegate);
          fparams;
          frettype = None;
          fbody = G.FBStmt v4;
        }
      |> G.e
  | `Anon_obj_crea_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = anonymous_object_member_declarator env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = anonymous_object_member_declarator env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v4 = Option.map (token env) v4 (* "," *) in
      let v5 = token env v5 (* "}" *) in
      AnonClass
        {
          ckind = (Class, v1);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = (v2, v3, v5);
        }
      |> G.e
  | `Array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = array_type env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lb, _, rb = v3 in
      let args = (lb, [ Arg (G.Container (G.Tuple, v3) |> G.e) ], rb) in
      New (v1, v2, empty_id_info (), args) |> G.e
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = type_ env v3 in
      (* TODO `as` is really a conditional cast *)
      Cast (v3, v2, v1) |> G.e
  | `Base tok ->
      let x = token env tok (* "base" *) in
      IdSpecial (Super, x) |> G.e
  | `Bin_exp x -> binary_expression env x
  | `Cast_exp x -> cast_expression env x
  | `Chec_exp x -> checked_expression env x
  | `Cond_access_exp x -> conditional_access_expression env x
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5) |> G.e
  | `Coll_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 =
        match v2 with
        | Some (v1, v2, _v3) ->
            let v1 = collection_element env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  collection_element env v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 = token env v3 (* "]" *) in
      Container (List, (v1, v2, v3)) |> G.e
  | `Defa_exp x -> default_expression env x
  | `Impl_array_crea_exp (v1, v2, v3, v4, v5) ->
      let _v1TODO = token env v1 (* "new" *) in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = List_.map (token env) (* "," *) v3 in
      let _v4 = token env v4 (* "]" *) in
      let v5 = initializer_expression env v5 in
      Container (Array, v5) |> G.e
  | `Impl_obj_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = argument_list env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lp, v2', rp = v2 in
      let args = (lp, v2' @ [ Arg (Container (Tuple, v3) |> G.e) ], rp) in
      (* old: was New *)
      let e = G.OtherExpr (("NewNoType", v1), []) |> G.e in
      Call (e, args) |> G.e
  | `Impl_stac_exp (v1, v2, v4, v5) ->
      let _v1 = token env v1 (* "stackalloc" *) in
      let _v2 = token env v2 (* "[" *) in
      let _v4 = token env v4 (* "]" *) in
      let v5 = initializer_expression env v5 in
      Container (Array, v5) |> G.e
  | `Init_exp x -> Container (Tuple, initializer_expression env x) |> G.e
  | `Inte_str_exp x -> interpolated_string_expression env x
  | `Is_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "is" *) in
      let v3 = type_ env v3 in
      Call (IdSpecial (Instanceof, v2) |> G.e, fb [ Arg v1; ArgType v3 ]) |> G.e
  | `Is_pat_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "is" *) in
      let v3 = pattern env v3 in
      LetPattern (v3, v1) |> G.e
  | `Lambda_exp ((v1, v2, v3, v4), v5, v6) ->
      let _v1TODO = List_.map (attribute_list_ env) v1 in
      let _v2TODO = List_.map (map_anon_choice_static_d4628a0 env) v2 in
      let v3 =
        match v3 with
        | Some x -> Some (type_pattern env x)
        | None -> None
      in
      let v4 = lambda_parameters env v4 in
      let v5 = (* "=>" *) token env v5 in
      let v6 =
        match v6 with
        | `Blk x -> G.FBStmt (block env x)
        | `Exp x -> G.FBExpr (expression env x)
      in
      Lambda { fkind = (Arrow, v5); fparams = v4; frettype = v3; fbody = v6 }
      |> G.e
  | `Make_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "__makeref" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      Ref (v1, v3) |> G.e
  | `Query_exp (v1, v2) ->
      let v1 = from_clause env v1 in
      let v2 = query_body env v2 in
      linq_to_expr v1 v2
  | `Range_exp (v1, v2, v3) ->
      let fake_zero = L (Int Parsed_int.fake_zero) |> G.e in
      let v1 =
        match v1 with
        | Some x -> expression env x
        | None -> fake_zero
      in
      let v2 = token env v2 (* ".." *) in
      let v3 =
        match v3 with
        | Some x -> expression env x
        | None -> new_index_from_end v2 fake_zero
      in
      Call (IdSpecial (Op Range, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Ref_exp (v1, v2) ->
      let v1 = token env v1 (* "ref" *) in
      let v2 = expression env v2 in
      G.Ref (v1, v2) |> G.e
  | `Addr_of_exp (v1, v2) ->
      let v1 = token env v1 (* "&" *) in
      let v2 = lvalue_expression env v2 in
      Ref (v1, v2) |> G.e
  | `Reft_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "__reftype" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      Call
        ( IdSpecial (Typeof, v1) |> G.e,
          (v2, [ Arg (DeRef (v1, v3) |> G.e) ], v4) )
      |> G.e
  | `Refv_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "__refvalue" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* "," *) in
      let _v5 = type_pattern env v5 in
      let _v6 = token env v6 (* ")" *) in
      DeRef (v1, v3) |> G.e
  | `Sizeof_exp x -> size_of_expression env x
  | `Stac_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "stackalloc" *) in
      let v2 = array_type env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lb, _, rb = v3 in
      let args = (lb, [ Arg (G.Container (G.Tuple, v3) |> G.e) ], rb) in
      New (v1, v2, empty_id_info (), args) |> G.e
  | `Switch_exp (v1, v2, (v3, v4, _vTODO, v5)) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "switch" *) in
      let _v3 = token env v3 (* "{" *) in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = switch_expression_arm env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = switch_expression_arm env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v5 = token env v5 (* "}" *) in
      (* TODO: use Switch instead? *)
      let st = G.Switch (v2, Some (G.Cond v1), v4) |> G.s in
      G.stmt_to_expr st
  | `Throw_exp (v1, v2) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      let throw = Throw (v1, v2, sc) |> G.s in
      G.stmt_to_expr throw
  | `Typeof_exp x -> type_of_expression env x
  | `With_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "with" *) in
      let with_fields = G.Record (with_body env v3) |> G.e in
      (* THINK:
       * - with-expressions may deserve first-class support in Generic AST
       * - record patterns perhaps should match with-expressions
       *)
      G.OtherExpr (("RecordWith", v2), [ G.E v1; G.E with_fields ]) |> G.e
  | `Lit x ->
      let x = literal env x in
      G.L x |> G.e
  | `Exp_stmt_exp x -> expression_statement_expression env x

and collection_element (env : env) (x : CST.collection_element) : G.expr =
  match x with
  | `Exp_elem x -> expression env x
  | `Spread_elem (v1, v2) ->
      let v1 = token env v1 (* ".." *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Spread, v1) |> G.e, fb [ Arg v2 ]) |> G.e

and with_initializer (env : env) ((v1, v2, v3) : CST.with_initializer) :
    G.field =
  let v1 = identifier env v1 in
  let _v2 = token env v2 (* "=" *) in
  let v3 = expression env v3 in
  G.basic_field v1 (Some v3) None

(* An attribute list is wrapped in a choice of one alternative, the other
 * alternative of the base grammar being a conditional-compilation block,
 * which is an extra in this grammar. *)
and attribute_list_ (env : env) (x : CST.attribute_list_) : attribute list =
  match x with
  | `Attr_list x -> attribute_list env x

and lambda_parameters (env : env) (x : CST.lambda_parameters) : G.parameters =
  match x with
  | `Param_list x -> parameter_list env x
  | `Id x -> fb [ Param (param_of_id (identifier env x)) ]
  | `Impl_param_list_with_modifs (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = implicit_parameter_with_modifiers env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            implicit_parameter_with_modifiers env v2)
          v3
      in
      let v4 = token env v4 (* ")" *) in
      (v1, v2 :: v3, v4)

(* A lambda parameter without a type, with modifiers such as `ref` or `out`. *)
and implicit_parameter_with_modifiers (env : env)
    (x : CST.anon_choice_rep1_choice_scoped_id_5b70ee9) : G.parameter =
  match x with
  | `Id x -> Param (param_of_id (identifier env x))
  | `Rep1_choice_scoped_id (v1, v2) ->
      let pattrs = parameter_modifiers env v1 in
      let v2 = identifier env v2 in
      Param { (param_of_id v2) with pattrs }

(* The modifiers of a parameter, each an attribute whose name is its keyword,
 * in the reverse of their source order. *)
and parameter_modifiers (env : env) (xs : CST.anon_choice_scoped_3da72f4 list)
    : G.attribute list =
  List.fold_left
    (fun attrs x ->
      let tok =
        match x with
        | `Scoped tok (* "scoped" *)
        | `Ref tok (* "ref" *)
        | `Out tok (* "out" *)
        | `In tok (* "in" *)
        | `Read tok (* "readonly" *) ->
            tok
      in
      NamedAttr (fake "@", H2.name_of_id (str env tok), fb []) :: attrs)
    [] xs

and simple_name (env : env) (x : CST.simple_name) :
    G.ident * G.type_arguments option =
  match x with
  | `Gene_name (v1, v2) ->
      let v1 = identifier env v1 (* identifier *) in
      let v2 = type_argument_list env v2 in
      (v1, Some v2)
  | `Id x -> (identifier env x, None)

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) :
    case_and_body list =
  let _v1 = token env v1 (* "{" *) in
  let v2 = switch_sections env v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

and anon_choice_param_ce11a32 (env : env) (x : CST.anon_choice_param_ce11a32) =
  match x with
  | `Param x -> parameter env x
  | `Param_array (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = token env v2 (* "params" *) in
      let v3 = type_ env v3 in
      let v4 = identifier env v4 (* identifier *) in
      ParamRest
        ( v2,
          {
            pname = Some v4;
            ptype = Some v3;
            pdefault = None;
            pattrs = v1;
            pinfo = empty_id_info ();
          } )

and map_anon_choice_pat_29be9ad (env : env) (x : CST.anon_choice_pat_29be9ad) =
  match x with
  | `Pat x -> pattern env x
  | `Slice_pat (v1, v2) ->
      (* ".." rest/slice pattern in a list pattern; ast_generic has no
       * native slice pattern, so preserve it as an OtherPat *)
      let v2 = v2 |> Option.to_list |> List_.map (fun x -> G.P (pattern env x)) in
      G.OtherPat (("SlicePattern", token env v1), v2)

and anon_opt_exp_elem_rep_interp_alig_clause_48c2617 (env : env)
    (opt : CST.anon_opt_exp_elem_rep_interp_alig_clause_48c2617) : expr list =
  match opt with
  | Some (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = List_.map (interpolation_alignment_clause env) v2 in
      v1 :: v2
  | None -> []

and type_parameter_list (env : env) ((v1, v2, v3, v4) : CST.type_parameter_list)
    : G.type_parameters =
  let lt = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let gt = token env v4 (* ">" *) in
  (lt, v2 :: v3, gt)

and type_parameter_constraint (env : env) (x : CST.type_parameter_constraint) :
    (G.todo_kind, type_) Either.t =
  match x with
  | `Choice_class_opt_QMARK (`Class_opt_QMARK (tok, _))
  (* "class" *)
  (* TODO handle question mark *)
  | `Choice_class_opt_QMARK (`Struct tok) (* "struct" *)
  | `Choice_class_opt_QMARK (`Notn tok) (* "notnull" *)
  | `Choice_class_opt_QMARK (`Unma tok) ->
      (* "unmanaged" *)
      let t = G.ty_builtin (str env tok) in
      Right t
  | `Choice_class_opt_QMARK (`Cons_cons (v1, v2, v3)) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = token env v3 (* ")" *) in
      let tok = Tok.combine_toks v1 [ v2; v3 ] in
      Left ("HasConstructor", tok)
  | `Choice_class_opt_QMARK (`Type x) -> Right (type_constraint env x)
  | `Allows_ref_struct (v1, v2, v3) ->
      let v1 = token env v1 (* "allows" *) in
      let v2 = token env v2 (* "ref" *) in
      let v3 = token env v3 (* "struct" *) in
      let tok = Tok.combine_toks v1 [ v2; v3 ] in
      Left ("AllowsRefStruct", tok)

and type_constraint (env : env) (x : CST.type_) : type_ =
  (* can't be `var` *)
  type_ env x

and local_variable_type (env : env) (x : CST.type_) : type_ option =
  match x with
  | `Impl_type _tok -> None (* "var" *)
  | x -> Some (type_ env x)

and expr_statement (env : env) (x : CST.expression_statement) : stmt =
  match x with
  | `Exp_stmt_exp_SEMI (v1, v2) ->
      let v1 = expression_statement_expression env v1 in
      let v2 = token env v2 (* ";" *) in
      G.ExprStmt (v1, v2) |> G.s
  | `Ellips_SEMI (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s
  | `Ellips v1 ->
      let v1 = token env v1 in
      let v2 = G.sc in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s
  | `Deep_ellips_SEMI (v1, v2) ->
      let v1 = deep_ellipsis env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (v1, v2) |> G.s
  | `Member_access_ellips_exp_SEMI (v1, v2) ->
      let v1 = member_access_ellipsis env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (v1, v2) |> G.s
  | `Semg_meta_SEMI (v1, v2) ->
      let v1 = (* semgrep_metavariable *) str env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (G.N (AST_generic_helpers.name_of_id v1) |> G.e, v2) |> G.s
  | `Typed_meta_SEMI (v1, v2) ->
      let v1 = typed_metavariable env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (v1, v2) |> G.s

and statement (env : env) (x : CST.statement) =
  match x with
  | `Blk x -> block env x
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 = token env v2 (* ";" *) in
      G.Break (v1, G.LNone, v2) |> G.s
  | `Chec_stmt (v1, v2) ->
      let v1 =
        match v1 with
        | `Chec tok (* "checked" *) ->
            let tchecked = token env tok in
            OSWS_Block ("Checked", tchecked)
        | `Unch tok (* "unchecked" *) ->
            let tunchecked = token env tok in
            OSWS_Block ("Unchecked", tunchecked)
      in
      let v2 = block env v2 in
      OtherStmtWithStmt (v1, [], v2) |> G.s
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 = token env v2 (* ";" *) in
      Continue (v1, LNone, v2) |> G.s
  | `Do_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement env v2 in
      let _v3 = token env v3 (* "while" *) in
      let _v4 = token env v4 (* "(" *) in
      let v5 = expression env v5 in
      let _v6 = token env v6 (* ")" *) in
      let _v7 = token env v7 (* ";" *) in
      DoWhile (v1, v2, v5) |> G.s
  | `Empty_stmt tok ->
      let v1 = token env tok (* ";" *) in
      Block (v1, [], v1) |> G.s
  (* Can we have the same token as start and end of block? *)
  | `Exp_stmt v1 -> expr_statement env v1
  | `Fixed_stmt (v1, v2, v3, v4, v5) ->
      (* `fixed (T* p = &x) body` pins a variable for the body's duration;
       * model it like `using` as a scoped resource acquisition. *)
      let v1 = token env v1 (* "fixed" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        variable_declaration env v3
        |> List_.map (fun (ent, vardef) -> DefStmt (ent, VarDef vardef) |> G.s)
        |> G.stmt1
      in
      let _v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      WithUsingResource (v1, [ v3 ], v5) |> G.s
  | `Fore_stmt ((v1, v2, v3, v4, v5, v6, v7), v8) ->
      let v1 = Option.map (token env) v1 (* "await" *) in
      let v2 = token env v2 (* "foreach" *) in
      let _v3 = token env v3 (* "(" *) in
      let v4 =
        match v4 with
        | `Type_choice_id (v1, v2) -> (
            let v2 =
              match v2 with
              | `Id x ->
                  let x = identifier env x in
                  PatId (x, empty_id_info ())
              | `Tuple_pat x -> tuple_pattern env x
            in
            let v1 = local_variable_type env v1 in
            match v1 with
            | Some t -> PatTyped (v2, t)
            | None -> v2)
        | `Exp x -> H2.expr_to_pattern (expression env x)
      in
      let v5 = token env v5 (* "in" *) in
      let v6 = expression env v6 in
      let v6 =
        match v1 with
        | Some tok -> Await (tok, v6) (* "await" *) |> G.e
        | None -> v6
      in
      let _v7 = token env v7 (* ")" *) in
      let v8 = statement env v8 in
      For (v2, ForEach (v4, v5, v6), v8) |> G.s
  | `For_stmt (v1, (v2, v3, v4, v5, v6, v7, v8), v9) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | Some x -> (
            match x with
            | `Var_decl x ->
                List_.map
                  (fun (e, v) -> ForInitVar (e, v))
                  (variable_declaration env x)
            | `Exp_rep_COMMA_exp (v1, v2) ->
                let v1 = expression env v1 in
                let v2 = List_.map (interpolation_alignment_clause env) v2 in
                let exprs = v1 :: v2 in
                List_.map (fun e -> ForInitExpr e) exprs)
        | None -> []
      in
      let _v4 = token env v4 (* ";" *) in
      let v5 = Option.map (expression env) v5 in
      let v6 = token env v6 (* ";" *) in
      let v7 = anon_opt_exp_elem_rep_interp_alig_clause_48c2617 env v7 in
      let v8 = token env v8 (* ")" *) in
      let v9 = statement env v9 in
      let next =
        match v7 with
        | [] -> None
        | [ e ] -> Some e
        | _exprs -> Some (Container (Tuple, (v6, v7, v8)) |> G.e)
      in
      let for_header = ForClassic (v3, v5, next) in
      For (v1, for_header, v9) |> G.s
  | `Goto_stmt (v1, v2, v3, v4) -> (
      let v1 = token env v1 (* "goto" *) in
      let v4 = token env v4 (* ";" *) in
      match (v2, v3) with
      (* Only a few of these are allowable. See
         https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar
      *)
      | None, Some exp -> (
          let exp = expression env exp in
          match exp.e with
          | N (Id (id, _)) -> Goto (v1, id, v4) |> G.s
          | _ ->
              (* This shouldn't be permitted by the grammar above. *)
              G.OtherStmt (G.OS_Todo, [ G.Tk v1; G.E exp; G.Tk v4 ]) |> G.s)
      (* `goto case <expr>;` and `goto default;` are jumps within a switch;
       * ast_generic has no dedicated construct, so preserve the operands. *)
      | Some (`Case tok), Some exp ->
          let tcase = token env tok (* "case" *) in
          let exp = expression env exp in
          G.OtherStmt (G.OS_Todo, [ G.Tk v1; G.Tk tcase; G.E exp; G.Tk v4 ])
          |> G.s
      | Some (`Defa tok), None ->
          let tdefault = token env tok (* "default" *) in
          G.OtherStmt (G.OS_Todo, [ G.Tk v1; G.Tk tdefault; G.Tk v4 ]) |> G.s
      | _ -> G.OtherStmt (G.OS_Todo, [ G.Tk v1; G.Tk v4 ]) |> G.s)
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "if" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "else" *) in
            let v2 = statement env v2 in
            Some v2
        | None -> None
      in
      G.If (v1, G.Cond v3, v5, v6) |> G.s
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = identifier env v1 (* identifier *) in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      Label (v1, v3) |> G.s
  | `Local_decl_stmt (v1, v2, v3, v4, v5) ->
      let _V1TODO = Option.map (token env) v1 (* "await" *) in
      let _V2TODO = Option.map (token env) v2 (* "using" *) in
      let attrs = List_.map (modifier env) v3 in
      let vardefs = variable_declaration env v4 in
      let sc = token env v5 (* ";" *) in
      var_def_stmt attrs vardefs sc
  | `Local_func_stmt ((v1, v2, v3, v4, v5, v6), v7, v8) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let v4 = identifier env v4 (* identifier *) in
      let _, tok = v4 in
      let tparams = Option.map (type_parameter_list env) v5 in
      let v6 = parameter_list env v6 in
      let v7 = List_.map (type_parameter_constraints_clause env) v7 in
      let v8 = function_body env v8 in
      let tparams = type_parameters_with_constraints tparams v7 in
      let idinfo = empty_id_info () in
      let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams } in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, tok);
            fparams = v6;
            frettype = Some v3;
            fbody = v8;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Lock_stmt (v1, v2, v3, v4, v5) ->
      let tlock = token env v1 (* "lock" *) in
      let _l = token env v2 (* "(" *) in
      let e = expression env v3 in
      let _r = token env v4 (* ")" *) in
      let st = statement env v5 in
      OtherStmtWithStmt (OSWS_Block ("Lock", tlock), [ E e ], st) |> G.s
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = Option.map (expression env) v2 in
      let v3 = token env v3 (* ";" *) in
      Return (v1, v2, v3) |> G.s
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 =
        match v2 with
        | `LPAR_exp_RPAR (_v1, v2, _v3) -> expression env v2
        | `Tuple_exp v2 -> tuple_expression env v2
      in
      let v3 = switch_body env v3 in
      G.Switch (v1, Some (G.Cond v2), v3) |> G.s
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = Option.map (expression env) v2 in
      let v3 = token env v3 (* ";" *) in
      (match v2 with
      | Some expr -> Throw (v1, expr, v3)
      | None -> OtherStmt (OS_ThrowNothing, [ Tk v1; Tk v3 ]))
      |> G.s
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let v3 = List_.map (catch_clause env) v3 in
      let v4 = Option.map (finally_clause env) v4 in
      Try (v1, v2, v3, None, v4) |> G.s
  | `Unsafe_stmt (v1, v2) ->
      let tunsafe = token env v1 (* "unsafe" *) in
      let v2 = block env v2 in
      OtherStmtWithStmt (OSWS_Block ("Unsafe", tunsafe), [], v2) |> G.s
  | `Using_stmt (v1, v2, v3, v4, v5, v6) ->
      let _v1TODO = Option.map (token env) v1 (* "await" *) in
      let v2 = token env v2 (* "using" *) in
      let _v3 = token env v3 (* "(" *) in
      let v4 =
        match v4 with
        | `Using_var_decl x ->
            let vardefs = using_variable_declaration env x in
            vardefs
            |> List_.map (fun (ent, vardef) ->
                   DefStmt (ent, VarDef vardef) |> G.s)
            |> G.stmt1
        | `Exp x ->
            let expr = expression env x in
            ExprStmt (expr, sc) |> G.s
      in
      let _v5 = token env v5 (* ")" *) in
      let v6 = statement env v6 in
      WithUsingResource (v2, [ v4 ], v6) |> G.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      While (v1, G.Cond v3, v5) |> G.s
  | `Yield_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        match v2 with
        | `Ret_exp (v1, v2) ->
            let _v1 = token env v1 (* "return" *) in
            let v2 = expression env v2 in
            Some v2
        | `Brk _tok -> None
        (* "break" *)
      in
      let v3 = token env v3 (* ";" *) in
      ExprStmt (Yield (v1, v2, false) |> G.e, v3) |> G.s

and interpolated_string_expression (env : env)
    (x : CST.interpolated_string_expression) =
  (* The opening token is the start, `$`, `$@`, `@$` or `$$...`, joined to
   * the opening quote that follows it. *)
  let opening start quote = Tok.combine_toks (token env start) [ token env quote ] in
  let x =
    match x with
    | `Interp_regu_start_interp_start_quote_rep_inte_str_content_interp_end_quote
        (v0, v1, v2, v3) ->
        let v1 = opening v0 v1 (* "$\"" *) in
        let v2 = List_.map (interpolated_string_content env) v2 in
        let v3 = token env v3 (* "\"" *) in
        (v1, v2, v3)
    | `Interp_verb_start_interp_start_quote_rep_inte_verb_str_content_interp_end_quote
        (v0, v1, v2, v3) ->
        let v1 = opening v0 v1 (* "$@\"" or "@$\"" *) in
        let v2 = List_.map (interpolated_verbatim_string_content env) v2 in
        let v3 = token env v3 (* "\"" *) in
        (v1, v2, v3)
    | `Interp_raw_start_interp_start_quote_rep_inte_raw_str_content_interp_end_quote
        (v0, v1, v2, v3) ->
        let v1 = opening v0 v1 (* "$\"\"\"" *) in
        let v2 = List_.map (interpolated_raw_string_content env) v2 in
        let v3 = (* "\"\"\"" *) token env v3 in
        (v1, v2, v3)
  in
  let v1, v2, _v3 = x in
  let args = fb (List_.map (fun e -> Arg e) v2) in
  (* TODO should we use FString here instead of InterpolatedConcat? *)
  Call (IdSpecial (ConcatString InterpolatedConcat, v1) |> G.e, args) |> G.e

and tuple_element (env : env) ((v1, v2) : CST.tuple_element) =
  let v1 = type_pattern env v1 in
  let _v2TODO = Option.map (identifier env) v2 (* identifier *) in
  v1

(* A name in a constant pattern is a value, never a binder: C# binders are
 * `var x` and `T x`, and a bare name is a constant or a type. *)
and constant_pattern (env : env) (x : CST.constant_pattern) : G.pattern =
  match x with
  | `Tuple_exp x -> positional_pattern env x
  | _ ->
      let e = constant_pattern_aux env x in
      G.OtherPat (("ExprToPattern", Tok.unsafe_fake_tok ""), [ G.E e ])

(* The grammar parses a positional pattern such as `(FOO, var y)` as a
 * constant pattern holding a tuple expression, and [declaration_expression]
 * converts `var y` to the same expression as a constant `y`. So the
 * elements are converted here, from the CST. *)
and positional_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) :
    G.pattern =
  let v1 = token env v1 (* "(" *) in
  let v2 = positional_pattern_element env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        positional_pattern_element env v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  G.PatTuple (v1, v2 :: v3, v4)

and positional_pattern_element (env : env) (x : CST.argument) : G.pattern =
  match x with
  | `Opt_id_COLON_opt_choice_ref_choice_exp (v1, _v2, v3) -> (
      let v1 =
        Option.map
          (fun (v1, v2) ->
            let _v2 = token env v2 (* ":" *) in
            identifier env v1)
          v1
      in
      let pat =
        match v3 with
        | `Decl_exp x -> H2.expr_to_pattern (declaration_expression env x)
        | `Exp (`Lvalue_exp (`Tuple_exp x)) -> positional_pattern env x
        | `Exp x -> (
            match expression env x with
            | { e = G.Ellipsis tok; _ } -> G.PatEllipsis tok
            | e -> G.OtherPat (("ExprToPattern", Tok.unsafe_fake_tok ""), [ G.E e ]))
      in
      match v1 with
      | None -> pat
      | Some id ->
          G.PatTuple
            (Tok.unsafe_fake_bracket [ G.PatId (id, G.empty_id_info ()); pat ]))
  | `Semg_vari_meta _ -> H2.expr_to_pattern (H2.argument_to_expr (argument env x))

(* TODO: generate directly pattern so no need to use H2.expr_to_pattern *)
and constant_pattern_aux env (x : CST.constant_pattern) : G.expr =
  match x with
  | `Cst_pat_bin_exp x ->
      binary_expression env (x :> CST.binary_expression)
  | `Defa_exp x -> default_expression env x
  | `Inte_str_exp x -> interpolated_string_expression env x
  | `Paren_exp x -> parenthesized_expression env x
  | `Post_un_exp x -> postfix_unary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Sizeof_exp x -> size_of_expression env x
  | `Tuple_exp x -> tuple_expression env x
  | `Typeof_exp x -> type_of_expression env x
  | `Member_access_exp x -> member_access_expression env x
  | `Name_invo_pat (v1, v2) ->
      let v1 = name_expression env v1 in
      let v2 = argument_list env v2 in
      G.Call (v1, v2) |> G.e
  | `Comp_invo_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Member_access_exp x -> member_access_expression env x
        | `Elem_access_exp x -> element_access_expression env x
        | `Invo_exp x -> invocation_expression env x
        | `Paren_exp x -> parenthesized_expression env x
        | `Cond_access_exp x -> conditional_access_expression env x
        | `Cast_exp x -> cast_expression env x
      in
      let v2 = argument_list env v2 in
      G.Call (v1, v2) |> G.e
  | `Cast_exp x -> cast_expression env x
  | `Simple_name x -> simple_name_expression env x
  | `Lit x ->
      let lit = literal env x in
      G.L lit |> G.e

(* A name used as an expression, converted as the expression grammar
 * converts a member access: `A.B` is a DotAccess on `A`. *)
and name_expression (env : env) (x : CST.name) : G.expr =
  match x with
  | `Simple_name x -> simple_name_expression env x
  | `Qual_name (v1, v2, v3) ->
      let v1 = name_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = simple_name env v3 in
      let n = H2.name_of_ids_with_opt_typeargs [ v3 ] in
      G.DotAccess (v1, v2, G.FN n) |> G.e
  | `Alias_qual_name _ -> N (name env x) |> G.e

and catch_declaration (env : env) ((v1, v2, v3, v4) : CST.catch_declaration) :
    G.catch_exn =
  let _v1 = token env v1 (* "(" *) in
  let v2 = type_pattern env v2 in
  let v3 = Option.map (identifier env) v3 (* identifier *) in
  let _v4 = token env v4 (* ")" *) in
  CatchParam (G.param_of_type v2 ?pname:v3)

and query_clause (env : env) (x : CST.query_clause) =
  match x with
  | `From_clause x -> from_clause env x
  | `Join_clause (v1, (v2, v3, v4, v5), (v6, v7, v8, v9), v10) ->
      let v1 = token env v1 (* "join" *) in
      let v2 = Option.map (type_pattern env) v2 in
      let v3 = identifier env v3 (* identifier *) in
      let _v4 = token env v4 (* "in" *) in
      let v5 = expression env v5 in
      let _v6 = token env v6 (* "on" *) in
      let v7 = expression env v7 in
      let _v8 = token env v8 (* "equals" *) in
      let v9 = expression env v9 in
      let v10 = Option.map (join_into_clause env) v10 in
      Join (v1, (v2, v3), v5, v7, v9, v10)
  | `Let_clause (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = identifier env v2 (* identifier *) in
      let _v3 = token env v3 (* "=" *) in
      let v4 = expression env v4 in
      Let (v1, v2, v4)
  | `Order_by_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "orderby" *) in
      let v2 = ordering env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = ordering env v2 in
            v2)
          v3
      in
      OrderBy (v1, v2 :: v3)
  | `Where_clause (v1, v2) ->
      let v1 = token env v1 (* "where" *) in
      let v2 = expression env v2 in
      Where (v1, v2)

and arrow_expression_clause (env : env) ((v1, v2) : CST.arrow_expression_clause)
    =
  let v1 = token env v1 (* "=>" *) in
  let v2 = expression env v2 in
  (v1, v2)

and attribute_argument (env : env) ((v1, v2) : CST.attribute_argument) =
  let v1 =
    Option.map
      (fun (v1, v2) ->
        let _v2 =
          match v2 with
          | `COLON tok -> token env tok (* ":" *)
          | `EQ tok -> token env tok (* "=" *)
        in
        identifier env v1)
      v1
  in
  let v2 = expression env v2 in
  match v1 with
  | Some name -> ArgKwd (name, v2)
  | None -> Arg v2

and catch_filter_clause (env : env) ((v1, v2, v3, v4) : CST.catch_filter_clause)
    =
  let _v1 = token env v1 (* "when" *) in
  let _v2 = token env v2 (* "(" *) in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* ")" *) in
  v3

and formal_parameter_list (env : env)
    ((v1, v2) : CST.anon_choice_param_rep_COMMA_choice_param_6325c4d) =
  let v1 = anon_choice_param_ce11a32 env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = anon_choice_param_ce11a32 env v2 in
        v2)
      v2
  in
  v1 :: v2

and equals_value_clause (env : env)
    ((v1, v2) : Tree_sitter_run.Token.t * CST.expression) : expr =
  let _v1 = token env v1 (* "=" *) in
  let v2 = expression env v2 in
  v2

(* A switch section of the C# specification is one or more labels followed by
 * statements. The grammar gives each label a section of its own, so the
 * labels of a section without statements join the next section. *)
and switch_sections (env : env) (xs : CST.switch_section list) :
    case_and_body list =
  let pending, sections =
    List.fold_left
      (fun (pending, sections) x ->
        match switch_section env x with
        | label, [] -> (label :: pending, sections)
        | label, stmts ->
            (* TODO: we convert list of statements to a block with fake brackets. Does this make sense? *)
            ( [],
              CasesAndBody (List.rev (label :: pending), G.stmt1 stmts)
              :: sections ))
      ([], []) xs
  in
  let sections =
    match pending with
    | [] -> sections
    | _ -> CasesAndBody (List.rev pending, G.stmt1 []) :: sections
  in
  List.rev sections

and switch_section (env : env) ((v1, v2, v3) : CST.switch_section) :
    G.case * stmt list =
  let v1 =
    match v1 with
    | `Case_choice_exp (v1, `Exp x) ->
        let v1 = token env v1 (* "case" *) in
        let x = expression env x in
        G.CaseEqualExpr (v1, x)
    (* A constant pattern tests equality with its constant, as the case
     * expression of a switch label does. A tuple is a positional pattern,
     * see [constant_pattern]. *)
    | `Case_choice_exp (v1, `Pat_opt_when_clause (`Cst_pat x, None))
      when match x with
           | `Tuple_exp _ -> false
           | _ -> true ->
        let v1 = token env v1 (* "case" *) in
        let x = constant_pattern_aux env x in
        G.CaseEqualExpr (v1, x)
    | `Case_choice_exp (v1, `Pat_opt_when_clause (x, when_)) ->
        let v1 = token env v1 (* "case" *) in
        let x = pattern env x in
        let x =
          match when_ with
          | Some w -> PatWhen (x, when_clause env w)
          | None -> x
        in
        G.Case (v1, x)
    | `Defa tok -> G.Default (token env tok (* "default" *))
  in
  let _v2 = token env v2 (* ":" *) in
  let v3 = List_.map (statement env) v3 in
  (v1, v3)

and attribute_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_list) :
    attribute list =
  (* TODO: Handle unused tokens. *)
  let _v1 = token env v1 (* "[" *) in
  let _v2 = Option.map (attribute_target_specifier env) v2 in
  let v3 = attribute env v3 in
  let v4 =
    List_.map
      (fun (x, y) ->
        token env x (* "," *) |> ignore;
        attribute env y)
      v4
  in
  let _v5 = Option.map (token env) v5 (* "," *) in
  let _v6 = token env v6 (* "]" *) in
  v3 :: v4

and bracketed_argument_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.bracketed_argument_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 = argument env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = argument env v2 in
        v2)
      v3
  in
  let _v4 = Option.map (token env) v4 (* "," *) in
  let v5 = token env v5 (* "]" *) in
  (v1, v2 :: v3, v5)

and pattern (env : env) (x : CST.pattern) : G.pattern =
  match x with
  | `Cst_pat x -> constant_pattern env x
  (* `var x`, which the grammar also reads as a declaration pattern with the
   * type `var`, is a var pattern. *)
  | `Decl_pat (`Impl_type v1, v2) ->
      let _v1 = token env v1 (* "var" *) in
      let v2 = variable_designation env v2 in
      v2
  | `Decl_pat (v1, v2) ->
      let v1 = type_pattern env v1 in
      let v2 = variable_designation env v2 in
      PatTyped (v2, v1)
  | `Disc tok -> PatWildcard (token env tok) (* "_" *)
  | `Recu_pat x -> recursive_pattern env x
  | `Var_pat (v1, v2) ->
      let _v1 = token env v1 (* "var" *) in
      let v2 = variable_designation env v2 in
      v2
  | `Nega_pat (v1, v2) ->
      let v1 = token env v1 (* "not" *) in
      let v2 = pattern env v2 in
      (* ast_generic has no negation pattern; preserve the inner pattern *)
      G.OtherPat (("NotPattern", v1), [ G.P v2 ])
  | `Paren_pat (v1, _v2lp, v3) ->
      (* parentheses are transparent; keep the inner pattern *)
      let _v1 = token env v1 (* "(" *) in
      let inner = pattern env _v2lp in
      let _v3 = token env v3 (* ")" *) in
      inner
  | `Paren_pat_with_desi (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = pattern env v2 in
      let _v3 = token env v3 (* ")" *) in
      pattern_with_designation env v2 v4
  | `Rela_pat x -> relational_pattern env x
  | `And_pat (v1, v2, v3) ->
      let v1 = pattern env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = pattern env v3 in
      (* ast_generic has no conjunction pattern; preserve both sides *)
      G.OtherPat (("AndPattern", v2), [ G.P v1; G.P v3 ])
  | `Or_pat (v1, v2, v3) ->
      let v1 = pattern env v1 in
      let _v2 = (* "or" *) token env v2 in
      let v3 = pattern env v3 in
      PatDisj (v1, v3)
  | `List_pat (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, _v3) ->
            let v1 = map_anon_choice_pat_29be9ad env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  map_anon_choice_pat_29be9ad env v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 = (* "]" *) token env v3 in
      let pat = PatList (v1, v2, v3) in
      (match v4 with
      | None -> pat
      | Some x -> pattern_with_designation env pat x)
  | `Type_pat x -> PatType (type_pattern env x)

and recursive_pattern (env : env) (x : CST.recursive_pattern) : G.pattern =
  let ty, pat, desig =
    match x with
    | `Name_posi_pat_clause_opt_prop_pat_clause_var_desi (v1, v2, v3, v4) ->
        (Some (type_name env v1), positional_and_property env v2 v3, Some v4)
    | `Name_posi_pat_clause_opt_prop_pat_clause (v1, v2, v3) ->
        (Some (type_name env v1), positional_and_property env v2 v3, None)
    | `Posi_pat_clause_var_desi (v1, v2) ->
        (None, positional_pattern_clause env v1, Some v2)
    | `Posi_pat_clause x -> (None, positional_pattern_clause env x, None)
    | `Type_choice_posi_pat_clause_opt_prop_pat_clause_opt_var_desi (v1, v2, v3)
      ->
        let pat =
          match v2 with
          | `Posi_pat_clause_opt_prop_pat_clause (v1, v2) ->
              positional_and_property env v1 v2
          | `Prop_pat_clause x -> property_pattern_clause env x
        in
        (Some (type_ env v1), pat, v3)
    | `Choice_posi_pat_clause_prop_pat_clause_opt_var_desi (v1, v2) ->
        let pat =
          match v1 with
          | `Posi_pat_clause_prop_pat_clause (v1, v2) ->
              positional_and_property env v1 (Some v2)
          | `Prop_pat_clause x -> property_pattern_clause env x
        in
        (None, pat, v2)
  in
  let pat =
    match ty with
    | None -> pat
    | Some t -> PatTyped (pat, t)
  in
  match desig with
  | None -> pat
  | Some x -> pattern_with_designation env pat x

and positional_and_property (env : env) (v1 : CST.positional_pattern_clause)
    (v2 : CST.property_pattern_clause option) : G.pattern =
  let positional = positional_pattern_clause env v1 in
  (* a positional clause may be followed by a property clause, e.g.
   * `Point (1, 2) { Name: "p" }`; preserve both when present *)
  match v2 with
  | None -> positional
  | Some x ->
      let prop = property_pattern_clause env x in
      G.OtherPat
        ( ("PositionalAndProperty", fake "recursive_pattern"),
          [ G.P positional; G.P prop ] )

and pattern_with_designation (env : env) (pat : G.pattern)
    (x : CST.variable_designation) : G.pattern =
  (* trailing binder, e.g. `Point { X: 0 } p` binds `p` *)
  let desig = variable_designation env x in
  match desig with
  | PatId (id, id_info) -> PatAs (pat, (id, id_info))
  | _ ->
      G.OtherPat
        ( ("PatternDesignation", fake "recursive_pattern"),
          [ G.P pat; G.P desig ] )

and positional_pattern_clause (env : env)
    ((v1, v2, v3) : CST.positional_pattern_clause) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (Some (v1, v2)) ->
        let v1 = subpattern env v1 in
        let v2 = List_.map (fun (_comma, v2) -> subpattern env v2) v2 in
        v1 :: v2
    | Some None
    | None ->
        []
  in
  let v3 = token env v3 (* ")" *) in
  PatTuple (v1, v2, v3)

and subpattern (env : env) ((_v1TODO, v2) : CST.subpattern) =
  let v2 = pattern env v2 in
  v2

(* A property subpattern `Name: pat` (or extended `A.B: pat`). Returns the
 * property name as a dotted_ident plus the pattern, for use in PatRecord. *)
and property_subpattern (env : env) ((v1, v2) : CST.subpattern) :
    G.dotted_ident * G.pattern =
  let name =
    match v1 with
    | Some (`Exp_COLON (name_expr, _colon)) -> (
        let e = expression env name_expr in
        let rec dotted_of_expr e =
          match e.G.e with
          | G.N (G.Id (id, _)) -> [ id ]
          | G.N (G.IdQualified { name_last = id, _; _ }) -> [ id ]
          | G.DotAccess (e, _, G.FN (G.Id (id, _))) -> dotted_of_expr e @ [ id ]
          | _ -> [ ("", fake "_") ]
        in
        dotted_of_expr e)
    | Some (`Id_COLON (id, _colon)) -> [ identifier env id ]
    | None -> [ ("", fake "_") ]
  in
  let pat = pattern env v2 in
  (name, pat)

and property_pattern_clause (env : env)
    ((v1, v2, _v3comma, v4) : CST.property_pattern_clause) =
  let lb = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2) ->
        let v1 = property_subpattern env v1 in
        let v2 =
          List_.map
            (fun (_comma, sp) -> property_subpattern env sp)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let rb = token env v4 (* "}" *) in
  PatRecord (lb, fields, rb)

and anonymous_object_member_declarator (env : env)
    (x : CST.anonymous_object_member_declarator) =
  match x with
  | `Id_EQ_exp x -> with_initializer env x
  | `Exp x ->
      let expr = expression env x in
      F (exprstmt expr)

and function_body (env : env) (x : CST.function_body) : G.function_body =
  match x with
  | `Blk x -> G.FBStmt (block env x)
  | `Arrow_exp_clause_SEMI (v1, v2) ->
      let v1 = arrow_expression_clause env v1 in
      let v2 = token env v2 (* ";" *) in
      let _arrow, expr = v1 in
      G.FBStmt (G.ExprStmt (expr, v2) |> G.s)
  | `SEMI tok ->
      let t = token env tok (* ";" *) in
      G.FBDecl t

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  (v1, v2)

and parameter (env : env) (v1 : CST.parameter) : G.parameter =
  match v1 with
  | `Rep_attr_list__opt_rep_choice_this_type_id_opt_EQ_exp x ->
      explicit_parameter env x
  | `Ellips v1 -> ParamEllipsis (token env v1)

and parameter_type_with_modifiers (env : env)
    ((v1, v2) : CST.parameter_type_with_modifiers) =
  (* reusing the attribute extern to match "extern" class to which we attach the method *)
  let this_attr =
    List.find_map
      (function
        | `This tok -> Some (KeywordAttr (Extern, token env tok (* "this" *)))
        | `Scoped _
        | `Ref _
        | `Out _
        | `In _
        | `Read _ ->
            None)
      v1
  in
  let modifiers =
    List.filter_map
      (function
        | `This _ -> None
        | (`Scoped _ | `Ref _ | `Out _ | `In _ | `Read _) as x -> Some x)
      v1
  in
  let attrs = parameter_modifiers env modifiers @ Option.to_list this_attr in
  let v2 = type_or_var env v2 in
  (v2, attrs)

and explicit_parameter (env : env)
    ((v1, v2, v3, v4) :
      CST.attribute_list_ list
      * CST.parameter_type_with_modifiers option
      * CST.identifier
      * (Tree_sitter_run.Token.t * CST.expression) option) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = Option.map (parameter_type_with_modifiers env) v2 in
  let v3 = identifier env v3 (* identifier *) in
  let v4 = Option.map (equals_value_clause env) v4 in
  Param
    {
      pname = Some v3;
      ptype = Option.map fst v2;
      pdefault = v4;
      pattrs = v1 @ List.concat (Option.to_list (Option.map snd v2));
      pinfo = empty_id_info ();
    }

and from_clause (env : env) ((v1, v2, v3, v4, v5) : CST.from_clause) :
    linq_query_part =
  let v1 = token env v1 (* "from" *) in
  let v2 = Option.map (type_pattern env) v2 in
  let v3 = identifier env v3 (* identifier *) in
  let _v4 = token env v4 (* "in" *) in
  let v5 = expression env v5 in
  From (v1, (v2, v3), v5)

and attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = name env v1 in
  let v2 =
    match v2 with
    | Some x -> attribute_argument_list env x
    | None -> fb []
  in
  match v1 with
  | G.Id (("get", tok), _) -> G.KeywordAttr (G.Getter, tok)
  | G.Id (("set", tok), _) -> G.KeywordAttr (G.Setter, tok)
  (* TODO get the first [ as token here? *)
  | _ -> G.NamedAttr (fake "[", v1, v2)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) : G.arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = argument env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and pointer_type env (v1, v2) =
  let v1 = pointer_base_type env v1 in
  let v2 = token env v2 (* "*" *) in
  TyPointer (v2, v1) |> G.t

and pointer_base_type (env : env) (x : CST.pointer_base_type) =
  match x with
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

and function_pointer_type env (v1, v2, _v3conv, v4, v5, v6, v7) =
  (* `delegate*<T1, T2, TRet>` is a function pointer type; model it as a
   * function type TyFun, like a delegate declaration. The optional calling
   * convention (v3) is dropped, but per-parameter ref/in/out modifiers are
   * preserved as attributes so rules can match on them. *)
  let _v1 = token env v1 (* "delegate" *) in
  let _v2 = token env v2 (* "*" *) in
  let _v4 = token env v4 (* "<" *) in
  let params =
    List_.map
      (fun ((ref_opt, base_ty), _comma) ->
        let pattrs =
          match ref_opt with
          | Some (`Ref tok) ->
              [ NamedAttr (fake "@", H2.name_of_id ("ref", token env tok), fb []) ]
          | Some (`Out tok) ->
              [ NamedAttr (fake "@", H2.name_of_id ("out", token env tok), fb []) ]
          | Some (`In tok) ->
              [ NamedAttr (fake "@", H2.name_of_id ("in", token env tok), fb []) ]
          | None -> []
        in
        G.Param (G.param_of_type ~pattrs (ref_base_type env base_ty)))
      v5
  in
  let ret = type_ env v6 in
  let _v7 = token env v7 (* ">" *) in
  TyFun (params, ret) |> G.t

and tuple_type (env : env) ((v1, v2, v3, v4) : CST.tuple_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 = tuple_element env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = tuple_element env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  TyTuple (v1, v2 :: v3, v4) |> G.t

and ref_type (env : env) ((v1, v2, v3) : CST.ref_type) =
  let v1 = (* "ref" *) token env v1 in
  let _v2TODO =
    match v2 with
    | Some tok -> Some ((* "readonly" *) token env tok)
    | None -> None
  in
  let v3 = type_or_var env v3 in
  (* not exactly a real ref
     https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref#passing-an-argument-by-reference
  *)
  OtherType (("ref", v1), [ G.T v3 ]) |> G.t

and type_name (env : env) (x : CST.name) =
  let n = name env x in
  TyN n |> G.t

and ref_base_type (env : env) (x : CST.ref_base_type) =
  match x with
  | `Impl_type tok -> G.OtherType (("var", token env tok), []) |> G.t
  | `Array_type x -> array_type env x
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

(* A type where the grammar accepts `var`, which converts as in
 * [ref_base_type]: the type of a ref local, of a parameter, of a receiver. *)
and type_or_var (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Impl_type tok -> G.OtherType (("var", token env tok), []) |> G.t
  | x -> type_ env x

and scoped_type (env : env) ((v1, v2) : CST.scoped_type) =
  (* I (Brandon) am not sure what this is. I couldn't find docs on it anywhere. *)
  G.OtherType (("scoped", token env v1), [ G.T (scoped_base_type env v2) ])
  |> G.t

and scoped_base_type (env : env) (x : CST.scoped_base_type) =
  match x with
  | `Name x -> type_name env x
  | `Ref_type x -> ref_type env x

and type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Impl_type _tok ->
      (* When type_ is called, we expect an explicit type, not "var".
         The implicit type is handled in local_variable_type. *)
      raise
        (Parsing_error.Other_error
           ("Expected explicit type", Parse_tree_sitter_helpers.token env _tok))
  | `Array_type x -> array_type env x
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x
  | `Ref_type x -> ref_type env x
  | `Scoped_type x -> scoped_type env x

and type_argument_list (env : env) ((v1, v2, v3) : CST.type_argument_list) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | `Rep_COMMA _xs -> [] (* TODO What's this case? <,,,>? *)
    | `Type_rep_COMMA_type (v1, v2) ->
        let v1 = type_pattern env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = type_pattern env v2 in
              v2)
            v2
        in
        v1 :: v2
  in
  let v3 = token env v3 (* ">" *) in
  (v1, List_.map (fun t -> TA t) v2, v3)

and type_parameter_constraints_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_constraints_clause) =
  let _v1 = token env v1 (* "where" *) in
  let v2 = identifier env v2 in
  let _v3 = token env v3 (* ":" *) in
  let v4 = type_parameter_constraint env v4 in
  let v5 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter_constraint env v2 in
        v2)
      v5
  in
  (v2, v4 :: v5)

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) : parameters
    =
  let lp = token env v1 (* "(" *) in
  let xs =
    match v2 with
    | Some x -> formal_parameter_list env x
    | None -> []
  in
  let rp = token env v3 (* ")" *) in
  (lp, xs, rp)

and attribute_argument_list (env : env)
    ((v1, v2, v3) : CST.attribute_argument_list) : arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = attribute_argument env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = attribute_argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and select_or_group_clause (env : env) (x : CST.select_or_group_clause) =
  match x with
  | `Group_clause (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "group" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* "by" *) in
      let v4 = expression env v4 in
      Group (v1, v2, v4)
  | `Select_clause (v1, v2) ->
      let v1 = token env v1 (* "select" *) in
      let v2 = expression env v2 in
      Select (v1, v2)

and declaration_expression (env : env) ((v1, v2) : CST.declaration_expression) =
  let v1 = local_variable_type env v1 in
  let v2 = identifier env v2 (* identifier *) in
  match v1 with
  | Some t ->
      let ent = basic_entity v2 in
      let vardef = { vinit = None; vtype = Some t; vtok = G.no_sc } in
      let st = DefStmt (ent, VarDef vardef) |> G.s in
      G.stmt_to_expr st
  | None -> N (Id (v2, empty_id_info ())) |> G.e

(* The declaration of a using statement, converted as a variable declaration. *)
and using_variable_declaration (env : env)
    ((v1, v2, v3) : CST.using_variable_declaration) :
    (entity * variable_definition) list =
  let vtype = local_variable_type env v1 in
  let declarator ((id, init) : CST.using_variable_declarator) =
    let id = identifier env id in
    let vinit = Option.map (equals_value_clause env) init in
    (basic_entity id, { vinit; vtype; vtok = G.no_sc })
  in
  let v2 = declarator v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        declarator v2)
      v3
  in
  v2 :: v3

and interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) :
    expr bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 = expression env v2 in
  let _v3_TODO = Option.map (interpolation_alignment_clause env) v3 in
  let _v4_TODO = Option.map (interpolation_format_clause env) v4 in
  let v5 = token env v5 (* "}" *) in
  (v1, v2, v5)

let explicit_interface_specifier (env : env)
    ((v1, v2) : CST.explicit_interface_specifier) =
  let v1 = name env v1 in
  let _v2 = token env v2 (* "." *) in
  v1

let accessor_declaration (env : env)
    ((v1, v2, v3, v4) : CST.accessor_declaration) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List_.map (modifier env) v2 in
  let v3 =
    match v3 with
    | `Get tok -> (str env tok, KeywordAttr (Getter, token env tok)) (* "get" *)
    | `Set tok -> (str env tok, KeywordAttr (Setter, token env tok)) (* "set" *)
    | `Add tok -> (str env tok, unhandled_keywordattr (str env tok)) (* "add" *)
    | `Remove tok ->
        (str env tok, unhandled_keywordattr (str env tok)) (* "remove" *)
    | `Init tok ->
        (str env tok, unhandled_keywordattr (str env tok)) (* "init" *)
    | `Id tok ->
        (* custom/contextual accessor name; preserve it like add/remove/init *)
        let id = identifier env tok in
        (id, unhandled_keywordattr id)
  in
  let v4 = function_body env v4 in
  let id, attr = v3 in
  ((attr :: v1) @ v2, id, v4)

let bracketed_parameter_list (env : env)
    ((v1, v2, v3) : CST.bracketed_parameter_list) : G.parameters =
  let lbra = token env v1 (* "[" *) in
  let params =
    match v2 with
    | Some x -> formal_parameter_list env x
    | None -> []
  in
  let rbra = token env v3 (* "]" *) in
  (lbra, params, rbra)

let constructor_initializer (env : env)
    ((v1, v2, v3) : CST.constructor_initializer) =
  let _v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | `Base tok -> IdSpecial (Super, token env tok) (* "base" *) |> G.e
    | `This tok -> IdSpecial (This, token env tok) |> G.e
    (* "this" *)
  in
  let v3 = argument_list env v3 in
  ExprStmt (Call (v2, v3) |> G.e, sc) |> G.s

let enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  match x with
  | `Rep_attr_list__id_opt_EQ_exp (v1, v2, v3) ->
      let _v1TODO = List.concat_map (attribute_list_ env) v1 in
      let v2 = identifier env v2 (* identifier *) in
      let v3 = Option.map (equals_value_clause env) v3 in
      OrEnum (v2, v3)
  | `Ellips tok -> OrEllipsis (token env tok)

(* Each parent may carry primary constructor arguments, e.g.
 * `class B(int x) : A(x)`. *)
let base_list (env : env) ((v1, v2, v3, v4) : CST.base_list) :
    G.class_parent list =
  let _v1 = token env v1 (* ":" *) in
  let parent (t : CST.type_) (args : CST.argument_list option) =
    (type_pattern env t, Option.map (argument_list env) args)
  in
  let v4 =
    List_.map
      (fun (v1, v2, v3) ->
        let _v1 = token env v1 (* "," *) in
        parent v2 v3)
      v4
  in
  parent v2 v3 :: v4

(* Like [base_list] but for records, where the parents are [type_name]s
 * rather than [type_pattern]s. The first parent may carry primary
 * constructor arguments (e.g. `record B(int x) : A(x)`). *)
let record_base (env : env) (x : CST.record_base) : G.class_parent list =
  match x with
  | `COLON_name_rep_COMMA_name (v1, v2, v3) ->
      let _v1 = token env v1 (* ":" *) in
      let v2 = type_name env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            type_name env v2)
          v3
      in
      v2 :: v3 |> List_.map (fun t -> (t, None))
  | `COLON_prim_cons_base_type_opt_COMMA_name_rep_COMMA_name (v1, v2, v3) ->
      let _v1 = token env v1 (* ":" *) in
      let v2_type, v2_args = v2 in
      let base_type = type_name env v2_type in
      let base_args = argument_list env v2_args in
      let rest =
        match v3 with
        | Some (_comma, first, others) ->
            let first = type_name env first in
            let others =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  type_name env v2)
                others
            in
            (first :: others) |> List_.map (fun t -> (t, None))
        | None -> []
      in
      (base_type, Some base_args) :: rest

let accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List_.map (accessor_declaration env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

let enum_member_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.enum_member_declaration_list) =
  let _v1 = token env v1 (* "{" *) in
  let member (`Enum_member_decl x) = enum_member_declaration env x in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = member v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = member v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = Option.map (token env) v3 (* "," *) in
  let _v4 = token env v4 (* "}" *) in
  v2

let rec declaration_list ?(this_param=None) (env : env)
    ((open_bracket, body, close_bracket) : CST.declaration_list) =
  let xs = List_.map (declaration ~this_param env) body in
  (token env open_bracket, xs, token env close_bracket)

and extern_alias_directive (env : env)
    ((v1, v2, v3, v4) : CST.extern_alias_directive) =
  let v1 = token env v1 (* "extern" *) in
  let _v2 = token env v2 (* "alias" *) in
  let v3 = identifier env v3 (* identifier *) in
  let v4 = token env v4 (* ";" *) in
  let extern =
    G.OtherDirective (("ExternAlias", v1), [ G.I v3; G.Tk v4 ]) |> G.d
  in
  G.DirectiveStmt extern |> G.s

and using_directive (env : env) ((v0, v1, v2, v3) : CST.using_directive) =
  let _globalTODO = Option.map (token env) v0 in
  let v1 = token env v1 (* "using" *) in
  let v3 = token env v3 (* ";" *) in
  (* NOTE: A using statement can be an import, but also a type alias. Since
   * we cannot know which one is it from the single file, we assume:
   * - name = import
   * - any other type = alias
   *)
  match v2 with
  | `Opt_unsafe_id_EQ_type (_unsafe, alias, _eq, `Name n) ->
      (* using Foo = System.Text; *)
      let n = name env n in
      let alias = identifier env alias in
      let import =
        G.ImportAs
          ( v1,
            G.DottedName (H2.dotted_ident_of_name n),
            Some (alias, empty_id_info ()) )
      in
      G.DirectiveStmt (import |> G.d) |> G.s
  | `Opt_unsafe_id_EQ_type (_unsafe, alias, _eq, t) ->
      let t = type_ env t in
      let alias = identifier env alias in
      let entity = basic_entity alias in
      let def = TypeDef { tbody = G.AliasType t } in
      G.DefStmt (entity, def) |> G.s
  | `Rep_choice_static_name (_mods, n) ->
      let n = name env n in
      (* using static System.Math; *)
      (* THINK: The generic AST is undistinguishable from that of `using Foo`. *)
      (* using System.IO; *)
      let import =
        G.ImportAll (v1, G.DottedName (H2.dotted_ident_of_name n), v3)
      in
      G.DirectiveStmt (import |> G.d) |> G.s

and global_attribute (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.global_attribute) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | `Asse tok -> str env tok (* "assembly" *)
    | `Module tok -> str env tok
    (* "module" *)
  in
  let _v3 = token env v3 (* ":" *) in
  let v4 = attribute env v4 in
  let v5 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = attribute env v2 in
        v2)
      v5
  in
  let _v6 = Option.map (token env) v6 (* "," *) in
  let _v7 = token env v7 (* "]" *) in
  let anys = List_.map (fun a -> At a) (v4 :: v5) in
  (* TODO: better as OtherStmt *)
  ExprStmt (OtherExpr (v2, anys) |> G.e, v1) |> G.s

and global_statement (env : env) (x : CST.global_statement) = statement env x

and namespace_member_declaration (env : env)
    (x : CST.namespace_member_declaration) =
  match x with
  | `Name_decl x -> namespace_declaration env x
  | `Type_decl x -> type_declaration env x

and compilation_unit (env : env) (xs : CST.compilation_unit) : any =
  match xs with
  | `Opt_sheb_dire_rep_top_level_item (_v1, v2) ->
      G.Pr (List_.map (top_level_item env) v2)
  | `Semg_exp (_v1, v2) ->
      let v2 = expression env v2 in
      G.E v2

and top_level_item (env : env) (x : CST.top_level_item) : stmt =
  match x with
  | `Choice_extern_alias_dire x -> top_level_item_no_statement env x
  | `Global_stmt x -> global_statement env x

and top_level_item_no_statement (env : env)
    (x : CST.top_level_item_no_statement) : stmt =
  match x with
  | `Extern_alias_dire x -> extern_alias_directive env x
  | `Using_dire x -> using_directive env x
  | `Global_attr x -> global_attribute env x
  | `Choice_name_decl x -> namespace_member_declaration env x
  | `File_scoped_name_decl x -> file_scoped_namespace_declaration env x

(* The members of a file-scoped namespace follow it as top-level items. *)
and file_scoped_namespace_declaration (env : env)
    ((v1, v2, v3) : CST.file_scoped_namespace_declaration) =
  let tnamespace = (* "namespace" *) token env v1 in
  let n = name env v2 in
  let _tsemi = (* ";" *) token env v3 in
  let dotted_ident = H2.dotted_ident_of_name n in
  let namespace = G.Package (tnamespace, dotted_ident) |> G.d in
  G.DirectiveStmt namespace |> G.s

and namespace_declaration (env : env)
    ((v1, v2, v3, _semi) : CST.namespace_declaration) =
  (*
        namespace MySpace { ... } ;
            v1       v2      v3  v4
      *)
  let _v1 = token env v1 (* "namespace" *) in
  let v2 = name env v2 in
  let open_brace, decls, close_brace = declaration_list env v3 in
  let body = G.Block (open_brace, decls, close_brace) |> G.s in
  let ent = { name = EN v2; attrs = []; tparams = None } in
  let mkind = G.ModuleStruct (None, [ body ]) in
  let def = { G.mbody = mkind } in
  G.DefStmt (ent, G.ModuleDef def) |> G.s

and type_declaration (env : env) (x : CST.type_declaration) : stmt =
  match x with
  | `Class_decl x -> class_declaration env x
  | `Inte_decl x -> interface_declaration env x
  | `Enum_decl x -> enum_declaration env x
  | `Record_decl x -> record_declaration env x
  | `Dele_decl x -> delegate_declaration env x
  | `Struct_decl x -> struct_declaration env x
  | `Ellips tok ->
      let tok = token env tok in
      ExprStmt (Ellipsis tok |> G.e, tok) |> G.s

(* The type parameters, primary constructor parameters and base list of a
 * class or a struct, which the grammar accepts in any order. *)
and class_parts (xs : CST.anon_choice_type_param_list_94781cc list) :
    CST.type_parameter_list option
    * CST.parameter_list option
    * CST.base_list option =
  List.fold_left
    (fun (tparams, params, bases) x ->
      match x with
      | `Type_param_list y -> (Some y, params, bases)
      | `Param_list y -> (tparams, Some y, bases)
      | `Base_list y -> (tparams, params, Some y))
    (None, None, None) xs

and class_declaration (env : env)
    (((v1, v2, v3, v4, v5, v6), v7) : CST.class_declaration) =
  let tparams, params, bases = class_parts v5 in
  class_struct env Class (v1, v2, [], v3, v4, tparams, params, bases, v6, v7)

and interface_declaration (env : env)
    (((v1, v2, v3, v4, v5, v6, v7), v8) : CST.interface_declaration) =
  class_struct env Interface (v1, v2, [], v3, v4, v5, None, v6, v7, v8)

and struct_declaration (env : env)
    (((v1, v2, v3, v4, v5, v6, v7), v8) : CST.struct_declaration) =
  let v3 =
    match v3 with
    | Some tok -> [ (* "ref" *) unhandled_keywordattr ("ref", token env tok) ]
    | None -> []
  in
  let tparams, params, bases = class_parts v6 in
  class_struct env Class (v1, v2, v3, v4, v5, tparams, params, bases, v7, v8)

and declaration_list_body (env : env) (x : CST.declaration_list_body) =
  match x with
  | `Decl_list_opt_semi (v1, v2) ->
      let _v2 = opt_semi env v2 in
      declaration_list env v1
  | `SEMI tok ->
      let t = token env tok (* ";" *) in
      (* no body: use a fake bracket around no fields *)
      (t, [], t)

(* NOTE: According to the docs
 * https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/tutorials/primary-constructors
 * primary constructors' parameters are equivalent to properties, while here
 * we translate them to fields. It is because properties are represented in
 * AST generic as a field with getter/setter functions. Here, we don't even have
 * tokens we could use for getter/setters, so they would never be matched anyway,
 * while a public field is sufficient for taint propagation.
 *)
and parameter_to_field (p : parameter) : stmt option =
  match p with
  | Param { pname = Some n; ptype; pdefault; pattrs; _ } ->
      let public = KeywordAttr (Public, Tok.unsafe_fake_tok "public") in
      let entity = basic_entity n ~attrs:(public :: pattrs) in
      let def = VarDef { vinit = pdefault; vtype = ptype; vtok = None } in
      Some (DefStmt (entity, def) |> s)
  | _ ->
      None

(* A class, a struct or an interface. The attributes [v3] follow the
 * modifiers: the `ref` of a ref struct. *)
and class_struct (env : env) (class_kind : G.class_kind)
    ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) :
      CST.attribute_list_ list
      * CST.modifier list
      * G.attribute list
      * Tree_sitter_run.Token.t
      * CST.identifier
      * CST.type_parameter_list option
      * CST.parameter_list option
      * CST.base_list option
      * CST.type_parameter_constraints_clause list
      * CST.declaration_list_body) =
  (*
   [Attr] public ref class MyClass<MyType>(args) : IClass where MyType : SomeType { ... };
      v1     v2   v3   v4    v5      v6      v7       v8           v9                 v10
   *)
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v4 = token env v4 (* "class" *) in
  let v5 = identifier env v5 (* identifier *) in
  let v6 = Option.map (type_parameter_list env) v6 in
  let primary_constr_args =
    match v7 with
    | None -> []
    | Some ps -> parameter_list env ps
                 |> Tok.unbracket
                 |> List.filter_map parameter_to_field
  in
  let v8 =
    match v8 with
    | Some x -> base_list env x
    | None -> []
  in
  let v9 = List.map (type_parameter_constraints_clause env) v9 in
  let open_bra, stmts, close_bra = declaration_list_body env v10 in
  let fields = List.map (fun x -> G.F x) (primary_constr_args @ stmts) in
  let tparams = type_parameters_with_constraints v6 v9 in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2 @ v3; tparams } in
  G.DefStmt
    ( ent,
      G.ClassDef
        {
          ckind = (class_kind, v4);
          cextends = v8;
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = (open_bra, fields, close_bra);
        } )
  |> G.s

and enum_declaration (env : env)
    (((v1, v2, v3, v4, v5), v6) : CST.enum_declaration) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List_.map (modifier env) v2 in
  let _v3TODO = token env v3 (* "enum" *) in
  let v4 = identifier env v4 (* identifier *) in
  let _v5TODO =
    match v5 with
    | Some x -> base_list env x
    | None -> []
  in
  let v6 =
    match v6 with
    | `Enum_member_decl_list_opt_semi (v1, v2) ->
        let _v2 = opt_semi env v2 (* ";" *) in
        enum_member_declaration_list env v1
    | `SEMI tok ->
        let _tok = token env tok (* ";" *) in
        []
  in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams = None } in
  G.DefStmt (ent, G.TypeDef { tbody = OrType v6 }) |> G.s

and delegate_declaration (env : env)
    (((v1, v2, v3, v4, v5, v6, v7), v8, v9) : CST.delegate_declaration) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List_.map (modifier env) v2 in
  let _v3 = token env v3 (* "delegate" *) in
  let v4 = type_pattern env v4 in
  let v5 = identifier env v5 (* identifier *) in
  let v6 = Option.map (type_parameter_list env) v6 in
  let _, params, _ = parameter_list env v7 in
  let v8 = List_.map (type_parameter_constraints_clause env) v8 in
  let _v9 = token env v9 (* ";" *) in
  let tparams = type_parameters_with_constraints v6 v8 in
  let func = TyFun (params, v4) |> G.t in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
  DefStmt (ent, TypeDef { tbody = NewType func }) |> G.s

(* Shared between `record class` and `record struct`. A record is modeled
 * as a ClassDef carrying the RecordClass keyword attribute. Positional
 * (primary constructor) parameters become fields, like for the primary
 * constructors of regular classes/structs. *)
and build_record_def env ~class_kind ~attrs_extra
    (v1, v2, record_tok, name, tparams_opt, params_opt, base_opt, constraints,
     body) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List_.map (modifier env) v2 in
  let record_tok = token env record_tok (* "record" *) in
  let name = identifier env name in
  let tparams_opt = Option.map (type_parameter_list env) tparams_opt in
  let primary_constr_args =
    match params_opt with
    | None -> []
    | Some ps ->
        parameter_list env ps |> Tok.unbracket
        |> List.filter_map parameter_to_field
  in
  let parents =
    match base_opt with
    | Some x -> record_base env x
    | None -> []
  in
  let constraints =
    List_.map (type_parameter_constraints_clause env) constraints
  in
  let lb, body_stmts, rb = declaration_list_body env body in
  let fields = List_.map (fun x -> G.F x) (primary_constr_args @ body_stmts) in
  let tparams = type_parameters_with_constraints tparams_opt constraints in
  let record_attr = KeywordAttr (RecordClass, record_tok) in
  let ent =
    {
      name = EN (Id (name, empty_id_info ()));
      attrs = v1 @ v2 @ (record_attr :: attrs_extra);
      tparams;
    }
  in
  G.DefStmt
    ( ent,
      G.ClassDef
        {
          ckind = (class_kind, record_tok);
          cextends = parents;
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = (lb, fields, rb);
        } )
  |> G.s

and record_declaration (env : env)
    (((v1, v2, v3, v4, v5, v6, v7, v8), v9) : CST.record_declaration) =
  (* v4 is the optional "class" or "struct" keyword; a record struct converts
     as a record class. *)
  let _v4 =
    Option.map
      (function
        | `Class tok (* "class" *)
        | `Struct tok (* "struct" *) ->
            token env tok)
      v4
  in
  let tparams, params =
    List.fold_left
      (fun (tparams, params) x ->
        match x with
        | `Type_param_list y -> (Some y, params)
        | `Param_list y -> (tparams, Some y))
      (None, None) v6
  in
  build_record_def env ~class_kind:Class ~attrs_extra:[]
    (v1, v2, v3, v5, tparams, params, v7, v8, v9)

and add_this_param ~(this_param : (G.tok -> G.parameter) option) ~(anchor : G.tok) (s : stmt) : stmt =
  match this_param, s.s with
  | Some tp, G.DefStmt (ent, FuncDef fdef) ->
      let (l, params, r) = fdef.fparams in
      let tp = tp anchor in
      let idents = List.filter_map H2.ident_of_parameter_opt params in
      (match H2.ident_of_parameter_opt tp with
      | Some i when List.mem (fst i) (List.map fst idents) ->
          (* don't insert the param: it would be shadowed anyway *)
          s
      | _ ->
          let params = tp :: params in
          let fdef = { fdef with fparams = (l, params, r) } in
          let res = G.DefStmt (ent, G.FuncDef fdef) |> G.s in
          let orig_range = H2.range_of_any_opt (G.S s) in
          (match orig_range with
          | Some _ -> res.s_range <- orig_range; res
          | None -> res))
  | _ -> s 
    
and fix_s_range (new_range : (Tok.location * Tok.location) option) (s : stmt) : stmt =
  (match new_range with
  | Some _ -> s.s_range <- new_range
  | _ -> ());
  s
  
and declaration ?(this_param=None) (env : env) (x : CST.declaration) : stmt =
  match x with
  | `Ellips v1 ->
      let v1 = token env v1 in
      G.ExprStmt (G.Ellipsis v1 |> G.e, sc) |> G.s
  | `Class_decl x -> class_declaration env x
  | `Dele_decl x -> delegate_declaration env x
  | `Enum_decl x -> enum_declaration env x
  | `Inte_decl x -> interface_declaration env x
  | `Record_decl x -> record_declaration env x
  | `Struct_decl x -> struct_declaration env x
  | `Cons_decl ((v1, v2, v3, v4, v5), v6) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = identifier env v3 (* identifier *) in
      let _, tok = v3 in
      let v4 = parameter_list env v4 in
      let v5 = Option.map (constructor_initializer env) v5 in
      let v6 = function_body env v6 in
      (* TODO? separate ctor initializer from body in G.function_definition?*)
      let fbody =
        match v5 with
        | Some init ->
            G.FBStmt (Block (fb [ init; H2.funcbody_to_stmt v6 ]) |> G.s)
        | None -> v6
      in
      let def =
        G.FuncDef
          { fkind = (G.Method, tok); fparams = v4; frettype = None; fbody }
      in
      let ctor = KeywordAttr (Ctor, tok) in
      let attrs = (ctor :: v1) @ v2 in
      let ent = basic_entity v3 ~attrs in
      G.DefStmt (ent, def) |> G.s
  | `Conv_op_decl (v1, v2, v3, v4, v7, v8, v9) ->
      (* https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/user-defined-conversion-operators *)
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 =
        match v3 with
        | `Impl tok -> ("op_Implicit", token env tok) (* "implicit" *)
        | `Expl tok -> ("op_Explicit", token env tok)
        (* "explicit" *)
      in
      (* The grammar accepts the interface specifier, `operator` and `checked`
       * in any number and order; the specification requires `operator`,
       * which is the declaration's keyword, and the conversion keyword stands
       * in for it when it is missing. *)
      let v5 =
        List.fold_left
          (fun keyword x ->
            match x with
            | `Op tok -> Some (token env tok) (* "operator" *)
            | `Expl_inte_spec x ->
                let _v4TODO = explicit_interface_specifier env x in
                keyword
            | `Chec tok ->
                let _v6TODO = (* "checked" *) token env tok in
                keyword)
          None v4
        |> Option.value ~default:(snd v3)
      in
      let v7 = type_pattern env v7 in
      let v8 = parameter_list env v8 in
      let v9 = function_body env v9 in
      let idinfo = empty_id_info () in
      let ent =
        { name = EN (Id (v3, idinfo)); attrs = v1 @ v2; tparams = None }
      in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, v5);
            fparams = v8;
            frettype = Some v7;
            fbody = v9;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Dest_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 =
        match v2 with
        | Some tok -> [ KeywordAttr (Extern, token env tok) ] (* "extern" *)
        | None -> []
      in
      let v3 = token env v3 (* "~" *) in
      let _v4TODO = identifier env v4 (* identifier *) in
      let v5 = parameter_list env v5 in
      let v6 = function_body env v6 in
      let name = ("Finalize", v3) in
      let def =
        G.FuncDef
          { fkind = (G.Method, v3); fparams = v5; frettype = None; fbody = v6 }
      in
      let dtor = KeywordAttr (Dtor, v3) in
      let ent = basic_entity name ~attrs:((dtor :: v1) @ v2) in
      G.DefStmt (ent, def) |> G.s
  | `Event_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let _v2TODO = List_.map (modifier env) v2 in
      let v3 = unhandled_keywordattr (str env v3) (* "event" *) in
      let v4 = type_pattern env v4 in
      let _v5TODO = Option.map (explicit_interface_specifier env) v5 in
      let id = identifier env v6 (* identifier *) in
      let fname, _ftok = id in
      let v7 =
        match v7 with
        | `Acce_list x ->
            let open_br, accs, close_br = accessor_list env x in
            let funcs =
              accs
              |> List_.map (fun (attrs, id, fbody) ->
                     let iname, itok = id in
                     (* CLR-style `get_<PropName>` / `set_<PropName>`;
                        the prefix-metavar support in m_ident (see
                        Generic_vs_generic.decompose_prefix_metavar)
                        lets patterns like `set_$P` match these. Mark
                        hidden so the prefilter regex doesn't demand
                        the literal mangled string. *)
                     let ent =
                       basic_entity ~hidden:true
                         (iname ^ "_" ^ fname, itok) ~attrs
                     in
                     let valparam =
                       Param
                         {
                           pname = Some ("value", fake "value");
                           ptype = Some v4;
                           pdefault = None;
                           pattrs = [];
                           (* C# setter's implicit `value` — the word
                              appears in source when the setter body uses
                              it. Eval_generic uses [is_hidden] to gate
                              `metavariable-regex`, so don't mark hidden. *)
                           pinfo = empty_id_info ();
                         }
                     in
                     let funcdef =
                       FuncDef
                         {
                           fkind = (Method, itok);
                           fparams = fb [ valparam ];
                           frettype = None;
                           fbody;
                         }
                     in
                     DefStmt (ent, funcdef) |> G.s)
            in
            (open_br, funcs, close_br)
        | `SEMI tok ->
            (* `event T Foo;` with no accessor block: just the field, no
               accessor methods *)
            let _tok = token env tok (* ";" *) in
            fb []
      in
      let ent = basic_entity id ~attrs:(v1 @ v1 @ [ v3 ]) in
      let vardef = { vinit = None; vtype = Some v4; vtok = G.no_sc } in
      let open_br, funcs, close_br = v7 in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> G.s) :: funcs, close_br)
      |> G.s
  | `Event_field_decl (v1, v2, v3, v4, v5) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = unhandled_keywordattr (str env v3) (* "event" *) in
      let attrs = (v3 :: v1) @ v2 in
      let vardefs = variable_declaration env v4 in
      let sc = token env v5 (* ";" *) in
      var_def_stmt attrs vardefs sc
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let attrs = v1 @ v2 in
      let vardefs = variable_declaration env v3 in
      let sc = token env v4 (* ";" *) in
      var_def_stmt attrs vardefs sc
  | `Inde_decl (v1, v2, v3, v4, v5, v6, v7) -> (
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let _v4TODO = Option.map (explicit_interface_specifier env) v4 in
      let _v5 = token env v5 (* "this" *) in
      let lbra, params, rbra = bracketed_parameter_list env v6 in
      let indexer_attrs = v1 @ v2 in
      match v7 with
      | `Acce_list x ->
          let open_br, accs, close_br = accessor_list env x in
          let funcs =
            accs
            |> List_.map (fun (attrs, id, fbody) ->
                   let iname, itok = id in
                   match iname with
                   | "get" ->
                       let ent = basic_entity ("get_Item", itok) ~attrs in
                       let funcdef =
                         FuncDef
                           {
                             fkind = (Method, itok);
                             fparams = (lbra, params, rbra);
                             frettype = Some v3;
                             fbody;
                           }
                       in
                       DefStmt (ent, funcdef) |> G.s
                   | "set" ->
                       let valparam =
                         Param
                           {
                             pname = Some ("value", fake "value");
                             ptype = Some v3;
                             pdefault = None;
                             pattrs = [];
                             pinfo = empty_id_info ();
                           }
                       in
                       let ent = basic_entity ("set_Item", itok) ~attrs in
                       let funcdef =
                         FuncDef
                           {
                             fkind = (Method, itok);
                             fparams = (lbra, params @ [ valparam ], rbra);
                             frettype = None;
                             fbody;
                           }
                       in
                       DefStmt (ent, funcdef) |> G.s
                   | _ -> raise Impossible)
          in
          Block (open_br, funcs, close_br) |> G.s
      | `Arrow_exp_clause_SEMI (v1, v2) ->
          let v1 = arrow_expression_clause env v1 in
          let v2 = token env v2 (* ";" *) in
          let arrow, expr = v1 in
          let fbody = G.FBStmt (ExprStmt (expr, v2) |> G.s) in
          let ent = basic_entity ("get_Item", arrow) ~attrs:indexer_attrs in
          let funcdef =
            FuncDef
              {
                fkind = (Arrow, arrow);
                fparams = (lbra, params, rbra);
                frettype = Some v3;
                fbody;
              }
          in
          DefStmt (ent, funcdef) |> G.s)
  | `Meth_decl (v1, v2, v3, _v4TODO, v5, v6, v7, v8, v9) ->
      (*
        [Attr] static int IList<T>.MyMethod<T>(int p1) where T : Iterator { ... }
          v1     v2   v3    v4        v5    v6  v7           v8              v9
      *)
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let v5 = identifier env v5 (* identifier *) in
      let _, tok = v5 in
      let v6 = Option.map (type_parameter_list env) v6 in
      let l, v7, r = parameter_list env v7 in
      let v8 = List_.map (type_parameter_constraints_clause env) v8 in
      let v9 = function_body env v9 in
      let tparams = type_parameters_with_constraints v6 v8 in
      let idinfo = empty_id_info () in
      let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, tok);
            fparams = (l, v7, r);
            frettype = Some v3;
            fbody = v9;
          }
      in
      G.DefStmt (ent, def)
      |> G.s
      |> add_this_param ~this_param ~anchor:(snd v5)
  | `Name_decl x -> namespace_declaration env x
  | `Op_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let _v4TODO =
        match v4 with
        | Some x -> Some (explicit_interface_specifier env x)
        | None -> None
      in
      let v5 = (* "operator" *) token env v5 in
      let _v6TODO =
        match v6 with
        | Some tok -> Some ((* "checked" *) token env tok)
        | None -> None
      in
      let v7 = overloadable_operator env v7 in
      let v8 = parameter_list env v8 in
      let v9 = function_body env v9 in
      (* TODO make clear that this is an operator overload, by using IdSpecial as the name, or adding a keyword attribute *)
      let idinfo = empty_id_info () in
      let ent =
        { name = EN (Id (v7, idinfo)); attrs = v1 @ v2; tparams = None }
      in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, v5);
            fparams = v8;
            frettype = Some v3;
            fbody = v9;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      (* [Attr] public string IFace.Field { get; public set { ... } } = "hello";
         [Attr] public string IFace.Field => "hello";
           v1     v2     v3    v4    v5      v6
         Map `Prop` as field. Map getter and setter as methods. *)
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List_.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let _v4TODO = Option.map (explicit_interface_specifier env) v4 in
      let v5 = identifier env v5 (* identifier *) in
      let fname, _ftok = v5 in
      (* The C#14 "field" keyword is mapped as an additional argument to
         get and set. The keyword `field` can appear in source when the
         accessor body uses it, so we don't mark pinfo hidden —
         Eval_generic uses [is_hidden] to gate `metavariable-regex`. *)
      let fieldParam tok =
        Param
          {
            pname = Some ("field", tok);
            ptype = Some v3;
            pdefault = Some (G.N (G.Id (v5, G.empty_id_info ())) |> G.e);
            pattrs = [G.KeywordAttr (G.Extern, fake "field")];
            pinfo = empty_id_info ~hidden:false ()
          }
      in
      let accessors, vinit =
        match v6 with
        | `Acce_list_opt_EQ_exp_SEMI (v1, v2) ->
            let v1 = accessor_list env v1 in
            let v2 =
              match v2 with
              | Some (v1, v2, v3) ->
                  let _v1 = token env v1 (* "=" *) in
                  let v2 = expression env v2 in
                  let _v3 = token env v3 (* ";" *) in
                  Some v2
              | None -> None
            in
            let open_br, v1, close_br = v1 in
            let funcs =
              List_.map
                (fun (attrs, id, fbody) ->
                  let iname, itok = id in
                  let has_params = iname <> "get" in
                  let has_return = iname = "get" in
                  let _property_attr =
                    if iname = "get"
                       then G.KeywordAttr (G.Getter, itok)
                       else G.KeywordAttr (G.Setter, itok)
                  in
                  let attrs = attrs in
                  (* CLR-style accessor name; see other accessor site
                     above for rationale. *)
                  let ent =
                    basic_entity ~hidden:true
                      (iname ^ "_" ^ fname, itok) ~attrs
                  in
                  let itok_loc = Tok.unsafe_loc_of_tok itok in
                  let new_loc loc n =
                      Tok.({
                      str = "";
                      pos =
                          {
                          loc.pos with
                          bytepos = loc.pos.bytepos + n;
                          column = loc.pos.column + n;
                          };
                      })
                  in
                  let this_token = 
                    Tok.OriginTok (new_loc itok_loc 0)
                  in
                  let value_token = 
                    Tok.OriginTok (new_loc itok_loc 1)
                  in
                  let field_token =
                    Tok.OriginTok (new_loc itok_loc 2)
                  in
                  let funcdef =
                    FuncDef
                      {
                        fkind = (Method, itok);
                        fparams =
                          fb
                            ((if has_params then
                               [
                                 Param
                                   {
                                     pname = Some ("value", value_token);
                                     ptype = Some v3;
                                     pdefault = None;
                                     pattrs = [];
                                     pinfo = empty_id_info ();
                                   };
                               ]
                             else []) @ [fieldParam field_token]);
                        (* NOTE: In order to make "set" be recognised by a $T $FOO(...) {...} pattern, *)
                        (* we need to give it the void type and point to a real token. *)
                        (* Synthetic `void` return — hidden to keep it out of the prefilter. *)
                        frettype = (if has_return then Some v3 else Some (G.TyN (Id (("void", itok), G.empty_id_info ~hidden:true ())) |> G.t));
                        (* TODO Should this be "void"? *)
                        fbody;
                      }
                  in
                  let fixed_range =
                    match Tok.loc_of_tok itok, H2.range_of_any_opt (H2.any_of_function_body fbody) with
                    | Ok l1, Some (_, l2) -> Some (l1, l2)
                    | _ -> None
                  in
                  DefStmt (ent, funcdef)
                  |> G.s
                  |> fix_s_range fixed_range
                  |> add_this_param ~this_param ~anchor:this_token)
                v1
            in
            ((open_br, funcs, close_br), v2)
        | `Arrow_exp_clause_SEMI (v1, v2) ->
            (* public int SomeProp => 3;
             * Convert it to `get_SomeProp { return 3; }` — CLR-style
             * accessor name; patterns use `set_$P` / `get_$P` and the
             * prefix-metavar support in m_ident binds the metavar.
             * Hidden so the prefilter regex doesn't demand the mangled
             * string literally. *)
            let v1 = arrow_expression_clause env v1 in
            let v2 = token env v2 (* ";" *) in
            let arrow, expr = v1 in
            let ent = basic_entity ~hidden:true ("get_" ^ fname, arrow) in
            let funcdef =
              FuncDef
                {
                  fkind = (Method, arrow);
                  fparams = fb [fieldParam (snd v5)] (* (Option.to_list (Option.map (fun f -> f ()) this_param)) *) ;
                  frettype = Some v3;
                  fbody = G.FBStmt (G.Block (fb [ExprStmt (expr, v2) |> G.s]) |> G.s);
                }
            in
            let func =
              DefStmt (ent, funcdef)
              |> G.s
              |> add_this_param ~this_param ~anchor:(snd v5)
            in
            ((arrow, [ func ], v2), None)
      in
      let ent = basic_entity v5 ~attrs:(v1 @ v2) in
      let vardef = { vinit; vtype = Some v3; vtok = G.no_sc } in
      let open_br, funcs, close_br = accessors in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> G.s) :: funcs, close_br)
      |> G.s
  | `Using_dire x -> using_directive env x
  | `Exte_decl (v1, v2, v3, v4, (_v5TODO, v5mods, v5type, v5name), v6, v7, v8)
    ->
      let _v1TODO = List.concat_map (attribute_list_ env) v1 in
      let _v2 = (* "extension" *) token env v2 in
      let v3 = Option.map (type_parameter_list env) v3 in
      let _v4 = (* "(" *) token env v4 in
      let typ, _attrs =
        parameter_type_with_modifiers env (v5mods, v5type)
      in
      let this_param =
        match v5name with
        | Some v5name ->
            let ident = identifier env v5name in
            Some (fun tok -> G.Param
              {pname = Some (fst ident, tok);
               ptype = Some typ;
               pdefault = None;
               pattrs = [ G.KeywordAttr (G.Extern, fake "this") ];
               pinfo = G.empty_id_info ~hidden:false ()
              })
        | _ -> None
      in
      let _v6 = (* ")" *) token env v6 in
      let v7 = List_.map (type_parameter_constraints_clause env) v7 in
      let tparams = type_parameters_with_constraints v3 v7 in
      let lb, members, rb, _semi = v8 in
      let members =
        List_.map
          (fun x ->
            extension_member_declaration ~this_param env x
            |> add_extension_tparams tparams)
          members
      in
      Block (token env lb, members, token env rb) |> G.s

and extension_member_declaration ?(this_param = None) (env : env)
    (x : CST.extension_member_declaration) : stmt =
  match x with
  | `Meth_decl x -> declaration ~this_param env (`Meth_decl x)
  | `Prop_decl x -> declaration ~this_param env (`Prop_decl x)
  | `Op_decl x -> declaration ~this_param env (`Op_decl x)
  | `Ellips tok -> declaration ~this_param env (`Ellips tok)

(* The type parameters of an extension block are type parameters of each
 * function among its members, which the compiler declares as generic
 * methods. A property converts to a block holding its accessors. *)
and add_extension_tparams (tparams : G.type_parameters option) (s : stmt) :
    stmt =
  match (tparams, s.s) with
  | None, _ -> s
  | Some (lt, xs, gt), G.DefStmt (ent, (G.FuncDef _ as def)) ->
      let tparams =
        match ent.tparams with
        | None -> Some (lt, xs, gt)
        | Some (lt', ys, gt') -> Some (lt', xs @ ys, gt')
      in
      { s with s = G.DefStmt ({ ent with tparams }, def) }
  | Some _, G.Block (lb, ss, rb) ->
      { s with s = G.Block (lb, List_.map (add_extension_tparams tparams) ss, rb) }
  | Some _, _ -> s

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_c_sharp.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match compilation_unit env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern_aux str =
  (* ugly: coupling: see grammar.js of csharp.
   * todo: will need to adjust position information in parsing errors! *)
  let expr_str = "__SEMGREP_EXPRESSION " ^ str in
  (* If possible, we always prefer to parse a pattern as an expression than
   * as a program, since an expression is also a statement, but a statement
   * is not an expression! E.g., `Foo()` as an statement will not match
   * `if (null == Foo()) ...` whereas as an expression it does. *)
  let res = Tree_sitter_c_sharp.Parse.string expr_str in
  match res.errors with
  | [] -> res
  | _ -> Tree_sitter_c_sharp.Parse.string str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_pattern_aux str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      compilation_unit env cst)
