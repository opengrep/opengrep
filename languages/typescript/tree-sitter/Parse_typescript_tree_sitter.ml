(**
   Derive a JS/TS AST from a tree-sitter TS (or TSX) CST.

   This is derived from generated code 'semgrep-typescript/lib/Boilerplate.ml'
   in tree-sitter-lang.
*)

open Common
open Fpath_.Operators
open Either_
module AST = Ast_js
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers
open Ast_js

(*
   Development notes

   - Try to change the structure of this file as little as possible,
     since it's derived from generated code and we'll have to merge
     updates as the grammar changes.
   - Typescript is a superset of Javascript.
   - We started by ignoring typescript-specific constructs and mapping
     the rest to a Javascript AST.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The source text, from which JSX text is cut as written, and the byte
 * offset at which each of its lines starts. *)
type source = { text : string; line_starts : int array Lazy.t }
type annotations = [ `Flow | `Typescript ]
type context = { source : source; annotations : annotations }
type env = context H.env

let source_of_string (text : string) : source =
  let rec line_starts (i : int) (acc : int list) : int list =
    match String.index_from_opt text i '\n' with
    | Some j -> line_starts (j + 1) ((j + 1) :: acc)
    | None -> acc
  in
  {
    text;
    line_starts = lazy (line_starts 0 [ 0 ] |> List.rev |> Array.of_list);
  }

(* The characters that the scanner skips as whitespace in JSX text *)
let is_jsx_whitespace (c : char) : bool =
  match c with
  | ' '
  | '\t'
  | '\n'
  | '\011'
  | '\012'
  | '\r' ->
      true
  | _ -> false

(* The JSX text from the start of [first] to the end of [last], with the
 * whitespace before and after them *)
let jsx_text_token (source : source) (first : Tree_sitter_run.Loc.t)
    (last : Tree_sitter_run.Loc.t) : Tree_sitter_run.Token.t =
  let text = source.text and line_starts = Lazy.force source.line_starts in
  let offset (pos : Tree_sitter_run.Loc.pos) : int =
    line_starts.(pos.row) + pos.column
  in
  let rec back (i : int) : int =
    if i > 0 && is_jsx_whitespace text.[i - 1] then back (i - 1) else i
  in
  let rec forward (i : int) : int =
    if i < String.length text && is_jsx_whitespace text.[i] then forward (i + 1)
    else i
  in
  let newlines (i : int) (j : int) : int =
    String.fold_left
      (fun (n : int) (c : char) -> if Char.equal c '\n' then n + 1 else n)
      0
      (String.sub text i (j - i))
  in
  let pos (row : int) (i : int) : Tree_sitter_run.Loc.pos =
    { row; column = i - line_starts.(row) }
  in
  let first_start = offset first.start and last_end = offset last.end_ in
  let start = back first_start and end_ = forward last_end in
  ( {
      start = pos (first.start.row - newlines start first_start) start;
      end_ = pos (last.end_.row + newlines last_end end_) end_;
    },
    String.sub text start (end_ - start) )

let token = H.token
let str = H.str
let fake = Tok.unsafe_fake_tok ""
let fb = Tok.unsafe_fake_bracket
let mk_functype ((_, params, _), rett) = TyFun (params, rett)

(* Note that this file also raises some Impossible and Ast_builder_error *)
let _todo _env _x = failwith "internal error: not implemented"

(*
   We preserve the distinction between a plain identifier and a more complex
   pattern because function parameters make this distinction.
   This conversion is intended to create a sub-pattern.
*)
let sub_pattern (id_or_pat : (a_ident, a_pattern) Either.t) : a_pattern =
  match id_or_pat with
  | Left id -> Id id
  | Right pat -> pat

let optional env opt f =
  match opt with
  | None -> None
  | Some x -> Some (f env x)

(* tree-sitter-typescript is now very laxist in what it accepts after
 * an 'extends' for classes and allow now any expression, even though
 * most expressions have the form foo.t in which case it's really a TyName.
 * For interfaces, typescript uses 'extends_type_clause' which
 * is simpler and restrict the 'extends' to be a type, but for regular
 * classes it uses this 'extends_clause' which is very laxist.
 * The function below tries to reverse-engineer a type from an expr
 * to match what we do in parser_js.mly.
 *)
let tyname_or_expr_of_expr e _targsTODO =
  let rec ids_of_expr = function
    | Id id -> [ id ]
    (* THINK: Probably shouldn't have a question mark here. *)
    | ObjAccess (e, (Dot, _), PN id) -> id :: ids_of_expr e
    | _ -> raise Not_found
  in
  try
    let ids = ids_of_expr e |> List.rev in
    Right (TyName ids)
  with
  | Not_found -> Left e

(*
   Map the comma-separated representation of a list to an ocaml list.
   The separator doesn't have to be a comma but must be a simple token.

   This is used usually where the commaSep function was used as a macro
   in the original grammar.js.

   Usage:

     map_sep_list env v1 v2 (fun env x ->
       ...
     )
*)
let map_sep_list (env : env) (head : 'a) (tail : (_ * 'a) list)
    (f : env -> 'a -> 'b) : 'b list =
  let head = f env head in
  let tail =
    List_.map (fun ((_sep : Tree_sitter_run.Token.t), elt) -> f env elt) tail
  in
  head :: tail

(* The slots of an array or array pattern: an empty one is a hole, except the
 * last, which is a trailing comma. *)
let array_slots (f : 'a -> expr) (head : 'a option) (tail : (_ * 'a option) list)
    : expr list =
  let slots = head :: List_.map snd tail in
  let slots =
    match List.rev slots with
    | None :: rest -> List.rev rest
    | _ -> slots
  in
  List_.map
    (function
      | Some x -> f x
      | None -> PatternHole)
    slots

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

module CST = CST_tree_sitter_typescript (* typescript+tsx, merged *)

let identifier (env : env) (tok : CST.identifier) : a_ident = str env tok

let identifier_ (env : env) (x : CST.identifier_) : expr =
  match x with
  | `Unde tok -> IdSpecial (Undefined, token env tok)
  | `Id tok -> identifier env tok |> idexp_or_special

(* LATER: this is overriden by another automatic_semicolon later, normal? *)
let automatic_semicolon (_env : env) (_tok : CST.automatic_semicolon) =
  (* do like in pfff: *)
  Tok.unsafe_fake_tok ";"

let semicolon (env : env) (x : CST.semicolon) =
  match x with
  | `Auto_semi tok -> automatic_semicolon env tok (* automatic_semicolon *)
  | `SEMI tok -> (* ";" *) token env tok

let this env tok = IdSpecial (This, token env tok)
let super env tok = IdSpecial (Super, token env tok)

let number (env : env) (tok : CST.number) =
  let s, t = str env tok (* number *) in
  (* TODO? float_of_string_opt_also_from_hexoctbin *)
  (Common2.float_of_string_opt s, t)

let empty_stmt env tok =
  let t = token env tok in
  Block (t, [], t)

let number_as_string (env : env) (tok : CST.number) =
  let _, s = tok in
  let opt_num, t = number env tok in
  let num_str =
    match opt_num with
    | None -> s
    | Some n -> string_of_float n
  in
  (num_str, t)

(* A string literal: its fragments joined, between its quotes *)
let quoted_string (open_ : tok) (fragments : string wrap list) (close : tok) :
    string wrap =
  let s = fragments |> List_.map fst |> String.concat "" in
  (s, Tok.combine_toks open_ ((fragments |> List_.map snd) @ [ close ]))

let string_ (env : env) (x : CST.string_) : string wrap =
  match x with
  | `DQUOT_rep_choice_unes_double_str_frag_DQUOT (v1, v2, v3) ->
      let open_ = token env v1 (* "\"" *) in
      let contents =
        List_.map
          (fun x ->
            match x with
            | `Unes_double_str_frag tok ->
                str env tok (* pattern "[^\"\\\\]+" *)
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let close = token env v3 (* "\"" *) in
      quoted_string open_ contents close
  | `SQUOT_rep_choice_unes_single_str_frag_SQUOT (v1, v2, v3) ->
      let open_ = token env v1 (* "'" *) in
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Unes_single_str_frag tok -> str env tok (* pattern "[^'\\\\]+" *)
            | `Esc_seq tok -> str env tok
            (* escape_sequence *))
          v2
      in
      let close = token env v3 (* "'" *) in
      quoted_string open_ v2 close

let namespace_import (env : env) ((v1, v2, v3) : CST.namespace_import) =
  let star = token env v1 (* "*" *) in
  let _as = token env v2 (* "as" *) in
  let id = identifier env v3 (* identifier *) in
  (star, id)

(* A string export name (`export { x as "a-b" }`) carries its contents. *)
let module_export_name (env : env) (x : CST.module_export_name) : a_ident =
  match x with
  | `Id tok -> identifier env tok
  | `Str x -> string_ env x

let namespace_export (env : env) ((v1, v2, v3) : CST.namespace_export) =
  let star = token env v1 (* "*" *) in
  let _as = token env v2 (* "as" *) in
  let id = module_export_name env v3 in
  (star, id)

let import_identifier (env : env) (x : CST.import_identifier) : a_ident =
  match x with
  | `Id tok
  | `Type tok ->
      identifier env tok

let jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  match x with
  | `Jsx_id tok ->
      str env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
  | `Id tok -> identifier env tok

let jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = jsx_identifier_ env v1 in
  let _v2 = token env v2 (* ":" *) in
  let v3 = jsx_identifier_ env v3 in
  (v1, v3)

let jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  match x with
  | `Choice_jsx_id x -> jsx_identifier_ env x
  | `Jsx_name_name x ->
      let id1, id2 = jsx_namespace_name env x in
      let str = fst id1 ^ ":" ^ fst id2 in
      (str, Tok.combine_toks (snd id1) [ snd id2 ])

let rec id_or_nested_id (env : env) (x : CST.anon_choice_type_id_42c0412) :
    a_ident list =
  match x with
  | `Id tok -> [ identifier env tok ] (* identifier *)
  | `Nested_id x -> nested_identifier env x

and nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = id_or_nested_id env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = identifier env v3 (* identifier *) in
  v1 @ [ v3 ]

let jsx_element_name (env : env) (x : CST.jsx_element_name) : a_ident =
  match x with
  | `Choice_jsx_id x -> jsx_identifier_ env x
  | `Nested_id x ->
      let xs = nested_identifier env x in
      let str = xs |> List_.map fst |> String.concat "." in
      let hd, tl =
        match xs with
        | [] -> raise Impossible
        | x :: xs -> (x, xs)
      in
      (str, Tok.combine_toks (snd hd) (tl |> List_.map snd))
  | `Jsx_name_name x ->
      let id1, id2 = jsx_namespace_name env x in
      let str = fst id1 ^ ":" ^ fst id2 in
      (str, Tok.combine_toks (snd id1) [ snd id2 ])

(* The closing tag carries no name in a fragment (`</>`). *)
let jsx_closing_element (env : env) ((v1, v2, v3) : CST.jsx_closing_element) :
    tok =
  let v1 = token env v1 (* "</" *) in
  let v2 =
    match v2 with
    | Some x -> [ snd (jsx_element_name env x) ]
    | None -> []
  in
  let v3 = token env v3 (* ">" *) in
  Tok.combine_toks v1 (v2 @ [ v3 ])

let from_clause (env : env) ((v1, v2) : CST.from_clause) : tok * string wrap =
  let v1 = token env v1 (* "from" *) in
  let v2 = string_ env v2 in
  (v1, v2)

let accessibility_modifier (env : env) (x : CST.accessibility_modifier) =
  match x with
  | `Public tok -> (Public, token env tok) (* "public" *)
  | `Priv tok -> (Private, token env tok) (* "private" *)
  | `Prot tok -> (* "protected" *) (Protected, token env tok)

let flow_variance (env : env) (x : CST.flow_variance) : tok =
  match x with
  | `PLUS tok (* "+" *)
  | `DASH tok (* "-" *) ->
      token env tok

let accessibility_modifier_opt_to_list env v =
  match v with
  | Some x -> [ accessibility_modifier env x ]
  | None -> []

let kwd_attr_opt_to_list env kwd v =
  match v with
  | Some tok -> [ (kwd, token env tok) ]
  | None -> []

let predefined_type (env : env) (x : CST.predefined_type) : a_ident =
  match x with
  | `Any tok
  | `Num tok
  | `Bool tok
  | `Str tok
  | `Symb tok
  | `Void tok
  | `Obj tok
  | `Never tok
  | `Unkn tok ->
      identifier env tok
  | `Unique_symb (v1, v2) ->
      let unique, t1 = identifier env v1 (* "unique" *) in
      let symbol, t2 = identifier env v2 (* "symbol" *) in
      (unique ^ " " ^ symbol, Tok.combine_toks t1 [ t2 ])

let anon_choice_PLUSPLUS_e498e28 (env : env)
    (x : CST.anon_choice_PLUSPLUS_e498e28) =
  match x with
  | `PLUSPLUS tok -> (G.Incr, token env tok) (* "++" *)
  | `DASHDASH tok -> (* "--" *) (G.Decr, token env tok)

let map_anon_choice_DOT_d88d0af (env : env)
    (x :
      [ `DOT of Tree_sitter_run.Token.t
      | `QMARKDOT of Tree_sitter_run.Token.t ]) =
  match x with
  | `DOT tok -> (* "." *) token env tok
  (* TODO: return something different *)
  | `QMARKDOT tok -> (* "?." *) token env tok

let map_anon_choice_priv_prop_id_89abb74 (env : env)
    (x : CST.anon_choice_priv_prop_id_89abb74) : a_ident =
  match x with
  | `Priv_prop_id tok -> (* private_property_identifier *) str env tok
  | `Id tok -> (* identifier *) str env tok

let type_or_typeof (env : env) (x : CST.anon_choice_type_2b11f6b) =
  match x with
  | `Type tok -> token env tok (* "type" *)
  | `Typeof tok -> (* "typeof" *) token env tok

let automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  token env tok

let automatic_semicolon_opt env v =
  match v with
  | Some tok -> Some (automatic_semicolon env tok)
  | None -> None

let anon_choice_get_8fb02de (env : env) (x : CST.anon_choice_get_8fb02de) =
  match x with
  | `Get tok -> (Get, token env tok) (* "get" *)
  | `Set tok -> (Set, token env tok) (* "set" *)
  | `STAR tok -> (* "*" *) (Generator, token env tok)

let reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Decl tok
  | `Name tok
  | `Type tok
  | `Public tok
  | `Priv tok
  | `Prot tok
  | `Over tok
  | `Read tok
  | `Module tok
  | `Any tok
  | `Num tok
  | `Bool tok
  | `Str tok
  | `Symb tok
  | `Export tok
  | `Obj tok
  | `Get tok
  | `Set tok
  | `Async tok
  | `Static tok
  | `Acce tok
  | `Abst tok
  | `Using tok
  | `Opaque tok
  | `Comp tok
  | `Hook tok
  | `Unique tok ->
      identifier env tok

let anon_choice_COMMA_5194cb4 (env : env) (x : CST.anon_choice_COMMA_5194cb4) =
  match x with
  | `COMMA tok -> token env tok (* "," *)
  | `Choice_auto_semi x -> semicolon env x

(* Note: We now include type-only imports in the AST so that pattern matching
   with ellipsis works correctly.
   For example `import { type Foo }` will match `import {...}`
   Previously these were skipped with a TODO comment. *)
let specifier_binding (env : env) (v1 : CST.anon_choice_type_2b11f6b option) :
    import_binding =
  match v1 with
  | None -> Binds_value
  | Some x ->
      let _tok = type_or_typeof env x in
      Binds_type

let import_specifier (env : env) (x : CST.import_specifier) :
    import_binding * (a_ident * a_ident option) =
  match x with
  | `Opt_choice_type_choice_import_id (v1, v2) -> (
      let binding = specifier_binding env v1 in
      match v2 with
      | `Import_id x -> (binding, (import_identifier env x, None))
      | `Choice_module_export_name_as_import_id (v1, _as_tok, v3) ->
          let name =
            match v1 with
            | `Module_export_name x -> module_export_name env x
            | `Type tok -> identifier env tok (* "type" *)
          in
          (binding, (name, Some (import_identifier env v3))))
  | `Choice_type_as_opt_as_choice_import_id (v1, v2, v3) ->
      (* `type as`, `type as as x` *)
      let binding = specifier_binding env (Some v1) in
      let name = identifier env v2 (* "as" *) in
      let alias =
        match v3 with
        | None -> None
        | Some (_as_tok, `Import_id x) -> Some (import_identifier env x)
        | Some (_as_tok, `As tok) -> Some (identifier env tok (* "as" *))
      in
      (binding, (name, alias))
  | `Type_as_as (v1, _as_tok, v3) ->
      (* `type as as`: the export `type` under the name `as` *)
      ( Binds_value,
        (identifier env v1 (* "type" *), Some (identifier env v3 (* "as" *))) )

let export_specifier (env : env) (x : CST.export_specifier) :
    import_binding * (a_ident * a_ident option) =
  let alias (x : (_ * CST.anon_choice_module_export_name_004ab2c) option) :
      a_ident option =
    match x with
    | None -> None
    | Some (_as_tok, `Module_export_name x) -> Some (module_export_name env x)
    | Some (_as_tok, `As tok) -> Some (identifier env tok (* "as" *))
  in
  match x with
  | `Opt_choice_type_module_export_name_opt_as_module_export_name (v1, v2, v3)
    ->
      let binding = specifier_binding env v1 in
      let opt_as_id =
        match v3 with
        | None -> None
        | Some (_as_tok, x) -> Some (module_export_name env x)
      in
      (binding, (module_export_name env v2, opt_as_id))
  | `Type_opt_as_choice_module_export_name (v1, v2) ->
      (* `type`, `type as x`: the binding `type` *)
      (Binds_value, (identifier env v1 (* "type" *), alias v2))
  | `Choice_type_as_opt_as_choice_module_export_name (v1, v2, v3) ->
      (* `type as`, `type as as x`: the binding `as` as a type *)
      (specifier_binding env (Some v1), (identifier env v2 (* "as" *), alias v3))

let concat_nested_identifier (idents : a_ident list) : a_ident =
  let str = idents |> List_.map fst |> String.concat "." in
  let tokens = List_.map snd idents in
  let x, xs =
    match tokens with
    | [] -> assert false
    | x :: xs -> (x, xs)
  in
  (str, Tok.combine_toks x xs)

(* 'import id = require(...)' are Commonjs-style import.
 * See https://www.typescriptlang.org/docs/handbook/2/modules.html#commonjs-style-import-and-export- for reference.
 * We translate them in regular typescript import.
 *  example:
 *      import zip = require("./ZipCodeValidator");
 *   => import * as zip from "./ZipCodeValidator"
 *
 *)
let import_require_clause tk (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.import_require_clause) =
  let v1 = identifier env v1 (* identifier *) in
  let _v2 = token env v2 (* "=" *) in
  let _v3 = identifier env v3 (* "require" *) in
  let _v4 = token env v4 (* "(" *) in
  let v5 = string_ env v5 in
  let _v6 = token env v6 (* ")" *) in
  ModuleAlias (tk, Binds_value, v1, v5, None)

let literal_type (env : env) (x : CST.literal_type) : expr =
  match x with
  | `Num_ (v1, v2) ->
      let s, t1 =
        match v1 with
        | `DASH tok -> str env tok (* "-" *)
        | `PLUS tok -> str env tok (* "+" *)
      in
      let s2, t2 = str env v2 (* number *) in
      (* TODO: float_of_string_opt_also_from_hexoctbin *)
      L (Num (float_of_string_opt (s ^ s2), Tok.combine_toks t1 [ t2 ]))
  | `Num tok ->
      let n = number env tok in
      L (Num n)
  | `Str x -> L (String (string_ env x))
  | `True tok -> L (Bool (true, token env tok (* "true" *)))
  | `False tok -> L (Bool (false, token env tok (* "false" *)))
  | `Null tok -> IdSpecial (Null, token env tok)
  | `Unde tok -> IdSpecial (Undefined, token env tok)

let rec type_keyword_nested_identifier (env : env)
    ((v1, v2, v3) : CST.type_keyword_nested_identifier) : a_ident list =
  let v1 =
    match v1 with
    | `Choice_comp (`Comp tok | `Unique tok | `Obj tok) ->
        [ identifier env tok ]
    | `Type_kw_nested_id x -> type_keyword_nested_identifier env x
  in
  let _v2 = token env v2 (* "." *) in
  let v3 = identifier env v3 (* identifier *) in
  v1 @ [ v3 ]

let nested_type_identifier (env : env)
    ((v1, v2, v3) : CST.nested_type_identifier) : a_ident list =
  let v1 =
    match v1 with
    | (`Id _ | `Nested_id _) as x -> id_or_nested_id env x
    | `Choice_comp (`Comp tok | `Unique tok | `Obj tok) ->
        [ identifier env tok ]
    | `Type_kw_nested_id x -> type_keyword_nested_identifier env x
  in
  let _v2 = token env v2 (* "." *) in
  let v3 = str env v3 (* identifier *) in
  v1 @ [ v3 ]

let id_or_reserved_id (env : env)
    (x :
      [ `Id of Tree_sitter_run.Token.t
      | `Choice_decl of CST.reserved_identifier ]) : a_ident =
  match x with
  | `Id tok -> identifier env tok (* identifier *)
  | `Choice_decl x -> reserved_identifier env x


let by_binding ~(binding : import_binding)
    (specifiers : (import_binding * (a_ident * a_ident option)) list)
    (of_group : import_binding -> (a_ident * a_ident option) list -> 'a) :
    'a list =
  let narrow (spec_binding : import_binding) : import_binding =
    match (binding, spec_binding) with
    | Binds_type, _
    | _, Binds_type ->
        Binds_type
    | Binds_value, Binds_value -> Binds_value
  in
  match specifiers with
  | [] -> [ of_group binding [] ]
  | _ :: _ ->
      let values, types =
        List.partition
          (fun ((spec_binding : import_binding), _) ->
            match narrow spec_binding with
            | Binds_value -> true
            | Binds_type -> false)
          specifiers
      in
      let group (kind : import_binding)
          (group_specs : (import_binding * (a_ident * a_ident option)) list) :
          'a list =
        match group_specs with
        | [] -> []
        | _ :: _ -> [ of_group kind (List_.map snd group_specs) ]
      in
      group Binds_value values @ group Binds_type types

let export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let _open = token env v1 (* "{" *) in
  let xs =
    match v2 with
    | Some (v1, v2) -> map_sep_list env v1 v2 export_specifier
    | None -> []
  in
  let _trailing_comma =
    match v3 with
    | Some _tok -> (* "," *) ()
    | None -> ()
  in
  let _close = token env v4 (* "}" *) in
  xs

let named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let _open = token env v1 (* "{" *) in
  let imports =
    match v2 with
    | Some (v1, v2) -> map_sep_list env v1 v2 import_specifier
    | None -> []
  in
  let _trailing_comma =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _close = token env v4 (* "}" *) in
  fun
    ~(binding : import_binding)
    ~(attributes : import_attributes option)
    (import_tok : tok)
    (from_path : a_filename)
  ->
    by_binding ~binding imports (fun (kind : import_binding) names ->
        Import (import_tok, Import_binds, kind, names, from_path, attributes))

let namespace_import_clause (env : env) (x : CST.namespace_import) =
  let _star, id = namespace_import env x in
  fun
    ~(binding : import_binding)
    ~(attributes : import_attributes option)
    (import_tok : tok)
    (from_path : a_filename)
  -> [ ModuleAlias (import_tok, binding, id, from_path, attributes) ]

let import_clause (env : env) (x : CST.import_clause) =
  match x with
  | `Name_import x -> namespace_import_clause env x
  | `Named_imports x -> named_imports env x
  | `Import_id_opt_COMMA_choice_name_import (v1, v2) ->
      let v1 = import_identifier env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 =
              match v2 with
              | `Name_import x -> namespace_import_clause env x
              | `Named_imports x -> named_imports env x
            in
            v2
        | None ->
            fun ~binding:_ ~attributes:_ (_import_tok : tok)
                (_from_path : a_filename) -> []
      in
      fun ~(binding : import_binding) ~(attributes : import_attributes option)
          (t : tok) (path : a_filename) ->
        let default =
          Import
            ( t,
              Import_binds,
              binding,
              [ ((default_entity, snd v1), Some v1) ],
              path,
              attributes )
        in
        default :: v2 ~binding ~attributes t path

(* `export d`: each definition of `d`, exported under its own name *)
let exported_definitions (export_tok : tok) (defs : definition list) :
    stmt list =
  defs
  |> List.concat_map (fun (((ent, _) as def) : definition) ->
         [ DefStmt def; M (Export (export_tok, ent.name)) ])

(* `export default d` *)
let default_exported_definitions (export_tok : tok) (default_tok : tok)
    (defs : definition list) : stmt list =
  defs
  |> List.concat_map (fun (((ent, _) as def) : definition) ->
         let default_decl, default_name =
           let expr = idexp ent.name in
           Ast_js.mk_default_entity_def default_tok expr
         in

         (*
             We translate into 3 statements:

               export default const foo = bar

             -->

               const foo = bar
               const default = foo
               export default
          *)
         [
           DefStmt def; DefStmt default_decl; M (Export (export_tok, default_name));
         ])

let rec decorator_member_expression (env : env)
    ((v1, v2, v3) : CST.decorator_member_expression) : a_ident list =
  let v1 = anon_choice_type_id_b8f8ced env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = identifier env v3 (* identifier *) in
  v1 @ [ v3 ]

and anon_choice_type_id_b8f8ced (env : env)
    (x : CST.anon_choice_type_id_b8f8ced) : a_ident list =
  match x with
  | `Id x -> [ identifier env x ]
  | `Deco_member_exp x -> decorator_member_expression env x

let rec parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | `Exp_opt_type_anno (v1, v2) -> (
        let v1 = expression env v1 in
        match v2 with
        | Some x ->
            let tok, ty = type_annotation env x in
            Cast (v1, tok, ty)
        | None -> v1)
    | `Seq_exp x -> sequence_expression env x
  in
  let v3 = token env v3 (* ")" *) in
  ParenExpr (v1, v2, v3)

(* The tag and attributes of an opening or self-closing element; a fragment
 * (`<>`) has none. *)
and jsx_tag_and_attributes (env : env)
    (x : CST.anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0) :
    (a_ident * xml_attribute list) option =
  match x with
  | None -> None
  | Some (v1, v2) ->
      let tag =
        match v1 with
        | `Choice_choice_jsx_id x -> jsx_attribute_name env x
        | `Choice_id_opt_type_args (v1, v2) ->
            let ids = id_or_nested_id env v1 in
            let id = concat_nested_identifier ids in
            (* TODO:
               let v2 = type_arguments env v2 |> PI.unbracket
                 |> Common.map (fun x -> G.TypeArg x) in
                H2.name_of_ids ~name_typeargs:(Some v2) v1
            *)
            let _v2TODO =
              match v2 with
              | Some x -> type_arguments env x |> Tok.unbracket
              | None -> []
            in
            id
      in
      Some (tag, List_.map (jsx_attribute_ env) v2)

and jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) :
    expr option bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x ->
        Some
          (match x with
          | `Exp x -> expression env x
          | `Seq_exp x -> sequence_expression env x
          | `Spread_elem x ->
              let t, e = spread_element env x in
              Apply (IdSpecial (Spread, t), fb [ e ]))
    (* abusing { } in XML to just add comments, e.g. { /* lint-ignore */ } *)
    | None -> None
  in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and jsx_attribute_ (env : env) (x : CST.jsx_attribute_) : xml_attribute =
  match x with
  | `Jsx_attr (v1, v2) ->
      let v1 = jsx_attribute_name env v1 in
      let teq, v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1bis = token env v1 (* "=" *) in
            let v2 = jsx_attribute_value env v2 in
            (v1bis, v2)
        (* see https://www.reactenlightenment.com/react-jsx/5.7.html *)
        | None -> (snd v1, L (Bool (true, snd v1)))
      in
      XmlAttr (v1, teq, v2)
  (* less: we could enforce that it's only a Spread operation *)
  | `Jsx_exp x ->
      let x = jsx_expression_some env x in
      XmlAttrExpr x

and jsx_expression_some env x =
  let t1, eopt, t2 = jsx_expression env x in
  match eopt with
  | None ->
      raise
        (Parsing_error.Ast_builder_error
           ("jsx_expression_some got a None expr", t1))
  | Some e -> (t1, e, t2)

and jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  match x with
  | `Jsx_str x -> L (String (jsx_string env x))
  | `Jsx_exp x ->
      let _, e, _ = jsx_expression_some env x in
      e
  (* an attribute value can be a jsx element? *)
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      Xml xml

(* JSX strings have no escape sequences; character references are kept as
 * written, as in JSX text. *)
and jsx_string (env : env) (x : CST.jsx_string) : string wrap =
  let fragment
      (x :
        [< `Unes_double_jsx_str_frag of CST.unescaped_double_jsx_string_fragment
        | `Unes_single_jsx_str_frag of CST.unescaped_single_jsx_string_fragment
        | `Html_char_ref of CST.html_character_reference ]) : string wrap =
    match x with
    | `Unes_double_jsx_str_frag tok
    | `Unes_single_jsx_str_frag tok
    | `Html_char_ref tok ->
        str env tok
  in
  let open_, fragments, close =
    match x with
    | `DQUOT_rep_choice_unes_double_jsx_str_frag_DQUOT (v1, v2, v3) ->
        (v1, List_.map fragment v2, v3)
    | `SQUOT_rep_choice_unes_single_jsx_str_frag_SQUOT (v1, v2, v3) ->
        (v1, List_.map fragment v2, v3)
  in
  quoted_string (token env open_) fragments (token env close)

(* The grammar splits text at every `&` (`a &amp; b`, `a & b`), and the
 * scanner skips whitespace that starts with a line break after such a piece.
 * Adjacent pieces are one text: the source from the first piece to the last,
 * with the whitespace around them, as written. *)
and jsx_children (env : env) (xs : CST.jsx_child list) : xml_body list =
  let flush (texts : Tree_sitter_run.Token.t list) (acc : xml_body list) :
      xml_body list =
    match (texts, List.rev texts) with
    | (last, _) :: _, (first, _) :: _ ->
        XmlText (str env (jsx_text_token env.extra.source first last)) :: acc
    | _ -> acc
  in
  let texts, acc =
    List.fold_left
      (fun ((texts : Tree_sitter_run.Token.t list), (acc : xml_body list))
           (x : CST.jsx_child) ->
        match x with
        | `Choice_jsx_text (`Jsx_text tok)
        | `Choice_jsx_text (`Html_char_ref tok)
        | `AMP tok
        | `GT tok ->
            (tok :: texts, acc)
        | `Choice_jsx_text (`Choice_jsx_elem x) ->
            ([], XmlXml (jsx_element_ env x) :: flush texts acc)
        | `Choice_jsx_text (`Jsx_exp x) ->
            ([], XmlExpr (jsx_expression env x) :: flush texts acc))
      ([], []) xs
  in
  List.rev (flush texts acc)

and jsx_element_ (env : env) (x : CST.jsx_element_) : xml =
  match x with
  | `Jsx_elem ((v1, v2, v3), children, closing) -> (
      let t0 = token env v1 (* "<" *) in
      let tag_and_attrs = jsx_tag_and_attributes env v2 in
      let t1 = token env v3 (* ">" *) in
      let body = jsx_children env children in
      let closing = jsx_closing_element env closing in
      match tag_and_attrs with
      | Some (tag, attrs) ->
          {
            xml_kind = XmlClassic (t0, tag, t1, closing);
            xml_attrs = attrs;
            xml_body = body;
          }
      | None ->
          {
            xml_kind = XmlFragment (Tok.combine_toks t0 [ t1 ], closing);
            xml_attrs = [];
            xml_body = body;
          })
  | `Jsx_self_clos_elem (v1, v2, v3) -> (
      let t0 = token env v1 (* "<" *) in
      let t1 = token env v3 (* "/>" *) in
      match jsx_tag_and_attributes env v2 with
      | Some (tag, attrs) ->
          {
            xml_kind = XmlSingleton (t0, tag, t1);
            xml_attrs = attrs;
            xml_body = [];
          }
      | None ->
          { xml_kind = XmlFragment (t0, t1); xml_attrs = []; xml_body = [] })

and pattern (env : env) (x : CST.pattern) : (a_ident, a_pattern) Either.t =
  match x with
  | `Choice_choice_member_exp e -> (
      let lhs = lhs_expression env e in
      match lhs with
      | Id x -> Left x
      | _ -> Right lhs)
  | `Rest_pat (v1, v2) ->
      let tok = token env v1 (* "..." *) in
      let lhs = lhs_expression env v2 in
      (* Wrap the binding inside an [Apply(Spread, [binding])] so the
       * destructure target preserves the [...rest] name. Mirrors how
       * spread elements are emitted in array/call-argument contexts. *)
      Right (Apply (IdSpecial (Spread, tok), fb [ lhs ]))

(*
   This is a pattern for destructuring an object property.
   It could use its own type rather than abusing the 'property' type.
   See notes in ast_js.ml in pfff.
*)
and object_property_pattern (env : env) (x : CST.anon_choice_pair_pat_3ff9cbe) :
    property =
  match x with
  | `Pair_pat (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let _v2 = token env v2 (* ":" *) in
      let body = pat_or_assign_pat env v3 |> sub_pattern in
      let ty = None in
      FieldColon
        { fld_name = v1; fld_attrs = []; fld_type = ty; fld_body = Some body }
  | `Rest_pat x ->
      let t, p = rest_pattern env x in
      let pat =
        match p with
        | Left id -> idexp id
        | Right pat -> pat
      in
      FieldSpread (t, pat)
  | `Obj_assign_pat (v1, v2, v3) ->
      let pat =
        match v1 with
        | `Choice_choice_decl x -> id_or_reserved_id env x |> idexp
        | `Dest_pat x -> destructuring_pattern env x
      in
      let tok = token env v2 (* "=" *) in
      (* default value for the property *)
      let e = expression env v3 in
      FieldPatDefault (pat, tok, e)
  | `Choice_id x ->
      let id = id_or_reserved_id env x in
      FieldColon
        {
          fld_name = PN id;
          fld_attrs = [];
          fld_type = None;
          fld_body = Some (idexp id);
        }

and destructuring_pattern (env : env) (x : CST.destructuring_pattern) :
    a_pattern =
  match x with
  | `Obj_pat (v1, v2, v3) ->
      (* similar to 'object_' *)
      let v1 = token env v1 (* "{" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            map_sep_list env v1 v2 (fun env x ->
                match x with
                | Some x -> [ object_property_pattern env x ]
                | None -> [])
            |> List_.flatten
        | None -> []
      in
      let v3 = token env v3 (* "}" *) in
      Obj (v1, v2, v3)
  | `Array_pat (v1, v2, v3) ->
      let open_ = token env v1 (* "[" *) in
      let elements =
        match v2 with
        | Some (v1, v2) ->
            array_slots (fun x -> pat_or_assign_pat env x |> sub_pattern) v1 v2
        | None -> []
      in
      let close = token env v3 (* "]" *) in
      Arr (open_, elements, close)

and variable_declaration (env : env)
    ((v1, v2, v3, v4) : CST.variable_declaration) : var list =
  let v1 = (Var, token env v1) (* "var" *) in
  let vars = map_sep_list env v2 v3 variable_declarator in
  let _v4 = semicolon env v4 in
  build_vars v1 vars

and function_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_expression) :
    function_definition * a_ident option =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 = token env v2 (* "function" *) in
  let v3 =
    match v3 with
    | Some tok -> Some (identifier env tok) (* identifier *)
    | None -> None
  in
  let _tparams, (v4, tret) = call_signature env v4 in
  let v5 = statement_block env v5 in
  let f_kind = (G.LambdaKind, v2) in
  ({ f_attrs = v1; f_params = v4; f_body = v5; f_rettype = tret; f_kind }, v3)

(* `Map<>`, Flow's empty type arguments, has the name only, like `Map<T>`. *)
and generic_type (env : env) (x : CST.generic_type) : type_ =
  match x with
  | `Choice_id_type_args (v1, _)
  | `Choice_id_flow_empty_type_args (v1, _) -> (
      match v1 with
      | `Id tok -> TyName [ identifier env tok ] (* identifier *)
      | `Nested_type_id x -> TyName (nested_type_identifier env x))
  | `Choice_comp_type_args ((`Comp tok | `Unique tok), _) ->
      TyName [ identifier env tok ]
  | `Choice_type_query_member_exp_in_type_anno_type_args (v1, _) ->
      import_type env v1

and implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) :
    type_ list =
  let _v1 = token env v1 (* "implements" *) in
  let types = map_sep_list env v2 v3 type_ in
  types

and anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  match x with
  | `Exp x -> expression env x
  | `Spread_elem x ->
      let t, e = spread_element env x in
      Apply (IdSpecial (Spread, t), fb [ e ])

and switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 = List.concat_map (statement env) v3 in
  Default (v1, stmt1 v2 v3)

and binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.And, v2), fb [ v1; v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Or, v2), fb [ v1; v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LSR, v2), fb [ v1; v3 ])
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.ASR, v2), fb [ v1; v3 ])
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LSL, v2), fb [ v1; v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitAnd, v2), fb [ v1; v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitXor, v2), fb [ v1; v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitOr, v2), fb [ v1; v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Plus, v2), fb [ v1; v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Minus, v2), fb [ v1; v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Mult, v2), fb [ v1; v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Div, v2), fb [ v1; v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Mod, v2), fb [ v1; v3 ])
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "**" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Pow, v2), fb [ v1; v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Lt, v2), fb [ v1; v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LtE, v2), fb [ v1; v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Eq, v2), fb [ v1; v3 ])
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "===" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.PhysEq, v2), fb [ v1; v3 ])
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.NotEq, v2), fb [ v1; v3 ])
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!==" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.NotPhysEq, v2), fb [ v1; v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.GtE, v2), fb [ v1; v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Gt, v2), fb [ v1; v3 ])
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Nullish, v2), fb [ v1; v3 ])
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "instanceof" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (Instanceof, v2), fb [ v1; v3 ])
  | `Choice_exp_in_exp (v1, v2, v3) ->
      (* `#x in o` tests whether `o` has the private field `#x`. *)
      let v1 =
        match v1 with
        | `Exp x -> expression env x
        | `Priv_prop_id tok -> str env tok |> idexp
      in
      let v2 = token env v2 (* "in" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (In, v2), fb [ v1; v3 ])

and arguments (env : env) ((v1, v2, v3) : CST.arguments) : a_arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 = anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2 in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and generator_function_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) :
    definition =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = [ attr (Generator, token env v3) ] (* "*" *) in
  let v4 = identifier env v4 (* identifier *) in
  let _tparams, (v5, tret) = call_signature env v5 in
  let v6 = statement_block env v6 in
  let _v7 = automatic_semicolon_opt env v7 in
  let f_kind = (G.Function, v2) in
  let f =
    { f_attrs = v1 @ v3; f_params = v5; f_body = v6; f_rettype = tret; f_kind }
  in
  (basic_entity v4, FuncDef f)

and variable_declarator (env : env) (x : CST.variable_declarator) =
  match x with
  | `Choice_id_opt_type_anno_opt_init (v1, v2, v3) ->
      let id_or_pat = id_or_destructuring_pattern env v1 in
      let type_ =
        match v2 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let default = initializer_opt env v3 in
      (id_or_pat, type_, default)
  | `Id_BANG_type_anno (v1, v2, v3) ->
      let id_or_pat = Left (identifier env v1 (* identifier *)) in
      (* definite assignment assertion
         TODO: add to AST? *)
      let _v2 = token env v2 (* "!" *) in
      let type_ = type_annotation env v3 |> snd in
      (id_or_pat, Some type_, None)

(* `a, b, c` is `a, (b, c)`. *)
and sequence_expression (env : env) ((v1, v2) : CST.sequence_expression) : expr
    =
  let rec nest (first : expr) (rest : (tok * expr) list) : expr =
    match rest with
    | [] -> first
    | (comma, e) :: rest ->
        Apply (IdSpecial (Seq, comma), fb [ first; nest e rest ])
  in
  nest (expression env v1)
    (List_.map
       (fun (comma, e) -> (token env comma (* "," *), expression env e))
       v2)

and type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) :
    type_ list bracket =
  let v1 = token env v1 (* "<" *) in
  let types = map_sep_list env v2 v3 type_ in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let v5 = token env v5 (* ">" *) in
  (v1, types, v5)

and add_decorators xs property =
  match property with
  | Field fld -> Field { fld with fld_attrs = xs @ fld.fld_attrs }
  | FieldColon fld -> FieldColon { fld with fld_attrs = xs @ fld.fld_attrs }
  (* less: modify ast_js to allow decorator on those constructs? *)
  | FieldSpread _
  | FieldPatDefault _
  | FieldEllipsis _
  | FieldTodo _
  | ClassStaticBlock _ ->
      property

(* TODO: types - class body can be just a signature. *)
and class_body (env : env) ((v1, v2, v3) : CST.class_body) :
    property list bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    v2
    |> List_.filter_map (function
       | `Semg_ellips tok -> Some (FieldEllipsis (token env tok))
       | `Rep_deco_choice_meth_defi_opt_choice_auto_semi (v1, v2) ->
           let decorators = List_.map (decorator env) v1 in
           let member =
             match v2 with
             | `Meth_defi_opt_choice_auto_semi (v1, v2) ->
                 let v1 = method_definition env v1 in
                 let _v2 =
                   match v2 with
                   | Some x -> Some (semicolon env x)
                   | None -> None
                 in
                 v1
             | `Meth_sign_choice_func_sign_auto_semi (v1, v2) ->
                 let v1 = method_signature env v1 in
                 let _v2 =
                   match v2 with
                   | `Func_sign_auto_semi tok ->
                       (* function_signature_automatic_semicolon *) token env tok
                   | `COMMA tok -> (* "," *) token env tok
                 in
                 Field v1
             | `Choice_abst_meth_sign_choice_choice_auto_semi (v1, v2) ->
                 let v1 =
                   match v1 with
                   | `Abst_meth_sign x ->
                       let v = abstract_method_signature env x in
                       Field v
                   | `Index_sign x ->
                       let ty = index_signature env x in
                       let _, lb, _, _, _ = x in
                       Field
                         {
                           fld_name = PN ("[]", token env lb);
                           fld_attrs = [];
                           fld_type = Some ty;
                           fld_body = None;
                         }
                   | `Meth_sign x -> Field (method_signature env x)
                   | `Public_field_defi x -> public_field_definition env x
                 in
                 let _v2 =
                   match v2 with
                   | `Choice_auto_semi x -> semicolon env x
                   | `COMMA tok -> token env tok
                 in
                 v1
           in
           Some (add_decorators decorators member)
       | `Class_static_blk (v1, v2, v3) ->
           let v1 = token env v1 (* "static" *) in
           let _v2 = automatic_semicolon_opt env v2 in
           Some (ClassStaticBlock (v1, statement_block env v3))
       | `SEMI tok ->
           let _t = token env tok (* ";" *) in
           None)
  in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and type_parameter (env : env)
    ((v0, v1, v2, v3, v4, v5) : CST.type_parameter) : a_type_parameter =
  let _variance = Option.map (flow_variance env) v0 in
  let _const = Option.map (token env) v1 (* "const" *) in
  let _in = Option.map (token env) v2 (* "in" *) in
  let name =
    match v3 with
    | `Out_id (v1, v2) ->
        let _out = token env v1 (* "out" *) in
        str env v2 (* identifier *)
    | `Choice_id (`Id tok)
    | `Choice_id (`Out tok) ->
        str env tok (* identifier *)
  in
  let _v4 =
    match v4 with
    | Some x -> Some (constraint_ env x)
    | None -> None
  in
  let _v5 =
    match v5 with
    | Some x -> Some (default_type env x)
    | None -> None
  in
  name

and member_expression (env : env) ((v1, v2, v3) : CST.member_expression) : expr
    =
  let expr =
    match v1 with
    | `Exp x -> expression env x
    | `Prim_exp x -> primary_expression env x
    | `Import tok -> identifier env tok (* import *) |> idexp
  in
  let dot =
    match v2 with
    | `DOT tok (* "." *) -> (Dot, token env tok)
    | `Opt_chain (* "?." *) tok -> (QuestDot, token env tok)
  in
  let id_tok =
    match v3 with
    | `Id x -> x
    | `Priv_prop_id x ->
        (* has a leading '#' indicating a private property.
           Should it have a special construct so we could match
           all private properties in semgrep with e.g. #$VAR ? *)
        x
  in
  let id = identifier env id_tok (* identifier *) in
  apply_postfix expr (fun e -> ObjAccess (e, dot, PN id))

and object_property (env : env) (x : CST.anon_choice_pair_20c9acd) : property =
  match x with
  | `Pair (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let _v2 = token env v2 (* ":" *) in
      let v3 = expression env v3 in
      FieldColon
        { fld_name = v1; fld_attrs = []; fld_type = None; fld_body = Some v3 }
  | `Spread_elem x ->
      let t, e = spread_element env x in
      FieldSpread (t, e)
  | `Meth_defi x -> method_definition env x
  (* { x } shorthand for { x: x }, like in OCaml *)
  | `Choice_id x ->
      let id = id_or_reserved_id env x in
      FieldColon
        {
          fld_name = PN id;
          fld_attrs = [];
          fld_type = None;
          fld_body = Some (idexp id);
        }

and subscript_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.subscript_expression) : expr =
  let expr =
    match v1 with
    | `Exp x -> expression env x
    | `Prim_exp x -> primary_expression env x
  in
  let _v2 =
    match v2 with
    | None -> None
    | Some tok -> (* "?." *) Some (token env tok)
  in
  let v3 = token env v3 (* "[" *) in
  let v4 = expressions env v4 in
  let v5 = token env v5 (* "]" *) in
  (* TODO: distinguish optional chaining "?." from a simple access "." *)
  apply_postfix expr (fun e -> ArrAccess (e, (v3, v4, v5)))

and initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let _v1 = token env v1 (* "=" *) in
  let v2 = expression env v2 in
  v2

and initializer_opt env v =
  match v with
  | Some x -> Some (initializer_ env x)
  | None -> None

and primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Semg_exp_ellips tok ->
      let tok = token env tok in
      Ellipsis tok
  | `Choice_choice_subs_exp x -> (
      match x with
      | `Choice_subs_exp x -> (
          match x with
          | `Subs_exp x -> subscript_expression env x
          | `Member_exp x -> member_expression env x
          | `Paren_exp x -> parenthesized_expression env x
          | `Choice_unde x -> identifier_ env x
          | `Choice_decl x ->
              reserved_identifier env x |> idexp_or_special
          | `This tok -> this env tok (* "this" *)
          | `Super tok -> super env tok (* "super" *)
          | `Num tok ->
              let n = number env tok (* number *) in
              L (Num n)
          | `Str x ->
              let s = string_ env x in
              L (String s)
          | `Temp_str x ->
              let t1, xs, t2 = template_string env x in
              Apply (IdSpecial (Encaps false, t1), (t1, xs, t2))
          | `Regex (v1, v2, v3, v4) ->
              let v1 = token env v1 (* "/" *) in
              let v2 = str env v2 (* regex_pattern *) in
              let v3 = token env v3 (* "/" *) in
              let v4 =
                match v4 with
                | Some tok -> Some (str env tok) (* pattern [a-z]+ *)
                | None -> None
              in
              L (Regexp ((v1, v2, v3), v4))
          | `True tok -> L (Bool (true, token env tok) (* "true" *))
          | `False tok -> L (Bool (false, token env tok) (* "false" *))
          | `Null tok -> IdSpecial (Null, token env tok) (* "null" *)
          | `Obj x ->
              let o = object_ env x in
              Obj o
          | `Array x -> array_ env x
          | `Func_exp x ->
              let f, idopt = function_expression env x in
              Fun (f, idopt)
          | `Arrow_func (v1, v2, v3, v4) ->
              let v1 =
                match v1 with
                | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
                | None -> []
              in
              let v2, tret =
                match v2 with
                | `Choice_choice_decl x ->
                    let id = id_or_reserved_id env x in
                    (fb [ ParamClassic (mk_param id) ], None)
                | `Call_sign x ->
                    let _tparams, (params, tret) = call_signature env x in
                    (params, tret)
              in
              let v3 = token env v3 (* "=>" *) in
              let v4 =
                match v4 with
                | `Exp x ->
                    let e = expression env x in
                    Return (v3, Some e, Tok.sc v3)
                | `Stmt_blk x -> statement_block env x
              in
              let f_kind = (G.Arrow, v3) in
              let f =
                {
                  f_attrs = v1;
                  f_params = v2;
                  f_body = v4;
                  f_rettype = tret;
                  f_kind;
                }
              in
              Fun (f, None)
          | `Gene_func (v1, v2, v3, v4, v5, v6) ->
              let v1 =
                match v1 with
                | Some tok -> [ (Async, token env tok) ] (* "async" *)
                | None -> []
              in
              let v2 = token env v2 (* "function" *) in
              let v3 = [ (Generator, token env v3) ] (* "*" *) in
              let v4 =
                match v4 with
                | Some tok -> Some (identifier env tok) (* identifier *)
                | None -> None
              in
              let _tparams, (v5, tret) = call_signature env v5 in
              let v6 = statement_block env v6 in
              let attrs = v1 @ v3 |> List_.map attr in
              let f_kind = (G.LambdaKind, v2) in
              let f =
                {
                  f_attrs = attrs;
                  f_params = v5;
                  f_body = v6;
                  f_rettype = tret;
                  f_kind;
                }
              in
              Fun (f, v4)
          | `Class (v1, v2, v3, v4, v5, v6) ->
              let v1 = List_.map (decorator env) v1 in
              let v2 = token env v2 (* "class" *) in
              let v3 =
                match v3 with
                | Some tok -> Some (identifier env tok) (* identifier *)
                | None -> None
              in
              (* TODO types *)
              let _v4 =
                match v4 with
                | Some x -> type_parameters env x
                | None -> []
              in
              let c_extends, c_implements =
                match v5 with
                | Some x -> class_heritage env x
                | None -> ([], [])
              in
              let v6 = class_body env v6 in
              let class_ =
                {
                  c_kind = (G.Class, v2);
                  c_attrs = v1;
                  c_extends;
                  c_implements;
                  c_body = v6;
                }
              in
              Class (class_, v3)
          | `Meta_prop (`New_DOT_target (v1, v2, v3)) ->
              let v1 = token env v1 (* "new" *) in
              let v2 = token env v2 (* "." *) in
              let v3 = token env v3 (* "target" *) in
              let t = Tok.combine_toks v1 [ v2; v3 ] in
              IdSpecial (NewTarget, t)
          | `Meta_prop (`Import_DOT_meta (v1, v2, v3)) ->
              let v1 = identifier env v1 (* "import" *) in
              let v2 = token env v2 (* "." *) in
              let v3 = identifier env v3 (* "meta" *) in
              ObjAccess (idexp v1, (Dot, v2), PN v3)
          | `Call_exp x -> call_expression env x)
      | `Non_null_exp x -> non_null_expression env x)

and call_expression (env : env) (x : CST.call_expression) =
  match x with
  | `Choice_choice_exp_opt_type_args_args x -> (
      match x with
      | `Choice_exp_opt_type_args_args (v1, v2, v3) ->
          let v1 =
            match v1 with
            | `Exp x -> expression env x
            | `Import tok -> identifier env tok (* import *) |> idexp
          in
          (* TODO: types *)
          let _v2TODO =
            match v2 with
            | Some x -> type_arguments env x |> Tok.unbracket
            | None -> []
          in
          let args = arguments env v3 in
          apply_postfix v1 (fun f -> Apply (f, args))
      | `Choice_prim_exp_temp_str (v1, v2) ->
          let v1 =
            match v1 with
            | `Prim_exp x -> primary_expression env x
            | `New_exp x -> new_expression env x
          in
          let t1, xs, t2 = template_string env v2 in
          apply_postfix v1 (fun f ->
              Apply (IdSpecial (Encaps true, t1), (t1, f :: xs, t2)))
      | `Prim_exp_QMARKDOT_opt_type_args_args (v1, v2, v3, v4) ->
          let v1 = primary_expression env v1 in
          let _v2 = token env v2 (* "?." *) in
          (* TODO: types *)
          let _v3TODO =
            match v3 with
            | Some x -> type_arguments env x |> Tok.unbracket
            | None -> []
          in
          let v4 = arguments env v4 in
          (* TODO: distinguish "?." from a simple application *)
          apply_postfix v1 (fun f -> Apply (f, v4)))
  | `Exp_type_args_temp_str (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 = type_arguments env v2 |> Tok.unbracket in
      let t1, xs, t2 = template_string env v3 in
      apply_postfix v1 (fun f ->
          Apply (IdSpecial (Encaps true, t1), (t1, f :: xs, t2)))

(* The grammar can read `await f<T>(x)`, `!f<T>(x)` and `a + f<T>(x)` as a
 * call of `await f`, `!f` or `a + f`, and a member access, subscript or call
 * that follows as applied to that call. Calls, member accesses and subscripts
 * bind tighter than these operators, whose operand they take only in
 * parentheses (kept as [ParenExpr]), so the postfix applies to the operand
 * that the operator takes last. *)
and apply_postfix (head : expr) (postfix : expr -> expr) : expr =
  let into_last (op : expr) ((l, args, r) : expr list bracket) : expr =
    match List.rev args with
    | last :: rest ->
        Apply (op, (l, List.rev (apply_postfix last postfix :: rest), r))
    | [] -> postfix head
  in
  match head with
  | Apply (IdSpecial (ArithOp G.NotNullPostfix, _), _) -> postfix head
  | Apply
      ( (IdSpecial
           ( ( Await
             | Yield
             | YieldStar
             | Typeof
             | Void
             | Delete
             | Seq
             | In
             | Instanceof
             | ArithOp _
             | IncrDecr (_, G.Prefix) ),
             _ ) as op),
        args ) ->
      into_last op args
  | _ -> postfix head

and anon_choice_prop_name_6cc9e4b (env : env)
    (x : CST.anon_choice_prop_name_6cc9e4b) =
  match x with
  | `Prop_name x -> (property_name env x, None)
  | `Enum_assign (v1, v2) ->
      let v1 = property_name env v1 in
      let v2 = initializer_ env v2 in
      (v1, Some v2)

and module__ (env : env) ((v1, v2) : CST.module__) =
  let v1 =
    (* module identifier *)
    match v1 with
    | `Str x -> string_ env x
    | `Id tok -> identifier env tok (* identifier *)
    | `Nested_id x -> nested_identifier env x |> concat_nested_identifier
  in
  let v2 =
    (* optional module body *)
    match v2 with
    | Some x -> Some (statement_block env x)
    | None -> None
  in
  (v1, v2)

and non_null_expression (env : env) ((v1, v2) : CST.non_null_expression) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "!" *) in
  let special = (ArithOp G.NotNullPostfix, v2) in
  apply_postfix v1 (fun e -> Apply (IdSpecial special, fb [ e ]))

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = expressions env v1 in
  let v2 = semicolon env v2 in
  (v1, v2)

and catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) : catch =
  let catch_tok = token env v1 (* "catch" *) in
  let stmts = statement_block env v3 in
  let catch =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _open = token env v1 (* "(" *) in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let type_ =
          match v3 with
          | Some x -> Some (type_annotation env x)
          | None -> None
        in
        let _close = token env v4 (* ")" *) in
        let pat =
          match id_or_pat with
          | Left id -> idexp id
          | Right pat -> pat
        in
        let pat =
          match type_ with
          | None -> pat
          | Some (colon_tok, type_) -> Cast (pat, colon_tok, type_)
        in
        BoundCatch (catch_tok, pat, stmts)
    | None -> UnboundCatch (catch_tok, stmts)
  in
  catch

and object_type (env : env) ((v1, v2, v3) : CST.object_type) =
  let v1 =
    match v1 with
    | `LCURL tok -> token env tok (* "{" *)
    | `LCURLBAR tok -> token env tok
    (* "{|" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _v1 =
          match v1 with
          | Some x ->
              Some
                (match x with
                | `COMMA tok -> token env tok (* "," *)
                | `SEMI tok -> token env tok (* ";" *))
          | None -> None
        in
        let v2 = anon_choice_export_stmt_42be789 env v2 in
        let v3 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = anon_choice_COMMA_5194cb4 env v1 in
              let v2 = anon_choice_export_stmt_42be789 env v2 in
              v2)
            v3
        in
        let _v4 =
          match v4 with
          | Some x -> Some (anon_choice_COMMA_5194cb4 env x)
          | None -> None
        in
        v2 :: v3
    | None -> []
  in
  let v3 =
    match v3 with
    | `RCURL tok -> token env tok (* "}" *)
    | `BARRCURL tok -> token env tok
    (* "|}" *)
  in
  (v1, v2, v3)

and template_string (env : env) ((v1, v2, v3) : CST.template_string) :
    expr list bracket =
  let v1 = token env v1 (* "`" *) in
  (* Combine [`Temp_chars] and [`Esc_seq] sequences. *)
  let merge_str = function
    | [] -> assert false
    | `Temp_subs _ :: _ -> assert false
    | [`Temp_chars tok | `Esc_seq tok] -> str env tok
    | vs ->
      let strs_toks =
        List.map
          (function
            | (`Temp_chars tok | `Esc_seq tok) -> str env tok
            | _ -> assert false)
          vs in
      let toks = List.map snd strs_toks in
      let combined_toks = Tok.combine_toks (List.hd toks) (List.tl toks) in
      let combined_str = String.concat "" (List.map fst strs_toks) in
      combined_str, combined_toks
  in
  let rec combine_str acc vs res =
    match acc, vs with
    | [], [] -> List.rev res
    | _, [] -> List.rev @@ (List.rev acc) :: res
    | [], x :: xs -> combine_str [x] xs res
    | (`Temp_chars _ | `Esc_seq _) :: _, (`Temp_chars _ | `Esc_seq _ as x) :: xs ->
      combine_str (x :: acc) xs res
    | _, x :: xs -> combine_str [x] xs ((List.rev acc) :: res)
  in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | (`Temp_chars _ | `Esc_seq _) :: _ as xs -> L (String (merge_str xs))
        | [ `Temp_subs x ] -> template_substitution env x
        | _ -> assert false)
      (combine_str [] v2 [])
  in
  let v3 = token env v3 (* "`" *) in
  (v1, v2, v3)

and template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution)
    : expr =
  let l = token env v1 (* "${" *) in
  let e = expressions env v2 in
  let r = token env v3 (* "}" *) in
  ParenExpr (l, e, r)

and map_template_literal_type (env : env)
    ((v1, v2, v3) : CST.template_literal_type) : type_ =
  let lback = (* "`" *) token env v1 in
  let xs =
    List_.map
      (fun x ->
        match x with
        | `Temp_chars tok ->
            TyLiteral (String (* template_chars *) (str env tok))
        | `Temp_type x ->
            let _, x, _ = map_template_type env x in
            x)
      v2
  in
  let _rback = (* "`" *) token env v3 in
  TypeTodo (("TemplateLitType", lback), xs |> List_.map (fun x -> Type x))

and map_template_type (env : env) ((v1, v2, v3) : CST.template_type) :
    type_ bracket =
  let l = (* "${" *) token env v1 in
  let ty =
    match v2 with
    | `Prim_type x -> primary_type env x
    | `Infer_type x -> map_infer_type env x
  in
  let r = (* "}" *) token env v3 in
  (l, ty, r)

and decorator (env : env) ((v1, v2) : CST.decorator) : attribute =
  let v1 = token env v1 (* "@" *) in
  let named_attr ((ids, args_opt) : a_ident list * a_arguments option) : attribute =
    NamedAttr (v1, ids, args_opt)
  in
  match v2 with
  | (`Id _ | `Deco_member_exp _ | `Deco_call_exp _) as x ->
      named_attr (decorator_expression env x)
  | `Deco_paren_exp (v2, v3, v4) ->
      let lp = token env v2 (* "(" *) in
      let e =
        match v3 with
        | (`Id _ | `Deco_member_exp _) as x -> decorator_operand env x
        | `Deco_call_exp (v1, v2, v3) ->
            let _type_args = Option.map (type_arguments env) v2 in
            Apply (decorator_operand env v1, arguments env v3)
        | `Exp x -> expression env x
      in
      let rp = token env v4 (* ")" *) in
      ExprAttr (v1, ParenExpr (lp, e, rp))

(* `a.b` in `@(a.b)`, as a member access *)
and decorator_operand (env : env) (x : CST.anon_choice_type_id_b8f8ced) : expr =
  match x with
  | `Id x -> idexp (identifier env x (* identifier *))
  | `Deco_member_exp (v1, v2, v3) ->
      ObjAccess
        ( decorator_operand env v1,
          (Dot, token env v2 (* "." *)),
          PN (identifier env v3 (* identifier *)) )

and decorator_expression (env : env)
    (x :
      [ `Id of CST.identifier
      | `Deco_member_exp of CST.decorator_member_expression
      | `Deco_call_exp of CST.decorator_call_expression ]) :
    a_ident list * a_arguments option =
  match x with
  | `Id x ->
      let id = identifier env x in
      ([ id ], None)
  | `Deco_member_exp x ->
      let ids = decorator_member_expression env x in
      (ids, None)
  | `Deco_call_exp x ->
      let ids, args = decorator_call_expression env x in
      (ids, Some args)

and internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let _v1 = token env v1 (* "namespace" *) in
  let v2 = module__ env v2 in
  v2

and anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 (env : env)
    (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4) =
  match opt with
  | Some (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> [ anon_choice_exp_9818c1b env x ]
        | None -> []
      in
      let v2 = anon_rep_COMMA_opt_choice_exp_ca698a5 env v2 in
      v1 @ v2
  | None -> []

and for_header (env : env) ((v1, v2, v3, v4, v5) : CST.for_header) : for_header
    =
  let _open = token env v1 (* "(" *) in
  let var_or_expr =
    match v2 with
    | `Choice_choice_choice_member_exp x ->
        let expr = paren_expr_or_lhs_expr env x in
        Right expr
    | `Var_choice_id_opt_init (v1, v2, v3) ->
        let var_kind = (Var, token env v1) in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let pat = id_or_pat |> sub_pattern in
        let rhs = initializer_opt env v3 in
        let var = Ast_js.var_pattern_to_var var_kind pat (snd var_kind) rhs in
        Left var
    | `Choice_let_choice_id_opt_auto_semi (v1, v2, v3) ->
        let _v3 = automatic_semicolon_opt env v3 in
        let var_kind =
          match v1 with
          | `Let tok -> (Let, token env tok) (* "let" *)
          | `Const tok -> (Const, token env tok)
          (* "const" *)
        in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let pat = id_or_pat |> sub_pattern in
        let var = Ast_js.var_pattern_to_var var_kind pat (snd var_kind) None in
        Left var
    | `Choice_using_id_opt_auto_semi (v1, v2, v3) ->
        let _v3 = automatic_semicolon_opt env v3 in
        let var_kind = using_kind env v1 in
        let id = identifier env v2 (* identifier *) in
        let var =
          Ast_js.var_pattern_to_var var_kind (Id id) (snd var_kind) None
        in
        Left var
  in

  let exprs = expressions env v4 in
  let for_header =
    match v3 with
    | `In tok -> (* "in" *) ForIn (var_or_expr, token env tok, exprs)
    | `Of tok -> (* "of" *) ForOf (var_or_expr, token env tok, exprs)
  in
  let _close = token env v5 (* ")" *) in
  for_header

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `As_exp (v1, v2, v3) ->
      (* type assertion of the form 'exp as type' *)
      let e = expression env v1 in
      let tas = token env v2 (* "as" *) in
      let ty =
        match v3 with
        | `Type x -> type_ env x
        | `Const tok -> TyName [ identifier env tok (* "const" *) ]
      in
      TypeAssert (e, tas, ty)
  | `Satiss_exp (v1, v2, v3) ->
      let e = expression env v1 in
      let tsat = token env v2 (* "satisfies" *) in
      let ty = type_ env v3 in
      TypeAssert (e, tsat, ty)
  | `Inst_exp (v1, v2) ->
      let e = expression env v1 in
      Instantiation (e, type_arguments env v2)
  | `Inte_module x -> (
      (* namespace (deprecated in favor of ES modules) *)
      (* TODO represent namespaces properly in the AST instead of the nonsense
         below. *)
      let name, opt_body = internal_module env x in
      match opt_body with
      | Some body ->
          let fun_ =
            {
              f_attrs = [];
              f_params = fb [];
              f_body = body;
              f_rettype = None;
              f_kind = (G.Function, fake);
            }
          in
          Apply (Fun (fun_, Some name), fb [])
      | None -> idexp name)
  | `Type_asse (v1, v2) -> (
      (* type assertion of the form <string>someValue *)
      let t1, xs, _ = type_arguments env v1 in
      let v2 = expression env v2 in
      match xs with
      | [ t ] -> TypeAssert (v2, t1, t)
      | _ ->
          raise (Parsing_error.Ast_builder_error ("wrong type assert expr", t1))
      )
  | `Prim_exp x -> primary_expression env x
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      Xml xml
  | `Assign_exp (v1, v2, v3) ->
      let v1 = paren_expr_or_lhs_expr env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = expression env v3 in
      Assign (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let lhs =
        match v1 with
        | `Choice_member_exp x -> (
            match x with
            | `Member_exp x -> member_expression env x
            | `Subs_exp x -> subscript_expression env x
            | `Choice_decl x ->
                let id = reserved_identifier env x in
                idexp id
            | `Id tok ->
                let id = identifier env tok (* identifier *) in
                idexp id
            | `Paren_exp x -> parenthesized_expression env x)
        | `Non_null_exp x -> non_null_expression env x
      in
      let op, is_logical, tok =
        match v2 with
        | `PLUSEQ tok -> (G.Plus, false, token env tok) (* "+=" *)
        | `DASHEQ tok -> (G.Minus, false, token env tok) (* "-=" *)
        | `STAREQ tok -> (G.Mult, false, token env tok) (* "*=" *)
        | `SLASHEQ tok -> (G.Div, false, token env tok) (* "/=" *)
        | `PERCEQ tok -> (G.Mod, false, token env tok) (* "%=" *)
        | `HATEQ tok -> (G.BitXor, false, token env tok) (* "^=" *)
        | `AMPEQ tok -> (G.BitAnd, false, token env tok) (* "&=" *)
        | `BAREQ tok -> (G.BitOr, false, token env tok) (* "|=" *)
        | `GTGTEQ tok -> (G.LSR, false, token env tok) (* ">>=" *)
        | `GTGTGTEQ tok -> (G.ASR, false, token env tok) (* ">>>=" *)
        | `LTLTEQ tok -> (G.LSL, false, token env tok) (* "<<=" *)
        | `STARSTAREQ tok -> (G.Pow, false, token env tok) (* "**=" *)
        | `AMPAMPEQ tok -> (G.And, true, token env tok) (* "&&=" *)
        | `BARBAREQ tok -> (G.Or, true, token env tok) (* "||=" *)
        | `QMARKQMARKEQ tok -> (G.Nullish, true, token env tok)
        (* "??=" *)
      in
      let rhs = expression env v3 in
      (* less: should use intermediate instead of repeating v1 *)
      if is_logical then
        Apply (IdSpecial (ArithOp op, tok), fb [ lhs; Assign (lhs, tok, rhs) ])
      else
        Assign (lhs, tok, Apply (IdSpecial (ArithOp op, tok), fb [ lhs; rhs ]))
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (Await, v1), fb [ v2 ])
  | `Un_exp x -> unary_expression env x
  | `Bin_exp x -> binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5)
  | `Update_exp x -> update_expression env x
  | `New_exp x -> new_expression env x
  | `Yield_exp (v1, v2) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        match v2 with
        | `STAR_exp (v1bis, v2) ->
            let _v1bis = token env v1bis (* "*" *) in
            let v2 = expression env v2 in
            Apply (IdSpecial (YieldStar, v1), fb [ v2 ])
        | `Opt_exp opt -> (
            match opt with
            | Some x ->
                let x = expression env x in
                Apply (IdSpecial (Yield, v1), fb [ x ])
            | None -> Apply (IdSpecial (Yield, v1), fb []))
      in
      v2

and new_expression (env : env) ((v1, v2, v3, v4) : CST.new_expression) : expr =
  let v1 = token env v1 (* "new" *) in
  let v2 = primary_expression env v2 in
  (* TODO types *)
  let _v3TODO =
    match v3 with
    | Some x -> type_arguments env x |> Tok.unbracket
    | None -> []
  in
  let t1, xs, t2 =
    match v4 with
    | Some x -> arguments env x
    | None -> fb []
  in
  New (v1, v2, (t1, xs, t2))

and paren_expr_or_lhs_expr (env : env)
    (x :
      [ `Paren_exp of CST.parenthesized_expression
      | `Choice_choice_member_exp of CST.lhs_expression ]) : expr =
  match x with
  | `Paren_exp x -> parenthesized_expression env x
  | `Choice_choice_member_exp x -> lhs_expression env x

and primary_type (env : env) (x : CST.primary_type) : type_ =
  match x with
  | `Choice_paren_type x -> (
      match x with
      | `Union_type (v1, v2, v3) -> (
          let v2 = token env v2 (* "|" *) in
          let v3 = type_ env v3 in
          match v1 with
          | Some x ->
              let x = type_ env x in
              TyOr (x, v2, v3)
          | None -> v3
          (* ?? *))
      | `Inte_type (v1, v2, v3) -> (
          let v2 = token env v2 (* "&" *) in
          let v3 = type_ env v3 in
          match v1 with
          | Some x ->
              let x = type_ env x in
              TyAnd (x, v2, v3)
          | None -> v3
          (* ?? *))
      | `Temp_lit_type x -> map_template_literal_type env x
      | `Const tok -> TyName [ identifier env tok (* "const" *) ]
      | `Paren_type (v1, v2, v3) ->
          let _v1 = token env v1 (* "(" *) in
          let v2 = type_ env v2 in
          let _v3 = token env v3 (* ")" *) in
          v2
      | `Pred_type x ->
          let id = predefined_type env x in
          (* less: could also be a G.TyBuiltin *)
          TyName [ id ]
      | `Id tok ->
          let id = identifier env tok (* identifier *) in
          TyName [ id ]
      | `Nested_type_id x ->
          let xs = nested_type_identifier env x in
          TyName xs
      | `Gene_type x -> generic_type env x
      | `Obj_type x ->
          let t1, xs, t2 = object_type env x in
          let xs =
            xs
            |> List_.filter_map (function
                 (* TODO *)
                 | Left prop -> Some prop
                 | Right _sts -> None)
          in
          TyRecordAnon (t1, xs, t2)
      | `Array_type (v1, v2, v3) ->
          let type_ = primary_type env v1 in
          let open_ = token env v2 (* "[" *) in
          let close = token env v3 (* "]" *) in
          TyArray (type_, (open_, (), close))
      | `Tuple_type (v1, v2, v3, v4) ->
          let open_ = token env v1 (* "[" *) in
          let members =
            match v2 with
            | None -> []
            | Some (v1, v2) -> map_sep_list env v1 v2 tuple_type_member
          in
          let _trailing_comma = optional env v3 token (* "," *) in
          let close = token env v4 (* "]" *) in
          TyTuple (open_, members, close)
      | `Flow_maybe_type x -> (
          match x with
          | `QMARK_prim_type (v1, v2) ->
              let v1 = token env v1 (* "?" *) in
              TyQuestion (v1, primary_type env v2)
          | `QMARK_array_type (v1, v2) ->
              let v1 = token env v1 (* "?" *) in
              TyQuestion (v1, primary_type env (`Choice_paren_type (`Array_type v2)))
          | `QMARK_opt_type_params_choice_formal_params_EQGT_type
              (v1, v2, v3, v4, v5) ->
              let v1 = token env v1 (* "?" *) in
              let _tparams =
                match v2 with
                | Some x -> type_parameters env x
                | None -> []
              in
              let params =
                match v3 with
                | `Formal_params x ->
                    formal_parameters env x |> function_type_parameters env
                | `Flow_func_type_params x -> flow_function_type_parameters env x
              in
              let _arrow = token env v4 (* "=>" *) in
              let ret = type_ env v5 in
              TyQuestion (v1, mk_functype (params, Some ret)))
      | `Type_query x -> type_query env x
      | `Index_type_query (v1, v2) ->
          let keyof = token env v1 (* "keyof" *) in
          let type_ = primary_type env v2 in
          TypeTodo (("KeyOf", keyof), [ Type type_ ])
      | `This tok ->
          let v1 = token env tok in
          (* "this" *)
          TypeTodo (("This", v1), [])
      | `Exis_type tok ->
          let v1 = token env tok (* "*" *) in
          TypeTodo (("*", v1), [])
      | `Lit_type x ->
          let v1 = literal_type env x in
          TypeTodo (("LitType", fake), [ Expr v1 ])
      | `Lookup_type (v1, v2, v3, v4) ->
          let v1 = primary_type env v1 in
          let v2 = token env v2 (* "[" *) in
          let v3 = type_ env v3 in
          let _v4 = token env v4 (* "]" *) in
          TypeTodo (("LookupType", v2), [ Type v1; Type v3 ])
      | `Cond_type (v1, v2, v3, v4, v5, v6, v7) ->
          let v1 = type_ env v1 in
          let _v2 = token env v2 (* "extends" *) in
          let v3 = type_ env v3 in
          let v4 = token env v4 (* "?" *) in
          let v5 = type_ env v5 in
          let _v6 = token env v6 (* ":" *) in
          let v7 = type_ env v7 in
          TypeTodo (("ConditionalType", v4), [ Type v1; Type v3; Type v5; Type v7 ]))
  | `Comp tok -> TyName [ identifier env tok (* "component" *) ]
  | `Unique tok -> TyName [ identifier env tok (* "unique" *) ]
  | `Flow_comp_type (v1, v2, v3) ->
      let v1 = token env v1 (* "component" *) in
      flow_component_type env v1 v2 v3
  | `Flow_inte_type (v1, v2, v3) ->
      (* an inline interface type, `interface extends A { ... }` *)
      let v1 = token env v1 (* "interface" *) in
      let parents =
        match v2 with
        | Some x ->
            map_extends_type_clause env x
            |> List_.filter_map (function
                 | Right t -> Some (Type t)
                 | Left _ -> None)
        | None -> []
      in
      let body = primary_type env (`Choice_paren_type (`Obj_type v3)) in
      TypeTodo (("InterfaceType", v1), parents @ [ Type body ])
  | ( `Type_query_member_exp_in_type_anno _
    | `Type_query_call_exp_in_type_anno _ ) as x ->
      import_type env x

(* the type of a component, `component(...props: P) renders R` *)
and flow_component_type (env : env) (component_tok : tok)
    (params : CST.flow_component_parameters)
    (renders : CST.flow_renders_annotation option) : type_ =
  let _params = flow_component_parameters env params in
  let renders =
    match renders with
    | Some x -> [ Type (flow_renders_annotation env x) ]
    | None -> []
  in
  TypeTodo (("ComponentType", component_tok), renders)

and flow_renders_annotation (env : env) ((v1, v2) : CST.flow_renders_annotation)
    : type_ =
  let _renders =
    match v1 with
    | `Renders tok
    | `Rend_583ce82 tok
    | `Rend_bfd97c1 tok ->
        token env tok (* "renders", "renders?" or "renders*" *)
  in
  type_ env v2

and index_signature (env : env) ((v1, v2, v3, v4, v5) : CST.index_signature) =
  let _v1 =
    match v1 with
    | Some (`Opt_choice_DASH_read (v1, v2)) ->
        let v1 =
          match v1 with
          | Some (`DASH tok) -> Some (token env tok) (* "-" *)
          | Some (`PLUS tok) -> Some (token env tok) (* "+" *)
          | None -> None
        in
        let v2 = token env v2 (* "readonly" *) in
        Some (v1, v2)
        (* TODO add to AST *)
    | Some (`Flow_vari x) ->
        let _variance = flow_variance env x in
        None
    | None -> None
  in
  let v2 = token env v2 (* "[" *) in
  let v3 =
    match v3 with
    | `Choice_id_COLON_type (v2, v3, v4) ->
        let v2 = id_or_reserved_id env v2 in
        let v3 = token env v3 (* ":" *) in
        let v4 = type_ env v4 in
        TypeTodo (("IndexKey", v3), [ Type (TyName [ v2 ]); Type v4 ])
    | `Mapped_type_clause x -> mapped_type_clause env x
  in
  let _v4 = token env v4 (* "]" *) in
  let v5 =
    match v5 with
    | `Type_anno x -> type_annotation env x |> snd
    | `Omit_type_anno (v1, v2) ->
        let _v1_TODO = token env v1 (* "-?:" *) in
        let v2 = type_ env v2 in
        v2
    | `Adding_type_anno (v1, v2) ->
        let _v1 = token env v1 (* "+?:" *) in
        let v2 = type_ env v2 in
        v2
    | `Opting_type_anno (v1, v2) ->
        let _v1_TODO = token env v1 (* "?:" *) in
        let v2 = type_ env v2 in
        v2
  in
  TypeTodo (("Indexsig", v2), [ Type v3; Type v5 ])

and type_query (env : env) ((v1, v2) : CST.type_query) : type_ =
  let ttypeof = (* "typeof" *) token env v1 in
  let e =
    match v2 with
    | `Type_query_subs_exp x -> map_type_query_subscript_expression env x
    | `Type_query_member_exp x -> map_type_query_member_expression env x
    | `Type_query_call_exp x -> map_type_query_call_expression env x
    | `Type_query_inst_exp (v1, v2) ->
        let _v2 = type_arguments env v2 |> Tok.unbracket in
        map_anon_choice_import_c99ceb4 env v1
    | `Id tok ->
        let id = (* identifier *) str env tok in
        idexp_or_special id
    | `This tok -> this env tok
  in
  TypeTodo (("Typeof", ttypeof), [ Expr e ])

and map_anon_choice_type_id_43e1312 (env : env)
    (x : CST.anon_choice_type_id_43e1312) : expr =
  match x with
  | `Id tok ->
      let id = (* identifier *) str env tok in
      idexp_or_special id
  | `This tok -> this env tok
  | `Type_query_subs_exp x -> map_type_query_subscript_expression env x
  | `Type_query_member_exp x -> map_type_query_member_expression env x
  | `Type_query_call_exp x -> map_type_query_call_expression env x

and map_anon_choice_import_c99ceb4 (env : env)
    (x : CST.anon_choice_import_c99ceb4) : expr =
  match x with
  (* ?? what is that? *)
  | `Import tok ->
      let id = (* import *) str env tok in
      idexp_or_special id
  | (`Id _ | `Type_query_member_exp _ | `Type_query_subs_exp _) as x ->
      map_anon_choice_type_id_43e1312 env x

and map_type_query_call_expression (env : env)
    ((v1, v2) : CST.type_query_call_expression) : expr =
  match v1 with
  | `Import tok -> import_type_module env (token env tok (* import *)) v2
  | (`Id _ | `Type_query_member_exp _ | `Type_query_subs_exp _) as x ->
      let e = map_anon_choice_type_id_43e1312 env x in
      let args = arguments env v2 in
      Apply (e, args)

(* `import("m")` in a type refers to the module "m" and loads nothing, so it is
 * not a call: a pattern for a dynamic import does not match it. *)
and import_type_module (env : env) (timport : tok) (args : CST.arguments) :
    expr =
  ExprTodo (("Import", timport), Tok.unbracket (arguments env args))

and import_type (env : env)
    (x :
      [ `Type_query_member_exp_in_type_anno of
        CST.type_query_member_expression_in_type_annotation
      | `Type_query_call_exp_in_type_anno of
        CST.type_query_call_expression_in_type_annotation ]) : type_ =
  let timport, e =
    match x with
    | `Type_query_member_exp_in_type_anno x ->
        map_type_query_member_expression_in_type_annotation env x
    | `Type_query_call_exp_in_type_anno x ->
        map_type_query_call_expression_in_type_annotation env x
  in
  TypeTodo (("ImportType", timport), [ Expr e ])

(* A type imported from a module, `import("m").T`; the result carries the
 * `import` token. *)
and map_type_query_member_expression_in_type_annotation (env : env)
    ((v1, v2, v3) : CST.type_query_member_expression_in_type_annotation) :
    tok * expr =
  let timport, e = map_type_query_operand_in_type_annotation env v1 in
  let tdot = token env v2 (* "." *) in
  let fld = map_anon_choice_priv_prop_id_89abb74 env v3 in
  (timport, ObjAccess (e, (Dot, tdot), PN fld))

and map_type_query_call_expression_in_type_annotation (env : env)
    ((v1, v2) : CST.type_query_call_expression_in_type_annotation) : tok * expr
    =
  match v1 with
  | `Import tok ->
      let timport = token env tok (* import *) in
      (timport, import_type_module env timport v2)
  | `Type_query_member_exp_in_type_anno x ->
      let timport, e =
        map_type_query_member_expression_in_type_annotation env x
      in
      let args = arguments env v2 in
      (timport, Apply (e, args))

and map_type_query_operand_in_type_annotation (env : env)
    (x :
      [< `Import of Tree_sitter_run.Token.t
      | `Type_query_member_exp_in_type_anno of
        CST.type_query_member_expression_in_type_annotation
      | `Type_query_call_exp_in_type_anno of
        CST.type_query_call_expression_in_type_annotation ]) : tok * expr =
  match x with
  | `Import tok ->
      let id = str env tok (* import *) in
      (snd id, idexp_or_special id)
  | `Type_query_member_exp_in_type_anno x ->
      map_type_query_member_expression_in_type_annotation env x
  | `Type_query_call_exp_in_type_anno x ->
      map_type_query_call_expression_in_type_annotation env x

and map_type_query_member_expression (env : env)
    ((v1, v2, v3) : CST.type_query_member_expression) : expr =
  let e = map_anon_choice_type_id_43e1312 env v1 in
  let tdot = map_anon_choice_DOT_d88d0af env v2 in
  let fld = map_anon_choice_priv_prop_id_89abb74 env v3 in
  ObjAccess (e, (Dot, tdot), PN fld)

and map_type_query_subscript_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_query_subscript_expression) : expr =
  let e = map_anon_choice_type_id_43e1312 env v1 in
  let _v2TODO =
    match v2 with
    | Some tok -> Some ((* "?." *) token env tok)
    | None -> None
  in
  let lbra = (* "[" *) token env v3 in
  let arg =
    match v4 with
    | `Pred_type x ->
        (* ?? *)
        let id = predefined_type env x in
        (* TODO? ExprTodo (TyName id)? *)
        Id id
    | `Str x ->
        let s = string_ env x in
        L (String s)
    | `Num tok ->
        let n = (* number *) number env tok in
        L (Num n)
  in
  let rbra = (* "]" *) token env v5 in
  ArrAccess (e, (lbra, arg, rbra))

and unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v2 = expression env v2 in
  match v1 with
  | `BANG v1 ->
      let v1 = token env v1 (* "!" *) in
      Apply (IdSpecial (ArithOp G.Not, v1), fb [ v2 ])
  | `TILDE v1 ->
      let v1 = token env v1 (* "~" *) in
      Apply (IdSpecial (ArithOp G.BitNot, v1), fb [ v2 ])
  | `DASH v1 ->
      let v1 = token env v1 (* "-" *) in
      Apply (IdSpecial (ArithOp G.Minus, v1), fb [ v2 ])
  | `PLUS v1 ->
      let v1 = token env v1 (* "+" *) in
      Apply (IdSpecial (ArithOp G.Plus, v1), fb [ v2 ])
  | `Typeof v1 ->
      let v1 = token env v1 (* "typeof" *) in
      Apply (IdSpecial (Typeof, v1), fb [ v2 ])
  | `Void v1 ->
      let v1 = token env v1 (* "void" *) in
      Apply (IdSpecial (Void, v1), fb [ v2 ])
  | `Delete v1 ->
      let v1 = token env v1 (* "delete" *) in
      Apply (IdSpecial (Delete, v1), fb [ v2 ])

and pat_or_assign_pat (env : env) (x : CST.anon_choice_pat_3297d92) :
    (a_ident, a_pattern) Either.t =
  match x with
  | `Pat x -> pattern env x
  | `Assign_pat (v1, v2, v3) ->
      let pat = pattern env v1 in
      let _eq = token env v2 (* "=" *) in
      let _default = expression env v3 in
      pat

and formal_parameter (env : env) (x : CST.formal_parameter) : parameter =
  let parameter_of_id_or_pat id_or_pat opt_type opt_default =
    match id_or_pat with
    | Left (id, attrs) ->
        ParamClassic
          {
            p_name = id;
            p_default = opt_default;
            p_dots = None;
            p_type = opt_type;
            p_attrs = attrs;
          }
    | Right pat ->
        ParamPattern
          { pp_pattern = pat; pp_type = opt_type; pp_default = opt_default }
  in

  match x with
  | `Requ_param (v1, v2, v3) ->
      (* required_parameter *)
      let id_or_pat = parameter_name env v1 in
      let type_ =
        match v2 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let default = initializer_opt env v3 in
      parameter_of_id_or_pat id_or_pat type_ default
  | `Opt_param (v1, v2, v3, v4) ->
      (* optional_parameter *)
      let id_or_pat = parameter_name env v1 in
      let _questionmark_TODO = token env v2 (* "?" *) in
      let opt_type =
        match v3 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let opt_default = initializer_opt env v4 in
      parameter_of_id_or_pat id_or_pat opt_type opt_default
  | `Semg_ellips v1 ->
      let tok = token env v1 in
      ParamEllipsis tok

and formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) :
    parameter list bracket =
  let open_ = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some (v1, v2, v3) ->
        let params = map_sep_list env v1 v2 formal_parameter in
        let _trailing_comma = optional env v3 token (* "," *) in
        params
    | None -> []
  in
  let close = token env v3 (* ")" *) in
  (open_, params, close)

(* class Component<Props = any, State = any> { ... *)
and default_type (env : env) ((v1, v2) : CST.default_type) =
  let _v1 = token env v1 (* "=" *) in
  let v2 = type_ env v2 in
  v2

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Switch_case x -> switch_case env x
        | `Switch_defa x -> switch_default env x)
      v2
  in
  let _v3 = token env v3 (* "}" *) in
  v2

and mapped_type_clause (env : env) ((v1, v2, v3, v4) : CST.mapped_type_clause) =
  let id = str env v1 (* identifier *) in
  let tin = token env v2 (* "in" *) in
  let ty = type_ env v3 in
  let asopt =
    match v4 with
    | Some (v1, v2) ->
        let _tas = (* "as" *) token env v1 in
        let ty = type_ env v2 in
        [ Type ty ]
    | None -> []
  in
  TypeTodo (("MappedType", tin), [ Expr (Id id); Type ty ] @ asopt)

and statement1 (env : env) (x : CST.statement) : stmt =
  statement env x |> unsafe_stmt1

and statement (env : env) (x : CST.statement) : stmt list =
  match x with
  | `Export_stmt x ->
      let xs = export_statement env x in
      xs
  | `Import_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "import" *) in
      let import_tok = v1 in
      let binding =
        match v2 with
        | Some x ->
            let _tok = type_or_typeof env x in
            Binds_type
        | None -> Binds_value
      in
      let attributes = Option.map (import_attribute env) v4 in
      let v3 =
        match v3 with
        | `Import_clause_from_clause (v1, v2) ->
            let f = import_clause env v1 in
            let _t, from_path = from_clause env v2 in
            f ~binding ~attributes import_tok from_path
        | `Import_requ_clause x -> [ import_require_clause v1 env x ]
        | `Str x ->
            let file = string_ env x in
            [ ImportFile (import_tok, file, attributes) ]
      in
      let _v5 = semicolon env v5 in
      v3 |> List_.map (fun m -> M m)
  | `Debu_stmt (v1, v2) ->
      let v1 = identifier env v1 (* "debugger" *) in
      let v2 = semicolon env v2 in
      [ ExprStmt (idexp v1, v2) ]
  | `Exp_stmt x -> (
      (* tree-sitter parses a top-level 'namespace Foo { ... }' as an
       * expression-statement; intercept it so it becomes a proper module
       * definition instead of an IIFE. *)
      match x with
      | `Exp (`Inte_module im), _semi ->
          let id, opt_body = internal_module env im in
          [ DefStmt (basic_entity id, ModuleDef opt_body) ]
      | _ ->
          let e, t = expression_statement env x in
          [ ExprStmt (e, t) ])
  | `Decl x -> declaration env x
  | `Stmt_blk x -> [ statement_block env x ]
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "else" *) in
            let v2 = statement1 env v2 in
            Some v2
        | None -> None
      in
      [ If (v1, v2, v3, v4) ]
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = switch_body env v3 in
      [ Switch (v1, v2, v3) ]
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | `Choice_lexi_decl (`Lexi_decl x) ->
            let vars = lexical_declaration env x in
            Left vars
        | `Choice_lexi_decl (`Var_decl x) ->
            let vars = variable_declaration env x in
            Left vars
        | `Choice_lexi_decl (`Using_decl x) ->
            let vars = using_declaration env x in
            Left vars
        | `Choice_exp_SEMI (v1, v2) ->
            let _t = token env v2 (* ";" *) in
            Right (expressions env v1)
        | `Empty_stmt tok ->
            let _x = token env tok (* ";" *) in
            Left []
      in
      let v4 =
        match v4 with
        | `Choice_exp_SEMI (v1, v2) ->
            let e = expressions env v1 in
            let _t = token env v2 (* ";" *) in
            Some e
        | `Empty_stmt tok ->
            let _x = token env tok (* ";" *) in
            None
      in
      let v5 =
        match v5 with
        | Some x -> Some (expressions env x)
        | None -> None
      in
      let _v6 = token env v6 (* ")" *) in
      let v7 = statement1 env v7 in
      [ For (v1, ForClassic (v3, v4, v5), v7) ]
  | `For_in_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "for" *) in
      let _v2TODO =
        match v2 with
        | Some tok -> Some (token env tok) (* "await" *)
        | None -> None
      in
      let v3 = for_header env v3 in
      let v4 = statement1 env v4 in
      [ For (v1, v3, v4) ]
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      [ While (v1, v2, v3) ]
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement1 env v2 in
      let _v3 = token env v3 (* "while" *) in
      let v4 = parenthesized_expression env v4 in
      let _v5 = Option.map (semicolon env) v5 in
      [ Do (v1, v2, v4) ]
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = statement_block env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (catch_clause env x)
        | None -> None
      in
      let v4 =
        match v4 with
        | Some x -> Some (finally_clause env x)
        | None -> None
      in
      [ Try (v1, v2, v3, v4) ]
  | `With_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "with" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      [ With (v1, v2, v3) ]
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Break (v1, v2, v3) ]
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Continue (v1, v2, v3) ]
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        match v2 with
        | Some x -> Some (expressions env x)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Return (v1, v2, v3) ]
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expressions env v2 in
      let v3 = semicolon env v3 in
      [ Throw (v1, v2, v3) ]
  | `Empty_stmt tok -> [ empty_stmt env tok (* ";" *) ]
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = id_or_reserved_id env v1 in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement1 env v3 in
      [ Label (v1, v3) ]

and import_attribute (env : env) ((v1, v2) : CST.import_attribute) :
    import_attributes =
  let kwd =
    match v1 with
    | `With tok
    | `Assert tok ->
        str env tok (* "with" or "assert" *)
  in
  (kwd, object_ env v2)

and method_definition (env : env)
    ((v1, v2, v2bis, v3, v4, v5, v6, v7, v8, v9) : CST.method_definition) :
    property =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 = kwd_attr_opt_to_list env Static v2 in
  let v2bis = kwd_attr_opt_to_list env Override v2bis in
  let v3 = kwd_attr_opt_to_list env Readonly v3 in
  let v4 = kwd_attr_opt_to_list env Async v4 in
  let v5 =
    match v5 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v6 = property_name env v6 in
  let v7 =
    match v7 with
    (* indicates optional method? *)
    | Some tok -> [ (Optional, token env tok) ] (* "?" *)
    | None -> []
  in
  let _tparams, (v8, tret) = call_signature env v8 in
  let v9 = statement_block env v9 in
  let attrs = v1 @ v2 @ v2bis @ v3 @ v4 @ v5 @ v7 |> List_.map attr in
  let f_kind = (G.Method, fake) in
  let f =
    { f_attrs = []; f_params = v8; f_body = v9; f_rettype = tret; f_kind }
  in
  let e = Fun (f, None) in
  let ty = None in
  Field { fld_name = v6; fld_attrs = attrs; fld_type = ty; fld_body = Some e }

and class_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) : definition =
  let v1 = List_.map (decorator env) v1 in
  let v2 = token env v2 (* "class" *) in
  let v3 = identifier env v3 (* identifier *) in
  (* TODO types: type_parameters *)
  let _v4 =
    match v4 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let c_extends, c_implements =
    match v5 with
    | Some x -> class_heritage env x
    | None -> ([], [])
  in
  let v6 = class_body env v6 in
  let _v7 = automatic_semicolon_opt env v7 in
  let c =
    {
      c_kind = (G.Class, v2);
      c_extends;
      c_implements;
      c_body = v6;
      c_attrs = v1;
    }
  in
  (basic_entity v3, ClassDef c)

and array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) -> array_slots (anon_choice_exp_9818c1b env) v1 v2
    | None -> []
  in
  let v3 = token env v3 (* "]" *) in
  Arr (v1, v2, v3)

(* The form 'export { a, b as c } from "mod"' becomes one import directive
 * per kind, marked as a re-export so that it binds no local name in this
 * module. *)
and reexport_specifiers ~(binding : import_binding)
    (attributes : import_attributes option) (tok2, path) names =
  by_binding ~binding names (fun (kind : import_binding) group ->
      M (Import (tok2, Import_reexports, kind, group, path, attributes)))

(* `export * from 'foo'`, `export * as foo from 'foo'` and
 * `export { a } from 'foo'`, with their import attributes *)
and reexport (env : env) (export_tok : tok)
    (attributes : import_attributes option)
    (x :
      [< `STAR_from_clause of Tree_sitter_run.Token.t * CST.from_clause
      | `Name_export_from_clause of CST.namespace_export * CST.from_clause
      | `Export_clause_from_clause of CST.export_clause * CST.from_clause ]) :
    stmt list =
  match x with
  | `STAR_from_clause (v1, v2) ->
      (* export * from 'foo'; *)
      let star = token env v1 (* "*" *) in
      let from, path = from_clause env v2 in
      [
        M
          (ReExportNamespace
             (export_tok, Binds_value, star, None, from, path, attributes));
      ]
  | `Name_export_from_clause (v1, v2) ->
      (* export * as foo from "module"; *)
      let star, alias = namespace_export env v1 (* * as foo *) in
      let from, path = from_clause env v2 (* from "module" *) in
      [
        M
          (ReExportNamespace
             (export_tok, Binds_value, star, Some alias, from, path, attributes));
      ]
  | `Export_clause_from_clause (v1, v2) ->
      (* export { name1, name2, nameN } from 'foo'; *)
      let v1 = export_clause env v1 in
      let from = from_clause env v2 in
      reexport_specifiers ~binding:Binds_value attributes from v1

(* 'export { a, b as c }': export the local names (aliasing via a const). *)
and export_specifiers_local export_tok names =
  names
  |> List.concat_map (fun (_binding, (n1, n2opt)) ->
         match n2opt with
         | None -> [ M (Export (export_tok, n1)) ]
         | Some n2 ->
             let v = Ast_js.mk_const_var n2 (idexp n1) in
             [ DefStmt v; M (Export (export_tok, n2)) ])

and export_statement (env : env) (x : CST.export_statement) : stmt list =
  match x with
  | `Choice_choice_export_choice_STAR_from_clause_choice_auto_semi x -> (
      match x with
      | `Choice_export_choice_STAR_from_clause_choice_auto_semi x -> (
          match x with
          | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2, v3) ->
              let export_tok = token env v1 (* "export" *) in
              let v2 =
                match v2 with
                | ( `STAR_from_clause _
                  | `Name_export_from_clause _
                  | `Export_clause_from_clause _ ) as x ->
                    reexport env export_tok None x
                | `Export_clause v1 ->
                    (* export { import1 as name1, import2 as name2, nameN } from 'foo'; *)
                    let v1 = export_clause env v1 in
                    export_specifiers_local export_tok v1
              in
              let _v3 = semicolon env v3 in
              v2
          | `Rep_deco_export_choice_decl (v1, v2, v3) ->
              let decorators = List_.map (decorator env) v1 in
              let export_tok = token env v2 (* "export" *) in
              let v3 =
                match v3 with
                | `Decl x ->
                    definitions env x
                    |> List_.map (fun ((ent, defkind) : definition) ->
                           ({ ent with attrs = ent.attrs @ decorators }, defkind))
                    |> exported_definitions export_tok
                | `Defa_choice_decl (v1, v2) -> (
                    let tok_default (* TODO *) = token env v1 (* "default" *) in
                    match v2 with
                    | `Decl x ->
                        definitions env x
                        |> List_.map (fun ((ent, defkind) : definition) ->
                               ({ ent with attrs = ent.attrs @ decorators }, defkind))
                        |> default_exported_definitions export_tok tok_default
                    | `Exp_choice_auto_semi (v1, v2) ->
                        let e = expression env v1 in
                        let _semi = semicolon env v2 in
                        let def, n = Ast_js.mk_default_entity_def tok_default e in
                        [ DefStmt def; M (Export (export_tok, n)) ])
              in
              v3)
      | `Export_type_export_clause_opt_from_clause_choice_auto_semi
          (v1, v2, v3, v4, v5) ->
          (* export type { foo, type bar } [from 'mod']; we model type re-exports
           * like value re-exports (the type-only distinction is dropped). *)
          let export_tok = token env v1 (* "export" *) in
          let _type = token env v2 (* "type" *) in
          let names = export_clause env v3 in
          let _sc = semicolon env v5 in
          (match v4 with
          | Some from_cl ->
              reexport_specifiers ~binding:Binds_type None
                (from_clause env from_cl) names
          | None -> export_specifiers_local export_tok names)
      | `Export_EQ_exp_choice_auto_semi (v1, v2, v3, v4) ->
          (* export = ZipCodeValidator; (TS/CommonJS export assignment), modeled
           * like a default export of the expression. *)
          let export_tok = token env v1 (* "export" *) in
          let teq = token env v2 (* "=" *) in
          let e = expression env v3 in
          let _v4 = semicolon env v4 in
          let def, n = Ast_js.mk_default_entity_def teq e in
          [ DefStmt def; M (Export (export_tok, n)) ]
      | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) ->
          (* export as namespace mathLib; (UMD global). We record the exported
           * global name. *)
          let export_tok = token env v1 (* "export" *) in
          let _v2 = token env v2 (* "as" *) in
          let _v3 = token env v3 (* "namespace" *) in
          let id = identifier env v4 (* identifier *) in
          let _v5 = semicolon env v5 in
          [ M (Export (export_tok, id)) ])
  | `Export_type_choice_STAR_from_clause_choice_auto_semi (v1, v2, v3, v4, v5)
    ->
      (* export type * [as foo] from 'foo'; *)
      let export_tok = token env v1 (* "export" *) in
      let _type = token env v2 (* "type" *) in
      let star, alias =
        match v3 with
        | `STAR tok -> (token env tok (* "*" *), None)
        | `Name_export x ->
            let star, alias = namespace_export env x in
            (star, Some alias)
      in
      let from, path = from_clause env v4 in
      let _v5 = semicolon env v5 in
      [
        M
          (ReExportNamespace
             (export_tok, Binds_type, star, alias, from, path, None));
      ]
  | `Export_choice_STAR_from_clause_import_attr_choice_auto_semi (v1, v2, v3, v4)
    ->
      let export_tok = token env v1 (* "export" *) in
      let attributes = import_attribute env v3 in
      let _v4 = semicolon env v4 in
      reexport env export_tok (Some attributes) v2

and type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = token env v1 (* ":" *) in
  let v2 = type_ env v2 in
  (v1, v2)

and anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env)
    (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  List_.filter_map
    (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 =
        match v2 with
        | Some x -> Some (anon_choice_exp_9818c1b env x)
        | None -> None
      in
      v2)
    xs

and decorator_call_expression (env : env)
    ((v1, v2, v3) : CST.decorator_call_expression) =
  let v1 = anon_choice_type_id_b8f8ced env v1 in
  let _v2 =
    match v2 with
    | Some x -> type_arguments env x |> Tok.unbracket
    | None -> []
  in
  let v3 = arguments env v3 in
  (v1, v3)

and update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let op, t = anon_choice_PLUSPLUS_e498e28 env v2 in
      apply_postfix v1 (fun e ->
          Apply (IdSpecial (IncrDecr (op, G.Postfix), t), fb [ e ]))
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let op, t = anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = expression env v2 in
      Apply (IdSpecial (IncrDecr (op, G.Prefix), t), fb [ v2 ])

and anon_choice_export_stmt_42be789 (env : env)
    (x : CST.anon_choice_export_stmt_42be789) =
  match x with
  | `Export_stmt x ->
      let xs = export_statement env x in
      Right xs
  | `Prop_sign (v1, v2, v2bis, v3, v3bis, v4, v5, v6) ->
      let v1 = accessibility_modifier_opt_to_list env v1 in
      let v2 = kwd_attr_opt_to_list env Static v2 in
      let v2bis = kwd_attr_opt_to_list env Override v2bis in
      let v3 = kwd_attr_opt_to_list env Readonly v3 in
      let _variance = Option.map (flow_variance env) v3bis in
      let v4 = property_name env v4 in
      let v5 =
        match v5 with
        | Some tok -> [ (Optional, token env tok) ] (* "?" *)
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let attrs = v1 @ v2 @ v2bis @ v3 @ v5 |> List_.map attr in
      let fld =
        { fld_name = v4; fld_attrs = attrs; fld_type = v6; fld_body = None }
      in
      Left (Field fld)
  | `Call_sign_ x ->
      let x = bodiless_signature env x in
      let ty = mk_functype x in
      (* an (anonymous) call signature '(args): T'; name it after the call
       * operator, like C++ names 'operator()' *)
      let lp, _, _ = fst x in
      let name = PN ("()", lp) in
      let fld =
        { fld_name = name; fld_attrs = []; fld_type = Some ty; fld_body = None }
      in
      Left (Field fld)
  | `Cons_sign (v0, v1, v2, v3, v4) ->
      let v0 = kwd_attr_opt_to_list env Abstract v0 in
      let v1 = token env v1 (* "new" *) in
      let _tparams =
        match v2 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v3 = formal_parameters env v3 in
      let v4 =
        match v4 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let ty = mk_functype (v3, v4) in
      let fld_name = PN ("new", v1) in
      let fld_attrs = v0 |> List_.map attr in
      let fld = { fld_name; fld_attrs; fld_type = Some ty; fld_body = None } in
      Left (Field fld)
  | `Index_sign x ->
      let ty = index_signature env x in
      (* an (anonymous) index signature '[k: K]: V'; name it after the index
       * operator *)
      let _, lb, _, _, _ = x in
      let name = PN ("[]", token env lb) in
      let fld =
        { fld_name = name; fld_attrs = []; fld_type = Some ty; fld_body = None }
      in
      Left (Field fld)
  | `Meth_sign x ->
      let x = method_signature env x in
      Left (Field x)
  | `Flow_obj_type_spread (v1, v2) ->
      (* `...` (an inexact object type) and `...T` (the fields of T) add no
       * field of their own *)
      let _dots = token env v1 (* "..." *) in
      let _ty = Option.map (type_ env) v2 in
      Right []

and public_field_definition (env : env)
    ((v1, v3, v4, v5, v6, v7) : CST.public_field_definition) : property =
  let access_modif =
    match v1 with
    | Some (`Decl_opt_acce_modi (v1, v2)) ->
        let _tok_declare = token env v1 (* "declare" *) in
        accessibility_modifier_opt_to_list env v2
    | Some (`Acce_modi_opt_decl (v1, v2)) ->
        let _tok_declare = optional env v2 token in
        [ accessibility_modifier env v1 ]
    | None -> []
  in
  let modifier_and_accessor (kwd : keyword_attribute)
      (modifier_tok : Tree_sitter_run.Token.t)
      (accessor_tok : Tree_sitter_run.Token.t) : keyword_attribute wrap list =
    access_modif @ [ (kwd, token env modifier_tok); (Accessor, token env accessor_tok) ]
  in
  let attributes =
    match v3 with
    | `Opt_static_opt_over_modi_opt_read (v1, v2bis, v2) ->
        let v1 = kwd_attr_opt_to_list env Static v1 in
        let v2 = kwd_attr_opt_to_list env Readonly v2 in
        let v2bis = kwd_attr_opt_to_list env Override v2bis in
        access_modif @ v1 @ v2 @ v2bis
    | `Opt_abst_opt_read (v1, v2)
    | `Opt_read_opt_abst (v2, v1) ->
        let v1 = kwd_attr_opt_to_list env Abstract v1 in
        let v2 = kwd_attr_opt_to_list env Readonly v2 in
        access_modif @ v1 @ v2
    | `Opt_acce v1 -> access_modif @ kwd_attr_opt_to_list env Accessor v1
    | `Static_acce (v1, v2) -> modifier_and_accessor Static v1 v2
    | `Over_modi_acce (v1, v2) -> modifier_and_accessor Override v1 v2
    | `Abst_acce (v1, v2) -> modifier_and_accessor Abstract v1 v2
    | `Opt_static_flow_vari (v1, v2) ->
        let _variance = flow_variance env v2 in
        access_modif @ kwd_attr_opt_to_list env Static v1
  in
  let prop_name = property_name env v4 in
  let _question_or_exclam =
    match v5 with
    | Some x -> (
        match x with
        | `QMARK tok -> [ (Optional, token env tok) ] (* "?" *)
        | `BANG tok -> [ (NotNull, token env tok) ] (* "!" *))
    | None -> []
  in
  let opt_type =
    match v6 with
    | Some x -> Some (type_annotation env x |> snd)
    | None -> None
  in
  let opt_init = initializer_opt env v7 in
  let attrs = attributes |> List_.map attr in
  Field
    {
      fld_name = prop_name;
      fld_attrs = attrs;
      fld_type = opt_type;
      fld_body = opt_init;
    }

and lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration)
    : var list =
  let kind =
    match v1 with
    | `Let tok -> (Let, token env tok (* "let" *))
    | `Const tok -> (Const, token env tok (* "const" *))
  in
  let vars = map_sep_list env v2 v3 variable_declarator in
  let _v4 = semicolon env v4 in
  build_vars kind vars

(* `using x = e` and `await using x = e` declare bindings that are disposed
 * at the end of the enclosing block. *)
and using_declaration (env : env) ((v1, v2, v3, v4) : CST.using_declaration) :
    var list =
  let kind = using_kind env v1 in
  let vars = map_sep_list env v2 v3 variable_declarator in
  let _v4 = semicolon env v4 in
  build_vars kind vars

and using_kind (env : env) (x : CST.anon_choice_using_33bdb0e) : var_kind wrap
    =
  match x with
  | `Using tok -> (Using, token env tok (* "using" *))
  | `Await_using (v1, v2) ->
      let await_tok = token env v1 (* "await" *) in
      let using_tok = token env v2 (* "using" *) in
      (AwaitUsing, Tok.combine_toks await_tok [ using_tok ])

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) :
    parent list =
  let _textends = (* "extends" *) token env v1 in
  let v2 = map_extends_clause_single env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        map_extends_clause_single env v2)
      v3
  in
  v2 :: v3

and map_extends_clause_single (env : env) ((v1, v2) : CST.extends_clause_single)
    : parent =
  let v1 = expression env v1 in
  let v2 =
    match v2 with
    | Some x -> type_arguments env x |> Tok.unbracket
    | None -> []
  in
  tyname_or_expr_of_expr v1 v2

and enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = token env v1 (* "{" *) in
  let body =
    match v2 with
    | Some (v1, v2, v3) ->
        let body = map_sep_list env v1 v2 anon_choice_prop_name_6cc9e4b in
        let _v3 =
          match v3 with
          | Some tok -> Some (token env tok) (* "," *)
          | None -> None
        in
        body
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, body, v3)

and class_heritage (env : env) (x : CST.class_heritage) :
    parent list * type_ list =
  match x with
  | `Extends_clause_opt_imples_clause (v1, v2) ->
      let v1 = map_extends_clause env v1 in
      let v2 =
        match v2 with
        | Some x -> implements_clause env x
        | None -> []
      in
      (v1, v2)
  | `Imples_clause x ->
      let x = implements_clause env x in
      ([], x)

and property_name (env : env) (x : CST.property_name) =
  match x with
  | `Flow_symb_name tok ->
      (* `@@iterator`, Flow's name of a well-known symbol *)
      PN (str env tok)
  | `New tok -> PN (str env tok)
  | `Choice_choice_id x -> (
      match x with
      | `Choice_id x ->
          let id = id_or_reserved_id env x in
          PN id
      | `Priv_prop_id tok ->
          let id = str env tok in
          PN id
      | `Str x ->
          let s = string_ env x in
          PN s
      | `Num tok ->
          let n = number_as_string env tok (* number *) in
          PN n
      | `Comp_prop_name (v1, v2, v3) ->
          let _v1 = token env v1 (* "[" *) in
          let v2 = expression env v2 in
          let _v3 = token env v3 (* "]" *) in
          PN_Computed v2)

and switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = expressions env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 = List.concat_map (statement env) v4 in
  Case (v1, v2, stmt1 v3 v4)

and spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = token env v1 (* "..." *) in
  let v2 = expression env v2 in
  (v1, v2)

and expressions (env : env) (x : CST.expressions) : expr =
  match x with
  | `Exp x -> expression env x
  | `Seq_exp x -> sequence_expression env x

and abstract_method_signature (env : env)
    ((v1, v2, v2bis, v3, v4, v5, v6) : CST.abstract_method_signature) =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 = [ (Abstract, token env v2) ] (* "abstract" *) in
  let v2bis = kwd_attr_opt_to_list env Override v2bis in
  let v3 =
    match v3 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v4 = property_name env v4 in
  let v5 =
    match v5 with
    | Some tok -> [ (Optional, token env tok) ] (* "?" *)
    | None -> []
  in
  let attrs = v1 @ v2 @ v2bis @ v3 @ v5 |> List_.map attr in
  let x = bodiless_signature env v6 in
  let t = mk_functype x in
  { fld_name = v4; fld_attrs = attrs; fld_type = Some t; fld_body = None }

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = statement_block env v2 in
  (v1, v2)

and map_type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = identifier env tok (* identifier *) in
        idexp_or_special id
    | `This tok -> this env tok
    | `Pred_type v1 ->
        let id = predefined_type env v1 in
        idexp_or_special id
    | `Comp tok -> identifier env tok (* "component" *) |> idexp_or_special
  in
  let tis = token env v2 (* "is" *) in
  let ty = type_ env v3 in
  TypeTodo (("IsType", tis), [ Expr v1; Type ty ])

(* The type carries the token of the annotation's ":", or of "asserts" in a
 * function type, which has no ":". *)
and map_asserts (env : env) (tok : tok) ((v1, v2) : CST.asserts) : type_ =
  let _asserts = token env v1 (* "asserts" *) in
  let any =
    match v2 with
    | `Type_pred x -> Type (map_type_predicate env x)
    | `Id tok ->
        let id = identifier env tok (* identifier *) in
        Expr (idexp_or_special id)
    | `This tok -> Expr (this env tok)
  in
  TypeTodo (("Asserts", tok), [ any ])

(* A signature without a body reads its parameters as a function type does. *)
and bodiless_signature (env : env) (x : CST.call_signature) :
    parameter list bracket * type_ option =
  let _tparams, (params, tret) = call_signature env x in
  (function_type_parameters env params, tret)

and call_signature (env : env) ((v1, v2, v3) : CST.call_signature) :
    a_type_parameter list * (parameter list bracket * type_ option) =
  let v1 =
    match v1 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let v2 = formal_parameters env v2 in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `Type_anno x -> Some (type_annotation env x |> snd)
        | `Asserts_anno (v1, v2) ->
            let tcolon = token env v1 (* ":" *) in
            Some (map_asserts env tcolon v2)
        | `Type_pred_anno (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = map_type_predicate env v2 in
            Some v2
        | `Flow_pred_anno (v1, v2, v3, v4) ->
            (* `: T %checks`, a Flow predicate function *)
            let _v1 = token env v1 (* ":" *) in
            let v2 = Option.map (type_ env) v2 in
            let _v3 = token env v3 (* "%checks" *) in
            let _v4 = Option.map (parenthesized_expression env) v4 in
            v2)
    | None -> None
  in
  (v1, (v2, v3))

and object_ (env : env) ((v1, v2, v3) : CST.object_) : a_obj =
  let v1 = token env v1 (* "{" *) in
  let properties =
    match v2 with
    | Some (v1, v2) ->
        map_sep_list env v1 v2 (fun env x ->
            match x with
            | Some x -> [ object_property env x ]
            | None -> [])
        |> List_.flatten
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, properties, v3)

and type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) : type_ =
  let _e =
    match v1 with
    | `Id tok -> (* identifier *) identifier env tok |> idexp
    | `This tok -> (* "this" *) this env tok
    | `Pred_type v1 -> predefined_type env v1 |> idexp
    | `Comp tok -> identifier env tok (* "component" *) |> idexp
  in
  let _is = token env v2 (* "is" *) in
  let type_ = type_ env v3 in
  type_

and type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Prim_type x -> primary_type env x
  | `Func_type
      (`Opt_type_params_formal_params_EQGT_choice_type (v1, v2, v3, v4)) ->
      let _tparams =
        match v1 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let params = formal_parameters env v2 |> function_type_parameters env in
      let _arrow = token env v3 (* "=>" *) in
      let type_ =
        match v4 with
        | `Type x -> type_ env x
        | `Asserts ((v1, _) as x) -> map_asserts env (token env v1) x
        | `Type_pred x ->
            let type_ = type_predicate env x in
            type_
      in
      mk_functype (params, Some type_)
  | `Func_type (`Prim_type_EQGT_type (v1, v2, v3)) -> (
      (* Flow's `T => U`; with `T` a parenthesised name, TypeScript reads a
       * parameter called that name *)
      let arrow = token env v2 (* "=>" *) in
      let ret = Some (type_ env v3) in
      let type_only () : type_ =
        mk_functype
          ((arrow, [ ParamTypeOnly (None, primary_type env v1) ], arrow), ret)
      in
      match (env.extra.annotations, v1) with
      | `Typescript, `Choice_paren_type (`Paren_type (lp, x, rp)) -> (
          match function_type_parameter_name env x with
          | Some id ->
              mk_functype
                ( (token env lp, [ ParamClassic (mk_param id) ], token env rp),
                  ret )
          | None -> type_only ())
      | `Typescript, _
      | `Flow, _ ->
          type_only ())
  | `Func_type
      (`Opt_type_params_flow_func_type_params_EQGT_type (v1, v2, v3, v4)) ->
      let _tparams =
        match v1 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let params = flow_function_type_parameters env v2 in
      let _arrow = token env v3 (* "=>" *) in
      mk_functype (params, Some (type_ env v4))
  | `Read_type (v1, v2) ->
      let _TODOreadonly = token env v1 (* "readonly" *) in
      let type_ = type_ env v2 in
      type_
  | `Cons_type (v0, v1, v2, v3, v4, v5) ->
      let _v0TODO = kwd_attr_opt_to_list env Abstract v0 in
      let v1 = token env v1 (* "new" *) in
      let _tparams =
        match v2 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v3 = formal_parameters env v3 in
      let _v4 = token env v4 (* "=>" *) in
      let v5 = type_ env v5 in
      let ty = mk_functype (v3, Some v5) in
      TypeTodo (("New", v1), [ Type ty ])
  | `Infer_type x -> map_infer_type env x

and map_infer_type (env : env) ((v1, v2, v3) : CST.infer_type) : type_ =
  let v1 = token env v1 (* "infer" *) in
  let v2 = identifier env v2 (* identifier *) in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _textends = token env v1 (* "extends" *) in
        [ Type (type_ env v2) ]
    | None -> []
  in
  TypeTodo (("Infer", v1), Type (TyName [ v2 ]) :: v3)

and type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) :
    a_type_parameter list =
  let _v1 = token env v1 (* "<" *) in
  let type_params = map_sep_list env v2 v3 type_parameter in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _v5 = token env v5 (* ">" *) in
  type_params

and constraint_ (env : env) ((v1, v2) : CST.constraint_) :
    a_type_parameter_constraint =
  let _v1 =
    match v1 with
    | `Extends tok -> token env tok (* "extends" *)
    | `COLON tok -> token env tok
    (* ":" *)
  in
  let v2 = type_ env v2 in
  v2

and parameter_name (env : env) ((v1, v2, v2bis, v3, v4) : CST.parameter_name) :
    (a_ident * attribute list, a_pattern) Either.t =
  let decorators = List_.map (decorator env) v1 in
  let accessibility =
    accessibility_modifier_opt_to_list env v2
    |> List_.map (fun attr -> KeywordAttr attr)
  in
  let override =
    kwd_attr_opt_to_list env Override v2bis
    |> List_.map (fun attr -> KeywordAttr attr)
  in
  let readonly =
    kwd_attr_opt_to_list env Readonly v3
    |> List_.map (fun attr -> KeywordAttr attr)
  in
  let id_or_pat =
    match v4 with
    | `Pat x -> pattern env x
    | `This tok ->
        (* treating 'this' as a regular identifier for now *)
        let id = identifier env tok (* "this" *) in
        Left id
  in
  id_or_pat
  |> Either.map_left (fun id ->
         (id, decorators @ accessibility @ override @ readonly))

and lhs_expression (env : env) (x : CST.lhs_expression) : expr =
  match x with
  | `Choice_member_exp x -> (
      match x with
      | `Member_exp x -> member_expression env x
      | `Subs_exp x -> subscript_expression env x
      | `Choice_unde x -> identifier_ env x
      | `Choice_decl x -> reserved_identifier env x |> idexp
      | `Dest_pat x -> destructuring_pattern env x)
  | `Non_null_exp x -> non_null_expression env x

and statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  let _v4 = automatic_semicolon_opt env v4 in
  Block (v1, v2, v3)

and function_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) : definition =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = identifier env v3 (* identifier *) in
  let _tparams, (v4, tret) = call_signature env v4 in
  let v5 = statement_block env v5 in
  let _v6 = automatic_semicolon_opt env v6 in
  let f_kind = (G.Function, v2) in
  let f =
    { f_attrs = v1; f_params = v4; f_body = v5; f_rettype = tret; f_kind }
  in
  (basic_entity v3, FuncDef f)

and anon_choice_type_id_940079a (env : env)
    (x : CST.anon_choice_type_id_940079a) : (a_ident, a_pattern) Either.t =
  match x with
  | `Id tok -> Left (identifier env tok) (* identifier *)
  | `Dest_pat x -> Right (destructuring_pattern env x)

and id_or_destructuring_pattern env x : (a_ident, a_pattern) Either.t =
  anon_choice_type_id_940079a env x

(* TODO: change type signature, return just a_pattern *)
and rest_pattern (env : env) ((v1, v2) : CST.rest_pattern) :
    tok * (a_ident, a_pattern) Either.t =
  let dots = token env v1 (* "..." *) in
  let e = lhs_expression env v2 in
  (dots, Right e)

and tuple_type_member (env : env) (x : CST.tuple_type_member) :
    tuple_type_member =
  match x with
  | `Tuple_param (v1, v2) ->
      (* tuple_parameter *)
      let _v1_TODO =
        match v1 with
        | `Id tok -> Left (identifier env tok) (* identifier *)
        | `Rest_pat x ->
            let _dots, id_or_pat = rest_pattern env x in
            id_or_pat
      in
      let v2 = type_annotation env v2 |> snd in
      TyTupMember v2
  | `Opt_tuple_param (v1, v2, v3) ->
      (* optional_tuple_parameter *)
      let _v1_TODO = identifier env v1 in
      let _v2_TODO = token env v2 (* "?" *) in
      let v3 = type_annotation env v3 |> snd in
      TyTupMember v3
  | `Opt_type (v1, v2) ->
      let v1 = type_ env v1 in
      let v2 = token env v2 (* "?" *) in
      TyTupOpt (v1, v2)
  | `Rest_type (v1, v2) ->
      let v1 = token env v1 (* "..." *) in
      let v2 = type_ env v2 in
      TyTupRest (v1, v2)
  | `Type x -> TyTupMember (type_ env x)

and method_signature (env : env)
    ((v1, v2, v2bis, v3, v4, v5, v6, v7, v8) : CST.method_signature) =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 = kwd_attr_opt_to_list env Static v2 in
  let v2bis = kwd_attr_opt_to_list env Override v2bis in
  let v3 = kwd_attr_opt_to_list env Readonly v3 in
  let v4 = kwd_attr_opt_to_list env Async v4 in
  let v5 =
    match v5 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v6 = property_name env v6 in
  let v7 = kwd_attr_opt_to_list env Optional v7 in
  let attrs = v1 @ v2 @ v2bis @ v3 @ v4 @ v5 @ v7 |> List_.map attr in
  let x = bodiless_signature env v8 in
  let t = mk_functype x in
  { fld_name = v6; fld_attrs = attrs; fld_type = Some t; fld_body = None }

(* TODO: types *)
(* This covers mostly type definitions but includes also javascript constructs
   like function parameters, so it will be called even if we ignore types. *)
and declaration (env : env) (x : CST.declaration) : stmt list =
  match x with
  | `Choice_choice_func_decl (`Import_alias (v1, v2, v3, v4, v5)) ->
      let import_tok = token env v1 in
      let alias = identifier env v2 in
      let _eq = token env v3 in
      let target = id_or_nested_id env v4 in
      let _semi = semicolon env v5 in
      [ M (ImportAlias (import_tok, alias, target)) ]
  | `Choice_choice_func_decl
      (`Ambi_decl
        (`Decl_choice_export_choice_decl (_declare, `Export_choice_decl (v1, v2))))
    -> (
      let export_tok = token env v1 (* "export" *) in
      let defs = definitions env x in
      match v2 with
      | `Defa_choice_decl (v1, `Decl _) ->
          let default_tok = token env v1 (* "default" *) in
          default_exported_definitions export_tok default_tok defs
      | `Decl _
      | `Defa_choice_decl (_, `Type_choice_auto_semi _)
      | `Flow_comp_sign _
      | `Flow_hook_sign _ ->
          exported_definitions export_tok defs)
  | _ -> definitions env x |> List_.map (fun def -> DefStmt def)

and definitions (env : env) (x : CST.declaration) : definition list =
  match x with
  | `Choice_choice_func_decl x -> (
      match x with
      | `Choice_func_decl x -> (
          match x with
          | `Func_decl x -> [ function_declaration env x ]
          | `Gene_func_decl x -> [ generator_function_declaration env x ]
          | `Class_decl x -> [ class_declaration env x ]
          | `Lexi_decl x -> lexical_declaration env x |> vars_to_defs
          | `Var_decl x -> variable_declaration env x |> vars_to_defs)
      | `Func_sign (v1, v2, v3, v4, v5) ->
          let _v1 =
            match v1 with
            | Some tok -> [ (Async, token env tok) ] (* "async" *)
            | None -> []
          in
          let v2 = token env v2 (* "function" *) in
          let v3 = identifier env v3 (* identifier *) in
          let x = bodiless_signature env v4 in
          let ty = mk_functype x in
          let _v5 =
            match v5 with
            | `Choice_auto_semi x -> semicolon env x
            | `Func_sign_auto_semi tok -> token env tok
          in
          [
            ( basic_entity v3,
              (* DefTodo? *)
              VarDef { v_kind = (Const, v2); v_init = None; v_type = Some ty } );
          ]
      | `Abst_class_decl (v1, v2, v3, v4, v5, v6, v7) ->
          let _v1_TODO = List_.map (decorator env) v1 in
          let v2 = attr (Abstract, token env v2) (* "abstract" *) in
          let v3 = token env v3 (* "class" *) in
          let v4 = identifier env v4 (* identifier *) in
          let _tparams =
            match v5 with
            | Some x -> type_parameters env x
            | None -> []
          in
          let c_extends, c_implements =
            match v6 with
            | Some x -> class_heritage env x
            | None -> ([], [])
          in
          let v7 = class_body env v7 in
          let attrs = [ v2 ] in
          let c =
            {
              c_kind = (G.Class, v3);
              c_extends;
              c_implements;
              c_body = v7;
              c_attrs = attrs;
            }
          in
          [ (basic_entity v4, ClassDef c) ]
      | `Module (v1, v2) ->
          (* 'module Foo { ... }' (ambient module, often in .d.ts) *)
          let _v1 = token env v1 (* "module" *) in
          let id, opt_body = module__ env v2 in
          [ (basic_entity id, ModuleDef opt_body) ]
      | `Inte_module x ->
          (* 'namespace Foo { ... }' *)
          let id, opt_body = internal_module env x in
          [ (basic_entity id, ModuleDef opt_body) ]
      | `Type_alias_decl (v1, v2, v3, v4, v5, v6) ->
          let typekwd = token env v1 (* "type" *) in
          let id = str env v2 (* identifier *) in
          let _tparamsTODO =
            match v3 with
            | Some x -> type_parameters env x
            | None -> []
          in
          let _teq = token env v4 (* "=" *) in
          let ty = type_ env v5 in
          let sc = semicolon env v6 in
          let ent = basic_entity id in
          [ (ent, DefTodo (("typedef", typekwd), [ Type ty; Tk sc ])) ]
      | `Enum_decl (v1, v2, v3, v4) ->
          let _v1 =
            match v1 with
            | Some tok -> [ token env tok ] (* "const" *)
            | None -> []
          in
          let _v2 = token env v2 (* "enum" *) in
          let _v3 = identifier env v3 (* identifier *) in
          let _v4 = enum_body env v4 in
          []
          (* TODO *)
      | `Inte_decl (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "interface" *) in
          let v2 = identifier env v2 (* identifier *) in
          let _v3 =
            match v3 with
            | Some x -> type_parameters env x
            | None -> []
          in
          let v4 =
            match v4 with
            | Some x -> map_extends_type_clause env x
            | None -> []
          in
          let t1, xs, t2 = object_type env v5 in
          let xs =
            xs
            |> List_.filter_map (function
                 | Left fld -> Some fld
                 | Right _sts -> None)
          in
          let c =
            {
              c_kind = (G.Interface, v1);
              c_extends = v4;
              c_implements = [];
              c_body = (t1, xs, t2);
              c_attrs = [];
            }
          in
          [ (basic_entity v2, ClassDef c) ]
      | `Import_alias _ -> []
      | `Ambi_decl (`Decl_choice_export_choice_decl (v1, v2)) -> (
          let _v1 = token env v1 (* "declare" *) in
          match v2 with
          | `Export_choice_decl (v1, `Decl x) ->
              let _export = token env v1 (* "export" *) in
              definitions env x
          | `Export_choice_decl (v1, `Defa_choice_decl (v2, `Decl x)) ->
              let _export = token env v1 (* "export" *) in
              let _default = token env v2 (* "default" *) in
              definitions env x
          | `Export_choice_decl
              (v1, `Defa_choice_decl (v2, `Type_choice_auto_semi (v3, v4))) ->
              (* `declare export default T;` *)
              let _export = token env v1 (* "export" *) in
              let tdefault = token env v2 (* "default" *) in
              let ty = type_ env v3 in
              let _sc = semicolon env v4 in
              [
                ( basic_entity (default_entity, tdefault),
                  VarDef
                    {
                      v_kind = (Const, tdefault);
                      v_init = None;
                      v_type = Some ty;
                    } );
              ]
          | `Export_choice_decl (v1, `Flow_comp_sign x) ->
              let _export = token env v1 (* "export" *) in
              flow_component_signature env x
          | `Export_choice_decl (v1, `Flow_hook_sign x) ->
              let _export = token env v1 (* "export" *) in
              flow_hook_signature env x
          | `Flow_comp_sign x -> flow_component_signature env x
          | `Flow_hook_sign x -> flow_hook_signature env x)
      | `Ambi_decl (`Decl_choice_decl (v1, v2)) ->
          let _v1 = token env v1 (* "declare" *) in
          let v2 =
            match v2 with
            | `Decl x -> definitions env x
            | `Global_stmt_blk (v1, v2) ->
                let v1 = token env v1 (* "global" *) in
                let v2 = statement_block env v2 in
                let name = ("!global!", v1) in
                let f_kind = (G.LambdaKind, fake) in
                let f =
                  {
                    f_attrs = [];
                    f_params = fb [];
                    f_body = v2;
                    f_rettype = None;
                    f_kind;
                  }
                in
                (* TODO: DefTodo *)
                [
                  ( basic_entity name,
                    VarDef
                      {
                        v_kind = (Const, v1);
                        v_init = Some (Fun (f, None));
                        v_type = None;
                      } );
                ]
            | `Module_DOT_id_COLON_type_choice_auto_semi (v1, v2, v3, v4, v5, v6) ->
                let tok_module = token env v1 (* "module" *) in
                let _dot = token env v2 (* "." *) in
                let name = identifier env v3 (* identifier *) in
                let _colon = token env v4 (* ":" *) in
                let type_ = type_ env v5 in
                let _semi = semicolon env v6 in
                (* TODO: DefTodo *)
                [
                  ( basic_entity name,
                    VarDef
                      {
                        v_kind = (Const, tok_module);
                        v_init = None;
                        v_type = Some type_;
                      } );
                ]
          in
          v2)
  | `Using_decl x -> using_declaration env x |> vars_to_defs
  | `Flow_opaque_type_decl (v1, v2, v3, v4, v5, v6, v7) ->
      (* `opaque type T: S = U` hides U outside its module *)
      let v1 = token env v1 (* "opaque" *) in
      let _v2 = token env v2 (* "type" *) in
      let id = str env v3 (* identifier *) in
      let _tparams =
        match v4 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let supertype =
        match v5 with
        | Some x -> [ Type (type_annotation env x |> snd) ]
        | None -> []
      in
      let value =
        match v6 with
        | Some x -> [ Type (default_type env x) ]
        | None -> []
      in
      let sc = semicolon env v7 in
      [
        ( basic_entity id,
          DefTodo (("opaque", v1), supertype @ value @ [ Tk sc ]) );
      ]
  | `Flow_comp_decl (v1, v2, v3, v4, v5, v6) ->
      (* A component is a function of one props object, so its parameters
       * are the properties of an object pattern. *)
      let v1 = token env v1 (* "component" *) in
      let v2 = identifier env v2 (* identifier *) in
      let _tparams =
        match v3 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let params = flow_component_parameters env v4 in
      let renders = Option.map (flow_renders_annotation env) v5 in
      let body = statement_block env v6 in
      let f =
        {
          f_attrs = [ attr (Component, v1) ];
          f_params =
            fb
              [
                ParamPattern
                  {
                    pp_pattern = Obj params;
                    pp_type = None;
                    pp_default = None;
                  };
              ];
          f_body = body;
          f_rettype = renders;
          f_kind = (G.Function, v1);
        }
      in
      [ (basic_entity v2, FuncDef f) ]
  | `Flow_hook_decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "hook" *) in
      let v2 = identifier env v2 (* identifier *) in
      let _tparams, (params, tret) = call_signature env v3 in
      let body = statement_block env v4 in
      let f =
        {
          f_attrs = [ attr (Hook, v1) ];
          f_params = params;
          f_body = body;
          f_rettype = tret;
          f_kind = (G.Function, v1);
        }
      in
      [ (basic_entity v2, FuncDef f) ]

(* A function-type parameter without a type annotation is a type in Flow,
 * `(Foo) => void` takes a `Foo`, and a name in TypeScript, `(string) => void`
 * takes a parameter called `string`. *)
and function_type_parameters (env : env)
    ((open_, params, close) : parameter list bracket) : parameter list bracket
    =
  let param (x : parameter) : parameter =
    match (env.extra.annotations, x) with
    | ( `Flow,
        ParamClassic
          { p_name; p_type = None; p_default = None; p_dots; p_attrs = [] } ) ->
        ParamTypeOnly (p_dots, TyName [ p_name ])
    | ( `Flow,
        ParamPattern
          {
            pp_pattern = Apply (IdSpecial (Spread, dots), (_, [ Id name ], _));
            pp_type = None;
            pp_default = None;
          } ) ->
        ParamTypeOnly (Some dots, TyName [ name ])
    | `Flow, _
    | `Typescript, _ ->
        x
  in
  (open_, List_.map param params, close)

(* The name that TypeScript reads in a function-type parameter that the grammar
 * parsed as a type. `void` is a reserved word, so no parameter carries it. *)
and function_type_parameter_name (env : env) (x : CST.type_) : a_ident option =
  match x with
  | `Prim_type (`Choice_paren_type (`Pred_type (`Void _ | `Unique_symb _))) ->
      None
  | `Prim_type (`Choice_paren_type (`Pred_type x)) ->
      Some (predefined_type env x)
  | `Prim_type (`Choice_paren_type (`Id tok)) -> Some (identifier env tok)
  | _ -> None

(* Flow function type parameters are types, or carry names as in
 * TypeScript. *)
and flow_function_type_parameters (env : env)
    ((v1, v2, v3, v4, v5) : CST.flow_function_type_parameters) :
    parameter list bracket =
  let param (x : CST.anon_choice_type_1c27c93) : parameter =
    match x with
    | `Type x -> (
        match (env.extra.annotations, function_type_parameter_name env x) with
        | `Typescript, Some id -> ParamClassic (mk_param id)
        | `Typescript, None
        | `Flow, _ ->
            ParamTypeOnly (None, type_ env x))
    | `Rest_type (v1, v2) ->
        let dots = token env v1 (* "..." *) in
        ParamTypeOnly (Some dots, type_ env v2)
    | `Requ_param x -> formal_parameter env (`Requ_param x)
    | `Opt_param x -> formal_parameter env (`Opt_param x)
  in
  let v1 = token env v1 (* "(" *) in
  let first = param v2 in
  let rest = List_.map (fun (_comma, x) -> param x) v3 in
  let _trailing_comma = v4 in
  let v5 = token env v5 (* ")" *) in
  (v1, first :: rest, v5) |> function_type_parameters env

and flow_component_signature (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.flow_component_signature) :
    definition list =
  let v1 = token env v1 (* "component" *) in
  let v2 = identifier env v2 (* identifier *) in
  let _tparams =
    match v3 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let ty = flow_component_type env v1 v4 v5 in
  let _sc = semicolon env v6 in
  [
    ( basic_entity v2,
      VarDef { v_kind = (Const, v1); v_init = None; v_type = Some ty } );
  ]

and flow_hook_signature (env : env)
    ((v1, v2, v3, v4) : CST.flow_hook_signature) : definition list =
  let v1 = token env v1 (* "hook" *) in
  let v2 = identifier env v2 (* identifier *) in
  let x = bodiless_signature env v3 in
  let _sc = semicolon env v4 in
  [
    ( basic_entity v2,
      VarDef
        { v_kind = (Const, v1); v_init = None; v_type = Some (mk_functype x) } );
  ]

and flow_component_parameters (env : env)
    ((v1, v2, v3) : CST.flow_component_parameters) : property list bracket =
  let v1 = token env v1 (* "(" *) in
  let props =
    match v2 with
    | Some (v1, v2, _trailing_comma) ->
        map_sep_list env v1 v2 flow_component_parameter
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, props, v3)

and flow_component_parameter (env : env)
    (x : CST.anon_choice_flow_comp_param_10f9d4d) : property =
  match x with
  | `Flow_comp_param (v1, v2, v3, v4, v5) -> (
      let name =
        match v1 with
        | `Id tok -> identifier env tok
        | `Choice_decl x -> reserved_identifier env x
        | `Str x -> string_ env x
      in
      let local =
        match v2 with
        | Some (_as, x) -> Some (id_or_destructuring_pattern env x)
        | None -> None
      in
      let attrs =
        match v3 with
        | Some tok -> [ attr (Optional, token env tok (* "?" *)) ]
        | None -> []
      in
      let ty = Option.map (fun x -> type_annotation env x |> snd) v4 in
      (* A generic pattern has no slot for a default value; the binding is
       * kept, as for `{a: b = 1}` in object patterns. *)
      let _default = Option.map (initializer_ env) v5 in
      let body =
        match local with
        | Some x -> sub_pattern x
        | None -> idexp name
      in
      FieldColon
        {
          fld_name = PN name;
          fld_attrs = attrs;
          fld_type = ty;
          fld_body = Some body;
        })
  | `Flow_comp_rest_param (v1, v2, _v3) ->
      let tok = token env v1 (* "..." *) in
      let pat = id_or_destructuring_pattern env v2 |> sub_pattern in
      FieldSpread (tok, pat)

and map_extends_type_clause (env : env) ((v1, v2, v3) : CST.extends_type_clause)
    : parent list =
  let _textends = (* "extends" *) token env v1 in
  let v2 = map_anon_choice_type_id_a85f573 env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_type_id_a85f573 env v2 in
        Right v2)
      v3
  in
  Right v2 :: v3

and map_anon_choice_type_id_a85f573 (env : env)
    (x : CST.anon_choice_type_id_a85f573) : type_ =
  match x with
  | `Id tok -> TyName [ (* identifier *) str env tok ]
  | `Nested_type_id x -> TyName (nested_type_identifier env x)
  | `Gene_type x -> generic_type env x

let toplevel env x = statement env x

let program (env : env) (x : CST.program) : any =
  match x with
  | `Opt_hash_bang_line_rep_stmt (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* pattern #!.* *)
        | None -> None
      in
      let v2 = List.concat_map (toplevel env) v2 in
      Program v2
  | `Switch_case v1 -> Partial (PartialSwitchCase (switch_case env v1))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

type dialect = [ `Typescript | `TSX ]

let guess_dialect opt_dialect file : dialect =
  match opt_dialect with
  | Some x -> x
  | None ->
      (* TODO: should remove the no_sem below, bug in ml_to_generic.ml *)
      if file =~ ".*\\.tsx" then (* nosem *)
        `TSX else `Typescript

type cst_result =
  ( CST_tree_sitter_typescript.program,
    CST_tree_sitter_typescript.extra )
  Tree_sitter_run.Parsing_result.t

let parse ~(annotations : annotations) ?dialect file =
  let debug = false in
  let contents = UFile.read_file file in
  H.wrap_parser
    (fun () ->
      let dialect = guess_dialect dialect !!file in
      match dialect with
      | `Typescript ->
          let cst =
            Tree_sitter_typescript.Parse.string ~src_file:!!file contents
          in
          (cst :> cst_result)
      | `TSX ->
          let cst = Tree_sitter_tsx.Parse.string ~src_file:!!file contents in
          (cst :> cst_result))
    (fun cst _extras ->
      let env =
        {
          H.file;
          conv = H.line_col_to_pos file;
          extra = { source = source_of_string contents; annotations };
        }
      in

      if debug then (
        Printexc.record_backtrace true;
        Boilerplate_tree_sitter_typescript.dump_tree cst);

      match program env cst with
      | Program p -> p
      | _ -> failwith "not a program")

let parse_pattern ~(annotations : annotations) str =
  H.wrap_parser
    (* TODO Should we use Tree_sitter_tsx so that we permit TSX constructs in
     * patterns? Or try both, since TSX is not strictly a superset? *)
      (fun () -> (Tree_sitter_typescript.Parse.string str :> cst_result))
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        {
          H.file;
          conv = H.line_col_to_pos_pattern str;
          extra = { source = source_of_string str; annotations };
        }
      in
      match program env cst with
      | Program ss -> Stmts ss
      | other -> other)
