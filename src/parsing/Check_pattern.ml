(* for these languages, we are sure that $x is an error *)
let lang_has_no_dollar_ids =
  Lang.(
    function
    | Apex
    | C
    | Circom
    | Cpp
    | Csharp
    | Dart
    | Elixir
    | Go
    | Html
    | Java
    | Json
    | Jsonnet
    | Julia
    | Kotlin
    | Lua
    | Move_on_aptos
    | Ocaml
    | Protobuf
    | Python
    | Python2
    | Python3
    | Ql
    | R
    | Scheme
    | Swift
    | Terraform
    | Vb
    | Xml
    | Yaml ->
        true
    | Bash
    | Cairo
    | Clojure
    | Dockerfile
    | Hack
    | Js
    | Crystal
    | Lisp
    | Move_on_sui
    | Php
    | Promql
    | Ruby
    | Rust
    | Scala
    | Solidity
    | Ts
    | Vue ->
        false)

class ['self] metavar_checker =
  object (_self : 'self)
    inherit [_] AST_generic.iter_no_id_info as super

    method! visit_ident (error, lang) id =
      let str, _tok = id in
      if
        str.[0] = '$'
        && (not (Mvar.is_metavar_name str))
        && not (Mvar.is_metavar_ellipsis str)
      then
        error
          (Common.spf
             "`%s' is neither a valid identifier in %s nor a valid \
              meta-variable"
             str (Lang.to_string lang));
      super#visit_ident (error, lang) id
  end

let metavar_checker_instance = new metavar_checker
let check_pattern_metavars error lang ast =
  if lang_has_no_dollar_ids lang then
    metavar_checker_instance#visit_any (error, lang) ast

(* A capture list, and a list of block-local variables, are matched in any
 * order, so the captures a pattern does not write cannot be split among two
 * [$...X]; a [$...X] stands for captures, not for one with an initialiser. *)
class ['self] capture_list_checker =
  object (_self : 'self)
    inherit [_] AST_generic.iter_no_id_info as super

    method! visit_captures error (captures : AST_generic.captures) =
      let is_metavar_ellipsis ((s, _) : AST_generic.ident) =
        Mvar.is_metavar_ellipsis s
      in
      let capture_metavars =
        captures.cexplicit
        |> List.filter_map (function
             | AST_generic.CaptureVar (_, id, _, _, init)
               when is_metavar_ellipsis id ->
                 Some (id, init)
             | AST_generic.CaptureVar _
             | AST_generic.CaptureThis _
             | AST_generic.CaptureEllipsis _ ->
                 None)
      in
      let local_metavars =
        captures.clocals |> List.filter (fun (id, _) -> is_metavar_ellipsis id)
      in
      (match capture_metavars with
      | _ :: _ :: _ ->
          error "a capture list holds at most one `$...X' metavariable"
      | _ -> ());
      (match local_metavars with
      | _ :: _ :: _ ->
          error
            "a list of block-local variables holds at most one `$...X' \
             metavariable"
      | _ -> ());
      capture_metavars
      |> List.iter (fun (((s, _) : AST_generic.ident), init) ->
             if Option.is_some init then
               error
                 (Common.spf "`%s' stands for captures and has no initialiser"
                    s));
      super#visit_captures error captures
  end

let capture_list_checker_instance = new capture_list_checker

exception CheckFailure of string

let check lang ast =
  let error s = raise (CheckFailure s) in
  try
    check_pattern_metavars error lang ast;
    Ok (capture_list_checker_instance#visit_any error ast)
  with
  | CheckFailure s -> Error s
