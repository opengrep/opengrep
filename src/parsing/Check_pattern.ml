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

exception CheckFailure of string

let check lang ast =
  let error s = raise (CheckFailure s) in
  try Ok (check_pattern_metavars error lang ast) with
  | CheckFailure s -> Error s
