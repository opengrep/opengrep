(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
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

(* Iterate every identifier each parameter binds. For plain
 * [Param]/[ParamRest] that is just the parameter's name. For
 * destructuring [ParamPattern] it is the synthetic [!!_implicit_param!]
 * followed by every leaf identifier inside the pattern — rules that
 * match against parameter-level metavariables expect each destructured
 * identifier to be visited as a potential source, independently of the
 * control-flow routing that goes through the implicit binder. *)
let fold :
    ('acc ->
    AST_generic.ident ->
    AST_generic.id_info ->
    AST_generic.expr option ->
    'acc) ->
    'acc ->
    IL.param list ->
    'acc =
 fun f acc params ->
  List.fold_left
    IL.(
      fun acc par ->
        match par with
        | Param { pname = name; pdefault }
        | ParamRest { pname = name; pdefault } ->
            f acc name.ident name.id_info pdefault
        | ParamPattern ({ pname = name; pdefault }, pat) ->
            let acc = f acc name.ident name.id_info pdefault in
            let ids = Visit_pattern_ids.visit (G.P pat) in
            List.fold_left
              (fun acc (id, pinfo) -> f acc id pinfo None)
              acc ids
        | IL.ParamFixme -> acc)
    acc params

(* Iterate exactly one identifier per parameter — the top-level
 * resolved binder (the [name_param]). Use this when the caller needs
 * positional arg-index correspondence to actual callsite arguments
 * (HOF signature extraction, lambda in-env setup), since enumerating
 * destructured leaves via [fold] would fabricate extra Arg entries
 * that no caller supplies. *)
let fold_top_level :
    ('acc ->
    AST_generic.ident ->
    AST_generic.id_info ->
    AST_generic.expr option ->
    'acc) ->
    'acc ->
    IL.param list ->
    'acc =
 fun f acc params ->
  List.fold_left
    IL.(
      fun acc par ->
        match par with
        | Param { pname = name; pdefault }
        | ParamRest { pname = name; pdefault }
        | ParamPattern ({ pname = name; pdefault }, _) ->
            f acc name.ident name.id_info pdefault
        | IL.ParamFixme -> acc)
    acc params
