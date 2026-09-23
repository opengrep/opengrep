(* Opengrep authors
 *
 * Copyright (C) 2026 Opengrep authors
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

(* TODO: Now with HOFs we run the risk of shadowing... *)
(** A simplified version of 'AST_generic.parameter', we use 'Other' to
    represent parameter kinds that we do not support yet. We don't want to
    just remove those unsupported parameters because we rely on the position
    of a parameter to represent taint variables, see 'Taint.arg'. *)
type param =
  | P of string
  | POpt of string (* a parameter that has a default *)
  | PRest of string
  | PKwd of string (* takes a named argument only: Ruby 'sep:' *)
  | Other
[@@deriving eq, ord, show]

type params = param list

let show_param = function
  | P s -> s
  | POpt s -> s ^ "=_"
  | PRest s -> "*" ^ s (* Python syntax for "rest" params *)
  | PKwd s -> s ^ ":"
  | Other -> "_?"

let equal_params params1 params2 = List.equal equal_param params1 params2

let compare_params params1 params2 =
  List.compare compare_param params1 params2

let show_params params = params |> List_.map show_param |> String.concat ", "

let of_IL_params il_params =
  il_params
  |> List.filter (function
       | IL.ParamReceiver _ -> false
       | _ -> true)
  |> List_.map (function
       | IL.Param { pname = { ident = s, _; _ }; pdefault = Some _; _ } -> POpt s
       | IL.Param { pname = { ident = s, _; _ }; pdefault = None; _ } -> P s
       (* function signatures don't look into the shape of the argument. *)
       | IL.ParamRest { pname = { ident = s, _; _ }; _ } -> PRest s
       | IL.ParamKwd { pname = { ident = s, _; _ }; _ } -> PKwd s
       | IL.ParamPattern ({ pname = { ident = s, _; _ }; _ }, pat) -> (
           match pat with
           | AST_generic.PatId (name, _) -> P (fst name)
           | AST_generic.PatTyped (AST_generic.PatId (name, _), _) ->
               P (fst name)
           | _ -> P s)
       | IL.ParamReceiver _ -> Other (* filtered above *)
       | IL.ParamFixme -> Other)
