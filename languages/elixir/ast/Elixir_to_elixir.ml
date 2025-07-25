(* Yoann Padioleau
 *
 * Copyright (c) 2023 Semgrep Inc.
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
open AST_elixir

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Transform "Raw" AST constructs in "Kernel" constructs (see AST_elixir.ml
 * top comment).
 *
 * references:
 *  - https://hexdocs.pm/elixir/Kernel.html
 *  - https://hexdocs.pm/elixir/Kernel.SpecialForms.html
 *)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
class ['self] visitor =
  let params_of_args (args : arguments bracket) : parameters =
      let l, (exprs, kwdargs), r = args in
      let xs =
        exprs
        |> List_.map (function
             | I id -> P { pname = id; pdefault = None }
             (* TODO: recognize default value with \\ *)
             | x -> OtherParamExpr x)
      in
      let ys =
        kwdargs
        |> List_.map (fun p ->
               (* TODO generate keyword parameter? *)
               (* HACK: If we find ... here, convert to P. But how about
                * `semgrep_ellipsis_metavar`, `$...ARG`? This will be done
                * later. *)
               match p with
                | Kw_expr (kwd, e) -> OtherParamPair (kwd, e)
                | Semg_ellipsis tok -> P { pname = IdEllipsis tok; pdefault = None })
      in
      (l, xs @ ys, r)
  in
  object (self : 'self)
    inherit [_] map

    method! visit_Call env (x : call) =
      match x with
      (* https://hexdocs.pm/elixir/Kernel.html#if/2
       * TODO? recognize also the compact form 'if(cond, :do then)' ?
       *)
      | ( I (Id ("if", tif)),
          (_, ([ e ], []), _),
          Some (tdo, (Body then_, xs), tend) ) -> (
          let cond = self#visit_expr env e in
          let then_ = self#visit_body env then_ in
          match xs with
          | [] -> S (If (tif, cond, tdo, then_, None, tend))
          | [ ((Else, telse), Body else_) ] ->
              let else_ = self#visit_body env else_ in
              S (If (tif, cond, tdo, then_, Some (telse, else_), tend))
          | _else_ ->
              (* TODO? warning about unrecognized form? failwith ? *)
              Call (self#visit_call env x))
      (* https://hexdocs.pm/elixir/Kernel.html#def/2
       * TODO: handle "implicit try" form
       *)
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ Call (I ident, args, None) ], []), _),
          Some (tdo, (Body body, []), tend) ) ->
          let body = self#visit_body env body in
          let params = params_of_args args in
          let def =
            {
              f_def = tdef;
              f_name = ident;
              f_params = params;
              f_body = (tdo, body, tend);
              f_is_private = String.equal def_str "defp";
            }
          in
          S (D (FuncDef def))
      (* https://hexdocs.pm/elixir/Kernel.html#defmodule/2 *)
      | ( I (Id ("defmodule", tdefmodule)),
          (_, ([ Alias mname ], []), _),
          Some (tdo, (Body body, []), tend) ) ->
          let body = self#visit_body env body in
          let def =
            {
              m_defmodule = tdefmodule;
              m_name = mname;
              m_body = (tdo, body, tend);
            }
          in
          S (D (ModuleDef def))
      | _else_ ->
          let x = self#visit_call env x in
          Call x
  end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let visitor_instance = new visitor
let map_program (x : program) : program =
  let env = () in
  visitor_instance#visit_program env x

let map_any (x : any) : any =
  (* alt: could also generate the visitors for any too *)
  match x with
  | Pr x -> Pr (map_program x)
