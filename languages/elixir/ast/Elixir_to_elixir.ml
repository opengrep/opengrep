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
let make_funcdef ~tdef ~ident ~params ~guard ~tdo ~body ~tend ~def_str =
    let f_body =
    match tdo, tend with
    | Some tdo, Some tend -> (tdo, body, tend)
    | _ -> Tok.unsafe_fake_bracket body
    in
    let def =
    {
        f_def = tdef;
        f_name = ident;
        f_params = params;
        f_guard = guard;
        f_body;
        f_is_private = String.equal def_str "defp";
    }
    in
    S (D (FuncDef [def]))

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
          make_funcdef ~tdef ~ident ~params ~guard:None ~tdo:(Some tdo)
                       ~body ~tend:(Some tend) ~def_str 
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ Call (I ident, args, None) ],
               [ Kw_expr ((X1 (do_kw, _), _tok_colon), body) ]), _),
          None) when String.starts_with ~prefix:"do:" do_kw ->
          let body = self#visit_expr env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:None ~tdo:None
                       ~body:([ body ]) ~tend:None ~def_str 
      (* def foo(x) when guard do ... end *)
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ When (Call (I ident, args, None), _twhen, E guard) ], []), _),
          Some (tdo, (Body body, []), tend) ) ->
          let guard = self#visit_expr env guard in
          let body = self#visit_body env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:(Some guard) ~tdo:(Some tdo)
                       ~body ~tend:(Some tend) ~def_str 
      (* def foo(x) when guard, do: body *)
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ When (Call (I ident, args, None), _twhen, E guard) ],
               [ Kw_expr ((X1 (do_kw, _), _tok_colon), body) ]), _),
          None) when String.starts_with ~prefix:"do:" do_kw ->
          let guard = self#visit_expr env guard in
          let body = self#visit_expr env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:(Some guard) ~tdo:None
                       ~body:([ body ]) ~tend:None ~def_str 
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
(* Grouping consecutive function clauses *)
(*****************************************************************************)

type func_clause_key = string * int * bool

let equal_func_clause_key (s1, i1, b1 : func_clause_key) (s2, i2, b2 : func_clause_key) : bool =
  String.equal s1 s2 && Int.equal i1 i2 && Bool.equal b1 b2

(* Key for grouping: same name, same arity, same visibility *)
let func_clause_key_opt (clause : function_definition) : func_clause_key option =
  let name = match clause.f_name with
    | Id (s, _) | IdMetavar (s, _) -> Some s
    | IdEllipsis _ -> None
  in
  match name with
  | None -> None
  | Some s ->
      let _, params, _ = clause.f_params in
      Some (s, List.length params, clause.f_is_private)

(* Group consecutive FuncDef stmts with same key into a single FuncDef *)
let rec group_func_clauses (stmts : stmts) : stmts =
  match stmts with
  | [] -> []
  | S (D (FuncDef [ clause ])) :: rest ->
      (match func_clause_key_opt clause with
       | None -> S (D (FuncDef [ clause ])) :: group_func_clauses rest
       | Some key ->
           let rec take_matching acc = function
             | S (D (FuncDef [ c ])) :: tl
               when Option.equal equal_func_clause_key (func_clause_key_opt c) (Some key) ->
                 take_matching (c :: acc) tl
             | remaining -> (acc, remaining)
           in
           let clauses, remaining = take_matching [ clause ] rest in
           S (D (FuncDef (List.rev clauses))) :: group_func_clauses remaining)
  | e :: rest -> e :: group_func_clauses rest

(* Recursively apply grouping into module and function bodies *)
let rec group_in_program (stmts : stmts) : stmts =
  stmts |> List_.map group_in_expr |> group_func_clauses

and group_in_expr (e : expr) : expr =
  match e with
  | S (D (ModuleDef md)) ->
      let tdo, body, tend = md.m_body in
      S (D (ModuleDef { md with m_body = (tdo, group_in_program body, tend) }))
  | S (D (FuncDef clauses)) ->
      let clauses =
        List_.map
          (fun c ->
            let tdo, body, tend = c.f_body in
            { c with f_body = (tdo, group_in_program body, tend) })
          clauses
      in
      S (D (FuncDef clauses))
  | _ -> e

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let visitor_instance = new visitor

let map_program (x : program) : program =
  let env = () in
  let x = visitor_instance#visit_program env x in
  group_in_program x

let map_any (x : any) : any =
  (* alt: could also generate the visitors for any too *)
  match x with
  | Pr x -> Pr (map_program x)
