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
let make_funcdef ~tdef ~ident ~params ~guard ~tdo ~body ~tend ~rescue ~def_str =
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
        f_rescue = rescue;
        f_is_private = String.equal def_str "defp";
    }
    in
    S (D (FuncDef [def]))

(* In Elixir, we can skip the arg list in function definition if it is empty.
 * We preprocess these cases to avoid code duplication in the visitor. *)
let normalize_function_header (x : call) : call =
  match x with
  |  (I (Id (( "def" | "defp" ), _)) as a),
     (l, ([ I ident ], kw), r),
     c ->
     a,
     (l, ([ Call (I ident, Tok.unsafe_fake_bracket ([], []), None) ], kw), r),
     c
  |  (I (Id (( "def" | "defp" ), _)) as a),
     (l, ([ When (I ident, wtok, wguard) ], kw), r),
     c ->
     a,
     (l, ([ When (Call (I ident, Tok.unsafe_fake_bracket ([], []), None), wtok, wguard) ], kw), r),
     c
  | _ -> x

class ['self] visitor =
  let params_of_args (args : arguments bracket) : parameters =
      let l, (exprs, kwdargs), r = args in
      let xs =
        exprs
        |> List_.map (function
             | I id -> P { pname = id; pdefault = None }
             (* In Elixir you can have a default value only for ident params (no pats) *)
             | BinaryOp (I id, (ODefault, tok), d) -> 
                 P { pname = id; pdefault = Some (tok, d) }
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

    method private for_clauses env (args : expr list) : for_clause list =
      List_.map (fun (arg : expr) ->
        match arg with
        | BinaryOp (pat, (OLeftArrow, tarrow), collection) ->
            let pat = self#visit_expr env pat in
            let collection = self#visit_expr env collection in
            ForGenerator (pat, tarrow, collection)
        | e ->
            let e = self#visit_expr env e in
            ForFilter e
      ) args

    method! visit_Call env (x : call) =
      match normalize_function_header x with
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
       * The optional extras (rescue/catch/after/else) are the "implicit try"
       * form; they are stored in f_rescue and translated to a Try block in
       * Elixir_to_generic.ml.
       *)
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ Call (I ident, args, None) ], []), _),
          Some (tdo, (Body body, extras), tend) ) ->
          let body = self#visit_body env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:None ~tdo:(Some tdo)
                       ~body ~tend:(Some tend) ~rescue:extras ~def_str
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ Call (I ident, args, None) ],
               [ Kw_expr ((X1 (do_kw, _), _tok_colon), body) ]), _),
          None) when String.starts_with ~prefix:"do:" do_kw ->
          let body = self#visit_expr env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:None ~tdo:None
                       ~body:([ body ]) ~tend:None ~rescue:[] ~def_str
      (* def foo(x) when guard do ... end *)
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ When (Call (I ident, args, None), _twhen, E guard) ], []), _),
          Some (tdo, (Body body, extras), tend) ) ->
          let guard = self#visit_expr env guard in
          let body = self#visit_body env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:(Some guard) ~tdo:(Some tdo)
                       ~body ~tend:(Some tend) ~rescue:extras ~def_str
      (* def foo(x) when guard, do: body *)
      | ( I (Id (( "def" | "defp" ) as def_str, tdef)),
          (_, ([ When (Call (I ident, args, None), _twhen, E guard) ],
               [ Kw_expr ((X1 (do_kw, _), _tok_colon), body) ]), _),
          None) when String.starts_with ~prefix:"do:" do_kw ->
          let guard = self#visit_expr env guard in
          let body = self#visit_expr env body in
          let params = params_of_args args in
          make_funcdef ~tdef ~ident ~params ~guard:(Some guard) ~tdo:None
                       ~body:([ body ]) ~tend:None ~rescue:[] ~def_str
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
      (* https://hexdocs.pm/elixir/Kernel.SpecialForms.html#throw/1 *)
      | ( I (Id ("throw", tthrow)), (_, ([ arg ], []), _), None ) ->
          let arg = self#visit_expr env arg in
          S (Throw (tthrow, arg))
      (* https://hexdocs.pm/elixir/Kernel.SpecialForms.html#try/1 *)
      | ( I (Id ("try", ttry)), (_, ([], []), _), Some do_block ) ->
          let do_block = self#visit_do_block env do_block in
          S (Try (ttry, do_block))
      (* https://hexdocs.pm/elixir/Kernel.SpecialForms.html#for/1
       * for pattern <- collection, filter, ... do body end *)
      | ( I (Id ("for", tfor)),
          (_, (args, _kwds), _),
          Some (tdo, (Body body, []), tend) ) ->
          let clauses = self#for_clauses env args in
          let body = self#visit_body env body in
          S (For (tfor, clauses, (tdo, body, tend)))
      (* for pattern <- collection, do: body (compact keyword form) *)
      | ( I (Id ("for", tfor)),
          (_, (args, kwds), _),
          None ) -> (
          match List.find_opt (fun (kwd : pair) ->
            match kwd with
            | Kw_expr ((X1 (do_kw, _), _), _)
              when String.starts_with ~prefix:"do:" do_kw -> true
            | _ -> false
          ) kwds with
          | Some (Kw_expr (_, body_expr)) ->
              let clauses = self#for_clauses env args in
              let body_expr = self#visit_expr env body_expr in
              let fake = Tok.unsafe_fake_tok "" in
              S (For (tfor, clauses, (fake, [ body_expr ], fake)))
          | _ ->
              let x = self#visit_call env x in
              Call x)
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
