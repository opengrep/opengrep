(*
 * Copyright (C) 2024 Opengrep contributors
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
open Fpath_.Operators
module CST = Tree_sitter_zig.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module G = AST_generic
module R = Raw_tree
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Zig parser using tree-sitter-lang/semgrep-zig and converting
 * directly to AST_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let todo (_env : env) _ = failwith "not implemented"
let str = H.str
let token = H.token
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* Generated from tree-sitter-zig/Boilerplate.ml — fill in incrementally. *)

let map_identifier (env : env) (tok : CST.identifier) : ident =
  str env tok

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let map_source_file (env : env) (x : CST.source_file) : G.stmt list =
  ignore (env, x);
  todo env x

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_zig.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      let stmts = map_source_file env cst in
      G.stmt_list_to_program stmts)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_zig.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      match map_source_file env cst with
      | [ s ] -> (
          match s.G.s with
          | ExprStmt (e, _) -> G.E e
          | _ -> G.S s)
      | stmts -> G.Ss stmts)
