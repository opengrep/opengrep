(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module T = Parser_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to add extra "closing" tokens
 * for the grammar to remain simple. The closing tokens
 * are usually one NEWLINE and a few DEDENTs.
 *
 * This file:
 *  class A:
 *     def f():
 *        bar()
 *
 * is tokenized as (skipping the TCommentSpace):
 *  [class; A; :; NEWLINE; INDENT;
 *   def; f; (; ); :; NEWLINE; IDENT;
 *   bar; (; ); NEWLINE; #1
 *   DEDENT; DEDENT; #2
 *   NEWLINE; EOF
 *
 * Instead if bar() is replaced by '...', many .pyi files do not have
 * the trailing NEWLINE which causes some missing DEDENT which would
 * cause parsing errors. This is why for those files we must
 * insert the NEWLINE at #1, and matching DEDENT at #2.
 *
 * alt:
 *  - could insert those closing tokens during error recovery
 *  - could look at state.offset_stack when encounters EOF in the lexer
 *    and also pop and create the DEDENT.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec add_dedent_aux num ii xs =
  if num <= 0 then xs else T.DEDENT ii :: add_dedent_aux (num - 1) ii xs

let add_dedent num ii xs =
  if num <= 0 then xs
    (* this closes the small_stmt from the stmt_list in suite (see grammar)
     * which then can be reduced by the series of DEDENT created by
     * add_dedent_aux.
     *)
  else T.NEWLINE ii :: add_dedent_aux num ii xs

(*****************************************************************************)
(* Soft keywords: match / case (PEP 634)                                      *)
(*****************************************************************************)
(* `match` and `case` are *soft* keywords in Python 3.10+: they are normal
 * identifiers except at the head of a structural-pattern-matching statement.
 * The lexer therefore tokenizes them as plain NAMEs. This pass rewrites those
 * NAMEs into dedicated MATCH / CASE tokens only when the surrounding shape is
 * unambiguously a match-statement, leaving ordinary uses (a variable,
 * function, or attribute called `match` or `case`) untouched.
 *)

let is_trivia = function
  | T.TCommentSpace _
  | T.TComment _ ->
      true
  | _ -> false

(* True if the next non-trivia token starts a new statement.
 * Start-of-file is also a boundary (the caller seeds the flag with true).
 *)
let is_stmt_boundary = function
  | T.NEWLINE _
  | T.INDENT _
  | T.DEDENT _
  | T.SEMICOL _ ->
      true
  | _ -> false

let rec skip_trivia = function
  | t :: xs when is_trivia t -> skip_trivia xs
  | xs -> xs

(* True iff [xs], the tokens right after the colon of a candidate match
 * statement header, begins with NEWLINE INDENT NAME("case") (modulo trivia).
 *)
let block_starts_with_case xs =
  match skip_trivia xs with
  | T.NEWLINE _ :: xs -> (
      match skip_trivia xs with
      | T.INDENT _ :: xs -> (
          match skip_trivia xs with
          | T.NAME ("case", _) :: _ -> true
          | _ -> false)
      | _ -> false)
  | _ -> false

(* Scan forward at paren depth [depth] looking for a match-header COLON.
 * Returns [Some xs] where [xs] are the tokens after the colon, or [None]
 * if we reach a top-level NEWLINE / EOF first (i.e. not a header line).
 *)
let rec find_header_after_colon depth = function
  | [] -> None
  | (T.LPAREN _ | T.LBRACK _ | T.LBRACE _) :: xs ->
      find_header_after_colon (depth + 1) xs
  | (T.RPAREN _ | T.RBRACK _ | T.RBRACE _) :: xs when depth > 0 ->
      find_header_after_colon (depth - 1) xs
  (* unbalanced closer at top level: malformed, bail out *)
  | (T.RPAREN _ | T.RBRACK _ | T.RBRACE _) :: _ -> None
  | T.COLON _ :: xs when depth = 0 -> Some xs
  | T.NEWLINE _ :: _ when depth = 0 -> None
  | T.EOF _ :: _ -> None
  | _ :: xs -> find_header_after_colon depth xs

(* True iff the current indent [depth] is exactly one level under the
 * innermost active match body, which is where a statement-start
 * NAME("case") should be rewritten.
 *)
let at_case_level depth = function
  | d :: _ -> depth = d + 1
  | [] -> false

(* Walk the token stream once, rewriting soft-keyword NAMEs in place.
 *   - [depth]: current absolute INDENT depth (0 at top level);
 *   - [match_depths]: stack of depths at which each enclosing active match
 *     body was opened (innermost first). Frames are popped when we DEDENT
 *     back to or below their opening depth, which transparently handles
 *     nested match statements.
 *   - [prev_bound]: whether the next non-trivia token starts a statement.
 *)
let rec rewrite depth match_depths prev_bound = function
  | [] -> []
  | T.INDENT ii :: xs ->
      T.INDENT ii :: rewrite (depth + 1) match_depths true xs
  | T.DEDENT ii :: xs ->
      let depth' = depth - 1 in
      let match_depths' =
        match match_depths with
        | d :: ds when depth' <= d -> ds
        | _ -> match_depths
      in
      T.DEDENT ii :: rewrite depth' match_depths' true xs
  | (T.NAME ("match", ii) as t) :: xs when prev_bound -> (
      match find_header_after_colon 0 xs with
      | Some after when block_starts_with_case after ->
          T.MATCH ii :: rewrite depth (depth :: match_depths) false xs
      | _ -> t :: rewrite depth match_depths false xs)
  | T.NAME ("case", ii) :: xs
    when prev_bound && at_case_level depth match_depths ->
      T.CASE ii :: rewrite depth match_depths false xs
  | t :: xs ->
      let prev_bound' =
        if is_trivia t then prev_bound else is_stmt_boundary t
      in
      t :: rewrite depth match_depths prev_bound' xs

let rewrite_match_case toks = rewrite 0 [] true toks

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let fix_tokens toks =
  let toks = rewrite_match_case toks in
  let rec aux indent xs =
    match xs with
    | [ T.NEWLINE ii; T.EOF _ ] -> add_dedent indent ii xs
    | [ T.EOF ii ] -> add_dedent indent ii [ T.NEWLINE ii; T.EOF ii ]
    | [] -> raise Common.Impossible
    | x :: xs ->
        let new_indent =
          match x with
          | T.INDENT _ -> indent + 1
          | T.DEDENT _ -> indent - 1
          | _ -> indent
        in
        x :: aux new_indent xs
  in
  aux 0 toks
