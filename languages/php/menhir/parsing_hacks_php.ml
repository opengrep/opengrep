(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)
open Common
open Parser_php
module TH = Token_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module transforms certain tokens like '>>', normally a T_SR
 * into two TGREATER tokens which helps avoid using ugly tricks in the grammar
 * regarding generics.
 *
 * This is similar to what we do for C/C++.
 * See cpp/.../parsing_hacks.ml for more information.
 *
 * In Hack they maintain those different states (InToplevel, InFunction,
 * InBlock, ...) in the lexer itself, I prefer for now to separate
 * concerns and do that entirely post-lexing (which introduces some performance
 * degradation, from 195s to parse www to 209s).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = { stack : ctx list; misc : unit }

and ctx =
  | Toplevel
  | ClassHeader
  | ClassBody
  | FunctionHeader
  | TypeHeader
  | UserAttribute
  | Block

(*****************************************************************************)
(* generics *)
(*****************************************************************************)

(* Split a single (assumed to be 2-chars wide) info and turn it
   into a (1-char) lhs and rhs. Used to convert `>>` into two `>`
*)
let split_two_char pi =
  let lhs = { pi with Tok.str = String.sub pi.Tok.str 0 1 } in
  let rhs =
    {
      Tok.str = String.sub pi.Tok.str 1 1;
      pos =
        { pi.pos with bytepos = pi.pos.bytepos + 1; column = pi.pos.column + 1 };
    }
  in
  (lhs, rhs)

let split_two_char_info i =
  let tok =
    match i with
    | Tok.OriginTok t -> t
    | _ -> failwith "Parse error..."
  in

  let lhspi, rhspi = split_two_char tok in
  let lhs = Tok.OriginTok lhspi in
  let rhs = Tok.OriginTok rhspi in
  (lhs, rhs)

(*
 * Utilities for lambda parsing
 *)

(*
 * Checks if the given tokens are compatible with a set of lambda params.
 * It must either be empty, as in () ==> ... or contain one variable/variadic.
 *
 * Both of these cases are not compatible with typehints, so we can safely
 * determine if a (...) expression is part of lambda's params or its typehint.
 *)
let is_params toks =
  List.length toks > 0
  && (List.for_all
        (function
          | T_LAMBDA_OPAR _
          | T_LAMBDA_CPAR _
          | TOPAR _
          | TCPAR _ ->
              true
          | x -> TH.is_comment x)
        toks
     || List.exists
          (function
            | T_VARIABLE _
            | T_ELLIPSIS _ ->
                true
            | _ -> false)
          toks)

(* Looks to see if the next token is a variable (ignoring comments) *)
let rec is_variable toks =
  match toks with
  | [] -> false
  | T_VARIABLE _ :: _ -> true
  | x :: xs -> if TH.is_comment x then is_variable xs else false

(*
 * Find the next group of parenthesized tokens, being sure to balance parens.
 * Returns an empty list if the parens were imbalanced or the first non-comment
 * token was anything except a close paren.
 *
 * Replaces the opening/closing parens with lambda parens if `replace` is true.
 *)
let find_paren_tokens toks replace =
  let rec aux toks acc depth =
    match toks with
    | [] -> ([], []) (* failure *)
    | x :: xs -> (
        match x with
        | TCPAR t ->
            let x' = if depth =|= 0 && replace then T_LAMBDA_CPAR t else x in
            aux xs (x' :: acc) (depth + 1)
        | TOPAR t ->
            if depth =|= 1 then
              let x' = if replace then T_LAMBDA_OPAR t else x in
              (List.rev (x' :: acc), xs)
            else aux xs (x :: acc) (depth - 1)
        | T_SR t ->
            if depth > 0 then
              if replace then
                (* In the context of lambda parens, >> only makes sense
                 * if we split it into two > tokens *)
                let lhs, rhs = split_two_char_info t in
                aux xs (TGREATER rhs :: TGREATER lhs :: acc) depth
              else aux xs (x :: acc) depth
            else ([], [])
        | _ ->
            if TH.is_comment x || depth > 0 then aux xs (x :: acc) depth
            else (* couldn't find the first closing paren *)
              ([], []))
  in
  aux toks [] 0

(*
 * Try to (roughly) match a lambda typehint - may have false positives.
 * On the other hand, it's guaranteed that any valid typehint will be matched.
 * False positives will most likely lead to an invalid set of lambda
 * parens, though.
 *)
let find_typehint toks =
  let rec aux toks acc depth =
    match toks with
    | [] -> ([], []) (* failure *)
    (* assume parens/brackets are balanced correctly *)
    | x :: xs -> (
        match x with
        | T_LAMBDA_CPAR _
        | TCPAR _
        | TGREATER _ ->
            aux xs (x :: acc) (depth + 1)
        | T_LAMBDA_OPAR _
        | TOPAR _
        | TSMALLER _ ->
            aux xs (x :: acc) (depth - 1)
        | T_SR t ->
            (* >> when we're looking for a typehint is only valid in the context
             * of closing a template, so split it up. *)
            let lhs, rhs = split_two_char_info t in
            aux xs (TGREATER rhs :: TGREATER lhs :: acc) (depth + 2)
        | T_DOUBLE_ARROW _
        | TOBRACE _
        | TCBRACE _ ->
            ([], []) (* absolutely will not be in a typehint *)
        | TCOLON _ ->
            if depth =|= 0 then (List.rev (x :: acc), xs)
            else aux xs (x :: acc) depth
        | _ -> aux xs (x :: acc) depth)
  in
  aux toks [] 0

(*****************************************************************************)
(* ampersands *)
(*****************************************************************************)

(* php-src decides in the lexer whether a '&' introduces a by-reference
 * variable or joins two types, by looking at whether a '$' or a '...' comes
 * next; intersection types accept only the latter spelling, by-reference
 * parameters only the former, and bitwise and either. Without the split,
 * 'f(X&Y $p)' and 'f(X &$p)' cannot be told apart until after the '&'.
 * ocamllex cannot look ahead, so the split is done here instead.
 * coupling: the "&" rules in Zend's zend_language_scanner.l
 *)
let split_ampersands xs =
  let rec followed_by_var_or_vararg = function
    | [] -> false
    | x :: xs ->
        if TH.is_comment x then followed_by_var_or_vararg xs
        else (
          match x with
          | T_VARIABLE _
          | T_METAVAR _
          | TDOLLAR _
          | TDOLLARDOLLAR _
          | T_ELLIPSIS _ ->
              true
          | _ -> false)
  in
  let rec aux acc xs =
    match xs with
    | [] -> List.rev acc
    | TAND ii :: rest ->
        let tok =
          if followed_by_var_or_vararg rest then TAND ii else TAND_NOT_VAR ii
        in
        aux (tok :: acc) rest
    | x :: rest -> aux (x :: acc) rest
  in
  aux [] xs

(*****************************************************************************)
(* heredocs *)
(*****************************************************************************)

(* Two things about a heredoc body are decided by its closing marker, which
 * the lexer only reaches once the body has been read:
 *
 *  - the newline before the marker is not part of the string;
 *  - since PHP 7.3 the marker may be indented, and that indentation comes off
 *    every line of the body.
 *
 * The body's own newlines are content rather than whitespace, so they are
 * turned into text here; the lexer emits them as TNewline, which the parser
 * would otherwise skip like any other newline.
 *)
let rec fix_heredocs xs =
  let shift n str ii =
    match ii with
    | Tok.OriginTok t ->
        Tok.OriginTok
          {
            Tok.str;
            pos =
              {
                t.Tok.pos with
                bytepos = t.Tok.pos.bytepos + n;
                column = t.Tok.pos.column + n;
              };
          }
    | _ -> Tok.rewrap_str str ii
  in
  (* how much of the marker's indentation this line actually has *)
  let strip_indent indent (str, ii) =
    let n = ref 0 in
    while
      !n < indent && !n < String.length str
      && (Char.equal str.[!n] ' ' || Char.equal str.[!n] '\t')
    do
      incr n
    done;
    let n = !n in
    if n =|= 0 then T_ENCAPSED_AND_WHITESPACE (str, ii)
    else
      let str' = String.sub str n (String.length str - n) in
      T_ENCAPSED_AND_WHITESPACE (str', shift n str' ii)
  in
  let indent_of_marker ii =
    match ii with
    | Tok.OriginTok t -> t.Tok.pos.column
    | _ -> 0
  in
  (* the body up to its own marker: a heredoc may be nested in a '{...}' of
   * this one, and its marker is not ours *)
  let rec body acc depth = function
    | T_END_HEREDOC ii :: rest when depth =|= 0 -> Some (List.rev acc, ii, rest)
    | (T_END_HEREDOC _ as x) :: rest -> body (x :: acc) (depth - 1) rest
    | (T_START_HEREDOC _ as x) :: rest -> body (x :: acc) (depth + 1) rest
    | [] -> None
    | x :: rest -> body (x :: acc) depth rest
  in
  (* the tokens of a '{...}' interpolation are code: a newline in one of them
   * is not part of the string, and nothing there is a line of the body *)
  let brace_depth depth x =
    match x with
    | T_CURLY_OPEN _
    | T_DOLLAR_OPEN_CURLY_BRACES _
    | TOBRACE _ ->
        depth + 1
    | TCBRACE _ -> depth - 1
    | _ -> depth
  in
  let rewrite indent toks =
    (* drop the line terminator that closes the last line: it precedes the
     * marker and is not part of the string. The body rule reads one
     * character at a time, so a CRLF arrives as two tokens.
     *)
    let is_nl ii str = String.equal (Tok.content_of_tok ii) str in
    let toks =
      match List.rev toks with
      | TNewline lf :: TNewline cr :: tl when is_nl lf "\n" && is_nl cr "\r" ->
          List.rev tl
      | TNewline _ :: tl -> List.rev tl
      | _ -> toks
    in
    let rec aux at_line_start depth acc = function
      | [] -> List.rev acc
      | TNewline ii :: tl when depth =|= 0 ->
          let str = Tok.content_of_tok ii in
          aux true depth (T_ENCAPSED_AND_WHITESPACE (str, ii) :: acc) tl
      | T_ENCAPSED_AND_WHITESPACE (str, ii) :: tl when depth =|= 0 && at_line_start
        ->
          aux false depth (strip_indent indent (str, ii) :: acc) tl
      | x :: tl -> aux false (brace_depth depth x) (x :: acc) tl
    in
    aux true 0 [] toks
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (T_START_HEREDOC _ as start) :: rest -> (
        match body [] 0 rest with
        | None -> List.rev_append (start :: acc) rest
        | Some (toks, marker_ii, rest) ->
            (* a heredoc nested in an interpolation is corrected on its own *)
            let toks = rewrite (indent_of_marker marker_ii) (fix_heredocs toks) in
            aux
              (T_END_HEREDOC marker_ii :: List.rev_append toks (start :: acc))
              rest)
    | x :: rest -> aux (x :: acc) rest
  in
  aux [] xs

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens xs =
  let rec aux env acc xs =
    match xs with
    (* need an acc, to be tail recursive, otherwise get some stack overflow *)
    | [] -> List.rev acc
    (* '>>', maybe should be split in two tokens '>' '>' when in generic
     * context
     *)
    | T_SR ii :: xs -> (
        match env.stack with
        (* type context, those are the only places where types allowed for
         * now, which makes the job easier than in parsing_hacks_java.ml
         *)
        | (ClassHeader | ClassBody | TypeHeader | FunctionHeader) :: _ ->
            let lhs, rhs = split_two_char_info ii in
            aux env (TGREATER rhs :: TGREATER lhs :: acc) xs
        | UserAttribute :: rest ->
            aux { env with stack = rest } (T_SR ii :: acc) xs
        | _ -> aux env (T_SR ii :: acc) xs)
    (* This must be part of a lambda expression.
     * The parameters of a lambda expression are extremely difficult to parse
     * due to their similarity to standard expressions.
     * To get around this, we'll try to mark the opening and closing parens
     * of the lambda's parameters with special lambda paren tokens.
     *)
    | T_DOUBLE_ARROW arrow :: xs ->
        let replaced, rest =
          (* Nothing needs to be done for $x ==> ... *)
          if is_variable acc then ([], acc)
          else
            (* The majority of the time, lambdas aren't typehinted so let's just
             * eagerly replace the parens assuming these are the params. *)
            let toks, rest = find_paren_tokens acc true in
            match toks with
            (* Not a set of parens - this is probably a typehint. *)
            | [] -> (
                let typehint, rest = find_typehint acc in
                match typehint with
                | [] -> ([], acc) (* ignore; let the parser deal with it *)
                | _ ->
                    let params, rest2 = find_paren_tokens rest true in
                    if is_params params then (typehint @ params, rest2)
                    else ([], acc)
                (* ignore *))
            (* There are two possibilities now:
             *
             * 1) The typehint is a tuple or function, in which case this
             *    closing paren is part of a typehint.
             * 2) The closing paren is part of the parameters.
             *
             * is_params will be able to distinguish between the two cases.
             *)
            | _ -> (
                if is_params toks then (toks, rest)
                else
                  (* try finding a typehint *)
                  let typehint, rest = find_typehint acc in
                  match typehint with
                  | [] -> ([], acc) (* no match *)
                  | _ ->
                      let params, rest2 = find_paren_tokens rest true in
                      if is_params params then (typehint @ params, rest2)
                      else ([], acc))
          (* ignore *)
        in
        aux env (T_DOUBLE_ARROW arrow :: (replaced @ rest)) xs
    | x :: xs ->
        let stack =
          (* quite similar to hack/lexing_modes.ml *)
          match (x, env.stack) with
          (* ugly: we check we are at toplevel because the keyword 'class'
           * could be used in a different context as part of an XHP attribute
           * name, see ident_xhp_attr_name_atom rule in parser_php.mly
           *)
          | (T_CLASS _ | T_TRAIT _ | T_INTERFACE _), Toplevel :: _rest ->
              ClassHeader :: env.stack
          | T_TYPE _, Toplevel :: _rest -> TypeHeader :: env.stack
          | T_FUNCTION _, (Toplevel | ClassHeader) :: _rest ->
              FunctionHeader :: env.stack
          | T_FUNCTION _, Block :: _rest -> FunctionHeader :: env.stack
          (* also FunctionHeader because we can have attributes on parameters *)
          | T_SL _, (Toplevel | ClassBody | FunctionHeader) :: _rest ->
              UserAttribute :: env.stack
          | TOBRACE _ii, ClassHeader :: rest -> ClassBody :: rest
          (* subtle: do not do Block::env.stack here otherwise we will
           * not pop up enough to get back to a Toplevel context
           *)
          | TOBRACE _ii, FunctionHeader :: rest -> Block :: rest
          | TOBRACE _ii, _ -> Block :: env.stack
          | (T_CURLY_OPEN _ | T_DOLLAR_OPEN_CURLY_BRACES _), _ ->
              Block :: env.stack
          | TCBRACE _ii, _x :: xs -> xs
          | TCBRACE ii, [] ->
              failwith
                (spf "unmatching closing brace at %s" (Tok.stringpos_of_tok ii))
          | TSEMICOLON _ii, (FunctionHeader | TypeHeader) :: rest -> rest
          (* default case *)
          | _, st -> st
        in
        aux { env with stack } (x :: acc) xs
  in
  aux { stack = [ Toplevel ]; misc = () } [] (fix_heredocs (split_ampersands xs))
[@@profiling]
