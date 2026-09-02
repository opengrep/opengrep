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
open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The text report of the rules that could not be loaded, one block per
 * error with an excerpt of the rule file around the token the error is
 * about, in the layout of pysemgrep's ErrorWithSpan:
 *
 *   rules/bad.yaml:4:8: Invalid rule schema in rule rules.bad
 *     --> rules/bad.yaml:4
 *   3 |     patterns:
 *   4 |       - patterns-either:
 *     |         ^^^^^^^^^^^^^^^
 *   5 |           - patterns:
 *   unexpected key patterns-either
 *
 * pysemgrep knew the whole YAML node its schema rejected; the parser of
 * the OCaml binary knows the exact token, shown with one line of context
 * on each side.
 *)

(*****************************************************************************)
(* Excerpt *)
(*****************************************************************************)

let context_lines = 1

(* the lines of the file around the location, each with its number *)
let excerpt_lines (loc : Out.location) : (int * string) list =
  let lines = UFile.read_file loc.path |> String.split_on_char '\n' in
  let first = max 1 (loc.start.line - context_lines) in
  let last = min (List.length lines) (loc.end_.line + context_lines) in
  lines
  |> List.mapi (fun (i : int) (line : string) -> (i + 1, line))
  |> List.filter (fun ((n : int), _) -> n >= first && n <= last)

(* python: ErrorWithSpan._format_line_number, a margin wide enough for the
   last line number shown *)
let pp_excerpt ppf (loc : Out.location) : unit =
  match excerpt_lines loc with
  | [] -> ()
  | lines ->
      let width =
        String.length (Int.to_string (fst (List_.last_opt lines |> Option.get)))
        + 1
      in
      let margin (n : int option) : string =
        let s = Option.fold ~none:"" ~some:Int.to_string n in
        s ^ String.make (width - String.length s) ' ' ^ "| "
      in
      Format.fprintf ppf "  --> %s:%d@\n" !!(loc.path) loc.start.line;
      lines
      |> List.iter (fun ((n : int), (line : string)) ->
             Format.fprintf ppf "%s%s@\n" (margin (Some n)) line;
             (* the carets under the token, when it is on one line *)
             if Int.equal n loc.start.line && Int.equal loc.end_.line n then
               Format.fprintf ppf "%s%s%s@\n" (margin None)
                 (String.make (loc.start.col - 1) ' ')
                 (String.make (max 1 (loc.end_.col - loc.start.col)) '^'))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let pp_error ppf (err : Core_error.t) : unit =
  let in_rule =
    match err.rule_id with
    | Some rule_id -> spf " in rule %s" (Rule_ID.to_string rule_id)
    | None -> ""
  in
  (match err.loc with
  | Some loc ->
      let loc = Semgrep_output_utils.location_of_token_location loc in
      Format.fprintf ppf "%s:%d:%d: %s%s@\n" !!(loc.path) loc.start.line
        loc.start.col
        (Error.string_of_error_type err.typ)
        in_rule;
      if Sys.file_exists !!(loc.path) then pp_excerpt ppf loc
  | None ->
      Format.fprintf ppf "%s%s@\n" (Error.string_of_error_type err.typ) in_rule);
  Format.fprintf ppf "%s@\n" (String.trim err.msg)

let pp_errors ppf (errors : Core_error.t list) : unit =
  errors |> List.iter (fun (err : Core_error.t) -> pp_error ppf err)

(* the files of the errors, as pysemgrep counts the invalid configs; an
   error without a file, such as a config that cannot be found, counts as
   one config *)
let invalid_configs (errors : Core_error.t list) : int =
  let with_file, without_file =
    errors
    |> List.partition_map (fun (err : Core_error.t) ->
           match err.loc with
           | Some (loc : Tok.location) -> Either.Left loc.pos.file
           | None -> Either.Right err)
  in
  List.length (List_.deduplicate with_file) + List.length without_file

(* the text of the Semgrep_error raised for invalid rules in text mode *)
let invalid_configs_message (errors : Core_error.t list) : string =
  spf "invalid configuration file found (%d configs were invalid)\n%s"
    (invalid_configs errors)
    (Fmt_.with_buffer_to_string (fun ppf -> pp_errors ppf errors) |> String.trim)
