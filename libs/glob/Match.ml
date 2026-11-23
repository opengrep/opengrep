(* Martin Jambon
 *
 * Copyright (C) 2023 Semgrep
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
open Pattern
module Log = Log_glob.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Matching of a glob pattern against a path.
   This is purely syntactic: the file system is not accessed.

   We could use Re.Glob from the ocaml-re library for parsing the patterns
   but it doesn't expose the AST of the glob pattern, and this prevents us
   from making transformations required by gitignore such as treating
   the pattern 'foo/bar' as equivalent to '/foo/bar' but not treat
   'foo' as '/foo'. However, we use ocaml-re to produce the regexp tree
   and then execute it to match a path given as a string.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type loc = {
  source_name : string;
  source_kind : string option;
  line_number : int;
  line_contents : string;
}

let show_loc x =
  Printf.sprintf "%s, line %i: %s" x.source_name x.line_number x.line_contents

let pp_loc fmt x = Format.pp_print_string fmt (show_loc x)

type compiled_pattern = { source : loc; re : Pcre2_.t }

let string_loc ?(source_name = "<pattern>") ~source_kind pat =
  { source_name; source_kind; line_number = 1; line_contents = pat }

(*****************************************************************************)
(* Compilation of a Glob_pattern.t to a PCRE2 pattern *)
(*****************************************************************************)
(*
   We used to use ocaml-re ('Re' module) to build directly a tree but
   unfortunately, it doesn't support lookhead assertions that we would
   need to match a glob pattern correctly. The issue is that the pattern
   'a/*b' matches 'a/b' but the pattern 'a/*' doesn't match 'a/'.
*)

let add = Buffer.add_string
let addc = Buffer.add_char
let quote_char buf c = add buf (Pcre2_.quote (String.make 1 c))

(* A character as a member of a PCRE2 character class: ASCII punctuation
   behind a backslash, which PCRE2 always reads as the character itself (an
   unquoted '[:', '[.' or '[=' would start POSIX syntax), letters, digits
   and space as they are, and other bytes by their code. *)
let quote_class_char (buf : Buffer.t) (c : char) : unit =
  match c with
  | '!' .. '/'
  | ':' .. '@'
  | '[' .. '`'
  | '{' .. '~' ->
      addc buf '\\';
      addc buf c
  | ' ' .. '~' -> addc buf c
  | _ -> add buf (Printf.sprintf "\\x{%02x}" (Char.code c))

(* With [match_leading_period], a segment that does not begin with a literal
   '.' matches a leading period but not the segments '.' and '..', which
   refer to the current and the parent folder, as in a shell with 'dotglob'
   set. *)
let not_dot_segment = {|(?!\.\.?(?:/|\z))|}

(* Without [match_leading_period], a segment that does not begin with a
   literal '.' and every segment that '**' covers do not match a leading
   period, as in a shell without 'dotglob'. *)
let no_leading_period = "(?![.])"

let translate_frag (buf : Buffer.t) (frag : Pattern.segment_fragment) : unit =
  match frag with
  | Char c -> quote_char buf c
  (* A bracket expression never matches '/'. *)
  | Char_class { complement; ranges = [] } ->
      add buf (if complement then "[^/]" else "(?!)")
  | Char_class { complement; ranges } ->
      add buf (if complement then "[^/" else "(?!/)[");
      ranges
      |> List.iter (fun (range : Pattern.char_class_range) ->
             match range with
             | Class_char c -> quote_class_char buf c
             | Range (a, b) when Char.compare a b > 0 -> quote_class_char buf a
             | Range (a, b) ->
                 quote_class_char buf a;
                 addc buf '-';
                 quote_class_char buf b);
      addc buf ']'
  | Question -> add buf "[^/]"
  | Star -> add buf "[^/]*"

let translate_seg ~(match_leading_period : bool) (buf : Buffer.t)
    (seg : segment_fragment list) : unit =
  match seg with
  | [] -> ()
  | first :: _ ->
      (* lookahead assertion that checks that the path segment is not empty,
         because pattern 'a/*' should not match path 'a/' which has an
         empty trailing segment. *)
      add buf "(?=[^/])";
      (match first with
      | Char_class _
      | Question
      | Star ->
          if match_leading_period then add buf not_dot_segment
          else
            (* leading dot must match literally *)
            add buf no_leading_period
      | Char _ -> ());
      List.iter (translate_frag buf) seg

(* beginning of string *)
let bos = {|\A|}

(* end of string *)
let eos = {|\z|}

let rec translate ~(match_leading_period : bool) (buf : Buffer.t)
    (pat : Pattern.t) : unit =
  match pat with
  | [ Segment seg ] ->
      translate_seg ~match_leading_period buf seg;
      add buf eos
  (* A trailing '**' matches everything inside the folder before it, not
     the folder itself: at least one more segment, as in gitignore. *)
  | [ Any_subpath ] ->
      if match_leading_period then add buf "(?=[^/])"
      else
        (* one or more segments joined by slashes: a slash between two
           segments is required, so that a segment is never split *)
        add buf
          ("/*" ^ no_leading_period ^ "[^/]+(?:/+" ^ no_leading_period
         ^ "[^/]+)*/*" ^ eos)
  | Segment seg :: pat ->
      translate_seg ~match_leading_period buf seg;
      add buf "/+";
      translate ~match_leading_period buf pat
  | Any_subpath :: pat ->
      if match_leading_period then add buf "/*(?:[^/]+/+)*"
      else add buf ("/*(?:" ^ no_leading_period ^ "[^/]+/+)*");
      translate ~match_leading_period buf pat
  | [] -> add buf eos

(* Create a pattern that's left-anchored and right-anchored *)
let translate_root ~(match_leading_period : bool) (pat : Pattern.t) : string =
  let buf = Buffer.create 128 in
  add buf bos;
  translate ~match_leading_period buf pat;
  Buffer.contents buf

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Compile a pattern into a PCRE2 regexp for fast matching *)
(* TODO: For windows, we should probably make lowecase since it's not
 * case sensitive. Then paths should also be transformed in the same
 * way. *)
let compile ~(match_leading_period : bool) ~source pat =
  let pcre = translate_root ~match_leading_period pat in
  let re =
    (* a translation that PCRE2 rejects is reported as an invalid pattern,
       which the callers handle *)
    (* byte by byte, as git matches its ignore patterns *)
    try Pcre2_.regexp_bytes pcre with
    | Pcre2.Error (Pcre2.BadPattern (msg, (_ : int))) ->
        raise (Lexer.Syntax_error msg)
  in
  { source; re }
[@@profiling "Glob.Match.compile"]

let normalise_fpath_str pth_str =
  match Sys.os_type with
  | "Win32" -> String.map (fun c -> if Char.equal c '\\' then '/' else c) pth_str
  | _ -> pth_str

(* XXX: We ensure paths use forward slashes here, which is not optimal but should
 * only have a performance effect on Windows, if any. We do this because it's safer
 * to only do this here, avoiding other instances of this bug to creep in. *)
let run matcher path =
  let normalised_path = normalise_fpath_str path in
  let res = Pcre2_.pmatch_noerr ~rex:matcher.re normalised_path in
  (* perf: this gets called a lot. The match-with is expected to make things
     faster by creating a closure for the anonymous function only in debug
     mode. *)
  (* nosemgrep: no-logs-in-library *)
  (match Logs.level () with
  | Some Debug ->
      Log.debug (fun m ->
        let extra_path_info = match Sys.os_type with
            | "Win32" -> Printf.sprintf " (normalized: %S)" normalised_path
            | _ -> "" in
        m "glob: %S  pcre: %s  path: %S%S  matches: %B"
          matcher.source.line_contents matcher.re.pattern path extra_path_info res)
  | _ -> ());
  res
[@@profiling "Glob.Match.run"]

let source matcher = matcher.source

let show_compiled_pattern x =
  Printf.sprintf "pattern at %s:\n%s" (show_loc x.source) x.re.pattern

let pp_compiled_pattern fmt x =
  Format.pp_print_string fmt (show_compiled_pattern x)
