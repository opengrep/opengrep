(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2023-2024 Semgrep Inc.
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
open Common
open Sexplib.Std
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* File position.
 *
 * See also Loc.t for file location (file region/range).
 *
 * similar code:
 *  - Lexing.position (also used for Spacegrep.Loc.Pos), but no convenient
 *    line x col
 *  - Semgrep_output_v1.position, but no filename
 *  - Tree_sitter_run.Loc.pos (derived itself from Tree_sitter_bindings.Tree_sitter_output_t.position),
 *    but no filename, no bytepos, just line x col.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Pos.t is used in many places in Semgrep, either directly
 * or indirectly via Tok.t.
 *
 * In theory, we should just have 'type t = int'. This would be nice
 * because Pos.t, which is used in Tok.t, is used to store the position of
 * every token in the generic AST (see AST_generic.ml) so keeping its type
 * small would help reduce the memory footprint of an AST.
 * However this would require a big refactoring effort. Indeed,
 * even though the current type is a bit "fat", it is also convenient
 * because you can easily get line x col or filename information.
 * Moving the filename out of Pos.t would require to pass it around
 * in parsers, evaluators, static analyzers, etc.
 * With the current design, once you have a Pos.t (or Tok.t), you
 * can easily issue an error message with a precise location.
 *
 * TODO: we could probably remove the line x column and compute them
 * on demand.
 *)
type t = {
  (* Does it handle UTF-8? This is a byte position, not a character
   * position, so in theory we should not have to care about UTF-8.
   *)
  bytepos : int; (* 0-based *)
  (* Those two fields can be derived from bytepos (See complete_position() *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
  (* TODO: use an Src.t/Origin.t instead? (see spacegrep Src_file.source *)
  file : Fpath_.t;
}
[@@deriving show, sexp]

let compare (pos1 : t) (pos2 : t) : int =
  match Fpath_.compare pos1.file pos2.file with
  | 0 -> Int.compare pos1.bytepos pos2.bytepos
  | other -> other

let equal (pos1 : t) (pos2 : t) : bool =
  pos1.bytepos =|= pos2.bytepos &&
  Fpath_.equal pos1.file pos2.file

(* basic file position (used to be Common2.filepos) (used in codemap) *)
type linecol = { l : int; c : int } [@@deriving show, eq]

(* alt: could use @@deriving make.
 * TODO? should we use 0 instead? -1 clearly mark the field has not been set
 *)
let make ?(line = -1) ?(column = -1) file bytepos =
  { bytepos; line; column; file }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let first_pos_of_file file = make ~line:1 ~column:0 file 0

(* for error reporting *)
let string_of_pos { file; line; column; _ } = spf "%s:%d:%d" !!file line column
let to_linecol { line; column; _ } = { l = line; c = column }

(*****************************************************************************)
(* Adjust line x col in a position *)
(*****************************************************************************)

(* conversion table, in the shape of a function *)
type bytepos_linecol_converters = {
  bytepos_to_linecol_fun : int -> int * int;
  linecol_to_bytepos_fun : int * int -> int;
}

(* Lexing.ml in the standard OCaml library does not handle
 * the line number position.
 * Even if there are certain fields in the Lexing.position structure, they are
 * not maintained by the lexing engine so the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence the function below to overcome the previous limitation,
 * alt:
 *   - in each lexer you need to take care of newlines and update manually
 *     the field.
 *)
let complete_position (file : Fpath.t) converters (x : t) =
  {
    x with
    file;
    line = fst (converters.bytepos_to_linecol_fun x.bytepos);
    column = snd (converters.bytepos_to_linecol_fun x.bytepos);
  }

(*
   [line_starts] holds the byte position at which each line starts, in order;
   the last line ends at [last_line_end]. A byte position after it maps to
   [after_last_line]. Positions are clamped to the [size] positions of the
   input.
*)
let converters_of_line_starts ~(size : int) ~(last_line_end : int)
    ~(after_last_line : int * int) (line_starts : int array) :
    bytepos_linecol_converters =
  match size with
  | 2 ->
      {
        bytepos_to_linecol_fun = (fun _i -> (1, 0));
        linecol_to_bytepos_fun = (fun _ -> 0);
      }
  | _ ->
      let cmp = Ord.to_comparison Int.compare in
      let nlines = Array.length line_starts in
      {
        bytepos_to_linecol_fun =
          (fun (i : int) ->
            let i = max 0 (min i (size - 1)) in
            if i > last_line_end then after_last_line
            else
              (* The line of [i] is the last one that starts at or before
                 [i]. *)
              match
                Ord.binary_search_arr
                  ~f:(fun _ (line_start : int) -> cmp i line_start)
                  line_starts
              with
              | Ok (idx, _) -> (idx + 1, 0)
              | Error idx -> (idx, i - line_starts.(idx - 1)));
        linecol_to_bytepos_fun =
          (fun ((line, col) : int * int) ->
            if line < 1 || line > nlines || col < 0 then raise Not_found
            else
              let line_start = line_starts.(line - 1) in
              let line_end =
                if line < nlines then line_starts.(line) - 1 else last_line_end
              in
              if line_start + col > line_end then raise Not_found
              else line_start + col);
      }

(* coupling: see also Parse_tree_sitter_helpers.line_col_to_pos *)
let full_converters_large (file : Fpath.t) : bytepos_linecol_converters =
  let size = UFile.filesize file + 2 in
  (* Each line covers its bytes and the newline after it, which
     input_line does not return.
     old: hack for weird Windows files containing a single
     carriage return (CR) (\r) instead of a carriage return +
     newline feed (LF) (\r\n) to delimit newlines.
       if i < len - 1 && String.get s i =$= '\r' then (
          incr line;
          col := -1);
     Not recognizing those single \r as a newline marker prevents
     Javascript ASI to correctly insert semicolons.
     However, this hack is commented because having one part
     of the program recognizing those single CR as newlines
     (e.g., the Javascript parser), and other parts not
     (e.g., any function using Stdlib.input_line such as
     UFile.lines_of_file_exn) can cause a mismatch such as
     array out of bound exceptions in some functions.
     Simpler to be consistent. Note that tools such
     as `wc -l` do not recognize either those single CR as newlines.
     Same for the tree-sitter libraries. *)
  let rec read_line_starts (chan : in_channel) (line_start : int)
      (line_starts : int list) : int list * int =
    match input_line chan with
    | line ->
        read_line_starts chan
          (line_start + String.length line + 1)
          (line_start :: line_starts)
    | exception End_of_file -> (line_starts, line_start)
  in
  let line_starts, next_line_start =
    UFile.with_open_in file (fun chan -> read_line_starts chan 0 [])
  in
  let line_starts = Array.of_list (List.rev line_starts) in
  converters_of_line_starts ~size ~last_line_end:(next_line_start - 1)
    ~after_last_line:(Array.length line_starts, 0)
    line_starts
[@@profiling]

(* This is mostly a copy-paste of full_charpos_to_pos_large,
   but using a string for a target instead of a file. *)
let full_converters_str (s : string) : bytepos_linecol_converters =
  let size = String.length s + 2 in
  let line_starts, next_line_start =
    String.split_on_char '\n' s
    |> List.fold_left
         (fun ((line_starts, line_start) : int list * int) (line : string) ->
           (line_start :: line_starts, line_start + String.length line + 1))
         ([], 0)
  in
  converters_of_line_starts ~size ~last_line_end:(next_line_start - 1)
    ~after_last_line:(0, 0)
    (Array.of_list (List.rev line_starts))
[@@profiling]

(*****************************************************************************)
(* unit tests *)
(*****************************************************************************)

let s = "a\nhi\n"
let converters = full_converters_str s

let equate_positions bytepos linecol =
  converters.bytepos_to_linecol_fun bytepos =*= linecol
  && converters.linecol_to_bytepos_fun linecol =*= bytepos

let%test _ = equate_positions 0 (1, 0)

(* newline character is counted as being on the same line *)
let%test _ = equate_positions 1 (1, 1)
let%test _ = equate_positions 2 (2, 0)
let%test _ = equate_positions 3 (2, 1)
let%test _ = equate_positions 4 (2, 2)
