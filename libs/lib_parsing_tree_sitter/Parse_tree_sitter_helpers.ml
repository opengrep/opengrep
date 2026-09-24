(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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

(* alt: could also use Log_lib_parsing.Log *)
module Log = Log_tree_sitter.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to help factorize code between the different
 * Parse_xxx_tree_sitter.ml files.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a env = {
  file : Fpath.t;
  (* get the charpos (offset) in file given a line x col.
     Raises Not_found! *)
  conv : int * int -> int;
  extra : 'a;
}

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* coupling: mostly a copy-paste of Pos.full_charpos_to_pos_large *)
let line_col_to_pos file =
  let size = UFile.filesize file + 2 in
  let h = Hashtbl.create size in
  UFile.with_open_in file (fun chan ->
      let charpos = ref 0 in
      let line = ref 0 in

      let full_charpos_to_pos_aux () =
        try
          while true do
            let s = input_line chan in
            (* we are using 1-based lines *)
            incr line;

            (* '... +1 do'  cos input_line does not return the trailing \n *)
            for i = 0 to String.length s - 1 + 1 do
              Hashtbl.add h (!line, i) (!charpos + i)
            done;
            charpos := !charpos + String.length s + 1
          done
        with
        | End_of_file ->
            (* We need to add this in case there is a trailing \n in the
               end of the file *)
            Hashtbl.add h (!line + 1, 0) !charpos
      in
      full_charpos_to_pos_aux ());
  Hashtbl.find h

(* Patterns are given as a one line string with '\n' characters (huh?)
   TODO: Explain the desired behavior *)
let line_col_to_pos_pattern _str (_line, col) = col

let token env (tok : Tree_sitter_run.Token.t) =
  let loc, str = tok in
  let start = loc.Tree_sitter_run.Loc.start in
  (* Parse_info is 1-line based and 0-column based, like Emacs *)
  let line = start.Tree_sitter_run.Loc.row + 1 in
  let column = start.Tree_sitter_run.Loc.column in
  let file = env.file in
  let bytepos =
    match env.conv (line, column) with
    | x -> x
    | exception Not_found ->
        raise
          (Tok.NoTokenLocation
             (spf
                "Could not convert (line %d, column %d) into a byte offset in \
                 file %s. Invalid location for token %S."
                line column !!file str))
  in
  let pos = Pos.make ~line ~column file bytepos in
  let tok_loc = { Tok.str; pos } in
  Tok.tok_of_loc tok_loc

let str env (tok : Tree_sitter_run.Token.t) =
  let _, s = tok in
  (s, token env tok)

(* Tree_sitter_run.Loc has no comparison of its own. *)
let compare_pos (a : Tree_sitter_run.Loc.pos) (b : Tree_sitter_run.Loc.pos) =
  match Int.compare a.row b.row with
  | 0 -> Int.compare a.column b.column
  | c -> c

let equal_loc (a : Tree_sitter_run.Loc.t) (b : Tree_sitter_run.Loc.t) =
  Int.equal (compare_pos a.start b.start) 0
  && Int.equal (compare_pos a.end_ b.end_) 0

(*****************************************************************************)
(* Heredocs *)
(*****************************************************************************)
(* The contracts are in the .mli. *)

let rec heredoc_markers ~(constructor : string)
    (tree : _ Tree_sitter_run.Raw_tree.t) : Tree_sitter_run.Token.t list =
  match tree with
  | Case (c, Token marker) when String.equal c constructor -> [ marker ]
  | Case (_, x)
  | Option (Some x) ->
      heredoc_markers ~constructor x
  | List xs
  | Tuple xs ->
      List.concat_map (heredoc_markers ~constructor) xs
  | Option None
  | Token _
  | Any _ ->
      []

let pair_heredocs ~(delimiter : string -> string)
    ~(terminator : 'body -> string) (markers : Tree_sitter_run.Token.t list)
    (bodies : (Tree_sitter_run.Loc.t * 'body) list) :
    (Tree_sitter_run.Loc.t * 'body) list =
  let is_body_of ((marker_loc : Tree_sitter_run.Loc.t), marker)
      ((loc : Tree_sitter_run.Loc.t), body) =
    let terminator = String.trim (terminator body) in
    compare_pos loc.start marker_loc.end_ >= 0
    && (String_.empty terminator || String.equal terminator (delimiter marker))
  in
  markers
  |> List.sort (fun ((a : Tree_sitter_run.Loc.t), _) ((b : Tree_sitter_run.Loc.t), _) ->
         compare_pos a.start b.start)
  |> List.fold_left
       (fun (heredocs, bodies) ((marker_loc, _) as marker) ->
         match List.find_opt (is_body_of marker) bodies with
         | Some ((_, body) as claimed) ->
             ( (marker_loc, body) :: heredocs,
               List.filter (fun x -> not (phys_equal x claimed)) bodies )
         | None -> (heredocs, bodies))
       ([], bodies)
  |> fst

let heredoc_body (marker_loc : Tree_sitter_run.Loc.t)
    (heredocs : (Tree_sitter_run.Loc.t * 'body) list) : 'body option =
  List.find_map
    (fun (loc, body) -> if equal_loc loc marker_loc then Some body else None)
    heredocs

let dedent (indentation : int) (text : string) : string =
  let rex = Pcre2_.regexp (spf "\n[ \t]{0,%d}" indentation) in
  Pcre2_.replace ~rex ~template:"\n" text

let drop_marker_newline
    ~(text_part : 'part -> (Tree_sitter_run.Token.t * string) option)
    ~(make_text_part : Tree_sitter_run.Token.t -> string -> 'part)
    (contents : 'part list) : 'part list =
  let after_newline s = String.sub s 1 (String.length s - 1) in
  match contents with
  | [] -> contents
  | first :: rest -> (
      match text_part first with
      | Some (((loc : Tree_sitter_run.Loc.t), text), value)
        when String.starts_with ~prefix:"\n" value -> (
          let start =
            { Tree_sitter_run.Loc.row = loc.start.row + 1; column = 0 }
          in
          match after_newline value with
          | "" -> rest
          | value ->
              make_text_part ({ loc with start }, after_newline text) value
              :: rest)
      | _ -> contents)

let debug_sexp_cst_after_error sexp_cst =
  let s = Printexc.get_backtrace () in
  Log.warn (fun m -> m "Some constructs are not handled yet. CST was: ");
  (* bugfix: do not use CST.dump_tree because it prints on stdout
   * and will mess up our interaction with semgrep python wrapper and
   * also for the parsing_stat CI job.
   *
   * alt: Use Print_sexp.to_stderr of martin
   *)
  Log.warn (fun m ->
      m "%s\nOriginal backtrace:\n %s" (Sexplib.Sexp.to_string_hum sexp_cst) s)

let wrap_parser tree_sitter_parser ast_mapper =
  let res : ('program, 'extra) Tree_sitter_run.Parsing_result.t =
    tree_sitter_parser ()
  in
  let program =
    match res.program with
    | Some cst ->
        (if res.errors <> [] then
           Log.warn (fun m ->
               let error_strs =
                 List_.map
                   (fun err -> err.Tree_sitter_run.Tree_sitter_error.msg)
                   res.errors
               in
               let error_str = String.concat "\n" error_strs in
               m "Partial errors returned by Tree-sitter parser\n%s" error_str));
        Some (ast_mapper cst res.extras)
    | None -> None
  in
  { res with program; extras = [] }

(* Stuff to put in entry point at the beginning:
   let todo _env _x = failwith "not implemented"

   let todo_any str t any =
     pr2 (AST.show_any any);
     raise (Parse_info.Ast_builder_error (str, t))

   let program =
   ...
   try
     program ...
   with
    (Failure "not implemented") as exn ->
      H.debug_sexp_cst_after_error (CST.sexp_of_program cst);
      raise exn
*)
