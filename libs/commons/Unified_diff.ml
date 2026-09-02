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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A line diff in the unified format without context lines, as difflib's
 * unified_diff(n=0) in Python and 'diff -U0' print it, but without the
 * file headers:
 *
 *   @@ -2 +2 @@
 *   -sys.exit(1)
 *   +somethingElse(1)
 *
 * The lines are compared by Testo_diff, a port of Python's simplediff.
 *)

module Diff = Testo_diff.Make (String)

(* A run of consecutive changes, with the 1-based number of the first line
   it starts at (or would start at, for a pure insertion) in each input. *)
type hunk = {
  old_start : int;
  old_lines : string list;
  new_start : int;
  new_lines : string list;
}

let hunks (diffs : Diff.t) : hunk list =
  let rec go (old_line : int) (new_line : int) (current : hunk option)
      (finished : hunk list) (diffs : Diff.t) : hunk list =
    let flush : hunk list =
      match current with
      | Some h -> h :: finished
      | None -> finished
    in
    let started : hunk =
      match current with
      | Some h -> h
      | None ->
          { old_start = old_line; old_lines = []; new_start = new_line; new_lines = [] }
    in
    match diffs with
    | [] -> List.rev flush
    | Diff.Equal lines :: rest ->
        let n = Array.length lines in
        go (old_line + n) (new_line + n) None flush rest
    | Diff.Deleted lines :: rest ->
        let h = { started with old_lines = started.old_lines @ Array.to_list lines } in
        go (old_line + Array.length lines) new_line (Some h) finished rest
    | Diff.Added lines :: rest ->
        let h = { started with new_lines = started.new_lines @ Array.to_list lines } in
        go old_line (new_line + Array.length lines) (Some h) finished rest
  in
  go 1 1 None [] diffs

(* "start,length", with the shorthands of the unified format: a single line
   is "start" alone, and an empty range starts at the line before it. *)
let range (start : int) (length : int) : string =
  match length with
  | 1 -> Int.to_string start
  | 0 -> Printf.sprintf "%d,0" (start - 1)
  | _ -> Printf.sprintf "%d,%d" start length

let lines_of_hunk (h : hunk) : string list =
  let header =
    Printf.sprintf "@@ -%s +%s @@"
      (range h.old_start (List.length h.old_lines))
      (range h.new_start (List.length h.new_lines))
  in
  (header :: List.map (fun (l : string) -> "-" ^ l) h.old_lines)
  @ List.map (fun (l : string) -> "+" ^ l) h.new_lines

let lines ~(old_ : string) ~(new_ : string) : string list =
  let split (s : string) : string array =
    Array.of_list (String.split_on_char '\n' s)
  in
  Diff.get_diff (split old_) (split new_) |> hunks |> List.concat_map lines_of_hunk
