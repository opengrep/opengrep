(* Brandon Wu, Yoann Padioleau
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Real paths.

   This module governs a path type, which can only produce values which
   are for normalized paths to existent files or directories.
   These paths are always canonically kept at their absolute forms, in order to
   maintain a canonicity property. Due to the `Rpath` constructor being
   private, we can enforce this invariant at the type level.

   history: this file was originally called `Path.ml`, but this conflicts with
   the OCaml compiler libraries. Then it was called FPath.ml but this
   was too close to the Fpath library by Daniel Buenzli. Then it was
   called FilePath.ml, but ultimately got renamed to Rpath.ml, so we would
   have Fpath.ml, Ppath.ml,and finally Rpath.ml. We could also have Dpath.ml
   for directories and that would put us closer to the nice filenames library
   in Scala with better types paths:
   https://www.lihaoyi.com/post/HowtoworkwithFilesinScala.html
*)

open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Rpath of Fpath.t [@@unboxed] [@@deriving show, eq]

(*****************************************************************************)
(* Main functions *)
(*****************************************************************************)

let adjust_unc_on_windows (path : string) =
  (* [Unix.realpath] returns a UNC\ prefixed string when accessing WSL paths from Windows,
   * and such paths do not work when we pass them to [Sys] functions or when we try to access
   * the files they map to. So we do this hack and obtain correct \\ prefixed paths in such cases. *)
  if Sys.win32 && String.starts_with ~prefix:"UNC\\" path then 
    "\\\\" ^ String.sub path 4 (String.length path - 4)
  else
    path

let realpath (path : string) = Unix.realpath path |> adjust_unc_on_windows

let of_string path =
  try
    let rpath = realpath path in
    Ok (Rpath (Fpath.v rpath))
  with
  | Unix.Unix_error (err, _, _) ->
      let msg =
        spf "Cannot determine physical path for %S: %s" path
          (Unix.error_message err)
      in
      Error msg
  | other_exn ->
      let msg =
        spf "Cannot determine physical path for %S: %s" path
          (Printexc.to_string other_exn)
      in
      Error msg

let of_string_exn path_str =
  match of_string path_str with
  | Ok rpath -> rpath
  | Error msg -> failwith msg

let of_fpath path = of_string (Fpath.to_string path)
let of_fpath_exn fpath = of_string_exn (Fpath.to_string fpath)
let to_fpath (Rpath x) = x
let canonical_exn s = to_fpath (of_fpath_exn s)

(* On case-insensitive filesystems (Windows), resolve [p] to its physical real
   path so its spelling (case and 8.3 short names) agrees with the canonical
   paths the OS and git report. On case-sensitive filesystems the command-line
   path is already canonical, so [p] is returned unchanged. If [p] can't be
   resolved (e.g. removed since it was last checked, or a parent directory is
   not traversable), it is returned unchanged too, matching the behaviour
   before canonicalisation. *)
let canonical_if_win p =
  if Sys.win32 then
    match of_fpath p with
    | Ok rp -> to_fpath rp
    | Error _ -> p
  else p

(* deprecated *)
let to_string (Rpath x) = Fpath.to_string x

let getcwd () =
  (* We could call 'Unix.getcwd' but it's not documented as returning
     necessarily a physical path. Hopefully, this is fast enough. *)
  of_string_exn "."

let parent (Rpath path) =
  let res = Fpath.parent path |> Fpath.rem_empty_seg in
  if res <> path then Some (Rpath res) else None
