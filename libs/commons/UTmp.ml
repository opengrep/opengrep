(* Martin Jambon
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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
module Log = Log_commons.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Operations dealing with files in /tmp (or whatever tmp directory is
 * in your OS).
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let temp_files_created : (Fpath.t, unit) Kcas_data.Hashtbl.t =
  Kcas_data.Hashtbl.create () (* 101 *)

(* old: was in Common2.cmdline_flags_devel()
    ( "-keep_tmp_files",
      Arg.Set UTmp.save_temp_files,
      " keep temporary generated files" );
*)

let save_temp_files = ref false

(* XXX: This is called outside of the domainslib loop, so it should be ok. *)
let erase_temp_files () =
  if not !save_temp_files then (
    temp_files_created
    |> Kcas_data.Hashtbl.iter (fun path () ->
           Log.info (fun m -> m "deleting: %s" !!path);
           USys.remove !!path);
    Kcas_data.Hashtbl.clear temp_files_created)

(* hooks for with_temp_file() *)
(* TODO[Issue #128] This does not look like it's used in a single thread.
 * Use a [Kcas_data] stack? But it seems the hooks are only added at the
 * top level only, in a couple of modules: core/Range.ml and engine/Xpattern_matcher. *)
let temp_file_cleanup_hooks = ref []

(* See the .mli for a long explanation.
 *
 * alt: define your own with_temp_file wrapper, for example:
 * let hmemo = Hashtbl.create 101
 * ...
 * let with_temp_file ~str ~ext f =
 *  UTmp.with_temp_file ~str ~ext (fun file ->
 *     Common.protect
 *       ~finally:(fun () -> Hashtbl.remove hmemo file)
 *       (fun () -> f file))
 *)
let register_temp_file_cleanup_hook f = Stack_.push f temp_file_cleanup_hooks

(* ex: new_temp_file ~prefix:"cocci-" ".c"
   will give "/tmp/cocci-3252-434465.c" *)

let default_temp_file_prefix = USys.argv.(0) |> Filename.basename

(* TODO: To avoid collusions, we could set the DLS key for the temp folder
 * on each domain. See [Filename.temp_file] in ocaml/filename.ml.
 * See also [{get, set}_temp_dir_name]. We could get the dir name and add a new
 * subdirectory based on the domain-id, which means there should be no collusions?
 * We can also use the thread-id. Note that collusions can happen since the function
 * is recursive and it can choose a name that is also chosen by another thread,
 * and then both will write to the file, quite possibly with different contents!
 * Or just use thread-id instead of the pid? Not enough with domainslib in general
 * but in our case with [chunk_size = 1] it should be ok. *)
let tmp_file_counter = Atomic.make 0 (* Ensure that we never create the same filename. *)
let new_temp_file ?(prefix = default_temp_file_prefix) ?(suffix = "") ?temp_dir
    () =
  let cnt = Atomic.fetch_and_add tmp_file_counter 1 in
  let pid = if !Common.jsoo then 42 else (Thread.id (Thread.self ())) (* UUnix.getpid () *) in
  let temp_file =
    UFilename.temp_file
      ?temp_dir:(Option.map Fpath.to_string temp_dir)
      (spf "%s%d-%d-" prefix pid cnt) suffix
    |> Fpath.v
  in
  (* NOTE: Scan works with the statement below uncommented...! So this is not used?
   * At least for opengrep-cli (osemgrep). But it's used in other places like Autofix. *)
  (* ignore ( assert false ); *)
  Log.debug (fun m -> m "creating temp file %s" (Fpath.to_string temp_file));
  Kcas_data.Hashtbl.replace (* because we don't need > 1 bindings per key *)
    temp_files_created temp_file ();
  temp_file

let erase_this_temp_file f =
  if not !save_temp_files then (
    Kcas_data.Hashtbl.remove temp_files_created f;
    Log.info (fun m -> m "deleting: %s" !!f);
    USys.remove !!f)

let with_temp_file ?(contents = "") ?(persist = false) ?prefix ?suffix ?temp_dir
    (f : Fpath.t -> 'a) : 'a =
  let temp_file_path = new_temp_file ?prefix ?suffix ?temp_dir () in
  Common.finalize
    (fun () ->
      (match contents with
      | "" -> ()
      | contents -> UFile.write_file ~file:temp_file_path contents);
      f temp_file_path)
    (fun () ->
      if not persist then (
        (* XXX: Not thread safe. But currently the list is not modified
         * after the modules are loaded. *)
        !temp_file_cleanup_hooks
        |> List.iter (fun cleanup -> cleanup temp_file_path);
        erase_this_temp_file temp_file_path))

(* TODO[Issue #129] Is [Filename.open_temp_file] thread-safe? See comments
 * on [new_temp_file] above. Note that the function does not seem used,
 * transitively following the usage. *)
let write_temp_file_with_autodelete ~prefix ~suffix ~data : Fpath.t =
  let tmp_path, oc =
    UFilename.open_temp_file
      ~mode:[ Open_creat; Open_excl; Open_wronly; Open_binary ]
      prefix suffix
  in
  let remove () = if USys.file_exists tmp_path then USys.remove tmp_path in
  (* Try to remove temporary file when program exits. *)
  UStdlib.at_exit remove;
  Common.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc data);
  Log.debug (fun m -> m "wrote %i bytes to %s" (String.length data) tmp_path);
  Fpath.v tmp_path

let replace_named_pipe_by_regular_file_if_needed ?(prefix = "named-pipe")
    (path : Fpath.t) : Fpath.t option =
  if !Common.jsoo then None
    (* don't bother supporting exotic things like fds if running in JS *)
  else
    match (UUnix.stat !!path).st_kind with
    | Unix.S_FIFO ->
        let data = UFile.read_file path in
        let suffix = "-" ^ Fpath.basename path in
        Some (write_temp_file_with_autodelete ~prefix ~suffix ~data)
    | _ -> None

let replace_stdin_by_regular_file ?(prefix = "stdin") () : Fpath.t =
  let data = In_channel.input_all UStdlib.stdin in
  Log.debug (fun m -> m "stdin data: %s" (*String_.show*) data);
  write_temp_file_with_autodelete ~prefix ~suffix:"" ~data

let get_temp_dir_name () = Fpath.v (UFilename.get_temp_dir_name ())
