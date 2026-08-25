(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep, Inc.
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

open Printf
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing combinations of multiple subcommands (e.g., login and scan).
 *
 * Many of those tests are slow because they interact for real with our
 * registry.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* TODO: Metrics are `Off` which means this fails. *)
(* no need for a token to access public rules in the registry *)
let test_scan_config_registry_no_token (caps : CLI.caps) =
  Testo.create __FUNCTION__ (fun () ->
      Testutil_files.with_tempdir ~chdir:true (fun _tmp_path ->
          let exit_code =
            CLI.main caps
              [|
                "opengrep";
                "scan";
                "--experimental";
                "--debug";
                "--config";
                "r/python.lang.correctness.useless-eqeq.useless-eqeq";
              |]
          in
          Exit_code.Check.ok exit_code))

let test_absolute_target_path caps =
  let func () =
    UTmp.with_temp_file ~contents:"hello\n" ~suffix:".py" (fun path ->
        assert (Fpath.is_abs path);
        (* We want 'path' to be in a folder other than the current
           folder. *)
        assert (!!(Fpath.parent path) <> Unix.getcwd ());
        Scan_subcommand.main caps
          [|
            "opengrep-scan";
            "--experimental";
            "-l";
            "python";
            "-e";
            "hello";
            !!path;
          |]
        |> Exit_code.Check.ok)
  in
  Testo.create "absolute path as target" func

(* 'opengrep --experimental ci' must reach the ci subcommand, not become a
   scan with 'ci' as scanning root *)
let test_subcommand_after_global_flag (caps : CLI.caps) () =
  let repo_files =
    Testutil_files.
      [
        File
          ( "rules.yaml",
            "rules:\n\
             - id: eqeq-bad\n\
            \  pattern: $X == $X\n\
            \  message: bad\n\
            \  languages: [python]\n\
            \  severity: ERROR\n" );
        File ("foo.py", "def foo(a, b):\n    return a + b == a + b\n");
      ]
  in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml"
        (fun () ->
          CLI.main caps
            [| "opengrep"; "--experimental"; "ci"; "--config"; "rules.yaml" |]
          |> Exit_code.Check.findings))

(* 'opengrep ci -d' must be accepted by the ci parser (so that without
   --experimental the run still falls back to pysemgrep, where the flag
   works) and dispatch to the show subcommand like scan does; a parser
   rejection would print an unknown-option usage error instead *)
let test_ci_dump_command_for_core (caps : CLI.caps) () =
  let repo_files =
    Testutil_files.
      [
        File
          ( "rules.yaml",
            "rules:\n\
             - id: eqeq-bad\n\
            \  pattern: $X == $X\n\
            \  message: bad\n\
            \  languages: [python]\n\
            \  severity: ERROR\n" );
        File ("foo.py", "def foo(a, b):\n    return a + b == a + b\n");
      ]
  in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml"
        (fun () ->
          CLI.main caps
            [|
              "opengrep"; "--experimental"; "ci"; "--config"; "rules.yaml"; "-d";
            |]
          |> Exit_code.Check.fatal))

let random_init = lazy (Random.self_init ())

let create_named_pipe () =
  Lazy.force random_init;
  let path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (sprintf "semgrep-test-%x.py" (Random.bits ()))
  in
  Unix.mkfifo path 0o644;
  Fpath.v path

(*
   This probably doesn't work on Windows due to the reliance on a shell
   command but could be ported (it doesn't need 'fork').
   TODO: switch to OCaml 5 and use parallelism.
*)
let with_read_from_named_pipe ~data func =
  let pipe_path = create_named_pipe () in
  Common.protect
    (fun () ->
      (* Start another process to write to the pipe in parallel *)
      UTmp.with_temp_file (fun reg_file ->
          (* We go through a regular file so as to avoid quoting issues. *)
          UFile.write_file ~file:reg_file data;
          let writer_command =
            (* Copy the data from the regular file into the named pipe *)
            sprintf "cat '%s' >> '%s'" !!reg_file !!pipe_path
          in
          (* Launch the process that feeds the pipe *)
          let writer = Unix.open_process_out writer_command in
          Common.protect
            (fun () ->
              (* This function can read the payload from the named pipe *)
              func pipe_path)
            ~finally:(fun () ->
              (* Close the helper process *)
              close_out_noerr writer)))
    ~finally:(fun () -> Sys.remove !!pipe_path)

let test_named_pipe (caps : Scan_subcommand.caps) =
  let func () =
    (* Search for pattern "hello" in a named pipe containing "hello" *)
    with_read_from_named_pipe ~data:"hello\n" (fun pipe_path ->
        Scan_subcommand.main caps
          [|
            "opengrep-scan";
            "--experimental";
            "-l";
            "python";
            "-e";
            "hello";
            !!pipe_path;
          |]
        |> Exit_code.Check.ok)
  in
  Testo.create "named pipe as target" func

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : CLI.caps) =
  let scan_caps = (caps :> Scan_subcommand.caps) in
  Testo.categorize "Osemgrep multi subcommands (e2e)"
    [
      test_scan_config_registry_no_token caps;
      Testo.create "subcommand after global flag"
        (test_subcommand_after_global_flag caps);
      Testo.create "ci accepts dump-command-for-core"
        ~checked_output:(Testo.stdxxx ())
        ~normalize:
          [
            Testutil_logs.mask_time;
            Testutil.mask_temp_paths ();
            Testutil_git.mask_temp_git_hash;
          ]
        (test_ci_dump_command_for_core caps);
      test_absolute_target_path scan_caps;
      test_named_pipe scan_caps;
    ]
