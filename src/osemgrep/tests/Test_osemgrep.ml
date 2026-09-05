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

(* '--help' and '-h' print the text of Help.ml, which the tool writes
   itself rather than letting cmdliner generate it, so it is snapshotted.
   python: test_help_text *)
let test_help (caps : CLI.caps) (flag : string) () =
  CLI.main caps [| "opengrep"; flag |] |> Exit_code.Check.ok

(* 'opengrep ... | head' closes the pipe once it has its line: the scan ends
   quietly with the conventional code for it, rather than reporting a
   Sys_error and a stack trace. The target is large enough for the output to
   leave the buffer of the standard channel while the scan runs. *)
let test_broken_pipe (caps : CLI.caps) () =
  let repo_files =
    Testutil_files.
      [
        File
          ( "rules.yaml",
            "rules:\n\
             - id: print-call\n\
            \  pattern: print(...)\n\
            \  message: found\n\
            \  languages: [python]\n\
            \  severity: WARNING\n" );
        File
          ( "many.py",
            String.concat ""
              (List.init 400 (fun (i : int) -> Printf.sprintf "print(%d)\n" i))
          );
      ]
  in
  Testutil_git.with_git_repo repo_files (fun _cwd ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml"
        (fun () ->
          Test_scan_helpers.with_stdout_to_closed_pipe (fun () ->
              CLI.main caps
                [|
                  "opengrep";
                  "scan";
                  "--experimental";
                  "--json";
                  "--config";
                  "rules.yaml";
                  "many.py";
                |])
          |> Exit_code.Check.broken_pipe))

(* Every exit code opengrep can return, from Exit_code.ml *)
let documented_exit_codes : string list =
  [ "0"; "1"; "2"; "3"; "4"; "5"; "7"; "8"; "141" ]

(* A subcommand's man page documents the exit codes that subcommand
   returns: not the codes of another one, and not cmdliner's defaults,
   which no code path here produces.
   coupling: the exits_* lists of CLI_common.ml *)
let test_man_page_exit_codes (caps : CLI.caps) (subcommand : string)
    (codes : string list) () =
  let exit_code, out =
    Testo.with_capture stdout (fun () ->
        let exit_code =
          CLI.main caps [| "opengrep"; subcommand; "--help=plain" |]
        in
        (* cmdliner leaves the page in the standard formatter, which the
           binary flushes on its way out and a test has to flush itself *)
        Format.pp_print_flush Format.std_formatter ();
        exit_code)
  in
  Exit_code.Check.ok exit_code;
  let documents (code : string) : bool =
    String_.contains ~term:(Printf.sprintf "\n       %s " code) out
  in
  documented_exit_codes
  |> List.iter (fun (code : string) ->
         Alcotest.(check bool)
           (Printf.sprintf "%s and the exit code %s" subcommand code)
           (List.exists (String.equal code) codes)
           (documents code));
  [ "123"; "124"; "125" ]
  |> List.iter (fun (code : string) ->
         Alcotest.(check bool)
           (Printf.sprintf "%s does not document cmdliner's %s" subcommand code)
           false (documents code))

let test_named_pipe (caps : Scan_subcommand.caps) =
  let func () =
    (* Search for pattern "hello" in a named pipe containing "hello" *)
    Test_scan_helpers.with_read_from_named_pipe ~data:"hello\n" (fun pipe_path ->
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
    ([
      test_scan_config_registry_no_token caps;
      Testo.create "subcommand after global flag"
        (test_subcommand_after_global_flag caps);
      test_absolute_target_path scan_caps;
      test_named_pipe scan_caps;
      Testo.create "a closed output pipe ends the scan quietly"
        (test_broken_pipe caps);
    ]
    @ ([
         ("ci", [ "0"; "1"; "2"; "7"; "141" ]);
         ("install-ci", [ "0"; "2" ]);
         ("lsp", [ "0"; "2"; "141" ]);
         ("scan", [ "0"; "1"; "2"; "3"; "4"; "5"; "7"; "8"; "141" ]);
         ("show", [ "0"; "2"; "3"; "4"; "7"; "141" ]);
         ("test", [ "0"; "1"; "2"; "7"; "141" ]);
         ("validate", [ "0"; "2"; "4"; "5"; "7"; "8"; "141" ]);
       ]
       |> List_.map (fun ((subcommand : string), (codes : string list)) ->
              Testo.create
                (Printf.sprintf "exit codes in the man page of %s" subcommand)
                (test_man_page_exit_codes caps subcommand codes)))
    @ ([ "--help"; "-h" ]
       |> List_.map (fun (flag : string) ->
              Testo.create
                (Printf.sprintf "help text of %s" flag)
                ~checked_output:(Testo.stdout ())
                (test_help caps flag))))
