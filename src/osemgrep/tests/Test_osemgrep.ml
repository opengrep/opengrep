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
      CLI.main caps
        [| "opengrep"; "--experimental"; "ci"; "--config"; "rules.yaml" |]
      |> Exit_code.Check.findings)

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
      |> Exit_code.Check.broken_pipe)

(* the exit codes of Exit_code.ml that a subcommand documents; 13 and 14 are
   declared there and returned by no subcommand *)
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
(* Output destinations and the document of a failed run *)
(*****************************************************************************)

let eval_rules : string =
  "rules:\n\
  \  - id: use-eval\n\
  \    pattern: eval($X)\n\
  \    fix: safe_eval($X)\n\
  \    message: eval is dangerous\n\
  \    languages: [python]\n\
  \    severity: WARNING\n"

let eval_target : string = "eval(\"1+1\")\n"

let with_eval_repo (f : unit -> unit) : unit =
  Testutil_git.with_git_repo
    Testutil_files.
      [ File ("rules.yaml", eval_rules); File ("foo.py", eval_target) ]
    (fun _cwd -> f ())

(* the cli output documents printed on stdout; a run prints one *)
let documents (out : string) : string list =
  String.split_on_char '\n' out
  |> List.filter (fun (line : string) ->
         String_.contains ~term:"\"results\":" line)

(* A fix that cannot be written is reported on stderr and leaves the run
   itself successful with its single document: the failure to write is not
   a failure of the scan, which carries on. *)
let test_autofix_on_read_only_file (caps : CLI.caps) () =
  with_eval_repo (fun () ->
      Unix.chmod "foo.py" 0o444;
      let exit_code, out =
        Testo.with_capture stdout (fun () ->
            CLI.main caps
              [|
                "opengrep";
                "scan";
                "--experimental";
                "--json";
                "--autofix";
                "--config";
                "rules.yaml";
                "foo.py";
              |])
      in
      Unix.chmod "foo.py" 0o644;
      Exit_code.Check.ok exit_code;
      Alcotest.(check int) "one document" 1 (List.length (documents out));
      Alcotest.(check string) "the target is left alone" eval_target
        (UFile.read_file (Fpath.v "foo.py")))

(* An extra output that cannot be written aborts the run before anything
   reaches stdout, rather than printing the findings and then a second
   document holding only the error, which would overwrite a -o
   destination. *)
let test_unwritable_extra_output (caps : CLI.caps) () =
  with_eval_repo (fun () ->
      let exit_code, out =
        Testo.with_capture stdout (fun () ->
            CLI.main caps
              [|
                "opengrep";
                "scan";
                "--experimental";
                "--json";
                (* a regular file is not a directory to write into *)
                "--sarif-output";
                "foo.py/report.sarif";
                "--config";
                "rules.yaml";
                "foo.py";
              |])
      in
      Exit_code.Check.fatal exit_code;
      Alcotest.(check int) "no document" 0 (List.length (documents out)))

(* 'opengrep ci --json' prints the document of an error that aborts the run
   before any result; the default --suppress-errors then makes it exit 0. *)
let test_ci_json_fatal_error (caps : CLI.caps) () =
  Testutil_git.with_git_repo
    Testutil_files.[ File ("foo.py", eval_target); Dir ("norules", []) ]
    (fun _cwd ->
      let exit_code, out =
        Testo.with_capture stdout (fun () ->
            CLI.main caps
              [|
                "opengrep";
                "--experimental";
                "ci";
                "--json";
                "--config";
                "norules";
              |])
      in
      Exit_code.Check.ok exit_code;
      Alcotest.(check int) "one document" 1 (List.length (documents out));
      let output = Semgrep_output_v1_j.cli_output_of_string out in
      Alcotest.(check bool) "the document carries the error" true
        (not (List_.null output.errors)))

(* 'scan --validate --json' prints the document of the errors it found, so a
   valid configuration produces no document at all: nothing on stdout, and
   no file at the destination -o names. An invalid one puts the document in
   that file and leaves stdout empty.
   python: scan in commands/scan.py called its output handler only when the
   validation had collected errors. *)
let test_validate_output_to_file (caps : CLI.caps) () =
  let validate_to_file (what : string) (rule : string)
      ~(document_in_file : bool) (check_exit_code : Exit_code.t -> unit) : unit
      =
    Testutil_git.with_git_repo
      Testutil_files.[ File ("rules.yaml", rule) ]
      (fun (_cwd : Fpath.t) ->
        let exit_code, out =
          Testo.with_capture stdout (fun () ->
              CLI.main caps
                [|
                  "opengrep";
                  "scan";
                  "--experimental";
                  "--validate";
                  "--json";
                  "--config";
                  "rules.yaml";
                  "-o";
                  "report.json";
                |])
        in
        check_exit_code exit_code;
        Alcotest.(check int)
          (Printf.sprintf "%s: nothing on stdout" what)
          0
          (List.length (documents out));
        let report : Fpath.t = Fpath.v "report.json" in
        Alcotest.(check bool)
          (Printf.sprintf "%s: the -o file exists" what)
          document_in_file (Sys.file_exists !!report);
        if document_in_file then
          Alcotest.(check int)
            (Printf.sprintf "%s: the document is in the -o file" what)
            1
            (List.length (documents (UFile.read_file report))))
  in
  validate_to_file "a valid configuration" eval_rules ~document_in_file:false
    Exit_code.Check.ok;
  validate_to_file "an invalid configuration"
    Test_scan_subcommand_config.unknown_language_rule ~document_in_file:true
    Exit_code.Check.invalid_language

(* A rule matching on the project's dependencies, which opengrep skips. *)
let supply_chain_rules : string =
  "rules:\n\
  \  - id: depends-on-requests\n\
  \    pattern: import requests\n\
  \    message: requests used\n\
  \    languages: [python]\n\
  \    severity: WARNING\n\
  \    r2c-internal-project-depends-on:\n\
  \      namespace: pypi\n\
  \      package: requests\n\
  \      version: \"<99\"\n"

(* A configuration the report calls invalid never leaves the run
   successful: the run ends with the code of the last error of severity
   Error, or with the fatal code when every error is below that severity.
   python: _final_raise in output.py exited with the code of that last
   error, and the "Please fix the above errors" SemgrepError that scan in
   commands/scan.py then raised carried no code of its own. *)
let test_validate_exit_codes (caps : CLI.caps) () =
  let validate ~(args : string list) (files : Testutil_files.t list)
      (config : string) : int * string =
    Testutil_git.with_git_repo files (fun (_ : Fpath.t) ->
        let exit_code, out =
          Testo.with_capture stdout (fun () ->
              CLI.main caps
                (Array.of_list
                   ([ "opengrep"; "scan"; "--experimental"; "--validate" ]
                   @ args
                   @ [ "--config"; config ])))
        in
        (Exit_code.to_int exit_code, out))
  in
  let check_exit_code (name : string) (rule : string) (expected : int) : unit =
    Alcotest.(check int) name expected
      (fst
         (validate ~args:[]
            Testutil_files.[ File ("rules.yaml", rule) ]
            "rules.yaml"))
  in
  (* an incompatible rule is reported with severity Info, so it is the
     fatal code of the final error that the run ends with, not the ok code
     of that entry *)
  check_exit_code "a rule that requires a newer version"
    Test_scan_subcommand_config.incompatible_rule 2;
  check_exit_code "a broken rule then an incompatible one"
    Test_scan_subcommand_config.unparsable_then_incompatible_rules 2;
  check_exit_code "an unknown language"
    Test_scan_subcommand_config.unknown_language_rule 8;
  check_exit_code "a file that is not valid YAML"
    Test_scan_subcommand_config.unparsable_yaml_rule 5;
  (* a rule opengrep skips because it matches on the project's dependencies
     is reported, but the configuration holding it is still valid *)
  check_exit_code "a rule matching on the project's dependencies"
    supply_chain_rules 0;
  (* an empty directory of rules is a valid configuration with no rule;
     only a scan fails on it, for having no rule to run *)
  Alcotest.(check int)
    "an empty directory of rules" 0
    (fst
       (validate ~args:[]
          Testutil_files.[ Dir ("rules", []); File ("target.py", "x == x\n") ]
          "rules"));
  (* a config that cannot be found ends the validation with the fatal code,
     while its entry keeps the missing-configuration code; only a scan exits
     with that code as well.
     differs from the Python wrapper: its entry carries code 2.
     python: sanity_check_resolved_config in run_scan.py, which a scan runs
     and a validation does not, raised the missing-configuration code *)
  let exit_code, out =
    validate ~args:[ "--json" ]
      Testutil_files.[ File ("target.py", "x == x\n") ]
      "nosuch.yaml"
  in
  Alcotest.(check int) "a config that does not exist" 2 exit_code;
  Alcotest.(check (list (pair string int)))
    "the error entry of a config that does not exist"
    [ ("Missing config", 7) ]
    ((Semgrep_output_v1_j.cli_output_of_string out).errors
    |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
           (Error.string_of_error_type e.type_, e.code)))

(* python: run_scan.py saves core_time right after the scan of the head,
   before the baseline worktree is scanned, so a baseline scan reports it
   like any other. *)
let test_baseline_core_time (caps : CLI.caps) () =
  with_eval_repo (fun () ->
      let baseline =
        String.trim
          (Git_wrapper.command (caps :> < Cap.exec >) [ "rev-parse"; "HEAD" ])
      in
      let exit_code, out =
        Testo.with_capture stdout (fun () ->
            CLI.main caps
              [|
                "opengrep";
                "scan";
                "--experimental";
                "--json";
                "--time";
                "--baseline-commit";
                baseline;
                "--config";
                "rules.yaml";
                ".";
              |])
      in
      Exit_code.Check.ok exit_code;
      let output = Semgrep_output_v1_j.cli_output_of_string out in
      let names : string list =
        match output.time with
        | None -> []
        | Some (time : Semgrep_output_v1_t.profile) ->
            List_.map fst time.profiling_times
      in
      Alcotest.(check bool) "core_time is reported" true
        (List.exists (String.equal "core_time") names))

(* --text does not win over a machine format: '--text --json' prints the
   JSON document. *)
let test_text_with_json (caps : CLI.caps) () =
  with_eval_repo (fun () ->
      let exit_code, out =
        Testo.with_capture stdout (fun () ->
            CLI.main caps
              [|
                "opengrep";
                "scan";
                "--experimental";
                "--text";
                "--json";
                "--config";
                "rules.yaml";
                "foo.py";
              |])
      in
      Exit_code.Check.ok exit_code;
      Alcotest.(check int) "the JSON document" 1 (List.length (documents out)))

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
      Testo.create "a fix that cannot be written keeps one document"
        (test_autofix_on_read_only_file caps);
      Testo.create "an output that cannot be written prints no document"
        (test_unwritable_extra_output caps);
      Testo.create "ci --json reports a fatal error as the document"
        (test_ci_json_fatal_error caps);
      (* Both validation tests run under the metarules fixture of
         Test_rule_errors: a validation fetches the 'p/semgrep-rule-lints'
         pack from the registry, and a test must not depend on the network.
         On macOS the name resolution of that fetch initialises
         CoreFoundation, which rewrites __CF_USER_TEXT_ENCODING to the uid of
         the process, and the environment check of the test harness then
         fails the test. *)
      Testo.create
        "scan --validate --json writes the -o file only for an invalid \
         configuration"
        (Test_rule_errors.with_metarules (test_validate_output_to_file caps));
      Testo.create "scan --validate fails on an invalid configuration"
        (Test_rule_errors.with_metarules (test_validate_exit_codes caps));
      Testo.create "a baseline scan reports core_time"
        (test_baseline_core_time caps);
      Testo.create "--text does not win over --json" (test_text_with_json caps);
    ]
    @ ([
         ("ci", [ "0"; "1"; "2"; "3"; "4"; "5"; "7"; "8"; "141" ]);
         ("install-ci", [ "0"; "2"; "141" ]);
         ("lsp", [ "0"; "2"; "141" ]);
         ("scan", [ "0"; "1"; "2"; "3"; "4"; "5"; "7"; "8"; "141" ]);
         ("show", [ "0"; "2"; "3"; "4"; "7"; "141" ]);
         ("test", [ "0"; "1"; "2"; "7"; "141" ]);
         ("validate", [ "0"; "2"; "3"; "4"; "5"; "7"; "8"; "141" ]);
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
