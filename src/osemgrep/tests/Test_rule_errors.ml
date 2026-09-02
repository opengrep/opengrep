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
module F = Testutil_files

let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of the reporting of the rules that cannot be loaded:
 * every rule file of tests/rule_errors/syntax scanned in text and JSON
 * output, and every file of tests/rule_errors/invalid-rules validated,
 * as snapshots. python: test_rule_parser.py and test_rule_validation.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fixtures_root : Fpath.t = Fpath.v "tests/rule_errors"

let normalize : (string -> string) list =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
  ]

let rule_files (dir : string) : string list =
  Fpath.(fixtures_root / dir)
  |> List_files.read_dir_entries_fpath
  |> List_.filter_map (fun (p : Fpath.t) ->
         if Fpath.has_ext ".yaml" p || Fpath.has_ext ".yml" p then
           Some (Fpath.basename p)
         else None)
  |> List.sort String.compare

(* the rule file copied into a temp repo with one target, and the CLI run
   on the given arguments, printing the exit code *)
let run_cli (caps : CLI.caps) ~(dir : string) ~(rule : string)
    (args : string list) : unit =
  let repo_files =
    [
      F.Dir ("rules", [ F.File (rule, UFile.read_file Fpath.(fixtures_root / dir / rule)) ]);
      F.File ("target.py", "x == x\n");
    ]
  in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml"
        (fun () ->
          let exit_code =
            CLI.main caps
              (Array.of_list ([ "opengrep"; "--experimental" ] @ args))
          in
          UCommon.pr (spf "exit code: %d" (Exit_code.to_int exit_code))))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* the scan in text output, then in JSON output. The validate subcommand
   reports the rule errors the same way, but it fetches its metarules from
   the registry, which a test must not depend on. *)
let scan_rule_file (caps : CLI.caps) ~(dir : string) (rule : string) : unit =
  let config = "rules/" ^ rule in
  run_cli caps ~dir ~rule [ "scan"; "--config"; config; "target.py" ];
  run_cli caps ~dir ~rule [ "scan"; "--json"; "--config"; config; "target.py" ]

let scan_tests (caps : CLI.caps) (dir : string) : Testo.t list =
  rule_files dir
  |> List_.map (fun (rule : string) ->
         t (spf "rule errors: scan %s/%s" dir rule)
           ~checked_output:(Testo.stdxxx ()) ~normalize (fun () ->
             scan_rule_file caps ~dir rule))

(* an unknown field of a rule is not an error *)
let test_extra_field_valid (caps : CLI.caps) () =
  scan_rule_file caps ~dir:"." "extra_field.yaml"

(* a config that cannot be found: exit code 7, and in the JSON an error of
   the same code. python: test_nonexisting_file *)
let test_missing_config_file (caps : CLI.caps) () =
  let repo_files = [ F.File ("target.py", "x == x\n") ] in
  Testutil_git.with_git_repo repo_files (fun _cwd ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml"
        (fun () ->
          let scan (args : string list) : Exit_code.t =
            CLI.main caps
              (Array.of_list
                 ([ "opengrep"; "--experimental"; "scan"; "--config"; "does_not_exist.yaml" ]
                 @ args @ [ "target.py" ]))
          in
          Exit_code.Check.missing_config (scan []);
          let exit_code, stdout_output = Testo.with_capture stdout (fun () -> scan [ "--json" ]) in
          Exit_code.Check.missing_config exit_code;
          let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
          Alcotest.(check (list (pair string int)))
            "one error, with the exit code"
            [ ("Missing config", 7) ]
            (out.errors
            |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
                   (Error.string_of_error_type e.type_, e.code)))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : CLI.caps) =
  Testo.categorize "Osemgrep rule errors (e2e)"
    (scan_tests caps "syntax" @ scan_tests caps "invalid-rules"
    @ [
        t "rule errors: scan extra_field.yaml"
          ~checked_output:(Testo.stdxxx ()) ~normalize
          (test_extra_field_valid caps);
        t "rule errors: missing config file" (test_missing_config_file caps);
      ])
