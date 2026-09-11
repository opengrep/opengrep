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

(* The "version" that opens the JSON output is the engine version, which
   would make these snapshots drift every release; it is recognisable by the
   "results" following it. *)
let normalize : (string -> string) list =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_pcre_pattern {|\{"version":"([^"]*)","results"|};
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
      let exit_code =
        CLI.main caps (Array.of_list ([ "opengrep"; "--experimental" ] @ args))
      in
      UCommon.pr (spf "exit code: %d" (Exit_code.to_int exit_code)))

(*****************************************************************************)
(* Validating rules *)
(*****************************************************************************)

(* 'validate' runs the metarules of a registry pack over the rule files, which
   a test must not fetch: this pack is served instead, and matches nothing
   (the metarules run on the rule files, which are YAML). *)
let metarules_content : string =
  {|rules:
  - id: metacheck-nothing
    pattern: $X == $X
    message: no metacheck in the tests
    languages: [python]
    severity: ERROR
|}

let with_metarules (f : unit -> unit) : unit -> unit =
  Http_mock_client.with_testing_client
    (fun (_req : Cohttp.Request.t) (_body : Cohttp_lwt.Body.t) ->
      Lwt.return
        (Http_mock_client.basic_response
           (Cohttp_lwt.Body.of_string metarules_content)))
    f

(* the rule file of the fixtures validated, printing the exit code; the
   arguments name the subcommand, so that both 'validate' and the legacy
   'scan --validate' are covered *)
let validate_rule_file (caps : CLI.caps) ~(dir : string) ~(rule : string)
    (args : string list) : unit -> unit =
  with_metarules (fun () -> run_cli caps ~dir ~rule args)

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
           ~checked_output:(Testo.split_stdout_stderr ()) ~normalize (fun () ->
             scan_rule_file caps ~dir rule))

(* an unknown field of a rule is not an error *)
let test_extra_field_valid (caps : CLI.caps) () =
  scan_rule_file caps ~dir:"." "extra_field.yaml"

(* a config that cannot be found: exit code 7, and in the JSON an error of
   the same code. python: test_nonexisting_file *)
let test_missing_config_file (caps : CLI.caps) () =
  let repo_files = [ F.File ("target.py", "x == x\n") ] in
  Testutil_git.with_git_repo repo_files (fun _cwd ->
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
               (Error.string_of_error_type e.type_, e.code))))

(* a rule matching on the project's dependencies is skipped: no finding of
   its own, an info-level error naming it, one warning line in the text
   output, no such line in the JSON output, no target counted as partially
   analysed, a successful run, and the other rules of its file still run *)
let test_supply_chain_rule_skipped (caps : CLI.caps) () =
  let repo_files =
    [
      F.Dir
        ( "rules",
          [
            F.File
              ( "supply_chain.yaml",
                UFile.read_file Fpath.(fixtures_root / "supply_chain.yaml") );
          ] );
      F.File ("target.py", "import requests\nx == x\n");
    ]
  in
  Testutil_git.with_git_repo repo_files (fun _cwd ->
      let scan (args : string list) : Exit_code.t =
        CLI.main caps
          (Array.of_list
             ([
                "opengrep";
                "--experimental";
                "scan";
                "--config";
                "rules/supply_chain.yaml";
              ]
             @ args @ [ "target.py" ]))
      in
      (* the findings and the summary of a run, the summary being what the
         scan writes on stderr *)
      let outputs_of (args : string list) : string * string =
        let (exit_code, (stdout_output : string)), (stderr_output : string) =
          Testo.with_capture stderr (fun () ->
              Testo.with_capture stdout (fun () -> scan args))
        in
        Exit_code.Check.ok exit_code;
        (stdout_output, stderr_output)
      in
      (* the line naming the skipped rule and the key it comes from *)
      let announcement =
        "[WARN] Unsupported supply-chain rule in rule rules.depends-on-requests"
      in
      let _text_stdout, text_stderr = outputs_of [] in
      Alcotest.(check bool)
        "the text output announces the skipped rule as a warning" true
        (String_.contains ~term:announcement text_stderr);
      Alcotest.(check bool)
        "the skipped rule is not also announced as information" false
        (String_.contains ~term:"[INFO] Unsupported supply-chain rule"
           text_stderr);
      (* the rule file the error points at is not a target, so no file was
         left partially analysed *)
      Alcotest.(check bool)
        "no file counts as partially analysed" false
        (String_.contains ~term:"Partially scanned" text_stderr);
      let stdout_output, json_stderr = outputs_of [ "--json" ] in
      Alcotest.(check bool)
        "the JSON output reports the skipped rule in the document alone" false
        (String_.contains ~term:"Unsupported supply-chain rule" json_stderr);
      Alcotest.(check bool)
        "no file counts as partially analysed in JSON output" false
        (String_.contains ~term:"Partially scanned" json_stderr);
      let out = Semgrep_output_v1_j.cli_output_of_string stdout_output in
      Alcotest.(check (list string))
        "only the rule without a dependency condition matches"
        [ "rules.eqeq-bad" ]
        (out.results
        |> List_.map (fun (x : Semgrep_output_v1_t.cli_match) ->
               Rule_ID.to_string x.check_id));
      let string_of_level (level : Semgrep_output_v1_t.error_severity) : string
          =
        match level with
        | `Error -> "error"
        | `Warning -> "warning"
        | `Info -> "info"
      in
      Alcotest.(check (list (triple string string string)))
        "one info-level error for the skipped rule"
        [
          ("Unsupported supply-chain rule", "rules.depends-on-requests", "info");
        ]
        (out.errors
        |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) ->
               ( Error.string_of_error_type e.type_,
                 Option.fold ~none:"" ~some:Rule_ID.to_string e.rule_id,
                 string_of_level e.level ))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : CLI.caps) =
  Testo.categorize "Osemgrep rule errors (e2e)"
    (scan_tests caps "syntax" @ scan_tests caps "invalid-rules"
    @ [
        t "rule errors: scan extra_field.yaml"
          ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
          (test_extra_field_valid caps);
        t "rule errors: missing config file" (test_missing_config_file caps);
        t "rule errors: a supply-chain rule is skipped"
          (test_supply_chain_rule_skipped caps);
        (* a configuration the report calls invalid fails the run, whether it
           is validated by the subcommand or by the legacy scan flag *)
        t "rule errors: validate a file that does not parse"
          ~checked_output:(Testo.stdout ()) ~normalize
          (validate_rule_file caps ~dir:"syntax" ~rule:"missing-toplevel.yaml"
             [ "validate"; "rules/missing-toplevel.yaml" ]);
        t "rule errors: validate a rule that does not satisfy the schema"
          ~checked_output:(Testo.stdout ()) ~normalize
          (validate_rule_file caps ~dir:"invalid-rules"
             ~rule:"missing-pattern.yaml"
             [ "validate"; "rules/missing-pattern.yaml" ]);
        (* --validate --json prints the document of the errors, as a scan
           does *)
        t "rule errors: scan --validate --json"
          ~checked_output:(Testo.stdout ()) ~normalize
          (validate_rule_file caps ~dir:"syntax" ~rule:"missing-field.yaml"
             [
               "scan"; "--validate"; "--json"; "--config";
               "rules/missing-field.yaml";
             ]);
        (* a valid rule file validates and exits 0, in text and in JSON *)
        t "rule errors: validate a good rule file"
          ~checked_output:(Testo.stdout ()) ~normalize
          (validate_rule_file caps ~dir:"syntax" ~rule:"good.yaml"
             [ "validate"; "rules/good.yaml" ]);
        (* the JSON document holds the errors of the configuration, so a
           configuration without errors prints no document *)
        t "rule errors: scan --validate --json, a good rule file"
          ~checked_output:(Testo.stdout ()) ~normalize
          (validate_rule_file caps ~dir:"syntax" ~rule:"good.yaml"
             [ "scan"; "--validate"; "--json"; "--config"; "rules/good.yaml" ]);
      ])
