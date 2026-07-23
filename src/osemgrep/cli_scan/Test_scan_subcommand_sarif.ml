(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests for the SARIF output of the scan subcommand.
 *
 * These were ported from cli/tests/default/e2e/test_output_sarif.py so that
 * the osemgrep-only code path is exercised on CI without depending on the
 * Python wrapper. The tests that Python marks [@osemfail] (taint labels,
 * --error exit behaviour, non-existent input files) are intentionally left
 * behind in Python for now.
 *
 * Fixture rules and targets live under tests/sarif/. The OCaml test harness
 * runs from the project root (see scripts/run-core-test), so relative paths
 * resolve before we descend into the temporary git repo.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fixtures_root : Fpath.t = Fpath.v "tests/sarif"

let read_fixture (rel : string) : string =
  UFile.read_file Fpath.(fixtures_root // v rel)

(* coupling: duplicated from Test_scan_subcommand.ml (hidden by its .mli).
 * Kept small and local to avoid widening the module's public surface. *)
let dummy_app_token : string = "FAKETESTINGAUTHTOKEN"

let without_settings (f : unit -> 'a) : 'a =
  Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml" f

let with_env_app_token ?(token : string = dummy_app_token) (f : unit -> 'a) : 'a =
  Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" token f

(* Shared masks for stable snapshots. The tool driver's "semanticVersion" is
 * the engine version and would make snapshots drift every release. The
 * root-level "version" is the SARIF spec version (stable) and must NOT be
 * masked. *)
let normalise : (string -> string) list =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_pcre_pattern {|"semanticVersion":"[^"]*"|};
  ]

(* Run the scan subcommand with --sarif over a fixture (rule + target) copied
 * into a throw-away git repo. Extra CLI flags can be appended. *)
let run_sarif_scan
    (caps : Scan_subcommand.caps)
    ~(rule : string)
    ~(targets : string list)
    ?(extra_args : string list = [])
    ()
  =
  let rule_content : string = read_fixture rule in
  let rule_file : string = Filename.basename rule in
  let target_entries : (string * string) list =
    List.map
      (fun (t : string) -> (Filename.basename t, read_fixture t))
      targets
  in
  with_env_app_token (fun () ->
      let repo_files : F.t list =
        F.File (rule_file, rule_content)
        :: List.map (fun ((name : string), (contents : string)) ->
               F.File (name, contents))
             target_entries
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                let argv : string array =
                  Array.of_list
                    ([
                       "opengrep-scan"; "--experimental";
                       "--config"; rule_file;
                       "--sarif";
                     ]
                    @ extra_args)
                in
                Scan_subcommand.main caps argv)
          in
          Exit_code.Check.ok exit_code))

(* Like run_sarif_scan but with caller-controlled format flags (no implicit
 * --sarif), for testing the -o/--<format>-output file destinations. After the
 * scan, the given output files are dumped on stdout so that they become part
 * of the snapshot, together with whatever the scan printed on stdout. *)
let run_scan_with_output_files (caps : Scan_subcommand.caps) ~(rule : string)
    ~(targets : string list) ~(args : string list) ~(output_files : string list)
    () =
  let rule_content : string = read_fixture rule in
  let rule_file : string = Filename.basename rule in
  let target_entries : (string * string) list =
    List.map (fun (t : string) -> (Filename.basename t, read_fixture t)) targets
  in
  with_env_app_token (fun () ->
      let repo_files : F.t list =
        F.File (rule_file, rule_content)
        :: List.map
             (fun ((name : string), (contents : string)) ->
               F.File (name, contents))
             target_entries
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                let argv : string array =
                  Array.of_list
                    ([
                       "opengrep-scan"; "--experimental"; "--config"; rule_file;
                     ]
                    @ args)
                in
                Scan_subcommand.main caps argv)
          in
          output_files
          |> List.iter (fun (path : string) ->
                 UCommon.pr (Printf.sprintf "--- content of %s ---" path);
                 UCommon.pr (UFile.read_file (Fpath.v path)));
          Exit_code.Check.ok exit_code))

(*****************************************************************************)
(* Individual tests                                                           *)
(*****************************************************************************)

(* Port of: test_sarif_output. Parameterised over rule + dataflow_traces. *)
let test_basic_sarif
    (caps : Scan_subcommand.caps)
    ~(rule : string)
    ~(target : string)
    ~(dataflow_traces : bool)
    ()
  =
  let extra_args : string list =
    "--verbose" :: (if dataflow_traces then [ "--dataflow-traces" ] else [])
  in
  run_sarif_scan caps ~rule ~targets:[ target ] ~extra_args ()

(* Port of: test_sarif_output_include_nosemgrep.
 * Verifies that nosemgrep-suppressed findings appear with a suppressions entry
 * in SARIF. *)
let test_sarif_nosemgrep (caps : Scan_subcommand.caps) () =
  run_sarif_scan caps
    ~rule:"rules/regex/regex-nosemgrep.yaml"
    ~targets:[ "targets/basic/regex-nosemgrep.txt" ]
    ()

(* Port of: test_sarif_output_rule_board.
 * Verifies rule-board metadata (metadata.semgrep.policy) reaches SARIF. *)
let test_sarif_rule_board (caps : Scan_subcommand.caps) () =
  run_sarif_scan caps
    ~rule:"rules/rule-board-eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ()

(* Port of: test_sarif_output_with_source.
 * Verifies that rules expose [helpUri] derived from metadata.source. The
 * Python test has a secondary MOCK_USING_REGISTRY run that is python-wrapper
 * specific; we keep only the behaviour exercised by the recorded snapshot. *)
let test_sarif_with_source (caps : Scan_subcommand.caps) () =
  run_sarif_scan caps
    ~rule:"rules/eqeq-source.yml"
    ~targets:[ "targets/basic/stupid.py" ]
    ()

(* Port of: test_sarif_output_with_source_edit.
 * Verifies that rich rule [help] (markdown + text) reaches SARIF. *)
let test_sarif_with_source_edit (caps : Scan_subcommand.caps) () =
  run_sarif_scan caps
    ~rule:"rules/eqeq-meta.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ()

(* Port of: test_sarif_output_with_autofix.
 * Verifies autofix suggestions appear as SARIF fixes. *)
let test_sarif_autofix (caps : Scan_subcommand.caps) () =
  run_sarif_scan caps
    ~rule:"rules/autofix/autofix.yaml"
    ~targets:[ "targets/autofix/autofix.py" ]
    ~extra_args:[ "--autofix"; "--dryrun" ]
    ()

(* Port of: test_sarif_output_with_dataflow_traces. *)
let test_sarif_dataflow_traces (caps : Scan_subcommand.caps) () =
  run_sarif_scan caps
    ~rule:"rules/taint.yaml"
    ~targets:[ "targets/taint/taint.py" ]
    ~extra_args:[ "--dataflow-traces" ]
    ()

(* --sarif-output alone: text report on stdout, SARIF written to the file. *)
let test_sarif_output_file (caps : Scan_subcommand.caps) () =
  run_scan_with_output_files caps ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~args:[ "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* --sarif --sarif-output: SARIF on stdout and in the file. *)
let test_sarif_output_file_with_sarif_stdout (caps : Scan_subcommand.caps) () =
  run_scan_with_output_files caps ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~args:[ "--sarif"; "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* --json --sarif-output: JSON on stdout, SARIF in the file. *)
let test_sarif_output_file_with_json_stdout (caps : Scan_subcommand.caps) () =
  run_scan_with_output_files caps ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~args:[ "--json"; "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* -o sends the primary format to the file instead of stdout. *)
let test_primary_output_to_file (caps : Scan_subcommand.caps) () =
  run_scan_with_output_files caps ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~args:[ "--sarif"; "-o"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* --sarif-output with a nested destination: parent directories are created. *)
let test_sarif_output_file_nested (caps : Scan_subcommand.caps) () =
  run_scan_with_output_files caps ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~args:[ "--sarif-output"; "sub/dir/findings.sarif" ]
    ~output_files:[ "sub/dir/findings.sarif" ]
    ()

(* Two different formats targeting the same destination must abort. *)
let test_conflicting_output_destination (caps : Scan_subcommand.caps) () =
  let rule_content : string = read_fixture "rules/eqeq.yaml" in
  let target_content : string = read_fixture "targets/basic/stupid.py" in
  with_env_app_token (fun () ->
      let repo_files : F.t list =
        [
          F.File ("eqeq.yaml", rule_content);
          F.File ("stupid.py", target_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          without_settings (fun () ->
              let argv : string array =
                [|
                  "opengrep-scan";
                  "--experimental";
                  "--config";
                  "eqeq.yaml";
                  "--json";
                  "-o";
                  "out.json";
                  "--sarif-output";
                  "out.json";
                |]
              in
              try
                let (_ : Exit_code.t) = Scan_subcommand.main caps argv in
                failwith
                  "expected the scan to abort on conflicting output formats"
              with
              | Error.Semgrep_error ((msg : string), _) ->
                  UCommon.pr ("aborted: " ^ msg))))

(*****************************************************************************)
(* Entry point                                                                *)
(*****************************************************************************)

let basic_cases : (string * string * string) list =
  [
    ("eqeq", "rules/eqeq.yaml", "targets/basic/stupid.py");
    ("cwe_tag", "rules/cwe_tag.yaml", "targets/basic/stupid.py");
    ("metavariable_type",
     "rules/metavariable_type.yaml",
     "targets/basic/stupid.py");
  ]

let tests (caps : < Scan_subcommand.caps >) =
  let basic_tests : Testo.t list =
    List.concat_map
      (fun ((label : string), (rule : string), (target : string)) ->
        [ true; false ]
        |> List.map (fun (dataflow_traces : bool) ->
               let suffix : string =
                 if dataflow_traces then " (dataflow-traces)" else ""
               in
               t
                 (Printf.sprintf "SARIF: basic %s%s" label suffix)
                 ~checked_output:(Testo.stdout ()) ~normalize:normalise
                 (test_basic_sarif caps ~rule ~target ~dataflow_traces)))
      basic_cases
  in
  Testo.categorize "Osemgrep Scan SARIF (e2e)"
    (basic_tests
     @ [
         t "SARIF: nosemgrep suppressions"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_nosemgrep caps);
         t "SARIF: rule-board metadata"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_rule_board caps);
         t "SARIF: rule metadata.source drives helpUri"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_with_source caps);
         t "SARIF: rule metadata drives rich help"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_with_source_edit caps);
         t "SARIF: autofix --dryrun"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_autofix caps);
         t "SARIF: taint --dataflow-traces"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_dataflow_traces caps);
         t "SARIF: --sarif-output file, text on stdout"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_output_file caps);
         t "SARIF: --sarif --sarif-output file"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_output_file_with_sarif_stdout caps);
         t "SARIF: --json --sarif-output file"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_output_file_with_json_stdout caps);
         t "SARIF: -o file, nothing on stdout"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_primary_output_to_file caps);
         t "SARIF: --sarif-output nested destination"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_sarif_output_file_nested caps);
         t "SARIF: conflicting formats for one destination"
           ~checked_output:(Testo.stdout ()) ~normalize:normalise
           (test_conflicting_output_destination caps);
       ])
