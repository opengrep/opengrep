(* SPDX-License-Identifier: LGPL-2.1-only *)

module F = Testutil_files

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Shared scaffolding for the end-to-end tests of the scan subcommand, used
 * by Test_scan_subcommand_sarif.ml and Test_scan_subcommand_output.ml.
 *
 * Fixture rules and targets live under tests/sarif/. The OCaml test harness
 * runs from the project root (see scripts/run-core-test), so relative paths
 * resolve before we descend into the temporary git repo.
 *)

(*****************************************************************************)
(* Fixtures and the environment *)
(*****************************************************************************)

let fixtures_root : Fpath.t = Fpath.v "tests/sarif"

let read_fixture (rel : string) : string =
  UFile.read_file Fpath.(fixtures_root // v rel)

let dummy_app_token : string = "FAKETESTINGAUTHTOKEN"

let without_settings (f : unit -> 'a) : 'a =
  Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml" f

let with_env_app_token ?(token : string = dummy_app_token) (f : unit -> 'a) : 'a
    =
  Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" token f

(* Shared masks for stable snapshots. The tool driver's "semanticVersion" is
 * the engine version and would make snapshots drift every release, and so
 * would the "version" that opens the JSON output, recognisable by the
 * "results" following it. The root-level "version" of SARIF is the spec
 * version (stable, followed by "runs") and must NOT be masked. *)
let normalise : (string -> string) list =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_pcre_pattern {|"semanticVersion":"[^"]*"|};
    Testo.mask_pcre_pattern {|\{"version":"([^"]*)","results"|};
  ]

(*****************************************************************************)
(* Running a scan *)
(*****************************************************************************)

(* Run the scan subcommand over a fixture (rule + targets) copied into a
 * throw-away git repo.
 * format_args defaults to --sarif and can be emptied to let extra_args pick
 * the formats. extra_files are added to the repo next to the fixtures.
 * output_files are dumped on stdout after the scan so that they become part
 * of the snapshot, together with whatever the scan printed there.
 * expect_abort makes the abort the expected outcome and prints its message.
 * check is the expected exit code of a scan that did not abort.
 *)
let run_scan (caps : Scan_subcommand.caps) ~(rule : string)
    ~(targets : string list) ?(format_args : string list = [ "--sarif" ])
    ?(extra_args : string list = []) ?(extra_files : F.t list = [])
    ?(output_files : string list = []) ?(expect_abort : bool = false)
    ?(check : Exit_code.t -> unit = Exit_code.Check.ok) ?(git : bool = true) ()
    =
  let rule_content : string = read_fixture rule in
  let rule_file : string = Filename.basename rule in
  let target_entries : (string * string) list =
    List.map (fun (t : string) -> (Filename.basename t, read_fixture t)) targets
  in
  with_env_app_token (fun () ->
      let repo_files : F.t list =
        (F.File (rule_file, rule_content)
        :: List.map
             (fun ((name : string), (contents : string)) ->
               F.File (name, contents))
             target_entries)
        @ extra_files
      in
      Testutil_git.with_git_repo ~verbose:true ~really_create_git_repo:git
        repo_files (fun _cwd ->
          let argv : string array =
            Array.of_list
              ([ "opengrep-scan"; "--experimental"; "--config"; rule_file ]
              @ format_args @ extra_args)
          in
          let run () =
            without_settings (fun () -> Scan_subcommand.main caps argv)
          in
          let exit_code : Exit_code.t option =
            if expect_abort then (
              try
                let (_ : Exit_code.t) = run () in
                failwith "expected the scan to abort"
              with
              | Error.Semgrep_error ((msg : string), _) ->
                  UCommon.pr ("aborted: " ^ msg);
                  None)
            else Some (run ())
          in
          output_files
          |> List.iter (fun (path : string) ->
                 UCommon.pr (Printf.sprintf "--- content of %s ---" path);
                 UCommon.pr (UFile.read_file (Fpath.v path)));
          match exit_code with
          | Some exit_code -> check exit_code
          | None -> ()))
