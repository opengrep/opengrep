(* SPDX-License-Identifier: LGPL-2.1-only *)

module F = Testutil_files
open Fpath_.Operators

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

let read_fixture ?(root : Fpath.t = fixtures_root) (rel : string) : string =
  UFile.read_file Fpath.(root // v rel)

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
(* Targets that are not files *)
(*****************************************************************************)

let random_init = lazy (Random.self_init ())

let create_named_pipe () : Fpath.t =
  Lazy.force random_init;
  let path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "opengrep-test-%x.py" (Random.bits ()))
  in
  Unix.mkfifo path 0o644;
  Fpath.v path

(* Run [func] on a named pipe that another process feeds with [data].
   This probably doesn't work on Windows due to the reliance on a shell
   command but could be ported (it doesn't need 'fork').
   TODO: switch to OCaml 5 and use parallelism. *)
let with_read_from_named_pipe ~(data : string) (func : Fpath.t -> 'a) : 'a =
  let pipe_path = create_named_pipe () in
  Common.protect
    (fun () ->
      (* Start another process to write to the pipe in parallel *)
      UTmp.with_temp_file (fun reg_file ->
          (* We go through a regular file so as to avoid quoting issues. *)
          UFile.write_file ~file:reg_file data;
          let writer_command =
            (* Copy the data from the regular file into the named pipe *)
            Printf.sprintf "cat '%s' >> '%s'" !!reg_file !!pipe_path
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

(* Run [func] with the standard input reading [data]. *)
let with_stdin_from ~(data : string) (func : unit -> 'a) : 'a =
  let saved_stdin = Unix.dup Unix.stdin in
  let reader, writer = Unix.pipe () in
  let (_ : int) = Unix.write_substring writer data 0 (String.length data) in
  Unix.close writer;
  Unix.dup2 reader Unix.stdin;
  Unix.close reader;
  Common.protect func ~finally:(fun () ->
      Unix.dup2 saved_stdin Unix.stdin;
      Unix.close saved_stdin)

(* The paths of such targets are temporary files with random names. *)
let mask_temp_targets : (string -> string) list =
  [
    Testo.mask_pcre_pattern
      {|opengrep-(?:stdin|named-pipe)-[0-9a-f]+(?:-[0-9]+)?|};
    Testo.mask_pcre_pattern {|"fingerprint":"([0-9a-f_]+)"|};
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
let run_scan (caps : Scan_subcommand.caps) ?(root : Fpath.t = fixtures_root)
    ?(rule : string option) ~(targets : string list)
    ?(format_args : string list = [ "--sarif" ])
    ?(extra_args : string list = []) ?(extra_files : F.t list = [])
    ?(output_files : string list = []) ?(expect_abort : bool = false)
    ?(check : Exit_code.t -> unit = Exit_code.Check.ok) ?(git : bool = true) ()
    =
  (* the rule file copied into the repo and named on the command line;
     none when the scan gets its rules another way, with -e *)
  let rule_file : (string * string) option =
    Option.map
      (fun (rule : string) -> (Filename.basename rule, read_fixture ~root rule))
      rule
  in
  let target_entries : (string * string) list =
    List.map
      (fun (t : string) -> (Filename.basename t, read_fixture ~root t))
      targets
  in
  with_env_app_token (fun () ->
      let repo_files : F.t list =
        List.map
          (fun ((name : string), (contents : string)) -> F.File (name, contents))
          (Option.to_list rule_file @ target_entries)
        @ extra_files
      in
      Testutil_git.with_git_repo ~verbose:true ~really_create_git_repo:git
        repo_files (fun _cwd ->
          let config_args : string list =
            match rule_file with
            | Some ((name : string), _) -> [ "--config"; name ]
            | None -> []
          in
          let argv : string array =
            Array.of_list
              ([ "opengrep-scan"; "--experimental" ]
              @ config_args @ format_args @ extra_args)
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
