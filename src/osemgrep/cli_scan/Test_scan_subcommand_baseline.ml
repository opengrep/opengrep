(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of scans with --baseline-commit: only the findings that
 * the baseline commit does not have are reported. python: test_baseline.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The tests drive git themselves, which the scan does not need. *)
type caps = < Scan_subcommand.caps ; Cap.exec >

(* The pattern the scans look for, and a line with it. *)
let sentinel : string = "23478921"
let sentinel_line : string = Printf.sprintf "y = %s\n" sentinel

let git (caps : caps) (args : string list) : string =
  Git_wrapper.command (caps :> < Cap.exec >) args

(* Commit what is staged, and return the commit. *)
let commit (caps : caps) (message : string) : string =
  let (_ : string) = git caps [ "commit"; "-q"; "--allow-empty"; "-m"; message ] in
  String.trim (git caps [ "rev-parse"; "HEAD" ])

(* Scan the repo for the sentinel with the given baseline, in JSON. *)
let baseline_scan (caps : caps) ~(baseline : string) () : Exit_code.t =
  without_settings (fun () ->
      Scan_subcommand.main (caps :> Scan_subcommand.caps)
        [|
          "opengrep-scan"; "--experimental"; "--json"; "-e";
          Printf.sprintf "$X = %s" sentinel; "-l"; "python"; "--baseline-commit";
          baseline;
        |])

(* The paths a baseline scan of [root] reports, sorted. *)
let baseline_paths (caps : caps) ~(baseline : string) (root : string) :
    string list =
  let exit_code, out =
    Testo.with_capture stdout (fun () ->
        without_settings (fun () ->
            Scan_subcommand.main
              (caps :> Scan_subcommand.caps)
              [|
                "opengrep-scan"; "--experimental"; "--json"; "--quiet"; "-e";
                Printf.sprintf "$X = %s" sentinel; "-l"; "python";
                "--baseline-commit"; baseline; root;
              |]))
  in
  Exit_code.Check.ok exit_code;
  (Semgrep_output_v1_j.cli_output_of_string out).results
  |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) ->
         Fpath.to_string m.path)
  |> List.sort String.compare

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* A fresh checkout of the baseline looks modified when a .gitattributes
   normalises line endings that the committed blobs do not have; the
   temporary worktree must still be removed, and the scan succeed.
   python: test_baseline_worktree_dirty_from_eol_normalization *)
let test_worktree_dirty_from_eol_normalization (caps : caps) () =
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true
        [ F.File ("foo.py", "x = 1\r\n") ]
        (fun _cwd ->
          (* the normalisation comes after the CRLF blob, and is staged
             alone: 'git add .' would re-read foo.py and store it as LF *)
          UFile.write_file ~file:(Fpath.v ".gitattributes") "*.py text eol=lf\n";
          let (_ : string) = git caps [ "add"; ".gitattributes" ] in
          let baseline = commit caps "normalise line endings" in
          UFile.write_file ~file:(Fpath.v "foo.py") ("x = 1\r\n" ^ sentinel_line);
          let (_ : string) = git caps [ "add"; "." ] in
          let (_ : string) = commit caps "add the sentinel" in
          baseline_scan caps ~baseline () |> Exit_code.Check.ok))

(* From a subdirectory, a scanning root above it selects the changed files
   outside the current directory too. *)
let test_root_above_cwd_with_baseline (caps : caps) () =
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true
        [
          F.Dir ("sub", [ F.File ("a.py", "x = 1\n") ]);
          F.Dir ("other", [ F.File ("b.py", "y = 1\n") ]);
        ]
        (fun _cwd ->
          let baseline = String.trim (git caps [ "rev-parse"; "HEAD" ]) in
          UFile.write_file ~file:(Fpath.v "sub/a.py") ("x = 1\n" ^ sentinel_line);
          UFile.write_file ~file:(Fpath.v "other/b.py") ("y = 1\n" ^ sentinel_line);
          let (_ : string) = git caps [ "add"; "." ] in
          let (_ : string) = commit caps "two findings" in
          Testutil_files.with_chdir (Fpath.v "sub") (fun () ->
              Alcotest.(check (list string))
                "the whole repository from one of its subdirectories"
                [ "../other/b.py"; "a.py" ]
                (baseline_paths caps ~baseline "..");
              Alcotest.(check (list string))
                "a sibling directory of the current one" [ "../other/b.py" ]
                (baseline_paths caps ~baseline "../other"))))

(* The current directory need not exist at the baseline commit. *)
let test_cwd_absent_from_baseline (caps : caps) () =
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true
        [ F.File ("old.py", "x = 1\n") ]
        (fun _cwd ->
          let baseline = String.trim (git caps [ "rev-parse"; "HEAD" ]) in
          Unix.mkdir "new" 0o755;
          UFile.write_file ~file:(Fpath.v "new/n.py") sentinel_line;
          let (_ : string) = git caps [ "add"; "." ] in
          let (_ : string) = commit caps "a new directory" in
          Testutil_files.with_chdir (Fpath.v "new") (fun () ->
              Alcotest.(check (list string))
                "the finding of a directory the baseline does not have"
                [ "n.py" ]
                (baseline_paths caps ~baseline "."))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : caps) =
  Testo.categorize "Osemgrep Scan baseline (e2e)"
    [
      t "worktree dirty from end-of-line normalisation"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_worktree_dirty_from_eol_normalization caps);
      t "a scanning root above the current directory"
        (test_root_above_cwd_with_baseline caps);
      t "a current directory absent from the baseline commit"
        (test_cwd_absent_from_baseline caps);
    ]
