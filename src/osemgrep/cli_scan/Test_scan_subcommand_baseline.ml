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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : caps) =
  Testo.categorize "Osemgrep Scan baseline (e2e)"
    [
      t "worktree dirty from end-of-line normalisation"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_worktree_dirty_from_eol_normalization caps);
    ]
