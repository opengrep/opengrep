(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of scans with --baseline-commit: only the findings that
 * the baseline commit does not have are reported. Each test prints the
 * findings of a plain scan and of a baseline scan, in text, on stdout.
 * python: test_baseline.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The tests drive git themselves, which the scan does not need. *)
type caps = < Scan_subcommand.caps ; Cap.exec >

(* The pattern the scans look for, and lines with it. *)
let sentinel : string = "23478921"
let x_line : string = Printf.sprintf "x = %s\n" sentinel
let y_line : string = Printf.sprintf "y = %s\n" sentinel

let git (caps : caps) (args : string list) : string =
  Git_wrapper.command (caps :> < Cap.exec >) args

let add_all (caps : caps) : unit = ignore (git caps [ "add"; "." ] : string)

(* Commit what is staged, and return the commit. The dates are fixed, on a
   year per [serial], as git chooses among several merge bases by date. *)
let commit ?(serial : int = 1) (caps : caps) (message : string) : string =
  let date = Printf.sprintf "Mon 10 Mar %d 00:00:00Z" (2000 + serial) in
  Semgrep_envvars.with_envvar "GIT_COMMITTER_DATE" date (fun () ->
      ignore
        (git caps
           [ "commit"; "-q"; "--allow-empty"; "-m"; message; "--date"; date ]
          : string));
  String.trim (git caps [ "rev-parse"; "HEAD" ])

let commit_all ?(serial : int = 1) (caps : caps) (message : string) : string
    =
  add_all caps;
  commit ~serial caps message

let write (name : string) (contents : string) : unit =
  UFile.write_file ~file:(Fpath.v name) contents

(* Scan for the sentinel, in text, with or without a baseline, after a line
   saying which. *)
let scan ?(baseline : string option) (caps : caps) : Exit_code.t =
  UCommon.pr
    (match baseline with
    | Some _ -> "--- scan with the baseline ---"
    | None -> "--- scan ---");
  let baseline_args =
    match baseline with
    | Some baseline -> [ "--baseline-commit"; baseline ]
    | None -> []
  in
  without_settings (fun () ->
      Scan_subcommand.main
        (caps :> Scan_subcommand.caps)
        (Array.of_list
           ([
              "opengrep-scan"; "--experimental"; "-e";
              Printf.sprintf "$X = %s" sentinel; "-l"; "python";
            ]
           @ baseline_args)))

(* A plain scan then a baseline scan, both expected to succeed. *)
let scan_both (caps : caps) ~(baseline : string) : unit =
  scan caps |> Exit_code.Check.ok;
  scan ~baseline caps |> Exit_code.Check.ok

let head (caps : caps) : string = String.trim (git caps [ "rev-parse"; "HEAD" ])

(* A repo with the given files committed, then [f]. *)
let in_repo (files : F.t list) (f : unit -> unit) : unit -> unit =
 fun () ->
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true files (fun _cwd -> f ()))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* python: test_one_commit_with_baseline; the baseline has both findings,
   the head only an empty commit on top *)
let test_one_commit_with_baseline (caps : caps) =
  in_repo [ F.File ("foo.py", x_line); F.File ("bar.py", y_line) ] (fun () ->
      let baseline = head caps in
      let (_ : string) = commit caps "noop" in
      scan_both caps ~baseline)

(* python: test_symlink; symlinks to a file, to a symlink, and broken *)
let test_symlink (caps : caps) =
  in_repo
    [
      F.File ("foo.py", x_line);
      F.File ("bar.py", y_line);
      F.Symlink ("bar_link.py", "bar.py");
      F.Symlink ("bar_link_link.py", "bar_link.py");
      F.Symlink ("broken_link.py", "broken");
    ]
    (fun () ->
      let baseline = head caps in
      let (_ : string) = commit caps "noop" in
      scan_both caps ~baseline)

(* python: test_renamed_dir; the files moved with their directory *)
let test_renamed_dir (caps : caps) =
  in_repo
    [ F.dir "dir_old" [ F.File ("foo.py", x_line); F.File ("bar.py", y_line) ] ]
    (fun () ->
      Sys.rename "dir_old" "dir_new";
      let (_ : string) = commit_all caps "rename the directory" in
      scan_both caps ~baseline:"HEAD^")

(* python: test_dir_symlink_changed; the symlink points to another
   directory *)
let test_dir_symlink_changed (caps : caps) =
  in_repo
    [
      F.dir "dir_one" [ F.File ("foo.py", x_line) ];
      F.dir "dir_two" [ F.File ("bar.py", y_line) ];
      F.Symlink ("dir_link", "dir_one");
    ]
    (fun () ->
      Sys.remove "dir_link";
      Unix.symlink "dir_two" "dir_link";
      let (_ : string) = commit_all caps "point the symlink elsewhere" in
      scan_both caps ~baseline:"HEAD^")

(* python: test_file_changed_to_dir *)
let test_file_changed_to_dir (caps : caps) =
  in_repo [ F.File ("file_or_dir.py", x_line) ] (fun () ->
      Sys.remove "file_or_dir.py";
      Unix.mkdir "file_or_dir.py" 0o755;
      write "file_or_dir.py/foo.py" x_line;
      write "file_or_dir.py/bar.py" y_line;
      let (_ : string) = commit_all caps "a directory instead" in
      scan_both caps ~baseline:"HEAD^")

(* python: test_dir_changed_to_file *)
let test_dir_changed_to_file (caps : caps) =
  in_repo
    [ F.dir "file_or_dir.py" [ F.File ("foo.py", x_line); F.File ("bar.py", y_line) ] ]
    (fun () ->
      Sys.remove "file_or_dir.py/foo.py";
      Sys.remove "file_or_dir.py/bar.py";
      Unix.rmdir "file_or_dir.py";
      write "file_or_dir.py" x_line;
      let (_ : string) = commit_all caps "a file instead" in
      scan_both caps ~baseline:"HEAD^")

(* python: test_no_findings_both *)
let test_no_findings_both (caps : caps) =
  in_repo [ F.File ("foo.py", "x = 1\n"); F.File ("bar.py", "y = 1\n") ]
    (fun () ->
      let baseline = head caps in
      write "baz.py" "z = 1";
      let (_ : string) = commit_all caps "another file without finding" in
      scan_both caps ~baseline)

(* python: test_file_changed_to_symlink *)
let test_file_changed_to_symlink (caps : caps) =
  in_repo [ F.File ("file_or_link.py", x_line) ] (fun () ->
      Sys.rename "file_or_link.py" "definitely_a_file.py";
      Unix.symlink "definitely_a_file.py" "file_or_link.py";
      let (_ : string) = commit_all caps "a symlink instead" in
      scan_both caps ~baseline:"HEAD^")

(* python: test_symlink_changed_to_file *)
let test_symlink_changed_to_file (caps : caps) =
  in_repo
    [
      F.File ("definitely_a_file.py", x_line);
      F.Symlink ("symlink_or_file.py", "definitely_a_file.py");
    ]
    (fun () ->
      Sys.remove "symlink_or_file.py";
      Sys.rename "definitely_a_file.py" "symlink_or_file.py";
      let (_ : string) = commit_all caps "a file instead" in
      scan_both caps ~baseline:"HEAD^")

(* python: test_no_findings_head; the baseline had findings, the head
   has none *)
let test_no_findings_head (caps : caps) =
  in_repo [ F.File ("foo.py", x_line); F.File ("bar.py", y_line) ] (fun () ->
      let baseline = head caps in
      write "baz.py" "z  = 1";
      write "foo.py" "";
      write "bar.py" "";
      let (_ : string) = commit_all caps "remove the findings" in
      scan_both caps ~baseline)

(* python: test_no_findings_baseline; the head has all the findings *)
let test_no_findings_baseline (caps : caps) =
  in_repo [ F.File ("foo.py", "x = 1") ] (fun () ->
      let baseline = head caps in
      write "bar.py" y_line;
      write "foo.py" x_line;
      let (_ : string) = commit_all caps "add the findings" in
      scan_both caps ~baseline)

(* python: test_some_intersection; one finding is in the baseline, one is
   new *)
let test_some_intersection (caps : caps) =
  in_repo [ F.File ("foo.py", x_line) ] (fun () ->
      let baseline = head caps in
      write "bar.py" y_line;
      let (_ : string) = commit_all caps "add a finding" in
      scan_both caps ~baseline)

(* python: test_all_intersect; the head only adds a line without finding *)
let test_all_intersect (caps : caps) =
  in_repo [ F.File ("foo.py", x_line); F.File ("bar.py", y_line) ] (fun () ->
      let baseline = head caps in
      write "foo.py" (x_line ^ "z = 1\n");
      let (_ : string) = commit_all caps "noop" in
      scan_both caps ~baseline)

(* python: test_no_intersection; the baseline finding is gone, another is
   new *)
let test_no_intersection (caps : caps) =
  in_repo [ F.File ("foo.py", Printf.sprintf "x = %s" sentinel) ] (fun () ->
      let baseline = head caps in
      write "bar.py" y_line;
      write "foo.py" "";
      let (_ : string) = commit_all caps "move the finding" in
      scan_both caps ~baseline)

(* python: test_renamed_file; enough unchanged text for git to see a
   rename, and a new finding in the renamed file *)
let test_renamed_file (caps : caps) ~(new_name : string) =
  let filler = String.concat "" (List.init 100 (fun _ -> "1\n\n")) in
  in_repo
    [ F.File ("foo.py", filler ^ Printf.sprintf "x = %s" sentinel) ]
    (fun () ->
      let baseline = head caps in
      let (_ : string) = git caps [ "mv"; "foo.py"; new_name ] in
      write new_name
        (filler ^ Printf.sprintf "x = %s\n\ny = %s" sentinel sentinel);
      let (_ : string) = commit_all caps "rename and add a finding" in
      scan_both caps ~baseline;
      let remaining =
        Sys.readdir "." |> Array.to_list
        |> List.filter (fun (name : string) -> Filename.check_suffix name ".py")
      in
      Alcotest.(check (list string)) "the old path is gone" [ new_name ] remaining)

(* python: test_unstaged_changes; the scan does not abort on them *)
let test_unstaged_changes (caps : caps) =
  in_repo [ F.dir "foo" [ F.File ("a.py", "") ] ] (fun () ->
      let baseline = head caps in
      write "foo/a.py" y_line;
      scan ~baseline caps |> Exit_code.Check.ok)

(* python: test_staged_changes; the findings of staged changes are
   reported *)
let test_staged_changes (caps : caps) =
  in_repo [ F.dir "foo" [ F.File ("a.py", y_line) ] ] (fun () ->
      write "foo/a.py" (y_line ^ Printf.sprintf "x = %s" sentinel);
      add_all caps;
      let baseline = head caps in
      scan ~baseline caps |> Exit_code.Check.ok)

(* A baseline scan that cannot run aborts: the CLI turns the git error
   into a fatal exit, the subcommand called directly raises it. *)
let expect_abort (f : unit -> Exit_code.t) : unit =
  match f () with
  | exception Error.Semgrep_error ((msg : string), _)
  | exception Git_wrapper.Error (msg : string) ->
      UCommon.pr ("aborted: " ^ msg)
  | exit_code -> Exit_code.Check.fatal exit_code

(* python: test_not_git_directory *)
let test_not_git_directory (caps : caps) () =
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true ~really_create_git_repo:false
        [ F.dir "foo" [ F.File ("a.py", "y = 55555555\n") ] ]
        (fun _cwd -> expect_abort (fun () -> scan ~baseline:"12345" caps)))

(* python: test_commit_doesnt_exist *)
let test_commit_doesnt_exist (caps : caps) =
  in_repo [ F.dir "foo" [ F.File ("a.py", "") ] ] (fun () ->
      expect_abort (fun () -> scan ~baseline:"12345" caps))

(* python: complex_merge_repo; three branches, foo, bar and baz, each
   growing a file by one finding per commit, with merges between them *)
let with_crisscrossing_merges (caps : caps) (f : unit -> unit) : unit =
  let append (name : string) (line : string) : unit =
    let old = if Sys.file_exists name then UFile.read_file (Fpath.v name) else "" in
    write name (old ^ line)
  in
  let grow (name : string) : string list =
    List.init 9 (fun (i : int) ->
        append (name ^ ".py") (Printf.sprintf "%s = %s\n\n" name sentinel);
        commit_all ~serial:(i + 1) caps (Printf.sprintf "%s #%d" name (i + 1)))
  in
  let checkout (args : string list) : unit =
    ignore (git caps ("checkout" :: "-q" :: args) : string)
  in
  (* the merges are dated before every commit *)
  let merge (rev : string) : unit =
    let date = "Mon 10 Mar 2000 00:00:00Z" in
    Semgrep_envvars.with_envvar "GIT_COMMITTER_DATE" date (fun () ->
        ignore
          (git caps
             [
               "merge"; "-q"; "--allow-unrelated-histories"; rev; "-m";
               "merge " ^ rev;
             ]
            : string);
        ignore
          (git caps [ "commit"; "-q"; "--amend"; "--no-edit"; "--date"; date ]
            : string))
  in
  checkout [ "-b"; "foo" ];
  let foo_commits = grow "foo" in
  checkout [ List.hd foo_commits ];
  checkout [ "-b"; "bar" ];
  let bar_commits = grow "bar" in
  checkout [ "foo" ];
  merge "bar~6";
  checkout [ List.hd foo_commits ];
  checkout [ "-b"; "baz" ];
  (* every second foo commit against every third bar commit, three pairs *)
  let every (n : int) (commits : string list) : string list =
    List.filteri (fun (i : int) (_ : string) -> i mod n = 0) commits
  in
  let first_three (commits : string list) : string list =
    List.filteri (fun (i : int) (_ : string) -> i < 3) commits
  in
  List.iter2
    (fun (foo_commit : string) (bar_commit : string) ->
      merge foo_commit;
      merge bar_commit)
    (first_three (every 2 foo_commits))
    (first_three (every 3 bar_commits));
  let (_ : string list) = grow "baz" in
  f ()

(* python: test_crisscrossing_merges *)
let test_crisscrossing_merges (caps : caps) ~(current : string)
    ~(baseline : string) =
  in_repo [ F.File ("README", "") ] (fun () ->
      with_crisscrossing_merges caps (fun () ->
          ignore (git caps [ "checkout"; "-q"; current ] : string);
          scan ~baseline caps |> Exit_code.Check.ok))

(* python: test_conflicting_file_and_main_branch_names; a file named like
   the branch is no ambiguity for the baseline *)
let test_conflicting_file_and_main_branch_names (caps : caps) =
  in_repo [ F.File ("main", "this is a file named 'main'\n") ] (fun () ->
      scan ~baseline:"main" caps |> Exit_code.Check.ok)

(* A fresh checkout of the baseline looks modified when a .gitattributes
   normalises line endings that the committed blobs do not have; the
   temporary worktree must still be removed, and the scan succeed.
   python: test_baseline_worktree_dirty_from_eol_normalization *)
let test_worktree_dirty_from_eol_normalization (caps : caps) =
  in_repo [ F.File ("foo.py", "x = 1\r\n") ] (fun () ->
      (* the normalisation comes after the CRLF blob, and is staged
         alone: 'git add .' would re-read foo.py and store it as LF *)
      write ".gitattributes" "*.py text eol=lf\n";
      let (_ : string) = git caps [ "add"; ".gitattributes" ] in
      let baseline = commit caps "normalise line endings" in
      write "foo.py" ("x = 1\r\n" ^ Printf.sprintf "y = %s\r\n" sentinel);
      let (_ : string) = commit_all caps "add the sentinel" in
      scan ~baseline caps |> Exit_code.Check.ok)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : caps) =
  let text (name : string) (func : unit -> unit) =
    t name ~checked_output:(Testo.stdout ()) ~normalize:normalise func
  in
  Testo.categorize "Osemgrep Scan baseline (e2e)"
    ([
       text "one commit with the baseline" (test_one_commit_with_baseline caps);
       text "symlinks" (test_symlink caps);
       text "renamed directory" (test_renamed_dir caps);
       text "directory symlink changed" (test_dir_symlink_changed caps);
       text "file changed to a directory" (test_file_changed_to_dir caps);
       text "directory changed to a file" (test_dir_changed_to_file caps);
       text "no findings in either" (test_no_findings_both caps);
       text "file changed to a symlink" (test_file_changed_to_symlink caps);
       text "symlink changed to a file" (test_symlink_changed_to_file caps);
       text "no findings in the head" (test_no_findings_head caps);
       text "no findings in the baseline" (test_no_findings_baseline caps);
       text "some findings in common" (test_some_intersection caps);
       text "all findings in common" (test_all_intersect caps);
       text "no finding in common" (test_no_intersection caps);
       text "renamed file" (test_renamed_file caps ~new_name:"bar.py");
       text "renamed file, case only" (test_renamed_file caps ~new_name:"Foo.py");
       text "unstaged changes" (test_unstaged_changes caps);
       text "staged changes" (test_staged_changes caps);
       text "not a git directory" (test_not_git_directory caps);
       text "the baseline commit does not exist" (test_commit_doesnt_exist caps);
       text "a file named like the main branch"
         (test_conflicting_file_and_main_branch_names caps);
       text "worktree dirty from end-of-line normalisation"
         (test_worktree_dirty_from_eol_normalization caps);
     ]
    @ (List.concat_map
         (fun (current : string) ->
           List.filter_map
             (fun (baseline : string) ->
               if String.equal current baseline then None
               else
                 Some
                   (text
                      (Printf.sprintf "crisscrossing merges: %s against %s"
                         current baseline)
                      (test_crisscrossing_merges caps ~current ~baseline)))
             [ "foo"; "bar"; "baz" ])
         [ "foo"; "bar"; "baz" ]))
