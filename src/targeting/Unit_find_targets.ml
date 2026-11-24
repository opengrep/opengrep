(*
   Tests for the Find_targets module.

   Unlike the unit tests for semgrepignore/include/exclude
   (in Unit_semgrepignore), this exercises the two main ways we have to list
   target files:

   - using 'git ls-files';
   - by scanning the file system ourselves.

   They differ in that the former returns a list of regular files, while
   the latter performs filtering on folder paths.
*)

open Printf
open Fpath_.Operators
module F = Testutil_files
module Out = Semgrep_output_v1_t

(*
   TODO: create two test environments:
   - git repo
   - not a git repo
*)

(*
   Generic function that puts files into a temporary workspace and lists them.

   with_git: make this a git repository
   non_git_files: extra files that must be created but won't be git-added
                  (only relevant if with_git is true)
   cwd: a folder of the workspace to run from, instead of its root
   scanning_root: may start with "<root>", the absolute path of the workspace
   project_root: a folder of the workspace forced as the project root,
                 like --project-root
   honor_gitignore: false to commit the files that '.gitignore' lists too
*)
let test_find_targets ?expected_outcome ?includes ?(excludes = [])
    ?(non_git_files : F.t list = []) ~with_git ?(honor_gitignore = true)
    ?(cwd = ".") ?(scanning_root = ".") ?project_root name (files : F.t list) =
  let category = if with_git then "with git" else "without git" in
  let test_func () =
    printf "Test name: %s > %s\n" category name;
    let git_files, non_git_files =
      if with_git then (F.sort files, F.sort non_git_files)
      else ([], F.sort (files @ non_git_files))
    in
    (match git_files with
    | [] -> ()
    | _ ->
        printf "--- Files added before 'git add' ---\n";
        F.print_files git_files);
    (match non_git_files with
    | [] -> ()
    | _ ->
        printf "--- Files not added to git ---\n";
        F.print_files non_git_files);

    Testutil_git.with_git_repo ~verbose:true ~honor_gitignore
      ~really_create_git_repo:with_git git_files (fun root ->
        F.write root non_git_files;

        let conf =
          {
            Find_targets.default_conf with
            include_ = includes;
            exclude = excludes;
            force_project_root =
              Option.map
                (fun (dir : string) ->
                  Find_targets.Filesystem (Rfpath.of_fpath_exn (root / dir)))
                project_root;
          }
        in
        let scanning_root =
          let placeholder = "<root>" in
          if String.starts_with ~prefix:placeholder scanning_root then
            !!root
            ^ String.sub scanning_root (String.length placeholder)
                (String.length scanning_root - String.length placeholder)
          else scanning_root
        in
        let targets =
          F.with_chdir (Fpath.v cwd) (fun () ->
              Find_targets.get_target_fpaths conf
                [ Scanning_root.of_fpath (Fpath.v scanning_root) ])
        in
        (match includes with
        | None -> ()
        | Some patterns ->
            printf "--- '--include' patterns ---\n";
            patterns |> List.iter (fun pat -> printf "%s\n" pat));
        (match excludes with
        | [] -> ()
        | patterns ->
            printf "--- '--exclude' patterns ---\n";
            patterns |> List.iter (fun pat -> printf "%s\n" pat));
        printf "--- Selected targets ---\n";
        targets.selected |> List.iter (fun path -> printf "selected %s\n" !!path);
        printf "--- Skipped targets ---\n";
        targets.skipped
        |> List.iter (fun (x : Out.skipped_target) ->
               printf "ignored %s [%s]\n" !!(x.path)
                 (Out.show_skip_reason x.reason)))
  in
  Testo.create name test_func ~category:[ category ] ?expected_outcome
    ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        Testutil.mask_temp_paths ();
        Testo.mask_line ~after:"(root-commit) " ~before:"]" ();
      ]

let tests_with_or_without_git ~with_git =
  [
    test_find_targets ~with_git "basic test" [ F.File (".gitignore", "") ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git "basic semgrepignore"
      [ F.File (".semgrepignore", "b\n"); F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git ~excludes:[ "b" ] "basic exclude"
      [ F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git ~includes:[ "a" ] "basic include"
      [ F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git ~includes:[ "a" ] "deep include"
      [ F.dir "dir" [ F.file "a"; F.file "b" ] ];
    test_find_targets ~with_git ~scanning_root:"a.py" "scanning root as a file"
      [ F.file "a.py" ];
    test_find_targets ~with_git ~scanning_root:"a.py"
      "scanning root as a symlink to a regular file"
      [ F.Symlink ("a.py", "b.py"); F.File ("b.py", "some content") ];
    test_find_targets ~with_git ~scanning_root:"a.py"
      "scanning root as a symlink to a missing regular file"
      [ F.Symlink ("a.py", "b.py") ];
    test_find_targets ~with_git ~scanning_root:"link-to-src"
      "scanning root as a symlink to a folder"
      [ F.dir "src" [ F.file "a.py" ]; F.Symlink ("link-to-src", "src") ];
    (*
       Test that the '--include' filter takes place after all the other
       filters.
    *)
    (* Can't select file 'a' via --include when semgrepignoring its folder. *)
    test_find_targets ~with_git ~includes:[ "*.c" ]
      "semgrepignore file takes precedence over --include"
      [
        F.File (".semgrepignore", "dir\n");
        F.dir "dir" [ F.file "a.c"; F.file "b.c" ];
        F.file "c.c";
      ];
    (* An explicit target is a scanning root that's also a target file
       and should not be ignored by the usual exclusion mechanisms
       (.semgrepignore, --include, --exclude) *)
    test_find_targets ~with_git ~scanning_root:"a.py" "scan explicit target"
      [ F.file "a.py"; F.File (".semgrepignore", "a.py\n") ];
    (* The paths keep the scanning root as given, whatever its form
       and wherever the command runs. *)
    test_find_targets ~with_git ~cwd:"dir" ~scanning_root:".."
      "scanning root above the working directory"
      [ F.dir "dir" [ F.file "a.c" ]; F.file "c.c" ];
    test_find_targets ~with_git ~cwd:"dir" ~scanning_root:"../dir"
      "scanning root written through the parent"
      [ F.dir "dir" [ F.file "a.c" ]; F.file "c.c" ];
    test_find_targets ~with_git ~cwd:"dir" ~scanning_root:"<root>"
      "absolute scanning root above the working directory"
      [ F.dir "dir" [ F.file "a.c" ]; F.file "c.c" ];
    test_find_targets ~with_git ~scanning_root:"<root>/dir"
      "absolute scanning root below the working directory"
      [ F.dir "dir" [ F.file "a.c" ]; F.file "c.c" ];
    (* A forced project root takes the scanning root as given: the symlink
       leaves the folder but its name is inside it. *)
    test_find_targets ~with_git ~project_root:"dir" ~scanning_root:"dir/link"
      "forced project root with a symlink leaving it"
      [
        F.dir "dir" [ F.Symlink ("link", "../other") ];
        F.dir "other" [ F.file "a.c" ];
      ];
    (* A symlink inside the project stays a name in the project path: the
       ignore file is anchored on the name, not on the folder it points to. *)
    test_find_targets ~with_git ~scanning_root:"link"
      "symlink inside the project keeps its name"
      [
        F.File (".semgrepignore", "link/b.c\n");
        F.dir "dir" [ F.file "a.c"; F.file "b.c" ];
        F.Symlink ("link", "dir");
      ];
    (* In a git project the repository root is the project, so its ignore
       file applies to a scanning root under it, with its patterns anchored
       at the repository root. Outside a VCS the scanning root is its own
       project and an ignore file above it is not read. *)
    test_find_targets ~with_git ~scanning_root:"dir"
      "semgrepignore above the scanning root"
      [ F.File (".semgrepignore", "dir/b\n"); F.dir "dir" [ F.file "a"; F.file "b" ] ];
    (* Outside a VCS the ignore file of the scanning root itself is read,
       whatever the folder the command runs from: only 'b' is ignored. In a
       git project both files are read, so 'a' is ignored as well. *)
    test_find_targets ~with_git ~scanning_root:"dir"
      "semgrepignore at the scanning root"
      [
        F.File (".semgrepignore", "a\n");
        F.dir "dir" [ F.File (".semgrepignore", "b\n"); F.file "a"; F.file "b" ];
      ];
    (* An ignore file in the folder the command runs from does not apply to
       a root outside it, whether or not its patterns are anchored: the
       folder the command runs from is not special. *)
    test_find_targets ~with_git ~cwd:"here" ~scanning_root:"../there"
      "semgrepignore of the working directory does not apply outside it"
      [
        F.dir "here" [ F.File (".semgrepignore", "deep/\n/anchored/\n") ];
        F.dir "there"
          [
            F.file "a.c";
            F.dir "deep" [ F.file "b.c" ];
            F.dir "anchored" [ F.file "c.c" ];
          ];
      ];
    (* An ignored folder given as the scanning root is not scanned. *)
    test_find_targets ~with_git ~scanning_root:"dir"
      "scanning root is a semgrepignored folder"
      [ F.File (".semgrepignore", "dir/\n"); F.dir "dir" [ F.file "a.c" ] ];
    (* The folder is reported once, not each file under it. *)
    test_find_targets ~with_git "semgrepignored folder is reported once"
      [
        F.File (".semgrepignore", "dir/\n");
        F.dir "dir" [ F.file "a.c"; F.file "b.c" ];
        F.file "c.c";
      ];
    (* The paths keep the scanning root as given, through the symlink. *)
    test_find_targets ~with_git ~scanning_root:"link/sub"
      "scanning root under a symlinked folder"
      [
        F.dir "dir"
          [
            F.dir "sub"
              [
                F.File (".semgrepignore", "x/\n");
                F.dir "x" [ F.file "a.c" ];
                F.file "b.c";
              ];
          ];
        F.Symlink ("link", "dir");
      ];
    (*
       A directory is decided by the built-in patterns, then the
       command-line patterns, then the semgrepignore files, read from the
       project root down as one gitignore. Within each, every pattern is
       tested on the directory with and without a trailing slash
       (directory-only patterns need the slash), and the last one that
       matches decides. A pattern ending with '/**' matches what is inside
       the directory, not the directory.
    *)
    (* Select 'app/y.js' and 'ok.js'. *)
    test_find_targets ~with_git
      "semgrepignore directory pattern then negation of the directory"
      [ F.File (".semgrepignore", "app/\n!app\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'ok.js', not 'app/y.js': 'app/' is the last pattern that
       matches 'app'. *)
    test_find_targets ~with_git
      "semgrepignore negation of the directory then directory pattern"
      [ F.File (".semgrepignore", "!app\napp/\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'app/y.js' and 'ok.js'. *)
    test_find_targets ~with_git
      "semgrepignore directory pattern then negated directory pattern"
      [ F.File (".semgrepignore", "app/\n!app/\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'ok.js', not 'app/y.js'. *)
    test_find_targets ~with_git
      "semgrepignore negated directory pattern then directory pattern"
      [ F.File (".semgrepignore", "!app/\napp/\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'app/y.js' and 'ok.js': '!app/' is the last pattern that
       matches 'app'. *)
    test_find_targets ~with_git
      "semgrepignore pattern then negated directory pattern"
      [ F.File (".semgrepignore", "app\n!app/\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'ok.js', not 'app.js': '!app.js/' applies to directories
       only. *)
    test_find_targets ~with_git
      "semgrepignore pattern then negated directory pattern, on a file"
      [ F.File (".semgrepignore", "app.js\n!app.js/\n"); F.file "app.js"; F.file "ok.js" ];
    (* Select 'ok.js', not 'a/b/x.js' or 'b/y.js': the negation matches 'a',
       not 'a/b'. *)
    test_find_targets ~with_git
      "semgrepignore directory pattern below a negated directory"
      [
        F.File (".semgrepignore", "!a\nb/\n");
        F.dir "a" [ F.dir "b" [ F.file "x.js" ] ];
        F.dir "b" [ F.file "y.js" ];
        F.file "ok.js";
      ];
    (* Select 'sub/keep.js' and 'ok.js', not 'sub/drop.js': the
       semgrepignore of 'sub' applies to the paths under 'sub', not to
       'sub' itself. *)
    test_find_targets ~with_git
      "semgrepignore of a directory matching everything under it"
      [
        F.dir "sub"
          [
            F.File (".semgrepignore", "**\n!keep.js\n");
            F.file "keep.js";
            F.file "drop.js";
          ];
        F.file "ok.js";
      ];
    (* Select 'frontend/src/app/x.js', not 'app/y.js'. *)
    test_find_targets ~with_git
      "semgrepignore negation re-includes a nested directory"
      [
        F.File (".semgrepignore", "app/\n!frontend/src/app\n");
        F.dir "app" [ F.file "y.js" ];
        F.dir "frontend" [ F.dir "src" [ F.dir "app" [ F.file "x.js" ] ] ];
      ];
    (* Select 'src/app/x.js', not 'app/y.js': the '!app' of 'src' comes
       after the root's 'app/'. *)
    test_find_targets ~with_git
      "nested semgrepignore negation against a root directory pattern"
      [
        F.File (".semgrepignore", "app/\n");
        F.dir "app" [ F.file "y.js" ];
        F.dir "src" [ F.File (".semgrepignore", "!app\n"); F.dir "app" [ F.file "x.js" ] ];
      ];
    (* Select 'src/app/x.js', not 'app/y.js': the '!app' of 'src' comes
       after the root's 'app'. *)
    test_find_targets ~with_git
      "nested semgrepignore negation against a root pattern"
      [
        F.File (".semgrepignore", "app\n");
        F.dir "app" [ F.file "y.js" ];
        F.dir "src" [ F.File (".semgrepignore", "!app\n"); F.dir "app" [ F.file "x.js" ] ];
      ];
    (* Select 'app/y.js', not 'src/app/x.js': the 'app/' of 'src' comes
       after the root's '!src/app'. *)
    test_find_targets ~with_git
      "root semgrepignore negation against a nested directory pattern"
      [
        F.File (".semgrepignore", "!src/app\n");
        F.dir "app" [ F.file "y.js" ];
        F.dir "src" [ F.File (".semgrepignore", "app/\n"); F.dir "app" [ F.file "x.js" ] ];
      ];
    (* Select 'frontend/src/app/x.js', not 'app/y.js'. With git, git leaves
       out 'app/y.js' and lists 'frontend/src/app/x.js', which the filter
       keeps. *)
    test_find_targets ~with_git
      "semgrepignore including a gitignore with a negation"
      [
        F.File (".gitignore", "app/\n!frontend/src/app\n");
        F.File (".semgrepignore", ":include .gitignore\n");
        F.dir "app" [ F.file "y.js" ];
        F.dir "frontend" [ F.dir "src" [ F.dir "app" [ F.file "x.js" ] ] ];
      ];
    (* Select 'ok.js', not 'app/y.js': a negation does not re-include a
       file under an ignored directory. *)
    test_find_targets ~with_git
      "semgrepignore negation of a file under an ignored directory"
      [ F.File (".semgrepignore", "app/\n!app/y.js\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'app/keep.js' and 'ok.js', not 'app/drop.js': 'app/**'
       ignores what is inside 'app', not 'app', so '!app/keep.js' brings
       'app/keep.js' back. *)
    test_find_targets ~with_git
      "semgrepignore negation of a file under 'dir/**'"
      [
        F.File (".semgrepignore", "app/**\n!app/keep.js\n");
        F.dir "app" [ F.file "keep.js"; F.file "drop.js" ];
        F.file "ok.js";
      ];
    (* Select 'ok.js', not 'app/a.js' or 'app/b.js': the files of 'app' are
       ignored one by one, not 'app'. *)
    test_find_targets ~with_git
      "semgrepignore 'dir/**' ignores what is inside the directory"
      [
        F.File (".semgrepignore", "app/**\n");
        F.dir "app" [ F.file "a.js"; F.file "b.js" ];
        F.file "ok.js";
      ];
    (* Select 'ok.js', not 'app/y.js': '!app/**' does not match 'app', which
       'app' ignores, and nothing under an ignored directory is brought
       back. *)
    test_find_targets ~with_git
      "semgrepignore negation of 'dir/**' under an ignored directory"
      [
        F.File (".semgrepignore", "app\n!app/**\n");
        F.dir "app" [ F.file "y.js" ];
        F.file "ok.js";
      ];
    (* Select 'ok.js', not 'app/y.js': '--exclude' comes before the
       semgrepignore files. *)
    test_find_targets ~with_git ~excludes:[ "app" ]
      "exclude takes precedence over a semgrepignore negation"
      [ F.File (".semgrepignore", "!app\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'ok.js', not 'src/tests/t.js': without a root semgrepignore
       the built-in patterns apply, before the nested semgrepignore files. *)
    test_find_targets ~with_git
      "built-in patterns take precedence over a nested semgrepignore negation"
      [
        F.dir "src"
          [ F.File (".semgrepignore", "!tests/\n"); F.dir "tests" [ F.file "t.js" ] ];
        F.file "ok.js";
      ];
    (* Select 'ok.js', not 'src/tests/t.js'. *)
    test_find_targets ~with_git
      "built-in patterns take precedence over a nested semgrepignore negation \
       without a trailing slash"
      [
        F.dir "src"
          [ F.File (".semgrepignore", "!tests\n"); F.dir "tests" [ F.file "t.js" ] ];
        F.file "ok.js";
      ];
    (* Select 'build/app/a.js': the built-in patterns apply from the folder
       given as a target downwards, not to the folders above it. *)
    test_find_targets ~with_git ~scanning_root:"build/app"
      "built-in patterns do not apply to the folders above a folder target"
      [ F.dir "build" [ F.dir "app" [ F.file "a.js" ]; F.file "b.js" ] ];
    (* Select 'a.js', run from 'build/app'. *)
    test_find_targets ~with_git ~cwd:"build/app" ~scanning_root:"."
      "built-in patterns do not apply to the folders above the current folder \
       as a target"
      [ F.dir "build" [ F.dir "app" [ F.file "a.js" ]; F.file "b.js" ] ];
    (* Select 'src/s.js', not 'src/build/b.js'. *)
    test_find_targets ~with_git ~scanning_root:"src"
      "built-in patterns apply below a folder target"
      [ F.dir "src" [ F.dir "build" [ F.file "b.js" ]; F.file "s.js" ] ];
    (* Select 'ok.js', not 'app/y.js': 'app' is reported once. *)
    test_find_targets ~with_git ~excludes:[ "app/" ]
      "exclude directory pattern takes precedence over a semgrepignore negation"
      [ F.File (".semgrepignore", "!app\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'keep.js', not 'drop.js': the semgrepignore of the scanning
       root applies to the paths under it, not to the root itself. *)
    test_find_targets ~with_git
      "semgrepignore of the scanning root matching everything under it"
      [
        F.File (".semgrepignore", "**\n!keep.js\n");
        F.file "keep.js";
        F.file "drop.js";
      ];
    (* Select 'sub/keep.js', not 'sub/drop.js'. *)
    test_find_targets ~with_git ~scanning_root:"sub"
      "semgrepignore of a folder scanning root matching everything under it"
      [
        F.dir "sub"
          [
            F.File (".semgrepignore", "**\n!keep.js\n");
            F.file "keep.js";
            F.file "drop.js";
          ];
      ];
    (* Select 'ok.js', not 'tests/a.js' or 'src/tests/b.js': the built-in
       patterns come before the command-line patterns. *)
    test_find_targets ~with_git ~excludes:[ "!tests" ]
      "built-in patterns take precedence over an exclude negation"
      [
        F.dir "tests" [ F.file "a.js" ];
        F.dir "src" [ F.dir "tests" [ F.file "b.js" ] ];
        F.file "ok.js";
      ];
    (* Select 'ok.js', not 'app/y.js': 'app' is reported as excluded by the
       command line, which comes before the semgrepignore files. *)
    test_find_targets ~with_git ~excludes:[ "app/" ]
      "exclude directory pattern reported before a semgrepignore pattern"
      [ F.File (".semgrepignore", "app\n"); F.dir "app" [ F.file "y.js" ]; F.file "ok.js" ];
    (* Select 'ok.txt', not '.hidden.js' or 'sub/.h2.js'. *)
    test_find_targets ~with_git
      "semgrepignore wildcard matches a leading period"
      [
        F.File (".semgrepignore", "*.js\n");
        F.file ".hidden.js";
        F.dir "sub" [ F.file ".h2.js" ];
        F.file "ok.txt";
      ];
    (* Select '.hidden.js' and 'a.js', not 'ok.txt'. *)
    test_find_targets ~with_git ~includes:[ "*.js" ]
      "include wildcard matches a leading period"
      [ F.file ".hidden.js"; F.file "a.js"; F.file "ok.txt" ];
    (* Select 'a.js', not 'ok.txt': the include pattern that is not valid
       selects nothing. *)
    test_find_targets ~with_git ~includes:[ "[abc"; "*.js" ]
      "include pattern that is not valid selects nothing"
      [ F.file "a.js"; F.file "ok.txt" ];
  ]

(*
   '.gitignore' files apply only in git projects, through the files that
   git lists.
*)
let tests_with_git_only =
  let with_git = true in
  [
    (* Select 'a' and 'b': 'b' is committed although '.gitignore' lists it,
       and git lists the files it tracks. *)
    test_find_targets ~with_git ~honor_gitignore:false
      "a committed file that gitignore lists is selected"
      [ F.File (".gitignore", "b\n"); F.file "a"; F.file "b" ];
    (* Select nothing: 'build/app' is in 'build', which the semgrepignore
       file of the project ignores. Outside any VCS the folder target is
       its own project and the file is not read. *)
    test_find_targets ~with_git ~scanning_root:"build/app"
      "semgrepignore applies to the folders above a folder target"
      [
        F.File (".semgrepignore", "build/\n");
        F.dir "build" [ F.dir "app" [ F.file "a.js" ] ];
      ];
    (* Select 'a' and 'c', not 'b'. *)
    test_find_targets ~with_git "untracked files that git ignores are not listed"
      ~non_git_files:[ F.file "a"; F.file "b" ]
      [ F.File (".gitignore", "b\n"); F.file "c" ];
    (* Can't select file 'a' via --include when gitignoring its folder. *)
    test_find_targets ~with_git ~includes:[ "a" ]
      "--include does not select a file that git leaves out"
      [
        F.File (".gitignore", "dir\n");
        F.dir "dir" [ F.file "a"; F.file "b" ];
        F.file "c";
      ];
    test_find_targets ~with_git "symlinks from git are filtered too"
      [ F.Symlink ("lnk", "missing"); F.File ("a", "some content") ];
    (* The ignored folder is above the scanning root, which is reported
       itself. *)
    test_find_targets ~with_git ~scanning_root:"dir/sub"
      "scanning root under a semgrepignored folder"
      [
        F.File (".semgrepignore", "dir/\n");
        F.dir "dir" [ F.dir "sub" [ F.file "a.c" ] ];
      ];
    (* Select 'frontend/src/app/x.js'. Outside a VCS the scanning root is its
       own project and the semgrepignore above it is not read. *)
    test_find_targets ~with_git ~scanning_root:"frontend/src/app"
      "semgrepignore negation re-includes the scanning root"
      [
        F.File (".semgrepignore", "app/\n!frontend/src/app\n");
        F.dir "app" [ F.file "y.js" ];
        F.dir "frontend" [ F.dir "src" [ F.dir "app" [ F.file "x.js" ] ] ];
      ];
  ]

let tests_without_git_only =
  let with_git = false in
  [
    (* '.gitignore' is not read: select 'a' and 'b'. *)
    test_find_targets ~with_git "gitignore is not read"
      [ F.File (".gitignore", "b\n"); F.file "a"; F.file "b" ];
  ]

let tests =
  Testo.categorize "Find_targets"
    (tests_with_or_without_git ~with_git:true
    @ tests_with_git_only
    @ tests_with_or_without_git ~with_git:false
    @ tests_without_git_only)
