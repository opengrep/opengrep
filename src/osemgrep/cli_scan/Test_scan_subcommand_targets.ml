(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Fpath_.Operators
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of which files a scan takes and what it reports about
 * the others: the selection by extension, shebang, language and per-rule
 * paths, the ignore files, the scanning roots, and the "Some files were
 * skipped" block of the summary, the verbose listing and the "paths" of
 * the JSON output.
 *
 * The fixtures are under tests/targeting.
 *)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

let root : Fpath.t = Fpath.v "tests/targeting"
let targets_root : Fpath.t = root / "targets"

(* A directory of the fixtures, copied into the repo under the same name,
   with extra entries added next to what was copied. *)
let target_dir_with (name : string) (extra : F.t list) : F.t =
  F.dir name (F.read (targets_root / name) @ extra)

(* A directory of the fixtures, copied into the repo under the same name. *)
let target_dir (name : string) : F.t = target_dir_with name []

(* The same, for a directory of the general scan fixtures. *)
let scan_target_dir (name : string) : F.t =
  F.dir name (F.read (fixtures_root / "targets" / name))

(* A tree with something for most reasons to skip a file, copied into the
   repo under targets/ignores, with its ignore files also at the root as
   the scan reads them from there. *)
let ignores_files : F.t list =
  let read (name : string) : string =
    UFile.read_file (targets_root / "ignores" / name)
  in
  [
    F.File (".semgrepignore", read ".semgrepignore");
    F.File (".gitignore", read ".gitignore");
    F.dir "targets" [ target_dir "ignores" ];
  ]

(* The options are chosen to trigger one entry for most reasons.
   python: test_semgrepignore_ignore_log_report *)
let ignores_args : string list =
  [
    "--include=ignore.*";
    "--include=tests";
    "--include=find.*";
    "--exclude=*.min.js";
    "--max-target-bytes=100";
    "--verbose";
    "targets/ignores";
  ]

(* Directories of files in several languages, scanned with rules/filecount.yaml
   whose rules are for python, js, regex and generic: the status and the
   summary count the files and the rules that had one.
   python: test_file_count_multifile *)
let filecount_dirs : string list =
  [ "multilangproj"; "language-filtering"; "exclude_include" ]

(* The option sets that --exclude and --include are tried with, over a tree
   whose directories and files are named "excluded" and "included": the scan
   only lists the files it would take.
   python: test_exclude_include *)
let exclude_include_options : string list list =
  [
    [ "--exclude"; "excluded.*" ];
    [ "--include"; "included.*" ];
    [ "--exclude"; "excluded" ];
    [ "--include"; "included" ];
    [ "--include"; "included"; "--exclude"; "excluded.*" ];
    [ "--exclude"; "excluded"; "--include"; "included.*" ];
    [ "--exclude"; "excluded.*"; "--exclude"; "included.*" ];
    [ "--exclude"; "excluded"; "--exclude"; "included" ];
    [ "--include"; "excluded.*"; "--include"; "included.*" ];
    [ "--include"; "excluded"; "--include"; "included" ];
    [ "--include"; "included.vue" ];
    [ "--include"; "included.vue"; "--skip-unknown-extensions" ];
    [ "--exclude"; "*.*" ];
    [ "--include"; "*.*" ];
  ]

(* A file and a directory the scan cannot read, next to one it can. Only
   the README of these is a fixture; the Python test creates the rest when
   it runs, since git does not keep permissions of this kind.
   python: prepare_workspace of test_permissions.py *)
let permissions_files : F.t list =
  [
    F.dir "targets"
      [
        F.dir "permissions"
          (F.read (targets_root / "permissions")
          @ [
              F.File ("readable_file.py", "a == a\n");
              F.Unreadable ("unreadable_file.py", "secret content\n");
              F.Unreadable_dir
                ("unreadable_subdir", [ F.File ("file.py", "b == b\n") ]);
            ]);
      ];
  ]

(* A file the scan cannot read, next to one it can, left out by the
   '.semgrepignore'. Deciding whether a skipped '.php' file is a target of
   the rule's language opens it to look for a shebang, and opening this one
   fails. *)
let unreadable_ignored_files : F.t list =
  [
    F.dir "targets"
      [
        F.dir "unreadable_ignored"
          [
            (* the scanning root is the project outside a VCS, so this is
               where the ignore file is read from *)
            F.File (".semgrepignore", "hidden.php\n");
            F.File ("main.php", "<?php $x == $x;\n");
            F.Unreadable ("hidden.php", "<?php $y == $y;\n");
          ];
      ];
  ]

(* Byte counts that --max-target-bytes is tried with over targets/basic:
   1MB keeps every file, 100B and 1B skip the bigger ones.
   python: test_max_target_bytes_results, test_max_target_bytes_output,
   test_max_target_bytes_output_pysemfail *)
let max_target_bytes : string list = [ "1MB"; "100B"; "1B" ]

(* A '.min.js', which targeting excludes by extension, and a '.d.ts', which
   is a TypeScript target like any other. The '.semgrepignore' at the root
   replaces the built-in patterns, whose '*.min.js' would hide the first
   file before a '--include' is applied. *)
let excluded_extension_files : F.t list =
  [
    F.File (".semgrepignore", "# no patterns\n");
    F.dir "targets" [ target_dir "excluded_extensions" ];
  ]

(* A file over '--max-target-bytes' next to a small one. The file is built
   here rather than kept as a fixture so that the limit can be small. *)
let size_files : F.t list =
  [
    F.dir "targets"
      [
        F.dir "size"
          [
            F.File ("big.py", "x == x\n" ^ String.make 200 '#' ^ "\n");
            F.File ("small.py", "y == y\n");
          ];
      ];
  ]

(* The '--include' and '--exclude' patterns that contain a slash, over a
   tree with the same directory name at two depths. A slash inside a
   pattern anchors it at the project root, where a leading '**/' matches
   at any depth. *)
let nested_path_options : string list list =
  [
    [ "--include"; "lib/b.js" ];
    [ "--include"; "**/lib/b.js" ];
    [ "--include"; "src/*" ];
    [ "--include"; "**/src/*" ];
    [ "--exclude"; "src/*" ];
    [ "--exclude"; "**/src/*" ];
  ]

(* A file and a symlink to it, so that a scanning root can be given
   through the symlink. *)
let symlink_to_file_files : F.t list =
  [ F.File ("real.py", "x == x\n"); F.Symlink ("link.py", "real.py") ]

(* The general scan fixtures, symlinked into the repo as the Python
   harness does with its targets; absolute, as the harness runs from the
   project root. *)
let symlinked_targets : F.t list =
  [ F.Symlink ("targets", !!(Fpath.v (Sys.getcwd ()) // fixtures_root / "targets")) ]

(* An ignore file the scan cannot read is reported on stderr, which the
   snapshot of a test checking stdout does not hold, and one such file is
   reported once however many readers meet it. *)
let with_one_ignore_file_warning ~(reason : string) (f : unit -> unit) :
    unit -> unit =
 fun () ->
  let (), (output : string) = Testo.with_capture stderr f in
  let warnings : string list =
    String.split_on_char '\n' output
    |> List.filter (String_.contains ~term:"Ignoring the file ")
  in
  Alcotest.(check int) "one warning about the ignore file" 1
    (List.length warnings);
  Alcotest.(check bool) "the warning says why" true
    (List.exists (String_.contains ~term:reason) warnings)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Rules whose 'paths:' tell apart the path they are matched against. The
   pattern does not match its own text, so the rule file has no finding. *)
let rule_paths_rules : string =
  let rule (id : string) (paths : string) : string =
    Printf.sprintf
      "  - id: %s\n\
      \    languages: [regex]\n\
      \    severity: INFO\n\
      \    message: m\n\
      \    pattern-regex: MA[R]K\n\
      \    paths:\n\
      \      %s\n"
      id paths
  in
  "rules:\n"
  ^ rule "include-star-a" "include: ['*/a.py']"
  ^ rule "exclude-sub" "exclude: [sub]"
  ^ rule "include-star-proj-a" "include: ['*/proj/a.py']"
  ^ rule "include-star-hidden" "include: ['*hidden.py']"

let rule_paths_files : F.t list =
  [
    F.File ("rules.yml", rule_paths_rules);
    F.File ("a.py", "MARK\n");
    F.File (".hidden.py", "MARK\n");
    F.Dir ("sub", [ F.File ("a.py", "MARK\n") ]);
    F.Dir (".dotdir", [ F.Dir ("proj", [ F.File ("a.py", "MARK\n") ]) ]);
  ]

(* In a git repository of [rule_paths_files], checks the files that the
   rule [rule] reports in a scan of [args], run in the folder [dir]. [args]
   and [expected] get the absolute path of the repository. *)
let check_rule_paths (caps : Scan_subcommand.caps) (msg : string)
    ~(expected : string -> string list) ~(rule : string) ~(dir : string)
    (args : string -> string list) : unit -> unit =
 fun () ->
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true rule_paths_files
        (fun (repo : Fpath.t) ->
          let repo = Fpath.to_string repo in
          Testutil_files.with_chdir (Fpath.v dir) (fun () ->
              Alcotest.(check (list string))
                msg (expected repo)
                (json_matches caps
                   ([ "--config"; Filename.concat repo "rules.yml" ]
                   @ args repo)
                |> List_.filter_map
                     (fun (m : Semgrep_output_v1_t.cli_match) ->
                       match Rule_ID.last_elt_opt m.check_id with
                       | Some (id : string) when String.equal id rule ->
                           Some (Fpath.to_string m.path)
                       | Some (_ : string)
                       | None ->
                           None)
                |> List.sort String.compare))))

(* In a git repository, 'opengrep scan -e MARK -l generic' with the
   arguments [args] stops before it starts, with the message [expected]. *)
let check_scan_aborts (caps : Scan_subcommand.caps) ~(args : string list)
    ~(expected : string) : unit -> unit =
 fun () ->
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true [ F.File ("a.js", "MARK\n") ]
        (fun (_ : Fpath.t) ->
          match
            without_settings (fun () ->
                Scan_subcommand.main caps
                  (Array.of_list
                     ([ "opengrep-scan"; "-e"; "MARK"; "-l"; "generic" ] @ args)))
          with
          | (_ : Exit_code.t) -> Alcotest.fail "expected the scan to abort"
          | exception Error.Semgrep_error ((msg : string), _) ->
              Alcotest.(check string) "the message" expected msg))

(* In a git repository of [files], the paths of the findings of
   'opengrep scan -e MARK -l generic' with the arguments [args], which get
   the absolute path of the repository, sorted. *)
let check_scanned_in_git_repo (caps : Scan_subcommand.caps) (msg : string)
    ~(files : F.t list) ~(expected : string -> string list)
    (args : string -> string list) : unit -> unit =
 fun () ->
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true files (fun (repo : Fpath.t) ->
          let repo = Fpath.to_string repo in
          Alcotest.(check (list string))
            msg (expected repo)
            (json_matches caps ([ "-e"; "MARK"; "-l"; "generic" ] @ args repo)
            |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) ->
                   Fpath.to_string m.path)
            |> List.sort String.compare)))

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan targets (e2e)"
    ((filecount_dirs
     |> List.map (fun (dir : string) ->
            t (Printf.sprintf "file count: %s" dir)
              ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
              (run_scan caps ~root ~format_args:[] ~rule:"rules/filecount.yaml"
                 ~targets:[] ~extra_files:[ target_dir dir ] ~extra_args:[ dir ])))
    @ (exclude_include_options
      |> List.map (fun (options : string list) ->
             t
               (Printf.sprintf "exclude and include: %s"
                  (String.concat " " options))
               ~checked_output:(Testo.stdout ()) ~normalize:normalise
               (run_scan caps ~root ~format_args:[] ~rule:"rules/eqeq-basic.yaml"
                  ~targets:[]
                  ~extra_files:[ F.dir "targets" [ target_dir "exclude_include" ] ]
                  ~extra_args:
                    (("--x-ls" :: options) @ [ "targets/exclude_include" ]))))
    (* differs from the Python wrapper: the status counts the files kept,
       where it counts the files found *)
    @ (nested_path_options
      |> List.map (fun (options : string list) ->
             t
               (Printf.sprintf "nested paths: %s" (String.concat " " options))
               ~checked_output:(Testo.stdout ()) ~normalize:normalise
               (run_scan caps ~root ~format_args:[] ~rule:"rules/eqeq-basic.yaml"
                  ~targets:[]
                  ~extra_files:[ F.dir "targets" [ target_dir "nested_paths" ] ]
                  ~extra_args:
                    (("--x-ls-long" :: options) @ [ "targets/nested_paths" ]))))
    @ (max_target_bytes
      |> List.concat_map (fun (bytes : string) ->
             let scan (format_args : string list) : unit -> unit =
               run_scan caps ~root:fixtures_root ~format_args
                 ~rule:"rules/eqeq.yaml" ~targets:[]
                 ~extra_files:[ F.dir "targets" [ scan_target_dir "basic" ] ]
                 ~extra_args:[ "--max-target-bytes"; bytes; "targets/basic" ]
             in
             [
               t
                 (Printf.sprintf "max target bytes: %s, JSON" bytes)
                 ~checked_output:(Testo.stdout ()) ~normalize:normalise
                 (scan [ "--json" ]);
               t
                 (Printf.sprintf "max target bytes: %s, text" bytes)
                 ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
                 (scan []);
             ]))
    @ [
      (* "R" is not a unit the byte count converter knows. The command line
         is read before the scan, so it raises rather than returning an exit
         code, and cmdliner writes the message, which it styles from the
         TERM of the process, so only the exit code is checked.
         python: test_max_target_bytes_results and
         test_max_target_bytes_output, parameter 1.3R *)
      t "max target bytes: unparseable count" (fun () ->
          try
            run_scan caps ~root:fixtures_root ~format_args:[ "--json" ]
              ~rule:"rules/eqeq.yaml" ~targets:[]
              ~extra_files:[ F.dir "targets" [ scan_target_dir "basic" ] ]
              ~extra_args:[ "--max-target-bytes"; "1.3R"; "targets/basic" ] ();
            failwith "expected the command line to be rejected"
          with
          | Error.Exit_code (code : Exit_code.t) -> Exit_code.Check.fatal code);
      (* A target folder that a semgrepignore file skips is reported with
         the pattern that skips it; the scan of the other target goes on. *)
      t "a target folder skipped by a semgrepignore file"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[] ~rule:"rules/eqeq-basic.yaml"
           ~targets:[]
           ~extra_files:
             [
               F.File (".semgrepignore", "build/\n");
               F.dir "build" [ F.dir "app" [ F.File ("a.js", "1 == 1\n") ] ];
               F.dir "src" [ F.File ("s.js", "2 == 2\n") ];
             ]
           ~extra_args:[ "build/app"; "src" ]);
      (* outside a git repository: no line about git, and no block when
         nothing was skipped *)
      t "summary: no git repository, nothing skipped"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false ~format_args:[]
           ~rule:"rules/eqeq.yaml" ~targets:[ "targets/basic/stupid.py" ]);
      t "summary: no git repository, a skipped file"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false ~format_args:[]
           ~rule:"rules/eqeq.yaml" ~targets:[ "targets/basic/stupid.py" ]
           ~extra_args:[ "--exclude=stupid.py" ]);
      (* --project-root with a scanning root under a symlink that leaves
         the project: the paths are taken as given *)
      t "forced project root with symlinked targets"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq.yaml" ~targets:[] ~extra_files:symlinked_targets
           ~extra_args:[ "--project-root"; "."; "targets/basic" ]);
      (* Every file is excluded: the verbose listing of the skipped files.
         python: test_exclude_include_verbose_sorted_1 *)
      (* differs from the Python wrapper: "Scanning 0 files" and "Ran 0
         rules" where it says "Scanning 5 files" and "Ran 4 rules" *)
      t "verbose listing: everything excluded by name"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~format_args:[]
           ~rule:"rules/eqeq.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "exclude_include" ] ]
           ~extra_args:
             [
               "--exclude"; "excluded.*"; "--exclude"; "included.*"; "--verbose";
               "targets/exclude_include";
             ]);
      (* The same over files of several languages, excluded by extension.
         python: test_exclude_include_verbose_sorted_2 *)
      t "verbose listing: everything excluded by extension"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~format_args:[]
           ~rule:"rules/nosem.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ scan_target_dir "basic" ] ]
           ~extra_args:[ "--exclude"; "*.*"; "--verbose"; "targets/basic" ]);
      (* The scan lists what it would take, leaving out what it cannot read.
         There is no git repository, as the Python harness copies the files
         instead of committing them.
         python: test_permissions_ls *)
      t "permissions: the file list" ?skipped:unless_root
        ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false ~format_args:[]
           ~rule:"rules/eqeq.yaml" ~targets:[] ~extra_files:permissions_files
           ~extra_args:[ "--x-ls"; "targets/permissions" ]);
      (* python: test_permissions_scan_full_strict *)
      t "permissions: JSON with --verbose" ?skipped:unless_root
        ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false ~format_args:[ "--json" ]
           ~rule:"rules/eqeq.yaml" ~targets:[] ~extra_files:permissions_files
           ~extra_args:[ "--verbose"; "targets/permissions" ]);
      (* the same scan in text mode: the unreadable file and directory are
         listed in the verbose block and counted in the summary *)
      t "permissions: text with --verbose" ?skipped:unless_root
        ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false ~format_args:[]
           ~rule:"rules/eqeq.yaml" ~targets:[] ~extra_files:permissions_files
           ~extra_args:[ "--verbose"; "targets/permissions" ]);
      (* python: test_permissions_scan_full_lax *)
      t "permissions: JSON" ?skipped:unless_root
        ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false ~format_args:[ "--json" ]
           ~rule:"rules/eqeq.yaml" ~targets:[] ~extra_files:permissions_files
           ~extra_args:[ "targets/permissions" ]);
      (* A file the '.semgrepignore' leaves out is reported as skipped
         whether or not the scan can read it: whether it is reported at all
         is decided from its name, and reading it is allowed to fail. *)
      t "permissions: unreadable file skipped by .semgrepignore"
        ?skipped:unless_root ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~git:false ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-php.yaml" ~targets:[]
           ~extra_files:unreadable_ignored_files
           ~extra_args:[ "--verbose"; "targets/unreadable_ignored" ]);
      (* A file the user names is scanned whatever its extension: the
         '.min.js' exclusion is for what walking a directory turns up. *)
      t "excluded extensions: named on the command line"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-js-ts.yaml" ~targets:[]
           ~extra_files:excluded_extension_files
           ~extra_args:
             [
               "--verbose"; "targets/excluded_extensions/min.min.js";
               "targets/excluded_extensions/t.d.ts";
             ]);
      (* ... and so is one a '--include' pattern of the user selects *)
      t "excluded extensions: matched by --include"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-js-ts.yaml" ~targets:[]
           ~extra_files:excluded_extension_files
           ~extra_args:
             [
               "--verbose"; "--include=*.min.js"; "--include=*.d.ts";
               "targets/excluded_extensions";
             ]);
      (* found by walking the directory, the '.min.js' is always skipped
         and the '.d.ts' is scanned as TypeScript *)
      t "excluded extensions: found by walking"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-js-ts.yaml" ~targets:[]
           ~extra_files:excluded_extension_files
           ~extra_args:[ "--verbose"; "targets/excluded_extensions" ]);
      (* A file the user names is scanned whatever its size, as the Python
         wrapper did; the skip reason of the one found by walking is the
         one the wrapper reported. *)
      t "max target bytes: file named on the command line"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[] ~extra_files:size_files
           ~extra_args:
             [
               "--verbose"; "--max-target-bytes"; "100"; "targets/size/big.py";
             ]);
      t "max target bytes: file found by walking"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[] ~extra_files:size_files
           ~extra_args:
             [ "--verbose"; "--max-target-bytes"; "100"; "targets/size" ]);
      (* '--force-exclude' puts the filters back on the file the user
         named, the size limit included, as it did for the wrapper. *)
      t "max target bytes: file named with --force-exclude"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[] ~extra_files:size_files
           ~extra_args:
             [
               "--verbose"; "--force-exclude"; "--max-target-bytes"; "100";
               "targets/size/big.py";
             ]);
      t "force exclude: a readable file named on the command line"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[] ~extra_files:size_files
           ~extra_args:
             [ "--verbose"; "--force-exclude"; "targets/size/small.py" ]);
      (* A scanning root that is a symlink to a file is followed and
         scanned under '--force-exclude' as it is without the flag; only
         the exclusions applied to it can leave it out. *)
      t "force exclude: a symlink to a file named on the command line"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[]
           ~extra_files:symlink_to_file_files
           ~extra_args:[ "--verbose"; "--force-exclude"; "link.py" ]);
      t "force exclude: a symlink to a file the exclusions leave out"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[]
           ~extra_files:symlink_to_file_files
           ~extra_args:
             [
               "--verbose"; "--force-exclude"; "--exclude"; "link.py";
               "link.py";
             ]);
      (* A scanning root written with a leading './' is reported without
         it, as the wrapper's Path did. *)
      t "scanning root written with a leading dot"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "nested_paths" ] ]
           ~extra_args:[ "./targets/nested_paths/src" ]);
      (* A '.' segment further down the scanning root is dropped as well,
         so the findings are reported under the path without it. *)
      t "scanning root written with an interior dot"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "nested_paths" ] ]
           ~extra_args:[ "targets/nested_paths/./src" ]);
      (* An unreadable target named on the command line is reported as
         skipped, kept out of the scanned files, and makes the run fail as
         it did for the wrapper. *)
      t "permissions: unreadable file named on the command line"
        ?skipped:unless_root ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false
           ~format_args:[ "--json" ] ~rule:"rules/eqeq.yaml" ~targets:[]
           ~extra_files:permissions_files ~check:Exit_code.Check.fatal
           ~extra_args:
             [ "--verbose"; "targets/permissions/unreadable_file.py" ]);
      t "permissions: unreadable directory named on the command line"
        ?skipped:unless_root ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root:fixtures_root ~git:false
           ~format_args:[ "--json" ] ~rule:"rules/eqeq.yaml" ~targets:[]
           ~extra_files:permissions_files ~check:Exit_code.Check.fatal
           ~extra_args:
             [ "--verbose"; "targets/permissions/unreadable_subdir" ]);
      (* An ignore file that cannot be read as a file holds no patterns:
         the scan warns and goes on, where it used to abort. The snapshot
         holds the scan that went on; the warning is on stderr. *)
      t "ignore file that is a directory" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (with_one_ignore_file_warning ~reason:"not a regular file"
           (run_scan caps ~root ~git:false ~format_args:[ "--json" ]
              ~rule:"rules/eqeq-basic.yaml" ~targets:[]
              ~extra_files:
                [
                  F.dir "targets"
                    [
                      target_dir_with "nested_paths"
                        [ F.dir ".semgrepignore" [ F.File ("inside", "") ] ];
                    ];
                ]
              ~extra_args:[ "targets/nested_paths" ]));
      t "ignore file without read permission" ?skipped:unless_root
        ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (with_one_ignore_file_warning ~reason:"Permission denied"
           (run_scan caps ~root ~git:false ~format_args:[ "--json" ]
              ~rule:"rules/eqeq-basic.yaml" ~targets:[]
              ~extra_files:
                [
                  F.dir "targets"
                    [
                      target_dir_with "nested_paths"
                        [ F.Unreadable (".semgrepignore", "src/\n") ];
                    ];
                ]
              ~extra_args:[ "targets/nested_paths" ]));
      (* the same for the file of an ':include' line *)
      t "included ignore file without read permission" ?skipped:unless_root
        (with_one_ignore_file_warning ~reason:"Permission denied"
           (run_scan caps ~root ~git:false ~format_args:[ "--json" ]
              ~rule:"rules/eqeq-basic.yaml" ~targets:[]
              ~extra_files:
                [
                  F.dir "targets"
                    [
                      target_dir_with "nested_paths"
                        [
                          F.File (".semgrepignore", ":include extra.ignore\n");
                          F.Unreadable ("extra.ignore", "src/\n");
                        ];
                    ];
                ]
              ~extra_args:[ "targets/nested_paths" ]));
      (* the line 'src/' in UTF-16, with its byte order mark *)
      t "ignore file in UTF-16" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (with_one_ignore_file_warning ~reason:"encoded in UTF-16"
           (run_scan caps ~root ~git:false ~format_args:[ "--json" ]
              ~rule:"rules/eqeq-basic.yaml" ~targets:[]
              ~extra_files:
                [
                  F.dir "targets"
                    [
                      target_dir_with "nested_paths"
                        [
                          F.File
                            (".semgrepignore", "\xFF\xFEs\000r\000c\000/\000\n\000");
                        ];
                    ];
                ]
              ~extra_args:[ "targets/nested_paths" ]));
      (* The folder git keeps its data in is never reported, as the
         wrapper's PATHS_ALWAYS_SKIPPED was not. *)
      t "the git folder is not reported" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "nested_paths" ] ]
           ~extra_args:[ "--no-git-ignore"; "--verbose"; "." ]);
      (* python: test_semgrepignore_ignore_log_report *)
      t "ignore log report: text" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[] ~rule:"rules/eqeq-basic.yaml"
           ~targets:[] ~extra_files:ignores_files ~extra_args:ignores_args);
      (* python: test_semgrepignore_ignore_log_json_report *)
      t "ignore log report: JSON" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[] ~extra_files:ignores_files
           ~extra_args:ignores_args);
      (* The ignore files of the repo, without other options.
         python: test_semgrepignore *)
      t "ignore files: JSON" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[] ~extra_files:ignores_files
           ~extra_args:[ "targets/ignores" ]);
      (* An ignore file under another name, given with the flag, replaces
         the .semgrepignore of the repo: its 'ok/' is skipped, and what the
         .semgrepignore ignores is scanned.
         python: test_internal_explicit_semgrepignore, through an
         environment variable of the wrapper *)
      t "ignore file given by name" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[]
           ~extra_files:
             (ignores_files @ [ F.File (".semgrepignore_explicit", "ok/\n") ])
           ~extra_args:
             [
               "--semgrepignore-filename"; ".semgrepignore_explicit";
               "targets/ignores";
             ]);
      (* Without a .semgrepignore, the default patterns still apply: only
         find.js is scanned. python: test_default_semgrepignore *)
      t "default semgrepignore patterns" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "ignores_default" ] ]
           ~extra_args:[ "targets/ignores_default" ]);
      (* A file without extension is scanned when named on the command line
         with --scan-unknown-extensions and a language given with -e.
         python: test_noextension_with_explicit_lang *)
      t "no extension: explicit target with -e and --lang"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ] ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "basic" ] ]
           ~extra_args:
             [
               "--scan-unknown-extensions"; "--lang"; "python"; "-e"; "hello";
               "targets/basic/simple_python_no_extension";
             ]);
      (* ... and with a rule file. python: test_noextension_filtering *)
      t "no extension: explicit target with a rule"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "basic" ] ]
           ~extra_args:
             [ "--scan-unknown-extensions"; "targets/basic/stupid_no_extension" ]);
      (* python: test_noextension_filtering_optimizations *)
      t "no extension: explicit target with a rule and --optimizations all"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "basic" ] ]
           ~extra_args:
             [
               "--scan-unknown-extensions"; "--optimizations"; "all";
               "targets/basic/stupid_no_extension";
             ]);
      (* A script without extension is scanned when its shebang names the
         language. python: test_script *)
      t "shebang script" ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-python.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "script" ] ]
           ~extra_args:[ "targets/script" ]);
      (* A rule for one language runs on the files of that language only.
         python: test_language_filtering *)
      t "language filtering" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/language-filtering.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "language-filtering" ] ]
           ~extra_args:[ "targets/language-filtering" ]);
      (* The target is what the standard input holds, given as '-'.
         python: test_stdin_input *)
      t "target from stdin" ~checked_output:(Testo.stdout ())
        ~normalize:(normalise @ mask_temp_targets)
        (fun () ->
          with_stdin_from ~data:"a\n" (fun () ->
              run_scan caps ~git:false ~format_args:[ "--json" ] ~targets:[]
                ~extra_args:[ "-e"; "a"; "--lang"; "js"; "-" ] ()));
      (* The target is a named pipe, as a process substitution gives.
         python: test_subshell_input *)
      t "target from a named pipe" ~checked_output:(Testo.stdout ())
        ~normalize:(normalise @ mask_temp_targets)
        (fun () ->
          with_read_from_named_pipe ~data:"a\n" (fun pipe ->
              run_scan caps ~git:false ~format_args:[ "--json" ] ~targets:[]
                ~extra_args:[ "-e"; "a"; "--lang"; "js"; !!pipe ] ()));
      (* Both pipes are scanned. Their temporary names are random and the
         results are sorted by path, so the matched lines are checked as a
         sorted list rather than as a snapshot.
         python: test_multi_subshell_input *)
      t "targets from two named pipes" (fun () ->
          with_read_from_named_pipe ~data:"a\n" (fun pipe1 ->
              with_read_from_named_pipe ~data:"b + a\n" (fun pipe2 ->
                  let (), stdout_output =
                    Testo.with_capture stdout (fun () ->
                        run_scan caps ~git:false ~format_args:[ "--json" ]
                          ~targets:[]
                          ~extra_args:
                            [ "-e"; "a"; "--lang"; "js"; !!pipe1; !!pipe2 ]
                          ())
                  in
                  (* the JSON comes after the listing of the repo's files *)
                  let json_line =
                    String.split_on_char '\n' stdout_output
                    |> List.find (String.starts_with ~prefix:"{")
                  in
                  let out = Semgrep_output_v1_j.cli_output_of_string json_line in
                  Alcotest.(check int)
                    "both pipes scanned" 2
                    (List.length out.paths.scanned);
                  Alcotest.(check (list string))
                    "the matched lines" [ "a"; "b + a" ]
                    (out.results
                    |> List_.map (fun (m : Semgrep_output_v1_t.cli_match) ->
                           m.extra.lines)
                    |> List.sort String.compare))));
      (* Eight rules whose 'paths' include or exclude a file name, a
         directory or a path. python: test_paths *)
      t "per-rule paths: include and exclude" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ] ~rule:"rules/paths.yaml"
           ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "exclude_include" ] ]
           ~extra_args:[ "targets/exclude_include" ]);
      (* The 'paths' of a rule select its files. python: test_per_rule_include *)
      t "per-rule include" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/per-rule-include.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "per-rule-include" ] ]
           ~extra_args:[ "targets/per-rule-include" ]);
      (* A file found under a folder target is matched by its path relative
         to the project root, whatever form the folder target is given in;
         a file given as a target by its path as given. *)
      t "rule paths: a file found from a subfolder"
        (check_rule_paths caps "the files of 'sub' are excluded" ~rule:"exclude-sub"
           ~dir:"sub" ~expected:(fun _ -> []) (fun _ -> [ "." ]));
      t "rule paths: a file found from the project root"
        (check_rule_paths caps "the files outside 'sub' are not excluded"
           ~rule:"exclude-sub" ~dir:"."
           ~expected:(fun _ -> [ ".dotdir/proj/a.py"; ".hidden.py"; "a.py" ])
           (fun _ -> [ "." ]));
      t "rule paths: a file found under '..'"
        (check_rule_paths caps "'*' matches a folder of the project, not '..'"
           ~rule:"include-star-a" ~dir:"sub"
           ~expected:(fun _ -> [ "../.dotdir/proj/a.py"; "../sub/a.py" ])
           (fun _ -> [ ".." ]));
      t "rule paths: a file found under a period-prefixed folder target"
        (check_rule_paths caps "'*' matches the period-prefixed folder"
           ~rule:"include-star-proj-a" ~dir:".dotdir/proj"
           ~expected:(fun _ -> [ "a.py" ])
           (fun _ -> [ "." ]));
      t "rule paths: a file found under an absolute folder target"
        (check_rule_paths caps "'*' does not match the folders above the project"
           ~rule:"include-star-a" ~dir:"."
           ~expected:(fun (repo : string) ->
             [
               Filename.concat repo ".dotdir/proj/a.py";
               Filename.concat repo "sub/a.py";
             ])
           (fun (repo : string) -> [ repo ]));
      t "rule paths: a file given as '../a.py'"
        (check_rule_paths caps "'*' does not match '..'" ~rule:"include-star-a"
           ~dir:"sub" ~expected:(fun _ -> []) (fun _ -> [ "../a.py" ]));
      t "rule paths: a file given as '../sub/a.py'"
        (check_rule_paths caps "'*' matches the folder 'sub'"
           ~rule:"include-star-a" ~dir:"sub"
           ~expected:(fun _ -> [ "../sub/a.py" ])
           (fun _ -> [ "../sub/a.py" ]));
      (* An --include or --exclude pattern that is not valid stops the scan
         before it starts. *)
      t "an --include pattern that is not valid"
        (check_scan_aborts caps ~args:[ "--include"; "[abc"; "." ]
           ~expected:"--include '[abc' is not a valid pattern (missing ']')");
      t "a negated --exclude pattern that is not valid"
        (check_scan_aborts caps ~args:[ "--exclude"; "![abc"; "." ]
           ~expected:"--exclude '![abc' is not a valid pattern (missing ']')");
      t "--project-root with a baseline commit"
        (check_scan_aborts caps
           ~args:[ "--project-root"; "."; "--baseline-commit"; "HEAD"; "." ]
           ~expected:
             "--project-root cannot be used with a baseline commit \
              (--baseline-commit, OPENGREP_BASELINE_COMMIT, \
              OPENGREP_BASELINE_REF, SEMGREP_BASELINE_COMMIT or \
              SEMGREP_BASELINE_REF)");
      (* A file given as a target is filtered as a given file even when a
         folder target holds it: a symbolic link given as a target is
         followed. *)
      t "a symbolic link given as a target with the folder that holds it"
        (check_scanned_in_git_repo caps "the link and the other file"
           ~files:
             [
               F.dir "other" [ F.File ("real.js", "MARK\n") ];
               F.dir "dir"
                 [
                   F.File ("plain.js", "MARK\n");
                   F.Symlink ("link.js", "../other/real.js");
                 ];
             ]
           ~expected:(fun _ -> [ "dir/link.js"; "dir/plain.js" ])
           (fun _ -> [ "--force-exclude"; "dir"; "dir/link.js" ]));
      (* Each folder target filters the files git lists for it: the built-in
         'vendor/' of 'src' skips 'src/vendor', which the other target
         scans. *)
      t "a folder target and a folder below it given by its absolute path"
        (check_scanned_in_git_repo caps "src/vendor scanned once, by its target"
           ~files:
             [
               F.dir "src"
                 [ F.File ("d.js", "MARK\n"); F.dir "vendor" [ F.File ("e.js", "MARK\n") ] ];
             ]
           ~expected:(fun (repo : string) ->
             [ Filename.concat repo "src/vendor/e.js"; "src/d.js" ])
           (fun (repo : string) -> [ "src"; Filename.concat repo "src/vendor" ]));
      t "rule paths: a dot file"
        (check_rule_paths caps "'*' matches the leading period"
           ~rule:"include-star-hidden" ~dir:"."
           ~expected:(fun _ -> [ ".hidden.py" ])
           (fun _ -> [ "." ]));
    ])
