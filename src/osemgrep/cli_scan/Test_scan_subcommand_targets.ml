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

(* A directory of the fixtures, copied into the repo under the same name. *)
let target_dir (name : string) : F.t = F.dir name (F.read (targets_root / name))

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

(* Byte counts that --max-target-bytes is tried with over targets/basic:
   1MB keeps every file, 100B and 1B skip the bigger ones.
   python: test_max_target_bytes_results, test_max_target_bytes_output,
   test_max_target_bytes_output_pysemfail *)
let max_target_bytes : string list = [ "1MB"; "100B"; "1B" ]

(* Files whose extension the targeting excludes by default, '.min.js' and
   '.d.ts'. The '.semgrepignore' at the root replaces the built-in
   patterns, whose '*.min.js' would hide the file before a '--include' is
   applied. *)
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
   tree with the same directory name at two depths. *)
let nested_path_options : string list list =
  [
    [ "--include"; "lib/b.js" ];
    [ "--include"; "src/*" ];
    [ "--exclude"; "src/*" ];
  ]

(* A file and a symlink to it, so that a scanning root can be spelled
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
         the project: the paths are taken as typed *)
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
      (* A file the user names is scanned whatever its extension: the
         default exclusions are for what walking a directory turns up.
         The wrapper had no such exclusions at all. *)
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
      (* found by walking the directory, the '.min.js' is skipped and the
         '.d.ts' is not a target of the rule *)
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
      (* A scanning root spelled with a leading './' is reported without
         it, as the wrapper's Path did. *)
      t "scanning root spelled with a leading dot"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "nested_paths" ] ]
           ~extra_args:[ "./targets/nested_paths/src" ]);
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
                  F.dir ".semgrepignore" [ F.File ("inside", "") ];
                  F.dir "targets" [ target_dir "nested_paths" ];
                ]
              ~extra_args:[ "targets/nested_paths/src" ]));
      t "ignore file without read permission" ?skipped:unless_root
        ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (with_one_ignore_file_warning ~reason:"Permission denied"
           (run_scan caps ~root ~git:false ~format_args:[ "--json" ]
              ~rule:"rules/eqeq-basic.yaml" ~targets:[]
              ~extra_files:
                [
                  F.Unreadable (".semgrepignore", "src/\n");
                  F.dir "targets" [ target_dir "nested_paths" ];
                ]
              ~extra_args:[ "targets/nested_paths/src" ]));
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
    ])
