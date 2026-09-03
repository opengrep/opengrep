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

(* The SARIF fixtures directory, symlinked into the repo as the Python
   harness does with its targets; absolute, as the harness runs from the
   project root. *)
let symlinked_targets : F.t list =
  [ F.Symlink ("targets", !!(Fpath.v (Sys.getcwd ()) // fixtures_root / "targets")) ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan targets (e2e)"
    ((filecount_dirs
     |> List.map (fun (dir : string) ->
            t (Printf.sprintf "file count: %s" dir)
              ~checked_output:(Testo.stdxxx ()) ~normalize:normalise
              (run_scan caps ~root ~format_args:[] ~rule:"rules/filecount.yaml"
                 ~targets:[] ~extra_files:[ target_dir dir ] ~extra_args:[ dir ])))
    @ [
      (* outside a git repository: no line about git, and no block when
         nothing was skipped *)
      t "summary: no git repository, nothing skipped"
        ~checked_output:(Testo.stdxxx ()) ~normalize:normalise
        (run_scan caps ~git:false ~format_args:[] ~rule:"rules/eqeq.yaml"
           ~targets:[ "targets/basic/stupid.py" ]);
      t "summary: no git repository, a skipped file"
        ~checked_output:(Testo.stdxxx ()) ~normalize:normalise
        (run_scan caps ~git:false ~format_args:[] ~rule:"rules/eqeq.yaml"
           ~targets:[ "targets/basic/stupid.py" ]
           ~extra_args:[ "--exclude=stupid.py" ]);
      (* --project-root with a scanning root under a symlink that leaves
         the project: the paths are taken as typed *)
      t "forced project root with symlinked targets"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/eqeq.yaml"
           ~targets:[] ~extra_files:symlinked_targets
           ~extra_args:[ "--project-root"; "."; "targets/basic" ]);
      (* python: test_semgrepignore_ignore_log_report *)
      t "ignore log report: text" ~checked_output:(Testo.stdxxx ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[] ~rule:"rules/eqeq-basic.yaml"
           ~targets:[] ~extra_files:ignores_files ~extra_args:ignores_args);
      (* python: test_semgrepignore_ignore_log_json_report *)
      t "ignore log report: JSON" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/eqeq-basic.yaml" ~targets:[] ~extra_files:ignores_files
           ~extra_args:ignores_args);
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
      (* The 'paths' of a rule select its files. python: test_per_rule_include *)
      t "per-rule include" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"rules/per-rule-include.yaml" ~targets:[]
           ~extra_files:[ F.dir "targets" [ target_dir "per-rule-include" ] ]
           ~extra_args:[ "targets/per-rule-include" ]);
    ])
