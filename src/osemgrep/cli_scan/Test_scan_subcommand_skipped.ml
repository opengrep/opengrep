(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of what a scan reports about the files it did not scan:
 * the "Some files were skipped" block of the summary, the verbose listing
 * and the "paths" of the JSON output.
 *)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

(* A tree with something for most reasons to skip a file, copied into the
   repo under targets/ignores, with its ignore files also at the root as
   the scan reads them from there. *)
let ignores_root : Fpath.t = Fpath.v "tests/ignores"

let ignores_files : F.t list =
  let read (name : string) : string = UFile.read_file Fpath.(ignores_root / name) in
  [
    F.File (".semgrepignore", read ".semgrepignore");
    F.File (".gitignore", read ".gitignore");
    F.dir "targets" [ F.dir "ignores" (F.read ignores_root) ];
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
let filecount_root : Fpath.t = Fpath.v "tests/filecount"
let filecount_dirs : string list =
  [ "multilangproj"; "language-filtering"; "exclude_include" ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan skipped (e2e)"
    ((filecount_dirs
     |> List.map (fun (dir : string) ->
            t (Printf.sprintf "file count: %s" dir)
              ~checked_output:(Testo.stdxxx ()) ~normalize:normalise
              (run_scan caps ~format_args:[] ~rule:"rules/filecount.yaml"
                 ~targets:[]
                 ~extra_files:[ F.dir dir (F.read Fpath.(filecount_root / dir)) ]
                 ~extra_args:[ dir ])))
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
      (* python: test_semgrepignore_ignore_log_report *)
      t "ignore log report: text" ~checked_output:(Testo.stdxxx ())
        ~normalize:normalise
        (run_scan caps ~format_args:[] ~rule:"rules/eqeq-basic.yaml"
           ~targets:[] ~extra_files:ignores_files ~extra_args:ignores_args);
      (* python: test_semgrepignore_ignore_log_json_report *)
      t "ignore log report: JSON" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (run_scan caps ~format_args:[ "--json" ] ~rule:"rules/eqeq-basic.yaml"
           ~targets:[] ~extra_files:ignores_files ~extra_args:ignores_args);
    ])
