(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests for where the scan subcommand sends its output: the
 * -o/--output destination and the --<format>-output ones.
 *
 * These are about the destinations rather than any one format, so they use
 * SARIF only as a convenient thing to write. The tests that check what a
 * SARIF document contains live in Test_scan_subcommand_sarif.ml.
 *)

(*****************************************************************************)
(* Individual tests                                                           *)
(*****************************************************************************)

(* --sarif-output alone: text report on stdout, SARIF written to the file. *)
let test_output_file (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* --sarif --sarif-output: SARIF on stdout and in the file. *)
let test_output_file_with_same_format_on_stdout (caps : Scan_subcommand.caps) ()
    =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--sarif"; "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* --json --sarif-output: JSON on stdout, SARIF in the file. *)
let test_output_file_with_other_format_on_stdout (caps : Scan_subcommand.caps)
    () =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--json"; "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* -o sends the primary format to the file instead of stdout. *)
let test_primary_output_to_file (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--sarif"; "-o"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* A nested destination: the parent directories are created. *)
let test_output_file_nested (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--sarif-output"; "sub/dir/findings.sarif" ]
    ~output_files:[ "sub/dir/findings.sarif" ]
    ()

(* A scan that finds nothing still writes its file, rather than leaving the
 * caller to meet an ENOENT. *)
let test_output_file_without_findings (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/regex/regex-nosemgrep.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "findings.sarif" ] ()

(* Two different formats targeting the same destination must abort. *)
let test_conflicting_output_destination (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_args:[ "--json"; "-o"; "out.json"; "--sarif-output"; "out.json" ]
    ~expect_abort:true ()

(* A repository can carry a symlink where the output is meant to go, and
 * writing through it would truncate whatever it resolves to. *)
let test_output_destination_is_symlink (caps : Scan_subcommand.caps) () =
  run_scan caps ~format_args:[] ~rule:"rules/eqeq.yaml"
    ~targets:[ "targets/basic/stupid.py" ]
    ~extra_files:
      [
        F.Symlink ("findings.sarif", "pointed_at.txt");
        F.File ("pointed_at.txt", "left alone\n");
      ]
    ~extra_args:[ "--sarif-output"; "findings.sarif" ]
    ~output_files:[ "pointed_at.txt" ] ~expect_abort:true ()

(*****************************************************************************)
(* Entry point                                                                *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan output (e2e)"
    [
      t "--<format>-output file, text on stdout"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_output_file caps);
      t "--<format>-output file, same format on stdout"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_output_file_with_same_format_on_stdout caps);
      t "--<format>-output file, another format on stdout"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_output_file_with_other_format_on_stdout caps);
      t "-o file, nothing on stdout" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (test_primary_output_to_file caps);
      t "--<format>-output nested destination"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_output_file_nested caps);
      t "output file written without findings"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_output_file_without_findings caps);
      t "conflicting formats for one destination"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (test_conflicting_output_destination caps);
      t "output destination is a symlink" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (test_output_destination_is_symlink caps);
    ]
