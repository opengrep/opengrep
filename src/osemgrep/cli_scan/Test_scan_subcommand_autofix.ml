(* Opengrep authors
 *
 * Copyright (C) 2026 Opengrep authors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module F = Testutil_files

let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of --autofix: every rule and target pair of
 * tests/autofix/scan, with and without --dryrun, in JSON and in text
 * output. The snapshot holds the output of the scan and the target after
 * it, which --dryrun leaves as it was.
 *)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

let fixtures_root : Fpath.t = Fpath.v "tests/autofix/scan"

(* (rule in rules/, target in targets/) *)
let pairs : (string * string) list =
  [
    ("autofix.yaml", "autofix.py");
    ("overlapping-collision.yaml", "collision.py");
    ("python-assert-statement.yaml", "python-assert-statement.py");
    ("python-ranges.yaml", "python-ranges.py");
    ("replace-field-yaml.yaml", "replace-field-yaml.yaml");
    ("three-autofixes.yaml", "three-autofixes.py");
    ("java-string-wrap.yaml", "java-string-wrap.java");
    ("exact-collision.yaml", "collision.py");
    ("redundant.yaml", "redundant.py");
    ("ocaml_paren_expr.yaml", "ocaml_paren_expr.ml");
    ("python-delete-import.yaml", "python-delete-import.py");
    ("two-autofixes.yaml", "two-autofixes.txt");
    ("csv-writer.yaml", "csv-writer.py");
    ("defaulthttpclient.yaml", "defaulthttpclient.java");
    ("flask-use-jsonify.yaml", "flask-use-jsonify.py");
    ("requests-use-timeout.yaml", "requests-use-timeout.py");
    ("django-none-password-default.yaml", "django-none-password-default.py");
    ("imported-entity.yaml", "imported-entity.py");
    ("add-metadata-hcl.yaml", "add-metadata-hcl.hcl");
    ("delete-partial-line.yaml", "delete-partial-line.py");
    ("utf-8.yaml", "utf-8.py");
  ]

(*****************************************************************************)
(* Running a scan *)
(*****************************************************************************)

(* The rule and the target are copied into a throw-away git repo, in their
 * rules/ and targets/ directories (a rule and its target can share a
 * name). The target is printed after the scan so that it is part of the
 * snapshot. *)
let run_autofix (caps : Scan_subcommand.caps) ~(rule : string)
    ~(target : string) ~(dryrun : bool) ~(format_args : string list) () =
  let read (rel : string) : string =
    UFile.read_file Fpath.(fixtures_root // v rel)
  in
  let rule_path = "rules/" ^ rule and target_path = "targets/" ^ target in
  let repo_files : F.t list =
    [
      F.Dir ("rules", [ F.File (rule, read rule_path) ]);
      F.Dir ("targets", [ F.File (target, read target_path) ]);
    ]
  in
  Test_scan_helpers.with_env_app_token (fun () ->
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let argv : string array =
            Array.of_list
              ([ "opengrep-scan"; "--experimental"; "--config"; rule_path; "--autofix" ]
              @ (if dryrun then [ "--dryrun" ] else [])
              @ format_args @ [ target_path ])
          in
          let exit_code : Exit_code.t =
            Test_scan_helpers.without_settings (fun () ->
                Scan_subcommand.main caps argv)
          in
          UCommon.pr (spf "--- %s after the scan ---" target_path);
          UCommon.pr (UFile.read_file (Fpath.v target_path));
          Exit_code.Check.ok exit_code))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan autofix (e2e)"
    (pairs
    |> List.concat_map (fun ((rule : string), (target : string)) ->
           (* the JSON goes to stdout; the text output and its stderr *)
           [
             ("json", [ "--json" ], Testo.stdout ());
             ("text", [], Testo.stdxxx ());
           ]
           |> List.concat_map
                (fun ( (label : string),
                       (format_args : string list),
                       (checked_output : Testo.checked_output_kind) ) ->
                  [ true; false ]
                  |> List_.map (fun (dryrun : bool) ->
                         t
                           (spf "autofix: %s %s %s%s" rule target label
                              (if dryrun then " --dryrun" else ""))
                           ~checked_output ~normalize:Test_scan_helpers.normalise
                           (run_autofix caps ~rule ~target ~dryrun ~format_args)))))
