(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Fpath_.Operators
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of how the rules of a scan are loaded: the spelling of
 * a --config path, a hidden rule file, several configs, a rule given
 * twice, nested pattern operators. The findings are checked in JSON.
 *
 * The fixtures are under tests/configs.
 *)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

let root : Fpath.t = Fpath.v "tests/configs"

(* The rules directory copied into the repo under the same name, for the
   tests that spell the config path. *)
let rules_dir : F.t = F.dir "rules" (F.read (root / "rules"))

(* Rule directories whose names carry the characters under test, copied
   into the repo under rules/. They are kept outside the fixtures' rules
   directory, which rules_dir above copies whole.
   python: test_rule_id_paths *)
let rule_id_dirs : F.t =
  F.dir "rules" [ F.dir "rule_id" (F.read (root / "rule_id")) ]

(* Three numbers separated by dots, as the Python test's regexp asks of the
   line that --version prints. *)
let is_version_number (line : string) : bool =
  let digits (s : string) : bool =
    (not (String.equal s ""))
    && String.for_all (fun (c : char) -> c >= '0' && c <= '9') s
  in
  match String.split_on_char '.' line with
  | major :: minor :: patch :: _ -> digits major && digits minor && digits patch
  | _ -> false

(* The file those rules are run on. *)
let rule_id_target : F.t =
  F.File ("hello.txt", read_fixture ~root "targets/rule_id/hello.txt")

(* The eqeq rule of the fixtures, absolute; the harness runs from the
   project root. *)
let absolute_eqeq : string = !!(Fpath.v (Sys.getcwd ()) // root / "rules" / "eqeq.yaml")

let json_scan (caps : Scan_subcommand.caps) ?rule ?(extra_files = [])
    ~(config_args : string list) ~(targets : string list) () =
  run_scan caps ~root ?rule ~format_args:[ "--json" ] ~targets ~extra_files
    ~extra_args:config_args ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan config (e2e)"
    [
      (* python: test_basic_rule__local *)
      t "config: a local file" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~rule:"rules/eqeq.yaml" ~config_args:[]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* python: test_basic_rule__relative *)
      t "config: a path with '..'" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~extra_files:[ rules_dir ]
           ~config_args:[ "--config"; "rules/../rules/eqeq.yaml" ]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* python: test_basic_rule__absolute *)
      t "config: an absolute path" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~config_args:[ "--config"; absolute_eqeq ]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* A hidden directory of rules, named explicitly.
         python: test_hidden_rule__explicit *)
      t "config: a hidden directory named explicitly"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_scan caps ~extra_files:[ rules_dir ]
           ~config_args:[ "--config"; "rules/hidden/.hidden" ]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* The hidden directory is found under the directory named.
         python: test_hidden_rule__implicit *)
      t "config: a directory holding a hidden one"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_scan caps ~extra_files:[ rules_dir ]
           ~config_args:[ "--config"; "rules/hidden" ]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* python: test_multiple_configs_file *)
      t "config: two files" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~rule:"rules/eqeq.yaml" ~extra_files:[ rules_dir ]
           ~config_args:[ "--config"; "rules/eqeq-python.yaml" ]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* A rule given twice, differing only in its metadata, runs once.
         python: test_deduplication *)
      t "config: a duplicated rule runs once" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~rule:"rules/duplicate-rule.yaml" ~config_args:[]
           ~targets:[ "targets/basic/stupid.py" ]);
      (* python: test_nested_patterns_rule *)
      t "config: nested patterns" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~rule:"rules/nested-patterns.yaml" ~config_args:[]
           ~targets:[ "targets/basic/nested-patterns.js" ]);
      (* python: test_nested_pattern_either_rule *)
      t "config: nested pattern-either" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~rule:"rules/nested-pattern-either.yaml"
           ~config_args:[] ~targets:[ "targets/basic/nested-patterns.js" ]);
      (* The rule id takes a prefix from the config path, which drops the
         '@' and the ';'. The pattern is generic and matches the rule files
         too, so the scan is given the target file.
         python: test_rule_id_paths *)
      t "config: a directory named '@'" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~extra_files:[ rule_id_dirs; rule_id_target ]
           ~config_args:[ "--config"; "rules/rule_id/@"; "hello.txt" ]
           ~targets:[]);
      t "config: a directory named ';'" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (json_scan caps ~extra_files:[ rule_id_dirs; rule_id_target ]
           ~config_args:[ "--config"; "rules/rule_id/;"; "hello.txt" ]
           ~targets:[]);
      t "config: a directory named '@npm-style'"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_scan caps ~extra_files:[ rule_id_dirs; rule_id_target ]
           ~config_args:[ "--config"; "rules/rule_id/@npm-style"; "hello.txt" ]
           ~targets:[]);
      (* Two configs, one of which holds a rule without an id: the scan
         stops on it rather than running the other config.
         differs from the Python wrapper: it ends with exit code 7,
         "missing configuration", opengrep with 5, "unparseable YAML"
         python: test_multi_config_fail *)
      t "config: two configs, one of them broken"
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"multi_config_fail/error.yaml"
           ~extra_files:
             [
               F.File
                 ( "no_error.yaml",
                   read_fixture ~root "multi_config_fail/no_error.yaml" );
             ]
           ~extra_args:[ "--config"; "no_error.yaml" ]
           ~targets:[ "targets/basic/stupid.py" ]
           ~check:Exit_code.Check.unparseable_yaml);
      (* A rule whose pattern does not parse: the JSON carries the rule
         parse error and nothing is scanned.
         differs from the Python wrapper: it ends with exit code 2 and
         reports the error with code 2, opengrep with exit code 4 and
         code 4, and it lists the target as scanned and the rule as a
         skipped path, where opengrep lists neither
         python: test_rule_parser__failure__error_messages *)
      t "config: a rule pattern that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"bad-java-rule.yaml"
           ~targets:[ "targets/bad/basic_java.java" ]
           ~extra_args:[ "--verbose"; "--strict"; "basic_java.java" ]
           ~check:Exit_code.Check.invalid_pattern);
      (* --version prints the version and nothing else. It changes at every
         release, so it is matched rather than snapshotted.
         python: test_version *)
      t "config: --version prints a version number" (fun () ->
          let (), (out : string) =
            Testo.with_capture stdout (fun () ->
                run_scan caps ~root ~git:false ~format_args:[] ~targets:[]
                  ~extra_args:[ "--version"; "--disable-version-check" ] ())
          in
          Alcotest.(check bool)
            "a version number was printed" true
            (String.split_on_char '\n' out |> List.exists is_version_number));
      (* min-version and max-version decide which rules run; the ones out
         of range are skipped and reported.
         differs from the Python wrapper: the message of a skipped rule is
         worded differently, see the report
         python: test_version_constraints *)
      t "config: rules with a version constraint"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (json_scan caps ~rule:"version-constraints.yaml" ~config_args:[]
           ~targets:[ "targets/version-constraints/x.py" ]);
    ]
