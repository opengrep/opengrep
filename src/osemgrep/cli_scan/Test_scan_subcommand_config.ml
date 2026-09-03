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
    ]
