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

(* An absolute --config path outside the scanned directory prefixes the rule
   ids with its own directories, as pysemgrep's convert_config_id_to_prefix
   did, so both the rule id and the fingerprint depend on where the checkout
   lives. *)
let mask_absolute_config_prefix : (string -> string) list =
  [
    Testo.mask_pcre_pattern {|"check_id":"([^"]*)tests\.configs\.rules\.|};
    Testo.mask_pcre_pattern {|"fingerprint":"([0-9a-f]+)_|};
  ]

(* The remote rule of the Python tests. Nothing is fetched: the fixture
   below is served in its place. *)
let template_url : string =
  "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"

(* Read from the project root, before a test descends into its repo. *)
let url_rule_content : string = read_fixture ~root "url-template.yaml"

let with_url_rule (f : unit -> unit) : unit -> unit =
  Http_mock_client.with_testing_client
    (fun (req : Cohttp.Request.t) (_body : Cohttp_lwt.Body.t) ->
      (* the request the mock client hands over carries no scheme *)
      let url : string =
        Uri.to_string (Uri.with_scheme (Cohttp.Request.uri req) (Some "https"))
      in
      if not (String.equal url template_url) then
        Alcotest.failf "unexpected request: %s" url;
      Lwt.return
        (Http_mock_client.basic_response
           (Cohttp_lwt.Body.of_string url_rule_content)))
    f

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
        ~normalize:(normalise @ mask_absolute_config_prefix)
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
      (* A rule loaded from a URL keeps its bare id.
         python: test_url_rule *)
      t "config: a rule from a URL" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (with_url_rule
           (json_scan caps ~config_args:[ "--config"; template_url ]
              ~targets:[ "targets/basic/stupid.py" ]));
      (* The rule from the local file takes a 'rules.' prefix from its
         path, the one from the URL keeps its bare id.
         python: test_multiple_configs_different_origins *)
      t "config: a local file and a URL" ~checked_output:(Testo.stdout ())
        ~normalize:normalise
        (with_url_rule
           (json_scan caps ~extra_files:[ rules_dir ]
              ~config_args:
                [ "--config"; "rules/eqeq.yaml"; "--config"; template_url ]
              ~targets:[ "targets/basic/stupid.py" ]));
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
         stops on it rather than running the other config, with the exit
         code of a configuration that could not be loaded, as the Python
         wrapper had.
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
           ~check:Exit_code.Check.missing_config);
      (* A rule whose pattern does not parse: the JSON carries the rule
         parse error and nothing is scanned.
         differs from the Python wrapper: it ends with exit code 2 and
         reports the error with code 2, opengrep aborts on the
         configuration with exit code 7 and reports the error with code 4,
         and the wrapper lists the target as scanned and the rule as a
         skipped path, where opengrep lists neither
         python: test_rule_parser__failure__error_messages *)
      t "config: a rule pattern that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"bad-java-rule.yaml"
           ~targets:[ "targets/bad/basic_java.java" ]
           ~extra_args:[ "--verbose"; "--strict"; "basic_java.java" ]
           ~check:Exit_code.Check.missing_config);
      (* A config that parses but holds no rule: the "No config given" error
         of the Python wrapper, with the exit code of a missing
         configuration. *)
      t "config: a file with an empty rules list"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~extra_files:[ F.File ("emptyrules.yaml", "rules: []\n") ]
           ~extra_args:[ "--config"; "emptyrules.yaml" ]
           ~targets:[ "targets/basic/stupid.py" ]
           ~check:Exit_code.Check.missing_config);
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
