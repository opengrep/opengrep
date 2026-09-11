(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Fpath_.Operators
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of how the rules of a scan are loaded: the form of
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
   tests of how the config path is given. *)
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
(* Configurations that do not load *)
(*****************************************************************************)

(* Rule files, each broken in one way, as the errors of a configuration
   decide the exit code of the scan. *)
let unparsable_pattern_rule : string =
  {|rules:
  - id: badpat
    pattern: "def foo("
    message: m
    languages: [python]
    severity: WARNING
|}

let unknown_language_rule : string =
  {|rules:
  - id: badlang
    pattern: foo()
    message: m
    languages: [nosuchlang]
    severity: WARNING
|}

(* an unterminated quoted scalar *)
let unparsable_yaml_rule : string =
  {|rules:
  - id: bad
    pattern: "foo(
    languages: [python]
|}

let incompatible_rule : string =
  {|rules:
  - id: too-new
    min-version: 99.0.0
    pattern: foo()
    message: m
    languages: [python]
    severity: WARNING
|}

(* the broken rule first, the incompatible one last: the exit code is that
   of the pattern that does not parse, the incompatible rule being below the
   Error severity *)
let unparsable_then_incompatible_rules : string =
  {|rules:
  - id: badpat
    pattern: "def foo("
    message: m
    languages: [python]
    severity: WARNING
  - id: too-new
    min-version: 99.0.0
    pattern: foo()
    message: m
    languages: [python]
    severity: WARNING
|}

(* Two rule files, each broken in a different way, one of them in a
   subdirectory of the config directory. Sorted by path, rules/lang.yaml
   comes before rules/sub/pat.yaml. *)
let broken_rules_dir : F.t list =
  [
    F.dir "rules"
      [
        F.File ("lang.yaml", unknown_language_rule);
        F.dir "sub" [ F.File ("pat.yaml", unparsable_pattern_rule) ];
      ];
  ]

(* The same two rule files with their names exchanged, so that the file
   whose pattern does not parse sorts before the one with the unknown
   language. *)
let broken_rules_dir_swapped : F.t list =
  [
    F.dir "rules"
      [
        F.File ("pat.yaml", unparsable_pattern_rule);
        F.dir "sub" [ F.File ("lang.yaml", unknown_language_rule) ];
      ];
  ]

(* The exit code of a --json scan of [files] run with --config [config], and
   the codes carried by the errors of the document it prints. *)
let scan_error_codes (caps : Scan_subcommand.caps) (files : F.t list)
    (config : string) : int * int list =
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo
        (F.File ("target.py", "foo()\n") :: files)
        (fun (_ : Fpath.t) ->
          let exit_code, out =
            Testo.with_capture stdout (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      [|
                        "opengrep-scan";
                        "--experimental";
                        "--json";
                        "--config";
                        config;
                        "target.py";
                      |]))
          in
          ( Exit_code.to_int exit_code,
            (Semgrep_output_v1_j.cli_output_of_string out).errors
            |> List_.map (fun (e : Semgrep_output_v1_t.cli_error) -> e.code) )))

(* The same for a configuration made of the single rule file [rule]. *)
let config_error_codes (caps : Scan_subcommand.caps) (rule : string) :
    int * int list =
  scan_error_codes caps [ F.File ("rule.yaml", rule) ] "rule.yaml"

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
         parse error and nothing is scanned. The run ends with the fatal
         exit code and the error carries code 2, as the Python wrapper's
         did.
         differs from the Python wrapper: it lists the target as scanned
         and the rule as a skipped path, where opengrep lists neither
         python: test_rule_parser__failure__error_messages *)
      t "config: a rule pattern that does not parse"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~rule:"bad-java-rule.yaml"
           ~targets:[ "targets/bad/basic_java.java" ]
           ~extra_args:[ "--verbose"; "--strict"; "basic_java.java" ]
           ~check:Exit_code.Check.fatal);
      (* A config that parses but holds no rule is not a missing
         configuration: the scan scans nothing, reports no error and
         succeeds. *)
      t "config: a file with an empty rules list"
        ~checked_output:(Testo.stdout ()) ~normalize:normalise
        (run_scan caps ~root ~format_args:[ "--json" ]
           ~extra_files:[ F.File ("emptyrules.yaml", "rules: []\n") ]
           ~extra_args:[ "--config"; "emptyrules.yaml" ]
           ~targets:[ "targets/basic/stupid.py" ]
           ~check:Exit_code.Check.ok);
      (* The exit code of a configuration that does not load is decided by
         the kinds of error it produced: an unknown language left
         config_resolver.py with its own code, a pattern the engine could not
         parse is reported by the scan and exits with the fatal code, and
         every other kind it collected was covered by the
         MISSING_CONFIG_EXIT_CODE run_scan.py raised.
         differs from the Python wrapper: it adds an entry of its own with
         code 7 to the errors of the invalid YAML, which opengrep reports
         with the entry of the YAML error alone *)
      t "config: the exit code of a configuration that does not load"
        (fun () ->
          let check (name : string) (rule : string)
              (expected : int * int list) : unit =
            Alcotest.(check (pair int (list int)))
              name expected (config_error_codes caps rule)
          in
          check "a pattern that does not parse" unparsable_pattern_rule
            (2, [ 2 ]);
          check "an unknown language" unknown_language_rule (8, [ 8 ]);
          check "a file that is not valid YAML" unparsable_yaml_rule (7, [ 5 ]);
          (* an incompatible rule is reported with severity Info, and a
             configuration whose errors are all below Error leaves the run
             successful *)
          check "a rule that requires a newer version" incompatible_rule
            (0, [ 0 ]);
          check "a broken rule then an incompatible one"
            unparsable_then_incompatible_rules (2, [ 2; 0 ]);
          (* a configuration that resolved and holds no rule is not a
             missing configuration: it scans nothing and succeeds *)
          check "a config with an empty rules list" "rules: []\n" (0, []));
      (* The rule files of a config directory are read in path order, so the
         errors come out in that order. The exit code is decided by which
         kinds of error the configuration produced, in a fixed order: an
         unknown language, then a pattern that does not parse, then any
         other error of severity Error. It therefore does not depend on
         which of the two files sorts first.
         differs from the Python wrapper: it reports the unknown language
         alone, where opengrep reports both broken rules *)
      t "config: a directory is read in path order" (fun () ->
          let check (name : string) (files : F.t list)
              (expected : int * int list) : unit =
            Alcotest.(check (pair int (list int)))
              name expected (scan_error_codes caps files "rules")
          in
          check
            "the unknown language of rules/lang.yaml, then the pattern of \
             rules/sub/pat.yaml"
            broken_rules_dir (8, [ 8; 2 ]);
          check
            "the pattern of rules/pat.yaml, then the unknown language of \
             rules/sub/lang.yaml"
            broken_rules_dir_swapped (8, [ 2; 8 ]));
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
