(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

(* the baseline test runs git itself *)
type caps = < Scan_subcommand.caps ; Cap.exec >

module F = Testutil_files
open Common
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of the skins other than 'legacy'.
 *
 * The rest of the suite sets OPENGREP_SKIN=legacy (see Test.ml), which
 * leaves the default skin untested. These snapshots cover 'simple' and
 * 'vivid': the sections they print, how they group findings, and their
 * dataflow traces.
 *
 * Under Testo the captured streams are regular files, so the style renderer
 * is off and a snapshot contains no escape sequence. Colour is therefore
 * checked separately, by assertion, together with the requirement that
 * 'vivid' remains readable without colour.
 *)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

(* Two severities, and one rule that matches twice in one file, so that the
   grouping by file and by rule both appear. *)
let two_rules =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
  - id: print-debug
    pattern: print(...)
    message: "left-over debugging print"
    languages: [python]
    severity: WARNING
|}

let a_py = {|
def f(a, b):
    return a + b == a + b

def g(x):
    return x == x
|}

(* a.py at the baseline commit, with one of the two findings, so that the
   baseline scan has a target and its plan is not empty *)
let a_py_baseline = {|
def f(a, b):
    return a + b == a + b
|}

let b_py = {|
def h(y):
    print(y)
    return y == y
|}

let taint_rule =
  {|
rules:
  - id: taint-flow
    mode: taint
    pattern-sources:
      - pattern: source()
    pattern-sinks:
      - pattern: sink(...)
    message: "tainted data reaches a sink"
    languages: [python]
    severity: ERROR
|}

let taint_py = {|
def go():
    x = source()
    y = x
    sink(y)
|}

(* A -e/--pattern run builds a rule with the id "-" and the pattern as its
   message; the report prints neither. One file and one match, so that the
   snapshot shows only the absence of the heading. *)
let pattern_py = {|
def h(y):
    print(y)
|}

let findings_files : F.t list =
  [
    F.File ("rules.yml", two_rules);
    F.File ("a.py", a_py);
    F.dir "sub" [ F.File ("b.py", b_py) ];
  ]

let taint_files : F.t list =
  [ F.File ("rules.yml", taint_rule); F.File ("t.py", taint_py) ]

let pattern_files : F.t list = [ F.File ("p.py", pattern_py) ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The horizontal line after a file path in 'vivid' extends to the width of
   the terminal, which Findings_layout reads once at start-up: under 'make
   test' that is the terminal the suite runs in. The mask keeps only the
   presence of the line, so that the snapshot does not depend on that
   terminal. The fixtures are short enough that nothing else wraps at the
   narrowest width the layout allows. *)
let mask_header_rule =
  Testo.mask_pcre_pattern ~replace:(fun (_ : string) -> "<RULE>") {|(?:\xe2\x94\x80){2,}|}

(* coupling: Test_scan_subcommand.normalize *)
let normalize =
  [
    Testutil_logs.mask_time;
    Test_scan_helpers.mask_test_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_line ~after:"Opengrep version: " ();
    mask_header_rule;
  ]

(* [Testutil_git.mask_temp_git_hash] masks only the line of the root commit,
   so a test that commits again needs this mask too. It is wider and includes
   the first, so it is not in the list above: every snapshot there would lose
   the "(root-commit)" marker of the line.
   coupling: Test_scan_subcommand.normalize_multi_commit *)
let normalize_multi_commit =
  normalize @ [ Testo.mask_line ~after:"[main " ~before:"]" () ]

(* The skin is set through the environment rather than --skin: Test.ml sets
   OPENGREP_SKIN for the whole suite, and the flag would take precedence over
   it with a warning that every snapshot would then contain. One test below
   covers the flag and its precedence. *)
let with_skin (skin : string) (f : unit -> 'a) : 'a =
  Semgrep_envvars.with_envvar "OPENGREP_SKIN" skin f

let config_argv = [ "opengrep-scan"; "--experimental"; "--config"; "rules.yml" ]

let pattern_argv =
  [ "opengrep-scan"; "--experimental"; "-e"; "print(...)"; "--lang"; "py" ]

let scan (caps : Scan_subcommand.caps) ?(argv : string list = config_argv)
    ~(files : F.t list) (extra_args : string list) : Exit_code.t =
  with_env_app_token (fun () ->
      Testutil_git.with_git_repo files (fun _cwd ->
          without_settings (fun () ->
              Scan_subcommand.main caps (Array.of_list (argv @ extra_args)))))

let test_findings (caps : Scan_subcommand.caps) (skin : string)
    (extra_args : string list) () =
  with_skin skin (fun () ->
      Exit_code.Check.ok (scan caps ~files:findings_files extra_args))

let test_pattern (caps : Scan_subcommand.caps) (skin : string) () =
  with_skin skin (fun () ->
      Exit_code.Check.ok (scan caps ~argv:pattern_argv ~files:pattern_files []))

let test_traces (caps : Scan_subcommand.caps) (skin : string) () =
  with_skin skin (fun () ->
      Exit_code.Check.ok
        (scan caps ~files:taint_files [ "--dataflow-traces" ]))

(* --baseline-commit runs the scan twice, and each run prints a plan. The
   second, of the baseline scan, is marked as such: it covers the same paths
   at the baseline commit, and without the mark its counts would appear to
   contradict the first. *)
let test_baseline_plan (caps : caps) (skin : string) () =
  let git (args : string list) : string =
    Git_wrapper.command (caps :> < Cap.exec >) args
  in
  with_skin skin (fun () ->
      with_env_app_token (fun () ->
          Testutil_git.with_git_repo
            [ F.File ("rules.yml", two_rules); F.File ("a.py", a_py_baseline) ]
            (fun _cwd ->
              let baseline = String.trim (git [ "rev-parse"; "HEAD" ]) in
              UFile.write_file ~file:(Fpath.v "a.py") a_py;
              ignore (git [ "add"; "." ] : string);
              ignore (git [ "commit"; "-q"; "-m"; "a second finding" ] : string);
              without_settings (fun () ->
                  Scan_subcommand.main
                    (caps :> Scan_subcommand.caps)
                    (Array.of_list
                       (config_argv @ [ "--baseline-commit"; baseline ])))
              |> Exit_code.Check.ok)))

(* The output of one run, on both streams, as a single string. *)
let captured (caps : Scan_subcommand.caps) ~(skin : string)
    ?(argv : string list = config_argv) ?(files : F.t list = findings_files)
    (extra_args : string list) : string =
  with_skin skin (fun () ->
      with_env_app_token (fun () ->
          Testutil_git.with_git_repo files (fun _cwd ->
              let _exit_code, output =
                Testo.with_capture stdout (fun () ->
                    without_settings (fun () ->
                        Scan_subcommand.main caps
                          (Array.of_list (argv @ extra_args))))
              in
              output)))

let has_escapes (output : string) : bool = String_.contains ~term:"\027[" output

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* With colour off, the severity stripe of 'vivid' remains as a character,
   so that the report stays readable. *)
let test_vivid_degrades (caps : Scan_subcommand.caps) () =
  let plain = captured caps ~skin:"vivid" [] in
  let coloured = captured caps ~skin:"vivid" [ "--force-color" ] in
  Alcotest.(check bool) "no colour off a terminal" false (has_escapes plain);
  Alcotest.(check bool) "colour with --force-color" true (has_escapes coloured);
  Alcotest.(check bool)
    "the stripe remains without colour" true
    (String_.contains ~term:"\u{258C}" plain)

(* --skin takes precedence over OPENGREP_SKIN, so that a single test can
   select another skin while the suite sets one. *)
let test_flag_beats_env (caps : Scan_subcommand.caps) () =
  let output = captured caps ~skin:"legacy" [ "--skin"; "simple" ] in
  Alcotest.(check bool)
    "the legacy severity arrows are gone" false
    (String_.contains ~term:"\u{276F}\u{2771}" output)

(* A skin renders only the text report. Each run has its own temporary
   repository, so the masked outputs are compared. *)
let test_json_unaffected (caps : Scan_subcommand.caps) () =
  let mask = Test_scan_helpers.mask_test_temp_paths () in
  let json (skin : string) = mask (captured caps ~skin [ "--json" ]) in
  let legacy = json "legacy" in
  Alcotest.(check string) "simple matches legacy" legacy (json "simple");
  Alcotest.(check string) "vivid matches legacy" legacy (json "vivid")

(* The rule of a -e/--pattern run has the id "-" and the pattern as its
   message. No skin prints either as a heading, which would look like a rule
   the user never wrote, but every skin reports the finding. *)
let test_pattern_has_no_heading (caps : Scan_subcommand.caps) () =
  [ "legacy"; "simple"; "vivid" ]
  |> List.iter (fun (skin : string) ->
         let output = captured caps ~skin ~argv:pattern_argv ~files:pattern_files [] in
         Alcotest.(check bool)
           (spf "%s: the finding is reported" skin)
           true
           (String_.contains ~term:"print(y)" output);
         Alcotest.(check bool)
           (spf "%s: the pattern is not used as a message" skin)
           false
           (String_.contains ~term:"print(...)" output))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : caps) =
  let narrow = (caps :> Scan_subcommand.caps) in
  Testo.categorize "Osemgrep Scan skins (e2e)"
    [
      t "simple" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_findings narrow "simple" []);
      t "vivid" ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_findings narrow "vivid" []);
      t "simple with dataflow traces"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_traces narrow "simple");
      t "vivid with dataflow traces"
        ~checked_output:(Testo.split_stdout_stderr ()) ~normalize
        (test_traces narrow "vivid");
      t "simple with -e" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (test_pattern narrow "simple");
      t "vivid with -e" ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize
        (test_pattern narrow "vivid");
      t "-e findings carry no synthesised heading"
        (test_pattern_has_no_heading narrow);
      t "simple names the baseline scan's plan"
        ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize:normalize_multi_commit
        (test_baseline_plan caps "simple");
      t "vivid names the baseline scan's plan"
        ~checked_output:(Testo.split_stdout_stderr ())
        ~normalize:normalize_multi_commit
        (test_baseline_plan caps "vivid");
      t "vivid degrades without colour" (test_vivid_degrades narrow);
      t "--skin wins over OPENGREP_SKIN" (test_flag_beats_env narrow);
      t "the skin does not reach --json" (test_json_unaffected narrow);
    ]
