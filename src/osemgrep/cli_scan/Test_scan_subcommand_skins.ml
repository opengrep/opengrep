(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

(* the baseline test drives git itself, which a scan does not need *)
type caps = < Scan_subcommand.caps ; Cap.exec >

module F = Testutil_files
open Common
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of the skins other than 'legacy'.
 *
 * The rest of the suite pins OPENGREP_SKIN=legacy (see Test.ml), which
 * leaves the skin a scan actually uses by default untested. These snapshots
 * are what covers 'simple' and 'vivid': the sections they emit, how they
 * group findings, and what they do with a dataflow trace.
 *
 * Under Testo the captured streams are regular files, so the style renderer
 * is off and a snapshot holds no escape sequence. Colour is therefore
 * checked separately, by assertion, together with the requirement that
 * 'vivid' stay readable on structure alone once colour is gone.
 *)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

(* Two severities, and one rule that matches twice in one file, so that a
   skin's grouping by file and by rule both show up. *)
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

(* a.py as it stood at the baseline commit: one of the two findings, so the
   replay has a file to look at and its plan is not the empty one *)
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

(* A -e/--pattern run builds a rule with the id "-" and the pattern text as
   its message; neither belongs in the report. Kept to one file and one
   match, so the snapshot is about the heading that is not there. *)
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

(* The rule drawn after a file name in 'vivid' runs to the width of the
   terminal, which Findings_layout reads once at start-up: under 'make test'
   that is whatever window the suite was launched from. Only the presence of
   the rule is checked, so that the snapshot is of the report and not of the
   developer's terminal. The fixtures are short enough that nothing else
   wraps at the narrowest width the layout allows. *)
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

(* [Testutil_git.mask_temp_git_hash] masks the root commit's line only, so a
   test that commits again needs this too. It is wider than that mask and
   subsumes it, which is why it is not in the list above: every snapshot
   there would lose the "(root-commit)" that names the line.
   coupling: Test_scan_subcommand.normalize_multi_commit *)
let normalize_multi_commit =
  normalize @ [ Testo.mask_line ~after:"[main " ~before:"]" () ]

(* The skin comes from the environment rather than from --skin: Test.ml sets
   OPENGREP_SKIN for the whole suite, and the flag would win over it with a
   warning that would then be part of every snapshot. One test below covers
   the flag, and that precedence, on its own. *)
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

(* --baseline-commit runs the scan twice, and each run states a plan. The
   second is the replay against the baseline commit, and says which scan it
   belongs to: it covers the same paths as they stood then, so its counts
   read as a contradiction of the first without that. *)
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

(* The plan's hard requirement for 'vivid': with colour off, the severity
   stripe must survive as a character, so that the report is still readable
   on structure alone. *)
let test_vivid_degrades (caps : Scan_subcommand.caps) () =
  let plain = captured caps ~skin:"vivid" [] in
  let coloured = captured caps ~skin:"vivid" [ "--force-color" ] in
  Alcotest.(check bool) "no colour off a terminal" false (has_escapes plain);
  Alcotest.(check bool) "colour with --force-color" true (has_escapes coloured);
  Alcotest.(check bool)
    "the stripe survives without colour" true
    (String_.contains ~term:"\u{258C}" plain)

(* --skin wins over OPENGREP_SKIN, which is what lets a single test ask for
   another skin while the suite pins one. *)
let test_flag_beats_env (caps : Scan_subcommand.caps) () =
  let output = captured caps ~skin:"legacy" [ "--skin"; "simple" ] in
  Alcotest.(check bool)
    "the legacy severity arrows are gone" false
    (String_.contains ~term:"\u{276F}\u{2771}" output)

(* A skin renders the text report and nothing else. Each run gets its own
   temporary repository, so the comparison is of the masked output. *)
let test_json_unaffected (caps : Scan_subcommand.caps) () =
  let mask = Test_scan_helpers.mask_test_temp_paths () in
  let json (skin : string) = mask (captured caps ~skin [ "--json" ]) in
  let legacy = json "legacy" in
  Alcotest.(check string) "simple matches legacy" legacy (json "simple");
  Alcotest.(check string) "vivid matches legacy" legacy (json "vivid")

(* The rule a -e/--pattern run synthesises has the id "-" and carries the
   pattern as its message. Heading a finding with either says nothing and
   reads as a rule the user never wrote, so no skin prints them -- while
   the finding itself still has to be reported. *)
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
