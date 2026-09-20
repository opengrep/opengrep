(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
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
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* The expected strings follow the algorithm described in Formula_string.ml
 * (every string under the pattern keys, sorted joins per level), with each
 * metavariable name then replaced by its content in the listed order. They
 * were checked against pysemgrep's rule.py formula_string() and
 * rule_match.py get_match_based_key().
 *)
let match_based_id_formula_expectations =
  [
    (* e2e/rules/eqeq.yaml 1st rule *)
    ( "basic rule",
      {|
rules:
  - id: assert-eqeq-is-ok
    pattern: $X == $X
    message: "possibly useless comparison but in eq function"
    languages: [python]
    severity: ERROR
|},
      [ ("$X", "1") ],
      "1 == 1" );
    (* e2e/rules/eqeq.yaml 2nd rule *)
    ( "many patterns",
      {|
rules:
  - id: eqeq-is-bad
    patterns:
      - pattern-not-inside: |
          def __eq__(...):
              ...
      - pattern-not-inside: assert(...)
      - pattern-not-inside: assertTrue(...)
      - pattern-not-inside: assertFalse(...)
      - pattern-either:
          - pattern: $X == $X
          - pattern: $X != $X
          - patterns:
              - pattern-inside: |
                  def __init__(...):
                      ...
              - pattern: self.$X == self.$X
      - pattern-not: 1 == 1
    message: "useless comparison operation `$X == $X` or `$X != $X`"
    languages: [python]
    severity: ERROR
    metadata:
      shortlink: https://sg.run/xyz1
      source: https://semgrep.dev/r/eqeq-bad
|},
      [ ("$X", "a+b") ],
      "a+b != a+b a+b == a+b def __init__(...):\n\
      \    ...\n\
      \ self.a+b == self.a+b 1 == 1 assert(...) assertFalse(...) \
       assertTrue(...) def __eq__(...):\n\
      \    ...\n" );
    (* e2e/rules/taint_trace.yaml: labels, requires and focus-metavariable
     * count, the metavariables not bound by the match stay as they are *)
    ( "taint with labels",
      {|
rules:
  - id: taint-trace
    message: found an error
    languages:
      - cpp
      - c
    severity: WARNING
    mode: taint
    metadata:
      interfile: true
    pattern-sources:
      - label: USER_CONTROLLED
        patterns:
          - pattern: SOURCE()
      - label: SCALAR
        requires: USER_CONTROLLED
        patterns:
          - pattern-either:
              - pattern: $LHS + $RHS
          - focus-metavariable:
              - $RHS
              - $LHS
    pattern-sinks:
      - requires: USER_CONTROLLED and SCALAR
        patterns:
          - pattern-either:
              - pattern: SINK(<... $SRC ...>)
          - focus-metavariable: $SRC
|},
      [ ("$RHS", "res1"); ("$SRC", "res2") ],
      "$LHS res1 $LHS + res1 SCALAR USER_CONTROLLED SOURCE() USER_CONTROLLED \
       res2 SINK(<... res2 ...>) USER_CONTROLLED and SCALAR" );
    (* e2e/rules/metavariable-regex/metavariable-regex.yaml: the condition's
     * metavariable name and regex count *)
    ( "metavariable-regex",
      {|
rules:
  - id: metavar-test
    patterns:
      - pattern: "metavariable_regex_test($X)"
      - metavariable-regex:
          metavariable: "$X"
          regex: '("test"|"example")'
    message: "Metavariable regex test"
    languages: [python]
    severity: ERROR
|},
      [ ("$X", "\"test\"") ],
      "\"test\" (\"test\"|\"example\") metavariable_regex_test(\"test\")" );
    (* a boolean under a pattern key empties the whole string *)
    ( "boolean under a pattern key",
      {|
rules:
  - id: strip
    patterns:
      - pattern: foo($X)
      - metavariable-comparison:
          metavariable: $X
          comparison: $X > 1
          strip: true
    message: m
    languages: [python]
    severity: ERROR
|},
      [ ("$X", "2") ],
      "" );
    ( "nested either with focus list",
      {|
rules:
  - id: nested
    patterns:
      - pattern-either:
          - pattern: a($X)
          - patterns:
              - pattern-inside: |
                  def f(...):
                    ...
              - pattern: b($X, $Y)
      - focus-metavariable:
          - $Y
          - $X
      - pattern-not: c()
    message: m
    languages: [python]
    severity: ERROR
|},
      [ ("$X", "1"); ("$Y", "2") ],
      "1 2 a(1) b(1, 2) def f(...):\n  ...\n c()" );
    ( "pattern-regex",
      {|
rules:
  - id: rx
    pattern-regex: (abc)+
    message: m
    languages: [generic]
    severity: ERROR
|},
      [],
      "(abc)+" );
    (* the substitution is a plain replace in the order of the metavariables:
     * $X inside $XY gets replaced too *)
    ( "metavariable name prefix of another",
      {|
rules:
  - id: prefix
    pattern: foo($X, $XY)
    message: m
    languages: [python]
    severity: ERROR
|},
      [ ("$X", "1"); ("$XY", "2") ],
      "foo(1, 1Y)" );
  ]

let test_match_based_id_formula _caps =
  Testo.categorize "match-based id formula"
    (match_based_id_formula_expectations
    |> List_.map (fun (title, rule, mvars, expected) ->
           t title (fun () ->
               UTmp.with_temp_file ~contents:rule (fun file ->
                   match Parse_rule.parse file with
                   | Ok [ rule ] ->
                       let mvars =
                         mvars
                         |> List_.map (fun (mvar, mvalue_str) ->
                                ( mvar,
                                  Out.
                                    {
                                      abstract_content = mvalue_str;
                                      propagated_value = None;
                                      (* not used by Metavar_replacement *)
                                      start = { line = 0; col = 0; offset = 0 };
                                      end_ = { line = 0; col = 0; offset = 0 };
                                    } ))
                       in
                       let res =
                         Semgrep_hashing_functions.Match_based_id.formula
                           Pysemgrep rule (Some mvars)
                       in
                       Alcotest.(check string) __LOC__ expected res
                   | _ ->
                       failwith
                         (spf "could not parse or more than one rule for %s"
                            title)))))

(*****************************************************************************)
(* Findings_layout *)
(*****************************************************************************)

(* the end column comes before the start column *)
let ends_left =
  {|def go():
    x = source(
        1,
    )
|}

(* the end column is past the end of the first line *)
let ends_right =
  {|def go():
    x = source(1,
               2222222222)
|}

let location (file : Fpath.t) ((l1 : int), (c1 : int)) ((l2 : int), (c2 : int))
    : Out.location =
  {
    path = file;
    start = { line = l1; col = c1; offset = 0 };
    end_ = { line = l2; col = c2; offset = 0 };
  }

(* What [pp] prints for the location in a file of [contents], with the
   highlight shown as brackets. *)
let render_location ~(contents : string)
    (pp : Format.formatter -> Out.location -> unit) (start : int * int)
    (end_ : int * int) : string =
  UTmp.with_temp_file ~contents (fun (file : Fpath.t) ->
      let buf = Buffer.create 80 in
      let ppf = Format.formatter_of_buffer buf in
      Fmt.set_style_renderer ppf `Ansi_tty;
      pp ppf (location file start end_);
      Format.pp_print_flush ppf ();
      Buffer.contents buf
      |> Str.global_replace (Str.regexp_string "\027[1m") "["
      |> Str.global_replace (Str.regexp_string "\027[0m") "]")

let gutter (n : int) : string = spf "%d|" n
let gutter_blank = " |"

let test_location_highlighted_per_line () =
  let pp ?dedent () =
    Findings_layout.pp_trace_location ?dedent ~prefix:"" ~gutter ~gutter_blank
      ~highlight:[ `Bold ]
  in
  Alcotest.(check string)
    "ending left of the start" "2|    x = [source(]\n |        [1,]\n |    [)]\n"
    (render_location ~contents:ends_left (pp ()) (2, 9) (4, 6));
  Alcotest.(check string)
    "ending right of the first line"
    "2|    x = [source(1,]\n |               [2222222222)]\n"
    (render_location ~contents:ends_right (pp ()) (2, 9) (3, 27));
  Alcotest.(check string)
    "dedented" "2|x = [source(]\n |    [1,]\n |[)]\n"
    (render_location ~contents:ends_left (pp ~dedent:true ()) (2, 9) (4, 6))

(* The legacy report prints what pysemgrep did, the arithmetic included:
   the highlight is cut out of the joined lines, and a location it cannot
   be cut out of is left out. *)
let test_legacy_location_cut_from_joined_lines () =
  let pp =
    Findings_layout.pp_joined_location ~prefix:"" ~gutter ~gutter_blank
      ~highlight:[ `Bold ]
  in
  Alcotest.(check string)
    "ending left of the start: left out" ""
    (render_location ~contents:ends_left pp (2, 9) (4, 6));
  Alcotest.(check string)
    "ending right of the first line: the cut runs on into the next"
    "2|    x = [source(1,\n |      ]         2222222222)\n"
    (render_location ~contents:ends_right pp (2, 9) (3, 27))

let findings_layout_tests =
  Testo.categorize "Findings_layout"
    [
      t "a location over several lines is highlighted on each"
        test_location_highlighted_per_line;
      t "the legacy report cuts the highlight from the joined lines"
        test_legacy_location_cut_from_joined_lines;
    ]

(*****************************************************************************)
(* Skin_simple *)
(*****************************************************************************)

(* An indented paragraph of a message fills the same width as a flush one,
   its indent taken off the width once. *)
let test_simple_indented_paragraph_fills_the_width () =
  let ctx = { (Output.skin_ctx Output.default) with Skin.width = 60 } in
  let message =
    "The first paragraph is flush left and long enough to wrap across more \
     than one line of the report.\n\n\
    \    An indented paragraph, also long enough to wrap across more than one \
     line of the report at this width.\n"
  in
  Alcotest.(check string)
    "both paragraphs run to 60 columns"
    "  The first paragraph is flush left and long enough to wrap\n\
    \  across more than one line of the report.\n\
     \n\
    \      An indented paragraph, also long enough to wrap across\n\
    \      more than one line of the report at this width.\n"
    (Fmt.str "%a" (Skin_common.pp_message ctx (Skin_simple.finding `Error)) message)

let skin_simple_tests =
  Testo.categorize "Skin_simple"
    [
      t "an indented paragraph fills the width"
        test_simple_indented_paragraph_fills_the_width;
    ]

(*****************************************************************************)
(* Skin_emit *)
(*****************************************************************************)

module M = Skin_model

let ci_env : M.Start.ci_env =
  {
    version = "1.0.0";
    ocaml_version = "5.0.0";
    environment = "git";
    event_name = "push";
  }

(* between them, every branch a skin's chunks take on the data *)
let starts : M.Start.t list =
  [
    {
      banner = true;
      features =
        [
          { name = "a feature"; description = "on"; enabled = true };
          { name = "another"; description = "off"; enabled = false };
        ];
      rule_source = M.Start.Local;
      ci = Some ci_env;
    };
    { banner = true; features = []; rule_source = M.Start.Pattern; ci = None };
  ]

let plans : M.Plan.t list =
  let plan : M.Plan.t =
    {
      num_targets = 2;
      tracked_by_git = true;
      num_rules = 1;
      num_files_with_a_rule = 1;
      num_rules_with_a_target = 1;
      languages = [ "python" ];
      lang_rows = [ { language = "python"; rules = 1; files = 1 } ];
      origin_rows = [ { origin = "local"; rules = 1 } ];
      run = M.Plan.Current;
    }
  in
  [
    plan;
    {
      plan with
      num_files_with_a_rule = 0;
      num_rules_with_a_target = 0;
      lang_rows = [];
      origin_rows = [];
      run = M.Plan.Baseline;
    };
  ]

let results : M.Result.t list =
  let phrase : M.phrase = { counts = [ (1, "file") ]; suffix = "" } in
  let tally : M.Result.tally =
    {
      rules_ran = 1;
      rules_with_findings = 1;
      files_scanned = 2;
      files_with_findings = 1;
      findings = 1;
    }
  in
  let empty : M.Summary.t =
    {
      limited = None;
      skipped = [];
      partially_analyzed = None;
      unplaced_warnings = 0;
    }
  in
  [
    {
      summary =
        {
          limited = Some "Scan was limited to files tracked by git.";
          skipped = [ phrase ];
          partially_analyzed = Some phrase;
          unplaced_warnings = 1;
        };
      tally = Some tally;
    };
    {
      summary = empty;
      tally =
        Some
          { tally with rules_with_findings = 0; files_with_findings = 0; findings = 0 };
    };
    (* 'opengrep ci' *)
    { summary = empty; tally = None };
  ]

let with_all_logs_on (f : unit -> 'a) : 'a =
  let saved =
    Logs.Src.list ()
    |> List_.map (fun (src : Logs.src) -> (src, Logs.Src.level src))
  in
  Common.protect
    ~finally:(fun () ->
      saved
      |> List.iter (fun ((src : Logs.src), (level : Logs.level option)) ->
             Logs.Src.set_level src level))
    (fun () ->
      saved
      |> List.iter (fun ((src : Logs.src), _) ->
             Logs.Src.set_level src (Some Logs.Debug));
      f ())

(* A chunk is rendered with the log mutex held, so a log call anywhere
   under it fails on the mutex. Most sources are off unless LOG_SRCS names
   them, which hides such a call from a plain --debug run; here every
   source is on.
   coupling: the contract on Skin.chunk *)
let test_chunks_render_with_all_logs_on (name : Skin.name) () =
  let module Sk = (val Skins.resolve name : Skin.S) in
  let ctx = Output.skin_ctx { Output.default with is_ci_invocation = true } in
  let chunks =
    List.concat_map (Sk.on_start ctx) starts
    @ List.concat_map (Sk.on_plan ctx) plans
    @ List.concat_map (Sk.on_result ctx) results
  in
  with_all_logs_on (fun () ->
      Testo.with_capture stderr (fun () -> Skin_emit.emit chunks) |> ignore)

let skin_emit_tests =
  Testo.categorize "Skin_emit"
    (Skin.all_names
    |> List_.map (fun ((str : string), (name : Skin.name)) ->
           t
             (spf "%s: chunks render with all logs on" str)
             (test_chunks_render_with_all_logs_on name)))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "Osemgrep reporting"
    [
      test_match_based_id_formula caps;
      findings_layout_tests;
      skin_simple_tests;
      skin_emit_tests;
    ]
