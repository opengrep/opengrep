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
(* Entry point *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "Osemgrep reporting" [ test_match_based_id_formula caps ]
