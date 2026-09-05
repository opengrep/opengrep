(* SPDX-License-Identifier: LGPL-2.1-only *)

let t = Testo.create

module F = Testutil_files
open Common
open Test_scan_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end tests of the text output, against what the Python wrapper
 * printed: where a code line is wrapped, how a message and a fix are
 * indented, the block of the scan summary, the --time report, and the
 * banner and the colours of a run that is not on a terminal.
 *
 * They assert on the captured output rather than on a snapshot, so that
 * each one states what it is about, and so that the two streams cannot
 * interleave differently from one run to the next.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* the width of a rendered line, in code points: the wrapper counted the
   characters of a line, not its bytes *)
let width : string -> int = Utf8.length

let has_escapes (output : string) : bool =
  String_.contains ~term:"\027[" output

(* the lines a run prints on [channel]; the other stream goes to the
   unchecked output of the test *)
let scan_output (caps : Scan_subcommand.caps) ~(channel : out_channel)
    ~(rule : string) ~(target : string * string) (extra_args : string list) :
    string list =
  with_env_app_token (fun () ->
      let target_name, target_content = target in
      let repo_files : F.t list =
        [ F.File ("rules.yml", rule); F.File (target_name, target_content) ]
      in
      Testutil_git.with_git_repo repo_files (fun _cwd ->
          let _exit_code, output =
            Testo.with_capture channel (fun () ->
                without_settings (fun () ->
                    Scan_subcommand.main caps
                      (Array.of_list
                         ([
                            "opengrep-scan";
                            "--experimental";
                            "--config";
                            "rules.yml";
                          ]
                         @ extra_args))))
          in
          String.split_on_char '\n' output))

let line_containing (term : string) (lines : string list) : string =
  match List.filter (fun (line : string) -> String_.contains ~term line) lines with
  | line :: _ -> line
  | [] -> Alcotest.fail (spf "no line containing %S" term)

(*****************************************************************************)
(* Fixtures *)
(*****************************************************************************)

let eqeq_rule =
  {|
rules:
  - id: eqeq
    pattern: $X == $X
    message: eq
    languages: [python]
    severity: ERROR
|}

(* a rule whose message is written as a block, with two paragraphs *)
let paragraphs_rule =
  {|
rules:
  - id: paragraphs
    pattern: $X == $X
    message: |
      The first line of the first paragraph.
      The second line of the same paragraph.

      A second paragraph.
    languages: [python]
    severity: ERROR
|}

let autofix_rule =
  {|
rules:
  - id: autofix
    pattern: $X == $X
    fix: $X.equals($X)
    message: fix
    languages: [python]
    severity: ERROR
|}

(* two rules of the same language, so that the status line replaces the
   Language and Origin tables *)
let two_python_rules =
  {|
rules:
  - id: eqeq
    pattern: $X == $X
    message: eq
    languages: [python]
    severity: ERROR
  - id: print
    pattern: print(...)
    message: print
    languages: [python]
    severity: WARNING
|}

let stupid_py = ("stupid.py", "x = 1 == 1\n")

(* a tab inside a code line, which the dedent of the snippet keeps *)
let tab_py = ("tab.py", "x = 1\t== 1\n")

(* two messages the hyphen rules of the filler tell apart: a hyphenated
   word that straddles the end of a line, and a word too long for a line
   of its own whose hyphens are preceded by digits *)
let hyphens_rule : string =
  let words =
    String.concat " " (List.init 19 (fun (i : int) -> spf "w%03d" i))
  in
  let long = String.concat "" (List.init 30 (fun (_ : int) -> "abc123-")) in
  spf
    {|
rules:
  - id: straddling
    pattern: $X == $X
    message: '%s log4j-scanner tail'
    languages: [python]
    severity: ERROR
  - id: longword
    pattern: $X == $X
    message: 'aaaa bbbb %s end'
    languages: [python]
    severity: ERROR
|}
    words long

(* a line that has to be wrapped, whose characters take more than one byte *)
let unicode_py =
  let repeat (n : int) (s : string) : string =
    String.concat "" (List.init n (fun (_ : int) -> s))
  in
  let text = repeat 10 "日本語 café " in
  ("unicode.py", spf "x = \"%s\" == \"%s\"\n" text text)

(*****************************************************************************)
(* Individual tests *)
(*****************************************************************************)

(* python: text.py format_finding_line(), textwrap.fill() counting code
   points: a wrapped line keeps its characters whole and the rendered line
   is bounded by --max-chars-per-line plus the two columns of the console *)
let test_wrapping (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stdout ~rule:eqeq_rule ~target:unicode_py
      [ "--max-chars-per-line"; "40" ]
  in
  let code_lines =
    List.filter (fun (line : string) -> String_.contains ~term:"日本語" line) lines
  in
  Alcotest.(check bool)
    "the line was wrapped" true
    (List.length code_lines > 1);
  List.iter
    (fun (line : string) ->
      Alcotest.(check bool)
        (spf "no character was cut in half in %S" line)
        true (Utf8.is_valid line);
      Alcotest.(check bool)
        (spf "%S is at most 42 columns" line)
        true
        (width line <= 42))
    code_lines;
  (* the code starts at the same column on every line of the finding: the
     number of the first one, the width of that prefix on the others *)
  Alcotest.(check bool)
    "the first line carries the line number" true
    (String_.contains ~term:"            1┆ x = " (List.nth code_lines 0));
  List.iter
    (fun (line : string) ->
      Alcotest.(check string)
        "a wrapped line is indented to the code"
        "               "
        (Str.first_chars line 15))
    (List.tl code_lines)

(* python: TextWrapper._munge_whitespace, which expanded the tabs of the
   whole line, its number included, before wrapping it *)
let test_code_line_with_a_tab (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stdout ~rule:eqeq_rule ~target:tab_py []
  in
  Alcotest.(check string)
    "the tab reaches the next multiple of eight columns"
    "            1┆ x = 1      == 1"
    (line_containing "x = 1" lines)

(* python: the message went through click.wrap_text, whose chunks end on a
   hyphen only between letters and whose long words are cut at the width *)
let test_message_hyphens (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stdout ~rule:hyphens_rule ~target:stupid_py []
  in
  let ends_with (suffix : string) (line : string) : bool =
    String.length line >= String.length suffix
    && String.equal (Str.last_chars line (String.length suffix)) suffix
  in
  Alcotest.(check bool)
    "a digit before the hyphen keeps the word in one piece" false
    (List.exists (ends_with "log4j-") lines);
  Alcotest.(check bool)
    "the word moves whole to the next line" true
    (List.exists
       (fun (line : string) ->
         String.equal (String.trim line) "log4j-scanner tail")
       lines);
  Alcotest.(check bool)
    "a word too long for a line is cut at the width, not after a hyphen"
    true
    (List.exists (ends_with "abc123") lines)

(* python: click.wrap_text(preserve_paragraphs=True), which joined the
   lines of a paragraph and indented every one of them *)
let test_message_paragraphs (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stdout ~rule:paragraphs_rule ~target:stupid_py []
  in
  let message_lines =
    List.filter
      (fun (line : string) ->
        String_.contains ~term:"line of the" line
        || String_.contains ~term:"same paragraph" line
        || String_.contains ~term:"A second paragraph." line)
      lines
  in
  List.iter
    (fun (line : string) ->
      Alcotest.(check string)
        (spf "%S is indented by ten columns" line)
        "          "
        (Str.first_chars line 10))
    message_lines;
  (* the two lines of the first paragraph are filled as one *)
  Alcotest.(check bool)
    "the lines of a paragraph are filled as one" true
    (String_.contains
       ~term:"The first line of the first paragraph. The second line"
       (String.concat " " (List_.map String.trim message_lines)));
  (* python: paragraphs are joined with a blank line *)
  Alcotest.(check bool)
    "a blank line between the paragraphs" true
    (let rec blank_before (lines : string list) : bool =
       match lines with
       | previous :: line :: _
         when String_.contains ~term:"A second paragraph." line ->
           String.equal previous ""
       | _ :: rest -> blank_before rest
       | [] -> false
     in
     blank_before lines)

(* python: text.py, the fix printed after (BASE_INDENT + 1) columns plus
   the two of the console *)
let test_autofix_indent (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stdout ~rule:autofix_rule ~target:stupid_py []
  in
  Alcotest.(check string)
    "the autofix line" "           ▶▶┆ Autofix ▶ 1.equals(1)"
    (line_containing "Autofix" lines)

(* python: target_manager.py, the block of the summary: the header above
   every fragment, the fragments indented, and no full stop *)
let test_summary_block (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stderr ~rule:eqeq_rule ~target:stupid_py
      [ "--exclude"; "*.py" ]
  in
  let rec block (lines : string list) : string list =
    match lines with
    | [] -> Alcotest.fail "no summary block"
    | line :: rest
      when String.equal line "Some files were skipped or only partially analyzed."
      ->
        List_.take_safe 3 rest
    | _ :: rest -> block rest
  in
  Alcotest.(check (list string))
    "the fragments of the block"
    [
      "  Scan was limited to files tracked by git.";
      "  Scan skipped: 1 files matching --exclude patterns";
      "  For a full list of skipped files, run opengrep with the --verbose \
       flag.";
    ]
    (block lines);
  (* python: output.py, executed_rule_count or len(filtered_rules) *)
  Alcotest.(check string)
    "the rules of a run where none had a target"
    "Ran 1 rule on 0 files: 0 findings." (line_containing "Ran " lines)

(* python: scan_report.py _print_sast_table(), one line instead of the
   Language and Origin tables when a single language is scanned *)
let test_status_one_language (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stderr ~rule:two_python_rules ~target:stupid_py []
  in
  Alcotest.(check string)
    "the files and rules of the only language"
    "  Scanning 1 file with 2 python rules."
    (line_containing "Scanning 1 file with" lines);
  Alcotest.(check int)
    "no Language table" 0
    (List.length
       (List.filter
          (fun (line : string) -> String_.contains ~term:"Language" line)
          lines))

(* python: main.py conditional_welcome(), which printed nothing when
   stdout was not a terminal *)
let test_no_banner_off_a_terminal (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stderr ~rule:eqeq_rule ~target:stupid_py []
  in
  List.iter
    (fun (term : string) ->
      Alcotest.(check int)
        (spf "no %S line" term)
        0
        (List.length
           (List.filter (fun (line : string) -> String_.contains ~term line) lines)))
    [ "Opengrep CLI"; "Opengrep OSS"; "Loading rules from" ]

(* python: text.py print_time_summary(), the block indented by the console
   and the size the engine reports for every file it lists *)
let test_time_report (caps : Scan_subcommand.caps) () =
  let lines =
    scan_output caps ~channel:stdout ~rule:eqeq_rule ~target:stupid_py
      [ "--time" ]
  in
  let block =
    let rec after (lines : string list) : string list =
      match lines with
      | [] -> Alcotest.fail "no time summary"
      | line :: rest when String_.contains ~term:"[ summary ]" line -> line :: rest
      | _ :: rest -> after rest
    in
    after lines
  in
  Alcotest.(check bool)
    "the label of the engine times" true
    (List.exists
       (fun (line : string) -> String.equal line "  Engine time:")
       block);
  List.iter
    (fun (line : string) ->
      Alcotest.(check bool)
        (spf "%S is indented by two columns" line)
        true
        (String.equal line ""
        || String.equal (Str.first_chars line 2) "  "))
    block;
  (* the size is the engine's num_bytes, taken from the content it read;
     the report makes no file-system call, so the file listed among the
     slowest and the language total report the same number *)
  let size_in (line : string) : string =
    match String.index_opt line '(' with
    | None -> Alcotest.fail (spf "no size in %S" line)
    | Some i -> Str.string_before (Str.string_after line (i + 1)) 4
  in
  Alcotest.(check string)
    "the size of the slowest file is the engine's"
    (size_in (line_containing "Analyzed:" block))
    (size_in (line_containing "stupid.py" block))

(* python: terminal.py, which turned colour off when the output is not a
   terminal and on for --force-color; 'validate' goes through the same
   precedence as a scan, where it used to force colour *)
let test_colour_precedence (caps : Scan_subcommand.caps) () =
  (* a rule file that both modes have something to say about: one rule to
     run, and one whose missing 'languages' is worth a warning *)
  let one_rule_and_a_warning =
    {|
rules:
  - id: eqeq
    pattern: $X == $X
    message: eq
    languages: [python]
    severity: ERROR
  - id: nolangs
    pattern: $X == $X
    message: m
    severity: ERROR
|}
  in
  let run (extra_args : string list) : string =
    String.concat "\n"
      (scan_output caps ~channel:stderr ~rule:one_rule_and_a_warning
         ~target:stupid_py extra_args)
  in
  Alcotest.(check bool)
    "no colour off a terminal" false
    (has_escapes (run []));
  Alcotest.(check bool)
    "colour with --force-color" true
    (has_escapes (run [ "--force-color" ]));
  (* 'validate' and 'test' take the flag rather than forcing colour on *)
  List.iter
    (fun ((argv : string array), (forced : bool)) ->
      Alcotest.(check bool)
        (spf "the colour of %s" (String.concat " " (Array.to_list argv)))
        forced
        (Validate_CLI.parse_argv argv).force_color)
    [
      ([| "opengrep-validate"; "rules.yml" |], false);
      ([| "opengrep-validate"; "--force-color"; "rules.yml" |], true);
    ];
  List.iter
    (fun ((argv : string array), (forced : bool)) ->
      Alcotest.(check bool)
        (spf "the colour of %s" (String.concat " " (Array.to_list argv)))
        forced
        (Test_CLI.parse_argv argv).force_color)
    [
      ([| "opengrep-test"; "--config"; "rules.yml"; "." |], false);
      ( [| "opengrep-test"; "--force-color"; "--config"; "rules.yml"; "." |],
        true );
    ]

(* python: terminal.py, os.environ.get("NO_COLOR") is not None; the branch
   treats an empty value as unset, like every other variable it reads *)
let test_no_color_any_value () =
  List.iter
    (fun (value : string) ->
      Alcotest.(check bool)
        (spf "NO_COLOR=%s turns colour off" value)
        true
        (Semgrep_envvars.with_envvar "NO_COLOR" value (fun () ->
             !Semgrep_envvars.v.no_color)))
    [ "1"; "x"; "always"; "false" ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* A config of INVENTORY rules only: the target is scanned by no rule, and
   the summary says so. python: test_inventory_finding_output *)
let test_inventory_rule_text (caps : Scan_subcommand.caps) () =
  let inventory_rule =
    {|
rules:
  - id: inventory
    pattern: $X == $X
    message: inventory
    languages: [python]
    severity: INVENTORY
|}
  in
  let lines =
    scan_output caps ~channel:stderr ~rule:inventory_rule ~target:stupid_py
      [ "--verbose"; fst stupid_py ]
  in
  Alcotest.(check string)
    "the rule not run" "1 rule of severity INVENTORY or EXPERIMENT not run"
    (line_containing "not run" lines |> String.trim
    |> Str.replace_first (Str.regexp "^.*INFO\\]: ") "");
  Alcotest.(check string)
    "the status line" "Scanning 1 file tracked by git with 0 Code rules:"
    (String.trim (line_containing "Scanning" lines));
  Alcotest.(check string)
    "the summary" "Ran 0 rules on 1 file: 0 findings."
    (String.trim (line_containing "Ran " lines))

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan text output (e2e)"
    [
      t "a wrapped code line keeps its characters whole"
        (test_wrapping (caps :> Scan_subcommand.caps));
      t "a tab in a code line is expanded before wrapping"
        (test_code_line_with_a_tab (caps :> Scan_subcommand.caps));
      t "the hyphens of a message follow click's rules"
        (test_message_hyphens (caps :> Scan_subcommand.caps));
      t "the paragraphs of a message are filled and indented"
        (test_message_paragraphs (caps :> Scan_subcommand.caps));
      t "the autofix line is indented like the wrapper's"
        (test_autofix_indent (caps :> Scan_subcommand.caps));
      t "the block of the scan summary"
        (test_summary_block (caps :> Scan_subcommand.caps));
      t "the scan status of a single language"
        (test_status_one_language (caps :> Scan_subcommand.caps));
      t "no banner when stdout is not a terminal"
        (test_no_banner_off_a_terminal (caps :> Scan_subcommand.caps));
      t "the --time report" (test_time_report (caps :> Scan_subcommand.caps));
      t "colour precedence, for validate and test too"
        (test_colour_precedence (caps :> Scan_subcommand.caps));
      t "NO_COLOR turns colour off whatever its value" test_no_color_any_value;
      t "INVENTORY rules only: the target is scanned by no rule"
        (test_inventory_rule_text (caps :> Scan_subcommand.caps));
    ]
