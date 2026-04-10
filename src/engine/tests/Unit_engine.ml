open Common
open Fpath_.Operators
module R = Rule
module MR = Mini_rule
module PM = Core_match
module E = Core_error
module Out = Semgrep_output_v1_t
module TCM = Test_compare_matches

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Tests exercising the engine.
 *
 * Some of those tests exercise just the semgrep patterns part (with the
 * .sgrep), and not the whole rule part (with the .yaml). We could move
 * them in matching/Unit_matcher.ml but matching/ does not depend
 * on parsing/, so it's simpler to put those tests here.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type fix_type =
  | Fix of string
  | FixRegex of (* regex *) string * int option * (* replacement *) string
  | NoFix

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* TODO: move these to the "main" for the test suite. *)
(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"
let tests_path_patterns = tests_path / "patterns"
let tests_path_autofix = tests_path / "autofix"
let polyglot_pattern_path = tests_path_patterns / "POLYGLOT"

(* TODO: infer dir and ext from lang using Lang helper functions *)
let full_lang_info =
  [
    (Lang.Bash, "bash", ".bash");
    (Lang.C, "c", ".c");
    (Lang.Cairo, "cairo", ".cairo");
    (Lang.Clojure, "clojure", ".clj");
    (Lang.Cpp, "cpp", ".cpp");
    (Lang.Csharp, "csharp", ".cs");
    (Lang.Dart, "dart", ".dart");
    (Lang.Dockerfile, "dockerfile", ".dockerfile");
    (Lang.Elixir, "elixir", ".ex");
    (Lang.Go, "go", ".go");
    (Lang.Hack, "hack", ".hack");
    (Lang.Html, "html", ".html");
    (Lang.Java, "java", ".java");
    (Lang.Js, "js", ".js");
    (Lang.Json, "json", ".json");
    (Lang.Jsonnet, "jsonnet", ".jsonnet");
    (Lang.Julia, "julia", ".jl");
    (Lang.Kotlin, "kotlin", ".kt");
    (Lang.Lua, "lua", ".lua");
    (Lang.Move_on_aptos, "move_on_aptos", ".move");
    (Lang.Move_on_sui, "move_on_sui", ".move");
    (Lang.Ocaml, "ocaml", ".ml");
    (Lang.Php, "php", ".php");
    (Lang.Promql, "promql", ".promql");
    (Lang.Python, "python", ".py");
    (Lang.Ql, "ql", ".ql");
    (Lang.R, "r", ".r");
    (Lang.Ruby, "ruby", ".rb");
    (Lang.Rust, "rust", ".rs");
    (Lang.Scala, "scala", ".scala");
    (Lang.Solidity, "solidity", ".sol");
    (Lang.Swift, "swift", ".swift");
    (Lang.Terraform, "terraform", ".tf");
    (Lang.Ts, "ts", ".ts");
    (Lang.Vb, "vb", ".vb");
    (Lang.Xml, "xml", ".xml");
    (Lang.Yaml, "yaml", ".yaml");
  ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* lang_test_fn should:
   Generate a test suite for a list of languages.
   Two main folders are expected:
   - test_pattern_path: folder containing one subfolder per language
   - polyglot_pattern_path: folder containing patterns shared by multiple
     languages.

   Each language is specified as a triple:
   - language of type Lang.t e.g. Lang.Ruby
   - subdirectory containing the test cases e.g. "ruby"
   - language extension of the target files e.g. ".rb"

   Each language folder contains pairs of files:
   - foo.rb: target file for the test case "foo".
   - foo.sgrep: target file for the test case "foo". If missing, it must
     exist in the polyglot folder.
*)
let pack_tests_for_lang
    ~(lang_test_fn :
       polyglot_pattern_path:Fpath.t ->
       Fpath.t list ->
       Language.t ->
       Testo.t list) ~test_pattern_path ~polyglot_pattern_path lang dir ext =
  Testo.categorize
    (spf "%s" (Lang.show lang))
    (let dir = test_pattern_path / dir in
     let files = Common2.glob (spf "%s/*%s" !!dir ext) |> Fpath_.of_strings in

     lang_test_fn ~polyglot_pattern_path files lang)

(*****************************************************************************)
(* Maturity tests *)
(*****************************************************************************)

(* coupling: https://semgrep.dev/docs/language-support/
 * See also https://r2c.quip.com/FOAuA4ThzULc/How-to-promote-a-language-
 *)
type maturity_level = GA | Beta | Experimental
[@@deriving show { with_path = false }]

(* coupling:
 * https://semgrep.dev/docs/language-support/#maturity-definitions
 * ../../scripts/generate-cheatsheet.py
 *)
let experimental_features =
  [
    "concrete_syntax";
    "deep_exprstmt";
    "dots_args";
    "dots_nested_stmts";
    "dots_stmts";
    "dots_string";
    "metavar_arg";
    "metavar_call";
    "metavar_equality_var";
  ]

let beta_features =
  experimental_features
  @ [
      "metavar_class_def";
      "metavar_func_def";
      "metavar_cond";
      "metavar_equality_expr";
      "metavar_equality_stmt";
      "metavar_import";
      "metavar_stmt";
    ]

let ga_features =
  beta_features
  @ [
      "deep_expr_operator";
      "dots_method_chaining";
      "dots_params";
      "equivalence_constant_propagation";
      "equivalence_naming_import";
      "metavar_anno";
      "metavar_key_value";
      "metavar_typed";
      "metavar_ellipsis_args";
      (* TODO: metavar_ellipsis_params *)
      (* TODO: metavar_string? *)
      "regexp_string";
    ]

let assoc_maturity_level =
  [
    (GA, ga_features);
    (Beta, beta_features);
    (Experimental, experimental_features);
  ]

(* coupling: ../../scripts/generate_cheatsheet.py LANGUAGE_EXCEPTIONS
 * Note that for some languages, e.g., JSON, certain tests do not
 * apply (NA), hence the exceptions listed above.
 * For others, we should really add the test and/or corresponding feature.
 *)
let language_exceptions =
  [
    (* GA languages *)

    (* TODO: why not regexp_string? NA for naming_import? *)
    ( Lang.Csharp,
      [ "equivalence_naming_import"; "regexp_string"; "dots_params" ] );
    (* TODO: metavar_anno sounds like an NA, but the other?? *)
    (Lang.Go, [ "metavar_class_def"; "metavar_import"; "metavar_anno" ]);
    (* TODO: NA for Java? *)
    (Lang.Java, [ "equivalence_naming_import"; "metavar_key_value" ]);
    (* metavar_typed is NA (dynamic language) *)
    (Lang.Js, [ "equivalence_naming_import"; "metavar_typed" ]);
    ( Lang.Ts,
      [
        "equivalence_naming_import";
        "metavar_typed";
        "metavar_anno";
        "metavar_class_def";
      ] );
    ( Lang.Php,
      [
        "equivalence_naming_import";
        "metavar_key_value";
        "metavar_typed";
        "dots_params";
      ] );
    (* good boy, metavar_typed is working just for constants though *)
    (Lang.Python, []);
    (* metavar_typed is NA (dynamic language), metavar_anno also NA? *)
    ( Lang.Ruby,
      [
        "equivalence_naming_import";
        "metavar_typed";
        "metavar_anno";
        "dots_params";
      ] );
    (* regexp_string feature has been deprecated *)
    (Lang.Scala, [ "regexp_string"; "metavar_ellipsis_args"; "dots_params" ]);
    (* Beta languages *)

    (* TODO: to fix *)
    (Lang.Kotlin, [ "dots_stmts"; "metavar_equality_var" ]);
    (* good boy *)
    (Lang.Rust, []);
    (Lang.Move_on_sui, [ "metavar_key_value"; "regexp_string" ]);
    (Lang.Move_on_aptos, [ "metavar_key_value"; "regexp_string" ]);
    (* Experimental languages *)

    (* TODO: dots_nested_stmts to fix for C and C++ *)
    (Lang.C, [ "dots_nested_stmts" ]);
    (Lang.Cpp, [ "dots_nested_stmts" ]);
    (* good boy *)
    (Lang.Lua, []);
    (* dots_stmts is maybe NA, same with deep_exprstmt *)
    (Lang.Ocaml, [ "deep_exprstmt"; "dots_stmts" ]);
    (* Experimental languages *)
    (Lang.R, [ "deep_exprstmt" ]);
    (* xxx_stmts is NA *)
    (Lang.Jsonnet, [ "dots_stmts"; "deep_exprstmt"; "dots_nested_stmts" ]);
    (* TODO *)
    (Lang.Clojure, [ "deep_exprstmt"; "dots_nested_stmts" ]);
  ]

(* TODO: infer dir and ext from lang using Lang helper functions *)
let make_maturity_tests ?(lang_exn = language_exceptions) lang dir ext maturity
    =
  Testo.categorize
    (spf "Maturity %s for %s" (show_maturity_level maturity) (Lang.show lang))
    (let dir = tests_path_patterns / dir in
     let features = assoc_maturity_level |> List.assoc maturity in
     let exns =
       try List.assoc lang lang_exn with
       | Not_found -> []
     in
     (* sanity check exns *)
     exns
     |> List.iter (fun base ->
            let path = dir / (base ^ ext) in
            if Sys.file_exists !!path then
              failwith
                (spf "%s actually exist! remove it from exceptions" !!path));
     let features = Common2.minus_set features exns in
     features
     |> List_.map (fun base ->
            Testo.create ~tags:(Test_tags.tags_of_lang lang) base (fun () ->
                let path = dir / (base ^ ext) in
                (* if it's a does-not-apply (NA) case, consider adding it
                 * to language_exceptions above
                 *)
                if not (Sys.file_exists !!path) then
                  failwith
                    (spf "missing test file %s for maturity %s" !!path
                       (show_maturity_level maturity)))))

let maturity_tests () =
  (* coupling: https://semgrep.dev/docs/language-support/ *)
  Testo.categorize_suites "Maturity levels"
    [
      (* GA *)
      make_maturity_tests Lang.Csharp "csharp" ".cs" GA;
      make_maturity_tests Lang.Go "go" ".go" GA;
      make_maturity_tests Lang.Java "java" ".java" GA;
      make_maturity_tests Lang.Js "js" ".js" GA;
      (* JSON has too many NA, not worth it *)
      make_maturity_tests Lang.Php "php" ".php" GA;
      make_maturity_tests Lang.Python "python" ".py" GA;
      make_maturity_tests Lang.Ruby "ruby" ".rb" GA;
      make_maturity_tests Lang.Ts "ts" ".ts" GA;
      make_maturity_tests Lang.Scala "scala" ".scala" GA;
      (* Beta *)
      make_maturity_tests Lang.Hack "hack" ".hack" Beta;
      make_maturity_tests Lang.Kotlin "kotlin" ".kt" Beta;
      make_maturity_tests Lang.Rust "rust" ".rs" Beta;
      make_maturity_tests Lang.Move_on_sui "move_on_sui" ".move" Beta;
      make_maturity_tests Lang.Move_on_aptos "move_on_aptos" ".move" Beta;
      (* Terraform/HCL has too many NA, not worth it *)

      (* Experimental *)
      make_maturity_tests Lang.Bash "bash" ".bash" Experimental;
      make_maturity_tests Lang.C "c" ".c" Experimental;
      make_maturity_tests Lang.Cpp "cpp" ".cpp" Experimental;
      (* TODO
         make_maturity_tests Lang.Dockerfile "dockerfile" ".dockerfile" Experimental;
      *)
      make_maturity_tests Lang.Lua "lua" ".lua" Experimental;
      make_maturity_tests Lang.Ocaml "ocaml" ".ml" Experimental;
      make_maturity_tests Lang.R "r" ".r" Experimental;
      make_maturity_tests Lang.Solidity "solidity" ".sol" Experimental;
      make_maturity_tests Lang.Swift "swift" ".swift" Experimental;
      make_maturity_tests Lang.Julia "julia" ".jl" Experimental;
      (* YAML has too many NA, not worth it *)
      make_maturity_tests Lang.Jsonnet "jsonnet" ".jsonnet" Experimental;
      make_maturity_tests Lang.Clojure "clojure" ".clj" Experimental
      (* Not even experimental yet *)
      (* HTML, XML, Dart *);
    ]

(*****************************************************************************)
(* Language-specific tests *)
(*****************************************************************************)

let match_pattern ~lang ~hook ~file ~pattern ~fix =
  (* TODO? enable the "semgrep.parsing" src level maybe here *)
  let pattern =
    match Parse_pattern.parse_pattern lang pattern with
    | Ok pat -> pat
    | Error s ->
        failwith
          (spf "fail to parse pattern `%s` with lang = %s: %s" pattern
             (Lang.to_string lang) s)
    | exception exn ->
        failwith
          (spf "fail to parse pattern `%s` with lang = %s (exn = %s)" pattern
             (Lang.to_string lang) (Common.exn_to_s exn))
  in
  let fix, fix_regexp =
    match fix with
    | NoFix -> (None, None)
    | Fix s -> (Some s, None)
    | FixRegex (regexp, count, replacement) ->
        (None, Some Rule.{ regexp; count; replacement })
  in
  let rule =
    {
      MR.id = Rule_ID.of_string_exn "unit-testing";
      pattern;
      inside = false;
      message = "";
      metadata = None;
      severity = `Error;
      langs = [ lang ];
      pattern_string = "test: no need for pattern string";
      fix;
      fix_regexp;
    }
  in
  let ast =
    try Parse_target.parse_and_resolve_name_fail_if_partial lang file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let equiv = [] in
  Match_patterns.check ~hook
    (Rule_options.default, equiv)
    [ rule ]
    (file, File file, lang, ast)

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
let regression_tests_for_lang ~polyglot_pattern_path files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang) (Fpath.basename file)
           (fun () ->
             let sgrep_file =
               match
                 Test_utils.related_file_of_target ~polyglot_pattern_path
                   ~ext:"sgrep" file
               with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let pattern = UFile.read_file sgrep_file in

             (* old: semgrep-core used to support user-defined
                * equivalences, but the feature has been now deprecated.
                *
                * (* Python == is not the same than !(==) *)
                * if lang <> Lang.Python then
                *   Parse_equivalences.parse
                *     (Filename.concat data_path "basic_equivalences.yml")
                * else []
             *)
             let matches = ref [] in
             match_pattern ~lang
               ~hook:(fun pm -> Stack_.push (TCM.location_of_pm pm) matches)
               ~file ~pattern ~fix:NoFix
             |> ignore;
             let actual = !matches in
             let expected = TCM.expected_error_lines_of_files [ file ] in
             TCM.compare_actual_to_expected_for_alcotest ~to_location:Fun.id
               actual expected))

(* used in Unit_pro_languages.ml *)
let make_lang_regression_tests ~test_pattern_path ~polyglot_pattern_path
    lang_data =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let lang_tests =
    lang_data
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:regression_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  Testo.categorize_suites "sgrep patterns" lang_tests

let lang_regression_tests ~polyglot_pattern_path =
  let test_pattern_path = tests_path_patterns in
  let regular_tests =
    full_lang_info
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:regression_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  let irregular_tests =
    [
      Testo.categorize "Typescript on Javascript (no JSX)"
        (let dir = test_pattern_path / "js" in
         let files = Common2.glob (spf "%s/*.js" !!dir) in
         let files =
           List_.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files
           |> Fpath_.of_strings
         in

         let lang = Lang.Ts in
         regression_tests_for_lang ~polyglot_pattern_path files lang);
      Testo.categorize "C++ on C tests"
        (let dir = test_pattern_path / "c" in
         let files = Common2.glob (spf "%s/*.c" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Cpp in
         regression_tests_for_lang ~polyglot_pattern_path files lang);
    ]
  in
  Testo.categorize_suites "sgrep patterns" (regular_tests @ irregular_tests)

(*****************************************************************************)
(* Autofix tests *)
(*****************************************************************************)

let autofix_tests_for_lang ~polyglot_pattern_path files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang) (Fpath.basename file)
           (fun () ->
             let sgrep_file =
               match
                 Test_utils.related_file_of_target ~polyglot_pattern_path
                   ~ext:"sgrep" file
               with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let pattern = UFile.read_file sgrep_file in
             let fix =
               match
                 Test_utils.related_file_of_target ~polyglot_pattern_path
                   ~ext:"fix" file
               with
               | Ok fix_file -> Fix (UFile.read_file fix_file)
               | Error _ -> (
                   (* A poor man's configuration format.
                      This can either be two lines, the regex to match
                      and the replacement content (one line),
                      or 3+ lines, the regex to match, the number of matches
                      to replace, and the replacement text (possibly multiline)
                   *)
                   match
                     Test_utils.related_file_of_target ~polyglot_pattern_path
                       ~ext:"fix-regex" file
                   with
                   | Ok fix_regex_file -> (
                       match UFile.cat fix_regex_file with
                       | [ l1; l2 ] -> FixRegex (l1, None, l2)
                       | l1 :: l2 :: l3 :: rest ->
                           FixRegex
                             ( l1,
                               Some (int_of_string l2),
                               String.concat "\n" (l3 :: rest) )
                       | _ ->
                           failwith
                             (Common.spf
                                "found fix-regex file %s with invalid number \
                                 of lines"
                                (Fpath.to_string fix_regex_file)))
                   | Error _ ->
                       failwith
                         (Common.spf "no fix file found for autofix test %s"
                            (Fpath.to_string file)))
             in

             let matches =
               match_pattern ~lang ~hook:(fun _ -> ()) ~file ~pattern ~fix
             in
             match fix with
             | NoFix -> ()
             | _ ->
                 Test_utils.compare_fixes ~polyglot_pattern_path ~file matches))

let lang_autofix_tests ~polyglot_pattern_path =
  let test_pattern_path = tests_path_autofix in
  let lang_tests =
    full_lang_info
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:autofix_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  Testo.categorize_suites "autofix" lang_tests

(*****************************************************************************)
(* Eval_generic tests *)
(*****************************************************************************)

let eval_regression_tests () =
  [
    t "Eval_generic" (fun () ->
        let dir = tests_path / "eval" in
        let files = Common2.glob (spf "%s/*.json" !!dir) in
        files
        |> List.iter (fun file ->
               let env, code = Eval_generic.parse_json file in
               let res = Eval_generic.eval env code in
               Alcotest.(check bool)
                 (spf "%s should evaluate to true" file)
                 true
                 (Eval_generic.Bool true =*= res)));
  ]

(*****************************************************************************)
(* Analyze_rule (filter irrelevant rules) tests *)
(*****************************************************************************)

let cache = Domain.DLS.new_key (fun () -> Hashtbl.create 101)

let test_irrelevant_rule rule_file target_file =
  (* Reset domain cache. *)
  let _ = Domain.DLS.set cache (Hashtbl.create 101) in
  let cache = Some cache in
  (* TODO: fail more gracefully for invalid rules? *)
  let rules = Parse_rule.parse rule_file |> Result.get_ok in
  rules
  |> List.iter (fun rule ->
         match Analyze_rule.regexp_prefilter_of_rule ~cache rule with
         | None ->
             Alcotest.fail
               (spf "Rule %s: no regex prefilter formula"
                  (Rule_ID.to_string (fst rule.id)))
         | Some (f, func) ->
             let content = UFile.read_file target_file in
             let s = Semgrep_prefilter_j.string_of_formula f in
             if func content then
               Alcotest.fail
                 (spf "Rule %s considered relevant by regex prefilter: %s"
                    (Rule_ID.to_string (fst rule.id))
                    s))

let test_irrelevant_rule_file target_file =
  t (Fpath.basename target_file) (fun () ->
      let rules_file =
        let d, b, _e = Filename_.dbe_of_filename !!target_file in
        let candidate1 = Filename_.filename_of_dbe (d, b, "yaml") in
        if Sys.file_exists candidate1 then Fpath.v candidate1
        else
          failwith
            (spf "could not find target file for irrelevant rule %s"
               !!target_file)
      in
      test_irrelevant_rule rules_file target_file)

(* These tests test that semgrep with filter_irrelevant_rules correctly
   does not run files when they lack necessary strings.

   To test that filter_irrelevant_rules does not mistakenly filter out
   any files, place the rule/target pair in the rules folder but annotate
   in a comment that the test targets filter_irrelevant_rules to help
   future debuggers. *)
let filter_irrelevant_rules_tests () =
  Testo.categorize "filter irrelevant rules"
    (let dir = tests_path / "irrelevant_rules" in
     let target_files =
       Common2.glob (spf "%s/*" !!dir)
       |> Fpath_.of_strings
       |> File_type.files_of_dirs_or_files (function
            | File_type.Config File_type.Yaml -> false
            | _ -> true (* TODO include .test.yaml*))
     in
     target_files
     |> List_.map (fun target_file -> test_irrelevant_rule_file target_file))

(*****************************************************************************)
(* Tainting tests *)
(*****************************************************************************)

let interfile_taint_tests () =
  let write_interfile_rule ?(extra_sections = "") rule_file =
    UFile.write_file rule_file
      ({|rules:
- id: interfile-python
  languages:
    - python
  severity: ERROR
  message: Interfile taint
  mode: taint
  options:
    interfile: true
  pattern-sources:
    - pattern: tainted(...)
  pattern-sinks:
    - pattern: sink(...)
|}
      ^ extra_sections)
  in
  let parse_taint_rule rule_file =
    match Parse_rule.parse rule_file |> Result.get_ok with
    | [ ({ R.mode = `Taint _ as mode; _ } as rule) ] -> { rule with mode }
    | _ -> Alcotest.fail "expected a single taint rule"
  in
  let mk_interfile_checker ?(rule_extra_sections = "") root files =
    let _ = Domain.DLS.set cache (Hashtbl.create 101) in
    let rule_file = root / "rule.yaml" in
    write_interfile_rule ~extra_sections:rule_extra_sections rule_file;
    let rule = parse_taint_rule rule_file in
    let xlang = Xlang.of_lang Lang.Python in
    let xtarget_of_file = Test_engine.xtarget_of_file xlang in
    let xtargets = files |> List_.map xtarget_of_file in
    let xconf =
      {
        Match_env.default_xconfig with
        filter_irrelevant_rules = Match_env.PrefilterWithCache cache;
      }
    in
    let interfile_context =
      Match_tainting_mode.build_interfile_contexts xconf [ (rule, xtargets) ]
    in
    fun file ->
      let xtarget = xtarget_of_file file in
      Match_rules.check ~match_hook:(fun _pm -> ()) ~timeout:None
        ~interfile_context xconf [ (rule :> R.rule) ] xtarget
  in
  let match_locations (check_file : Fpath.t -> Core_result.matches_single_file)
      file =
    let (res : Core_result.matches_single_file) = check_file file in
    res.matches |> List_.map TCM.location_of_pm
  in
  let check_single_match ~name ~file ~line matches =
    Alcotest.(check int) (spf "one finding for %s" name) 1 (List.length matches);
    let actual_file, actual_line = List.hd matches in
    Alcotest.(check bool) (spf "match is reported on %s" name) true
      (Fpath.equal file actual_file);
    Alcotest.(check int) (spf "match line is correct for %s" name) line
      actual_line
  in
  let check_match_lines ~name ~file ~lines matches =
    Alcotest.(check int)
      (spf "expected number of findings for %s" name)
      (List.length lines) (List.length matches);
    let actual_lines =
      matches
      |> List_.map (fun (actual_file, actual_line) ->
             Alcotest.(check bool) (spf "match is reported on %s" name) true
               (Fpath.equal file actual_file);
             actual_line)
      |> List.sort compare
    in
    Alcotest.(check (list int)) (spf "match lines are correct for %s" name)
      (List.sort compare lines) actual_lines
  in
  let check_no_matches ~name matches =
    Alcotest.(check int) (spf "no findings for %s" name) 0 (List.length matches)
  in
  [
    t "interfile taint across direct imported sources" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file app_file
              {|from source import source

def run():
    sink(source())  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the direct imported source sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across imported module-level values" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from source import payload

def run():
    sink(payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the imported module-level value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across directly aliased imported module-level values"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from source import payload as imported_payload

def run():
    sink(imported_payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the directly aliased imported module-level value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint does not overtaint imported safe module-level values"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
safe_payload = "safe"
|};
            UFile.write_file app_file
              {|from source import safe_payload

def run():
    sink(safe_payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the imported safe module-level value sink"
              app_matches));
    t "interfile taint across python imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helpers.py" in
            let app_file = root / "app.py" in
            let safe_app_file = root / "safe_app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file app_file
              {|import helpers

def run():
    sink(helpers.helper())  # ruleid: interfile-python
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file safe_app_file
              {|import helpers

def run():
    sink("safe")
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; helper_file; app_file; safe_app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            let safe_app_matches =
              let res = check_file safe_app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool) "match is reported on the sink file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int) "match is reported on the sink line" 4
              actual_line;
            Alcotest.(check int) "no finding in safe file" 0
              (List.length safe_app_matches)));
    t "interfile taint across local imports inside imported helpers" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper():
    from source import source
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the helper sink reached through an upstream local import"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across local module imports inside imported helpers"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper():
    import source as source_mod
    return source_mod.source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:
                "the helper sink reached through an upstream local module import"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across local relative imports inside imported helpers"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = pkg_dir / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper():
    from .source import source
    return source()
|};
            UFile.write_file app_file
              {|from .helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:
                "the helper sink reached through an upstream local relative import"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint is independent of scan input order" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ app_file; helper_file; source_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the reversed scan-order sink" ~file:app_file
              ~line:4 app_matches));
    t "interfile taint across python package imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "src" / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = pkg_dir / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from pkg.source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg.helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one package interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool) "match is reported on the package sink file"
              true (Fpath.equal app_file actual_file);
            Alcotest.(check int) "match is reported on the package sink line" 4
              actual_line));
    t "interfile taint across aliased module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|import helper as helper_mod

def run():
    sink(helper_mod.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one aliased module interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool)
              "match is reported on the aliased module sink file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int)
              "match is reported on the aliased module sink line" 4 actual_line));
    t "interfile taint across aliased function imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper as run_helper

def run():
    sink(run_helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one aliased function interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool)
              "match is reported on the aliased function sink file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int)
              "match is reported on the aliased function sink line" 4
              actual_line));
    t "interfile taint across aliased upstream imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source as source_fn

def helper():
    return source_fn()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased upstream helper sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across python relative imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = pkg_dir / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from .helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one relative import interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool)
              "match is reported on the relative import sink file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int)
              "match is reported on the relative import sink line" 4 actual_line));
    t "interfile taint across python parent relative imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            let subpkg_dir = pkg_dir / "subpkg" in
            UFile.make_directories subpkg_dir;
            let pkg_init = pkg_dir / "__init__.py" in
            let subpkg_init = subpkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = subpkg_dir / "helper.py" in
            let app_file = subpkg_dir / "app.py" in
            UFile.write_file pkg_init "";
            UFile.write_file subpkg_init "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from ..source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from .helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ pkg_init; subpkg_init; source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int)
              "one parent relative import interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool)
              "match is reported on the parent relative import sink file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int)
              "match is reported on the parent relative import sink line" 4
              actual_line));
    t "interfile taint across aliased relative function imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = pkg_dir / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from .helper import helper as run_helper

def run():
    sink(run_helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased relative helper sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across package re-exports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .helper import helper
|};
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one package re-export interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool)
              "match is reported on the package re-export sink file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int)
              "match is reported on the package re-export sink line" 4
              actual_line));
    t "interfile taint across package re-exported module-level values"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .source import payload
|};
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from pkg import payload

def run():
    sink(payload)  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ init_file; source_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the package re-exported value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased package re-exported module-level values"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .source import payload as exported_payload
|};
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from pkg import exported_payload

def run():
    sink(exported_payload)  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ init_file; source_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased package re-exported value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased symbol package re-exports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .helper import helper
|};
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|import pkg as pkg_mod

def run():
    sink(pkg_mod.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased package re-export sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across package submodule imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg import helper

def run():
    sink(helper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the package submodule sink" ~file:app_file
              ~line:4 app_matches));
    t "interfile taint across aliased package submodule imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg import helper as helper_mod

def run():
    sink(helper_mod.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased package submodule sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased dotted package module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|import pkg.helper as helper_mod

def run():
    sink(helper_mod.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased dotted package module sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased package re-exports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .helper import helper as exported_helper
|};
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg import exported_helper as run_helper

def run():
    sink(run_helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased package re-exported sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased dotted package symbol imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg.helper import helper as run_helper

def run():
    sink(run_helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased dotted package helper sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across imported class methods" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    @staticmethod
    def helper():
        return source()
|};
            UFile.write_file app_file
              {|from helper import Helper

def run():
    sink(Helper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int) "one imported method interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool) "match is reported on the imported method sink file"
              true (Fpath.equal app_file actual_file);
            Alcotest.(check int) "match is reported on the imported method sink line"
              4 actual_line));
    t "interfile taint across imported classmethod parameters" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    @classmethod
    def run(cls, value):
        sink(value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner.run(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the imported classmethod-parameter sink"
              ~file:helper_file ~line:4 helper_matches));
    t "interfile taint across imported instance methods" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    def helper(self):
        return source()
|};
            UFile.write_file app_file
              {|from helper import Helper

def run():
    sink(Helper().helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int)
              "one imported instance method interfile finding" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool)
              "match is reported on the imported instance method sink file"
              true (Fpath.equal app_file actual_file);
            Alcotest.(check int)
              "match is reported on the imported instance method sink line" 4
              actual_line));
    t "interfile taint does not overtaint safe instance methods from mixed classes"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    def helper(self):
        return source()

    def safe_helper(self):
        return "safe"
|};
            UFile.write_file app_file
              {|from helper import Helper

def run():
    sink(Helper().safe_helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the safe instance method imported from a mixed-taint class"
              app_matches));
    t "interfile taint across aliased imported instance methods" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    def helper(self):
        return source()
|};
            UFile.write_file app_file
              {|from helper import Helper as ImportedHelper

def run():
    sink(ImportedHelper().helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased imported instance-method sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased imported class methods" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    @staticmethod
    def helper():
        return source()
|};
            UFile.write_file app_file
              {|from helper import Helper as ImportedHelper

def run():
    sink(ImportedHelper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased imported class-method sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across module-qualified imported class methods" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    @staticmethod
    def helper():
        return source()
|};
            UFile.write_file app_file
              {|import helper

def run():
    sink(helper.Helper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the module-qualified imported class-method sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across module-qualified imported instance methods"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

class Helper:
    def helper(self):
        return source()
|};
            UFile.write_file app_file
              {|import helper

def run():
    sink(helper.Helper().helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the module-qualified imported instance-method sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint resolves the correct package when sibling modules share a basename"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let pkg1_dir = root / "pkg1" in
            let pkg2_dir = root / "pkg2" in
            let pkg1_init = pkg1_dir / "__init__.py" in
            let pkg2_init = pkg2_dir / "__init__.py" in
            let pkg1_util = pkg1_dir / "util.py" in
            let pkg2_util = pkg2_dir / "util.py" in
            let app_file = root / "app.py" in
            UFile.make_directories pkg1_dir;
            UFile.make_directories pkg2_dir;
            UFile.write_file pkg1_init "";
            UFile.write_file pkg2_init "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file pkg1_util
              {|def helper():
    return "safe"
|};
            UFile.write_file pkg2_util
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg2.util import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; pkg1_util; pkg2_util; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int)
              "one interfile finding through the imported package helper" 1
              (List.length app_matches);
            let actual_file, actual_line = List.hd app_matches in
            Alcotest.(check bool) "match is reported on the app file" true
              (Fpath.equal app_file actual_file);
            Alcotest.(check int) "match is reported on the sink line" 4
              actual_line));
    t "interfile taint does not overtaint sibling packages that share a basename"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let pkg1_dir = root / "pkg1" in
            let pkg2_dir = root / "pkg2" in
            let pkg1_init = pkg1_dir / "__init__.py" in
            let pkg2_init = pkg2_dir / "__init__.py" in
            let pkg1_util = pkg1_dir / "util.py" in
            let pkg2_util = pkg2_dir / "util.py" in
            let app_file = root / "app.py" in
            UFile.make_directories pkg1_dir;
            UFile.make_directories pkg2_dir;
            UFile.write_file pkg1_init "";
            UFile.write_file pkg2_init "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file pkg1_util
              {|def helper():
    return "safe"
|};
            UFile.write_file pkg2_util
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg1.util import helper

def run():
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; pkg1_util; pkg2_util; app_file ]
            in
            let app_matches =
              let res = check_file app_file in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int)
              "no interfile finding through the safe sibling package helper" 0
              (List.length app_matches)));
    t "interfile taint does not overtaint sibling packages with relative imports"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg1_dir = root / "pkg1" in
            let pkg2_dir = root / "pkg2" in
            let pkg1_init = pkg1_dir / "__init__.py" in
            let pkg2_init = pkg2_dir / "__init__.py" in
            let pkg1_util = pkg1_dir / "util.py" in
            let pkg2_util = pkg2_dir / "util.py" in
            let pkg1_app = pkg1_dir / "app.py" in
            UFile.make_directories pkg1_dir;
            UFile.make_directories pkg2_dir;
            UFile.write_file pkg1_init "";
            UFile.write_file pkg2_init "";
            UFile.write_file pkg1_util
              {|def helper():
    return "safe"
|};
            UFile.write_file pkg2_util
              {|def helper():
    return tainted()
|};
            UFile.write_file pkg1_app
              {|from .util import helper

def run():
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root
                [ pkg1_init; pkg2_init; pkg1_util; pkg2_util; pkg1_app ]
            in
            let app_matches =
              let res = check_file pkg1_app in
              res.matches |> List_.map TCM.location_of_pm
            in
            Alcotest.(check int)
              "no interfile finding through the safe relative import helper" 0
              (List.length app_matches)));
    t "interfile taint across dotted package module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|import pkg.helper

def run():
    sink(pkg.helper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the dotted package module sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across multi-hop python imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let relay_file = root / "relay.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file relay_file
              {|from helper import helper

def relay():
    return helper()
|};
            UFile.write_file app_file
              {|from relay import relay

def run():
    sink(relay())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; helper_file; relay_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the multi-hop sink" ~file:app_file ~line:4
              app_matches));
    t "interfile taint reports all sink files that share a tainted helper"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_one_file = root / "app_one.py" in
            let app_two_file = root / "app_two.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_one_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            UFile.write_file app_two_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; helper_file; app_one_file; app_two_file ]
            in
            let app_one_matches = match_locations check_file app_one_file in
            let app_two_matches = match_locations check_file app_two_file in
            check_single_match ~name:"the first shared-helper sink"
              ~file:app_one_file ~line:4 app_one_matches;
            check_single_match ~name:"the second shared-helper sink"
              ~file:app_two_file ~line:4 app_two_matches));
    t "interfile taint reports multiple sink locations in one file" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the repeated sink file"
              ~file:app_file ~lines:[ 4; 5 ] app_matches));
    t "interfile taint reaches sinks at module top level" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the top-level sink" ~file:app_file
              ~line:3 app_matches));
    t "interfile taint does not overtaint sibling helpers defined in the same module"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()

def safe_helper():
    return "safe"
|};
            UFile.write_file app_file
              {|from helper import safe_helper

def run():
    sink(safe_helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the safe helper imported from a mixed-taint module" app_matches));
    t "interfile taint resolves aliased imports even when the original name is shadowed locally"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper as imported_helper

def helper():
    return "safe"

def run():
    sink(helper())
    sink(imported_helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the aliased helper sink after local shadowing" ~file:app_file
              ~line:8 app_matches));
    t "interfile taint does not overtaint when a local definition shadows an imported helper"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def helper():
    return "safe"

def run():
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the locally shadowed helper sink"
              app_matches));
    t "interfile taint does not overtaint when an imported helper is shadowed by a parameter"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run(helper):
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the parameter-shadowed helper sink"
              app_matches));
    t "interfile taint does not overtaint when an imported helper is shadowed in local scope"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def safe_helper():
    return "safe"

def run():
    helper = safe_helper
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the locally rebound helper sink"
              app_matches));
    t "interfile taint does not overtaint when an imported helper is rebound"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def safe_helper():
    return "safe"

helper = safe_helper

def run():
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the rebound imported helper sink"
              app_matches));
    t "interfile taint does not overtaint when an imported module alias is rebound"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|import helper as helper_mod

class SafeModule:
    @staticmethod
    def helper():
        return "safe"

helper_mod = SafeModule

def run():
    sink(helper_mod.helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the rebound imported module alias sink"
              app_matches));
    t "interfile taint does not report when upstream modules are outside the scan inputs"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())
|};
            let check_file = mk_interfile_checker root [ app_file ] in
            let app_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the sink with upstream modules excluded from the scan inputs"
              app_matches));
    t "interfile taint does not report when the source module is outside the scan inputs"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())
|};
            let check_file = mk_interfile_checker root [ helper_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the sink with the source module excluded from the scan inputs"
              app_matches));
    t "interfile taint across relative module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = pkg_dir / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from . import helper

def run():
    sink(helper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the relative module import sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across wildcard imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import *

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the wildcard import sink" ~file:app_file
              ~line:4 app_matches));
    t "interfile taint across wildcard-imported module-level values" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from source import *

def run():
    sink(payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the wildcard-imported value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across wildcard package re-exports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = root / "app.py" in
            UFile.make_directories pkg_dir;
            UFile.write_file init_file
              {|from .helper import helper
|};
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from pkg import *

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the wildcard package re-export sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across wildcard package re-exported module-level values"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let app_file = root / "app.py" in
            UFile.make_directories pkg_dir;
            UFile.write_file init_file
              {|from .source import payload
|};
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from pkg import *

def run():
    sink(payload)  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ init_file; source_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the wildcard package re-exported value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint does not overtaint wildcard-imported safe helpers from mixed modules"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()

def safe_helper():
    return "safe"
|};
            UFile.write_file app_file
              {|from helper import *

def run():
    sink(safe_helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the wildcard-imported safe helper sink"
              app_matches));
    t "interfile taint does not overtaint wildcard-imported safe module-level values from mixed modules"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
safe_payload = "safe"
|};
            UFile.write_file app_file
              {|from source import *

def run():
    sink(safe_payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the wildcard-imported safe value sink"
              app_matches));
    t "interfile taint only reports sinks before a wildcard-imported module-level value is rebound"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from source import *

sink(payload)  # ruleid: interfile-python

payload = "safe"

sink(payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the rebound wildcard-imported value sinks"
              ~file:app_file ~lines:[ 3 ] app_matches));
    t "interfile taint across local symbol imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file app_file
              {|def run():
    from source import source
    sink(source())  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the local symbol import sink"
              ~file:app_file ~line:3 app_matches));
    t "interfile taint across local imported module-level values" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|def run():
    from source import payload
    sink(payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the local imported value sink"
              ~file:app_file ~line:3 app_matches));
    t "interfile taint across local aliased symbol imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file app_file
              {|def run():
    from source import source as imported_source
    sink(imported_source())  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the local aliased symbol import sink"
              ~file:app_file ~line:3 app_matches));
    t "interfile taint across local aliased imported module-level values"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|def run():
    from source import payload as imported_payload
    sink(imported_payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the local aliased imported value sink"
              ~file:app_file ~line:3 app_matches));
    t "interfile taint across local module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|def run():
    import helper
    sink(helper.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the local module import sink"
              ~file:app_file ~line:3 app_matches));
    t "interfile taint across local aliased module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|def run():
    import helper as helper_mod
    sink(helper_mod.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the local aliased module import sink"
              ~file:app_file ~line:3 app_matches));
    t
      "interfile taint conservatively keeps local-imported helper sinks after a rebind once the imported helper was used"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|def safe_helper():
    return "safe"

def run():
    from helper import helper
    sink(helper())  # ruleid: interfile-python
    helper = safe_helper
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the locally rebound helper sinks"
              ~file:app_file ~lines:[ 6; 8 ] app_matches));
    t "interfile taint only reports local-imported module-level value sinks before a rebind"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|def run():
    from source import payload
    sink(payload)  # ruleid: interfile-python
    payload = "safe"
    sink(payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the locally rebound imported-value sinks"
              ~file:app_file ~lines:[ 3 ] app_matches));
    t "interfile taint across imported module-level values via module attributes"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|import source

def run():
    sink(source.payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the imported module attribute sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint across aliased imported module-level values" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|import source as source_mod

def run():
    sink(source_mod.payload)  # ruleid: interfile-python
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased imported module value sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint keeps imported module attributes symbol-specific" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
safe_payload = "safe"
|};
            UFile.write_file app_file
              {|import source

def run():
    sink(source.safe_payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the safe imported module attribute sink"
              app_matches));
    t "interfile taint across package-imported module-level attributes"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .source import payload
|};
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|import pkg

def run():
    sink(pkg.payload)  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ init_file; source_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the package-imported value attribute sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint keeps package-imported module-level attributes symbol-specific"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file init_file
              {|from .source import payload, safe_payload
|};
            UFile.write_file source_file
              {|payload = tainted()
safe_payload = "safe"
|};
            UFile.write_file app_file
              {|import pkg

def run():
    sink(pkg.safe_payload)
|};
            let check_file =
              mk_interfile_checker root [ init_file; source_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the safe package-imported value attribute sink"
              app_matches));
    t "interfile taint keeps same-named imported symbols distinct across modules"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let tainted_source_file = root / "tainted_source.py" in
            let safe_source_file = root / "safe_source.py" in
            let app_file = root / "app.py" in
            UFile.write_file tainted_source_file
              {|def build():
    return tainted()
|};
            UFile.write_file safe_source_file
              {|def build():
    return "safe"
|};
            UFile.write_file app_file
              {|from tainted_source import build as tainted_build
from safe_source import build as safe_build

def run():
    sink(safe_build())
    sink(tainted_build())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ tainted_source_file; safe_source_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the same-named imported symbol sink"
              ~file:app_file ~line:6 app_matches));
    t "interfile taint keeps mixed named imports symbol-specific" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()

def safe_helper():
    return "safe"
|};
            UFile.write_file app_file
              {|from helper import helper, safe_helper

def run():
    sink(safe_helper())
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the mixed named-import sink"
              ~file:app_file ~line:5 app_matches));
    t
      "interfile taint conservatively keeps imported helper sinks after a rebind once the imported helper was used"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

sink(helper())  # ruleid: interfile-python

def safe_helper():
    return "safe"

helper = safe_helper

sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the rebound helper sinks"
              ~file:app_file ~lines:[ 3; 10 ] app_matches));
    t "interfile taint only reports sinks before an imported module alias is rebound"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|import helper as helper_mod

sink(helper_mod.helper())  # ruleid: interfile-python

class SafeModule:
    @staticmethod
    def helper():
        return "safe"

helper_mod = SafeModule

sink(helper_mod.helper())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the rebound module-alias sinks"
              ~file:app_file ~lines:[ 3 ] app_matches));
    t "interfile taint only reports sinks before an imported module-level value is rebound"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from source import payload

sink(payload)  # ruleid: interfile-python

payload = "safe"

sink(payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_match_lines ~name:"the rebound imported-value sinks"
              ~file:app_file ~lines:[ 3 ] app_matches));
    t "interfile taint does not overtaint when an imported module-level value is shadowed by a parameter"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|payload = tainted()
|};
            UFile.write_file app_file
              {|from source import payload

def run(payload):
    sink(payload)
|};
            let check_file = mk_interfile_checker root [ source_file; app_file ] in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the parameter-shadowed imported-value sink"
              app_matches));
    t "interfile taint across aliased relative module imports" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg_dir = root / "pkg" in
            UFile.make_directories pkg_dir;
            let init_file = pkg_dir / "__init__.py" in
            let source_file = pkg_dir / "source.py" in
            let helper_file = pkg_dir / "helper.py" in
            let app_file = pkg_dir / "app.py" in
            UFile.write_file init_file "";
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from .source import source

def helper():
    return source()
|};
            UFile.write_file app_file
              {|from . import helper as helper_mod

def run():
    sink(helper_mod.helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ init_file; source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the aliased relative module sink"
              ~file:app_file ~line:4 app_matches));
    t "interfile taint does not overtaint cyclic imports without a tainted path"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let helper_file = root / "helper.py" in
            let relay_file = root / "relay.py" in
            let app_file = root / "app.py" in
            UFile.write_file helper_file
              {|from relay import relay

def helper():
    return relay()

def noop():
    return "safe"
|};
            UFile.write_file relay_file
              {|from helper import noop

def relay():
    noop()
    return "safe"
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())
|};
            let check_file =
              mk_interfile_checker root [ helper_file; relay_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches ~name:"the safe cyclic-import sink" app_matches));
    t "interfile taint handles cyclic imports without losing the tainted flow"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let relay_file = root / "relay.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from relay import relay

def helper():
    return relay()

def noop():
    return "safe"
|};
            UFile.write_file relay_file
              {|from helper import noop
from source import source

def relay():
    noop()
    return source()
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())  # ruleid: interfile-python
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; helper_file; relay_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match ~name:"the cyclic-import sink" ~file:app_file
              ~line:4 app_matches));
    t "interfile taint across imported helper parameters" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(value):
    sink(value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match ~name:"the imported helper-parameter sink"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint across imported helper default parameters" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper(value=source()):
    sink(value)
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    helper()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the imported helper sink reached through a tainted default"
              ~file:helper_file ~line:4 helper_matches));
    t
      "interfile taint does not overtaint imported helper default parameters when callers override them"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def helper(value=source()):
    sink(value)
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    helper("safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the imported helper sink when a tainted default is overridden"
              helper_matches));
    t "interfile taint across module-qualified imported helper parameters"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(value):
    sink(value)
|};
            UFile.write_file app_file
              {|from source import source
import helper as helper_mod

def run():
    helper_mod.helper(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the module-qualified imported helper-parameter sink"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint maps imported helper arguments by position" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(first, second):
    sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper("safe", source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the imported helper sink reached through the second argument"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint keeps imported helper argument positions distinct"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(first, second):
    sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(source(), "safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the imported helper sink when only a non-sunk parameter is tainted"
              helper_matches));
    t "interfile taint maps imported helper keyword arguments to parameters"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(first, second):
    sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(second=source(), first="safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:
                "the imported helper sink reached through a keyword-mapped argument"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint keeps imported helper keyword arguments parameter-specific"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(first, second):
    sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(first=source(), second="safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:
                "the imported helper sink when only a non-sunk keyword argument is tainted"
              helper_matches));
    t "interfile taint maps imported helper keyword argument relays" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def sink_value(danger):
    sink(danger)

def helper(value):
    sink_value(danger=value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:
                "the imported helper sink reached through a keyword relay"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint maps imported variadic helper arguments by position"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(*args):
    sink(args[1])
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper("safe", source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the imported variadic helper sink reached through the second argument"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint maps imported keyword-only helper arguments after variadics"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(first, *args, y):
    sink(first)
    sink(y)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(source(), "safe", y=source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_match_lines
              ~name:
                "the imported variadic helper sinks reached through the positional and keyword-only arguments"
              ~file:helper_file ~lines:[ 2; 3 ] helper_matches));
    t "interfile taint respects sanitizers at imported helper call sites"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let sanitizer_file = root / "sanitizer.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file sanitizer_file
              {|def sanitize(value):
    return "safe"
|};
            UFile.write_file helper_file
              {|def helper(value):
    sink(value)
|};
            UFile.write_file app_file
              {|from helper import helper
from sanitizer import sanitize
from source import source

def run():
    helper(sanitize(source()))
|};
            let check_file =
              mk_interfile_checker
                ~rule_extra_sections:
                  {|  pattern-sanitizers:
    - pattern: sanitize(...)
|}
                root [ source_file; sanitizer_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the imported helper sink sanitized by the caller"
              helper_matches));
    t "interfile taint respects sanitizers in imported helper returns" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|from source import source

def sanitize(value):
    return "safe"

def helper():
    return sanitize(source())
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    sink(helper())
|};
            let check_file =
              mk_interfile_checker
                ~rule_extra_sections:
                  {|  pattern-sanitizers:
    - pattern: sanitize(...)
|}
                root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the imported helper return sanitized before the sink"
              app_matches));
    t "interfile taint respects sanitizers in imported helper parameters"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def sanitize(value):
    return "safe"

def helper(value):
    sink(sanitize(value))
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(source())
|};
            let check_file =
              mk_interfile_checker
                ~rule_extra_sections:
                  {|  pattern-sanitizers:
    - pattern: sanitize(...)
|}
                root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the imported helper-parameter sink sanitized across files"
              helper_matches));
    t "interfile taint does not overtaint imported helper parameters without a tainted caller"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(value):
    sink(value)
|};
            UFile.write_file app_file
              {|from helper import helper

def run():
    helper("safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the helper-parameter sink with only safe callers"
              helper_matches));
    t "interfile taint reports imported helper parameters with mixed callers"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(value):
    sink(value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run_safe():
    helper("safe")

def run_tainted():
    helper(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match ~name:"the helper-parameter sink with mixed callers"
              ~file:helper_file ~line:2 helper_matches));
    t "interfile taint does not overtaint when imported helper parameters are rebound"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|def helper(value):
    value = "safe"
    sink(value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import helper

def run():
    helper(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the rebound helper-parameter sink from a tainted caller"
              helper_matches));
    t "interfile taint across imported instance-method parameters" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, value):
        sink(value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner().run(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match ~name:"the imported instance-method parameter sink"
              ~file:helper_file ~line:3 helper_matches));
    t "interfile taint maps imported instance-method arguments by position"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, first, second):
        sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner().run("safe", source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:
                "the imported instance-method sink reached through the second argument"
              ~file:helper_file ~line:3 helper_matches));
    t "interfile taint keeps imported instance-method argument positions distinct"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, first, second):
        sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner().run(source(), "safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:
                "the imported instance-method sink when only a non-sunk parameter is tainted"
              helper_matches));
    t "interfile taint maps imported instance-method keyword arguments to parameters"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, first, second):
        sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner().run(second=source(), first="safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:
                "the imported instance-method sink reached through a keyword-mapped argument"
              ~file:helper_file ~line:3 helper_matches));
    t "interfile taint keeps imported instance-method keyword arguments parameter-specific"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, first, second):
        sink(second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner().run(first=source(), second="safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:
                "the imported instance-method sink when only a non-sunk keyword argument is tainted"
              helper_matches));
    t "interfile taint across stored imported instance-method parameters"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, value):
        sink(value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner()
    runner.run(source())
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the stored imported instance-method parameter sink"
              ~file:helper_file ~line:3 helper_matches));
    t "interfile taint does not report stored imported instance-method parameters with safe input"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def run(self, value):
        sink(value)
|};
            UFile.write_file app_file
              {|from helper import Runner

def run():
    runner = Runner()
    runner.run("safe")
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the stored imported instance-method sink with safe input"
              helper_matches));
    t "interfile taint across imported constructor state" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value

    def run(self):
        sink(self.value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner(source()).run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match ~name:"the imported constructor-state sink"
              ~file:helper_file ~line:6 helper_matches));
    t "interfile taint across aliased imported constructor state" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value

    def run(self):
        sink(self.value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner as ImportedRunner

def run():
    ImportedRunner(source()).run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the aliased imported constructor-state sink"
              ~file:helper_file ~line:6 helper_matches));
    t "interfile taint across module-qualified imported constructor state"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value

    def run(self):
        sink(self.value)
|};
            UFile.write_file app_file
              {|from source import source
import helper as helper_mod

def run():
    helper_mod.Runner(source()).run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the module-qualified imported constructor-state sink"
              ~file:helper_file ~line:6 helper_matches));
    t "interfile taint across stored imported constructor state" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value

    def run(self):
        sink(self.value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner(source())
    runner.run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the stored imported constructor-state sink"
              ~file:helper_file ~line:6 helper_matches));
    t "interfile taint does not report stored imported constructor state with safe input"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value

    def run(self):
        sink(self.value)
|};
            UFile.write_file app_file
              {|from helper import Runner

def run():
    runner = Runner("safe")
    runner.run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the stored imported constructor-state sink with safe input"
              helper_matches));
    t "interfile taint exposes stored imported constructor fields to callers"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner(source())
    sink(runner.value)
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let app_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the caller-side sink for stored imported constructor state"
              ~file:app_file ~line:6 app_matches));
    t "interfile taint maps imported constructor keyword arguments to fields"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, first, second):
        self.first = first
        self.second = second

    def run(self):
        sink(self.second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner(second=source(), first="safe")
    runner.run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the imported constructor sink reached through a keyword field"
              ~file:helper_file ~line:7 helper_matches));
    t "interfile taint maps imported constructor arguments by position" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, first, second):
        self.first = first
        self.second = second

    def run(self):
        sink(self.second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner("safe", source())
    runner.run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_single_match
              ~name:"the imported constructor sink reached through the second positional field"
              ~file:helper_file ~line:7 helper_matches));
    t "interfile taint keeps imported constructor argument positions distinct"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, first, second):
        self.first = first
        self.second = second

    def run(self):
        sink(self.second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner(source(), "safe")
    runner.run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:
                "the imported constructor sink when only a non-sunk positional field is tainted"
              helper_matches));
    t "interfile taint keeps imported constructor keyword arguments field-specific"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, first, second):
        self.first = first
        self.second = second

    def run(self):
        sink(self.second)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    runner = Runner(first=source(), second="safe")
    runner.run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:
                "the imported constructor sink when only a non-sunk keyword field is tainted"
              helper_matches));
    t "interfile taint does not overtaint overwritten imported constructor state"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let helper_file = root / "helper.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file helper_file
              {|class Runner:
    def __init__(self, value):
        self.value = value

    def run(self):
        self.value = "safe"
        sink(self.value)
|};
            UFile.write_file app_file
              {|from source import source
from helper import Runner

def run():
    Runner(source()).run()
|};
            let check_file =
              mk_interfile_checker root [ source_file; helper_file; app_file ]
            in
            let helper_matches = match_locations check_file app_file in
            check_no_matches
              ~name:"the overwritten constructor-state sink from a tainted caller"
              helper_matches));
    t "interfile taint across multi-hop imported parameter relays" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let source_file = root / "source.py" in
            let sink_helper_file = root / "sink_helper.py" in
            let relay_file = root / "relay.py" in
            let app_file = root / "app.py" in
            UFile.write_file source_file
              {|def source():
    return tainted()
|};
            UFile.write_file sink_helper_file
              {|def sink_value(value):
    sink(value)
|};
            UFile.write_file relay_file
              {|from sink_helper import sink_value

def relay(value):
    sink_value(value)
|};
            UFile.write_file app_file
              {|from source import source
from relay import relay

def run():
    relay(source())
|};
            let check_file =
              mk_interfile_checker root
                [ source_file; sink_helper_file; relay_file; app_file ]
            in
            let sink_matches = match_locations check_file app_file in
            check_single_match ~name:"the multi-hop imported relay sink"
              ~file:sink_helper_file ~line:2 sink_matches));
  ]

let interfile_project_graph_tests () =
  let build_project_graph files =
    let xlang = Xlang.of_lang Lang.Python in
    let xtarget_of_file = Test_engine.xtarget_of_file xlang in
    let project_inputs =
      files
      |> List_.map (fun file ->
             let xtarget = xtarget_of_file file in
             let ast, _ = Lazy.force xtarget.lazy_ast_and_errors in
             (xtarget.path, ast, []))
    in
    Graph_from_AST.build_project_call_graph ~lang:Lang.Python project_inputs
  in
  let count_vertices_by_name graph name =
    Call_graph.G.fold_vertex
      (fun vertex count ->
        if String.equal (Function_id.show vertex) name then count + 1 else count)
      graph 0
  in
  let count_vertices graph =
    Call_graph.G.fold_vertex (fun _ count -> count + 1) graph 0
  in
  [
    t "project call graph keeps distinct top-level nodes per file" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let file_a = root / "a.py" in
            let file_b = root / "b.py" in
            UFile.write_file file_a
              {|def f():
    return 1
|};
            UFile.write_file file_b
              {|def g():
    return 2
|};
            let graph = build_project_graph [ file_a; file_b ] in
            Alcotest.(check int) "one top-level node per file" 2
              (count_vertices_by_name graph "<top_level>")));
    t "project call graph keeps distinct class-init nodes per file" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let file_a = root / "a.py" in
            let file_b = root / "b.py" in
            UFile.write_file file_a
              {|class Widget:
    data = 1
|};
            UFile.write_file file_b
              {|class Widget:
    data = 2
|};
            let graph = build_project_graph [ file_a; file_b ] in
            Alcotest.(check int) "one class-init node per file" 2
              (count_vertices_by_name graph "Class:Widget")));
    t "project call graph keeps distinct function nodes per file" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let file_a = root / "a.py" in
            let file_b = root / "b.py" in
            UFile.write_file file_a
              {|def helper():
    return 1
|};
            UFile.write_file file_b
              {|def helper():
    return 2
|};
            let graph = build_project_graph [ file_a; file_b ] in
            Alcotest.(check int) "one function node per file" 2
              (count_vertices_by_name graph "helper")));
    t "project call graph keeps same-basename module functions distinct across directories"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg1_dir = root / "pkg1" in
            let pkg2_dir = root / "pkg2" in
            let pkg1_util = pkg1_dir / "util.py" in
            let pkg2_util = pkg2_dir / "util.py" in
            UFile.make_directories pkg1_dir;
            UFile.make_directories pkg2_dir;
            UFile.write_file pkg1_util
              {|def helper():
    return 1
|};
            UFile.write_file pkg2_util
              {|def helper():
    return 2
|};
            let graph = build_project_graph [ pkg1_util; pkg2_util ] in
            Alcotest.(check int)
              "one top-level node per same-basename module" 2
              (count_vertices_by_name graph "<top_level>");
            Alcotest.(check int)
              "one helper node per same-basename module" 2
              (count_vertices_by_name graph "helper")));
    t "project call graph keeps distinct method nodes per file" (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let file_a = root / "a.py" in
            let file_b = root / "b.py" in
            UFile.write_file file_a
              {|class Widget:
    def helper(self):
        return 1
|};
            UFile.write_file file_b
              {|class Widget:
    def helper(self):
        return 2
|};
            let graph = build_project_graph [ file_a; file_b ] in
            Alcotest.(check int) "top-level, class-init, and method nodes stay per-file"
              6 (count_vertices graph)));
    t "project call graph keeps same-named methods distinct across classes in one file"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let file = root / "a.py" in
            UFile.write_file file
              {|class First:
    def helper(self):
        return 1

class Second:
    def helper(self):
        return 2
|};
            let graph = build_project_graph [ file ] in
            Alcotest.(check int)
              "top-level, both class-init nodes, and both methods are preserved"
              5 (count_vertices graph)));
    t "project call graph keeps same-basename class methods distinct across directories"
      (fun () ->
        Testutil_files.with_tempdir ~chdir:true (fun root ->
            let pkg1_dir = root / "pkg1" in
            let pkg2_dir = root / "pkg2" in
            let pkg1_util = pkg1_dir / "util.py" in
            let pkg2_util = pkg2_dir / "util.py" in
            UFile.make_directories pkg1_dir;
            UFile.make_directories pkg2_dir;
            UFile.write_file pkg1_util
              {|class Widget:
    def helper(self):
        return 1
|};
            UFile.write_file pkg2_util
              {|class Widget:
    def helper(self):
        return 2
|};
            let graph = build_project_graph [ pkg1_util; pkg2_util ] in
            Alcotest.(check int)
              "top-level, class-init, and method nodes stay per same-basename file"
              6 (count_vertices graph)));
  ]

let lang_tainting_tests () =
  let taint_tests_path = tests_path / "tainting_rules" in
  let lang_specs =
    [
      (* lang, dir, ext *)
      (Lang.Apex, "apex", ".trigger"); (* TODO: Use Lang.* functions to derive .2, .3 *)
      (Lang.Clojure, "clojure", ".clj");
      (Lang.Csharp, "csharp", ".cs");
      (Lang.Dart, "dart", ".dart");
      (Lang.Elixir, "elixir", ".ex");
      (Lang.Go, "go", ".go");
      (Lang.Java, "java", ".java");
      (Lang.Js, "js", ".js");
      (Lang.Php, "php", ".php");
      (Lang.Python, "python", ".py");
      (Lang.Ruby, "ruby", ".rb");
      (Lang.Ruby, "rust", ".rs");
      (Lang.Scala, "scala", ".scala");
      (Lang.Ts, "ts", ".ts");
      (Lang.Vb, "vb", ".vb");
    ]
  in
  Testo.categorize_suites "lang tainting rules"
    (List_.map
       (fun (lang, dir, _ext) ->
          Testo.categorize (spf "tainting %s" (Lang.show lang))
            (Test_engine.make_tests [ taint_tests_path / dir ]))
    lang_specs)

(*****************************************************************************)
(* Full rule tests *)
(*****************************************************************************)

(* DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/rules tests/rules_v2'
 *)
let full_rule_regression_tests () =
  let path = tests_path / "rules" in
  let tests1 = Test_engine.make_tests ~prepend_lang:true [ path ] in
  let path = tests_path / "rules_v2" in
  let tests2 = Test_engine.make_tests ~prepend_lang:true [ path ] in
  let tests = tests1 @ tests2 in
  let groups =
    tests
    |> List_.map (fun (test : Testo.t) ->
           let group =
             match String.split_on_char ' ' test.name with
             | lang :: _ -> lang
             | _ -> test.name
           in
           (group, test))
    |> Assoc.group_assoc_bykey_eff
  in

  Testo.categorize_suites "full rule"
    (groups |> List_.map (fun (group, tests) -> Testo.categorize group tests))

(* TODO: For now we only have taint maturity tests for Beta, there are no
 * specific tests for GA.
 * TODO: We should also have here an explicit list of test filenames, like
 * "taint_if", that is then checked for every language, like we do for the
 * search mode maturity tests.
 * TODO: We could have a taint_maturity/POLYGLOT/ directory to put reusable
 * rules that can work for multiple languages (like we have
 * for tests/patterns/POLYGLOT/
 * DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/taint_maturity'
 *)
let full_rule_taint_maturity_tests () =
  let path = tests_path / "taint_maturity" in
  Testo.categorize "taint maturity" (Test_engine.make_tests [ path ])

(*
   Special exclusions for Semgrep JS
*)
let mark_todo_js (test : Testo.t) =
  match test.name with
  | s
    when (* The target file has an unsupported .erb extension, making it excluded
            correctly by the OCaml test suite but not by the JS test suite
            (or something close to this). *)
         s =~ ".*/ruby/rails/security/brakeman/check-reverse-tabnabbing.yaml"
         || (* Not sure why this fails *)
         s =~ ".*/ruby/lang/security/divide-by-zero.yaml" ->
      Testo.update test ~tags:(Test_tags.todo_js :: test.tags)
  | _ -> test

(* quite similar to full_rule_regression_tests but prefer to pack_tests
 * with "semgrep-rules repo Java", so one can just run the Java tests
 * with ./test Java
 * alt: do like in semgrep-pro and call the toplevel engine
 * in a Unit_runner.ml instead of using Test_engine.make_tests
 * TODO: get rid of this and rely on `osemgrep test` code instead of
 * Test_engine.make_tests as they differ sligltly and we're using
 * osemgrep test in the semgrep-rules repo CI, not -test_rules.
 * DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/semgrep-rules'
 *)
let semgrep_rules_repo_tests () : Testo.t list =
  let path = tests_path / "semgrep-rules" in
  let tests = Test_engine.make_tests [ path ] in
  let groups =
    tests
    |> List_.filter_map (fun (test : Testo.t) ->
           let test = mark_todo_js test in
           let group_opt =
             match test.name with
             (* note that there is no need to filter rules without targets; This
              * is now handled in Test_engine.make_tests which will generate
              * an XFAIL Testo test for those.
              *)
             | s
               when (* TODO: we're skipping those rules because e.g. for bidy.yml
                       we're using a languages: [bash,c, ..., python] and a
                       bidy.py target file which cause Test_engine.make_test to
                       parse bidy.py with a bash parser (the first one in the
                       list) which then cause a parse error. (py|o)semgrep test
                       do not have the issue because they use the extension of
                       the file to decide which language to use instead of what
                       is in the rule
                    *)
                       s =~ ".*/unicode/security/bidi.yml"
                    (* This test now passes. *)
                    (* || s =~ ".*/dockerfile/security/dockerd-socket-mount.yaml" *)
                    || s =~ ".*/yaml/semgrep/consistency/.*" ->
                 Some "XFAIL"
             (* FIXME: the following test seems to have an error in the pattern (has $M(){} should have $T $M(){}) *)
             | s when s =~ ".*/apex/lang/security/ncino/injection/ApexSOQLInjectionUnescapedParam.yaml" -> None
             (* not rule files *)
             | s when s =~ ".*.test.yml" -> None
             (* not languages tests *)
             | s when s =~ ".*/semgrep-rules/stats/" -> None
             (* this was 'todoruleid' and it now passes. *)
             | s when s =~ ".*/semgrep-rules/ocaml/lang/correctness/useless-let.yaml" -> None
             (* these have wrong syntax for comments, we should make them pass and
              * add elsewhere. *)
             | s when s =~ ".*/semgrep-rules/clojure/security/clojure-read-string/" -> None
             | s when s =~ ".*/semgrep-rules/clojure/lang/security/command-injection-shell-call.yaml" -> None
             (* FIXME: This fails, unblocking to create a binary for testing. *)
             | s when s =~ ".*/semgrep-rules/clojure/lang/security/documentbuilderfactory-xxe.yaml" -> None
             (* FIXME: The rule is too restrictive, the patterns should contain a (...); see the comment in PR
             https://github.com/opengrep/opengrep/pull/626 *)
             | s when s =~ ".*/semgrep-rules/ruby/lang/security/weak-hashes-sha1.yaml" -> None
             (* ok let's keep all the other one with the appropriate group name *)
             | s when s =~ ".*/semgrep-rules/\\([a-zA-Z]+\\)/.*" ->
                 (* This is confusing because it looks like a programming
                    language from Lang.t but there's no guarantee that
                    it's a valid one.
                    TODO: don't capitalize? leave a slash? *)
                 let s = Common.matched1 test.name in
                 Some (String.capitalize_ascii s)
             (* TODO: This is not skipped! See above. It should move further up to be
              * excluded! Remove exclusion? *)
             (* this skips a test that incorrectly fails for cross-function tainting (because of false positives) *)
             (* | s when s =~ ".*/semgrep-rules/java/lang/security/audit/xss/no-direct-response-writer.yaml" -> None *)
             (* this skips the semgrep-rules/.github entries *)
             | _ ->
                 Logs.info (fun m -> m "skipping %s" test.name);
                 None
           in
           group_opt |> Option.map (fun groupname -> (groupname, test)))
    |> Assoc.group_assoc_bykey_eff
  in

  Testo.categorize_suites "semgrep-rules repo"
    (groups
    |> List_.map (fun (group, tests) ->
           tests
           |> List_.map (fun (test : Testo.t) ->
                  match group with
                  | "XFAIL" ->
                      (* TODO: populate the excuse below with the exact reason
                         found in the comments above *)
                      Testo.update test
                        ~expected_outcome:
                          (Should_fail
                             "excluded semgrep-rule (see OCaml source file for \
                              details)")
                  | _ -> test)
           |> Testo.categorize group))

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests () =
  List_.flatten
    [
      (* full testing for many languages *)
      lang_regression_tests ~polyglot_pattern_path;
      lang_autofix_tests ~polyglot_pattern_path;
      eval_regression_tests ();
      filter_irrelevant_rules_tests ();
      interfile_taint_tests ();
      interfile_project_graph_tests ();
      maturity_tests ();
      full_rule_taint_maturity_tests ();
      full_rule_regression_tests ();
      semgrep_rules_repo_tests ();
      lang_tainting_tests ();
    ]
