(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
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
open Fpath_.Operators
module Out = Semgrep_output_v1_j
module A = Test_annotation

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a semgrep-test command, execute it and return an exit code.
 *
 * There are multiple ways to call `semgrep test` but the main one is to call
 * it with a directory as an argument as in `semgrep test semgrep-rules/ocaml`
 * which will run all the tests in the directory recursively.
 * For each directory containing YAML rules, it run those rules on the file in
 * the same directory with the same name but a different extension
 * (e.g., eqeq.yaml runs on eqeq.py). It then validates that the output is
 * annotated in the source file with by looking for a comment like:
 * ```
 * # ruleid:eqeq-is-bad
 * ```
 * On the preceeding line.
 *
 * For more info on how to use the Semgrep rule testing infrastructure, see
 * https://semgrep.dev/docs/writing-rules/testing-rules/.
 *
 * Note that there was no 'pysemgrep test' subcommand. Tests were run via
 * 'semgrep scan --test ...' but it's better to have a separate subcommand.
 * Note that the legacy 'semgrep scan --test' is redirected to this file after
 * having built a compatible Test_CLI.conf.
 *
 * TODO: conf.ignore_todo? conf.strict?
 * LATER: factorize code with Unit_engine.ml and Test_engine.ml
 *
 * This is a port of test.py
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* = Cap.stdout + Core_scan.caps + Cap.tmp (for Deep_scan.caps)
 * (no need for Cap.network; the tested rules should be local)
 *)
type caps =
  < Cap.stdout ; Cap.fork ; Cap.time_limit ; Cap.memory_limit ; Cap.tmp >

(* Core_scan.caps | Deep_scan.caps *)
type scan_caps = < Cap.fork ; Cap.time_limit ; Cap.memory_limit ; Cap.tmp >

(* Rules and targets to test together.
 * Usually the target list contains just one file, but in some cases
 * one rule can be tested on multiple files such as a with a .js and a .ts
 * LATER: for interfile tests, it will be normal to have multiple targets.
 *)
type tests = (Fpath.t (* rule file *) * Fpath.t list (* targets *)) list

type tests_result =
  (Fpath.t (* rule file *) * test_result list * fixtest_result list) list

(* Intermediate type because semgrep_output_v1.atd does not have
 * a good type name for those. Note that we can't easily change the .atd
 * because we must remain backward compatible with pysemgrep and current
 * users of semgrep --test.
 * For example, the Out.checks type introduces a useless intermediate record
 * and the rule ids are strings instead of a proper type, but because
 * we use <json repr="object">, we can't even use a proper wrap rule_id type
 * for it. At least here we use Rule_ID.t.
 *)
and test_result = Rule_ID.t * Out.rule_result

(* TODO? add diff between .fixed and actual for error management? *)
and fixtest_result = {
  target : Fpath.t;
  fixtest : Fpath.t;
  (* from the fixtest to the fixed target, empty when the fixtest passed *)
  diff : string list;
  result : Out.fixtest_result;
}

(* TODO: define clearly in semgrep_output_v1.atd config_with_errors type
 * and also the errors in rule_result.
 * type config_with_error_output = ...
 * The config_error type of the .atd has no message field, so UnparsableRule
 * below keeps the message of the rule loader for the text report.
 *)
type error =
  (* there is a rule but there is no target file *)
  | MissingTest of Fpath.t (* rule file *)
  (* the rule when applied produces fixes, but there is no .fixed file *)
  | MissingFixtest of Fpath.t (* rule file *)
  (* the rule file could not be loaded, with the message of the loader *)
  | UnparsableRule of Fpath.t (* rule file *) * string (* error message *)
  (* the rule ids the annotations of a test file name are not the ones that
     matched in it *)
  | RuleIdMismatch of Fpath.t (* test file *) * Rule_ID.t list (* unmatched *)

(* to avoid having functions with lots of parameters *)
type env = {
  (* currently processed rule and targets files *)
  rule_file : Fpath.t;
  target_files : Fpath.t list;
  (* use a ref so easy to store all the errors returned by different functions.
   * alt: get each functions returning different kind of errors
   *)
  errors : error list ref;
  conf : Test_CLI.conf;
}

(* TODO: move in core/ ? used in other files? was in constants.py in pysemgrep *)
let break_line =
  "--------------------------------------------------------------------------------"

(*****************************************************************************)
(* File targeting (the set of tests) *)
(*****************************************************************************)

(* TODO? Move to Rule_tests.ml? *)
let find_targets_for_rule (rule_file : Fpath.t) : Fpath.t list =
  let dir, base = Fpath.split_base rule_file in
  (* ex: "useless-if" (without the ".yaml") *)
  let base_no_ext = Fpath.rem_ext base in
  dir |> List_files.read_dir_entries_fpath
  |> List_.exclude (fun p ->
         Fpath.equal p base || List.mem "fixed" (Fpath_.exts p))
  |> List_.filter_map (fun p ->
         (* the ~multi:true should then handle the foo.test.yaml *)
         if Fpath.equal (Fpath.rem_ext ~multi:true p) base_no_ext then
           Some (dir // p)
         else None)

let rules_and_targets (kind : Test_CLI.target_kind) (errors : error list ref) :
    tests =
  match kind with
  | Test_CLI.Dir (dir, None) ->
      (* coupling: similar to Test_engine.test_rules() *)
      let rule_files =
        [ dir ] |> UFile.files_of_dirs_or_files_no_vcs_nofilter
        |> List.filter Rule_file.is_valid_rule_filename
        (* a rule file listed under '.' is reported without the './' prefix *)
        |> List_.map Fpath_.strip_leading_dot_and_trailing_slash
      in
      rule_files
      |> List_.filter_map (fun (rule_file : Fpath.t) ->
             match find_targets_for_rule rule_file with
             | [] ->
                 (* stricter: (but reported via config_missing_tests in JSON)*)
                 Logs.warn (fun m ->
                     m "could not find target for %s" !!rule_file);
                 Stack_.push (MissingTest rule_file) errors;
                 None
             | xs ->
                 Logs.debug (fun m ->
                     m "found targets for %s: %s" !!rule_file
                       (xs |> List_.map Fpath.to_string |> String.concat ", "));
                 Some (rule_file, xs))
  | Test_CLI.Files (targets, config_str) -> (
      match Rules_config.parse_config_string ~in_docker:false config_str with
      | File rule_file -> [ (rule_file, targets) ]
      | Dir _
      | URL _
      | Git _
      | R _ ->
          (* stricter: *)
          failwith "the config must be a local file")
  (* this is to allow to have the rules in a different directories than
   * the targets as in `osemgrep test --config tests/rules/ tests/targets/
   * see https://semgrep.dev/docs/writing-rules/testing-rules#storing-rules-and-test-targets-in-different-directories
   *)
  | Test_CLI.Dir (dir_targets, Some config_str) -> (
      match Rules_config.parse_config_string ~in_docker:false config_str with
      | Dir dir_rules ->
          let rule_files = Rule_tests.get_config_filenames dir_rules in
          Rule_tests.get_config_test_filenames ~original_config:dir_rules
            ~configs:rule_files ~original_target:dir_targets
          |> List_.filter_map (fun (rule_file, targets) ->
                 if List_.null targets then (
                   Logs.warn (fun m ->
                       m "could not find target for %s" !!rule_file);
                   Stack_.push (MissingTest rule_file) errors;
                   None)
                 else (
                   Logs.debug (fun m ->
                       m "found targets for %s: %s" !!rule_file
                         (targets |> List_.map Fpath.to_string
                         |> String.concat ", "));
                   Some (rule_file, targets)))
      | File _
      | URL _
      | Git _
      | R _ ->
          (* stricter: *)
          failwith "the config must be a local directory")

(*****************************************************************************)
(* Fixtest *)
(*****************************************************************************)

(* TODO? Move to Rule_tests.ml? *)
let fixtest_of_target_opt (target : Fpath.t) : Fpath.t option =
  let stem, ext = Fpath_.split_ext ~multi:true target in
  let fixtest = stem |> Fpath.add_ext (".fixed" ^ ext) in
  if Sys.file_exists !!fixtest then Some fixtest else None

let fixtest_result_for_target (_env : env) (target : Fpath.t)
    (fixtest : Fpath.t) (pms : Core_match.t list) : fixtest_result =
  Logs.info (fun m -> m "Using %s for fixtest" !!fixtest);
  let (textedits : Textedit.t list) =
    pms |> List.concat_map (fun pm -> Autofix.render_fix pm |> Option.to_list)
  in
  (* stricter? *)
  if List_.null textedits then
    Logs.info (fun m -> m "no autofix generated for %s" !!target);

  let expected_content = UFile.read_file fixtest in
  let actual_res =
    Textedit.apply_edits_to_text target (UFile.read_file target) textedits
  in
  (* the diff is printed by the final report *)
  let (diff : string list) =
    match actual_res with
    | Textedit.Success actual_content ->
        if String.equal expected_content actual_content then []
        else Unified_diff.lines ~old_:expected_content ~new_:actual_content
    | Overlap _ ->
        Logs.err (fun m -> m "fixes overlap for %s" !!target);
        (* TODO? return an error instead ?*)
        [ "the fixes overlap" ]
  in
  { target; fixtest; diff; result = Out.{ passed = List_.null diff } }

(* LATER: still enough in steps mode? *)
let rule_contain_fix_or_fix_regex (rule : Rule.t) : bool =
  match rule with
  | { fix = Some _; _ }
  | { fix_regexp = Some _; _ } ->
      true
  | _else_ -> false

(*****************************************************************************)
(* Diagnosis (Brandon's experiment) *)
(*****************************************************************************)

let report_diagnosis print (res : Out.tests_result) : unit =
  let diagnoses =
    res.results
    |> List.concat_map (fun (rule_file, (checks : Out.checks)) ->
           checks.checks
           |> List.concat_map (fun (_rule_id, (rule_res : Out.rule_result)) ->
                  match rule_res.diagnosis with
                  | Some d -> [ (rule_file, d) ]
                  | None -> []))
  in
  if List.length diagnoses <> 0 then (
    print break_line;
    print "Matching diagnosis:";
    diagnoses
    |> List.iter (fun (rule_file, d) ->
           print (Diagnosis.report ~rule_file:(Fpath.v rule_file) d)))

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

let tests_result_of_tests_result (results : tests_result) (errors : error list)
    : Out.tests_result =
  let fixtest_results : (string * Out.fixtest_result) list =
    results
    |> List.concat_map (fun (_rule_file, _checks, fixtest_results) ->
           fixtest_results
           |> List_.map (fun (fixtest_result : fixtest_result) ->
                  (!!(fixtest_result.target), fixtest_result.result)))
  in
  Out.
    {
      results =
        results
        |> List_.map (fun (rule_file, checks, _fix) ->
               ( !!rule_file,
                 {
                   checks =
                     checks
                     |> List_.map (fun (id, xs) -> (Rule_ID.to_string id, xs));
                 } ));
      fixtest_results;
      (* TODO: change the schema and use an enum instead of those fields *)
      config_missing_tests =
        errors
        |> List_.filter_map (function
             | MissingTest rule_file -> Some rule_file
             | _else_ -> None)
        |> List.sort Fpath.compare;
      config_missing_fixtests =
        errors
        |> List_.filter_map (function
             | MissingFixtest rule_file -> Some rule_file
             | _else_ -> None)
        (* a rule file with several test targets pushes one MissingFixtest per
         * target; python: test.py lists each rule file once *)
        |> List.sort_uniq Fpath.compare;
      (* TODO: rename to just 'errors' and put the missing_tests and missing
       * fixtests here as a kind of error.
       *)
      config_with_errors =
        errors
        |> List_.filter_map (function
             | UnparsableRule (rule_file, _msg) -> Some rule_file
             | _else_ -> None)
        |> List.sort Fpath.compare
        |> List_.map (fun (file : Fpath.t) ->
               { Out.file; reason = `UnparsableRule });
    }

(* python: _generate_check_output_line *)
let pp_failed_check ppf ((rule_id : string), (rule_res : Out.rule_result)) :
    unit =
  let pp_lines ppf (lines : int list) =
    Format.fprintf ppf "[%s]"
      (lines |> List_.map Int.to_string |> String.concat ", ")
  in
  Format.fprintf ppf "\t✖ %s@\n" rule_id;
  (* python: the soft errors of the check, which are tolerated by a scan but
     fatal here, so that a timeout or a target that does not parse is not
     reported as empty line lists *)
  (match rule_res.errors with
  | [] -> ()
  | errors ->
      let json =
        errors
        |> List_.map Semgrep_output_v1_j.string_of_cli_error
        |> String.concat ","
        |> fun (s : string) -> Yojson.Safe.prettify (spf "[%s]" s)
      in
      Format.fprintf ppf "\terrors: %s@\n" json);
  rule_res.matches
  |> List.iter (fun (_file, (m : Out.expected_reported)) ->
         let _common, missed, incorrect =
           Common2.diff_set_eff m.expected_lines m.reported_lines
         in
         Format.fprintf ppf "\tmissed lines: %a, incorrect lines: %a@\n"
           pp_lines missed pp_lines incorrect);
  Format.fprintf ppf "\ttest file path: %s@\n"
    (rule_res.matches |> List_.map fst |> String.concat " ")

(* python: _generate_fixcheck_output_line *)
let pp_failed_fixtest ppf (fixtest_result : fixtest_result) : unit =
  Format.fprintf ppf "\t✖ %s <> autofix applied to %s@\n@\n"
    !!(fixtest_result.fixtest) !!(fixtest_result.target);
  fixtest_result.diff
  |> List.iter (fun (line : string) -> Format.fprintf ppf "\t%s@\n" line)

let report_tests_result (caps : < Cap.stdout >) ~matching_diagnosis ~json
    ~(config_errors : (Fpath.t * string) list) (res : Out.tests_result)
    (fixtest_results : fixtest_result list) : unit =
  let print str = CapConsole.print caps#stdout str in
  if json then
    let s = Out.string_of_tests_result res in
    print s
  else
    (* the failed checks, sorted by rule id within each rule file as in
       pysemgrep *)
    let failed_checks : (string * Out.rule_result) list =
      res.results
      |> List.concat_map (fun (_rule_file, (checks : Out.checks)) ->
             checks.checks
             |> List.filter (fun (_rule_id, (rule_res : Out.rule_result)) ->
                    not rule_res.passed)
             |> List.sort (fun (a, _) (b, _) -> String.compare a b))
    in
    let failed_fixtests =
      fixtest_results
      |> List.filter (fun (r : fixtest_result) -> not r.result.passed)
    in
    let total =
      res.results
      |> List_.map (fun (_rule_file, (checks : Out.checks)) ->
             List.length checks.checks)
      |> Common2.sum
    in
    let passed = total - List.length failed_checks in
    let fixtest_total = List.length res.fixtest_results in
    let fixtest_passed = fixtest_total - List.length failed_fixtests in
    let print_failures : 'a. (Format.formatter -> 'a -> unit) -> 'a list -> unit
        =
     fun pp failures ->
      print break_line;
      (* the blocks separated by a blank line, and one after the last *)
      failures
      |> List_.map (fun failure ->
             Fmt_.with_buffer_to_string (fun ppf -> pp ppf failure))
      |> String.concat "\n" |> print
    in

    (* "unit" tests *)
    (match () with
    | _ when total =|= 0 ->
        (* TODO: exit error code instead? *)
        print
          "No unit tests found. See \
           https://semgrep.dev/docs/writing-rules/testing-rules"
    | _ when passed =|= total ->
        print (spf "%d/%d: ✓ All tests passed" passed total)
    | _else_ ->
        print
          (spf "%d/%d: %d unit tests did not pass:" passed total
             (total - passed));
        print_failures pp_failed_check failed_checks);
    (* fix tests *)
    (match () with
    | _ when fixtest_total =|= 0 -> print "No tests for fixes found."
    | _ when fixtest_passed =|= fixtest_total ->
        print (spf "%d/%d: ✓ All fix tests passed" fixtest_passed fixtest_total)
    | _else_ ->
        print
          (spf "%d/%d: %d fix tests did not pass:" fixtest_passed fixtest_total
             (fixtest_total - fixtest_passed));
        print_failures pp_failed_fixtest failed_fixtests);
    if matching_diagnosis then report_diagnosis print res;
    (* the rule files that could not be loaded, as in pysemgrep *)
    match config_errors with
    | [] -> ()
    | _ :: _ ->
        print break_line;
        print "The following config files produced errors:";
        print
          ("\t"
          ^ (config_errors
            |> List_.map (fun ((rule_file : Fpath.t), (msg : string)) ->
                   spf "%s: %s" !!rule_file msg)
            |> String.concat "\n\t"))

(*****************************************************************************)
(* Calling the engine *)
(*****************************************************************************)

(* There are multiple entry points to the "engine":
 *  - 1: matching/Match_patterns.check(), many patterns vs 1 target,
 *       but no rule (no formula)
 *  - 2: engine/Match_search_mode.check_rule(), 1 (search) rule vs 1 target,
 *       but just search rule
 *  - 3: engine/Match_rules.check(), many rules vs 1 target,
 *       but just the checking part, and for just one target
 *  - 3': engine/Test_engine.check(), which is used by semgrep-core -test_rules,
 *       and make core-test, and which calls Match_rules.check(),
 *       but too tied to our semgrep-core test infra (Testo)
 *  - 4: core_scan/Core_scan.scan(), many rules vs many targets in //, and
 *       also handle nosemgrep, and errors, and cache, and many other things,
 *       but require complex arguments (a Core_scan_config)
 *       update: Core_scan_config.t is now simpler and smaller
 *  - 5: core_scan/Pre_post_core_scan.call_with_pre_and_post_processor()
 *       to handle autofix and secrets validations
 *  - 6: osemgrep/core_runner/Core_runner.mk_scan_func_for_osemgrep()
 *       to fit osemgrep,
 *       but it requires even more complex arguments than Core_scan.scan()
 *  - 7: osemgrep/cli_scan/Scan_subcommand.run_scan_conf()
 *       but requires a dependency to cli_scan/, and is a bit heavyweight
 *       for our need which is just to run a few rules on a target test file.
 *
 * For 'osemgrep test', it is better to call Core_scan.scan(), especially
 * now that Core_scan_config.t has been simplified. We used to call
 * Match_rules.check() and use a few helpers from Test_engine.ml,
 * but this was then difficult to extend to support --pro. By using
 * Core_scan.scan(), it's relatively easy to add hooks to switch to
 * Deep_scan.scan() for pro rules and interfile tests.
 * Using Core_scan.scan() would also make it easier to support extract rules.
 *
 * See also server/src/.../Studio_service.ml comment
 * on where to plug to the semgrep engine.
 *)

let core_scan_config (conf : Test_CLI.conf) (rules : Rule.t list)
    (targets : Target.t list) : Core_scan_config.t =
  {
    Core_scan_config.default with
    rule_source = Rules rules;
    target_source = Targets targets;
    output_format = NoOutput;
    (* activate matching explanations for Diagnosis to work *)
    matching_explanations = conf.matching_diagnosis;
    (* try to be as close as possible as a real scan to avoid differences
     * between semgrep test and semgrep scan behavior
     *)
    filter_irrelevant_rules = true;
    (* in a test context, we don't want to honor the paths: (include/exclude)
     * directive since the test target file, which must have the same
     * basename without the extension than the rule, may not match the
     * paths: directive of the rule
     *)
    respect_rule_paths = false;
    taint_intrafile = conf.taint_intrafile;
    (* the ignore annotations of a scan apply to a test run too *)
    engine_config =
      {
        Engine_config.default with
        custom_ignore_pattern = conf.opengrep_ignore_pattern;
      };
    (* without the flags we run the limits of a scan, so that a rule that
     * never finishes fails the test instead of hanging *)
    timeout =
      Option.value conf.timeout ~default:Core_runner.default_conf.timeout;
    timeout_threshold =
      Option.value conf.timeout_threshold
        ~default:Core_runner.default_conf.timeout_threshold;
    max_memory_mb =
      Option.value conf.max_memory_mb
        ~default:Core_runner.default_conf.max_memory_mb;
    effect_guards = false
  }

let run_rules_against_targets caps (env : env) (rules : Rule.t list)
    (targets : Target.t list) : Core_result.t =
  (* old:
   * let xtarget = Test_engine.xtarget_of_file xlang target in
   * let xconf = { Match_env.default_xconfig with matching_explanations = true} in
   * Match_rules.check ~match_hook:(fun _ ->()) ~timeout:None xconf rules xtarget
   *)
  let config = core_scan_config env.conf rules targets in
  match Core_scan.scan caps config with
  | Error exn -> Exception.reraise exn
  (* TODO? fail early or add a kind of error in the json output?
     | Ok { errors = _x::_; _} -> failwith "TODO"
  *)
  | Ok res -> res

(* The annotations that take part in the comparison: an 'ok:' line expects
 * nothing. Todoruleid is kept so that the comparison below can drop the
 * lines it annotates from both the expected and the reported set, as
 * test.py does.
 *)
let expected_annots (annots : A.annotations) : A.annotations =
  annots
  |> List.filter (fun ((annot : A.t), (_ : A.linenb)) ->
         match annot.kind with
         | Ruleid
         | Todook
         | Todoruleid ->
             true
         | Ok -> false)

(*****************************************************************************)
(* Comparing *)
(*****************************************************************************)

let diff_findings (actual : int list) (expected : int list) : string =
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff expected actual
  in
  (if List_.null only_in_expected then ""
   else
     spf "missing findings lines %s."
       (only_in_expected |> List_.map Int.to_string |> String.concat ", "))
  ^
  if List_.null only_in_actual then ""
  else
    spf "unexpected findings lines %s."
      (only_in_actual |> List_.map Int.to_string |> String.concat ", ")

(* alt: use Test_compare_matches.compare_actual_to_expected but
 * it does not handle the actual rule id in the annotations and is
 * not compatible with what 'pysemgrep test' was doing when comparing.
 *)
let compare_actual_to_expected (env : env) (matches : Core_match.t list)
    (annots : (Fpath.t * A.annotations) list)
    (explanations : Matching_explanation.t list option)
    ~(errors : Out.cli_error list) : test_result list =

  (* cf. src/reporting/Core_json_output.ml, function [process_matches_with_rule_options]. *)
  let rule_opts =
    Core_match.to_rule_id_options_map matches
  in
  let restrict_matches_using_options options matches =
    match options with
    | Some Core_match.{max_match_per_file = Some limit; _}
      when List.length matches > limit ->
      List_.take (max limit 0) matches (* pre: already sorted *)
    | _ -> matches
  in
  (* actual matches *)
  let matches_by_ruleid_and_file :
      (Rule_ID.t, (Fpath.t, Core_match.t list) Assoc.t) Assoc.t =
    if List_.null matches then
      (* stricter: *)
      Logs.info (fun m -> m "nothing matched for %s" !!(env.rule_file));
    matches
    |> Assoc.group_by (fun (pm : Core_match.t) -> pm.rule_id.id)
    |> List_.map (fun (rule_id, pms) ->
           ( rule_id,
             pms
             |> Assoc.group_by (fun (pm : Core_match.t) ->
                    (* We need Fpath.normalize because for unclear reasons DeepScan
                     * returns matches with paths that may differ from
                     * the one below in the annotations so simpler to normalize
                     * both so path like ./foo/bar.c and foo/bar.c are considered
                     * the same
                     * TODO: we don't need that for CoreScan and ProScan so we
                     * should probably fix DeepScan instead to not mess up
                     * with the Targets paths.
                     *)
                    Fpath.normalize pm.path.internal_path_to_content) ))
  in
  (* expected matches *)
  let expected_by_ruleid_and_file :
      (Rule_ID.t, (Fpath.t, A.linenb list) Assoc.t) Assoc.t =
    let h = Hashtbl.create 101 in
    annots
    |> List.iter (fun (file, annotations) ->
           let file = Fpath.normalize file in
           let expected_by_rule_id : (Rule_ID.t, A.linenb list) Assoc.t =
             A.group_by_rule_id annotations
           in
           expected_by_rule_id
           |> List.iter (fun (rule_id, lines) ->
                  Hashtbl_.push h rule_id (file, lines)));
    h |> Hashtbl_.map (fun _k vref -> !vref) |> Hashtbl_.hash_to_list
  in

  let all_rule_ids : Rule_ID.t list =
    Assoc.join_keys matches_by_ruleid_and_file expected_by_ruleid_and_file
  in
  (* regular ruleid tests *)
  let checks : (Rule_ID.t * Out.rule_result) list =
    all_rule_ids
    |> List_.map (fun (id : Rule_ID.t) ->
           let actual : (Fpath.t, Core_match.t list) Assoc.t =
             matches_by_ruleid_and_file |> Assoc.find_opt id
             |> List_.optlist_to_list
           in
           let expected : (Fpath.t, A.linenb list) Assoc.t =
             expected_by_ruleid_and_file |> Assoc.find_opt id
             |> List_.optlist_to_list
           in
           let all_files : Fpath.t list = Assoc.join_keys actual expected in
           let rule_opts_for_id = (Rule_ID.Map.find_opt id rule_opts) in
           let res : (bool * (Fpath.t * Out.expected_reported)) list =
             all_files
             |> List_.map (fun (target : Fpath.t) ->
                    let matches : Core_match.t list =
                      actual |> Assoc.find_opt target |> List_.optlist_to_list
                    in
                    let (reported_lines : A.linenb list) =
                      matches
                      |> List_.map (fun (pm : Core_match.t) ->
                             pm.range_loc |> fst |> fun (loc : Loc.t) ->
                             loc.pos.line)
                      |> List.sort_uniq Int.compare
                      |> (restrict_matches_using_options rule_opts_for_id)
                    in
                    let expected_lines : A.linenb list =
                      expected |> Assoc.find_opt target |> List_.optlist_to_list
                      |> List.sort_uniq Int.compare
                    in
                    (* python: test.py takes the todook: and todoruleid: lines
                     * out of both sets and compares what is left, so a line
                     * whose annotation says "do not judge me" cannot fail the
                     * check, whichever way it went. The same two sets are what
                     * the JSON reports; only the annotations of the rule
                     * being compared apply.
                     *)
                    let file_annots =
                      Assoc.find_opt target annots |> List_.optlist_to_list
                      |> List.filter (fun (((annot : A.t), _line) : A.t * A.linenb) ->
                             Rule_ID.equal annot.id id)
                    in
                    let reported_lines =
                      A.filter_todo file_annots reported_lines
                    in
                    let expected_lines =
                      A.filter_todo file_annots expected_lines
                    in
                    let passed = reported_lines =*= expected_lines in
                    (* the final report prints the failures *)
                    if not passed then
                      Logs.info (fun m ->
                          m "test failed for rule id %s on target %s (%s)"
                            (Rule_ID.to_string id) !!target
                            (diff_findings reported_lines expected_lines));
                    let expected_reported =
                      { Out.reported_lines; expected_lines }
                    in
                    (passed, (target, expected_reported)))
           in
           let diagnosis =
             let* explanations = explanations in
             match res with
             | [ (_passed, (target, expected_reported)) ] ->
                 Some
                   (Diagnosis.diagnose ~target ~rule_file:env.rule_file
                      expected_reported explanations)
             (* a rule with several test targets (e.g. a .js and a .ts one)
              * is ordinary; the diagnosis has no way to attribute the
              * explanations to one of them, so it gives none rather than
              * stopping the run *)
             | _ ->
                 Logs.info (fun m ->
                     m "no matching diagnosis for %s: %d test targets"
                       (Rule_ID.to_string id) (List.length res));
                 None
           in
           let (rule_result : Out.rule_result) =
             Out.
               {
                 passed =
                   res |> List_.map fst |> Common2.and_list
                   && List_.null errors;
                 matches =
                   res
                   |> List_.map (fun (_passed, (target, expected_reported)) ->
                          (* TODO: not sure why but pysemgrep uses realpaths
                           * here, which is a bit annoying because it forces
                           * us to use masks in test snapshots
                           *)
                          let filename = Unix.realpath !!target in
                          (filename, expected_reported));
                 (* like pysemgrep, the errors of the rule file's run are
                  * attached to each of its checks *)
                 errors;
                 diagnosis;
               }
           in
           (id, rule_result))
  in
  checks

let compare_for_autofix (env : env) (rules : Rule.t list)
    (matches : Core_match.t list) : fixtest_result list =
  env.target_files
  |> List_.filter_map (fun target ->
         match
           ( fixtest_of_target_opt target,
             rules |> List.exists rule_contain_fix_or_fix_regex )
         with
         | None, true ->
             (* stricter: (reported in JSON at least via config_missing_fixtests) *)
             Logs.warn (fun m ->
                 m "no fixtest for test %s but the rule file %s uses autofix"
                   !!target !!(env.rule_file));
             Stack_.push (MissingFixtest env.rule_file) env.errors;
             None
         | Some fixtest, false ->
             (* stricter? *)
             Logs.err (fun m ->
                 m
                   "found the fixtest %s but the rule file %s does not contain \
                    autofix"
                   !!fixtest !!(env.rule_file));
             None
         | None, false -> None
         | Some fixtest_target, true ->
             let matches =
               matches
               |> List.filter (fun (pm : Core_match.t) ->
                      Fpath.equal pm.path.internal_path_to_content target)
             in
             Some (fixtest_result_for_target env target fixtest_target matches))

(*****************************************************************************)
(* Run the tests *)
(*****************************************************************************)

(* alt: call it run_env? *)
let run_engine (caps : < scan_caps ; .. >) (env : env) (rules : Rule.t list)
    (targets : Target.t list)
    (files_and_annots : (Fpath.t * A.annotations) list) :
    test_result list * fixtest_result list =
  let res : Core_result.t = run_rules_against_targets caps env rules targets in
  let expected : (Fpath.t * A.annotations) list =
    files_and_annots
    |> List_.map (fun ((file : Fpath.t), (annots : A.annotations)) ->
           (file, expected_annots annots))
  in
  let matches =
    res.processed_matches
    (* python: a match on a line with a nosem annotation, or with the one of
     * --opengrep-ignore-pattern, is not reported, as in a scan *)
    |> List_.exclude (fun (x : Core_result.processed_match) -> x.is_ignored)
    |> List_.map (fun (x : Core_result.processed_match) -> x.pm)
  in
  (* python: check_rule_id_mismatch. The rule ids the annotations of a file
   * name must be the ones that matched in it: a misspelt id, or a rule that
   * matched without any annotation, fails the run. *)
  if not (List_.null matches) then
    files_and_annots
    |> List.iter (fun ((file : Fpath.t), (annots : A.annotations)) ->
           let ids (xs : Rule_ID.t list) : Rule_ID.t list =
             List.sort_uniq Rule_ID.compare xs
           in
           let annotated =
             ids (annots |> List_.map (fun ((a : A.t), (_ : A.linenb)) -> a.id))
           in
           let reported =
             matches
             |> List.filter (fun (pm : Core_match.t) ->
                    Fpath.equal
                      (Fpath.normalize pm.path.internal_path_to_content)
                      (Fpath.normalize file))
             |> List_.map (fun (pm : Core_match.t) -> pm.rule_id.id)
             |> ids
           in
           if not (List.equal Rule_ID.equal annotated reported) then (
             let unmatched =
               List.filter
                 (fun (id : Rule_ID.t) ->
                   not (List.exists (Rule_ID.equal id) reported))
                 annotated
             in
             Logs.err (fun m ->
                 m
                   "Found rule id mismatch - file=%s 'ruleid' annotation with \
                    no YAML rule=%s"
                   !!file
                   (unmatched |> List_.map Rule_ID.to_string
                  |> String.concat ", "));
             Stack_.push (RuleIdMismatch (file, unmatched)) env.errors));
  let checks =
    compare_actual_to_expected env matches expected res.explanations
      ~errors:
        (res.errors
        |> List_.map (fun (err : Core_error.t) ->
               Cli_json_output.cli_error_of_core_error
                 (Core_json_output.error_to_error err)))
  in
  (* optional fixtest *)
  let fixtest = compare_for_autofix env rules matches in
  (checks, fixtest)

(* run the tests of one rule file *)
let run_test (caps : < scan_caps ; .. >) (conf : Test_CLI.conf)
    (rule_file : Fpath.t) (rules : Rule.t list) (target_files : Fpath.t list)
    (errors : error list ref) : test_result list * fixtest_result list =
  (* note that even one target file can result in different targets
   * if the rules contain multiple xlangs.
   *)
  let targets : Target.t list =
    Core_runner.targets_for_files_and_rules target_files rules
  in
  let files_and_annots : (Fpath.t * A.annotations) list =
    target_files |> List_.map (fun file -> (file, A.annotations file))
  in

  let env = { rule_file; target_files; conf; errors } in
  run_engine caps env rules targets files_and_annots

let run_tests (caps : < scan_caps ; .. >) (conf : Test_CLI.conf) (tests : tests)
    (errors : error list ref) :
    (Fpath.t (* rule file *) * test_result list * fixtest_result list) list =
  (* LATER: in theory we could use Parmap here *)
  tests
  |> List_.filter_map (fun (rule_file, target_files) ->
         Logs.info (fun m -> m "processing rule file %s" !!rule_file);
         (* as in pysemgrep: the run goes on with the other rule files and
          * the file is reported in config_with_errors
          *)
         let unparsable (msg : string) =
           Logs.warn (fun m -> m "could not load %s: %s" !!rule_file msg);
           Stack_.push (UnparsableRule (rule_file, msg)) errors;
           None
         in
         (* TODO? sanity check? call metachecker Check_rule.check()? *)
         match Parse_rule.parse_and_filter_invalid_rules rule_file with
         | Ok (rules, []) ->
             Logs.info (fun m ->
                 m "processing target(s) %s"
                   (target_files |> Fpath_.to_strings |> String.concat ", "));
             let checks, fixtest =
               run_test caps conf rule_file rules target_files errors
             in
             Some (rule_file, checks, fixtest)
         (* capture 's' and return it in the error so the user will see something
          * like "Missing semgrep extenstion needed for parsing X. Try --pro"
          *)
         | Ok (_, (MissingPlugin s, _, _) :: _)
         | Error { kind = InvalidRule (MissingPlugin s, _, _); _ } ->
             (* alt: could Stack_.push (MissingPlugin rule_file) errors *)
             raise
               (Error.Semgrep_error (s, Some (Exit_code.missing_config ~__LOC__)))
         | Ok (_, invalid_rule :: _) ->
             unparsable (Rule_error.string_of_invalid_rule invalid_rule)
         | Error err -> unparsable (Rule_error.string_of_error err)
         | (exception Parsing_error.Syntax_error _)
         | (exception Parsing_error.Other_error _) ->
             failwith "impossible: Parse_rule should not raise exns anymore")

(*****************************************************************************)
(* Run the conf *)
(*****************************************************************************)
let run_conf (caps : < caps ; .. >) (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:conf.force_color
    ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));
  let matching_diagnosis = conf.matching_diagnosis in
  let errors = ref [] in

  (* step1: compute the set of tests (rule + target) *)
  (* We now support multiple targets (e.g., .jsx/.tsx) analyzed independently.
   * TODO: multiple targets analyzed together for --pro interfile analysis.
   *)
  let tests : tests = rules_and_targets conf.target errors in

  (* step2: run the tests *)
  let result : tests_result = run_tests caps conf tests errors in
  (* python: check_rule_id_mismatch exits before any report *)
  if
    !errors
    |> List.exists (function
         | RuleIdMismatch _ -> true
         | _ -> false)
  then (
    Logs.err (fun m ->
        m
          "Failing due to rule id mismatch. There is a test denoted with \
           'ruleid: <rule name>' where the rule name does not exist or is not \
           expected in the test file.");
    Exit_code.fatal ~__LOC__)
  else
  (* step3: report the test results *)
  let res : Out.tests_result = tests_result_of_tests_result result !errors in
  (* the message of the loader, which the JSON config_with_errors cannot hold *)
  let config_errors : (Fpath.t * string) list =
    !errors
    |> List_.filter_map (function
         | UnparsableRule (rule_file, msg) -> Some (rule_file, msg)
         | _else_ -> None)
    |> List.sort (fun (a, _) (b, _) -> Fpath.compare a b)
  in
  (* pysemgrep is reporting some "successfully modified 1 file."
   * before the final report, but actually it reports that even on failing
   * fixtests, so better to not imitate for now.
   *)
  (* final report *)
  report_tests_result
    (caps :> < Cap.stdout >)
    ~matching_diagnosis ~json:conf.json ~config_errors res
    (result |> List.concat_map (fun (_rule_file, _checks, fixtests) -> fixtests));

  (* step4: compute the exit code *)

  (* A rule file that does not load fails the run, with or without --strict:
   * our loader rejects a pattern or a regex that does not compile, which
   * pysemgrep only found at scan time, where it failed the checks of that
   * file. The file is still reported and the other files are still tested.
   *)
  let config_error = not (List_.null res.config_with_errors) in
  let any_failures =
    res.results
    |> List.exists (fun (_rule_file, (checks : Out.checks)) ->
           checks.checks
           |> List.exists (fun (_rule_id, (res : Out.rule_result)) ->
                  not res.passed))
  in
  let any_fixtest_failures =
    res.fixtest_results
    |> List.exists (fun (_target_file, (res : Out.fixtest_result)) ->
           not res.passed)
  in
  if config_error || any_failures || any_fixtest_failures then
    Exit_code.findings ~__LOC__
  else Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Test_CLI.parse_argv argv in
  run_conf caps conf
