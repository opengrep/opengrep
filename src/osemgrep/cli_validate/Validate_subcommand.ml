(* Yoann Padioleau
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a semgrep-validate command, execute it and return an exit code.
 *
 * This module performs rule validation by checking whether a rule is
 * correct by detecting different kinds of errors:
 *
 *  (1) YAML syntax errors (e.g., unclosed quote). We detect those
 *    errors thanks to our YAML parser and Yaml_to_generic.ml
 *
 *  (2) Semgrep rule schema errors (e.g., missing 'message:' field). We
 *    detect those thanks to Parse_rule.ml
 *    Note that Parse_rule.ml also parses the patterns and validates they
 *    have the right syntax (unlike jsonschema). This is why
 *    osemgrep validate accepts a --pro flag as opposed to pysemgrep because
 *    we need access to the pro languages parsers to parse the pro languages
 *    patterns.
 *    TODO: use the OCaml grace library of Cooper for better error messaging?
 *     or fallback to jsonschema in pysemgrep just for the error message like
 *     zz did?
 *
 *  (3) "Logical" errors, by running Semgrep rules (also called "meta" rules)
 *     on those target rules. We run Semgrep on Semgrep! We detect
 *     those thanks to the 'p/semgrep-rule-lints' ruleset.
 *     (hence the need for the network capability and Core_scan.caps below)
 *
 *  (4) TODO Other errors that can't be detected easily using Semgrep rules. We
 *    detect those thanks to Check_rule.ml.
 *
 * IMPORTANT: 'validate' is meant to validate *local* rules (a file or a
 * directory of rules). The metachecks in (3) run on the rule files on disk,
 * so only rules that come from a local config are fully validated. Rules
 * fetched from a non-local config (registry 'p/...'/'r/...', a URL, or a
 * 'git+<url>' repo) are still parsed, so (1) and (2) apply, but they provide
 * no persistent file to metacheck and are therefore skipped in (3). When no
 * config is local we emit a warning (see find_targets_rules) so this is not
 * silent.
 *
 * For more info see
 * https://semgrep.dev/docs/writing-rules/testing-rules#validating-rules
 *
 * Note that there was no 'pysemgrep validate' subcommand. Rule validation
 * was run with 'semgrep scan --validate ...' but it's better to have a separate
 * subcommand. Note that the legacy 'semgrep scan --validate' is redirected to
 * this file after having built a compatible Validate_CLI.conf.
 *
 * LATER: get rid of semgrep-core -check_rules and cleanup duplicated code
 * in Check_rule.ml.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* Cap.stdout + Core_scan.caps + network (we run metachecking rules) *)
(* TODO: should use stdout, right now we abuse Logs.app
 * TODO? why Cap.tmp?
 *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(* The "meta" rules are stored in the semgrep-rules public repository here:
 * https://github.com/semgrep/semgrep-rules/tree/develop/yaml/semgrep
 *
 * The pack below is defined in the semgrep-rules-pack private repository here:
 * https://github.com/semgrep/semgrep-rule-packs/blob/master/semgrep-rule-lints.json
 *)
let metarules_pack = "p/semgrep-rule-lints"

(*****************************************************************************)
(* Pro hooks *)
(*****************************************************************************)

(*****************************************************************************)
(* Targeting (finding the semgrep yaml files to validate) *)
(*****************************************************************************)
let find_targets_rules (caps : < caps ; .. >) ~(strict : bool)
    (rules_source : Rules_source.t) :
    Fpath.t list * int * Core_error.t list * Core_error.t list =
  (* Checking (1) and (2). Parsing the rules is already a form of validation.
   * Before running metachecks on those rules, we make sure we can parse them.
   * TODO: report not only Rule.invalid_rule_errors but all Rule.Error.t for (1)
   * in Config_resolver.errors.
   * TODO? need the network here since anyway we filter_map registry config
   * later. We currently abuse the ability for --config and rules_source
   * to specify a dir but since anyway we want to filter yaml files
   * that don't look like rules, probably better do our own file
   * targeting here, especially in osemgrep validate which does not need
   * to be backward compatible.
   *)
  let rules_and_origin, fatal_errors =
    Rule_fetching.rules_from_rules_source ~rewrite_rule_ids:true
      ~strict
      (caps :> < Cap.network ; Cap.tmp >)
      rules_source
  in
  (* ex: missing toplevel 'rules:' (probably not a semgrep rule file) *)
  let pp_rule_error (err : Core_error.t) : unit =
    (* alt: Error.abort *)
    Logs.warn (fun m ->
        m "%s"
          (Fmt_.with_buffer_to_string (fun ppf ->
               Rule_errors_report.pp_errors ppf [ err ])
          |> String.trim))
  in
  let fatal_core_errors =
    fatal_errors |> List_.map Core_error.error_of_rule_error
  in
  fatal_core_errors |> List.iter pp_rule_error;
  let rules, invalid_rules =
    Rule_fetching.partition_rules_and_invalid rules_and_origin
  in
  let invalid_rule_errors =
    invalid_rules
    |> List_.map (fun (err : Rule_error.invalid_rule) ->
           match err with
           (* to get the "Missing semgrep extension ... install --pro" error *)
           (* alt: just warn *)
           | MissingPlugin s, _, _ -> Error.abort s
           | _ ->
               let core_error = Core_error.error_of_invalid_rule err in
               pp_rule_error core_error;
               core_error)
  in
  (* In a validate context, rules are actually targets of metarules.
   * alt: could also process Configs to compute the targets.
   *)
  (* TODO(cooper): don't understand motivation of this filter_map. Not
   * sure why we wouldn't do this on non-local files (understand for
   * registry)
   *
   * Seems to be because we can't easily get the tmpfile and we are still
   * entirely file-oriented rather than being able to scan buffers.
   *)
  let targets =
    rules_and_origin
    |> List_.filter_map (fun (x : Rule_fetching.rules_and_origin) ->
           match x.origin with
           | Local_file path -> Some path
           | CLI_argument
           | Registry
           | Untrusted_remote _
           | Git_repo _ ->
               (* These origins don't provide a local rule file to run the
                * metachecks against (registry/URL/git+ rules are fetched into
                * transient files that are already gone). They are still parsed
                * above, but not metachecked. See the warning below. *)
               None)
  in
  (* Metachecks run on the rule *files*, which only local configs provide. If
   * no config resolved to a local file, no rule is metachecked at all. The
   * rules were still parsed (a first form of validation), but warn so this
   * weaker gate is not silent. *)
  if List_.null targets && not (List_.null rules) then
    Logs.warn (fun m ->
        m
          "no rules were metachecked: none of the given configs is a local \
           file or directory (registry, URL and git+ configs are parsed but \
           not metachecked)");
  (targets, List.length rules, fatal_core_errors, invalid_rule_errors)

(*****************************************************************************)
(* Checking the rules *)
(*****************************************************************************)

(* Checking (3) *)
let check_targets_rules (caps : < caps ; .. >) targets_rules
    core_runner_conf =
  let in_docker = !Semgrep_envvars.v.in_docker in
  let (config : Rules_config.t) =
    Rules_config.parse_config_string ~in_docker metarules_pack
  in
  (* There should not be any errors, because we got these rules online. *)
  let metarules_and_origin, _errors =
    Rule_fetching.rules_from_dashdash_config
      ~rewrite_rule_ids:true (* default *)
      (caps :> < Cap.network ; Cap.tmp >)
      config
  in
  let metarules, metaerrors =
    Rule_fetching.partition_rules_and_invalid metarules_and_origin
  in
  if metaerrors <> [] then
    Error.abort (spf "error in metachecks! please fix %s" metarules_pack);

  (* TODO? why using Core_runner instead of directly Core_scan? *)
  let core_run_func =
    Core_runner.mk_core_run_for_osemgrep (Core_scan.scan caps)
  in
  let result_or_exn =
    core_run_func.run ~git_repo:false core_runner_conf
      (* These two configs are irrelevant to the "validate" subcommand *)
      Find_targets.default_conf Match_patterns.default_matching_conf
      (metarules, []) targets_rules
  in

  let results =
    match result_or_exn with
    | Error exn -> Exception.reraise exn
    | Ok result ->
        let res = Core_runner.mk_result metarules result in
        (* TODO? sanity check errors below too? *)
        let Out.{ results; errors = _; _ } : Out.cli_output =
          Cli_json_output.cli_output_of_runner_result ~fixed_lines:false
            res.core res.hrules res.scanned
        in
        (* TOPORT?
                          ... run -check_rules in semgrep-core ...
                          parsed_errors += [
                            core_error_to_semgrep_error(e) for e in core_output.errors
           s               ]
                          return dedup_errors(parsed_errors)
                       ...
                       def dedup_errors(errors: List[SemgrepCoreError]) -> List[SemgrepCoreError]:
                          return list({uniq_error_id(e): e for e in errors}.values())

                     def uniq_error_id(
                         error: SemgrepCoreError,
                     ) -> Tuple[int, Path, core.Position, core.Position, str]:
                         return (
                             error.code,
                             Path(error.core.location.path),
                             error.core.location.start,
                             error.core.location.end,
                             error.core.message,
                         )
        *)
        (* metarules match results are actually metacheck errors *)
        results
  in
  (* TODO: checking (4) *)
  results

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

(* the error of a metacheck match, as pysemgrep reported it in the errors of
 * its output.
 * coupling: with Check_rule.error and use of SemgrepMatchFound *)
let core_error_of_metacheck_error (x : Out.cli_match) : Core_error.t =
  let loc : Tok.location =
    {
      str = "";
      pos =
        {
          bytepos = x.start.offset;
          line = x.start.line;
          column = x.start.col - 1;
          file = x.path;
        };
    }
  in
  Core_error.mk_error ~msg:x.extra.message ~loc Out.SemgrepMatchFound

(* TODO: use CapConsole not Logs.app ? *)
let report_errors (_caps : < Cap.stdout >) ~metacheck_errors ~num_errors
    ~num_fatal_errors ~num_rules =
  (* was logger.info, but works without --verbose, so Logs.app better *)
  Logs.app (fun m ->
      m
        "Configuration is %s - found %d fatal errors, %d skippable error(s), \
         and %d rule(s)."
        (if num_errors + num_fatal_errors =|= 0 then "valid" else "invalid")
        num_fatal_errors num_errors num_rules);
  (* coupling: with Check_rule.error and use of SemgrepMatchFound *)
  metacheck_errors
  |> List.iter (fun (x : Out.cli_match) ->
         Logs.err (fun m ->
             m "Opengrep match found at line %s:%d\n%s" !!(x.path) x.start.line
               x.extra.message));
  ()

(*****************************************************************************)
(* Run the conf *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (conf : Validate_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:conf.force_color
    ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Validate_CLI.show_conf conf));

  (* step1: getting the targets (which contain rules) *)
  let targets_rules, num_rules, fatal_errors, invalid_rule_errors =
    find_targets_rules caps ~strict:conf.core_runner_conf.strict
      conf.rules_source
  in

  (* step2: checking the rules *)
  let metacheck_errors =
    check_targets_rules caps targets_rules conf.core_runner_conf
  in

  (* step3: summarizing findings (errors) *)
  (* the fatal errors, usually a file that is not a rule file, count too:
   * the report calls such a configuration invalid, so the run must fail *)
  let num_errors =
    List.length invalid_rule_errors + List.length metacheck_errors
  in
  let num_fatal_errors = List.length fatal_errors in
  report_errors
    (caps :> < Cap.stdout >)
    ~metacheck_errors ~num_errors ~num_fatal_errors ~num_rules;

  (* step3': the same errors as the document of the format asked for, as
   * pysemgrep's output handler emitted them for 'scan --validate --json' *)
  let errors : Core_error.t list =
    fatal_errors @ invalid_rule_errors
    @ List_.map core_error_of_metacheck_error metacheck_errors
  in
  if conf.json then
    Output.output_result ~keep_ignored:false
      (caps :> < Cap.stdout >)
      { Output.default with output_format = Output_format.Json }
      (Profiler.make ())
      (Core_runner.mk_result [] (Core_result.mk_result_with_just_errors errors))
    |> ignore;

  (* step4: exit code. A configuration the report calls invalid always fails,
   * with the code of the last error as pysemgrep's _final_raise gave it. *)
  match List.rev errors with
  | [] -> Exit_code.ok ~__LOC__
  | (last : Core_error.t) :: _ ->
      (* was a raise SemgrepError originally *)
      Logs.err (fun m -> m "Please fix the above errors and try again.");
      Cli_json_output.exit_code_of_error_type last.typ

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Validate_CLI.parse_argv argv in
  run_conf caps conf
