open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Convert results coming from Core_runner (semgrep-core JSON output)
 * to the formally specified Opengrep CLI JSON output.
 *
 * I'm skipping lots of Python code and lots of intermediate modules for now
 * and just go directly from the Core_runner results to the final Cli_output.
 * In the Python codebase it goes through many intermediate data-structures
 * (e.g., RuleMatchMap, SemgrepCoreError, FileTargetingLog, ProfilingData)
 * and many modules:
 *  - scan.py
 *  - semgrep_main.py
 *  - core_runner.py
 *  - core_output.py
 *  - error.py
 *  - output.py
 *  - formatter/base.py
 *  - formatter/json.py
 *)

(*****************************************************************************)
(* Core error to cli error *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

let core_location_to_error_span (loc : Out.location) : Out.error_span =
  {
    file = loc.path;
    start = loc.start;
    end_ = loc.end_;
    source_hash = None;
    config_start = None;
    config_end = None;
    config_path = None;
    context_start = None;
    context_end = None;
  }

(* Generate error message exposed to user *)
let error_message ~rule_id ~(location : Out.location option)
    ~(error_type : Out.error_type) ~core_message : string =
  match error_type with
  (* an error of the command itself (e.g. a scanning root that does not
   * exist, or no config given), not of the scan engine: the message is
   * already complete, as with pysemgrep's SemgrepError *)
  | SemgrepError
  | MissingConfig ->
      core_message
  | _ -> (
      let rule_id_str_opt = Option.map Rule_ID.to_string rule_id in
      let error_context =
        match (rule_id_str_opt, error_type) with
        (* For rule errors, the path is a temporary JSON file containing
           the broken rule(s). *)
        | Some id, (RuleParseError | PatternParseError _) ->
            spf "in rule %s" id
        | ( Some id,
            ( PartialParsing _ | ParseError | OtherParseError
            | AstBuilderError | InvalidYaml | MatchingError
            | SemgrepMatchFound | TooManyMatches | FatalError | Timeout
            | OutOfMemory | TimeoutDuringInterfile
            | OutOfMemoryDuringInterfile ) ) ->
            let suffix =
              match location with
              | None -> ""
              | Some loc -> spf " on %s" !!(loc.path)
            in
            spf "when running %s%s" id suffix
        | Some id, IncompatibleRule _ -> id
        | Some id, MissingPlugin -> spf "for rule %s" id
        | _ -> (
            match location with
            | None -> ""
            | Some loc -> spf "at line %s:%d" !!(loc.path) loc.start.line)
      in
      spf "%s%s:\n %s"
        (Error.string_of_error_type error_type)
        (if String.equal error_context "" then "" else " " ^ error_context)
        core_message)

let error_spans ~(error_type : Out.error_type) ~(location : Out.location) =
  match error_type with
  | PatternParseError _yaml_pathTODO ->
      (* TOPORT
         yaml_path = err.error_type.value.value[::-1]
         spans = [dataclasses.replace(..., config_path=yaml_path)]
      *)
      let span =
        (* This code matches the Python code.
           Not sure what it does, frankly. *)
        {
          (core_location_to_error_span location) with
          config_start = Some (Some { line = 0; col = 1; offset = -1 });
          config_end =
            Some
              (Some
                 {
                   line = location.end_.line - location.start.line;
                   col = location.end_.col - location.start.col + 1;
                   offset = -1;
                 });
        }
      in
      Some [ span ]
  | PartialParsing locs -> Some (locs |> List_.map core_location_to_error_span)
  (* the token of the rule file the error is about *)
  | InvalidRuleSchemaError -> Some [ core_location_to_error_span location ]
  | _else_ -> None

(* # TODO benchmarking code relies on error code value right now
   * # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
*)
let exit_code_of_error_type (error_type : Out.error_type) : Exit_code.t =
  match error_type with
  | ParseError
  | LexicalError
  | PartialParsing _ ->
      Exit_code.invalid_code ~__LOC__
  (* rule errors: the code of the error, which is also the exit code of a
     scan whose rules could not be loaded *)
  | InvalidYaml -> Exit_code.unparseable_yaml ~__LOC__
  | RuleParseError
  | PatternParseError _
  | PatternParseError0
  | InvalidRuleSchemaError ->
      Exit_code.invalid_pattern ~__LOC__
  | OtherParseError
  | AstBuilderError
  | MatchingError
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError
  | Timeout
  | OutOfMemory
  | StackOverflow
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile
  (* TODO? really? fatal for SemgrepWarning? *)
  | SemgrepWarning
  | SemgrepError ->
      Exit_code.fatal ~__LOC__
  | UnknownLanguageError -> Exit_code.invalid_language ~__LOC__
  | MissingConfig -> Exit_code.missing_config ~__LOC__
  | IncompatibleRule _
  | IncompatibleRule0
  | MissingPlugin
  | DependencyResolutionError _ ->
      Exit_code.ok ~__LOC__

(* A parse error quotes the bytes it choked on, so an error entry takes
 * strings from its target just as a match does, and every one of them is
 * sanitised for the same reason (see 'sanitize_cli_match' below).
 *)
let sanitize_cli_error (e : Out.cli_error) : Out.cli_error =
  let sanitize = Option.map String_.sanitize_utf8 in
  {
    e with
    message = sanitize e.message;
    long_msg = sanitize e.long_msg;
    short_msg = sanitize e.short_msg;
    help = sanitize e.help;
  }

(* Skipping the intermediate python SemgrepCoreError for now.
 * TODO: should we return an Error.Semgrep_core_error instead? like we
 * do in python? and then generate an Out.cli_error out of it?
 *)
let cli_error_of_core_error (x : Out.core_error) : Out.cli_error =
  sanitize_cli_error
  @@
  match x with
  | {
   error_type;
   severity;
   location;
   message = core_message;
   rule_id;
   (* LATER *) details = _;
  } ->
      let exit_code = exit_code_of_error_type error_type in
      let rule_id =
        match error_type with
        (* # Rule id not important for parse errors *)
        | ParseError
        | LexicalError
        | PartialParsing _
        | SemgrepWarning
        | SemgrepError
        | MissingConfig ->
            None
        (* pysemgrep's schema validator did not know the rule; our parser
           does *)
        | InvalidRuleSchemaError
        | OtherParseError
        | AstBuilderError
        | RuleParseError
        | PatternParseError _
        | PatternParseError0
        | InvalidYaml
        | UnknownLanguageError
        | MatchingError
        | SemgrepMatchFound
        | TooManyMatches
        | FatalError
        | Timeout
        | OutOfMemory
        | StackOverflow
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | IncompatibleRule _
        | IncompatibleRule0
        | MissingPlugin
        | DependencyResolutionError _ ->
            rule_id
      in
      let path =
        (* # For rule errors path is a temp file so will just be confusing *)
        match error_type with
        | RuleParseError
        | PatternParseError _
        | PatternParseError0 ->
            None
        | ParseError
        | LexicalError
        | PartialParsing _
        | OtherParseError
        | AstBuilderError
        | InvalidYaml
        | InvalidRuleSchemaError
        | UnknownLanguageError
        | MatchingError
        | SemgrepMatchFound
        | TooManyMatches
        | FatalError
        | Timeout
        | OutOfMemory
        | StackOverflow
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | SemgrepWarning
        | SemgrepError
        | MissingConfig
        | IncompatibleRule _
        | IncompatibleRule0
        | MissingPlugin
        | DependencyResolutionError _ ->
            location |> Option.map (fun (x : Out.location) -> x.path)
      in
      let message =
        Some (error_message ~rule_id ~error_type ~location ~core_message)
      in
      let spans =
        let* location = location in
        error_spans ~error_type ~location
      in
      {
        (* LATER? seems to be either 2 (fatal) or 3 (invalid_code), so maybe
         * better to change the ATD spec and use a variant for cli_error.code
         *)
        code = Exit_code.to_int exit_code;
        level = severity;
        type_ = error_type;
        rule_id;
        path;
        message;
        spans;
        (* python: ErrorWithSpan, for the errors on the structure of a rule *)
        long_msg =
          (match error_type with
          | InvalidRuleSchemaError -> Some core_message
          | _ -> None);
        short_msg =
          (match error_type with
          | InvalidRuleSchemaError -> Some "Invalid rule schema"
          | _ -> None);
        help = None;
      }

(*****************************************************************************)
(* Core match to cli match *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

let make_fixed_lines fixes_env fix path (start : Out.position)
    (end_ : Out.position) =
  let edit =
    Textedit.
      { path; start = start.offset; end_ = end_.offset; replacement_text = fix }
  in
  Fixed_lines.make_fixed_lines fixes_env edit

(* A target is a sequence of bytes and nothing guarantees it is UTF-8, but
 * every output format has to be: a JSON or SARIF document with a stray byte
 * in it is rejected by every reader. pysemgrep read its targets with
 * errors="replace", so the bytes it could not decode became U+FFFD; we do the
 * same to the strings a match takes from its target file, once, here.
 *)
let sanitize_cli_match (m : Out.cli_match) : Out.cli_match =
  let sanitize = String_.sanitize_utf8 in
  let sanitize_metavar_value (mval : Out.metavar_value) : Out.metavar_value =
    {
      mval with
      abstract_content = sanitize mval.abstract_content;
      propagated_value =
        mval.propagated_value
        |> Option.map (fun (v : Out.svalue_value) ->
               {
                 v with
                 Out.svalue_abstract_content =
                   sanitize v.Out.svalue_abstract_content;
               });
    }
  in
  let sanitize_intermediate_var (v : Out.match_intermediate_var) :
      Out.match_intermediate_var =
    { v with content = sanitize v.content }
  in
  let rec sanitize_call_trace (trace : Out.match_call_trace) :
      Out.match_call_trace =
    match trace with
    | CliLoc (loc, content) -> CliLoc (loc, sanitize content)
    | CliCall ((loc, content), vars, trace) ->
        CliCall
          ( (loc, sanitize content),
            List_.map sanitize_intermediate_var vars,
            sanitize_call_trace trace )
  in
  let sanitize_dataflow_trace (trace : Out.match_dataflow_trace) :
      Out.match_dataflow_trace =
    {
      taint_source = trace.taint_source |> Option.map sanitize_call_trace;
      intermediate_vars =
        trace.intermediate_vars
        |> Option.map (List_.map sanitize_intermediate_var);
      taint_sink = trace.taint_sink |> Option.map sanitize_call_trace;
    }
  in
  let extra = m.extra in
  {
    m with
    extra =
      {
        extra with
        message = sanitize extra.message;
        lines = sanitize extra.lines;
        fix = extra.fix |> Option.map sanitize;
        fixed_lines = extra.fixed_lines |> Option.map (List_.map sanitize);
        metavars =
          extra.metavars
          |> Option.map
               (List_.map (fun ((name : string), (mval : Out.metavar_value)) ->
                    (name, sanitize_metavar_value mval)));
        dataflow_trace =
          extra.dataflow_trace |> Option.map sanitize_dataflow_trace;
      };
  }

let cli_match_of_core_match ~fixed_lines fixed_env (hrules : Rule.hrules)
    (m : Out.core_match) : Out.cli_match =
  sanitize_cli_match
  @@
  match m with
  | {
   check_id = rule_id;
   path;
   start;
   end_;
   extra =
     {
       message;
       severity;
       metadata;
       metavars;
       engine_kind;
       extra_extra;
       validation_state;
       historical_info;
       fix;
       is_ignored;
       dataflow_trace;
       sca_match;
       enclosing_context
     };
  } ->
      let rule =
        try Hashtbl.find hrules rule_id with
        | Not_found -> raise Impossible
      in
      let rule_message = rule.message in
      let message =
        match message with
        | Some s when not String.(equal empty s) -> s
        | Some _
        | None ->
            rule_message
      in
      let check_id = rule_id in
      let metavars = Some metavars in
      let metadata =
        match metadata with
        | None -> `Assoc []
        | Some json -> json
      in
      (* LATER: this should be a variant in semgrep_output_v1.atd
       * and merged with Constants.rule_severity
       *)
      let severity = severity ||| rule.severity in
      let fixed_lines =
        match (fix, fixed_lines) with
        | None, _
        | _, false ->
            None
        | Some fix, true -> make_fixed_lines fixed_env fix path start end_
      in
      (* Can't use content_of_file_at_range because we want to include the
       * entirety of every line involved in the match, not just the text that
       * matched. *)
      let lines =
        Semgrep_output_utils.lines_of_file_at_range_exn (start, end_) path
      in
      (* python: "".join(rule_match.lines).rstrip() *)
      let lines = lines |> String.concat "\n" |> String_.rstrip in
      {
        check_id;
        path;
        start;
        end_;
        extra =
          {
            metavars;
            lines;
            (* fields derived from the rule (and the match) *)
            message;
            severity;
            metadata;
            fix;
            is_ignored = Some is_ignored;
            fingerprint =
              Semgrep_hashing_functions.Match_based_id.partial rule rule_id
                metavars !!path;
            sca_info = sca_match;
            fixed_lines;
            dataflow_trace;
            (* It's optional in the CLI output, but not in core match results!*)
            engine_kind = Some engine_kind;
            validation_state;
            historical_info;
            extra_extra;
            enclosing_context;
          };
      }

(* This is the same algorithm for indexing as in pysemgrep. We shouldn't need to update this *)
(* match based ids have an index appended at the end which indicates what
 * # finding of that exact id it is in a file. This is used to dedup findings
 * on the app side.
 * Example:
 * foo.py
bad_function() # bad_function is a finding
bad_function() # 2nd call
 * The above findings will have the exact same match based id, but the index
 * will be different. So the first will be <match_based_id>_0 and the second
 * will be <match_based_id>_1.
 *)
let index_match_based_ids (matches : Out.cli_match list) : Out.cli_match list =
  matches
  (* preserve order *)
  |> List_.mapi (fun i x -> (i, x))
  (* Group by rule and path *)
  (* XXX: can we do with grouping by fingerprint only? *)
  |> Assoc.group_by (fun (_, (x : Out.cli_match)) ->
         (x.path, x.check_id, x.extra.fingerprint))
  (* Sort by start line *)
  |> List_.map (fun (path_and_rule_id, matches) ->
         ( path_and_rule_id,
           List.sort
             (fun (_, (a : Out.cli_match)) (_, (b : Out.cli_match)) ->
               compare a.start.offset b.start.offset)
             matches ))
  (* Index per file *)
  |> List_.map (fun (path_and_rule_id, matches) ->
         let matches =
           List_.mapi
             (fun i (i', (x : Out.cli_match)) ->
               ( i',
                 {
                   x with
                   extra =
                     {
                       x.extra with
                       fingerprint = spf "%s_%d" x.extra.fingerprint i;
                     };
                 } ))
             matches
         in
         (path_and_rule_id, matches))
  (* Flatten *)
  |> List.concat_map snd
  |> List.sort (fun (a, _) (b, _) -> a - b)
  |> List_.map snd

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* The 3 regular parameters are mostly Core_runner.result but we don't want
 * to depend on cli_scan/ from reporting/ here, hence the duplication.
 * alt: we could move Core_runner.result type in src/osemgrep/core/
 *)
let cli_output_of_runner_result ~fixed_lines (core : Out.core_output)
    (hrules : Rule.hrules) (scanned : Fpath.t Set_.t) : Out.cli_output =
  match core with
  | {
   version;
   results = matches;
   errors;
   paths =
     {
       skipped;
       (* TODO? should be [] and None given Core_json_output.ml code *)
       scanned = _;
     };
   skipped_rules;
   explanations;
   interfile_languages_used;
   time;
   (* LATER *)
   rules_by_engine = _;
   engine_requested = _;
  } ->
      (* TODO: not sure how it's sorted. Look at rule_match.py keys? *)
      (* The fixed lines of overlapping fixes go to the first finding in
         reported order, the one whose fix is applied. *)
      let matches = Semgrep_output_utils.sort_core_matches_as_reported matches in
      (* TODO: not sure how it's sorted, but Set_.elements return
       * elements in OCaml compare order (=~ lexicographic for strings)
       * python: scanned=[str(path) for path in sorted(self.all_targets)]
       *)
      let scanned = scanned |> Set_.elements in
      (* Skipping the python intermediate FileTargetingLog for now.
       * We used to have a cli_skipped_target and core_skipped_target type,
       * but now they are merged so this function is the identity.
       * In theory we could remove the details: and rule_id: from it
       * because they used to not be included in the final JSON output
       * (but the info was used in the text output to display skipping
       * information).
       *
       * Still? skipped targets are coming from the FileIgnoreLog which is
       * populated from many places in the code.
       * Still? see _make_failed_to_analyze() in output.py,
       * core_failure_lines_by_file in target_manager.py
       * Still? need to sort
       *)
      let (paths : Out.scanned_and_skipped) = { scanned; skipped } in
      let skipped_rules =
        (* TODO: return skipped_rules with --develop
           if maturity = Develop then
             invalid_rules
           else
        *)
        (* compatibility with pysemgrep *)
        ignore skipped_rules;
        []
      in
      let fixed_env = Fixed_lines.mk_env () in
      {
        version = Some version;
        (* Skipping the python intermediate RuleMatchMap for now *)
        results =
          matches
          |> List_.map (cli_match_of_core_match ~fixed_lines fixed_env hrules)
          |> Semgrep_output_utils.sort_cli_matches;
        errors = errors |> List_.map cli_error_of_core_error;
        paths;
        skipped_rules;
        explanations;
        interfile_languages_used;
        time;
        (* LATER *)
        rules_by_engine = None;
        engine_requested = None;
      }
