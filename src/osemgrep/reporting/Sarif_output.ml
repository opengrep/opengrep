open Common
module Out = Semgrep_output_v1_t
module Sarif = Sarif.Sarif_v_2_1_0_v

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Formats the CLI output to the SARIF format using the sarif OPAM package.
 *
 * Originally written based on:
 *  - https://help.github.com/en/github/finding-security-vulnerabilities-and-errors-in-your-code/about-sarif-support-for-code-scanning
 *   - Which links to this schema:
 *     https://github.com/oasis-tcs/sarif-spec/blob/master/Schemata/sarif-schema-2.1.0.json
 *
 * Full spec:
 *  https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html (2023)
 *
 * Ported from formatters/sarif.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* SARIF v2.1.0-compliant severity string.
 * See the "level" property in the spec
 * See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
 *)
let severity_of_severity sev : Sarif.notification_level =
  match sev with
  | `Info
  | `Low ->
      `Note
  | `Warning
  | `Medium ->
      `Warning
  (* both critical and high are mapped to the same `Error *)
  | `Error
  | `Critical
  | `High ->
      `Error
  | `Experiment
  | `Inventory ->
      raise Todo

(* GitHub Code Scanning security-severity score mapping.
 * See https://docs.github.com/en/code-security/code-scanning/integrating-with-code-scanning/sarif-support-for-code-scanning#reportingdescriptor-object
 * GitHub translates numerical scores as follows:
 *   over 9.0 = critical
 *   7.0 to 8.9 = high
 *   4.0 to 6.9 = medium
 *   0.1 to 3.9 = low
 *)
let security_severity_of_severity sev : string =
  match sev with
  | `Critical -> "9.0"
  | `High | `Error -> "8.0"
  | `Medium | `Warning -> "6.0"
  | `Low | `Info -> "3.0"
  | `Experiment | `Inventory -> "1.0"

let message ?markdown text = Sarif.create_message ?markdown ~text ()

let multiformat_message ?markdown text =
  Sarif.create_multiformat_message_string ?markdown ~text ()

let region ?message ?snippet (start : Out.position) (end_ : Out.position) =
  (* The sarif package is a bit annoying by using int64 for posititons *)
  let start_line = Int64.of_int start.line
  and start_column = Int64.of_int start.col
  and end_line = Int64.of_int end_.line
  and end_column = Int64.of_int end_.col in
  let snippet =
    Option.map (fun text -> Sarif.create_artifact_content ~text ()) snippet
  in
  Sarif.create_region ~start_line ~start_column ~end_line ~end_column ?message
    ?snippet ()

(* Tags to display on SARIF-compliant UIs, such as GitHub security scans. *)
let tags_of_metadata metadata =
  (* XXX: Tags likely have to be strings, but what do we do with non-string json?! *)
  let best_effort_string = function
    | JSON.String s -> s
    | non_string -> JSON.string_of_json non_string
  in
  (* Also add the "security" tag when the rule has CWE tags. *)
  let cwe =
    match JSON.member "cwe" metadata with
    | Some (JSON.Array cwe) -> List_.map best_effort_string cwe @ [ "security" ]
    | Some single_cwe -> [ best_effort_string single_cwe; "security" ]
    | None -> []
  in
  let owasp =
    match JSON.member "owasp" metadata with
    | Some (JSON.Array owasp) ->
        List_.map (fun o -> "OWASP-" ^ best_effort_string o) owasp
    | Some o -> [ "OWASP-" ^ best_effort_string o ]
    | None -> []
  in
  let confidence =
    match JSON.member "confidence" metadata with
    | Some c -> [ best_effort_string c ^ " CONFIDENCE" ]
    | None -> []
  in
  let semgrep_policy_slug =
    match JSON.member "semgrep.policy" metadata with
    | Some (JSON.Object _ as sp) -> (
        match JSON.member "slug" sp with
        | Some slug -> [ best_effort_string slug ]
        | None -> [])
    | Some _
    | None ->
        []
  in
  let tags =
    match JSON.member "tags" metadata with
    | Some (JSON.Array tags) -> List_.map best_effort_string tags
    | Some _
    | None ->
        []
  in
  let all_tags = cwe @ owasp @ confidence @ semgrep_policy_slug @ tags in
  List.sort_uniq String.compare all_tags

(* Check if a rule is security-related based on its metadata.
 * A rule is considered security-related if it has CWE, OWASP tags, or
 * an explicit "security" tag in its metadata.
 *)
let is_security_rule metadata =
  let has_cwe = JSON.member "cwe" metadata <> None in
  let has_owasp = JSON.member "owasp" metadata <> None in
  let has_security_tag =
    match JSON.member "tags" metadata with
    | Some (JSON.Array tags) ->
        List.exists
          (function
            | JSON.String s -> String.lowercase_ascii s = "security"
            | _ -> false)
          tags
    | _ -> false
  in
  has_cwe || has_owasp || has_security_tag

(* We want to produce a json object? with the following shape:
   { id; name;
     shortDescription; fullDescription;
     helpUri; help;
     defaultConfiguration = { level };
     properties }
*)
let rule (rule_id, rule) : Sarif.reporting_descriptor =
  let metadata = rule.Rule.metadata ||| JSON.Null in
  let short_description =
    match JSON.member "shortDescription" metadata with
    | Some (JSON.String shortDescription) -> shortDescription
    | Some _ -> raise Impossible
    | None -> spf "Opengrep Finding: %s" (Rule_ID.to_string rule_id)
  and source =
    match JSON.member "source" metadata with
    | Some (JSON.String source) -> Some source
    | Some _
    | None ->
        None
  and rule_help_text =
    match JSON.member "help" metadata with
    | Some (JSON.String txt) -> txt
    | Some _
    | None ->
        rule.message
  in
  let security_severity =
    (* Use explicit security-severity from metadata if provided,
     * otherwise compute it from rule severity for security-related rules.
     * This enables GitHub Code Scanning to display findings with
     * security severities (Critical/High/Medium/Low) instead of
     * quality severities (Error/Warning/Note).
     * See https://github.com/opengrep/opengrep/issues/540
     *)
    match JSON.member "security-severity" metadata with
    | Some json ->
        [ ("security-severity", (JSON.to_yojson json :> Yojson.Safe.t)) ]
    | None when is_security_rule metadata ->
        [ ("security-severity", `String (security_severity_of_severity rule.severity)) ]
    | None -> []
  in
  let properties =
    let tags = tags_of_metadata metadata in
    [
      ("precision", `String "very-high");
      ("tags", `List (List_.map (fun s -> `String s) tags));
    ]
    @ security_severity
  in
  let references =
    Option.to_list (Option.map (fun s -> spf "[Rule](%s)" s) source)
  in
  let other_references =
    match JSON.member "references" metadata with
    | Some (JSON.String s) -> [ spf "[%s](%s)" s s ]
    | Some (JSON.Array xs) ->
        List_.map
          (function
            | JSON.String s -> spf "[%s](%s)" s s
            | non_string -> JSON.string_of_json non_string)
          xs
    | Some _
    | None ->
        []
  in
  let references_joined =
    List_.map (fun s -> spf " - %s\n" s) (references @ other_references)
  in
  let references_markdown =
    match references_joined with
    | [] -> ""
    | xs -> "\n\n<b>References:</b>\n" ^ String.concat "" xs
  in
  Sarif.create_reporting_descriptor
    ~id:(Rule_ID.to_string rule_id)
    ~name:(Rule_ID.to_string rule_id)
    ~short_description:(multiformat_message short_description)
    ~full_description:(multiformat_message rule.message)
    ~default_configuration:
      (Sarif.create_reporting_configuration
         ~level:(severity_of_severity rule.severity)
         ())
    ~help:
      (multiformat_message
         ~markdown:(rule_help_text ^ references_markdown)
         rule_help_text)
    ?help_uri:source ~properties ()

let sarif_fixes (cli_match : Out.cli_match) : Sarif.fix list option =
  let* fixed_lines = cli_match.extra.fixed_lines in
  let description_text =
    spf "%s\n Autofix: rule suggested fix" cli_match.extra.message
  in
  let fix =
    let artifact_change =
      Sarif.create_artifact_change
        ~artifact_location:
          (Sarif.create_artifact_location
             ~uri:(Fpath.to_string cli_match.path)
             ())
        ~replacements:
          [
            Sarif.create_replacement
              ~deleted_region:(region cli_match.start cli_match.end_)
              ~inserted_content:
                (Sarif.create_artifact_content
                   ~text:(String.concat "\n" fixed_lines)
                   ())
              ();
          ]
        ()
    in
    Sarif.create_fix ~description:(message description_text)
      ~artifact_changes:[ artifact_change ] ()
  in
  Some [ fix ]

let thread_flow_location message
    (location : Out.location) content nesting_level =
  let location =
    Sarif.create_location ~message
      ~physical_location:
        (Sarif.create_physical_location
           ~region:
             (region ~message ~snippet:content location.start location.end_)
           ~artifact_location:
             (Sarif.create_artifact_location
                ~uri:(Fpath.to_string location.path)
                ())
           ())
      ()
  in
  Sarif.create_thread_flow_location
    ~nesting_level:(Int64.of_int nesting_level)
    ~location ()

let intermediate_var_locations intermediate_vars =
  intermediate_vars
  |> List_.map (fun ({ location; content } : Out.match_intermediate_var) ->
         let propagation_message_text =
           spf "Propagator : '%s' @ '%s:%d'" content
             (Fpath.to_string location.path)
             location.start.line
           |> message
         in
         thread_flow_location propagation_message_text location
           content 0)

let rec final_location_of_trace (mct: Out.match_call_trace) : Out.location =
  match mct with
  | CliLoc (location, _content) -> location
  | CliCall (_, _, mct) -> final_location_of_trace mct

type flow_kind = Source | Sink

let rec thread_flows
    ?(nesting_level = 0)
    ~(flow_kind : flow_kind)
    (match_call_trace: Out.match_call_trace) =
  match match_call_trace with
  | CliLoc (location, content) ->
    let loc_message_text =
      spf "%s: '%s' @ '%s:%d'"
        (match flow_kind with Source -> "Source" | Sink -> "Sink")
        content
        (Fpath.to_string location.path)
        location.start.line
      |> message
    in
    [ thread_flow_location loc_message_text location content nesting_level ]
  | CliCall ((location, content), intermediate_vars, nested_match_call_trace) ->
    let nested_flows = 
      thread_flows
        ~nesting_level:(nesting_level + 1)
        ~flow_kind
        nested_match_call_trace
    in
    let call_message_text =
      spf "Call: '%s' @ '%s:%d'"
        content
        (Fpath.to_string location.path)
        location.start.line
      |> message
    in
    let call_flow_location =
      thread_flow_location call_message_text location content nesting_level
    in
    let intermediate_var_locations =
      intermediate_var_locations intermediate_vars
    in
    match flow_kind with
    | Source ->
      nested_flows @ (intermediate_var_locations @ [ call_flow_location ])
    | Sink ->
      call_flow_location :: (intermediate_var_locations @ nested_flows)

let sarif_codeflow (cli_match : Out.cli_match) : Sarif.code_flow list option =
  match cli_match.extra.dataflow_trace with
  | None
  | Some { Out.taint_source = None; _ } ->
      None
  | Some { Out.taint_source = Some _; taint_sink = None ; _ } ->
      None (* TODO: raise, this should not happen. *)
  | Some
      ({ taint_source = Some source_mct;
         intermediate_vars;
         taint_sink = Some sink_mct })
    ->
      let location = final_location_of_trace source_mct
      in
      (* TODO from sarif.py: handle taint_sink *)
      let code_flow_message =
        spf "Untrusted dataflow from %s:%d to %s:%d"
          (Fpath.to_string location.path)
          location.start.line
          (Fpath.to_string cli_match.path)
          cli_match.start.line
      in
      let source_flows =
        thread_flows
          ~flow_kind:Source
          source_mct
      in
      let intermediate_var_locations =
        match intermediate_vars with
        | None -> []
        | Some intermediate_vars ->
          intermediate_var_locations intermediate_vars
      in
      let sink_flows =
        thread_flows
          ~flow_kind:Sink
          sink_mct
      in
      let thread_flows =
        [ Sarif.create_thread_flow
          ~locations:
            (source_flows @ intermediate_var_locations @ sink_flows)
          ()
        ]
      in
      Some
        [
          Sarif.create_code_flow
            ~message:(message code_flow_message)
            ~thread_flows ();
        ]

let result show_dataflow_traces
    (cli_match : Out.cli_match) : Sarif.result =
  let location =
    let physical_location =
      Sarif.create_physical_location
        ~artifact_location:
          (Sarif.create_artifact_location
             ~uri:(Fpath.to_string cli_match.path)
             ~uri_base_id:"%SRCROOT%" ())
        ~region:
          (region ~snippet:cli_match.extra.lines cli_match.start cli_match.end_)
        ()
    in
    Sarif.create_location ~physical_location ()
  in
  let suppressions =
    match cli_match.extra.is_ignored with
    | None
    | Some false ->
        None
    | Some true -> Some [ Sarif.create_suppression ~kind:`InSource () ]
  in
  let fixes = sarif_fixes cli_match in
  let code_flows =
    if show_dataflow_traces then sarif_codeflow cli_match else None
  in
  let properties =
    match Exposure.of_cli_match_opt cli_match with
    | None -> []
    | Some exposure -> [ ("exposure", `String (Exposure.string_of exposure)) ]
  in
  let fingerprints =
    [ ("matchBasedId/v1", cli_match.extra.fingerprint) ]
  in
  Sarif.create_result
    ~rule_id:(Rule_ID.to_string cli_match.check_id)
    ~message:(message cli_match.extra.message)
    ~locations:[ location ] ~fingerprints ~properties ?code_flows ?fixes
    ?suppressions ()

let error_to_sarif_notification (e : Out.cli_error) =
  let level = severity_of_severity e.level in
  let message = message (e.message ||| (e.long_msg ||| (e.short_msg ||| ""))) in
  let descriptor =
    Sarif.create_reporting_descriptor_reference
      ~id:(Error.string_of_error_type e.type_)
      ()
  in
  Sarif.create_notification ~message ~descriptor ~level ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let sarif_output hrules (cli_output : Out.cli_output)
    ~hide_nudge ~engine_label ~show_dataflow_traces : Sarif.sarif_json_schema =
  ignore hide_nudge;
  let sarif_schema =
    "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
  in
  let show_dataflow_traces = show_dataflow_traces in
  let run =
    let rules =
      Some
        (hrules |> Hashtbl.to_seq |> List.of_seq
        (* sorting for snapshot stability *)
        |> List.sort (fun (aid, _) (bid, _) -> Rule_ID.compare aid bid)
        |> List_.map rule)
    in
    let tool =
      let driver =
        Sarif.create_tool_component
          ~name:(spf "Opengrep %s" engine_label)
          ~semantic_version:Version.version ?rules ()
      in
      Sarif.create_tool ~driver ()
    in
    let results =
      cli_output.results |> Semgrep_output_utils.sort_cli_matches
      |> List_.map (result show_dataflow_traces)
    in
    let invocation =
      (* TODO no test case(s) for executionNotifications being non-empty *)
      let tool_execution_notifications =
        cli_output.errors
        |> List.sort (fun (a : Out.cli_error) (b : Out.cli_error) ->
               match (a.path, b.path) with
               (* less: could sort more *)
               | Some a1, Some b1 -> Fpath.compare a1 b1
               | _else_ -> Stdlib.compare a b)
        |> List_.map error_to_sarif_notification
      in
      Sarif.create_invocation ~execution_successful:true
        ~tool_execution_notifications ()
    in
    Sarif.create_run ~tool ~results ~invocations:[ invocation ] ()
  in
  Sarif.create_sarif_json_schema ~version:`TwoDotOneDotZero ~schema:sarif_schema
    ~runs:[ run ] ()
