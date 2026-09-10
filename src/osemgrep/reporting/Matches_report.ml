module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The legacy skin's findings: the group headings, the block of each finding,
 * and the "RULES FIRED" sections of a ci run.
 *
 * The wrapping and the code-line rendering it uses are in Findings_layout,
 * shared with the other skins.
 *)

open Findings_layout

type report_group =
  [ OutJ.validation_state
  | `Unreachable
  | `Undetermined
  | `Reachable
  | `Nonblocking
  | `Blocking
  | `Merged ]

let group_titles : report_group -> string = function
  | `Unreachable -> "Unreachable Supply Chain Finding"
  | `Undetermined -> "Undetermined Supply Chain Finding"
  | `Reachable -> "Reachable Supply Chain Finding"
  | `Nonblocking -> "Non-blocking Code Finding"
  | `Blocking -> "Blocking Code Finding"
  | `Merged -> "Code Finding"
  | `Confirmed_valid -> "Valid Secrets Finding"
  | `Confirmed_invalid -> "Invalid Secrets Finding"
  | `Validation_error -> "Secrets Validation Error"
  | `No_validator -> "Unvalidated Secrets Finding"

let sort_by_groups als =
  (* This is the order that groups will be desplayed in. *)
  let group_order : report_group -> int = function
    | `Blocking -> 1
    | `Reachable -> 2
    | `Confirmed_valid -> 3
    | `Undetermined -> 4
    | `Validation_error -> 5
    | `No_validator -> 6
    | `Nonblocking -> 7
    | `Unreachable -> 8
    | `Confirmed_invalid -> 9
    | `Merged -> 10
  in
  let compare_group x y = group_order x - group_order y in
  als |> List.stable_sort (Common.on compare_group fst)

let one_line_of_code (code : string) : string =
  code |> String.split_on_char '\n' |> List_.map String.trim
  |> List.filter (fun (s : string) -> not (String.equal s ""))
  |> String.concat " "

let pp_sources_of_sink ppf (findings : OutJ.cli_match list) : unit =
  findings
  |> List.iter (fun (finding : OutJ.cli_match) ->
         match finding.extra.dataflow_trace with
         | Some { OutJ.taint_source = Some source; _ } ->
             let loc, code = Core_json_output.leaf_of_call_trace source in
             Fmt.pf ppf "%s source %a:%d  %a@." findings_indent
               Fmt.(styled (`Fg `Cyan) string)
               !!(loc.path) loc.start.line
               Fmt.(styled `Bold string)
               (one_line_of_code code)
         | Some _
         | None ->
             ())

let pp_finding ~max_chars_per_line ~max_lines_per_finding
    ~show_dataflow_traces ~append_separator
    ~(is_interfile : Rule_ID.t -> bool)
    ~(sink_findings : OutJ.cli_match list) ppf (m : OutJ.cli_match) =
  let lines =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
  in
  let lines, dedented = dedent_lines lines in
  let lines, trimmed =
    let ll = List.length lines in
    let max_lines =
      if max_lines_per_finding = 0 then ll else max_lines_per_finding
    in
    let keep = min ll max_lines in
    if keep = ll then (lines, None)
    else (List_.take keep lines, Some (ll - keep))
  in
  let start_line = m.start.line in
  (* python: per_line_max_chars_limit, the whole rendered line being
     wrapped at --max-chars-per-line, or at the width of the findings
     block when the flag asks for more *)
  let width =
    safe_width
      (if max_chars_per_line > 0 then min max_chars_per_line findings_text_width
       else findings_text_width)
  in
  lines
  |> List.iteri (fun (i : int) (line : string) ->
         let line_number = start_line + i in
         let col c = max 0 (c - 1 - dedented) in
         let bold_start = if line_number > start_line then 0 else col m.start.col in
         let bold_end =
           max bold_start
             (if line_number >= m.end_.line then
                min
                  (if m.start.line = m.end_.line then
                     bold_start + (m.end_.col - m.start.col)
                   else col m.end_.col)
                  (String.length line)
              else String.length line)
         in
         (* TODO(secrets): Apply masking to the bold part *)
         pp_wrapped_code_line ppf ~line_number ~width ~bold_start ~bold_end
           line);
  if is_interfile m.check_id then pp_sources_of_sink ppf sink_findings;
  (if show_dataflow_traces then
     sink_findings
     |> List.iter (fun (finding : OutJ.cli_match) ->
            match finding.extra.dataflow_trace with
            | Some trace ->
                pp_dataflow_trace ~finding_path:finding.path ppf trace
            | None -> ()));
  match trimmed with
  | Some num ->
      Fmt.pf ppf
        "%s [hid %d additional lines, adjust with --max-lines-per-finding]@."
        findings_indent num
  | None ->
      if append_separator then
        Fmt.pf ppf "%s⋮┆%s" findings_indent (String.make fill_count '-')

(* TODO: factorize more this code, just the color and >>> change below *)
let pp_styled_severity ppf (severity : OutJ.match_severity) =
  match severity with
  | `Critical ->
      Fmt.pf ppf "%s%a" rule_leading_indent
        Fmt.(styled (`Fg `Magenta) string)
        "❯❯❯❱"
  | `Error
  | `High ->
      Fmt.pf ppf "%s%a" rule_leading_indent Fmt.(styled (`Fg `Red) string) "❯❯❱"
  | `Warning
  | `Medium ->
      Fmt.pf ppf "%s%a" rule_leading_indent
        Fmt.(styled (`Fg `Yellow) string)
        " ❯❱"
  | `Info
  | `Low ->
      Fmt.pf ppf "%s%a" rule_leading_indent
        Fmt.(styled (`Fg `Green) string)
        "  ❱"
  | `Inventory
  | `Experiment ->
      Fmt.pf ppf "%s%s" rule_leading_indent "   "

let same_sink (a : OutJ.cli_match) (b : OutJ.cli_match) : bool =
  Fpath.equal a.path b.path
  && Rule_ID.equal a.check_id b.check_id
  && Int.equal a.start.offset b.start.offset
  && Int.equal a.end_.offset b.end_.offset

let group_findings_by_sink (matches : OutJ.cli_match list) :
    OutJ.cli_match list list =
  List.fold_left
    (fun (groups : OutJ.cli_match list list) (m : OutJ.cli_match) ->
      match groups with
      | (previous :: _ as group) :: older when same_sink previous m ->
          (m :: group) :: older
      | _ -> [ m ] :: groups)
    [] matches
  |> List_.map List.rev |> List.rev

let pp_text_outputs ~max_chars_per_line ~max_lines_per_finding
    ~show_dataflow_traces
    ~(interfile_dedup_by : Core_match.interfile_dedup_by)
    ~(is_interfile : Rule_ID.t -> bool) ppf
    (matches : OutJ.cli_match list) =
  let print_one_match ~(prev : OutJ.cli_match option) ~(cur : OutJ.cli_match)
      ~(next : OutJ.cli_match option) ~(sink_findings : OutJ.cli_match list) =
    (* Separation of concerns:
       Keep side effect separate from value-returning computations *)
    (match prev with
    | None -> Fmt.pf ppf "@."
    | Some _ -> ());
    (* Nesting hierarchy:
       file > rule > message derived from template in rule *)
    let must_print_file =
      (* must print file because it's a match in a new file *)
      match prev with
      | None -> true
      | Some m -> m.path <> cur.path
    in
    (* the rule name and its message are printed together, for a match of
       a new rule or with a different message (the message is derived from
       a template of the rule) *)
    let must_print_rule =
      must_print_file
      ||
      match prev with
      | None -> true
      | Some m ->
          (not (Rule_ID.equal m.check_id cur.check_id))
          || not (String.equal m.extra.message cur.extra.message)
    in
    let has_rule_name = cur.check_id <> Rule_ID.dash_e in
    (if must_print_file then
       Fmt.pf ppf "  %a@."
         Fmt.(styled (`Fg `Cyan) (esc_prefix ppf ++ string))
         !!(cur.path));
    (if must_print_rule then
       let rule_name_lines =
         if has_rule_name then (
           pp_styled_severity ppf cur.extra.severity;
           (* python: RULE_TEXT_WIDTH and RULE_INDENT *)
           wrap_lines ~filler:Textwrap ~width:(safe_width rule_text_width)
             ~initial_indent:0
             ~subsequent_indent:(rule_indent_size - console_indent_size)
             (Rule_ID.to_string cur.check_id))
         else []
       in
       match rule_name_lines with
       | [] -> ()
       | (_, txt) :: rest ->
           (* Print indented severity with 1 trailing space and then
              first line *)
           Fmt.pf ppf " %a@." Fmt.(styled `Bold string) txt;
           List.iter
             (fun (indentation, txt) ->
               Fmt.pf ppf "%s%a@." indentation Fmt.(styled `Bold string) txt)
             rest;
           (* python: DESC_TEXT_WIDTH and BASE_INDENT, the message being
              filled paragraph by paragraph *)
           cur.extra.message |> message_paragraphs
           |> List.iteri
                (fun (i : int) ((extra_indent : int), (paragraph : string)) ->
                  if i > 0 then Fmt.pf ppf "@.";
                  let indent =
                    detail_indent_size - console_indent_size + extra_indent
                  in
                  wrap_lines ~filler:Click ~width:(safe_width desc_text_width)
                    ~initial_indent:indent ~subsequent_indent:indent paragraph
                  |> List.iter (fun (indentation, txt) ->
                         Fmt.pf ppf "%s%s@." indentation txt));
           (match metadata_member "shortlink" cur.extra.metadata with
           | `String txt -> Fmt.pf ppf "%sDetails: %s@." detail_indent txt
           | _ -> ());
           Fmt.pf ppf "@.");
    (match cur.extra.fix with
    | None -> ()
    | Some fix ->
        (* the fix on one line, wrapped after the tag; an empty fix deletes
           the match *)
        let autofix_tag = "▶▶┆ Autofix ▶ " in
        let fix_text = one_line_of_code fix in
        (* python: (BASE_INDENT + 1) columns, plus those of the console *)
        Fmt.pf ppf "%s%a"
          (String.make (detail_indent_size + 1) ' ')
          Fmt.(styled (`Fg `Green) string)
          autofix_tag;
        if String.equal fix_text "" then
          Fmt.pf ppf "%a@." Fmt.(styled (`Fg `Red) string) "delete"
        else
          (* python: AUTOFIX_TEXT_WIDTH, and BASE_INDENT + 4 for the line
             number of the wrapped lines *)
          wrap_lines ~filler:Textwrap ~width:(safe_width autofix_text_width)
            ~initial_indent:0
            ~subsequent_indent:(detail_indent_size + 4 - console_indent_size)
            fix_text
          |> List.iteri (fun (i : int) ((indentation : string), (txt : string)) ->
                 if i = 0 then Fmt.pf ppf "%s@." txt
                 else Fmt.pf ppf "%s%s@." indentation txt));
    let same_file_next =
      match next with
      | None -> false
      | Some next -> Fpath.equal next.path cur.path
    in
    let same_rule_next =
      match next with
      | None -> false
      | Some next -> Rule_ID.equal next.check_id cur.check_id
    in
    pp_finding ~max_chars_per_line ~max_lines_per_finding
      ~show_dataflow_traces ~append_separator:(same_file_next && same_rule_next)
      ~is_interfile ~sink_findings ppf cur;
    Fmt.pf ppf "@."
  in
  let groups =
    match interfile_dedup_by with
    | Core_match.Sink -> List_.map (fun (m : OutJ.cli_match) -> [ m ]) matches
    | Core_match.Source_sink -> group_findings_by_sink matches
  in
  let head (group : OutJ.cli_match list) : OutJ.cli_match option =
    match group with
    | m :: _ -> Some m
    | [] -> None
  in
  groups
  |> List_.iter_with_view_into_neighbor_elements
       (fun ~(prev : OutJ.cli_match list option) ~(cur : OutJ.cli_match list)
            ~(next : OutJ.cli_match list option) ->
         match cur with
         | [] -> ()
         | first :: _ ->
             print_one_match ~prev:(Option.bind prev head) ~cur:first
               ~next:(Option.bind next head) ~sink_findings:cur)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* the check_ids of the blocking rules behind [matches], for the
 * "RULES FIRED" sections of the ci output; "-" is the id of a -e/--pattern
 * rule and is discarded like in pyopengrep text.py *)
let blocking_rule_ids (matches : OutJ.cli_match list) : string list =
  matches
  |> List_.map (fun (m : OutJ.cli_match) -> Rule_ID.to_string m.check_id)
  |> List.filter (fun id -> not (String.equal id "-"))
  |> Set_.of_list |> Set_.elements |> List.sort String.compare

let pp_rules_fired ppf (title : string) (ids : string list) : unit =
  if not (List_.null ids) then (
    Fmt.pf ppf "@.  %s@." title;
    ids |> List.iter (fun id -> Fmt.pf ppf "    %s@." id))

let pp_cli_output
    ~max_chars_per_line
    ~max_lines_per_finding
    ~show_dataflow_traces
    ~(interfile_dedup_by : Core_match.interfile_dedup_by)
    ~(is_interfile : Rule_ID.t -> bool)
    ?(is_ci_invocation = false)
    ppf
    (cli_output : OutJ.cli_output) =
  let groups =
    cli_output.results |> Semgrep_output_utils.sort_cli_matches
    |> Assoc.group_by (fun (m : OutJ.cli_match) ->
           match Product.of_cli_match m with
           | `SCA ->
               (* TO PORT:
                         subgroup = match.exposure_type or "undetermined"

                          figuring out the product, python uses (rule.py):
                             RuleProduct.sca
                             if "r2c-internal-project-depends-on" in self._raw
                             else RuleProduct.sast

                          and exposure_type (rule_match.py):
                          if "sca_info" not in self.extra:
                              return None

                          if self.metadata.get("sca-kind") == "upgrade-only":
                              return "reachable"
                          elif self.metadata.get("sca-kind") == "legacy":
                              return "undetermined"
                          else:
                              return "reachable" if self.extra["sca_info"].reachable else "unreachable" *)
               `Undetermined
           | `SAST when is_blocking m.extra.metadata -> `Blocking
           | `SAST -> `Nonblocking
           | `Secrets ->
               (Option.value ~default:`No_validator m.extra.validation_state
                 :> report_group))
  in
  let groups =
    (* from text.py:
       if not is_ci_invocation: *)
    if is_ci_invocation then groups
    else
      let merged =
        (try List.assoc `Nonblocking groups with
        | Not_found -> [])
        @
        try List.assoc `Blocking groups with
        | Not_found -> []
      in
      (`Merged, merged)
      :: List.filter
           (fun (k, _) -> not (k = `Nonblocking || k = `Blocking))
           groups
  in
  groups |> sort_by_groups
  |> List.iter (fun (group, matches) ->
         if not (List_.null matches) then
           Fmt_.pp_heading ppf
             (String_.unit_str (List.length matches) (group_titles group));
         pp_text_outputs ~max_chars_per_line ~max_lines_per_finding
           ~show_dataflow_traces ~interfile_dedup_by ~is_interfile ppf matches);
  if is_ci_invocation then (
    pp_rules_fired ppf "BLOCKING CODE RULES FIRED:"
      (match List.assoc_opt `Blocking groups with
      | Some matches -> blocking_rule_ids matches
      | None -> []);
    (* fork: without secrets validators every Secrets match lands in the
     * No_validator group, where pyopengrep falls back to the plain
     * "dev.semgrep.actions" check *)
    let secrets_ids =
      ([ `Confirmed_valid; `Confirmed_invalid; `Validation_error; `No_validator ]
        : report_group list)
      |> List.concat_map (fun g ->
             match List.assoc_opt g groups with
             | Some matches ->
                 matches
                 |> List.filter (fun (m : OutJ.cli_match) ->
                        is_blocking m.extra.metadata)
                 |> blocking_rule_ids
             | None -> [])
      |> Set_.of_list |> Set_.elements |> List.sort String.compare
    in
    pp_rules_fired ppf "BLOCKING SECRETS RULES FIRED:" secrets_ids);
  (* the "time" field is there with --time *)
  match cli_output.time with
  | Some time ->
      (* python: a blank line separates the block from the findings, and
         the last finding printed one already *)
      if List_.null cli_output.results then Fmt.pf ppf "@.";
      Time_report.pp_time_summary ppf time cli_output.errors
  | None -> ()
