module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The structure shared by the simple and vivid skins.
 *
 * Both draw the same report: the same parts of a finding in the same order,
 * the findings grouped by file, the same ci sections, and the same lines
 * before and after the findings. They differ in how each part is drawn (the
 * margin of a line, the style of the severity and the line numbers, the
 * wording of the lines around the findings), which each skin sets in a
 * [look].
 *
 * The legacy skin does not use this module: it reproduces the python report,
 * which has its own structure.
 *)

module M = Skin_model

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* How a trace is drawn under a finding: see
   Findings_layout.pp_dataflow_tree. *)
type trace_look = {
  line_prefix : string;
  gutter : int -> string;
  gutter_blank : string;
  highlight : Fmt.style list;
}

(* How the parts of one finding are drawn, chosen by its severity, so that a
   skin can colour the whole finding by it. *)
type finding_look = {
  (* the margin of a line of the heading or the message, and its width *)
  pp_head_margin : Format.formatter -> unit;
  head_width : int;
  (* the margin of a line below them (the sources, the fix), and its width *)
  pp_body_margin : Format.formatter -> unit;
  body_width : int;
  (* an empty line inside the finding *)
  pp_blank : Format.formatter -> unit;
  (* the start of the heading, and the rule id after it *)
  badge : string;
  pp_badge : string Fmt.t;
  pp_rule_id : string Fmt.t;
  (* The width of a line of code in a report [width] columns wide, before
     --max-chars-per-line, where [digits] is the width of the largest line
     number in the finding. *)
  code_width : width:int -> digits:int -> int;
  (* a line of the match, margin and number included, filled at [width] *)
  pp_code_line : digits:int -> width:int -> Findings_layout.code_line Fmt.t;
  (* the note on the lines of the match left out, after the body margin *)
  pp_more_lines : digits:int -> string Fmt.t;
  (* the label before a source, and its width *)
  pp_from_label : Format.formatter -> unit;
  from_label_width : int;
  (* the look of a trace, its styled parts rendered for this formatter *)
  trace : Format.formatter -> digits:int -> trace_look;
  (* the label before a fix, and its width *)
  pp_fix_label : Format.formatter -> unit;
  fix_label_width : int;
}

type look = {
  finding : OutJ.match_severity -> finding_look;
  (* the file of a run of findings, printed once above them *)
  pp_file_header : Skin.ctx -> string Fmt.t;
  (* the heading of a ci section, blocking or not, with its number of
     findings *)
  pp_ci_heading : blocking:bool -> int Fmt.t;
  (* the title above the rules of the blocking findings *)
  pp_rules_fired_title : Skin.ctx -> string Fmt.t;
  (* the environment of an 'opengrep ci' run *)
  pp_ci_environment : Skin.ctx -> M.Start.ci_env Fmt.t;
  (* the plan of a scan, or of its baseline scan: the number of files and of
     rules, or None when there is nothing to scan *)
  pp_plan : M.Plan.run -> (string * string) option Fmt.t;
  (* whether the summary lines are sentences: capitalised, and ending with a
     full stop where they are complete *)
  sentences : bool;
  (* the counts of rules, files and findings that end the report *)
  pp_tally : M.Result.tally Fmt.t;
}

(*****************************************************************************)
(* A finding *)
(*****************************************************************************)

(* A long rule id is wrapped rather than shortened, since a reader copies it
   to suppress or search for the rule. The break falls where the width ends,
   mid-token if needed, as in the legacy report. The later lines are
   indented to the start of the first. *)
let pp_heading (ctx : Skin.ctx) (look : finding_look) ppf (m : OutJ.cli_match)
    : unit =
  (* the column of the id, after the margin *)
  let id_column = String.length look.badge + 2 in
  match
    Findings_layout.wrap_lines ~filler:Textwrap
      ~width:
        (Findings_layout.safe_width (ctx.width - look.head_width - id_column))
      ~initial_indent:0 ~subsequent_indent:0
      (Rule_ID.to_string m.check_id)
  with
  | [] -> ()
  | (_, first) :: rest ->
      look.pp_head_margin ppf;
      Fmt.pf ppf "%a  %a@." look.pp_badge look.badge look.pp_rule_id first;
      let hanging = String.make id_column ' ' in
      rest
      |> List.iter (fun ((_ : string), (txt : string)) ->
             look.pp_head_margin ppf;
             Fmt.pf ppf "%s%a@." hanging look.pp_rule_id txt)

(* Nothing is printed for a rule without a message: the margin alone would
   be a line of trailing whitespace. An indented paragraph keeps its
   indentation, which is subtracted from the width. *)
let pp_message (ctx : Skin.ctx) (look : finding_look) ppf (message : string) :
    unit =
  if not (String.equal (String.trim message) "") then
    message |> Findings_layout.message_paragraphs
    |> List.iteri
         (fun (i : int) ((extra_indent : int), (paragraph : string)) ->
           if i > 0 then look.pp_blank ppf;
           let indentation = String.make extra_indent ' ' in
           Findings_layout.wrap_lines ~filler:Click
             ~width:
               (Findings_layout.safe_width
                  (ctx.width - look.head_width - extra_indent))
             ~initial_indent:0 ~subsequent_indent:0 paragraph
           |> List.iter (fun ((_ : string), (txt : string)) ->
                  look.pp_head_margin ppf;
                  Fmt.pf ppf "%s%s@." indentation txt))

(* The lines of the match, numbered in a gutter as wide as the largest line
   number of this finding, so that the code of a short file is not shifted
   right by a long file elsewhere. *)
let pp_code (ctx : Skin.ctx) (look : finding_look) ppf (m : OutJ.cli_match) :
    unit =
  let lines, trimmed =
    Findings_layout.code_lines ~max_lines_per_finding:ctx.max_lines_per_finding
      m
  in
  let digits =
    String.length (string_of_int (m.start.line + max 0 (List.length lines - 1)))
  in
  let available = look.code_width ~width:ctx.width ~digits in
  let width =
    Findings_layout.safe_width
      (if ctx.max_chars_per_line > 0 then min ctx.max_chars_per_line available
       else available)
  in
  lines |> List.iter (look.pp_code_line ~digits ~width ppf);
  trimmed
  |> Option.iter (fun (n : int) ->
         look.pp_body_margin ppf;
         look.pp_more_lines ~digits ppf
           (Printf.sprintf "… %s more, adjust with --max-lines-per-finding"
              (String_.unit_str n "line")))

(* Under --interfile-dedup-by source-sink the findings that share this sink
   differ only in their source, so the sink is drawn once with each source
   under it. Each source is followed by its own trace, since a trace alone
   does not show which source it belongs to.

   The styled parts of a trace are rendered with the renderer of this
   formatter, not of stdout: the same report also goes to -o/--text-output,
   whose buffer has no renderer and must contain no escapes while the
   terminal receives colour. *)
let pp_origins (ctx : Skin.ctx) (look : finding_look) ppf (m : OutJ.cli_match)
    (group : OutJ.cli_match list) : unit =
  let origins =
    Findings_layout.origins ~is_interfile:ctx.is_interfile
      ~show_dataflow_traces:ctx.show_dataflow_traces m group
  in
  let digits =
    Findings_layout.trace_line_digits
      (List_.map (fun (o : Findings_layout.origin) -> o.finding) origins)
  in
  let trace_look = look.trace ppf ~digits in
  let faint (glyph : string) : string =
    Fmt.str_like ppf "%a" Fmt.(styled `Faint string) glyph
  in
  origins
  |> List.iter (fun (o : Findings_layout.origin) ->
         if o.gap_before then look.pp_blank ppf;
         o.source
         |> Option.iter (fun ((where : string), (code : string)) ->
                let hanging = String.make look.from_label_width ' ' in
                (* the location is coloured and the code is not, as in the
                   snippets above *)
                Findings_layout.from_clause_lines
                  ~width:(ctx.width - look.body_width - look.from_label_width)
                  ~located:where ~code
                |> List.iteri
                     (fun (i : int) ((located : string), (code : string)) ->
                       look.pp_body_margin ppf;
                       if i = 0 then look.pp_from_label ppf
                       else Fmt.string ppf hanging;
                       (* a line with only code opens no colour span: an
                          empty styled string would print only two escapes *)
                       if String.equal located "" then Fmt.pf ppf "%s@." code
                       else
                         Fmt.pf ppf "%a%s@."
                           Fmt.(styled (`Fg `Cyan) string)
                           located
                           (if String.equal code "" then "" else "  " ^ code)));
         o.trace
         |> Option.iter (fun (trace : OutJ.match_dataflow_trace) ->
                Findings_layout.pp_dataflow_tree ~finding_path:o.finding.path
                  ~line_prefix:trace_look.line_prefix ~glyph:faint
                  ~gutter:trace_look.gutter
                  ~gutter_blank:trace_look.gutter_blank
                  ~highlight:trace_look.highlight ppf trace))

(* [heading] is false for a match with the same rule and message as the
   previous one in the same file: the report prints the rule once and the
   snippets after it. The blank line under the message is omitted too, since
   the previous finding ends with one. [more_follows] is true when the next
   finding is such a repeat. *)
let pp_finding ?(group : OutJ.cli_match list = []) ?(heading = true)
    ?(more_follows = false) (ctx : Skin.ctx) (look : look) ppf
    (m : OutJ.cli_match) : unit =
  let finding_look = look.finding m.extra.severity in
  (* Checked here rather than where [heading] is computed, so that no caller
     can print a heading for a -e finding. *)
  if heading && Findings_layout.has_rule_name m then begin
    pp_heading ctx finding_look ppf m;
    pp_message ctx finding_look ppf m.extra.message;
    finding_look.pp_blank ppf
  end;
  pp_code ctx finding_look ppf m;
  pp_origins ctx finding_look ppf m group;
  (match
     Findings_layout.fix_display
       ~width:
         (Findings_layout.safe_width
            (ctx.width - finding_look.body_width - finding_look.fix_label_width))
       m
   with
  (* an empty fix deletes the match when --autofix runs *)
  | Some [] ->
      finding_look.pp_blank ppf;
      finding_look.pp_body_margin ppf;
      finding_look.pp_fix_label ppf;
      Fmt.pf ppf "%a@." Fmt.(styled (`Fg `Red) string) "delete"
  | Some (_ :: _ as lines) ->
      (* the lines after the first are indented to the text, not to the
         label *)
      let hanging = String.make finding_look.fix_label_width ' ' in
      finding_look.pp_blank ppf;
      lines
      |> List.iter (fun (line : Findings_layout.fix_line) ->
             match line with
             | Findings_layout.Fix_first txt ->
                 finding_look.pp_body_margin ppf;
                 finding_look.pp_fix_label ppf;
                 Fmt.pf ppf "%s@." txt
             | Findings_layout.Fix_blank -> finding_look.pp_blank ppf
             | Findings_layout.Fix_more txt ->
                 finding_look.pp_body_margin ppf;
                 Fmt.pf ppf "%s%s@." hanging txt)
  | None -> ());
  (* A finding ends with a plain blank line, except before a repeat, where
     the blank line carries the margin of the finding so that the margin
     continues into the repeat. *)
  if more_follows then finding_look.pp_blank ppf else Fmt.pf ppf "@."

(* The findings grouped by file, each file printed once, in input order. *)
let pp_by_file (look : look) (ctx : Skin.ctx) ppf
    (matches : OutJ.cli_match list) : unit =
  Findings_layout.place_findings ctx.interfile_dedup_by matches
  |> List.iter (fun (p : Findings_layout.placed) ->
         if p.opens_file then look.pp_file_header ctx ppf !!(p.lead.path);
         pp_finding ~group:p.group ~heading:p.heading ~more_follows:p.continued
           ctx look ppf p.lead)

(* The distinct rules of the blocking findings. There is no list for the
   non-blocking ones, which do not fail the run. *)
let pp_rules_fired (look : look) (ctx : Skin.ctx) ppf
    (matches : OutJ.cli_match list) : unit =
  let ids =
    matches
    |> List_.map (fun (m : OutJ.cli_match) -> Rule_ID.to_string m.check_id)
    |> List.sort_uniq String.compare
  in
  if not (List_.null ids) then begin
    look.pp_rules_fired_title ctx ppf "blocking rules fired";
    ids
    |> List.iter (fun (id : string) ->
           Fmt.pf ppf "  %a@." Fmt.(styled (`Fg `Cyan) string) id);
    Fmt.pf ppf "@."
  end

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let line (f : Format.formatter -> unit) : Skin.chunk = Skin.Line (Logs.App, f)

let on_start (look : look) (ctx : Skin.ctx) (start : M.Start.t) :
    Skin.chunk list =
  (match start.ci with
  | Some env -> [ line (fun ppf -> look.pp_ci_environment ctx ppf env) ]
  | None -> [])
  @
  if not start.banner then []
  else [ line (fun ppf -> Skin_banner.pp ~margin:"" ppf) ]

(* No rules status line, and so no spinner. *)
let rules_status (_ctx : Skin.ctx) (_start : M.Start.t) : string option = None

let on_plan (look : look) (_ctx : Skin.ctx) (plan : M.Plan.t) :
    Skin.chunk list =
  [
    line (fun ppf ->
        look.pp_plan plan.run ppf
          (if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0
           then None
           else
             (* The files paired with a rule and the rules paired with a
                file, not all the targets found and all the rules loaded:
                on a mixed repository the two counts differ widely. *)
             Some
               ( String_.unit_str plan.num_files_with_a_rule "file",
                 String_.unit_str plan.num_rules_with_a_target "rule" )));
    (* an empty line before the findings *)
    line (fun _ppf -> ());
  ]

let pp_summary ~(sentences : bool) ppf (summary : M.Summary.t) : unit =
  let label (txt : string) : string =
    if sentences then String.capitalize_ascii txt else txt
  in
  Option.iter (fun (txt : string) -> Fmt.pf ppf "%s@." txt) summary.limited;
  (* Not M.string_of_phrase: its noun is the whole of "N files only
     partially analyzed due to a ... error", which would repeat the label
     and is plural for any count. Only the count is printed. *)
  summary.partially_analyzed
  |> Option.iter (fun (p : M.phrase) ->
         Fmt.pf ppf "%s: %s (parse or internal error)@."
           (label "partially analyzed")
           (String_.unit_str (M.total_of_phrase p) "file"));
  if summary.unplaced_warnings > 0 then
    Fmt.pf ppf "%s: %s about the scan, see --verbose%s@."
      (label "analysis limited")
      (String_.unit_str summary.unplaced_warnings "warning")
      (if sentences then "." else "");
  match summary.skipped with
  | [] -> ()
  | xs ->
      Fmt.pf ppf "%s: %s@." (label "skipped")
        (xs |> List_.map M.string_of_phrase |> String.concat ", ")

let on_result (look : look) (_ctx : Skin.ctx) (result : M.Result.t) :
    Skin.chunk list =
  let summary =
    if M.Summary.is_empty result.summary then []
    else
      [ line (fun ppf -> pp_summary ~sentences:look.sentences ppf result.summary) ]
  in
  let tally =
    match result.tally with
    | None -> []
    | Some (t : M.Result.tally) -> [ line (fun ppf -> look.pp_tally ppf t) ]
  in
  (Skin.Findings :: summary) @ tally

let pp_findings (look : look) (ctx : Skin.ctx) ppf
    (cli_output : OutJ.cli_output) : unit =
  Findings_layout.pp_findings ~is_ci_invocation:ctx.is_ci_invocation
    ~pp_by_file:(pp_by_file look ctx) ~pp_ci_heading:look.pp_ci_heading
    ~pp_rules_fired:(pp_rules_fired look ctx) ppf cli_output
