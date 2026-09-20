module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The simple and vivid skins, but for their look.
 *
 * The two draw the same report: the same parts of a finding in the same
 * order, the findings grouped by file, the same ci sections, and the same
 * lines before and after the findings. They differ in how each part is
 * drawn -- what opens a line, how the severity and the line numbers look,
 * the wording of the lines around the findings -- and a skin says only
 * that, in a [look]. The rest is here, once.
 *
 * The legacy skin is not built on this: it reproduces the python report,
 * which has a structure of its own.
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

(* How the parts of one finding are drawn, for its severity, which a skin
   may colour the whole finding by. *)
type finding_look = {
  (* opens a line of the heading or the message, and the columns it takes *)
  pp_head_margin : Format.formatter -> unit;
  head_width : int;
  (* opens a line of what sits under them -- the sources, the fix -- and
     the columns it takes *)
  pp_body_margin : Format.formatter -> unit;
  body_width : int;
  (* a line with nothing on it, inside the finding *)
  pp_blank : Format.formatter -> unit;
  (* what the heading opens with, and the rule id beside it *)
  badge : string;
  pp_badge : string Fmt.t;
  pp_rule_id : string Fmt.t;
  (* The columns a line of code has in a report [width] wide, before
     --max-chars-per-line, [digits] being the width of the largest line
     number in the finding. *)
  code_width : width:int -> digits:int -> int;
  (* a line of the match, margin and number included, filled at [width] *)
  pp_code_line : digits:int -> width:int -> Findings_layout.code_line Fmt.t;
  (* the note on the lines of the match left out, after the body margin *)
  pp_more_lines : digits:int -> string Fmt.t;
  (* what a named source opens with, and the columns it takes *)
  pp_from_label : Format.formatter -> unit;
  from_label_width : int;
  (* the look of a trace, its styled parts rendered for this formatter *)
  trace : Format.formatter -> digits:int -> trace_look;
  (* what a fix opens with, and the columns it takes *)
  pp_fix_label : Format.formatter -> unit;
  fix_label_width : int;
}

type look = {
  finding : OutJ.match_severity -> finding_look;
  (* the file a run of findings is in, stated once above them *)
  pp_file_header : Skin.ctx -> string Fmt.t;
  (* A ci report splits its findings into the ones that fail the run and
     the ones that do not, under a heading each, with the number of
     findings in it. *)
  pp_ci_heading : blocking:bool -> int Fmt.t;
  (* the title above the rules the blocking findings come from *)
  pp_rules_fired_title : Skin.ctx -> string Fmt.t;
  (* what 'opengrep ci' runs in *)
  pp_ci_environment : Skin.ctx -> M.Start.ci_env Fmt.t;
  (* the plan of a scan, or of its --baseline-commit replay: the number of
     files and of rules, or None when there is nothing to scan *)
  pp_plan : M.Plan.run -> (string * string) option Fmt.t;
  (* whether the summary says its lines as sentences: capitalised, and
     ending with a period where they are whole *)
  sentences : bool;
  (* the number of findings, which closes the report *)
  pp_tally : M.Result.tally Fmt.t;
}

(*****************************************************************************)
(* A finding *)
(*****************************************************************************)

(* A long id is wrapped rather than shortened: it is what a reader copies
   to silence or search for the rule, so all of it has to be there. The
   break lands wherever the width falls, mid-token if need be, as the
   legacy report breaks it -- ids this long are rare enough that a tidier
   rule is not worth the machinery. Its later lines hang under its first. *)
let pp_heading (ctx : Skin.ctx) (look : finding_look) ppf (m : OutJ.cli_match)
    : unit =
  (* the column the id starts at, past the margin *)
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

(* Nothing is printed for a rule with no message: the margin on its own
   would be a blank line of trailing whitespace. An indented paragraph
   keeps its indent, which is taken off the width once. *)
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

(* The lines of the match, numbered in a gutter whose width is that of the
   largest number in this finding, so the code of a short file is not pushed
   right by a long one elsewhere. *)
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

(* Under --interfile-dedup-by source-sink the findings sharing this sink
   differ only in where the taint started, so the sink is drawn once and
   each source named under it. Without this the block appears once per
   source with nothing to tell the copies apart.

   Each source is followed by its own trace rather than all the sources
   first and all the traces after: the pairing is what makes a trace
   readable, since on its own it does not say which source it explains.

   The styled parts of a trace are rendered with the renderer of this
   formatter, not of stdout: the same report also goes to -o/--text-output,
   whose buffer has no renderer and must stay free of escapes even while
   the terminal is getting colour. *)
let pp_origins (ctx : Skin.ctx) (look : finding_look) ppf (m : OutJ.cli_match)
    (group : OutJ.cli_match list) : unit =
  let origins =
    Findings_layout.origins ~is_interfile:ctx.is_interfile
      ~show_dataflow_traces:ctx.show_dataflow_traces m group
  in
  (* wide enough for the largest number the traces below will draw, which
     is not the finding's own: a step can sit far down another file *)
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
                (* The path is never shortened -- it is what a reader
                   opens -- so the clause wraps instead, the break falling
                   on the gap before the code where it can and inside the
                   path only when the path alone is wider than the line.
                   The code is still cut, since a source spanning several
                   lines arrives here as one. *)
                let hanging = String.make look.from_label_width ' ' in
                (* the locator is coloured, the code beside it is not: it
                   is code, and reads as the snippets above do *)
                Findings_layout.from_clause_lines
                  ~width:(ctx.width - look.body_width - look.from_label_width)
                  ~located:where ~code
                |> List.iteri
                     (fun (i : int) ((located : string), (code : string)) ->
                       look.pp_body_margin ppf;
                       if i = 0 then look.pp_from_label ppf
                       else Fmt.string ppf hanging;
                       (* a line of code alone opens no colour span: an
                          empty styled string is just two escapes *)
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

(* [heading] is false for a match that repeats the rule and message of the
   one before it in the same file: those are the same finding said again of
   another line, and the report states the rule once and lets the snippets
   follow. The blank line under the message goes with it, the finding above
   having already closed with one. [more_follows] when the next finding is
   such a repeat. *)
let pp_finding ?(group : OutJ.cli_match list = []) ?(heading = true)
    ?(more_follows = false) (ctx : Skin.ctx) (look : look) ppf
    (m : OutJ.cli_match) : unit =
  let finding_look = look.finding m.extra.severity in
  (* Guarded here rather than where [heading] is decided, so that no
     caller can head a -e finding by asking for one. *)
  if heading && Findings_layout.has_rule_name m then begin
    pp_heading ctx finding_look ppf m;
    pp_message ctx finding_look ppf m.extra.message;
    (* sets the code apart from the message *)
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
  (* a fix with no text deletes the match, which the report has to say:
     the code goes away when --autofix runs *)
  | Some [] ->
      finding_look.pp_blank ppf;
      finding_look.pp_body_margin ppf;
      finding_look.pp_fix_label ppf;
      Fmt.pf ppf "%a@." Fmt.(styled (`Fg `Red) string) "delete"
  | Some (_ :: _ as lines) ->
      (* set apart from the snippet, as the snippet is from the message;
         the lines after the first hang under the text, not the label *)
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
  (* A finding closes with a plain blank line, but one that the next only
     repeats closes with a blank line of its own look: a margin drawn down
     the whole of a finding carries on into the repeat. *)
  if more_follows then finding_look.pp_blank ppf else Fmt.pf ppf "@."

(* The findings of one file under a name stated once, in reported order. *)
let pp_by_file (look : look) (ctx : Skin.ctx) ppf
    (matches : OutJ.cli_match list) : unit =
  Findings_layout.place_findings ctx.interfile_dedup_by matches
  |> List.iter (fun (p : Findings_layout.placed) ->
         if p.opens_file then look.pp_file_header ctx ppf !!(p.lead.path);
         pp_finding ~group:p.group ~heading:p.heading ~more_follows:p.continued
           ctx look ppf p.lead)

(* The distinct rules behind the findings that fail the run: what a reader
   has to go and fix before the build passes. There is no non-blocking
   counterpart, as there is nothing to act on. *)
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

(* No status line, and so no spinner either: a skin says nothing until it
   has something to report. *)
let rules_status (_ctx : Skin.ctx) (_start : M.Start.t) : string option = None

let on_plan (look : look) (_ctx : Skin.ctx) (plan : M.Plan.t) :
    Skin.chunk list =
  [
    line (fun ppf ->
        look.pp_plan plan.run ppf
          (if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0
           then None
           else
             (* The files a rule will look at and the rules that have one
                to look at, not everything targeting found and everything
                that was loaded. On a mixed repository the two are far
                apart -- one Java rule over a Java benchmark pairs 2766
                files out of the 5703 found -- and this line is what the
                scan is about to do. *)
             Some
               ( String_.unit_str plan.num_files_with_a_rule "file",
                 String_.unit_str plan.num_rules_with_a_target "rule" )));
    (* the findings start their own block *)
    line (fun _ppf -> ());
  ]

let pp_summary ~(sentences : bool) ppf (summary : M.Summary.t) : unit =
  let label (txt : string) : string =
    if sentences then String.capitalize_ascii txt else txt
  in
  Option.iter (fun (txt : string) -> Fmt.pf ppf "%s@." txt) summary.limited;
  (* Worded here rather than taken plain: the phrase's own noun is the
     whole of "N files only partially analyzed due to a ... error", which
     under a label of the same name would be said twice, and which is
     plural whatever it counts. The count is all this line needs. *)
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
