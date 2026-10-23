module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A plain report: the structure of the vivid one without its bars and most
 * of its colour.
 *
 * Each file is printed once above its findings; a finding is a severity, a
 * rule id and a message, with the code under them, each line after its
 * number at the left edge. Colour marks only the severity and the rule id;
 * the line numbers, the labels and the code around a match are faint.
 *
 * This module defines the look; Skin_common draws the report with it.
 *)

module M = Skin_model

(*****************************************************************************)
(* Measurements *)
(*****************************************************************************)

(* The column of the message. Findings_layout.chunk_indentation adds two
   columns of its own (those rich added in the python wrapper), so an indent
   of 0 there starts here. *)
let indent_size = 2
let indent = String.make indent_size ' '

(* The note on the omitted lines of a finding is one level deeper than the
   heading and the message. *)
let body_size = indent_size + 2
let body = String.make body_size ' '

(* Findings_layout.chunk_indentation adds console_indent_size columns, so a
   gutter with this indent starts at the left edge. *)
let gutter_indent = -Findings_layout.console_indent_size

(* A line number is right-aligned in a column of fixed width, so that the
   code starts at the same column in every finding. *)
let line_number_width = 6
let separator = ": "

(* A located line of a trace is under the path printed before it, which
   Findings_layout.esc_prefix indents by two columns. *)
let trace_number_indent = "  "
let separator_width = Utf8.length separator

(* the column of the code, after its number and the separator *)
let code_column = line_number_width + separator_width

(* a trace is drawn from the column of the code above it *)
let trace_margin = String.make code_column ' '

(* The lines of a label around the code have no common margin: each label
   carries its own indentation. A label is right-aligned in the column of
   the line numbers, so that its colon is in line with theirs and the text
   after it starts at the column of the code. *)
let gutter_label (word : string) : string =
  String.make (max 0 (line_number_width - String.length word)) ' '
  ^ word ^ separator

let context_label = gutter_label "in"
let from_label = gutter_label "from"
let fix_label = gutter_label "fix"

(*****************************************************************************)
(* Styling *)
(*****************************************************************************)

let severity_word (severity : OutJ.match_severity) : string * Fmt.style =
  match severity with
  | `Critical -> ("critical", `Fg `Magenta)
  | `Error
  | `High ->
      ("error", `Fg `Red)
  | `Warning
  | `Medium ->
      ("warn", `Fg `Yellow)
  | `Info
  | `Low ->
      ("info", `Fg `Green)
  | `Inventory
  | `Experiment ->
      ("note", `Fg `Cyan)

(* In the snippet and in a trace, the matched code is plain and the rest of
   its lines faint, rather than the match bold, which some terminals render
   as a heavier font and others as a brighter colour. *)
let match_highlight : Fmt.style list = []
let match_unhighlighted : Fmt.style list = [ `Faint ]

(* the line numbers, in the snippet and in a trace, the labels and the note
   on omitted lines *)
let annotation_style : Fmt.style list = [ `Faint ]
let pp_annotation : string Fmt.t =
  Findings_layout.styles annotation_style Fmt.string

(* a line number right-aligned in [width] columns and its separator, faint,
   rendered for [ppf] *)
let numbered (ppf : Format.formatter) (width : int) (n : int) : string =
  Fmt.str_like ppf "%a" pp_annotation (Printf.sprintf "%*d%s" width n separator)

(*****************************************************************************)
(* A finding *)
(*****************************************************************************)

(* "  warn  os-system-concat", with the message under it at the same
   indent, and the code under them, each line after its number *)
let finding (severity : OutJ.match_severity) : Skin_common.finding_look =
  let word, style = severity_word severity in
  {
    pp_head_margin = (fun ppf -> Fmt.string ppf indent);
    head_width = indent_size;
    pp_body_margin = (fun (_ : Format.formatter) -> ());
    body_width = 0;
    pp_blank = (fun ppf -> Fmt.pf ppf "@.");
    blank_before_fix = false;
    badge = word;
    pp_badge = Fmt.(styled style string);
    pp_rule_id = Fmt.(styled (`Fg `Cyan) string);
    code_width = (fun ~width ~digits:_ -> width - code_column);
    (* the line number is part of what pp_wrapped_code_line fills *)
    pp_code_line =
      (fun ~digits:_ ~width ppf (line : Findings_layout.code_line) ->
        Findings_layout.pp_wrapped_code_line ~number_indent:gutter_indent
          ~code_indent:(gutter_indent + code_column) ~number_width:code_column
          ~separator ppf ~line_number:line.line_number
          ~width:(width + code_column)
          ~gutter_style:annotation_style ~highlight:match_highlight
          ~unhighlighted:match_unhighlighted ~highlight_start:line.match_start
          ~highlight_end:line.match_end line.text);
    pp_more_lines =
      (fun ~digits:_ ppf (txt : string) ->
        Fmt.pf ppf "%s%a@." body pp_annotation txt);
    pp_context_label =
      (fun (ppf : Format.formatter) -> pp_annotation ppf context_label);
    context_label_width = String.length context_label;
    pp_context_name = Fmt.string;
    pp_from_label =
      (fun (ppf : Format.formatter) -> pp_annotation ppf from_label);
    from_label_width = String.length from_label;
    pp_from_location = Fmt.string;
    (* the line numbers of a trace are as wide as the largest of them, after
       the label of each step *)
    trace =
      (fun (ppf : Format.formatter) ~code_digits:_ ~(digits : int) ->
        {
          line_prefix = trace_margin;
          gutter =
            (fun (n : int) -> trace_number_indent ^ numbered ppf digits n);
          gutter_blank =
            trace_number_indent ^ String.make (digits + separator_width) ' ';
          highlight = match_highlight;
          unhighlighted = match_unhighlighted;
        });
    pp_fix_label =
      (fun (ppf : Format.formatter) -> pp_annotation ppf fix_label);
    fix_label_width = String.length fix_label;
  }

(*****************************************************************************)
(* Around the findings *)
(*****************************************************************************)

(* A long path is wrapped rather than shortened, since the reader opens it
   and the report prints it only here. *)
let pp_file_header (ctx : Skin.ctx) ppf (path : string) : unit =
  Findings_layout.wrap_lines ~filler:Textwrap
    ~width:(Findings_layout.safe_width ctx.width) ~initial_indent:0
    ~subsequent_indent:0 path
  |> List.iter (fun ((_ : string), (txt : string)) ->
         Fmt.pf ppf "%a@." Fmt.(styled `Bold string) txt);
  Fmt.pf ppf "@."

(* the heading of a ci section; the findings under it carry no blocking
   mark of their own *)
let pp_section ppf (title : string) (style : Fmt.style) (count : int) : unit =
  Fmt.pf ppf "%a %a@.@."
    Fmt.(styled style string)
    title
    Fmt.(styled `Faint string)
    (Printf.sprintf "· %s" (String_.unit_str count "finding"))

(* the environment of an 'opengrep ci' run, on one line *)
let pp_ci_environment (_ctx : Skin.ctx) ppf (env : M.Start.ci_env) : unit =
  Fmt.pf ppf "opengrep %s on OCaml %s · %s · %s@." env.version
    env.ocaml_version env.environment env.event_name

(* A --baseline-commit scan prints two plans; the second, of the baseline
   scan, starts with "baseline". *)
let pp_plan (run : M.Plan.run) ppf (counts : (string * string) option) : unit
    =
  let prefix =
    match run with
    | M.Plan.Current -> ""
    | M.Plan.Baseline -> "baseline · "
  in
  match counts with
  | None -> Fmt.pf ppf "%snothing to scan" prefix
  | Some (files, rules) -> Fmt.pf ppf "%s%s · %s" prefix files rules

(* "no findings" rather than "0 findings in 0 files" *)
let pp_tally ppf (t : M.Result.tally) : unit =
  if Int.equal t.findings 0 then Fmt.pf ppf "no findings"
  else
    Fmt.pf ppf "%s in %s"
      (String_.unit_str t.findings "finding")
      (String_.unit_str t.files_with_findings "file")

let look : Skin_common.look =
  {
    finding;
    pp_file_header;
    pp_ci_heading =
      (fun ~(blocking : bool) ppf (count : int) ->
        if blocking then pp_section ppf "blocking" (`Fg `Red) count
        else pp_section ppf "non-blocking" `Faint count);
    pp_rules_fired_title =
      (fun _ctx ppf (title : string) ->
        Fmt.pf ppf "%a@." Fmt.(styled `Bold string) title);
    pp_ci_environment;
    pp_plan;
    sentences = false;
    pp_tally;
  }

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let doc = "A plain report: no boxes, and little colour."
let on_start = Skin_common.on_start look
let rules_status = Skin_common.rules_status
let on_plan = Skin_common.on_plan look
let on_result = Skin_common.on_result look
let pp_findings = Skin_common.pp_findings look
let pp_matches = Skin_common.pp_by_file look
let shows_status_bar = true
let pp_log_message = Some Skin_common.pp_log_message
let shows_enclosing_context = true
