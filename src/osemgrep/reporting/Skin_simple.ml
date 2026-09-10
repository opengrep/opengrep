module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A plain report: the shape of the vivid one without its bars and its
 * colour.
 *
 * A file is named once above its findings; a finding is a severity, a rule
 * and a message, with the code under them in a thin gutter. Colour marks
 * only the severity and the rule id, and the code sits four columns from
 * the margin rather than twelve.
 *)

module M = Skin_model

(*****************************************************************************)
(* Measurements *)
(*****************************************************************************)

(* The message and the gutter start here. Findings_layout.chunk_indentation
   already adds two columns of its own (the ones rich added in the python
   wrapper), so an indent of 0 there lands exactly here. *)
let indent_size = 2
let indent = String.make indent_size ' '

(* The code and everything that belongs to it — the snippet, its trace, the
   fix — sit one level deeper than the heading and the message. *)
let body_size = indent_size + 2
let body = String.make body_size ' '

(* Findings_layout.chunk_indentation adds console_indent_size columns of its
   own, so a gutter asking for this much lands at body_size. *)
let gutter_indent = body_size - Findings_layout.console_indent_size

(* " │ " between the line number and the code *)
let separator = " │ "
let separator_width = 3

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

(*****************************************************************************)
(* A finding *)
(*****************************************************************************)

(* the file a run of findings is in, stated once above them *)
let pp_file_header ppf (path : string) : unit =
  Fmt.pf ppf "%a@.@." Fmt.(styled `Bold string) path

(* "  warn  os-system-concat"; the line is in the gutter of the snippet *)
let pp_heading ppf (m : OutJ.cli_match) : unit =
  let word, style = severity_word m.extra.severity in
  Fmt.pf ppf "%s%a  %a@." indent
    Fmt.(styled style string)
    word
    Fmt.(styled (`Fg `Cyan) string)
    (Rule_ID.to_string m.check_id)

let pp_message (ctx : Skin.ctx) ppf (message : string) : unit =
  message |> Findings_layout.message_paragraphs
  |> List.iteri (fun (i : int) ((extra_indent : int), (paragraph : string)) ->
         if i > 0 then Fmt.pf ppf "@.";
         Findings_layout.wrap_lines ~filler:Click
           ~width:
             (Findings_layout.safe_width
                (ctx.width - indent_size - extra_indent))
           ~initial_indent:extra_indent ~subsequent_indent:extra_indent
           paragraph
         |> List.iter (fun ((indentation : string), (txt : string)) ->
                Fmt.pf ppf "%s%s@." indentation txt))

(* The lines of the match, numbered in a gutter whose width is that of the
   largest number in this finding, so the code of a short file is not pushed
   right by a long one elsewhere. *)
let pp_code (ctx : Skin.ctx) ppf (m : OutJ.cli_match) : unit =
  let lines =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
  in
  let lines, dedented = Findings_layout.dedent_lines lines in
  let lines, trimmed =
    let total = List.length lines in
    let keep =
      if ctx.max_lines_per_finding = 0 then total
      else min total ctx.max_lines_per_finding
    in
    if keep = total then (lines, None)
    else (List_.take keep lines, Some (total - keep))
  in
  let start_line = m.start.line in
  let digits =
    String.length (string_of_int (start_line + max 0 (List.length lines - 1)))
  in
  let number_width = digits + separator_width in
  let available = ctx.width - body_size in
  let width =
    Findings_layout.safe_width
      (if ctx.max_chars_per_line > 0 then
         min ctx.max_chars_per_line available
       else available)
  in
  lines
  |> List.iteri (fun (i : int) (line : string) ->
         let line_number = start_line + i in
         (* the bold range of the match, moved by the dedent, exactly as the
            legacy skin computes it *)
         let col c = max 0 (c - 1 - dedented) in
         let bold_start =
           if line_number > start_line then 0 else col m.start.col
         in
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
         Findings_layout.pp_wrapped_code_line ~number_indent:gutter_indent
           ~code_indent:(gutter_indent + number_width) ~number_width ~separator
           ppf ~line_number ~width ~bold_start ~bold_end line);
  trimmed
  |> Option.iter (fun (n : int) ->
         Fmt.pf ppf "%s%a@." body
           Fmt.(styled (`Fg `Cyan) string)
           (Printf.sprintf "… %s more" (String_.unit_str n "line")));
  (* --dataflow-traces, in the same gutter as the snippet above *)
  match m.extra.dataflow_trace with
  | Some trace when ctx.show_dataflow_traces ->
      let faint (glyph : string) : string =
        Fmt.str_like ppf "%a" Fmt.(styled `Faint string) glyph
      in
      Findings_layout.pp_dataflow_tree ~line_prefix:body ~glyph:faint
        ~gutter:(fun (n : int) -> Printf.sprintf "%*d%s" digits n separator)
        ~gutter_blank:(String.make digits ' ' ^ separator)
        ~highlight:[ `Bold ] ppf trace
  | _ -> ()

let pp_finding (ctx : Skin.ctx) ppf (m : OutJ.cli_match) : unit =
  pp_heading ppf m;
  pp_message ctx ppf m.extra.message;
  (* the blank line that sets the snippet apart from the message *)
  Fmt.pf ppf "@.";
  pp_code ctx ppf m;
  (match Option.map (Findings_layout.fix_lines ~first_col:m.start.col) m.extra.fix with
  | Some (first :: rest) ->
      (* set apart from the snippet, as the snippet is from the message;
         the lines after the first hang under the text, not the label *)
      Fmt.pf ppf "@.";
      Fmt.pf ppf "%sfix: %s@." body first;
      let hanging = body ^ String.make (String.length "fix: ") ' ' in
      rest |> List.iter (fun (l : string) -> Fmt.pf ppf "%s%s@." hanging l)
  | _ -> ());
  Fmt.pf ppf "@."

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let name = "simple"
let doc = "A plain report: no boxes, and colour only where it names things."

let line (f : Format.formatter -> unit) : Skin.chunk =
  Skin.Line (Skin.Stderr Logs.App, f)

(* Only on a terminal, so a log does not gain it. *)
let on_start (_ctx : Skin.ctx) (start : M.Start.t) : Skin.chunk list =
  if not start.banner then []
  else [ line (fun ppf -> Skin_banner.pp ~margin:"" ppf) ]

let on_plan (_ctx : Skin.ctx) (plan : M.Plan.t) : Skin.chunk list =
  [
    line (fun ppf ->
        if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0
        then Fmt.pf ppf "nothing to scan"
        else
          Fmt.pf ppf "%s · %s"
            (String_.unit_str plan.num_targets "file")
            (String_.unit_str plan.num_rules "rule"));
    (* the findings start their own block *)
    line (fun _ppf -> ());
  ]

let pp_summary ppf (summary : M.Summary.t) : unit =
  let str = M.string_of_phrase in
  Option.iter (fun (txt : string) -> Fmt.pf ppf "%s@." txt) summary.limited;
  summary.partially_analyzed
  |> Option.iter (fun p -> Fmt.pf ppf "partially analyzed: %s@." (str p));
  if summary.unplaced_warnings > 0 then
    Fmt.pf ppf "analysis limited: %s about the scan, see --verbose@."
      (String_.unit_str summary.unplaced_warnings "warning");
  match summary.skipped with
  | [] -> ()
  | xs ->
      Fmt.pf ppf "skipped: %s@."
        (xs |> List_.map str |> String.concat ", ")

let on_result (_ctx : Skin.ctx) (result : M.Result.t) : Skin.chunk list =
  let summary =
    if M.Summary.is_empty result.summary then []
    else [ line (fun ppf -> pp_summary ppf result.summary) ]
  in
  let tally =
    match result.tally with
    | None -> []
    | Some (t : M.Result.tally) ->
        [
          line (fun ppf ->
              Fmt.pf ppf "%s · %s"
                (String_.unit_str t.files_scanned "file")
                (String_.unit_str t.findings "finding"));
        ]
  in
  (Skin.Findings :: summary) @ tally

(* the findings of one file under a name stated once, in reported order *)
let pp_by_file (ctx : Skin.ctx) ppf (matches : OutJ.cli_match list) : unit =
  matches
  |> List.fold_left
       (fun (previous : string option) (m : OutJ.cli_match) ->
         let path = !!(m.path) in
         let here = Some path in
         if previous <> here then pp_file_header ppf path;
         pp_finding ctx ppf m;
         here)
       None
  |> ignore

let pp_matches (ctx : Skin.ctx) ppf (matches : OutJ.cli_match list) : unit =
  pp_by_file ctx ppf matches


(* the "time" field is there with --time *)
let pp_time ppf (cli_output : OutJ.cli_output) : unit =
  match cli_output.time with
  | Some time ->
      if List_.null cli_output.results then Fmt.pf ppf "@.";
      Time_report.pp_time_summary ppf time cli_output.errors
  | None -> ()

let pp_findings (ctx : Skin.ctx) ppf (cli_output : OutJ.cli_output) : unit =
  cli_output.results |> Semgrep_output_utils.sort_cli_matches
  |> pp_by_file ctx ppf;
  pp_time ppf cli_output

let live = None
