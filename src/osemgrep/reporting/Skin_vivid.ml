module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A report that uses colour to be read faster.
 *
 * A solid stripe in the severity's colour runs the full height of every
 * finding, so severity is legible from the edge of the page without reading
 * a word. Nothing boxes the code, so a line too wide to fit cannot break a
 * frame. The line numbers sit in a band of their own and the matched span is
 * tinted rather than merely emboldened.
 *
 * Everything here degrades: with $NO_COLOR, off a terminal, or in a file,
 * the stripe stays as a character and the bands simply vanish, which leaves
 * the report readable on its structure alone.
 *)

module M = Skin_model

(*****************************************************************************)
(* Measurements *)
(*****************************************************************************)

(* The report sits at the left edge: the banner, the file names, the scan
   lines and the summary all start there. Only the findings are inset, so a
   file's contents read as belonging to the name above them. *)
let margin = ""
let finding_margin = "  "
let stripe_glyph = "▌"

(* A trace explains the match rather than standing beside it: it keeps the
   match's own bar, and hangs off it as a tree, one branch per step. *)
let trace_inset = "  "

(* "  " ^ "▌" ^ " " *)
let prefix_width = 4

(* the spaces around the line number inside its band *)
let band_padding = 2

(*****************************************************************************)
(* Styling *)
(*****************************************************************************)

(* what `Fg and `Bg accept *)
type tone = [ Fmt.color | `Hi of Fmt.color ]

let severity_color (severity : OutJ.match_severity) : tone =
  match severity with
  | `Critical -> `Magenta
  | `Error
  | `High ->
      `Red
  | `Warning
  | `Medium ->
      `Yellow
  | `Info
  | `Low ->
      `Blue
  | `Inventory
  | `Experiment ->
      `Cyan

let severity_word (severity : OutJ.match_severity) : string =
  match severity with
  | `Critical -> "CRITICAL"
  | `Error
  | `High ->
      "ERROR"
  | `Warning
  | `Medium ->
      "WARN"
  | `Info
  | `Low ->
      "INFO"
  | `Inventory
  | `Experiment ->
      "NOTE"

(* A light foreground on a dark band, so that the band reads on a light
   terminal as well as a dark one. *)
let band_style : Fmt.style list = [ `Bg (`Hi `Black); `Fg (`Hi `White) ]

let styles (xs : Fmt.style list) (pp : 'a Fmt.t) : 'a Fmt.t =
  List.fold_left (fun acc style -> Fmt.styled style acc) pp xs

(* every line of a finding opens with the stripe, in the severity's colour *)
let pp_stripe (color : tone) ppf : unit =
  Fmt.pf ppf "%s%a " finding_margin Fmt.(styled (`Fg color) string) stripe_glyph

(* the stripe carrying on across a gap, with no trailing space *)
let pp_blank_stripe (color : tone) ppf : unit =
  Fmt.pf ppf "%s%a@." finding_margin Fmt.(styled (`Fg color) string) stripe_glyph

(*****************************************************************************)
(* A finding *)
(*****************************************************************************)

(* "  a.py ────────────────────────────" *)
let pp_file_header (ctx : Skin.ctx) ppf (path : string) : unit =
  let used = String.length margin + Utf8.length path + 1 in
  let rule =
    String.concat ""
      (List.init (max 3 (ctx.width - used)) (fun _ -> "─"))
  in
  Fmt.pf ppf "%s%a %a@." margin
    Fmt.(styled `Bold string)
    path
    Fmt.(styled `Faint string)
    rule

let pp_heading (color : tone) ppf (m : OutJ.cli_match) : unit =
  pp_stripe color ppf;
  Fmt.pf ppf "%a  %a@."
    (styles [ `Bg color; `Fg (`Hi `White); `Bold ] Fmt.string)
    (Printf.sprintf " %s " (severity_word m.extra.severity))
    Fmt.(styled `Faint string)
    (Rule_ID.to_string m.check_id)

let pp_message (ctx : Skin.ctx) (color : tone) ppf (message : string) :
    unit =
  message |> Findings_layout.message_paragraphs
  |> List.iteri (fun (i : int) ((extra_indent : int), (paragraph : string)) ->
         if i > 0 then pp_blank_stripe color ppf;
         Findings_layout.wrap_lines ~filler:Click
           ~width:
             (Findings_layout.safe_width
                (ctx.width - prefix_width - extra_indent))
           ~initial_indent:0 ~subsequent_indent:0 paragraph
         |> List.iter (fun ((_indentation : string), (txt : string)) ->
                pp_stripe color ppf;
                Fmt.pf ppf "%s%s@." (String.make extra_indent ' ') txt))

(* The lines of the match. The number sits in a band of its own; the matched
   span is tinted in the severity's colour rather than emboldened. *)
let pp_code (ctx : Skin.ctx) (color : tone) ppf (m : OutJ.cli_match) :
    unit =
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
  let band_width = digits + (2 * band_padding) in
  let available = ctx.width - prefix_width - band_width - 1 in
  let width =
    Findings_layout.safe_width
      (if ctx.max_chars_per_line > 0 then min ctx.max_chars_per_line available
       else available)
  in
  let pp_band ppf (label : string) : unit =
    Fmt.pf ppf "%a " (styles band_style Fmt.string) label
  in
  let blank_band = String.make band_width ' ' in
  lines
  |> List.iteri (fun (i : int) (line : string) ->
         let line_number = start_line + i in
         (* the tinted range of the match, moved by the dedent, exactly as
            the legacy skin computes it *)
         let col c = max 0 (c - 1 - dedented) in
         let tint_start =
           if line_number > start_line then 0 else col m.start.col
         in
         let tint_end =
           max tint_start
             (if line_number >= m.end_.line then
                min
                  (if m.start.line = m.end_.line then
                     tint_start + (m.end_.col - m.start.col)
                   else col m.end_.col)
                  (String.length line)
              else String.length line)
         in
         let text, offset_of = Findings_layout.munge_whitespace_with_offsets line in
         let moved (x : int) : int =
           offset_of.(max 0 (min (String.length line) x))
         in
         let tint_start = moved tint_start and tint_end = moved tint_end in
         Findings_layout.fill_chunks ~filler:Textwrap ~width ~initial_indent:0
           ~subsequent_indent:0 text
         |> List.iteri (fun (j : int) ((offset : int), (length : int)) ->
                let chunk = String.sub text offset length in
                let from = max 0 (min length (tint_start - offset)) in
                let upto = max from (min length (tint_end - offset)) in
                let a, b, c = Findings_layout.cut chunk from upto in
                pp_stripe color ppf;
                pp_band ppf
                  (if j = 0 then
                     Printf.sprintf "%*s%*d%*s" band_padding "" digits
                       line_number band_padding ""
                   else blank_band);
                Fmt.pf ppf "%s%a%s@." a
                  (styles [ `Bg color; `Fg (`Hi `White) ] Fmt.string)
                  b c));
  trimmed
  |> Option.iter (fun (n : int) ->
         pp_stripe color ppf;
         Fmt.pf ppf "%a@."
           Fmt.(styled `Faint string)
           (Printf.sprintf "%s… %s more" blank_band
              (String_.unit_str n "line")));
  (* --dataflow-traces. The stripe has to open every line the trace prints,
     so it is handed over already rendered. It is rendered with the renderer
     of this formatter, not of stdout: the same report also goes to
     -o/--text-output, whose buffer has no renderer and must stay free of
     escapes even while the terminal is getting colour. *)
  match m.extra.dataflow_trace with
  | Some trace when ctx.show_dataflow_traces ->
      let bar =
        Fmt.str_like ppf "%s%a" finding_margin
          Fmt.(styled (`Fg color) string)
          stripe_glyph
      in
      (* the same band the snippet above puts its numbers in *)
      let banded (label : string) : string =
        Fmt.str_like ppf "%a " (styles band_style Fmt.string) label
      in
      let gutter (n : int) : string =
        banded
          (Printf.sprintf "%*s%*d%*s" band_padding "" digits n band_padding "")
      in
      let gutter_blank = banded (String.make band_width ' ') in
      let faint (glyph : string) : string =
        Fmt.str_like ppf "%a" Fmt.(styled `Faint string) glyph
      in
      Findings_layout.pp_dataflow_tree
        ~line_prefix:(bar ^ trace_inset) ~glyph:faint ~gutter ~gutter_blank
        ~highlight:[ `Bg color; `Fg (`Hi `White) ]
        ppf trace
  | _ -> ()

let pp_finding (ctx : Skin.ctx) ppf (m : OutJ.cli_match) : unit =
  let color = severity_color m.extra.severity in
  pp_heading color ppf m;
  pp_message ctx color ppf m.extra.message;
  (* the stripe carries on across the gap between the message and the code *)
  pp_blank_stripe color ppf;
  pp_code ctx color ppf m;
  (match Option.map (Findings_layout.fix_lines ~first_col:m.start.col) m.extra.fix with
  | Some (first :: rest) ->
      (* set apart from the snippet, as the snippet is from the message; the
         bar opens every line of the fix, not just its first *)
      pp_blank_stripe color ppf;
      pp_stripe color ppf;
      Fmt.pf ppf "%a %s@."
        Fmt.(styled (`Fg (`Hi `Green)) string)
        "fix" first;
      let hanging = String.make (String.length "fix ") ' ' in
      rest
      |> List.iter (fun (l : string) ->
             pp_stripe color ppf;
             Fmt.pf ppf "%s%s@." hanging l)
  | _ -> ());
  Fmt.pf ppf "@."

(* the findings of one file under a header of its own, in the order they
   were reported *)
let pp_by_file (ctx : Skin.ctx) ppf (matches : OutJ.cli_match list) : unit =
  matches
  |> List.fold_left
       (fun (previous : string option) (m : OutJ.cli_match) ->
         let path = !!(m.path) in
         let here = Some path in
         if previous <> here then (
           pp_file_header ctx ppf path;
           Fmt.pf ppf "@.");
         pp_finding ctx ppf m;
         here)
       None
  |> ignore

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let name = "vivid"
let doc = "A colourful report: a severity stripe, banded line numbers."

let line (f : Format.formatter -> unit) : Skin.chunk =
  Skin.Line (Skin.Stderr Logs.App, f)

(* Only on a terminal, so a log does not gain it. *)
let on_start (_ctx : Skin.ctx) (start : M.Start.t) : Skin.chunk list =
  if not start.banner then []
  else [ line (fun ppf -> Skin_banner.pp ~margin:margin ppf) ]

let on_plan (_ctx : Skin.ctx) (plan : M.Plan.t) : Skin.chunk list =
  [
    line (fun ppf ->
        if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0
        then Fmt.pf ppf "%sNothing to scan." margin
        else
          Fmt.pf ppf "%sScanning %a with %a." margin
            Fmt.(styled `Bold string)
            (String_.unit_str plan.num_targets "file")
            Fmt.(styled `Bold string)
            (String_.unit_str plan.num_rules "rule"));
    (* the findings start their own block *)
    line (fun _ppf -> ());
  ]

let pp_summary ppf (summary : M.Summary.t) : unit =
  let str = M.string_of_phrase in
  Option.iter
    (fun (txt : string) -> Fmt.pf ppf "%s%s@." margin txt)
    summary.limited;
  summary.partially_analyzed
  |> Option.iter (fun p ->
         Fmt.pf ppf "%sPartially analyzed: %s@." margin (str p));
  if summary.unplaced_warnings > 0 then
    Fmt.pf ppf "%sAnalysis limited: %s about the scan, see --verbose.@." margin
      (String_.unit_str summary.unplaced_warnings "warning");
  match summary.skipped with
  | [] -> ()
  | xs ->
      Fmt.pf ppf "%sSkipped: %s@." margin
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
              Fmt.pf ppf "%s%a in %s, from %s." margin
                Fmt.(styled `Bold string)
                (String_.unit_str t.findings "finding")
                (String_.unit_str t.files_scanned "file")
                (String_.unit_str t.rules_ran "rule"));
        ]
  in
  (Skin.Findings :: summary) @ tally

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
