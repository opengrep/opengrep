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

(* Every badge is as wide as the longest severity word, so that the filled
   rectangles line up down the page and the id after them starts at one
   column whatever the severity.
   coupling: a severity added to [severity_word] belongs here too, or a
   badge of that severity will be the one that sticks out. *)
let severity_field_width : int =
  [ `Critical; `Error; `High; `Warning; `Medium; `Info; `Low; `Inventory;
    `Experiment ]
  |> List.fold_left
       (fun (acc : int) (s : OutJ.match_severity) ->
         max acc (String.length (severity_word s)))
       0

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
(* a name with a rule running out to the width of the report *)
(* A deep path is wrapped rather than shortened: it is what a reader opens,
   and the report has no other copy of it. The rule closes the last line,
   so the header still reads as one band however many lines it took. *)
let pp_section (ctx : Skin.ctx) ppf (name : string) : unit =
  let width =
    Findings_layout.safe_width (ctx.width - String.length margin - 1)
  in
  let lines =
    Findings_layout.wrap_lines ~filler:Textwrap ~width ~initial_indent:0
      ~subsequent_indent:0 name
    |> List_.map snd
  in
  let last = List.length lines - 1 in
  lines
  |> List.iteri (fun (i : int) (txt : string) ->
         if i < last then
           Fmt.pf ppf "%s%a@." margin Fmt.(styled `Bold string) txt
         else
           let used = String.length margin + Utf8.length txt + 1 in
           let rule =
             String.concat ""
               (List.init (max 3 (ctx.width - used)) (fun _ -> "─"))
           in
           Fmt.pf ppf "%s%a %a@." margin
             Fmt.(styled `Bold string)
             txt
             Fmt.(styled `Faint string)
             rule)

let pp_file_header (ctx : Skin.ctx) ppf (path : string) : unit =
  pp_section ctx ppf path

(* A ci report splits its findings into the ones that fail the run and the
   ones that do not, under a heading each, so nothing is repeated on every
   finding: the section it sits in already says which it is. The heading
   outweighs the file rules beneath it by carrying a badge rather than a
   second rule. *)
let pp_ci_section ppf ~(badge : bool) (label : string)
    (style : Fmt.style list) (count : int) : unit =
  (* the padding belongs to a filled badge, which has a background to put
     it on; plain text would only gain a stray space either side *)
  let text = if badge then Printf.sprintf " %s " label else label in
  Fmt.pf ppf "%s%a %a@.@." margin
    (styles style Fmt.string)
    text
    Fmt.(styled `Faint string)
    (Printf.sprintf "· %s" (String_.unit_str count "finding"))

(* The distinct rules behind the findings that fail the run: what a reader
   has to go and fix before the build passes. There is no non-blocking
   counterpart, as there is nothing to act on. *)
let pp_rules_fired (ctx : Skin.ctx) ppf (matches : OutJ.cli_match list) : unit =
  let ids =
    matches
    |> List_.map (fun (m : OutJ.cli_match) -> Rule_ID.to_string m.check_id)
    |> List.sort_uniq String.compare
  in
  if not (List_.null ids) then begin
    pp_section ctx ppf "blocking rules fired";
    Fmt.pf ppf "@.";
    ids
    |> List.iter (fun (id : string) ->
           Fmt.pf ppf "%s  %a@." margin Fmt.(styled (`Fg `Cyan) string) id);
    Fmt.pf ppf "@."
  end

(* A long id is wrapped rather than shortened: it is what a reader copies
   to silence or search for the rule, so all of it has to be there. The
   break lands wherever the width falls, mid-token if need be, as the
   legacy report breaks it. Every line of it opens with the stripe. *)
let pp_heading (ctx : Skin.ctx) (color : tone) ppf (m : OutJ.cli_match) : unit
    =
  let badge =
    Printf.sprintf " %-*s " severity_field_width (severity_word m.extra.severity)
  in
  (* the column the id starts at, past the stripe the line opens with *)
  let id_column = String.length badge + 2 in
  let pp_id : string Fmt.t = Fmt.(styled `Faint string) in
  match
    Findings_layout.wrap_lines ~filler:Textwrap
      ~width:
        (Findings_layout.safe_width (ctx.width - prefix_width - id_column))
      ~initial_indent:0 ~subsequent_indent:0
      (Rule_ID.to_string m.check_id)
  with
  | [] -> ()
  | (_, first) :: rest ->
      pp_stripe color ppf;
      Fmt.pf ppf "%a  %a@."
        (styles [ `Bg color; `Fg (`Hi `White); `Bold ] Fmt.string)
        badge pp_id first;
      let hanging = String.make id_column ' ' in
      rest
      |> List.iter (fun ((_ : string), (txt : string)) ->
             pp_stripe color ppf;
             Fmt.pf ppf "%s%a@." hanging pp_id txt)

(* Nothing is printed for a rule with no message: the stripe on its own
   would be a blank line of trailing whitespace. *)
let pp_message (ctx : Skin.ctx) (color : tone) ppf (message : string) :
    unit =
  if String.equal (String.trim message) "" then ()
  else
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
  ()

(* Under --interfile-dedup-by source-sink the findings sharing this sink
   differ only in where the taint started, so the sink is drawn once and
   each source named under it, inside the stripe. Each source is followed by
   its own trace rather than all the sources first and all the traces after:
   the pairing is what makes a trace readable, since on its own it does not
   say which source it explains.

   The stripe has to open every line a trace prints, so it is handed over
   already rendered. It is rendered with the renderer of this formatter, not
   of stdout: the same report also goes to -o/--text-output, whose buffer
   has no renderer and must stay free of escapes even while the terminal is
   getting colour. *)
let pp_origins (ctx : Skin.ctx) (color : tone) ppf (m : OutJ.cli_match)
    (group : OutJ.cli_match list) : unit =
  let entries = if group = [] then [ m ] else group in
  (* Named for every interfile finding, not only where several share a
     sink. The line is provenance, not a way of telling duplicates apart:
     the snippet is the sink, and for a cross-file flow it says nothing
     about where the untrusted value entered. The legacy report names the
     source whenever the rule is interfile, and this is the default skin. *)
  let name_sources = ctx.is_interfile m.check_id in
  let traces = ctx.show_dataflow_traces in
  if name_sources || traces then begin
    (* wide enough for the largest number the traces below will draw,
       which is not the finding's own: a step can sit far down another
       file *)
    let digits = Findings_layout.trace_line_digits entries in
    let band_width = digits + (2 * band_padding) in
    let bar =
      Fmt.str_like ppf "%s%a" finding_margin
        Fmt.(styled (`Fg color) string)
        stripe_glyph
    in
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
    entries
    |> List.iteri (fun (i : int) (finding : OutJ.cli_match) ->
           (* a gap opens the block when a source line leads it, and
              divides one entry from the next only once each carries a
              trace. A trace following the snippet needs no gap of its own:
              its spine already joins the two, and a bare list of sources
              reads better tight. *)
           if (i = 0 && name_sources) || (i > 0 && traces) then pp_blank_stripe color ppf;
           if name_sources then
             Findings_layout.source_of_finding finding
             |> Option.iter (fun ((loc : OutJ.location), (code : string)) ->
                    let where =
                      Printf.sprintf "%s:%d" !!(loc.path) loc.start.line
                    in
                    (* The path is never shortened -- it is what a
                       reader opens -- so the clause wraps instead, the
                       break falling on the gap before the code where it
                       can. The code is still cut, since a source spanning
                       several lines arrives here as one. Every line opens
                       with the stripe. *)
                    let column = String.length "from " in
                    let hanging = String.make column ' ' in
                    (* the locator is coloured, the code beside it is not:
                       it is code, and reads as the snippets above do *)
                    Findings_layout.from_clause_lines
                      ~width:(ctx.width - prefix_width - column)
                      ~located:where ~code
                    |> List.iteri
                         (fun (i : int)
                              ((located : string), (code : string)) ->
                           pp_stripe color ppf;
                           if i = 0 then
                             Fmt.pf ppf "%a "
                               Fmt.(styled `Faint string)
                               "from"
                           else Fmt.pf ppf "%s" hanging;
                           (* a line of code alone opens no colour span:
                              an empty styled string is just two escapes *)
                           if String.equal located "" then
                             Fmt.pf ppf "%s@." code
                           else
                             Fmt.pf ppf "%a%s@."
                               Fmt.(styled (`Fg `Cyan) string)
                               located
                               (if String.equal code "" then ""
                                else "  " ^ code)));
           if traces then
             finding.extra.dataflow_trace
             |> Option.iter (fun trace ->
                    Findings_layout.pp_dataflow_tree
                      ~finding_path:finding.path
                      ~line_prefix:(bar ^ trace_inset) ~glyph:faint ~gutter
                      ~gutter_blank
                      ~highlight:[ `Bg color; `Fg (`Hi `White) ]
                      ppf trace))
  end

(* [heading] is false for a match that repeats the rule and message of the
   one before it in the same file: those are the same finding said again of
   another line, and the report states the rule once and lets the snippets
   follow. *)
let pp_finding ?(group : OutJ.cli_match list = []) ?(heading = true)
    ?(more_follows = false) (ctx : Skin.ctx) ppf (m : OutJ.cli_match) : unit =
  let color = severity_color m.extra.severity in
  (* see the note in Skin_simple.pp_finding *)
  if heading && Findings_layout.has_rule_name m then begin
    pp_heading ctx color ppf m;
    pp_message ctx color ppf m.extra.message;
    (* the stripe carries on across the gap between the message and the
       code *)
    pp_blank_stripe color ppf
  end;
  pp_code ctx color ppf m;
  pp_origins ctx color ppf m group;
  (match Option.map (Findings_layout.fix_lines ~first_col:m.start.col) m.extra.fix with
  (* a fix with no text deletes the match, which the report has to say:
     the code goes away when --autofix runs *)
  | Some [] ->
      pp_blank_stripe color ppf;
      pp_stripe color ppf;
      Fmt.pf ppf "%a %a@."
        Fmt.(styled (`Fg (`Hi `Green)) string)
        "fix"
        Fmt.(styled (`Fg `Red) string)
        "delete"
  | Some (_ :: _ as fix) ->
      (* set apart from the snippet, as the snippet is from the message; the
         bar opens every line of the fix, not just its first. A one-line fix
         can be far wider than the report, so each line is wrapped as the
         legacy report wraps it. *)
      let label = "fix " in
      let hanging = String.make (String.length label) ' ' in
      let width =
        Findings_layout.safe_width
          (ctx.width - prefix_width - String.length label)
      in
      pp_blank_stripe color ppf;
      let first = ref true in
      fix
      |> List.iter (fun (line : string) ->
             Findings_layout.wrap_lines ~filler:Textwrap ~width
               ~initial_indent:0 ~subsequent_indent:0 line
             |> List.iter (fun ((_ : string), (txt : string)) ->
                    if !first then (
                      pp_stripe color ppf;
                      Fmt.pf ppf "%a %s@."
                        Fmt.(styled (`Fg (`Hi `Green)) string)
                        "fix" txt;
                      first := false)
                    else if String.equal txt "" then
                      (* a blank line of the fix is blank: the stripe that
                         opens an ordinary line ends in a space, so this
                         one is drawn by the blank form instead *)
                      pp_blank_stripe color ppf
                    else begin
                      pp_stripe color ppf;
                      Fmt.pf ppf "%s%s@." hanging txt
                    end))
  | None -> ());
  (* A finding closes with a plain blank line, but one that is only a
     further snippet of the rule above keeps the stripe running: the bar is
     meant to span the whole of what the rule found in this file. *)
  if more_follows then pp_blank_stripe color ppf else Fmt.pf ppf "@."

(* the findings of one file under a header of its own, in the order they
   were reported *)
let pp_by_file (ctx : Skin.ctx) ppf (matches : OutJ.cli_match list) : unit =
  let groups =
    match ctx.interfile_dedup_by with
    | Core_match.Sink -> List_.map (fun (m : OutJ.cli_match) -> [ m ]) matches
    | Core_match.Source_sink -> Findings_layout.group_findings_by_sink matches
  in
  (* Two findings continue one another when they are the same rule saying
     the same thing about the same file: the report states that once and
     lets the snippets follow, the stripe running unbroken between them. *)
  let continues (a : OutJ.cli_match) (b : OutJ.cli_match) : bool =
    Fpath.equal a.path b.path
    && Rule_ID.equal a.check_id b.check_id
    && String.equal a.extra.message b.extra.message
  in
  let head (g : OutJ.cli_match list) : OutJ.cli_match option =
    match g with
    | m :: _ -> Some m
    | [] -> None
  in
  (* each group with whether the next one carries on from it *)
  let rec paired (gs : OutJ.cli_match list list) :
      (OutJ.cli_match list * bool) list =
    match gs with
    | [] -> []
    | [ g ] -> [ (g, false) ]
    | g :: (h :: _ as rest) ->
        let carries_on =
          match (head g, head h) with
          | Some a, Some b -> continues a b
          | _ -> false
        in
        (g, carries_on) :: paired rest
  in
  paired groups
  |> List.fold_left
       (fun ((previous : string option), (said : (Rule_ID.t * string) option))
            ((group : OutJ.cli_match list), (more_follows : bool)) ->
         match group with
         | [] -> (previous, said)
         | (m : OutJ.cli_match) :: _ ->
             let path = !!(m.path) in
             let here = Some path in
             let file_changed = previous <> here in
             if file_changed then (
               pp_file_header ctx ppf path;
               Fmt.pf ppf "@.");
             let heading =
               file_changed
               ||
               match said with
               | None -> true
               | Some (id, msg) ->
                   (not (Rule_ID.equal id m.check_id))
                   || not (String.equal msg m.extra.message)
             in
             pp_finding ~group ~heading ~more_follows ctx ppf m;
             (here, Some (m.check_id, m.extra.message)))
       (None, None)
  |> ignore

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let doc = "A colourful report: a severity stripe, banded line numbers."

let line (f : Format.formatter -> unit) : Skin.chunk =
  Skin.Line (Skin.Stderr Logs.App, f)

(* Only on a terminal, so a log does not gain it. *)
(* what 'opengrep ci' runs in, under a rule of its own *)
let ci_environment (ctx : Skin.ctx) (env : M.Start.ci_env) : Skin.chunk list =
  [
    line (fun ppf ->
        pp_section ctx ppf "Debugging info";
        Fmt.pf ppf "@.";
        Fmt.pf ppf "%s  versions     opengrep %a on OCaml %a@." margin
          Fmt.(styled `Bold string)
          env.version
          Fmt.(styled `Bold string)
          env.ocaml_version;
        Fmt.pf ppf "%s  environment  %a, triggering event is %a@." margin
          Fmt.(styled `Bold string)
          env.environment
          Fmt.(styled `Bold string)
          env.event_name);
  ]

let on_start (ctx : Skin.ctx) (start : M.Start.t) : Skin.chunk list =
  (match start.ci with
  | Some env -> ci_environment ctx env
  | None -> [])
  @
  if not start.banner then []
  else [ line (fun ppf -> Skin_banner.pp ~margin:margin ppf) ]

(* No status line, and so no spinner either: this skin says nothing until
   it has something to report. *)
let rules_status (_ctx : Skin.ctx) (_start : M.Start.t) : string option = None

let on_plan (_ctx : Skin.ctx) (plan : M.Plan.t) : Skin.chunk list =
  [
    line (fun ppf ->
        (* A --baseline-commit scan says this twice; the second is the
           replay, and says so. *)
        let nothing, scanning =
          match plan.run with
          | M.Plan.Current -> ("Nothing to scan.", "Scanning")
          | M.Plan.Baseline ->
              ("Baseline: nothing to scan.", "Baseline: scanning")
        in
        if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0
        then Fmt.pf ppf "%s%s" margin nothing
        else
          (* see the note in Skin_simple.on_plan *)
          Fmt.pf ppf "%s%s %a with %a." margin scanning
            Fmt.(styled `Bold string)
            (String_.unit_str plan.num_files_with_a_rule "file")
            Fmt.(styled `Bold string)
            (String_.unit_str plan.num_rules_with_a_target "rule"));
    (* the findings start their own block *)
    line (fun _ppf -> ());
  ]

let pp_summary ppf (summary : M.Summary.t) : unit =
  let str = M.string_of_phrase in
  Option.iter
    (fun (txt : string) -> Fmt.pf ppf "%s%s@." margin txt)
    summary.limited;
  (* see the note in Skin_simple.pp_summary *)
  summary.partially_analyzed
  |> Option.iter (fun (p : M.phrase) ->
         Fmt.pf ppf "%sPartially analyzed: %s (parse or internal error)@."
           margin
           (String_.unit_str (M.total_of_phrase p) "file"));
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
              (* "in 0 files" says nothing; a clean scan just says so *)
              if Int.equal t.findings 0 then
                Fmt.pf ppf "%s%a." margin
                  Fmt.(styled `Bold string)
                  "No findings"
              else
                Fmt.pf ppf "%s%a in %s, from %s." margin
                  Fmt.(styled `Bold string)
                  (String_.unit_str t.findings "finding")
                  (String_.unit_str t.files_with_findings "file")
                  (String_.unit_str t.rules_with_findings "rule"));
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
  let sorted = cli_output.results |> Semgrep_output_utils.sort_cli_matches in
  (if not ctx.is_ci_invocation then pp_by_file ctx ppf sorted
   else
     (* a ci report is read to answer one question first -- what fails the
        run -- so the findings that do come first, under a heading of their
        own, and the rules behind them close the report *)
     let blocking, advisory =
       List.partition
         (fun (m : OutJ.cli_match) ->
           Findings_layout.is_blocking m.extra.metadata)
         sorted
     in
     let section ~(badge : bool) (label : string) (style : Fmt.style list)
         (matches : OutJ.cli_match list) : unit =
       if not (List_.null matches) then begin
         pp_ci_section ppf ~badge label style (List.length matches);
         pp_by_file ctx ppf matches
       end
     in
     section ~badge:true "BLOCKING"
       [ `Bg `Red; `Fg (`Hi `White); `Bold ]
       blocking;
     section ~badge:false "non-blocking" [ `Faint ] advisory;
     pp_rules_fired ctx ppf blocking);
  pp_time ppf cli_output

let wants_status_bar = true
