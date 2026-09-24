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

(* A located line of a trace sits under the file name that introduces it,
   which Findings_layout.esc_prefix indents by two columns. *)
let trace_number_indent = "  "
(* derived, so that changing [separator] moves the gutter with it *)
let separator_width = Utf8.length separator

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
(* A deep path is wrapped rather than shortened: it is what a reader opens,
   and the report has no other copy of it. *)
let pp_file_header (ctx : Skin.ctx) ppf (path : string) : unit =
  Findings_layout.wrap_lines ~filler:Textwrap
    ~width:(Findings_layout.safe_width ctx.width) ~initial_indent:0
    ~subsequent_indent:0 path
  |> List.iter (fun ((_ : string), (txt : string)) ->
         Fmt.pf ppf "%a@." Fmt.(styled `Bold string) txt);
  Fmt.pf ppf "@."

(* A ci report splits its findings into the ones that fail the run and the
   ones that do not, under a heading each, so nothing is repeated on every
   finding: the section it sits in already says which it is. *)
let pp_section ppf (title : string) (style : Fmt.style) (count : int) : unit =
  Fmt.pf ppf "%a %a@.@."
    Fmt.(styled style string)
    title
    Fmt.(styled `Faint string)
    (Printf.sprintf "· %s" (String_.unit_str count "finding"))

(* The distinct rules behind the findings that fail the run: what a reader
   has to go and fix before the build passes. There is no non-blocking
   counterpart, as there is nothing to act on. *)
let pp_rules_fired ppf (matches : OutJ.cli_match list) : unit =
  let ids =
    matches
    |> List_.map (fun (m : OutJ.cli_match) -> Rule_ID.to_string m.check_id)
    |> List.sort_uniq String.compare
  in
  if not (List_.null ids) then begin
    Fmt.pf ppf "%a@." Fmt.(styled `Bold string) "blocking rules fired";
    ids
    |> List.iter (fun (id : string) ->
           Fmt.pf ppf "%s%a@." indent Fmt.(styled (`Fg `Cyan) string) id);
    Fmt.pf ppf "@."
  end

(* "  warn  os-system-concat"; the line is in the gutter of the snippet *)
(* A long id is wrapped rather than shortened: it is what a reader copies
   to silence or search for the rule, so all of it has to be there. The
   break lands wherever the width falls, mid-token if need be, as the
   legacy report breaks it -- ids this long are rare enough that a tidier
   rule is not worth the machinery. *)
let pp_heading (ctx : Skin.ctx) ppf (m : OutJ.cli_match) : unit =
  let word, style = severity_word m.extra.severity in
  (* the column the id starts at, which its later lines hang under *)
  let id_column = indent_size + String.length word + 2 in
  let pp_id : string Fmt.t = Fmt.(styled (`Fg `Cyan) string) in
  match
    Findings_layout.wrap_lines ~filler:Textwrap
      ~width:(Findings_layout.safe_width (ctx.width - id_column))
      ~initial_indent:0 ~subsequent_indent:0
      (Rule_ID.to_string m.check_id)
  with
  | [] -> ()
  | (_, first) :: rest ->
      Fmt.pf ppf "%s%a  %a@." indent Fmt.(styled style string) word pp_id first;
      let hanging = String.make id_column ' ' in
      rest
      |> List.iter (fun ((_ : string), (txt : string)) ->
             Fmt.pf ppf "%s%a@." hanging pp_id txt)

(* Nothing is printed for a rule with no message: the indent on its own
   would be a blank line of trailing whitespace. *)
let pp_message (ctx : Skin.ctx) ppf (message : string) : unit =
  if String.equal (String.trim message) "" then ()
  else
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
  ()

(* Under --interfile-dedup-by source-sink the findings sharing this sink
   differ only in where the taint started, so the sink is drawn once and
   each source named under it. Without this the block appears once per
   source with nothing to tell the copies apart.

   Each source is followed by its own trace rather than all the sources
   first and all the traces after: the pairing is what makes a trace
   readable, since on its own it does not say which source it explains. *)
let pp_origins (ctx : Skin.ctx) ppf (m : OutJ.cli_match)
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
           if (i = 0 && name_sources) || (i > 0 && traces) then Fmt.pf ppf "@.";
           if name_sources then
             Findings_layout.source_of_finding finding
             |> Option.iter (fun ((loc : OutJ.location), (code : string)) ->
                    let where =
                      Printf.sprintf "%s:%d" !!(loc.path) loc.start.line
                    in
                    (* The path is never shortened -- it is what a
                       reader opens -- so the clause wraps instead, the
                       break falling on the gap before the code where it
                       can and inside the path only when the path alone is
                       wider than the line. The code is still cut, since a
                       source spanning several lines arrives here as one.
                       Labelled as the fix below it is, so the two read as
                       the same kind of aside. *)
                    let label = "from: " in
                    let column = body_size + String.length label in
                    let hanging = String.make column ' ' in
                    (* the locator is coloured, the code beside it is not:
                       it is code, and reads as the snippets above do *)
                    Findings_layout.from_clause_lines
                      ~width:(ctx.width - column) ~located:where ~code
                    |> List.iteri
                         (fun (i : int)
                              ((located : string), (code : string)) ->
                           let opener =
                             if i = 0 then body ^ label else hanging
                           in
                           (* a line of code alone opens no colour span:
                              an empty styled string is just two escapes *)
                           if String.equal located "" then
                             Fmt.pf ppf "%s%s@." opener code
                           else
                             Fmt.pf ppf "%s%a%s@." opener
                               Fmt.(styled (`Fg `Cyan) string)
                               located
                               (if String.equal code "" then ""
                                else "  " ^ code)));
           if traces then
             finding.extra.dataflow_trace
             |> Option.iter (fun trace ->
                    Findings_layout.pp_dataflow_tree
                      ~finding_path:finding.path ~line_prefix:body ~glyph:faint
                      ~gutter:(fun (n : int) ->
                        Printf.sprintf "%s%*d%s" trace_number_indent digits n
                          separator)
                      ~gutter_blank:
                        (trace_number_indent ^ String.make digits ' '
                       ^ separator)
                      ~highlight:[ `Bold ] ppf trace))
  end

(* [heading] is false for a match that repeats the rule and message of the
   one before it in the same file: those are the same finding said again of
   another line, and the report states the rule once and lets the snippets
   follow. The blank line under the message goes with it, the finding above
   having already closed with one. *)
let pp_finding ?(group : OutJ.cli_match list = []) ?(heading = true)
    (ctx : Skin.ctx) ppf (m : OutJ.cli_match) : unit =
  (* Guarded here rather than where [heading] is decided, so that no
     caller can head a -e finding by asking for one. *)
  if heading && Findings_layout.has_rule_name m then begin
    pp_heading ctx ppf m;
    pp_message ctx ppf m.extra.message;
    (* the blank line that sets the snippet apart from the message *)
    Fmt.pf ppf "@."
  end;
  pp_code ctx ppf m;
  pp_origins ctx ppf m group;
  (match Option.map (Findings_layout.fix_lines ~first_col:m.start.col) m.extra.fix with
  (* a fix with no text deletes the match, which the report has to say:
     the code goes away when --autofix runs *)
  | Some [] ->
      Fmt.pf ppf "@.";
      Fmt.pf ppf "%sfix: %a@." body Fmt.(styled (`Fg `Red) string) "delete"
  | Some (_ :: _ as fix) ->
      (* set apart from the snippet, as the snippet is from the message;
         the lines after the first hang under the text, not the label. A
         one-line fix can be far wider than the report, so each line is
         wrapped as the legacy report wraps it. *)
      let label = "fix: " in
      let hanging = body ^ String.make (String.length label) ' ' in
      let width =
        Findings_layout.safe_width
          (ctx.width - body_size - String.length label)
      in
      Fmt.pf ppf "@.";
      let first = ref true in
      fix
      |> List.iter (fun (line : string) ->
             Findings_layout.wrap_lines ~filler:Textwrap ~width
               ~initial_indent:0 ~subsequent_indent:0 line
             |> List.iter (fun ((_ : string), (txt : string)) ->
                    if !first then (
                      Fmt.pf ppf "%s%s%s@." body label txt;
                      first := false)
                    else if String.equal txt "" then
                      (* a blank line of the fix is blank: the hanging
                         indent alone would be trailing whitespace *)
                      Fmt.pf ppf "@."
                    else Fmt.pf ppf "%s%s@." hanging txt))
  | None -> ());
  Fmt.pf ppf "@."

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let doc = "A plain report: no boxes, and colour only where it names things."

let line (f : Format.formatter -> unit) : Skin.chunk =
  Skin.Line (Skin.Stderr Logs.App, f)

(* Only on a terminal, so a log does not gain it. *)
(* what 'opengrep ci' runs in, stated in one line rather than a block *)
let ci_environment (env : M.Start.ci_env) : Skin.chunk list =
  [
    line (fun ppf ->
        Fmt.pf ppf "opengrep %s on OCaml %s · %s · %s@." env.version
          env.ocaml_version env.environment env.event_name);
  ]

let on_start (_ctx : Skin.ctx) (start : M.Start.t) : Skin.chunk list =
  (match start.ci with
  | Some env -> ci_environment env
  | None -> [])
  @
  if not start.banner then []
  else [ line (fun ppf -> Skin_banner.pp ~margin:"" ppf) ]

(* No status line, and so no spinner either: this skin says nothing until
   it has something to report. *)
let rules_status (_ctx : Skin.ctx) (_start : M.Start.t) : string option = None

let on_plan (_ctx : Skin.ctx) (plan : M.Plan.t) : Skin.chunk list =
  [
    line (fun ppf ->
        (* A --baseline-commit scan says this twice; the second is the
           replay, and says so. *)
        let prefix =
          match plan.run with
          | M.Plan.Current -> ""
          | M.Plan.Baseline -> "baseline · "
        in
        if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0
        then Fmt.pf ppf "%snothing to scan" prefix
        else
          (* The files a rule will look at and the rules that have one to
             look at, not everything targeting found and everything that
             was loaded. On a mixed repository the two are far apart -- one
             Java rule over a Java benchmark pairs 2766 files out of the
             5703 found -- and this line is what the scan is about to do. *)
          Fmt.pf ppf "%s%s · %s" prefix
            (String_.unit_str plan.num_files_with_a_rule "file")
            (String_.unit_str plan.num_rules_with_a_target "rule"));
    (* the findings start their own block *)
    line (fun _ppf -> ());
  ]

let pp_summary ppf (summary : M.Summary.t) : unit =
  let str = M.string_of_phrase in
  Option.iter (fun (txt : string) -> Fmt.pf ppf "%s@." txt) summary.limited;
  (* Worded here rather than taken plain: the phrase's own noun is the
     whole of "N files only partially analyzed due to a ... error", which
     under a label of the same name would be said twice, and which is
     plural whatever it counts. The count is all this line needs. *)
  summary.partially_analyzed
  |> Option.iter (fun (p : M.phrase) ->
         Fmt.pf ppf "partially analyzed: %s (parse or internal error)@."
           (String_.unit_str (M.total_of_phrase p) "file"));
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
              (* "in 0 files" says nothing; a clean scan just says so *)
              if Int.equal t.findings 0 then Fmt.pf ppf "no findings"
              else
                Fmt.pf ppf "%s in %s"
                  (String_.unit_str t.findings "finding")
                  (String_.unit_str t.files_with_findings "file"));
        ]
  in
  (Skin.Findings :: summary) @ tally

(* the findings of one file under a name stated once, in reported order *)
let pp_by_file (ctx : Skin.ctx) ppf (matches : OutJ.cli_match list) : unit =
  let groups =
    match ctx.interfile_dedup_by with
    | Core_match.Sink -> List_.map (fun (m : OutJ.cli_match) -> [ m ]) matches
    | Core_match.Source_sink -> Findings_layout.group_findings_by_sink matches
  in
  groups
  |> List.fold_left
       (fun ((previous : string option), (said : (Rule_ID.t * string) option))
            (group : OutJ.cli_match list) ->
         match group with
         | [] -> (previous, said)
         | (m : OutJ.cli_match) :: _ ->
             let path = !!(m.path) in
             let here = Some path in
             let file_changed = previous <> here in
             if file_changed then pp_file_header ctx ppf path;
             (* the message and not just the rule: it carries the
                metavariables of this match, so two findings of one rule can
                still have something different to say *)
             let heading =
               file_changed
               ||
               match said with
               | None -> true
               | Some (id, msg) ->
                   (not (Rule_ID.equal id m.check_id))
                   || not (String.equal msg m.extra.message)
             in
             pp_finding ~group ~heading ctx ppf m;
             (here, Some (m.check_id, m.extra.message)))
       (None, None)
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
     let section (title : string) (style : Fmt.style)
         (matches : OutJ.cli_match list) : unit =
       if not (List_.null matches) then begin
         pp_section ppf title style (List.length matches);
         pp_by_file ctx ppf matches
       end
     in
     section "blocking" (`Fg `Red) blocking;
     section "non-blocking" `Faint advisory;
     pp_rules_fired ppf blocking);
  pp_time ppf cli_output

let wants_status_bar = true
