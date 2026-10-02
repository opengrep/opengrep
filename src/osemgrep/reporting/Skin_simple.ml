module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A plain report: the structure of the vivid one without its bars and most
 * of its colour.
 *
 * Each file is printed once above its findings; a finding is a severity, a
 * rule id and a message, with the code under them in a thin gutter. Colour
 * marks only the severity and the rule id, and the code starts four columns
 * from the margin rather than twelve.
 *
 * This module defines the look; Skin_common draws the report with it.
 *)

module M = Skin_model

(*****************************************************************************)
(* Measurements *)
(*****************************************************************************)

(* The column of the message and the gutter. Findings_layout.chunk_indentation
   adds two columns of its own (those rich added in the python wrapper), so
   an indent of 0 there starts here. *)
let indent_size = 2
let indent = String.make indent_size ' '

(* The code and its parts (the snippet, its trace, the fix) are one level
   deeper than the heading and the message. *)
let body_size = indent_size + 2
let body = String.make body_size ' '

(* Findings_layout.chunk_indentation adds console_indent_size columns, so a
   gutter with this indent starts at body_size. *)
let gutter_indent = body_size - Findings_layout.console_indent_size

let separator = " │ "

(* A located line of a trace is under the path printed before it, which
   Findings_layout.esc_prefix indents by two columns. *)
let trace_number_indent = "  "
let separator_width = Utf8.length separator

let from_label = "from: "
let fix_label = "fix: "

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

(* "  warn  os-system-concat", with the message under it at the same
   indent, and the code, the sources and the fix one level deeper *)
let finding (severity : OutJ.match_severity) : Skin_common.finding_look =
  let word, style = severity_word severity in
  {
    pp_head_margin = (fun ppf -> Fmt.string ppf indent);
    head_width = indent_size;
    pp_body_margin = (fun ppf -> Fmt.string ppf body);
    body_width = body_size;
    pp_blank = (fun ppf -> Fmt.pf ppf "@.");
    badge = word;
    pp_badge = Fmt.(styled style string);
    pp_rule_id = Fmt.(styled (`Fg `Cyan) string);
    (* the gutter is part of what pp_wrapped_code_line fills *)
    code_width = (fun ~width ~digits:_ -> width - body_size);
    pp_code_line =
      (fun ~digits ~width ppf (line : Findings_layout.code_line) ->
        let number_width = digits + separator_width in
        Findings_layout.pp_wrapped_code_line ~number_indent:gutter_indent
          ~code_indent:(gutter_indent + number_width) ~number_width ~separator
          ppf ~line_number:line.line_number ~width ~bold_start:line.match_start
          ~bold_end:line.match_end line.text);
    pp_more_lines =
      (fun ~digits:_ ppf (txt : string) ->
        Fmt.pf ppf "%a@." Fmt.(styled (`Fg `Cyan) string) txt);
    pp_from_label = (fun ppf -> Fmt.string ppf from_label);
    from_label_width = String.length from_label;
    trace =
      (fun _ppf ~digits ->
        {
          line_prefix = body;
          gutter =
            (fun (n : int) ->
              Printf.sprintf "%s%*d%s" trace_number_indent digits n separator);
          gutter_blank =
            trace_number_indent ^ String.make digits ' ' ^ separator;
          highlight = [ `Bold ];
        });
    pp_fix_label = (fun ppf -> Fmt.string ppf fix_label);
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
