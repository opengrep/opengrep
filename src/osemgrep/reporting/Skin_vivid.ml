module OutJ = Semgrep_output_v1_t

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
 *
 * Only the look is here: Skin_common draws the report with it.
 *)

module M = Skin_model

(*****************************************************************************)
(* Measurements *)
(*****************************************************************************)

(* The report sits at the left edge: the banner, the file names, the scan
   lines and the summary all start there. Only the findings are inset, so a
   file's contents read as belonging to the name above them. *)
let finding_margin = "  "
let stripe_glyph = "▌"

(* A trace explains the match rather than standing beside it: it keeps the
   match's own bar, and hangs off it as a tree, one branch per step. *)
let trace_inset = "  "

(* "  " ^ "▌" ^ " " *)
let prefix_width = 4

(* the spaces around the line number inside its band *)
let band_padding = 2

(* the band the line numbers of a finding sit in, for numbers [digits]
   wide *)
let band_width (digits : int) : int = digits + (2 * band_padding)

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

let pp_band ppf (label : string) : unit =
  Fmt.pf ppf "%a " (styles band_style Fmt.string) label

(*****************************************************************************)
(* A finding *)
(*****************************************************************************)

(* A line of the match. The number sits in a band of its own; the matched
   span is tinted in the severity's colour rather than emboldened. *)
let pp_code_line (color : tone) ~(digits : int) ~(width : int) ppf
    (line : Findings_layout.code_line) : unit =
  let text, offset_of = Findings_layout.munge_whitespace_with_offsets line.text in
  (* the tinted range of the match, moved as the whitespace is *)
  let moved (x : int) : int = offset_of.(max 0 (min (String.length line.text) x)) in
  let tint_start = moved line.match_start and tint_end = moved line.match_end in
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
              Printf.sprintf "%*s%*d%*s" band_padding "" digits line.line_number
                band_padding ""
            else String.make (band_width digits) ' ');
         Fmt.pf ppf "%s%a%s@." a
           (styles [ `Bg color; `Fg (`Hi `White) ] Fmt.string)
           b c)

(* The trace keeps the stripe: it opens every line the trace prints, so it
   is handed over already rendered, with the numbers in the same band as
   the code's. *)
let trace (color : tone) ppf ~(digits : int) : Skin_common.trace_look =
  let bar =
    Fmt.str_like ppf "%s%a" finding_margin
      Fmt.(styled (`Fg color) string)
      stripe_glyph
  in
  let banded (label : string) : string =
    Fmt.str_like ppf "%a " (styles band_style Fmt.string) label
  in
  {
    line_prefix = bar ^ trace_inset;
    gutter =
      (fun (n : int) ->
        banded
          (Printf.sprintf "%*s%*d%*s" band_padding "" digits n band_padding ""));
    gutter_blank = banded (String.make (band_width digits) ' ');
    highlight = [ `Bg color; `Fg (`Hi `White) ];
  }

(* Every line of a finding opens with the stripe: the heading, a filled
   badge and the id; the message; the code; the sources; the fix. *)
let finding (severity : OutJ.match_severity) : Skin_common.finding_look =
  let color = severity_color severity in
  {
    pp_head_margin = pp_stripe color;
    head_width = prefix_width;
    pp_body_margin = pp_stripe color;
    body_width = prefix_width;
    pp_blank = pp_blank_stripe color;
    badge = Printf.sprintf " %-*s " severity_field_width (severity_word severity);
    pp_badge = styles [ `Bg color; `Fg (`Hi `White); `Bold ] Fmt.string;
    pp_rule_id = Fmt.(styled `Faint string);
    code_width =
      (fun ~width ~digits -> width - prefix_width - band_width digits - 1);
    pp_code_line = pp_code_line color;
    pp_more_lines =
      (fun ~digits ppf (txt : string) ->
        Fmt.pf ppf "%a@."
          Fmt.(styled `Faint string)
          (String.make (band_width digits) ' ' ^ txt));
    pp_from_label =
      (fun ppf -> Fmt.pf ppf "%a " Fmt.(styled `Faint string) "from");
    from_label_width = String.length "from ";
    trace = trace color;
    pp_fix_label =
      (fun ppf -> Fmt.pf ppf "%a " Fmt.(styled (`Fg (`Hi `Green)) string) "fix");
    fix_label_width = String.length "fix ";
  }

(*****************************************************************************)
(* Around the findings *)
(*****************************************************************************)

(* "a.py ──────────────": a name with a rule running out to the width of
   the report. A deep path is wrapped rather than shortened: it is what a
   reader opens, and the report has no other copy of it. The rule closes
   the last line, so the header still reads as one band however many lines
   it took. *)
let pp_section (ctx : Skin.ctx) ppf (name : string) : unit =
  let width = Findings_layout.safe_width (ctx.width - 1) in
  let lines =
    Findings_layout.wrap_lines ~filler:Textwrap ~width ~initial_indent:0
      ~subsequent_indent:0 name
    |> List_.map snd
  in
  let last = List.length lines - 1 in
  lines
  |> List.iteri (fun (i : int) (txt : string) ->
         if i < last then Fmt.pf ppf "%a@." Fmt.(styled `Bold string) txt
         else
           let used = Utf8.length txt + 1 in
           let rule =
             String.concat ""
               (List.init (max 3 (ctx.width - used)) (fun _ -> "─"))
           in
           Fmt.pf ppf "%a %a@."
             Fmt.(styled `Bold string)
             txt
             Fmt.(styled `Faint string)
             rule)

let pp_file_header (ctx : Skin.ctx) ppf (path : string) : unit =
  pp_section ctx ppf path;
  Fmt.pf ppf "@."

(* The heading of a ci section: nothing is repeated on every finding, as
   the section it sits in already says which kind it is. The heading
   outweighs the file rules beneath it by carrying a badge rather than a
   second rule. *)
let pp_ci_section ppf ~(badge : bool) (label : string)
    (style : Fmt.style list) (count : int) : unit =
  (* the padding belongs to a filled badge, which has a background to put
     it on; plain text would only gain a stray space either side *)
  let text = if badge then Printf.sprintf " %s " label else label in
  Fmt.pf ppf "%a %a@.@."
    (styles style Fmt.string)
    text
    Fmt.(styled `Faint string)
    (Printf.sprintf "· %s" (String_.unit_str count "finding"))

(* what 'opengrep ci' runs in, under a rule of its own *)
let pp_ci_environment (ctx : Skin.ctx) ppf (env : M.Start.ci_env) : unit =
  pp_section ctx ppf "Debugging info";
  Fmt.pf ppf "@.";
  Fmt.pf ppf "  versions     opengrep %a on OCaml %a@."
    Fmt.(styled `Bold string)
    env.version
    Fmt.(styled `Bold string)
    env.ocaml_version;
  Fmt.pf ppf "  environment  %a, triggering event is %a@."
    Fmt.(styled `Bold string)
    env.environment
    Fmt.(styled `Bold string)
    env.event_name

(* A --baseline-commit scan states its plan twice; the second is the
   replay, and says so. *)
let pp_plan (run : M.Plan.run) ppf (counts : (string * string) option) : unit
    =
  let nothing, scanning =
    match run with
    | M.Plan.Current -> ("Nothing to scan.", "Scanning")
    | M.Plan.Baseline -> ("Baseline: nothing to scan.", "Baseline: scanning")
  in
  match counts with
  | None -> Fmt.string ppf nothing
  | Some (files, rules) ->
      Fmt.pf ppf "%s %a with %a." scanning
        Fmt.(styled `Bold string)
        files
        Fmt.(styled `Bold string)
        rules

(* "in 0 files" says nothing; a clean scan just says so *)
let pp_tally ppf (t : M.Result.tally) : unit =
  if Int.equal t.findings 0 then
    Fmt.pf ppf "%a." Fmt.(styled `Bold string) "No findings"
  else
    Fmt.pf ppf "%a in %s, from %s."
      Fmt.(styled `Bold string)
      (String_.unit_str t.findings "finding")
      (String_.unit_str t.files_with_findings "file")
      (String_.unit_str t.rules_with_findings "rule")

let look : Skin_common.look =
  {
    finding;
    pp_file_header;
    pp_ci_heading =
      (fun ~(blocking : bool) ppf (count : int) ->
        if blocking then
          pp_ci_section ppf ~badge:true "BLOCKING"
            [ `Bg `Red; `Fg (`Hi `White); `Bold ]
            count
        else pp_ci_section ppf ~badge:false "non-blocking" [ `Faint ] count);
    pp_rules_fired_title =
      (fun (ctx : Skin.ctx) ppf (title : string) ->
        pp_section ctx ppf title;
        Fmt.pf ppf "@.");
    pp_ci_environment;
    pp_plan;
    sentences = true;
    pp_tally;
  }

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let doc = "A colourful report: a severity stripe, banded line numbers."
let on_start = Skin_common.on_start look
let rules_status = Skin_common.rules_status
let on_plan = Skin_common.on_plan look
let on_result = Skin_common.on_result look
let pp_findings = Skin_common.pp_findings look
let pp_matches = Skin_common.pp_by_file look
let wants_status_bar = true
