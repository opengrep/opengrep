(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The report opengrep has always printed.
 *
 * This is the default skin, and the one the end-to-end tests pin: every
 * chunk below reproduces one of the print calls the driver used to make,
 * in the order it made them, so that its output stays byte for byte what
 * it was.
 *)

module M = Skin_model

(*****************************************************************************)
(* Styling *)
(*****************************************************************************)

(* These strings go to stderr through Logs.app, so they are styled with the
   renderer of stderr, which follows --force-color, $NO_COLOR and the tty
   like every other output (see CLI_common.setup_logging). *)
let styled (style : Fmt.style) (text : string) : string =
  Fmt.str_like Fmt.stderr "%a" Fmt.(styled style string) text

let feature_status ~(enabled : bool) : string =
  if enabled then styled (`Fg `Green) "✔" else styled (`Fg `Red) "✘"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let logo =
  {|
┌──────────────┐
│ Opengrep CLI │
└──────────────┘
|}

(* one chunk per line the driver used to hand to Logs.app *)
let app (f : Format.formatter -> unit) : Skin.chunk =
  Skin.Line (Skin.Stderr Logs.App, f)

let app_str (s : string) : Skin.chunk = app (fun ppf -> Fmt.pf ppf "%s" s)

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let name = "legacy"
let doc = "The report opengrep has always printed."

let on_start (_ctx : Skin.ctx) (start : M.Start.t) : Skin.chunk list =
  if not start.banner then []
  else
    let features =
      match start.rule_source with
      (* python: the pattern mode is one of the alternate modes, which had
         no feature section of their own *)
      | M.Start.Pattern -> [ app_str (styled `Bold "  Code scanning.\n") ]
      | Registry
      | Git
      | Local ->
          start.features
          |> List.concat_map (fun (f : M.Start.feature) ->
                 [
                   app (fun ppf ->
                       Fmt.pf ppf "%s %s"
                         (feature_status ~enabled:f.enabled)
                         (styled `Bold f.name));
                   app (fun ppf ->
                       Fmt.pf ppf "  %s %s\n"
                         (feature_status ~enabled:f.enabled)
                         f.description);
                 ])
    in
    let source =
      match start.rule_source with
      | M.Start.Registry -> styled `Bold "  Loading rules from registry..."
      | Git -> styled `Bold "  Loading rules from git repository..."
      | Local -> styled `Bold "  Loading rules from local config..."
      | Pattern -> "  Using custom pattern."
    in
    (app_str logo :: features) @ [ app_str source ]

let on_plan (_ctx : Skin.ctx) (plan : M.Plan.t) : Skin.chunk list =
  [ app (fun ppf -> Status_report.pp_status ppf plan) ]

let on_result (_ctx : Skin.ctx) (result : M.Result.t) : Skin.chunk list =
  let tally =
    match result.tally with
    | None -> []
    | Some (t : M.Result.tally) ->
        [
          app (fun ppf ->
              Fmt.pf ppf "Ran %s on %s: %s."
                (String_.unit_str t.rules_ran "rule")
                (String_.unit_str t.files_scanned "file")
                (String_.unit_str t.findings "finding"));
        ]
  in
  (Skin.Findings
  :: [ app (fun ppf -> Summary_report.pp_summary ppf result.summary) ])
  @ tally

let pp_findings (ctx : Skin.ctx) ppf (cli_output : Semgrep_output_v1_t.cli_output)
    : unit =
  Matches_report.pp_cli_output ~max_chars_per_line:ctx.max_chars_per_line
    ~max_lines_per_finding:ctx.max_lines_per_finding ~color_output:ctx.color
    ~show_dataflow_traces:ctx.show_dataflow_traces
    ~is_ci_invocation:ctx.is_ci_invocation ppf cli_output

let pp_matches (ctx : Skin.ctx) ppf
    (matches : Semgrep_output_v1_t.cli_match list) : unit =
  Matches_report.pp_text_outputs ~max_chars_per_line:ctx.max_chars_per_line
    ~max_lines_per_finding:ctx.max_lines_per_finding ~color_output:ctx.color
    ~show_dataflow_traces:ctx.show_dataflow_traces ppf matches

let live = None
