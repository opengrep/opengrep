(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The legacy report: the text output of pysemgrep, byte for byte, which
 * the end-to-end tests check. Each chunk below is one line of it, in order.
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

(* one chunk per line of the report, at the Logs.App level *)
let app (f : Format.formatter -> unit) : Skin.chunk =
  Skin.Line (Logs.App, f)

let app_str (s : string) : Skin.chunk = app (fun ppf -> Fmt.pf ppf "%s" s)

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

let doc = "The report layout of earlier opengrep releases."

(* 'opengrep ci' prints its environment with or without a banner, so this
   is outside the banner test below. *)
let ci_environment (env : M.Start.ci_env) : Skin.chunk list =
  [
    app (fun ppf -> Fmt_.pp_heading ppf "Debugging Info");
    app (fun ppf ->
        Fmt.pf ppf "  %a" Fmt.(styled `Underline string) "SCAN ENVIRONMENT");
    app (fun ppf ->
        Fmt.pf ppf "  versions    - opengrep %a on OCaml %a"
          Fmt.(styled `Bold string)
          env.version
          Fmt.(styled `Bold string)
          env.ocaml_version);
    app (fun ppf ->
        Fmt.pf ppf
          "  environment - running in environment %a, triggering event is %a@."
          Fmt.(styled `Bold string)
          env.environment
          Fmt.(styled `Bold string)
          env.event_name);
  ]

let on_start (_ctx : Skin.ctx) (start : M.Start.t) : Skin.chunk list =
  (match start.ci with
  | Some env -> ci_environment env
  | None -> [])
  @
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
    app_str logo :: features

(* The spinner draws on this line and erases it once the rules are
   loaded. *)
let rules_status (_ctx : Skin.ctx) (start : M.Start.t) : string option =
  if not start.banner then None
  else
    Some
      (match start.rule_source with
      | M.Start.Registry -> styled `Bold "  Loading rules from registry..."
      | Git -> styled `Bold "  Loading rules from git repository..."
      | Local -> styled `Bold "  Loading rules from local config..."
      | Pattern -> "  Using custom pattern.")

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
    ~max_lines_per_finding:ctx.max_lines_per_finding
    ~show_dataflow_traces:ctx.show_dataflow_traces
    ~interfile_dedup_by:ctx.interfile_dedup_by ~is_interfile:ctx.is_interfile
    ~is_ci_invocation:ctx.is_ci_invocation ppf cli_output

let pp_matches (ctx : Skin.ctx) ppf
    (matches : Semgrep_output_v1_t.cli_match list) : unit =
  Matches_report.pp_text_outputs ~max_chars_per_line:ctx.max_chars_per_line
    ~max_lines_per_finding:ctx.max_lines_per_finding
    ~show_dataflow_traces:ctx.show_dataflow_traces
    ~interfile_dedup_by:ctx.interfile_dedup_by ~is_interfile:ctx.is_interfile
    ppf matches

(* the earlier report had no status line *)
let shows_status_bar = false
