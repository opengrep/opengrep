(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The interface of a skin: the module that renders the text report of a
 * scan.
 *
 * A skin determines the structure of the report, not only its glyphs and
 * colours, so the driver does not call it for a fixed list of sections in a
 * fixed order. The driver passes it data at the points of the scan where
 * the data is available, and the skin returns the chunks to write, in its
 * own order. An empty list writes nothing, so a skin can drop a section,
 * merge two, or defer everything to the end.
 *
 * The chunks are printers that the driver runs. A skin is therefore a pure
 * function of the data, which also lets the file destinations reuse it.
 * Redrawing the terminal while the scan runs is the job of Status_bar,
 * which a skin enables with [shows_status_bar].
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type chunk =
  (* Part of the report header or footer, written to stderr through Logs at
     this level, so that --quiet and --verbose still control what is shown;
     Logs.App prints without a "[LEVEL]" prefix.

     The printer runs inside the log message, with the log mutex held, so it
     must not log or call anything that logs. *)
  | Line of Logs.level * (Format.formatter -> unit)
  (* The position of the findings. The driver renders them here, in the
     requested format, with their diagnostics; a skin without it reports no
     findings. *)
  | Findings

(* The terminal and report settings that a skin renders with. *)
type ctx = {
  (* the width of the report in columns, already clamped *)
  width : int;
  (* --max-chars-per-line and --max-lines-per-finding *)
  max_chars_per_line : int;
  max_lines_per_finding : int;
  show_dataflow_traces : bool;
  (* 'opengrep ci' keeps blocking and non-blocking findings in separate
   * groups and appends the "RULES FIRED" sections *)
  is_ci_invocation : bool;
  (* how findings that share a sink are grouped, which determines whether a
     finding stands alone or heads a group that lists its sources *)
  interfile_dedup_by : Core_match.interfile_dedup_by;
  (* whether a rule runs across files; only then are the sources of a shared
     sink listed *)
  is_interfile : Rule_ID.t -> bool;
}

(*****************************************************************************)
(* The interface *)
(*****************************************************************************)

module type S = sig
  (* the description of this skin in the --skin help, which Skin_CLI builds
     from these *)
  val doc : string

  (* before the rules are fetched: the banner and the rule source *)
  val on_start : ctx -> Skin_model.Start.t -> chunk list

  (* The line shown while the rules are fetched, which the spinner, where it
     runs, animates and erases when the fetch ends. None for a skin without
     such a line, and then there is no spinner; None also off a terminal,
     where nothing would erase it. *)
  val rules_status : ctx -> Skin_model.Start.t -> string option

  (* Called once targeting and rule loading have paired targets with rules.
     Building the plan walks every job, so it is not built, and this is not
     called, while logging is off (--quiet). *)
  val on_plan : ctx -> Skin_model.Plan.t -> chunk list

  (* At the end of the scan: the position of the findings and what follows
     them. Building the summary calls stat on every ignored path, so it is
     Summary.empty while logging is off. *)
  val on_result : ctx -> Skin_model.Result.t -> chunk list

  (* The findings in the text format, written at Skin.Findings and again
     for a -o/--text-output file. *)
  val pp_findings : ctx -> Semgrep_output_v1_t.cli_output Fmt.t

  (* The matches of one file, printed while the scan runs, for
     --incremental-output. *)
  val pp_matches : ctx -> Semgrep_output_v1_t.cli_match list Fmt.t

  val shows_status_bar : bool
end

(*****************************************************************************)
(* The values of --skin *)
(*****************************************************************************)

(* A variant rather than a first-class module, so that the conf records that
 * carry it can derive show. Skins.resolve maps it to the module. *)
type name =
  | Legacy
  | Simple
  | Vivid
[@@deriving show]

let default : name = Simple

let all_names : (string * name) list =
  [ ("legacy", Legacy); ("simple", Simple); ("vivid", Vivid) ]
