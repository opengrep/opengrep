(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* What a skin is: the module that decides what a scan's report looks like.
 *
 * A skin owns the structure of the report, not just its glyphs and colours,
 * so the driver cannot call it back for a fixed list of sections in a fixed
 * order. Instead the driver hands it data at the points of the scan where
 * something is known, and the skin answers with the pieces it wants drawn,
 * in the order it wants them. An empty list draws nothing, so a skin is free
 * to drop a section, merge two, or keep everything until the end.
 *
 * The pieces are documents, not effects: the driver decides which stream
 * they reach and renders them. That keeps a skin a pure function of the
 * data, which is also what lets the file destinations reuse it. A skin that
 * genuinely needs to repaint the terminal while the scan runs implements
 * LIVE as well.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Where a piece of the report goes.
 *
 * Stdout is the report proper: it survives --quiet, as the findings must.
 * Stderr carries the chrome around it, through Logs, so that --quiet and
 * --verbose keep deciding what is shown; Logs.App is the level that prints
 * without a "[LEVEL]" prefix.
 *)
type dest =
  | Stdout
  | Stderr of Logs.level

type chunk =
  | Line of dest * (Format.formatter -> unit)
  (* Where the findings belong. The driver renders them at this point, in
     whichever format was asked for, together with the diagnostics that
     accompany them. A skin that leaves it out reports no findings at all. *)
  | Findings

(* What a skin needs to know about the terminal it draws on. *)
type ctx = {
  (* whether ANSI styling reaches the reader; follows --force-color,
   * $NO_COLOR and the tty, like every other output *)
  color : bool;
  is_tty : bool;
  (* --verbose or --debug *)
  verbose : bool;
  (* the columns the report draws within, already clamped *)
  width : int;
  (* --max-chars-per-line and --max-lines-per-finding *)
  max_chars_per_line : int;
  max_lines_per_finding : int;
  show_dataflow_traces : bool;
  (* 'opengrep ci' keeps blocking and non-blocking findings in separate
   * groups and appends the "RULES FIRED" sections *)
  is_ci_invocation : bool;
}

(*****************************************************************************)
(* The interface *)
(*****************************************************************************)

(* Only for a skin that repaints while the scan runs; ordinary skins answer
 * None and stay pure. *)
module type LIVE = sig
  type t

  val start : ctx -> t
  val stop : t -> unit
end

module type S = sig
  val name : string
  val doc : string

  (* before the rules are fetched: the banner, and what the rules come from *)
  val on_start : ctx -> Skin_model.Start.t -> chunk list

  (* once targeting and rule loading have paired files with rules *)
  val on_plan : ctx -> Skin_model.Plan.t -> chunk list

  (* the end of the scan: where the findings go, and what follows them *)
  val on_result : ctx -> Skin_model.Result.t -> chunk list

  (* The findings themselves, in the text format. Called where the skin put
     Skin.Findings, and again for a -o/--text-output file. *)
  val pp_findings : ctx -> Semgrep_output_v1_t.cli_output Fmt.t

  (* The matches of one file, printed while the scan runs, for
     --incremental-output. *)
  val pp_matches : ctx -> Semgrep_output_v1_t.cli_match list Fmt.t

  val live : (module LIVE) option
end

(*****************************************************************************)
(* Naming *)
(*****************************************************************************)

(* A plain variant rather than a first-class module, so that it can sit in
 * the conf records, which derive show. Skins.resolve turns it into the
 * module. *)
type name =
  | Legacy
  | Simple
  | Vivid
[@@deriving show]

(* what --skin accepts *)
let all_names : (string * name) list =
  [ ("legacy", Legacy); ("simple", Simple); ("vivid", Vivid) ]
