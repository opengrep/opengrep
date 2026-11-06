(*
   Utilities for working with the types defined in Semgrep_output_v1.atd
*)

(* internal function used for the 'lines:' field in the JSON output
 * but also now in Output.ml for the Emacs output.
 * This may raise Common.ErrorOnFile "out of bound access".
 *)
val lines_of_file_at_range_exn :
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position ->
  Fpath.t ->
  string list

(* used to interpolate metavars in the 'message:' field and
 * for the dataflow call traces.
 *)
val content_of_file_at_range :
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position ->
  Fpath.t ->
  string

val position_of_token_location : Tok.location -> Semgrep_output_v1_t.position

val position_range :
  Tok.location ->
  Tok.location ->
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position

val location_of_token_location : Tok.location -> Semgrep_output_v1_t.location
val tokens_to_single_loc : Tok.t list -> Semgrep_output_v1_t.location option

(* Whether the step is the source or a call to a function in which the
   source is. *)
val leads_to_source : Semgrep_output_v1_t.match_dataflow_step -> bool

(* Whether the step is the sink or a call to a function in which the sink
   is. *)
val leads_to_sink : Semgrep_output_v1_t.match_dataflow_step -> bool

(* The step of the trace that leads to the source, the steps after it, and
   the step that leads to the sink, if the trace has both. *)
val split_dataflow_trace :
  Semgrep_output_v1_t.match_dataflow_trace ->
  (Semgrep_output_v1_t.match_dataflow_step
  * Semgrep_output_v1_t.match_dataflow_trace
  * Semgrep_output_v1_t.match_dataflow_step)
  option

(* The steps and the steps under them, each step before the steps under it,
   as a flat report prints them. *)
val flat_dataflow_steps :
  Semgrep_output_v1_t.match_dataflow_trace ->
  Semgrep_output_v1_t.match_dataflow_step list

(* The source of the trace, at any depth. *)
val dataflow_source :
  Semgrep_output_v1_t.match_dataflow_trace ->
  Semgrep_output_v1_t.match_dataflow_step option

(*
   Sort results in the most natural way possible, typically preferring
   match location first.
*)
val sort_core_matches :
  Semgrep_output_v1_t.core_match list -> Semgrep_output_v1_t.core_match list

val compare_cli_matches :
  Semgrep_output_v1_t.cli_match ->
  Semgrep_output_v1_t.cli_match ->
  int

(* Sort matches in an order suitable for displaying results. *)
val sort_cli_matches :
  Semgrep_output_v1_t.cli_match list -> Semgrep_output_v1_t.cli_match list

(* The order of sort_cli_matches, for core matches: the order in which the
   findings are reported, which is also the order in which their autofixes
   are applied when they overlap. *)
val sort_core_matches_as_reported :
  Semgrep_output_v1_t.core_match list -> Semgrep_output_v1_t.core_match list
