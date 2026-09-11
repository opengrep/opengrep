(* Semgrep text output. For the JSON output see Cli_json_output.ml *)

(* is_ci_invocation keeps the blocking and non-blocking findings in
 * separate groups and appends the "RULES FIRED" sections, like the
 * pyopengrep text formatter does for 'opengrep ci' *)
val pp_cli_output :
  max_chars_per_line:int ->
  max_lines_per_finding:int ->
  color_output:'a ->
  show_dataflow_traces:bool ->
  ?is_ci_invocation:bool ->
  Format.formatter ->
  Semgrep_output_v1_t.cli_output ->
  unit

(* internals, used also for incremental display of matches *)
val pp_text_outputs :
  max_chars_per_line:int ->
  max_lines_per_finding:int ->
  color_output:'a ->
  show_dataflow_traces:bool ->
  Format.formatter ->
  Semgrep_output_v1_t.cli_match list ->
  unit
