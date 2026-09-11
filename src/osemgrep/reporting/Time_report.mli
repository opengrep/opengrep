(* The --time summary of the text output, from the "time" field of the
 * output and its errors. *)
val pp_time_summary :
  Format.formatter ->
  Semgrep_output_v1_t.profile ->
  Semgrep_output_v1_t.cli_error list ->
  unit
