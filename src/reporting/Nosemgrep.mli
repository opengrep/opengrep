val rule_id_re_str : string
val get_nosem_inline_re : ?config:Engine_config.t -> unit -> Pcre2_.t
val get_nosem_previous_line_re : ?config:Engine_config.t -> unit -> Pcre2_.t

(* produce the `is_ignored` fields for the processed match, without filtering
   them out
*)
val produce_ignored :
  ?config:Engine_config.t ->
  Core_result.processed_match list ->
  Core_result.processed_match list * Core_error.t list

(* remove the matches in that were whitelisted by a 'nosemgrep:' comment in
   the code by the user.
   requires the ignores to have been "produced" via [produce_ignored] above first!
*)
val filter_ignored :
  keep_ignored:bool ->
  Semgrep_output_v1_t.core_match list ->
  Semgrep_output_v1_t.core_match list
