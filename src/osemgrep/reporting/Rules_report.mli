val pp_rules :
  too_many_entries:int ->
  Format.formatter ->
  Rules_source.t * Rule.t list ->
  unit

(* One warning for the interfile rules that run per file because
   --enable-interfile is not given, and a hint printed by [pp_hint];
   nothing for no rule. *)
val warn_rules_run_per_file : pp_hint:string Fmt.t -> Rule_ID.t list -> unit
