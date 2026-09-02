(* The text report of the rules that could not be loaded: one block per
   error with the location, an excerpt of the rule file around the token
   the error is about, and the message. *)
val pp_errors : Format.formatter -> Core_error.t list -> unit

(* "invalid configuration file found (N configs were invalid)" followed by
   the report, the text of the fatal error in text mode. *)
val invalid_configs_message : Core_error.t list -> string
