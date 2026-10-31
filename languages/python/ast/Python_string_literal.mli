val is_raw_prefix : string -> bool
val is_format_prefix : string -> bool

(* The value of a string literal with [prefix] whose source text between the
   quotes is [s], as the Python language reference defines it. [pattern] holds
   when the literal is part of a pattern. *)
val value : python2:bool -> pattern:bool -> prefix:string -> string -> string
