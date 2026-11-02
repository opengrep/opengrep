open Common

(* A string literal "=~/regexp/flags" in a pattern denotes a regular
   expression. *)

(* TODO: deprecate this *)
let regexp_regexp_string = "^=~/\\(.*\\)/\\([mi]?\\)$"

(* TODO: deprecate this *)
let is_regexp_string (s : string) : bool = s =~ regexp_regexp_string
