(* UTF-8 strings. A maximal malformed subpart counts as one character, as
   String.get_utf_8_uchar reports it. *)

val is_valid : string -> bool

(* the number of characters *)
val length : string -> int

(* The byte offset of every character of the string, with its length as a
   last element, so that the characters [i, j) are the bytes
   [offsets.(i), offsets.(j)). *)
val code_point_offsets : string -> int array

(* Replace every malformed subpart by U+FFFD, the replacement character,
   and return the string unchanged when it is valid UTF-8. This is what
   Python does when a file is read with errors="replace". *)
val sanitize : string -> string
