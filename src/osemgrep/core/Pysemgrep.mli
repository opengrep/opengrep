(* This function never returns! It calls instead pysemgrep *)
(* TODO: This could be renamed to pyopengrep. *)
val pysemgrep : < Cap.exec > -> string array -> 'a

(* To be used to signal we want to fallback to pysemgrep. The exception
 * must still be handled in the caller which then must call explicitely
 * pysemgrep().
 *)
exception Fallback
