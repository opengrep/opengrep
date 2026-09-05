(* See also Core_error.ml and semgrep_output_v1.atd error_type *)

exception Semgrep_error of string * Exit_code.t option

(* Please avoid the name 'Exit' since it's already a standard exception. *)
exception Exit_code of Exit_code.t

(* [is_broken_pipe exn] holds when a reader closed the pipe opengrep writes
   to; that is a normal end of output, to be reported by no one. Use
   [drop_buffered_stdout] before exiting so that the flush Stdlib does from
   at_exit does not raise the same error again. *)
val is_broken_pipe : exn -> bool
val drop_buffered_stdout : unit -> unit

(* shortcut *)
val abort : string -> 'a
val exit_code_exn : Exit_code.t -> 'a

(* used for CLI text output and for the metrics payload.errors.errors *)
val string_of_error_type : Semgrep_output_v1_t.error_type -> string
