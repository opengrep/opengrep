(* You should prefer to use the safer CapExec.ml module. This UCmd module
 * is for Unsafe use of Cmd (hence the name). see TCB/Cap.mli for more info.
 *)

(*
   The following functions capture the error output of the command being run
   and logs it as the info level, allowing it to be silenced by adjusting
   the log level.
*)
val string_of_run :
  trim:bool -> Cmd.t -> (string * Cmd.run_status, [> Rresult.R.msg ]) result

(* The building blocks of string_of_run, for callers that must run the
   command themselves: log the command at info level (redacted), and run a
   function while capturing the process error output, logging it at info
   level (redacted). *)
val log_command : Cmd.t -> unit
val capture_and_log_stderr : (unit -> 'a) -> 'a

(* The command with its program looked up only in the absolute entries of
   PATH, as a regular file that can be executed; with PATH unset, no program
   is found. *)
val resolve_program : Cmd.t -> (Bos.Cmd.t, [> Rresult.R.msg ]) result

(* Removes the entries of PATH that are not absolute paths from the process
   environment, so that the programs started afterwards, and the programs
   they start, search only those; on Windows, also sets
   NoDefaultCurrentDirectoryInExePath, so that cmd.exe and the system do not
   search the current directory first. Called once at start-up. *)
val restrict_program_search : unit -> unit

val string_of_run_with_stderr :
  trim:bool ->
  Cmd.t ->
  (string * Cmd.run_status, [> Rresult.R.msg ]) result * string
(** Like string_of_run but instead of logging the stderr output, it captures it and returns it (in both success and failure cases). *
 * The first part of the return type matches the return value of `string_of_run`; the last string part contains the stderr contents *)

val lines_of_run :
  trim:bool ->
  Cmd.t ->
  (string list * Cmd.run_status, [> Rresult.R.msg ]) result

val status_of_run :
  ?quiet:bool -> Cmd.t -> (Bos.OS.Cmd.status, [> Rresult.R.msg ]) result
