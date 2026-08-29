(* no exit, no argv
 * TODO: Cap.files_argv, Cap.domain, Cap.thread, Cap.alarm
 *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.random
  ; Cap.signal
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(*
   Parse the opengrep command line, run the requested subcommand, and return
   an exit status.

   If called as a standalone program, the 'exit' function should be called
   with this exit status. If testing, the exit status can be checked
   against expectations.

   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).
*)
val main : caps -> string array -> Exit_code.t

(* Insert --experimental into argv, after the subcommand when there is one.
   Used by Main.ml for the bare 'opengrep' binary. *)
val with_experimental_flag : string array -> string array
