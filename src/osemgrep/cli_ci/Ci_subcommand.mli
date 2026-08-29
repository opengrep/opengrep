(*
   Parse an opengrep-ci command, execute it and exit.

   Usage: main caps [| "opengrep-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)

type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Ci_CLI.conf -> Exit_code.t
