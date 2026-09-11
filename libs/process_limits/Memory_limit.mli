(*
   Avoid segfaults when the process runs out of memory.
*)

exception ExceededMemoryLimit of string

val run_with_global_memory_limit :
  < Cap.memory_limit > ->
  ?get_context:(unit -> string) ->
  mem_limit_mb:int ->
  (unit -> 'a) ->
  'a
