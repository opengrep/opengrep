(*
   Launch the specified computation and abort if it takes longer than the
   time limit specified (in seconds).

   The [granularity_float_s] parameter is optional and specifies the amount
   of time to sleep before checking if the memprof-limits timeout token should
   be set, otherwise the timeout thread will make the domain wait and the scan
   result will be delayed. 
*)
val set_timeout :
  < Cap.time_limit > ->
  ?granularity_float_s:float ->
  name:string ->
  float ->
  (unit -> 'a) ->
  'a option

(*
   Only set a timer if a time limit is specified. Uses 'set_timeout'.
*)
val set_timeout_opt :
  ?granularity_float_s:float ->
  name:string ->
  (float * < Cap.time_limit >) option ->
  (unit -> 'a) ->
  'a option

(*
   A time limit shared by several computations, possibly on different
   domains: the length of the union of their running intervals, the time
   during which at least one of them runs.
*)
type shared_limit

(* A shared limit of the given number of seconds. *)
val create_shared_limit : float -> shared_limit

(*
   Runs the computation under 'set_timeout' of the time the shared limit has
   left when it starts. Returns None when that time runs out, and does not run
   the computation when none is left.
*)
val with_shared_limit :
  < Cap.time_limit > ->
  name:string ->
  shared_limit ->
  (unit -> 'a) ->
  'a option
