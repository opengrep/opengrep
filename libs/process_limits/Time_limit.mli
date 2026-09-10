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
