(* Contains the name given by the user to the timer and the time limit *)
type timeout_info

(*
   If ever caught, this exception must be re-raised immediately so as
   to not interfere with the timeout handler. See function 'set_timeout'.
*)
exception Timeout of timeout_info

(* Show name and time limit in a compact format for debugging purposes. *)
val string_of_timeout_info : timeout_info -> string

(*
   Launch the specified computation and abort if it takes longer than
   specified (in seconds).

   This uses a local timer. An Invalid_argument exception will be raised
   if the timer is already running.

   The [granularity_float_s] parameter is optional and specifies the amount
   of time to sleep before checking if the memprof-limits token is set,
   otherwise the timeout thread will make the domain wait and the scan result
   will be delayed. 
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
