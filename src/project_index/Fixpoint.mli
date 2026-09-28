(* Apply [step] until it reports no change or [max_steps] steps have reported
   a change; returns the last state and the number of steps that reported a
   change, which equals [max_steps] only when the cap stopped the loop. *)

val run :
  max_steps : int ->
  step : ('s -> 's * bool) ->
  's -> 's * int
