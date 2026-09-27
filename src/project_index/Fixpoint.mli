(* Apply [step] until it reports no change; returns the last state and the
   number of steps that reported a change. *)

val run :
  step : ('s -> 's * bool) ->
  's -> 's * int
