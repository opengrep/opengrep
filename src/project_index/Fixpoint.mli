(* Bounded-iteration fixpoint: apply [step] until [equal s_n s_{n+1}] or
   [max_iterations]; returns final state and iterations run. [equal] must be
   value-aware — [Type_state] setters are last-wins, so a key-only [equal] would
   stop early when a binding's value changes without growing the map. *)

val run :
  equal : ('s -> 's -> bool) ->
  step : ('s -> 's) ->
  max_iterations : int ->
  's -> 's * int
