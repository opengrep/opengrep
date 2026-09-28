let run (type s) ~(max_steps : int) ~(step : s -> s * bool) (init : s)
    : s * int =
  let rec loop (state : s) (changes : int) : s * int =
    if changes >= max_steps then (state, changes)
    else
      match step state with
      | state', true -> loop state' (changes + 1)
      | state', false -> (state', changes)
  in
  loop init 0
