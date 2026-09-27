let run (type s) ~(step : s -> s * bool) (init : s) : s * int =
  let rec loop (state : s) (changes : int) : s * int =
    match step state with
    | state', true -> loop state' (changes + 1)
    | state', false -> (state', changes)
  in
  loop init 0
