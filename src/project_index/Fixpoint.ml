let run (type s)
    ~(equal : s -> s -> bool)
    ~(step : s -> s)
    ~(max_iterations : int)
    (init : s) : s * int =
  let rec loop state i =
    if i >= max_iterations then (state, i)
    else
      let state' = step state in
      if equal state state' then (state, i)
      else loop state' (i + 1)
  in
  loop init 0
