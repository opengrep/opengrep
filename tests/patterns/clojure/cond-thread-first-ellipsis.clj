;; ERROR: match
(cond-> x
    true func1
    false func2
    true sink)
