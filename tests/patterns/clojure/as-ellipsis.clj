(defn f [y]
  ;; ERROR: match
  (as-> y x
    (func1 x)
    (func2 x)
    (func3 x x)
    (sink x)))
