(defn process-builtin-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
