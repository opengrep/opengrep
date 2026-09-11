(defn process-builtin-keep [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
