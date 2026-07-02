(defn process-builtin-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
