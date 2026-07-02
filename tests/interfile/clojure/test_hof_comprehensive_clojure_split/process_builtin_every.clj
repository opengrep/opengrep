(defn process-builtin-every [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
