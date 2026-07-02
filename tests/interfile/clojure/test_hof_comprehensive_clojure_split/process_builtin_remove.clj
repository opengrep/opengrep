(defn process-builtin-remove [x]
  ;; ruleid: test-hof-taint
  (sink x)
  false)
