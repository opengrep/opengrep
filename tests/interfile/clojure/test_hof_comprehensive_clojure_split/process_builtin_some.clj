(defn process-builtin-some [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
