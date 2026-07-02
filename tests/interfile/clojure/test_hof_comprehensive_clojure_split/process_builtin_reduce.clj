(defn process-builtin-reduce [acc x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
