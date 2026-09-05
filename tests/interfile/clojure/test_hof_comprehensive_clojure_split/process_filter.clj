(defn process-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
