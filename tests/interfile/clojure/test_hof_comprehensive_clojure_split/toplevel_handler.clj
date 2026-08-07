(defn toplevel-handler [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
