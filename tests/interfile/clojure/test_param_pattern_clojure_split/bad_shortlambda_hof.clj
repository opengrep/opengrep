(defn bad-shortlambda-hof []
  ;; ruleid: test-param-pattern-taint
  (run-cb #(sink %) (source)))
