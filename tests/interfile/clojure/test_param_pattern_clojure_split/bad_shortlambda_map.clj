(defn bad-shortlambda-map []
  ;; ruleid: test-param-pattern-taint
  (map #(sink %) [(source)]))
