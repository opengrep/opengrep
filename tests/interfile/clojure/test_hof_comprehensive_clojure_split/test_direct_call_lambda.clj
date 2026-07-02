(defn test-direct-call-lambda []
  ;; ruleid: test-hof-taint
  (direct-call (fn [x] (sink x)) (source)))
