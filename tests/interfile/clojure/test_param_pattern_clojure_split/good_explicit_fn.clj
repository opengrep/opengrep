(defn good-explicit-fn []
  ;; ruleid: test-param-pattern-taint
  (run-cb (fn [v] (sink v)) (source)))
