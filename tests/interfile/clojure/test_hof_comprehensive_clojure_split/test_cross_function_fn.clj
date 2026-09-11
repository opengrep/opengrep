(defn test-cross-function-fn []
  ;; ruleid: test-hof-taint
  (let [r (fn [x] (sink x))]
    (r (source))))
