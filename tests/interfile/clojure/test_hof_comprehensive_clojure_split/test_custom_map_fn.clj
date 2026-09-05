(defn test-custom-map-fn []
  (custom-map-builtin (source) (fn [x]
                                 ;; ruleid: test-hof-taint
                                 (sink x)
                                 x)))
