(defn test-custom-filter-fn []
  (custom-filter (source) (fn [x]
                            ;; ruleid: test-hof-taint
                            (sink x)
                            true)))
