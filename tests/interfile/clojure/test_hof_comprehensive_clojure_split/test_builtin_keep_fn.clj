(defn test-builtin-keep-fn []
  (keep (fn [x]
          ;; ruleid: test-hof-taint
          (sink x)
          x)
        (source)))
