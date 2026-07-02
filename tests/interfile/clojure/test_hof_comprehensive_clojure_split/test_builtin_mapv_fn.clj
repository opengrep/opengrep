(defn test-builtin-mapv-fn []
  (mapv (fn [x]
          ;; ruleid: test-hof-taint
          (sink x)
          x)
        (source)))
