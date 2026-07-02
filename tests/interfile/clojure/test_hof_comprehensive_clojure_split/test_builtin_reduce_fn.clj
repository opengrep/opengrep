(defn test-builtin-reduce-fn []
  (reduce (fn [acc x]
            ;; ruleid: test-hof-taint
            (sink x)
            x)
          nil
          (source)))
