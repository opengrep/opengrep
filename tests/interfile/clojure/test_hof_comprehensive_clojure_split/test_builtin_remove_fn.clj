(defn test-builtin-remove-fn []
  (remove (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            false)
          (source)))
