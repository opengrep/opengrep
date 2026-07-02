(defn test-builtin-filterv-fn []
  (filterv (fn [x]
             ;; ruleid: test-hof-taint
             (sink x)
             true)
           (source)))
