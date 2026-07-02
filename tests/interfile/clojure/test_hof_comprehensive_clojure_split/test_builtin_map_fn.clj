(defn test-builtin-map-fn []
  (map (fn [x]
         ;; ruleid: test-hof-taint
         (sink x)
         x)
       (source)))
