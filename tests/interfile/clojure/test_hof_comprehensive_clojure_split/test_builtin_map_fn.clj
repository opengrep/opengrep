(ns test-hof-comprehensive-clojure-split.test-builtin-map-fn)

(defn test-builtin-map-fn []
  (map (fn [x]
         ;; ruleid: test-hof-taint
         (sink x)
         x)
       (source)))
