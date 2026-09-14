(ns test-hof-comprehensive-clojure-split.test-builtin-filter-fn)

(defn test-builtin-filter-fn []
  (filter (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            true)
          (source)))
