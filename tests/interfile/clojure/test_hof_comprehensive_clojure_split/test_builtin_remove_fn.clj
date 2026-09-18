(ns test-hof-comprehensive-clojure-split.test-builtin-remove-fn)

(defn test-builtin-remove-fn []
  (remove (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            false)
          (source)))
