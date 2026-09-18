(ns test-hof-comprehensive-clojure-split.test-builtin-every-fn)

(defn test-builtin-every-fn []
  (every? (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            true)
          (source)))
