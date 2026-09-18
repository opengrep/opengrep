(ns test-hof-comprehensive-clojure-split.test-builtin-some-fn)

(defn test-builtin-some-fn []
  (some (fn [x]
          ;; ruleid: test-hof-taint
          (sink x)
          true)
        (source)))
