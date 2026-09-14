(ns test-hof-comprehensive-clojure-split.test-cross-function-let)

(defn test-cross-function-let []
  (let [tainted (source)]
    ;; ruleid: test-hof-taint
    (sink tainted)))
