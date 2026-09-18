(ns test-hof-comprehensive-clojure-split.test-cross-function-fn)

(defn test-cross-function-fn []
  ;; ruleid: test-hof-taint
  (let [r (fn [x] (sink x))]
    (r (source))))
