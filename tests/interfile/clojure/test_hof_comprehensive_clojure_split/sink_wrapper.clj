(ns test-hof-comprehensive-clojure-split.sink-wrapper)

(defn sink-wrapper [x]
  ;; ruleid: test-hof-taint
  (sink x))
