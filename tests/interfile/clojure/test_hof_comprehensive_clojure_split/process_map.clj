(ns test-hof-comprehensive-clojure-split.process-map)

(defn process-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
