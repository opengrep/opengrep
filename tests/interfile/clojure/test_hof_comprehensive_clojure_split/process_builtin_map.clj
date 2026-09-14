(ns test-hof-comprehensive-clojure-split.process-builtin-map)

(defn process-builtin-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
