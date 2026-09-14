(ns test-hof-comprehensive-clojure-split.process-builtin-keep)

(defn process-builtin-keep [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
