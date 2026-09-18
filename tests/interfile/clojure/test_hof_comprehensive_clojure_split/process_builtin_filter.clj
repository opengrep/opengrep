(ns test-hof-comprehensive-clojure-split.process-builtin-filter)

(defn process-builtin-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
