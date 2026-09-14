(ns test-hof-comprehensive-clojure-split.process-filter)

(defn process-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
