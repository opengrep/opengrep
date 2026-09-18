(ns test-hof-comprehensive-clojure-split.process-direct)

(defn process-direct [x]
  ;; ruleid: test-hof-taint
  (sink x))
