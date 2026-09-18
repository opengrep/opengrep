(ns test-hof-comprehensive-clojure-split.process-builtin-remove)

(defn process-builtin-remove [x]
  ;; ruleid: test-hof-taint
  (sink x)
  false)
