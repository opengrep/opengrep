(ns test-hof-comprehensive-clojure-split.process-builtin-reduce)

(defn process-builtin-reduce [acc x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
