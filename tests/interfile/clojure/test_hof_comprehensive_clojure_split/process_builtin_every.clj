(ns test-hof-comprehensive-clojure-split.process-builtin-every)

(defn process-builtin-every [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
