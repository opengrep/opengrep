(ns test-hof-comprehensive-clojure-split.process-builtin-some)

(defn process-builtin-some [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)
