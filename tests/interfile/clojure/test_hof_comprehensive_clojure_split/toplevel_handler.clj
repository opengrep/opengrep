(ns test-hof-comprehensive-clojure-split.toplevel-handler)

(defn toplevel-handler [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)
