(ns test-hof-comprehensive-clojure-split.variadic-rest-sink)

(defn variadic-rest-sink [x & rest]
  ;; ruleid: test-hof-taint
  (sink rest))
