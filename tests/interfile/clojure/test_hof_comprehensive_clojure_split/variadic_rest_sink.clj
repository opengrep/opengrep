(defn variadic-rest-sink [x & rest]
  ;; ruleid: test-hof-taint
  (sink rest))
