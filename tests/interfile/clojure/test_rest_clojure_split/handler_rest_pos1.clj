(ns test-rest-clojure-split.handler-rest-pos1)

(defn handler-rest-pos1 [a & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))
