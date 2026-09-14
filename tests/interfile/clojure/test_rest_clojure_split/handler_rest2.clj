(ns test-rest-clojure-split.handler-rest2)

(defn handler-rest2 [a b & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))
