(ns test-rest-clojure-split.handler-rest2-deep)

(defn handler-rest2-deep [a b & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))
