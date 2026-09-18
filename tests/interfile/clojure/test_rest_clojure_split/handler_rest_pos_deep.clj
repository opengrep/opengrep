(ns test-rest-clojure-split.handler-rest-pos-deep)

(defn handler-rest-pos-deep [a & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))
