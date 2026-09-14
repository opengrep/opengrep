(ns test-rest-clojure-split.handler-rest-source-in-head)

(defn handler-rest-source-in-head [a & rest]
  ;; ok: test-rest-clojure
  (sink rest))
