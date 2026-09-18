(ns test-rest-clojure-split.handler-rest2-source-in-b)

(defn handler-rest2-source-in-b [a b & rest]
  ;; ok: test-rest-clojure
  (sink rest))
