(ns test-rest-clojure-split.handler-rest-clean)

(defn handler-rest-clean [a & rest]
  ;; ok: test-rest-clojure
  (sink rest))
