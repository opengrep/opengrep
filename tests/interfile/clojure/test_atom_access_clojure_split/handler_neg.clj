(ns test-atom-access-clojure-split.handler-neg)

(defn handler-neg [m]
  ;; ok: test-atom-access-taint
  (sink (:body m)))
