(ns test-atom-access-clojure-split.handler-pos)

(defn handler-pos [m]
  ;; ruleid: test-atom-access-taint
  (sink (:body m)))
