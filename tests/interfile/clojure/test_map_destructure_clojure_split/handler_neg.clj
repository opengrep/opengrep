(ns test-map-destructure-clojure-split.handler-neg)

(defn handler-neg [{body :body user :user}]
  ;; ok: test-map-destructure-taint
  (sink body))
