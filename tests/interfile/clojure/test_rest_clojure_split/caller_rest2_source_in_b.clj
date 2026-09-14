(ns test-rest-clojure-split.caller-rest2-source-in-b
  (:require [test-rest-clojure-split.handler-rest2-source-in-b :refer [handler-rest2-source-in-b]]))

(defn caller-rest2-source-in-b []
  ;; source at position 1 binds [b]; [rest] covers positions [2..]
  (handler-rest2-source-in-b "safe" (source) "ok"))
