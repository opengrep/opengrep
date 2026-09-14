(ns test-rest-clojure-split.caller-rest-source-in-head
  (:require [test-rest-clojure-split.handler-rest-source-in-head :refer [handler-rest-source-in-head]]))

(defn caller-rest-source-in-head []
  ;; source goes to fixed slot [a]; [rest] covers positions [1..]
  (handler-rest-source-in-head (source) "ok" "x"))
