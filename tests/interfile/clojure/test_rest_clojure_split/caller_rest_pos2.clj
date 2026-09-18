(ns test-rest-clojure-split.caller-rest-pos2
  (:require [test-rest-clojure-split.handler-rest-pos2 :refer [handler-rest-pos2]]))

(defn caller-rest-pos2 []
  (handler-rest-pos2 "safe" "ok" (source)))
