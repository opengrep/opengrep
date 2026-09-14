(ns test-rest-clojure-split.caller-rest-pos1
  (:require [test-rest-clojure-split.handler-rest-pos1 :refer [handler-rest-pos1]]))

(defn caller-rest-pos1 []
  (handler-rest-pos1 "safe" (source) "x"))
