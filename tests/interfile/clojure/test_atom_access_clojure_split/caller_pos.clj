(ns test-atom-access-clojure-split.caller-pos
  (:require [test-atom-access-clojure-split.handler-pos :refer [handler-pos]]))

(defn caller-pos []
  (handler-pos {:body (source) :user "safe"}))
