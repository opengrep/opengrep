(ns test-map-destructure-clojure-split.caller-pos
  (:require [test-map-destructure-clojure-split.handler-pos :refer [handler-pos]]))

(defn caller-pos []
  (handler-pos {:body (source) :user "safe"}))
