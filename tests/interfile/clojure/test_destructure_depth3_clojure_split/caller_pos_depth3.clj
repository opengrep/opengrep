(ns test-destructure-depth3-clojure-split.caller-pos-depth3
  (:require [test-destructure-depth3-clojure-split.handler-depth3-pos :refer [handler-depth3-pos]]))

(defn caller-pos-depth3 []
  (handler-depth3-pos {:outer {:middle {:body (source) :other "safe"}}}))
