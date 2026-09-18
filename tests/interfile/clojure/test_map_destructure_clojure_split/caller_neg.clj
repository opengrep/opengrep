(ns test-map-destructure-clojure-split.caller-neg
  (:require [test-map-destructure-clojure-split.handler-neg :refer [handler-neg]]))

(defn caller-neg []
  (handler-neg {:body "safe" :user (source)}))
