(ns test-destructure-depth3-clojure-split.caller-neg-depth3
  (:require [test-destructure-depth3-clojure-split.handler-depth3-neg :refer [handler-depth3-neg]]))

(defn caller-neg-depth3 []
  (handler-depth3-neg {:outer {:middle {:body "safe" :other (source)}}}))
