(ns test-multi-arity-clojure-split.caller-f4
  (:require [test-multi-arity-clojure-split.f4 :refer [f4]]))

(defn caller-f4 []
  (f4 (source) "a" "b"))
