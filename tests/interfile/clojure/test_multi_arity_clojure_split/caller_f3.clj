(ns test-multi-arity-clojure-split.caller-f3
  (:require [test-multi-arity-clojure-split.f3 :refer [f3]]))

(defn caller-f3 []
  (f3 (source) "ok"))
