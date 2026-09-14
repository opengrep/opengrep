(ns test-multi-arity-clojure-split.caller-f1
  (:require [test-multi-arity-clojure-split.f1 :refer [f1]]))

(defn caller-f1 []
  (f1 (source)))
