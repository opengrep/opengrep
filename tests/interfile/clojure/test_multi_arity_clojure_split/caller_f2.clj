(ns test-multi-arity-clojure-split.caller-f2
  (:require [test-multi-arity-clojure-split.f2 :refer [f2]]))

(defn caller-f2 []
  (f2 (source)))
