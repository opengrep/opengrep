(ns test-guard-cap-arity-clojure-split.call-wrong
  (:require [test-guard-cap-arity-clojure-split.f-wrong :refer [f-wrong]]
            [test-guard-cap-arity-clojure-split.source :refer [source]]))

(defn call-wrong [] (f-wrong (source) 1))
