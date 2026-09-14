(ns test-guard-cap-arity-clojure-split.call-right
  (:require [test-guard-cap-arity-clojure-split.f-right :refer [f-right]]
            [test-guard-cap-arity-clojure-split.source :refer [source]]))

(defn call-right [] (f-right (source)))
