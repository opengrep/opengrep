(ns test-param-pattern-clojure-split.good-explicit-fn
  (:require [test-param-pattern-clojure-split.run-cb :refer [run-cb]]))

(defn good-explicit-fn []
  ;; ruleid: test-param-pattern-taint
  (run-cb (fn [v] (sink v)) (source)))
