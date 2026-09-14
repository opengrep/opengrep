(ns test-param-pattern-clojure-split.bad-shortlambda-hof
  (:require [test-param-pattern-clojure-split.run-cb :refer [run-cb]]))

(defn bad-shortlambda-hof []
  ;; ruleid: test-param-pattern-taint
  (run-cb #(sink %) (source)))
