(ns test-hof-comprehensive-clojure-split.test-direct-call-lambda
  (:require [test-hof-comprehensive-clojure-split.direct-call :refer [direct-call]]))

(defn test-direct-call-lambda []
  ;; ruleid: test-hof-taint
  (direct-call (fn [x] (sink x)) (source)))
