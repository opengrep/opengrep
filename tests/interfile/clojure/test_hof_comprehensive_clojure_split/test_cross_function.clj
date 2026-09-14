(ns test-hof-comprehensive-clojure-split.test-cross-function
  (:require [test-hof-comprehensive-clojure-split.sink-wrapper :refer [sink-wrapper]]))

(defn test-cross-function []
  (sink-wrapper (source)))
