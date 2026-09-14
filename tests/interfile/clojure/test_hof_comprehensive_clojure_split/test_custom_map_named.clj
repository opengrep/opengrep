(ns test-hof-comprehensive-clojure-split.test-custom-map-named
  (:require [test-hof-comprehensive-clojure-split.custom-map-builtin :refer [custom-map-builtin]]
            [test-hof-comprehensive-clojure-split.process-map :refer [process-map]]))

(defn test-custom-map-named []
  (custom-map-builtin (source) process-map))
