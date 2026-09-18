(ns test-hof-comprehensive-clojure-split.test-custom-filter-named
  (:require [test-hof-comprehensive-clojure-split.custom-filter :refer [custom-filter]]
            [test-hof-comprehensive-clojure-split.process-filter :refer [process-filter]]))

(defn test-custom-filter-named []
  (custom-filter (source) process-filter))
