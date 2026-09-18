(ns test-hof-comprehensive-clojure-split.test-builtin-map-named
  (:require [test-hof-comprehensive-clojure-split.process-builtin-map :refer [process-builtin-map]]))

(defn test-builtin-map-named []
  (map process-builtin-map (source)))
