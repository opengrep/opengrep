(ns test-hof-comprehensive-clojure-split.test-builtin-filter-named
  (:require [test-hof-comprehensive-clojure-split.process-builtin-filter :refer [process-builtin-filter]]))

(defn test-builtin-filter-named []
  (filter process-builtin-filter (source)))
