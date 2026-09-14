(ns test-hof-comprehensive-clojure-split.test-custom-foreach-named
  (:require [test-hof-comprehensive-clojure-split.custom-for-each :refer [custom-for-each]]
            [test-hof-comprehensive-clojure-split.process-foreach :refer [process-foreach]]))

(defn test-custom-foreach-named []
  (custom-for-each process-foreach (source)))
