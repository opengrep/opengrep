(ns test-hof-comprehensive-clojure-split.test-builtin-remove-named
  (:require [test-hof-comprehensive-clojure-split.process-builtin-remove :refer [process-builtin-remove]]))

(defn test-builtin-remove-named []
  (remove process-builtin-remove (source)))
