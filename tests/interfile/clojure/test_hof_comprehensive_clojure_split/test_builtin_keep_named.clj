(ns test-hof-comprehensive-clojure-split.test-builtin-keep-named
  (:require [test-hof-comprehensive-clojure-split.process-builtin-keep :refer [process-builtin-keep]]))

(defn test-builtin-keep-named []
  (keep process-builtin-keep (source)))
