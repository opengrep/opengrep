(ns test-hof-comprehensive-clojure-split.test-builtin-some-named
  (:require [test-hof-comprehensive-clojure-split.process-builtin-some :refer [process-builtin-some]]))

(defn test-builtin-some-named []
  (some process-builtin-some (source)))
