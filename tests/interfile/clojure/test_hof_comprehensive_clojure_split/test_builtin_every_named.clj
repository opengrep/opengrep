(ns test-hof-comprehensive-clojure-split.test-builtin-every-named
  (:require [test-hof-comprehensive-clojure-split.process-builtin-every :refer [process-builtin-every]]))

(defn test-builtin-every-named []
  (every? process-builtin-every (source)))
