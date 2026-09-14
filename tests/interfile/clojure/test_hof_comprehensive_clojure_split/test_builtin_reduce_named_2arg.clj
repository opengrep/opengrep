(ns test-hof-comprehensive-clojure-split.test-builtin-reduce-named-2arg
  (:require [test-hof-comprehensive-clojure-split.process-builtin-reduce :refer [process-builtin-reduce]]))

(defn test-builtin-reduce-named-2arg []
  (reduce process-builtin-reduce (source)))
