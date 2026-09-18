(ns test-hof-comprehensive-clojure-split.test-builtin-reduce-named-3arg
  (:require [test-hof-comprehensive-clojure-split.process-builtin-reduce :refer [process-builtin-reduce]]))

(defn test-builtin-reduce-named-3arg []
  (reduce process-builtin-reduce nil (source)))
