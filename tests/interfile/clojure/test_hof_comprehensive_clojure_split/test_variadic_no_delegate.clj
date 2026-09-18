(ns test-hof-comprehensive-clojure-split.test-variadic-no-delegate
  (:require [test-hof-comprehensive-clojure-split.variadic-no-delegate :refer [variadic-no-delegate]]))

(defn test-variadic-no-delegate []
  (variadic-no-delegate (source)))
