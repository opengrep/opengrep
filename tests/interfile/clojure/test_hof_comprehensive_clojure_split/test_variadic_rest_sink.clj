(ns test-hof-comprehensive-clojure-split.test-variadic-rest-sink
  (:require [test-hof-comprehensive-clojure-split.variadic-rest-sink :refer [variadic-rest-sink]]))

(defn test-variadic-rest-sink []
  (variadic-rest-sink nil (source)))
