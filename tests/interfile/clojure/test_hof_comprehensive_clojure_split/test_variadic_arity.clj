(ns test-hof-comprehensive-clojure-split.test-variadic-arity
  (:require [test-hof-comprehensive-clojure-split.variadic-call :refer [variadic-call]]))

(defn test-variadic-arity []
  (variadic-call (source)))
