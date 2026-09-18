(ns test-hof-comprehensive-clojure-split.test-multi-arity
  (:require [test-hof-comprehensive-clojure-split.multi-arity-call :refer [multi-arity-call]]))

(defn test-multi-arity []
  (multi-arity-call (source)))
