(ns test-hof-comprehensive-clojure-split.test-multi-arity-reverse
  (:require [test-hof-comprehensive-clojure-split.multi-arity-reverse :refer [multi-arity-reverse]]))

(defn test-multi-arity-reverse []
  (multi-arity-reverse nil (source)))
