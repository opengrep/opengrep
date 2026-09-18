(ns test-hof-comprehensive-clojure-split.test-direct-call-named
  (:require [test-hof-comprehensive-clojure-split.direct-call :refer [direct-call]]
            [test-hof-comprehensive-clojure-split.process-direct :refer [process-direct]]))

(defn test-direct-call-named []
  (direct-call process-direct (source)))
