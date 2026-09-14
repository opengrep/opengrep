(ns test-hof-comprehensive-clojure-split.test-complex-example
  (:require [test-hof-comprehensive-clojure-split.get-history :refer [get-history]]
            [test-hof-comprehensive-clojure-split.process-history :refer [process-history]]))

(defn test-complex-example []
  (let [history (get-history "name" "owner")]
    (mapcat process-history [history])))
