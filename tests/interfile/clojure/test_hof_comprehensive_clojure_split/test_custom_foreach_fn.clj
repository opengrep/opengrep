(ns test-hof-comprehensive-clojure-split.test-custom-foreach-fn
  (:require [test-hof-comprehensive-clojure-split.custom-for-each :refer [custom-for-each]]))

(defn test-custom-foreach-fn []
  (custom-for-each (fn [x]
                     (sink x))
                   (source)))
