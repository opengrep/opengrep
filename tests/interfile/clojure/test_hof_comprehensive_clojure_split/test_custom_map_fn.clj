(ns test-hof-comprehensive-clojure-split.test-custom-map-fn
  (:require [test-hof-comprehensive-clojure-split.custom-map-builtin :refer [custom-map-builtin]]))

(defn test-custom-map-fn []
  (custom-map-builtin (source) (fn [x]
                                 ;; ruleid: test-hof-taint
                                 (sink x)
                                 x)))
