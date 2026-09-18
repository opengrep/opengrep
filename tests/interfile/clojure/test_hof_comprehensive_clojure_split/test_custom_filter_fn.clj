(ns test-hof-comprehensive-clojure-split.test-custom-filter-fn
  (:require [test-hof-comprehensive-clojure-split.custom-filter :refer [custom-filter]]))

(defn test-custom-filter-fn []
  (custom-filter (source) (fn [x]
                            ;; ruleid: test-hof-taint
                            (sink x)
                            true)))
