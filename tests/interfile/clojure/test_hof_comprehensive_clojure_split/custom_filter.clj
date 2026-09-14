(ns test-hof-comprehensive-clojure-split.custom-filter)

(defn custom-filter [arr callback]
  (filter callback arr))
