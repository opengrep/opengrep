(ns test-hof-comprehensive-clojure-split.custom-map-builtin)

(defn custom-map-builtin [arr callback]
  (map callback arr))
