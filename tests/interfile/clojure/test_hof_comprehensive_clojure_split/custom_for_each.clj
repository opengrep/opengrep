(ns test-hof-comprehensive-clojure-split.custom-for-each)

(defn custom-for-each [callback coll]
  (doseq [item coll]
    (callback item)))
