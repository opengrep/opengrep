(defn custom-for-each [callback coll]
  (doseq [item coll]
    (callback item)))
