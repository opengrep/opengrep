(ns test-hof-comprehensive-clojure-split.test-builtin-doseq)

(defn test-builtin-doseq []
  (let [arr (source)]
    (doseq [x arr]
      (sink x))))
