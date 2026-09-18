(ns test-hof-comprehensive-clojure-split.test-builtin-for)

(defn test-builtin-for []
  (let [arr (source)]
    (for [x arr]
      (do
        (sink x)
        x))))
