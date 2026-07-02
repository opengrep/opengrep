(defn test-builtin-for []
  (let [arr (source)]
    (for [x arr]
      (do
        (sink x)
        x))))
