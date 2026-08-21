(defn test-builtin-doseq []
  (let [arr (source)]
    (doseq [x arr]
      (sink x))))
