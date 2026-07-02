(defn test-cross-function-let []
  (let [tainted (source)]
    ;; ruleid: test-hof-taint
    (sink tainted)))
