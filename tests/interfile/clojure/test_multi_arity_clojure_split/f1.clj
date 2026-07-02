(defn f1
  ([x]      (foo x))     ; leg 1: len == 1, no sink
  ;; ok: test-multi-arity-clojure
  ([x & r]  (sink x)))
