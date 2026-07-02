(defn f4
  ([x]      (foo x))     ; leg 1: len == 1
  ([x y]    (foo x))     ; leg 2: len == 2
  ;; ruleid: test-multi-arity-clojure
  ([x & r]  (sink x)))
