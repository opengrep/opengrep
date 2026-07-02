(defn f2
  ;; ruleid: test-multi-arity-clojure
  ([x]      (sink x))    ; leg 1: len == 1, fires at len==1
  ([x & r]  (foo x)))
