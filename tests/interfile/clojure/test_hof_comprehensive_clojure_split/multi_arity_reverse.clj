(defn multi-arity-reverse
  ([x]
   ;; ruleid: test-hof-taint
   (sink x))
  ([x y] (multi-arity-reverse y)))
