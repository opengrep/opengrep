(defn variadic-no-delegate
  ([x] x)
  ([x y & rest]
   ;; ok: test-hof-taint
   (sink x)))
