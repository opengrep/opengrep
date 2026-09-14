(ns test-hof-comprehensive-clojure-split.multi-arity-reverse-3)

(defn multi-arity-reverse-3
  ([x]
   ;; ruleid: test-hof-taint
   (sink x))
  ([x y] (multi-arity-reverse-3 y))
  ([x y z] (multi-arity-reverse-3 y z)))
