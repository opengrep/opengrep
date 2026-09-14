(ns test-hof-comprehensive-clojure-split.multi-arity-call)

(defn multi-arity-call
  ([x] (multi-arity-call x nil))
  ([x y]
   ;; ruleid: test-hof-taint
   (sink x)))
