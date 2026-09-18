(ns test-hof-comprehensive-clojure-split.variadic-call)

(defn variadic-call
  ([x] (variadic-call x nil))
  ([x y & rest]
   ;; ruleid: test-hof-taint
   (sink x)))
