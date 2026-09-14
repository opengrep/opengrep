(ns test-hof-comprehensive-clojure-split.variadic-no-delegate)

(defn variadic-no-delegate
  ([x] x)
  ([x y & rest]
   ;; ok: test-hof-taint
   (sink x)))
