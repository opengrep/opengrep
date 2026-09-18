(ns test-hof-comprehensive-clojure-split.direct-call)

(defn direct-call [callback value]
  (callback value))
