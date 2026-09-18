(ns test-intrafile-clojure-split.j)

(defn j [x] 
  (let [z (source x)
        ;; ruleid: taint-call
        i (fn [v] (sink v))]
    (i z)))
