(ns test-intrafile-clojure-split.f)

(defn f [x] 
  ;; ruleid: taint-call
  (sink x))
