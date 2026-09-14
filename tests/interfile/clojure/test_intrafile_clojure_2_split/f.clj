(ns test-intrafile-clojure-2-split.f)

(def f
  ;; ruleid: taint-call
  (fn [x] (sink x)))
