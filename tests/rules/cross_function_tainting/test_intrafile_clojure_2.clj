(def f
  ;; ruleid: taint-call
  (fn [x] (sink x)))

(defn g []
  (f (source)))
