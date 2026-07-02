(defn h [] 
  ;; ruleid: taint-call
  (let [r (fn [x] (sink x))]
    (r (source))))
