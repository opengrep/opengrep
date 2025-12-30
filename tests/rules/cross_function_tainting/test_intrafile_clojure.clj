(defn f [x] 
  ;; ruleid: taint-call
  (sink x))

(defn g [] 
  (f (source)))

