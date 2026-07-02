(defn handler-pos [m]
  ;; ruleid: test-atom-access-taint
  (sink (:body m)))
