(defn handler-neg [m]
  ;; ok: test-atom-access-taint
  (sink (:body m)))
