(defn with-default [m]
  ;; ruleid: test-atom-access-taint
  (sink (:body m (source))))
