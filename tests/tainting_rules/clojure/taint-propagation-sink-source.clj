
(fn [x] 
  (let [z (source x)]
    ;; ruleid: taint-call
    (sink z)))

