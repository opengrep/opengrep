(defn handler-pos [{body :body user :user}]
  ;; ruleid: test-map-destructure-taint
  (sink body))
