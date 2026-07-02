(defn handler-neg [{body :body user :user}]
  ;; ok: test-map-destructure-taint
  (sink body))
