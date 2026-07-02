(defn handler-depth3-neg [{outer :outer}]
  (let [{middle :middle} outer
        {body :body} middle]
    ;; ok: test-destructure-depth3-clojure
    (sink body)))
