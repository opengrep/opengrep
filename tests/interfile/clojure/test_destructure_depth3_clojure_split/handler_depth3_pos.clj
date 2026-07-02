(defn handler-depth3-pos [{outer :outer}]
  (let [{middle :middle} outer
        {body :body} middle]
    ;; ruleid: test-destructure-depth3-clojure
    (sink body)))
