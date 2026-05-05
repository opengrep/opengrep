(defn handler-depth3-neg [{outer :outer}]
  (let [{middle :middle} outer
        {body :body} middle]
    ;; ok: test-destructure-depth3-clojure
    (sink body)))

(defn caller-neg-depth3 []
  (handler-depth3-neg {:outer {:middle {:body "safe" :other (source)}}}))


(defn handler-depth3-pos [{outer :outer}]
  (let [{middle :middle} outer
        {body :body} middle]
    ;; ruleid: test-destructure-depth3-clojure
    (sink body)))

(defn caller-pos-depth3 []
  (handler-depth3-pos {:outer {:middle {:body (source) :other "safe"}}}))
