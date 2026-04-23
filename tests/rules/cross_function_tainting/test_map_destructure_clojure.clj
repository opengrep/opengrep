;; Field-sensitive taint through a Clojure map destructure.
;; The destructure binds `body` to `:body` and `user` to `:user`.
;; At the caller, we pass maps where exactly one key carries a
;; source; only the sink whose destructured leaf matches the tainted
;; key should fire.

(defn handler-pos [{body :body user :user}]
  ;; ruleid: test-map-destructure-taint
  (sink body))

(defn caller-pos []
  (handler-pos {:body (source) :user "safe"}))

(defn handler-neg [{body :body user :user}]
  ;; ok: test-map-destructure-taint
  (sink body))

(defn caller-neg []
  (handler-neg {:body "safe" :user (source)}))
