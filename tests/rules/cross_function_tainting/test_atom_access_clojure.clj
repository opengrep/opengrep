;; Field-sensitive taint through Clojure's keyword-as-function
;; [(:body m)] read-side idiom. At the caller, a map is built with
;; exactly one tainted key; only the handler that reads the tainted
;; key should fire.

(defn handler-pos [m]
  ;; ruleid: test-atom-access-taint
  (sink (:body m)))

(defn caller-pos []
  (handler-pos {:body (source) :user "safe"}))

(defn handler-neg [m]
  ;; ok: test-atom-access-taint
  (sink (:body m)))

(defn caller-neg []
  (handler-neg {:body "safe" :user (source)}))

;; Two-argument form [(:body m default)]: Clojure evaluates [default]
;; eagerly regardless of whether [m.:body] is nil, and the lowering
;; emits a conditional [if tmp == nil then tmp = default], so any
;; taint in the default path reaches the result.
(defn with-default [m]
  ;; ruleid: test-atom-access-taint
  (sink (:body m (source))))
