;; Clojure variadic rest [a & rest]: [rest] binds the trailing slice
;; of the call's argument list from index k (the rest start) onwards.
;; A source at any position >= k reaches [rest]; a source at a fixed
;; slot before [&] does not.

(defn handler-rest-pos1 [a & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))

(defn caller-rest-pos1 []
  (handler-rest-pos1 "safe" (source) "x"))


(defn handler-rest-pos2 [a & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))

(defn caller-rest-pos2 []
  (handler-rest-pos2 "safe" "ok" (source)))


(defn handler-rest-pos-deep [a & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))

(defn caller-rest-pos-deep []
  ;; source four positions into the rest range
  (handler-rest-pos-deep "safe" "a" "b" "c" "d" (source)))


(defn handler-rest-clean [a & rest]
  ;; ok: test-rest-clojure
  (sink rest))

(defn caller-rest-clean []
  (handler-rest-clean "safe" "ok" "no-source"))


(defn handler-rest-source-in-head [a & rest]
  ;; ok: test-rest-clojure
  (sink rest))

(defn caller-rest-source-in-head []
  ;; source goes to fixed slot [a]; [rest] covers positions [1..]
  (handler-rest-source-in-head (source) "ok" "x"))


;; Two-fixed-slots form [a b & rest]; rest covers positions [2..]

(defn handler-rest2 [a b & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))

(defn caller-rest2-pos2 []
  (handler-rest2 "safe" "ok" (source)))


(defn handler-rest2-deep [a b & rest]
  ;; ruleid: test-rest-clojure
  (sink rest))

(defn caller-rest2-deep []
  (handler-rest2-deep "safe" "ok" "x" "y" (source)))


(defn handler-rest2-source-in-b [a b & rest]
  ;; ok: test-rest-clojure
  (sink rest))

(defn caller-rest2-source-in-b []
  ;; source at position 1 binds [b]; [rest] covers positions [2..]
  (handler-rest2-source-in-b "safe" (source) "ok"))
