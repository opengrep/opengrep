;; Multi-arity Clojure dispatch: at a given call length, only the
;; matching leg's effects fire. The engine emits each leg's body
;; under the conjunction of the prior legs' negated conditions and
;; this leg's own condition. A sink in a later leg must not fire
;; for call lengths that an earlier leg matches.

(defn f1
  ([x]      (foo x))     ; leg 1: len == 1, no sink
  ;; ok: test-multi-arity-clojure
  ([x & r]  (sink x)))   ; leg 2: len >= 1, must NOT fire at len==1

(defn caller-f1 []
  (f1 (source)))


(defn f2
  ;; ruleid: test-multi-arity-clojure
  ([x]      (sink x))    ; leg 1: len == 1, fires at len==1
  ([x & r]  (foo x)))    ; leg 2: len >= 1, no sink

(defn caller-f2 []
  (f2 (source)))


(defn f3
  ([x]      (foo x))     ; leg 1: len == 1
  ([x y]    (foo x))     ; leg 2: len == 2
  ;; ok: test-multi-arity-clojure
  ([x & r]  (sink x)))   ; leg 3: len >= 1, must NOT fire at len==2

(defn caller-f3 []
  (f3 (source) "ok"))


(defn f4
  ([x]      (foo x))     ; leg 1: len == 1
  ([x y]    (foo x))     ; leg 2: len == 2
  ;; ruleid: test-multi-arity-clojure
  ([x & r]  (sink x)))   ; leg 3: len >= 1, fires at len==3

(defn caller-f4 []
  (f4 (source) "a" "b"))
