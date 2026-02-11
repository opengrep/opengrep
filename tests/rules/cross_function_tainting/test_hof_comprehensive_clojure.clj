;; Comprehensive HOF test for Clojure: Custom higher-order functions
;; Tests per-arity signature extraction for multi-arity functions

;; ===== Custom HOF Functions =====

;; Single-arity HOF that calls its callback with a value
(defn direct-call [callback value]
  (callback value))

;; Multi-arity HOF: 1-arity delegates to 2-arity
(defn multi-arity-call
  ([x] (multi-arity-call x nil))
  ([x y]
   ;; ruleid: test-hof-taint
   (sink x)))

;; HOF that applies callback to each element (loop style)
(defn custom-for-each [callback coll]
  (doseq [item coll]
    (callback item)))

;; ===== Test Cases =====

;; Test 1: Custom HOF with named callback
(defn process-direct [x]
  ;; ruleid: test-hof-taint
  (sink x))

(defn test-direct-call-named []
  (direct-call process-direct (source)))

;; Test 2: Custom HOF with inline lambda
(defn test-direct-call-lambda []
  ;; ruleid: test-hof-taint
  (direct-call (fn [x] (sink x)) (source)))

;; Test 3: Multi-arity delegation -- taint flows through 1-arity to 2-arity
(defn test-multi-arity []
  (multi-arity-call (source)))

;; Test 4: Built-in map with named callback (vector arg for coverage)
(defn process-map [x]
  ;; ruleid: test-hof-taint
  (sink x))

(defn test-builtin-map []
  (map process-map [(source)]))

;; Test 5: Built-in map with inline lambda
(defn test-builtin-map-lambda []
  ;; ruleid: test-hof-taint
  (map (fn [x] (sink x)) [(source)]))
