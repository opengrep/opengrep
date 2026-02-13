;; Comprehensive HOF test for Clojure: Custom and built-in higher-order functions
;; Tests all lambda syntaxes: named functions, fn lambdas, and shorthand #() lambdas
;; All of these should detect taint flow from source() to sink()

(ns test-hof
  (:require [clojure.core :as core]))

;; ===== Custom HOF Functions =====

;; Delegates to built-in (tests ToSinkInCall propagation)
(defn custom-map-builtin [arr callback]
  (map callback arr))

(defn custom-filter [arr callback]
  (filter callback arr))

(defn custom-reduce [arr callback init]
  (reduce callback init arr))

(defn direct-call [callback]
  (callback (source)))

;; ===== Named function tests =====

(defn process-custom-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

;; Test custom HOF delegating to built-in + named function
(defn test-custom-map-builtin-named []
  (let [arr [(source)]]
    (custom-map-builtin arr process-custom-map)))

(defn process-builtin-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-builtin-map-named []
  (let [arr [(source)]]
    (map process-builtin-map arr)))

(defn process-builtin-mapv [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-builtin-mapv-named []
  (let [arr [(source)]]
    (mapv process-builtin-mapv arr)))

(defn process-custom-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-custom-filter-named []
  (let [arr [(source)]]
    (custom-filter arr process-custom-filter)))

(defn process-builtin-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-builtin-filter-named []
  (let [arr [(source)]]
    (filter process-builtin-filter arr)))

(defn process-builtin-filterv [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-builtin-filterv-named []
  (let [arr [(source)]]
    (filterv process-builtin-filterv arr)))

(defn process-direct-call [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-direct-call-named []
  (direct-call process-direct-call))

;; ===== Regular fn lambda tests =====

(defn test-custom-map-builtin-fn-lambda []
  (let [arr [(source)]]
    (custom-map-builtin arr (fn [x]
                               ;; ruleid: test-hof-taint
                               (sink x)
                               x))))

(defn test-builtin-map-fn-lambda []
  (let [arr [(source)]]
    (map (fn [x]
           ;; ruleid: test-hof-taint
           (sink x)
           x)
         arr)))

(defn test-builtin-mapv-fn-lambda []
  (let [arr [(source)]]
    (mapv (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            x)
          arr)))

(defn test-custom-filter-fn-lambda []
  (let [arr [(source)]]
    (custom-filter arr (fn [x]
                         ;; ruleid: test-hof-taint
                         (sink x)
                         true))))

(defn test-builtin-filter-fn-lambda []
  (let [arr [(source)]]
    (filter (fn [x]
              ;; ruleid: test-hof-taint
              (sink x)
              true)
            arr)))

(defn test-builtin-filterv-fn-lambda []
  (let [arr [(source)]]
    (filterv (fn [x]
               ;; ruleid: test-hof-taint
               (sink x)
               true)
             arr)))

(defn test-direct-call-fn-lambda []
  (direct-call (fn [x]
                 ;; ruleid: test-hof-taint
                 (sink x)
                 x)))

;; ===== Shorthand lambda tests =====

(defn test-custom-map-builtin-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (custom-map-builtin arr #(sink %))))

(defn test-builtin-map-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (map #(sink %) arr)))

(defn test-builtin-mapv-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (mapv #(sink %) arr)))

(defn test-custom-filter-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (custom-filter arr #(do (sink %) true))))

(defn test-builtin-filter-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (filter #(do (sink %) true) arr)))

(defn test-builtin-filterv-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (filterv #(do (sink %) true) arr)))

;; ===== Reduce tests =====

(defn process-reduce [acc x]
  ;; ruleid: test-hof-taint
  (sink x)
  (conj acc x))

(defn test-builtin-reduce-named []
  (let [arr [(source)]]
    (reduce process-reduce [] arr)))

(defn test-builtin-reduce-fn-lambda []
  (let [arr [(source)]]
    (reduce (fn [acc x]
              ;; ruleid: test-hof-taint
              (sink x)
              (conj acc x))
            []
            arr)))

(defn test-builtin-reduce-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (reduce #(do (sink %2) (conj %1 %2)) [] arr)))

;; ===== Apply tests =====

(defn test-apply-named []
  ;; ruleid: test-hof-taint
  (apply sink [(source)]))

(defn test-apply-fn-lambda []
  (apply (fn [x]
           ;; ruleid: test-hof-taint
           (sink x))
         [(source)]))

(defn test-apply-shorthand []
  ;; ruleid: test-hof-taint
  (apply #(sink %) [(source)]))

;; ===== Keep tests =====

(defn process-keep [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-builtin-keep-named []
  (let [arr [(source)]]
    (keep process-keep arr)))

(defn test-builtin-keep-fn-lambda []
  (let [arr [(source)]]
    (keep (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            x)
          arr)))

(defn test-builtin-keep-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (keep #(do (sink %) %) arr)))

;; ===== Collection operation tests =====

(defn test-collection-conj []
  ;; Tests that taint flows from a tainted collection element through conj and first
  (let [coll [(source)]
        result (conj coll 42)]
    ;; ruleid: test-hof-taint
    (sink (first result))))

(defn test-collection-get []
  (let [coll {:key (source)}
        result (get coll :key)]
    ;; ruleid: test-hof-taint
    (sink result)))

(defn test-collection-first []
  (let [coll [(source)]
        result (first coll)]
    ;; ruleid: test-hof-taint
    (sink result)))

(defn test-collection-nth []
  (let [coll [(source)]
        result (nth coll 0)]
    ;; ruleid: test-hof-taint
    (sink result)))

;; ===== Complex real-world example =====

(defn get-history [name owner]
  (source))

(defn process-mapcat [node]
  (let [changes node]
    ;; ruleid: test-hof-taint
    (sink changes)
    [changes]))

(defn test-original-example []
  (let [history (get-history "name" "owner")]
    (mapcat process-mapcat [history])))

;; ===== Threading macro tests =====

(defn test-thread-last []
  (let [data (source)]
    ;; ruleid: test-hof-taint
    (->> data
         (map identity)
         (filter identity)
         (sink))))

(defn test-thread-first []
  (let [data (source)]
    ;; ruleid: test-hof-taint
    (-> data
        (conj)
        (sink))))

;; ===== Namespace-qualified function tests =====

(defn test-qualified-map-named []
  (let [arr [(source)]]
    (clojure.core/map process-builtin-map arr)))

(defn test-qualified-filter-fn-lambda []
  (let [arr [(source)]]
    (clojure.core/filter (fn [x]
                           ;; ruleid: test-hof-taint
                           (sink x)
                           true)
                         arr)))

(defn test-qualified-reduce-shorthand []
  (let [arr [(source)]]
    ;; ruleid: test-hof-taint
    (clojure.core/reduce #(do (sink %2) (conj %1 %2)) [] arr)))

(defn test-qualified-apply []
  ;; ruleid: test-hof-taint
  (clojure.core/apply sink [(source)]))

;; Stub functions
(defn source []
  "tainted")

(defn sink [s]
  :ok)
