;; False-positive edge-case tests for Clojure intrafile taint tracking.
;; NONE of the (sink ...) calls below should be flagged.
;; Every `sink` line is annotated `ok:` meaning "no finding expected".

(ns test-clojure-fp
  (:require [clojure.core :as core]))

;; =========================================================================
;; 1. Clean data through helper functions — no source involved
;; =========================================================================

(defn helper-clean [x]
  ;; ok: test-clojure-fp
  (sink x))

(defn test-clean-call []
  (helper-clean "safe"))

;; =========================================================================
;; 2. Sanitized data — source is sanitized before reaching sink
;; =========================================================================

(defn test-sanitized-direct []
  (let [x (source)
        y (sanitize x)]
    ;; ok: test-clojure-fp
    (sink y)))

(defn pass-through [v] v)

(defn test-sanitized-through-helper []
  (let [x (source)
        y (sanitize x)]
    (pass-through y)
    ;; ok: test-clojure-fp
    (sink (pass-through (sanitize (source))))))

;; =========================================================================
;; 3. HOF with clean data — map/filter on untainted collections
;; =========================================================================

(defn sink-wrapper [x]
  ;; ok: test-clojure-fp
  (sink x))

(defn test-map-clean-data []
  (let [arr ["a" "b" "c"]]
    (map sink-wrapper arr)))

(defn test-filter-clean-data []
  (let [arr [1 2 3]]
    (filter (fn [x]
              ;; ok: test-clojure-fp
              (sink x)
              true) arr)))

(defn test-mapv-clean-data []
  (let [arr ["safe"]]
    (mapv #(do
             ;; ok: test-clojure-fp
             (sink %)
             %) arr)))

;; =========================================================================
;; 4. HOF with sanitized data — tainted source sanitized before HOF
;; =========================================================================

(defn test-map-sanitized []
  (let [arr [(sanitize (source))]]
    (map (fn [x]
           ;; ok: test-clojure-fp
           (sink x)) arr)))

(defn test-reduce-sanitized []
  (let [arr [(sanitize (source))]]
    (reduce (fn [acc x]
              ;; ok: test-clojure-fp
              (sink x)
              (+ acc x)) 0 arr)))

;; =========================================================================
;; 5. Different variable — source assigned to one var, clean var sunk
;; =========================================================================

(defn test-different-var []
  (let [tainted (source)
        clean "safe"]
    ;; ok: test-clojure-fp
    (sink clean)))

(defn test-different-var-in-fn []
  (let [tainted (source)
        clean "safe"]
    ((fn [x]
       ;; ok: test-clojure-fp
       (sink x)) clean)))

;; =========================================================================
;; 6. Overwritten variable — source overwritten before reaching sink
;; =========================================================================

(defn test-overwritten []
  (let [x (source)
        x "safe"]
    ;; ok: test-clojure-fp
    (sink x)))

;; =========================================================================
;; 7. Named function called with clean arg despite taint elsewhere
;; =========================================================================

(defn process-item [x]
  ;; ok: test-clojure-fp
  (sink x))

(defn test-clean-call-despite-taint-elsewhere []
  (let [tainted (source)]
    ;; Calling with a CLEAN value — should NOT flag
    (process-item "clean")))

;; =========================================================================
;; 8. HOF callback that does NOT reach a sink (callback itself is safe)
;; =========================================================================

(defn safe-callback [x]
  (str "processed: " x))

(defn test-map-safe-callback []
  (let [arr [(source)]]
    ;; The callback doesn't call sink, so no finding
    (map safe-callback arr)))

(defn test-filter-safe-callback []
  (let [arr [(source)]]
    (filter safe-callback arr)))

;; =========================================================================
;; 9. Reduce where only accumulator is sunk, not element
;;    Known FP: Clojure compresses multi-param callbacks into a single
;;    !!_implicit_param! with destructuring. Taint covers the entire param.
;; =========================================================================

(defn test-reduce-acc-only []
  (let [arr [(source)]]
    (reduce (fn [acc x]
              ;; todook: test-clojure-fp
              (sink acc)
              (+ acc 1)) 0 arr)))

;; =========================================================================
;; 10. Apply with clean args
;; =========================================================================

(defn test-apply-clean []
  (let [f (fn [x]
            ;; ok: test-clojure-fp
            (sink x))]
    (apply f ["safe"])))

;; =========================================================================
;; 11. Chained HOFs where sanitizer is in the middle
;;     Known FP: The builtin HOF ToReturn effect propagates data taint
;;     directly to the return value, bypassing callback semantics.
;; =========================================================================

(defn test-map-then-sanitize-then-map []
  (let [arr [(source)]
        step1 (map sanitize arr)]
    (map (fn [x]
           ;; todook: test-clojure-fp
           (sink x)) step1)))

;; =========================================================================
;; 12. Independent functions — one has source, one has sink, but no flow
;; =========================================================================

(defn producer []
  (source))

(defn consumer [x]
  ;; ok: test-clojure-fp
  (sink x))

(defn test-no-cross-contamination []
  ;; producer creates taint, but consumer is called with clean data
  (producer)
  (consumer "clean"))

;; =========================================================================
;; 13. Nested lambdas — inner lambda uses clean captured variable
;; =========================================================================

(defn test-nested-lambda-clean-capture []
  (let [clean "safe"
        tainted (source)]
    ((fn []
       ((fn [x]
          ;; ok: test-clojure-fp
          (sink x)) clean)))))

;; =========================================================================
;; 14. Shorthand lambda #() with clean literal
;; =========================================================================

(defn test-shorthand-clean []
  (let [tainted (source)]
    (#(do
        ;; ok: test-clojure-fp
        (sink %)) "clean")))
