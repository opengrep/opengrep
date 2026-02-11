;; Comprehensive HOF test for Clojure: Custom and built-in higher-order functions
;; Tests taint flow from source() to sink() across multiple HOF scenarios.

;; ===== Custom HOF Functions =====

;; Single-arity HOF that calls its callback with a value
(defn direct-call [callback value]
  (callback value))

;; Delegates to built-in (tests ToSinkInCall propagation)
(defn custom-map-builtin [arr callback]
  (map callback arr))

;; HOF that applies callback to each element (loop style)
(defn custom-for-each [callback coll]
  (doseq [item coll]
    (callback item)))

(defn custom-filter [arr callback]
  (filter callback arr))

;; Multi-arity HOF: 1-arity delegates to 2-arity
(defn multi-arity-call
  ([x] (multi-arity-call x nil))
  ([x y]
   ;; ruleid: test-hof-taint
   (sink x)))

;; ===== Custom HOF with named callbacks =====

(defn process-direct [x]
  ;; ruleid: test-hof-taint
  (sink x))

(defn test-direct-call-named []
  (direct-call process-direct (source)))

(defn process-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-custom-map-named []
  (custom-map-builtin (source) process-map))

;; todoruleid: test-hof-taint
(defn process-foreach [x]
  (sink x))

(defn test-custom-foreach-named []
  (custom-for-each process-foreach (source)))

(defn process-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-custom-filter-named []
  (custom-filter (source) process-filter))

;; ===== Custom HOF with inline fn =====

(defn test-direct-call-lambda []
  ;; ruleid: test-hof-taint
  (direct-call (fn [x] (sink x)) (source)))

(defn test-custom-map-fn []
  (custom-map-builtin (source) (fn [x]
                                 ;; ruleid: test-hof-taint
                                 (sink x)
                                 x)))

;; todoruleid: test-hof-taint
(defn test-custom-foreach-fn []
  (custom-for-each (fn [x]
                     (sink x))
                   (source)))

(defn test-custom-filter-fn []
  (custom-filter (source) (fn [x]
                            ;; ruleid: test-hof-taint
                            (sink x)
                            true)))

;; ===== Multi-arity delegation =====

(defn test-multi-arity []
  (multi-arity-call (source)))

;; ===== Cross-function return taint (implicit return) =====

(defn get-history [name owner]
  (source))

(defn process-history [node]
  ;; ruleid: test-hof-taint
  (sink node)
  [node])

(defn test-complex-example []
  (let [history (get-history "name" "owner")]
    (mapcat process-history [history])))

;; ===== Built-in HOF with named callbacks =====

(defn process-builtin-map [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-builtin-map-named []
  (map process-builtin-map (source)))

(defn process-builtin-filter [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-builtin-filter-named []
  (filter process-builtin-filter (source)))

(defn process-builtin-reduce [acc x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-builtin-reduce-named-3arg []
  (reduce process-builtin-reduce nil (source)))

(defn test-builtin-reduce-named-2arg []
  (reduce process-builtin-reduce (source)))

(defn process-builtin-keep [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

(defn test-builtin-keep-named []
  (keep process-builtin-keep (source)))

(defn process-builtin-remove [x]
  ;; ruleid: test-hof-taint
  (sink x)
  false)

(defn test-builtin-remove-named []
  (remove process-builtin-remove (source)))

(defn process-builtin-some [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-builtin-some-named []
  (some process-builtin-some (source)))

(defn process-builtin-every [x]
  ;; ruleid: test-hof-taint
  (sink x)
  true)

(defn test-builtin-every-named []
  (every? process-builtin-every (source)))

;; ===== Built-in HOF with inline fn =====

(defn test-builtin-map-fn []
  (map (fn [x]
         ;; ruleid: test-hof-taint
         (sink x)
         x)
       (source)))

(defn test-builtin-filter-fn []
  (filter (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            true)
          (source)))

(defn test-builtin-reduce-fn []
  (reduce (fn [acc x]
            ;; ruleid: test-hof-taint
            (sink x)
            x)
          nil
          (source)))

(defn test-builtin-mapv-fn []
  (mapv (fn [x]
          ;; ruleid: test-hof-taint
          (sink x)
          x)
        (source)))

(defn test-builtin-filterv-fn []
  (filterv (fn [x]
             ;; ruleid: test-hof-taint
             (sink x)
             true)
           (source)))

(defn test-builtin-keep-fn []
  (keep (fn [x]
          ;; ruleid: test-hof-taint
          (sink x)
          x)
        (source)))

(defn test-builtin-remove-fn []
  (remove (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            false)
          (source)))

(defn test-builtin-some-fn []
  (some (fn [x]
          ;; ruleid: test-hof-taint
          (sink x)
          true)
        (source)))

(defn test-builtin-every-fn []
  (every? (fn [x]
            ;; ruleid: test-hof-taint
            (sink x)
            true)
          (source)))

;; ===== doseq and for (binding-vector forms) =====

;; todoruleid: test-hof-taint
(defn test-builtin-doseq []
  (let [arr (source)]
    (doseq [x arr]
      (sink x))))

;; todoruleid: test-hof-taint
(defn test-builtin-for []
  (let [arr (source)]
    (for [x arr]
      (do
        (sink x)
        x))))

;; ===== Cross-function taint flow =====

(defn sink-wrapper [x]
  ;; ruleid: test-hof-taint
  (sink x))

(defn test-cross-function []
  (sink-wrapper (source)))

(defn test-cross-function-let []
  (let [tainted (source)]
    ;; ruleid: test-hof-taint
    (sink tainted)))

(defn test-cross-function-fn []
  ;; ruleid: test-hof-taint
  (let [r (fn [x] (sink x))]
    (r (source))))

;; ===== Top-level tests =====

(defn toplevel-handler [x]
  ;; ruleid: test-hof-taint
  (sink x)
  x)

;; Top-level direct call
(toplevel-handler (source))

;; Top-level with built-in HOF
(def toplevel-items (source))
(doall (map toplevel-handler toplevel-items))
