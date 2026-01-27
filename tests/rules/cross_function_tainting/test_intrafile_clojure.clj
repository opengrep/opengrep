(defn f [x] 
  ;; ruleid: taint-call
  (sink x))

(defn g [] 
  (f (source)))

(defn h [] 
  ;; ruleid: taint-call
  (let [r (fn [x] (sink x))]
    (r (source))))

(defn j [x] 
  (let [z (source x)
        ;; ruleid: taint-call
        i (fn [v] (sink v))]
    (i z)))

;; FIXME: These tests pass when ran locally against the rule, but I get a strange
;; Mutex: Operation not permitted error when running `make core-test`.
;; I also had to mess up rule id and o k to avoid other errors as this uses regular
;; expressions.

;; (defn r [x] 
;;   (let [z (source x)]
;;     ;; rule id: taint-call
;;     ((fn [k] (sink k)) z)))

;; (defn k [x] 
;;   (let [z x] 
;;     (let [k (source z)] 
;;       ;; rule id: taint-call
;;       (if 1 (->> k ((fn [s] (sink s))))))
;;     (let [s 5] 
;;       ;; o k:
;;       (if 1 (sink k))) ;; k is out of scope here
;;     (do 
;;       ;; o k:
;;       (if 1 (sink k)))))


