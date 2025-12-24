
(fn [x] 
  (let [z (source x)]
    ;; ruleid: taint-call
    (sink z)))

;; FIXME: not working because lambdas:

;; (defn f [x] 
;;   (let [z (source x)]
;;     ;; todoruleid: taint-call
;;     ((fn [k] (sink k)) z)))

;; (defn f [x] 
;;   (let [z (source x)
;;         f (fn [v] (sink v))]
;;     ;; todoruleid: taint-call
;;     (f z)))

;; (defn f [x] 
;;   (let [z x] 
;;     (let [k (source z)] 
;;       ;; todoruleid: taint-call
;;       (if 1 (->> k ((fn [s] (sink s))))))
;;     (let [s 5] 
;;       ;; ok:
;;       (if 1 (sink k))) ;; k is out of scope here
;;     (do 
;;       ;; ok:
;;       (if 1 (sink k)))))

