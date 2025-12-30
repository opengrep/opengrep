(ns taint-propagation)

;; FIXME: Organise this a bit.
;; TODO: Add more nested examples, sink as str arg etc.

(def f
  (fn [x]
    ;; ruleid: taint-call
    (sink x)))

(def f
  (fn [x]
    ;; ruleid: taint-call
    {::a/sink (sink x)}))

(def f
  (fn [x]
    ;; ruleid: taint-call
    {:my-ns/sink (sink x)}))

(def f
  (fn [x]
    ;; ruleid: taint-call
    {:sink (sink x)}))

(defn destr [& {:keys [a b] :as opts}]
  ;; ruleid: taint-call
  (sink a b opts))

(fn [x y z & rest]
  ;; ruleid: taint-call
  (sink rest))

(fn [x y z & [k r & rest]]
  ;; ruleid: taint-call
  (sink rest))

(fn [[ x y z & rest :as a], k]
  ;; ruleid: taint-call
  (sink a))

(fn [[ x y z & rest :as a]]
  ;; ruleid: taint-call
  (sink a))

(fn [x y z & rest]
  ;; ruleid: taint-call
  (sink x))

(fn f 
  ([] 1) 
  ([x]
    ;; ruleid: taint-call
   (sink x)) 
  ([x y]
   ;; ruleid: taint-call
   (sink y))
  ([a b c]
   ;; ok
   (sink x)))

(def res
  (fn [careful input]
    (do
      (let [x (source input)]
        (if careful
          (let [si (sanitize x)]
            ;; OK:
            (sink si))
          ;; ruleid: taint-call
          (sink x)))
      (print "called the sink")
      1)))

(defn f [{:keys [x d/y ::z] :or {x 1} :as opts}]
  (if x
    ;; ruleid: taint-call
    (sink y)
    ;; ruleid: taint-call
    (sink z)))

;; no error on rebinding
(defn f [{:keys [x y x] :as opts}]
  ;; ruleid: taint-call
  (sink x y))

(defn f [{:keys [x y] :as opts}]
  ;; ruleid: taint-call
  (sink opts))

(defn f [{x :x [y1 y2] :y, :as opts :or {x 1}}]
  ;; ruleid: taint-call
  (sink y1))

(defn f [{:syms [::x y] :as opts}]
  (if opts
    ;; ruleid: taint-call
    (sink x)
    ;; ruleid: taint-call
    (sink y)))

(def f
  (fn [x] 
    (let [z x] 
      (let [k z] 
        ;; ruleid: taint-call
        (sink k)))))

(def f
  (fn [x] 
    (let [z x] 
      (let [k z] 
        ;; ruleid: taint-call
        (if 1 (sink k))))))

(def f
  (fn [x] 
    (let [z x] 
      (let [k z] 
        (if 1 
          (sink 5) 
          (let [g k]
            ;; ruleid: taint-call
            (sink g)))))))

(def f
  (fn [x] 
    (let [z (g x)]
      ;; ruleid: taint-call
      (sink z))))

(def f
  (fn [x] 
    (let [z (if 1 (g x) (g 5))]
      ;; ruleid: taint-call
      (sink z))))

(def f
  (fn [x] 
    (let [z (g x)]
      (let [k (g z)]
        ;; ruleid: taint-call
        (sink k)))))

(defn f [x] 
  (let [z x] 
    (let [k z] 
      ;; ruleid: taint-call
      (if 1 (->> k ((fn [s] (sink s))))))
    (let [s 5] 
      ;; ok:
      (if 1 (sink k))) ;; k is out of scope here
    (do 
      ;; ok:
      (if 1 (sink k)))))

(defn f [x] 
  (let [z x] 
    (let [k z] 
      ;; ruleid: taint-call
      (if 1 (sink k)))))

(defn f [x] 
  (let [z x] 
    (let [k z] 
      ;; ruleid: taint-call
      (if 1 (sink k) nil))))

(def f 
  (fn [x] 
    (let [z x] 
      (let [k z] 
        ;; ruleid: taint-call
        (if 1 (sink k)))
      (let [s 5] 
        ;; ok:
        (if 1 (sink k)))
      (do 
        ;; ok:
        (if 1 (sink k))))))

(def f
  (fn [x]
    ;; this has to be on new line else intermediate variable
    ;; not highlighted in text...
    (let [k 9 z x i 5] 
      ;; ruleid: taint-call
      (sink z))))

(defn f[x]
  ;; ruleid: taint-call
  (-> x
      (:user)

      (sink)))

(fn [x] 
  ;; ruleid: taint-call
  (-> x
      (-> func)
      :input

      (sink)))

;; letfn
(letfn [(helper [a]
          ;; ruleid: taint-call
          (sink a))
        (helper_2 [b]
          ;; ruleid: taint-call
          (sink b))]
  (fn [x]
    ;; this should match with --taint-intrafile
    (helper x)
    ;; ruleid: taint-call
    (sink x)
    ;; ok:
    (sink a)))

;; ruleid: taint-call
(def _ #(sink % %& %2))

;; ok:
(def _ #(sink 5))

;; ruleid: taint-call
(def _ #(let [x %2 y %3] (sink x)))

;; ruleid: taint-call
(def _ #(let [z 5] (let [x %2 y %3] (sink x))))

;; ruleid: taint-call
(def _ #(let [x %2 y %3] (sink %&)))

;; ok:
(comment #(sink % %& %2))

;; ok:
#_#(sink % %& %2)

;; ruleid: taint-call
#(sink % %& %2)

;; when
(defn f [x]
  (when x
    (let [y x]
      ;; ruleid: taint-call
      (sink y))
    ;; ruleid: taint-call
    (sink x)
    ;; ok:
    (sink 5)))

;; when-let
(defn f [x]
    ;; ruleid: taint-call
    (when-let [[y1 y2] x] (sink y1)))

;; when-some
(defn f [x]
    ;; ruleid: taint-call
    (when-some [[y1 y2] x] (sink y1)))

;; when-first
;; note: we do not encode proper, [y1 y2] should be bound to
;; (first xs) when it's not null.
(defn f [xs]
    ;; ruleid: taint-call
    (when-first [[y1 y2] xs] (sink y2)))

(defn f [y]
  (as-> y x
  (func1 x)
  (func2 x)
  ;; ruleid: taint-call
  (sink x)))

(defn f [y]
  (as-> y x
  (sanitize x)
  ;; ok:
  (sink x)))

(defn f [y]
  ;; ruleid: taint-call
  (as-> (sink y) x))

(defn f [y]
  ;; ruleid: taint-call
  (-> (sink y)
      (as-> x
        (func x)
        ;; ruleid: taint-call
        (sink x))))

(defn f [y]
  (if-let [x y]
  ;; ruleid: taint-call
  (-> x sink)
  ;; ok:
  (sink x)))

(defn f [y]
  (if-some [x y]
  ;; ruleid: taint-call
  (-> x sink)
  ;; ok:
  (sink x)))

(defn f [x]
  (if-not x
  ;; ruleid: taint-call
  (-> x sink)
  ;; ruleid: taint-call
  (sink x)))

;; cond->
(defn f [x]
  ;; ruleid: taint-call
  (cond-> x
      true (->> sink)
      false (->> sanitizes sink)))

(defn f [x]
  ;; ok: taint-call
  (cond->> x ;; vs -> which leads to taint.
      true (-> sink) ;; (x sink) is not tainted.
      false sinkz))

(defn f [x]
  ;; ok: taint-call
  (cond->> x
      true sink
      true sanitize
      true sink))

(defn f[x]
  ;; ruleid: taint-call
  (some-> x
      (:user)
      (sink)))
