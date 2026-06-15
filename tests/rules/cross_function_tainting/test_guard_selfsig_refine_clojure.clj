;; Multi-arity self-delegation where each self-sig pass fuses a new arity
;; disjunct into the guard of the same ToReturn effect: pass 2 folds the
;; 3-arity leg's guard at the [(f x y 2)] call and fuses a [len == 2]
;; disjunct; pass 3 does the same for the 1-arity leg. The stability check
;; must treat guard-only refinement as growth ([Effects.equal_with_guards]):
;; comparing without guards stops the loop after pass 2, the [len == 1]
;; disjunct is never fused, and the 1-argument call below folds the stored
;; guard to false -- losing this finding.

(defn source [] "taint")
(defn sink [x] x)

(defn f
  ([x] (f x 1))
  ([x y] (f x y 2))
  ([x y z] (if (= z 2) x "")))

(defn call []
  ;; ruleid: test-guard-selfsig-refine
  (sink (f (source))))
