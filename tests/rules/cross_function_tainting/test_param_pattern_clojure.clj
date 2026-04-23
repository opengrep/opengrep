;; Clojure shortlambda: the callback `#(sink %)` has parameter `%`
;; represented as G.ParamPattern(PatList[PatAs(PatId %1, %)]).
;; Taint from (source) must flow through run-cb's application of cb to x
;; into the shortlambda's implicit parameter and onward to (sink %).

(defn run-cb [cb x] (cb x))

;; Baseline: explicit (fn [v] (sink v)) uses plain G.Param. Passes today.
(defn good-explicit-fn []
  ;; ruleid: test-param-pattern-taint
  (run-cb (fn [v] (sink v)) (source)))

;; Shortlambda HOF: currently fails because ParamPattern carries no
;; resolved parameter name that HOF argument routing can hang taint off.
(defn bad-shortlambda-hof []
  ;; ruleid: test-param-pattern-taint
  (run-cb #(sink %) (source)))

;; Shortlambda with map: same failure mode via a higher-order collection op.
(defn bad-shortlambda-map []
  ;; ruleid: test-param-pattern-taint
  (map #(sink %) [(source)]))
