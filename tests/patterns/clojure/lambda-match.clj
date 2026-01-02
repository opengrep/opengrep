;; ERROR: match
(def id (fn [x] x))

;; ERROR: match
(def also-id (-> [x] (fn x)))
;; Left this test here to show the limitations (or advantages?)
;; of the current translation. If we wanted to not match this,
;; we could tag (using an OtherExpr wrapper) the fn form, and
;; all other forms.
;; I find that excessive but it's something we can do if needed.
;;
;; Note: the inverse does not match:
;; a pattern (-> [...] (fn ...)) only matches -> forms.
;;
;; TODO: If we really want this to be compositional, we need to
;; allow E to match with OtherExpr(_, [ G.E E ]) in clojure.
;; But this should propably not be the general case.
