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
