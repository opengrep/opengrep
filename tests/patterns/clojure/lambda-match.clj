;; ERROR: match
(def id (fn [x] x))

;; ERROR: match
(def also-id (-> [x] (fn x)))
;; Left this test here to show the limitations (or advantages?)
;; of the current translation. Note: the inverse does not match:
;; a pattern (-> [...] (fn ...)) only matches -> forms.
