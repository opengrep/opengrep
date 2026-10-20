;; ERROR: match
(let [{x :a} m] (f x))

;; ERROR: match
(let [{y :b x :a} m] (f x))

(let [{x :b} m] (f x))
