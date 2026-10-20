;; ERROR: match
(let [{x :a} m] (f x))

;; A map binding form ignores other keys, and its entries have no order.
;; ERROR: match
(let [{y :b x :a} m] (f x))

(let [{x :b} m] (f x))
