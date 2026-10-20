;; ERROR: match
(let [{:keys [d/y]} m] (f y))

;; The qualifier is part of the key.
(let [{:keys [e/y]} m] (f y))

(let [{:keys [y]} m] (f y))
