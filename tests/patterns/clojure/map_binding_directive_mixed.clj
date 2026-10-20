;; Directives mix with other entries, in any order.
;; ERROR: match
(let [{c :c :keys [x]} m] (f x))

;; ERROR: match
(let [{:or {x 0} :keys [x]} m] (f x))

;; ERROR: match
(let [{:strs [d] :keys [x]} m] (f x))
