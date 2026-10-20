;; ERROR: match
(let [{:keys [x]} m] (f x))

;; The :strs directive looks up string keys.
(let [{:strs [x]} m] (f x))

;; The same binding written without the directive is another form.
(let [{x :x} m] (f x))
