;; OK:
(def id (fn [x] x))

;; ERROR: match
(def also-id (-> [x] (fn x)))
