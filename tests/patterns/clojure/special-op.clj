;; ERROR: match
(def redundant-or (or true false true nil))
;; Does not commute, not sure why.
