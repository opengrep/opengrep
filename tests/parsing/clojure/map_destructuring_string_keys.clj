;; Parsing test: map destructuring with string keys.

(defn f [{x "a"}] x)

(defn g [{x "a" y "b"}] [x y])

(defn h [{x :kw y "str"}] [x y])

(let [{x "a"} {"a" 1}] x)

(defn i [{x "a" :as opts :or {x 0}}] [x opts])
