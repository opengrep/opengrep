(defn f
  ([x] (f x 1))
  ([x y] (f x y 2))
  ([x y z] (if (= z 2) x "")))
