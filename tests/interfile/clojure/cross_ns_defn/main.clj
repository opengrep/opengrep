(ns cross-ns-defn.main
  (:require [cross-ns-defn.impl :refer [greet]]))

(defn source [] (System/getenv "SECRET"))

(defn -main [& _]
  (let [tainted (source)]
    (greet tainted)))
