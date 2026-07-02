(ns app.main
  (:require [app.handlers :as h]))

(defn source [] (System/getenv "SECRET"))

(defn -main [& _]
  (let [tainted (source)]
    (h/handle tainted)))
