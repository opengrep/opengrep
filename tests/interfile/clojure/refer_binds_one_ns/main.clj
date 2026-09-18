(ns app.main
  (:require [app.handlers :refer [handle]]))

(defn source [] (System/getenv "SECRET"))

(defn -main [& _]
  (let [tainted (source)]
    (handle tainted)))
