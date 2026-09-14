(ns app.main)

(require '[app.handlers :refer :all])

(defn source [] (System/getenv "SECRET"))

(defn -main [& _]
  (let [tainted (source)]
    (handle tainted)))
