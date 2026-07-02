(ns app.handlers)

(defn sink [x] (println x))

(defn handle [msg]
  ;; ruleid: test-as-aliased-require
  (sink msg))
