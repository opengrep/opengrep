(ns app.handlers)

(defn sink [x] (println x))

(defn handle [msg]
  ;; ruleid: test-refer-binds-one-ns
  (sink msg))
