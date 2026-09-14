(ns other.handlers)

(defn sink [x] (println x))

(defn handle [msg]
  ;; ok: test-refer-binds-one-ns
  (sink msg))
