(ns other.handlers)

(defn sink [x] (println x))

(defn handle [msg]
  ;; ok: test-require-form-refer-all
  (sink msg))
