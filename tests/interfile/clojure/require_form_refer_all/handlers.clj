(ns app.handlers)

(defn sink [x] (println x))

(defn handle [msg]
  ;; ruleid: test-require-form-refer-all
  (sink msg))
