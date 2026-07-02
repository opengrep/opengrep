(ns cross-ns-defn.impl)

(defn sink [x]
  (println x))

(defn greet [msg]
  ;; ruleid: test-cross-ns-defn
  (sink msg))
