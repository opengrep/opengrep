(ns test-rest-clojure-split.caller-rest2-pos2
  (:require [test-rest-clojure-split.handler-rest2 :refer [handler-rest2]]))

(defn caller-rest2-pos2 []
  (handler-rest2 "safe" "ok" (source)))
