(ns test-rest-clojure-split.caller-rest-pos-deep
  (:require [test-rest-clojure-split.handler-rest-pos-deep :refer [handler-rest-pos-deep]]))

(defn caller-rest-pos-deep []
  ;; source four positions into the rest range
  (handler-rest-pos-deep "safe" "a" "b" "c" "d" (source)))
