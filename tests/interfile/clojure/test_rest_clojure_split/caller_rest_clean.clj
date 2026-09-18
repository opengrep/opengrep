(ns test-rest-clojure-split.caller-rest-clean
  (:require [test-rest-clojure-split.handler-rest-clean :refer [handler-rest-clean]]))

(defn caller-rest-clean []
  (handler-rest-clean "safe" "ok" "no-source"))
