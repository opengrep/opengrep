(ns test-rest-clojure-split.caller-rest2-deep
  (:require [test-rest-clojure-split.handler-rest2-deep :refer [handler-rest2-deep]]))

(defn caller-rest2-deep []
  (handler-rest2-deep "safe" "ok" "x" "y" (source)))
