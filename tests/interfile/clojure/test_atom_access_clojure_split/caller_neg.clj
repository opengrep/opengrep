(ns test-atom-access-clojure-split.caller-neg
  (:require [test-atom-access-clojure-split.handler-neg :refer [handler-neg]]))

(defn caller-neg []
  (handler-neg {:body "safe" :user (source)}))
