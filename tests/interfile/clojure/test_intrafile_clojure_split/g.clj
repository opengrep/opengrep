(ns test-intrafile-clojure-split.g
  (:require [test-intrafile-clojure-split.f :refer [f]]))

(defn g [] 
  (f (source)))
