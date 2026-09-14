(ns test-intrafile-clojure-2-split.g
  (:require [test-intrafile-clojure-2-split.f :refer [f]]))

(defn g []
  (f (source)))
