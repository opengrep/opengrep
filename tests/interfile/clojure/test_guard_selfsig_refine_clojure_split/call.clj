(ns test-guard-selfsig-refine-clojure-split.call
  (:require [test-guard-selfsig-refine-clojure-split.f :refer [f]]
            [test-guard-selfsig-refine-clojure-split.sink :refer [sink]]
            [test-guard-selfsig-refine-clojure-split.source :refer [source]]))

(defn call []
  ;; ruleid: test-guard-selfsig-refine
  (sink (f (source))))
