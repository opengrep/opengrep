(ns test-hof-comprehensive-clojure-split.zz-main
  (:require [test-hof-comprehensive-clojure-split.toplevel-handler :refer [toplevel-handler]]
            [test-hof-comprehensive-clojure-split.toplevel-items :refer [toplevel-items]]))

(toplevel-handler (source))
(doall (map toplevel-handler toplevel-items))
