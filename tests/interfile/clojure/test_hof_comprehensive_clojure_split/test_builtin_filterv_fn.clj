(ns test-hof-comprehensive-clojure-split.test-builtin-filterv-fn)

(defn test-builtin-filterv-fn []
  (filterv (fn [x]
             ;; ruleid: test-hof-taint
             (sink x)
             true)
           (source)))
