(defn test-custom-foreach-fn []
  (custom-for-each (fn [x]
                     (sink x))
                   (source)))
