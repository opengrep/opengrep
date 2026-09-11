(defn test-complex-example []
  (let [history (get-history "name" "owner")]
    (mapcat process-history [history])))
