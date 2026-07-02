(defn caller-neg-depth3 []
  (handler-depth3-neg {:outer {:middle {:body "safe" :other (source)}}}))
