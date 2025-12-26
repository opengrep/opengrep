(defn f []
  ;ERROR:
  (setq user_data (get))
  (print "do stuff")
  (foobar)
  (eval user_data)
)
