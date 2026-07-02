(defn caller-rest-source-in-head []
  ;; source goes to fixed slot [a]; [rest] covers positions [1..]
  (handler-rest-source-in-head (source) "ok" "x"))
