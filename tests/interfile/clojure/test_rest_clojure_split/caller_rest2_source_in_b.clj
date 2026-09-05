(defn caller-rest2-source-in-b []
  ;; source at position 1 binds [b]; [rest] covers positions [2..]
  (handler-rest2-source-in-b "safe" (source) "ok"))
