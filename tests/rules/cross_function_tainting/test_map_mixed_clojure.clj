;; Field-sensitive taint through a Clojure map destructure that mixes a
;; directive with other entries, in any order.

(defn mixed-pos [{user :user :keys [body]}]
  ;; ruleid: test-map-mixed-taint
  (sink body))

(defn mixed-pos-caller []
  (mixed-pos {:body (source) :user "safe"}))

(defn mixed-neg [{:or {body "default"} :keys [body] user :user}]
  ;; ok: test-map-mixed-taint
  (sink body))

(defn mixed-neg-caller []
  (mixed-neg {:body "safe" :user (source)}))
