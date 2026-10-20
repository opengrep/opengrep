;; Field-sensitive taint through the directives of a Clojure map
;; destructure. `:keys` looks up keyword keys, `:strs` string keys and
;; `:syms` symbol keys, as a quoted symbol key does; only the sink whose
;; key carries a source at the caller should fire.

(defn keys-pos [{:keys [body user]}]
  ;; ruleid: test-map-directive-taint
  (sink body))

(defn keys-pos-caller []
  (keys-pos {:body (source) :user "safe"}))

(defn keys-neg [{:keys [body user]}]
  ;; ok: test-map-directive-taint
  (sink body))

(defn keys-neg-caller []
  (keys-neg {:body "safe" :user (source)}))

(defn strs-pos [{:strs [body]}]
  ;; ruleid: test-map-directive-taint
  (sink body))

(defn strs-pos-caller []
  (strs-pos {"body" (source) :body "safe"}))

(defn strs-neg [{:strs [body]}]
  ;; ok: test-map-directive-taint
  (sink body))

(defn strs-neg-caller []
  (strs-neg {"body" "safe" :body (source)}))

(defn syms-pos [{:syms [body]}]
  ;; ruleid: test-map-directive-taint
  (sink body))

(defn syms-pos-caller []
  (syms-pos {'body (source) "body" "safe" :body "safe"}))

(defn syms-neg [{:syms [body]}]
  ;; ok: test-map-directive-taint
  (sink body))

(defn syms-neg-caller []
  (syms-neg {'body "safe" "body" (source) :body (source)}))

(defn quoted-key-pos [{body 'body}]
  ;; ruleid: test-map-directive-taint
  (sink body))

(defn quoted-key-pos-caller []
  (quoted-key-pos {'body (source) "body" "safe"}))
