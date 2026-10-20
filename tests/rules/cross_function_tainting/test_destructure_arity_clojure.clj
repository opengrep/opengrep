;; A multi-arity leg whose parameter is a destructure is taken by every call
;; with its number of arguments: the destructure never fails, so a map
;; without the key, nil, or a shorter vector still binds the parameters.

(defn str-key
  ([{x "a"} y]
   ;; ruleid: test-destructure-arity-taint
   (sink y))
  ([y] y))

(defn str-key-caller []
  (str-key {"b" 1} (source)))

(defn keys-nil
  ([{:keys [x]} y]
   ;; ruleid: test-destructure-arity-taint
   (sink y))
  ([y] y))

(defn keys-nil-caller []
  (keys-nil nil (source)))

(defn vec-short
  ([[a b] y]
   ;; ruleid: test-destructure-arity-taint
   (sink y))
  ([y] y))

(defn vec-short-caller []
  (vec-short [1] (source)))
