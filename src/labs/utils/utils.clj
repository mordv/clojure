(ns labs.utils.utils)

(def eps 0.01)

(defn double-eq?
  [x y]
  (< (Math/abs (double (- x y))) eps))

(defn double-zero?
  [x]
  (double-eq? x 0))
