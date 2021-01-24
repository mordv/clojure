(ns labs.lab2.task1
  (:use labs.utils.utils))


(def default-step 0.001)

(defn trapezoid-area
  [a b h]
  (/ (* (+ a b) h) 2.)
  )

(defn integral-function
  ([f x] (integral-function f x default-step))
  ([f x step] (integral-function f x step 0))
  ([f x step acc]
   (if (double-zero? x)
     acc
     (recur f (- x step) step (+
                                acc
                                (trapezoid-area
                                  (f x)
                                  (f (- x step))
                                  step)))
     )))

(def memoized-integral-function
  (memoize integral-function))

(defn integral
  [f]
  (fn [x] (integral-function f x)))

(defn memoized-integral
  [f]
  (fn [x] (memoized-integral-function f x)))



