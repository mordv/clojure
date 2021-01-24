(ns labs.lab2.task2
  (:require [labs.lab2.task1 :refer :all]))

(defn get-steps-count
  [interval]
  (int (/ interval default-step)))

(defn lazy-partial-integral-sums
  [f]
  (map first (iterate
               (fn [[sum i]]
                 [(+ sum (trapezoid-area (f (* i default-step)) (f (* (dec i) default-step)) default-step))
                  (inc i)])
               [0 0])))

(defn integral-partial-sums
  [f]
  (fn [x] (let [n (get-steps-count x)]
            (nth (lazy-partial-integral-sums f) n))))
